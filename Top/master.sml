(*
	The master starts by reading a project description and
	throwing out interfaces and units that are not relevant to the
	user's command line.

	The master maintains a DAG that has a node for every
	(relevant) entry in the project description, and an arrow a ->
	b when a occurs free in b.  Every node has a mutable status;
	the possible status values are:

		WAITING: some prerequisite nodes do not have up to
			 date pinterface files
		READY: waiting for analysis
		PENDING: waiting for compilation.  A pending node may
			 have up to date pinterface and tali files;
			 PENDING values carry a boolean.
		WORKING: compilation in progress and pinterface file
			 is out of date.
		PROCEEDING: compilation in progress and pinterface
			    file is up to date.
		PENDING': waiting for assembly.  pinterface and asm
			  files are up-to-date.
		WORKING': assembly in progress
		DONE: up to date

	The status transition graphs are status.dot (for units) and
	status-iface.dot (for interfaces).  Status values carry times
	so that we can measure:

		total time:  time to go from READY to DONE
		slave time:  time spent in WORKING and PROCEEDING
		master time: time spent in WORKING'

	NB a node C transitions from WAITING to READY when every node
	reachable from C has an up to date pinterface file.  One might
	suppose that if every neighbor of C has an up to date
	pinterface file, then every node reachable from C does.  This
	is false.  Consider the project description

		interface I = "I.int" { }
		unit A = "A.sml" { }
		unit B : I = "B.sml" { A }
		unit C = "C.sml" { B }

	The neighbors of C are {B}.  The nodes reachable from C are
	{I, A, B}.  I may be compiled before A so that B may have an
	up to date pinterface file before A.  Thus, it is possible for
	every neighbor of C to have an up to date pinterface file
	while there exists a node reachable from C that does not.

	The master maintains worklists that classify nodes according
	to their status, track available and working slaves, and track
	local assembly jobs.  Bug: The master does not notice when a
	slave is "lost" and will not terminate while waiting for a
	response from such a slave.

XXX:
	Warn the user if source changes between analysis and compilation.

XXX:
	Consider

		interface I = "I.int" {}
		unit A : I = "a.sml" {}
		unit U = "u.sml" {A}

	On untyped backends, the manager can compile U as soon as I is
	compiled; U does not wait for A.

	On the TAL backend, the manager can not assemble U before A's TAL
	interface (tali) is ready.  This seems to imply that U must wait
	for A to be compiled.

	We reduce the amount of parallelism for talx86 parallel makes; in
	this example, we have U wait for A to be compiled.

	Eventually, we should change the DAG.  For every node u that
	represents compiling a unit U with an ascribed interface I, add a
	second node ui that represents generating the tali file for U. In
	this example, the node for compiling U would depend on the node ai
	for generating A's tali file rather than the node a for compiling
	A.
*)
structure Master :> MASTER =
struct
    structure I = IntSyn
    structure Set = Name.LabelSet
    type label = Name.label

    val MasterDiag = Stats.ff("MasterDiag")
    fun msg str = if (!MasterDiag) then print str else ()

    val MasterVerbose = Stats.ff("MasterVerbose")
    fun verbose str = if (!MasterVerbose) then print str else ()

    val MasterVVerbose = Stats.ff("MasterVVerbose")
    fun vverbose str = if (!MasterVVerbose) then print str else ()

    val CheckpointVerbose = Stats.tt("CheckpointVerbose")

    fun print_strings (skip : int) (ss : string list) : unit =
	let val spaces = "    "
	    val nspaces = size spaces

	    fun f (str : string,col : int) : int =
		let val cur = nspaces + size str
		in  if (col + cur > !Formatter.Pagewidth)
		    then (print "\n"; print spaces; print str; cur)
		    else (print spaces; print str; col + cur)
		end
	in  ignore(foldl f skip ss)
	end

    val error = fn s => Util.error "master.sml" s
    val reject = Util.reject

    type targets = label list

    type time = Time.time
    type readytime = time
    type workingtime = time * time	(* ready, working *)
    type slavetime = time * time	(* ready, slave *)
    type workingtime' = time * time * time (* ready, slave, working' *)
    type totaltime = time * time * time	(* total, slave, master *)

    datatype status =
	WAITING
      | READY of readytime
      | PENDING of readytime * bool	(* interfaces up to date? *)
      | WORKING of workingtime
      | PROCEEDING of workingtime
      | PENDING' of slavetime
      | WORKING' of workingtime'
      | DONE of totaltime

    fun statusName WAITING = "waiting"
      | statusName (READY _) = "ready"
      | statusName (PENDING (_,interfaces)) =
	    "pending(" ^ Bool.toString interfaces ^ ")"
      | statusName (WORKING _) = "working"
      | statusName (PROCEEDING _) = "proceeding"
      | statusName (WORKING' _) = "working'"
      | statusName (PENDING' _) = "pending'"
      | statusName (DONE _) = "done"

    local
	structure Key =
	struct
	    type hash_key = Name.label
	    val hashVal = HashString.hashString o Name.label2string
	    val sameKey = Name.eq_label
	end
	structure Node = Node(structure Key = Key
			      val toString = Name.label2string)
	structure Graph = LabelGraph(SpeedUpGraph(Graph(Node)))

	type attr = {status : status ref,
		     pdec : I.pdec}

	type graph = attr Graph.graph

	val dummy : label = Name.internal_label "dummy"

	val graph : graph ref = ref (Graph.empty dummy)
	val desc : I.desc ref = ref nil

	fun wrap (f : graph -> 'a -> 'b) : 'a -> 'b = fn x => f (!graph) x

	val lookup : label -> attr = wrap Graph.attribute

	fun get_targets (desc:I.desc, targets:targets) : Set.set =
	    let val labels = map I.P.D.name desc
		val bound = Set.addList(Set.empty,labels)
		fun notbound t = not(Set.member(bound,t))
	    in  (case List.find notbound targets
		   of NONE =>
			if null targets then bound
			else Set.addList(Set.empty,targets)
		    | SOME bad =>
			reject (Name.label2longname bad ^
				" not defined in project"))
	    end

	fun check_file (descfiles:string list, desc:I.desc) (file:string) : unit =
	    let fun bad (why:string) : 'a =
		    reject(concat[file, " conflicts with ",why])
		val file = OS.Path.mkCanonical file
		fun checkDescFile (descfile:string) : unit =
		    let val descfile = OS.Path.mkCanonical descfile
		    in  if descfile = file then
			    bad "project description"
			else ()
		    end
		val _ = app checkDescFile descfiles
		fun checkarc (arc:string) : unit =
		    if arc = I.F.TM then bad "TILT's private files" else ()
		val {arcs,...} = OS.Path.fromString file
		val _ = app checkarc arcs
		fun checksrc (pdec:I.pdec) : unit =
		    (case (I.P.D.src' pdec)
		       of SOME src =>
			    if src = file then
				let val pos = I.P.D.pos pdec
				    val name = I.P.D.name pdec
				in  bad (Name.label2longname name ^ " (" ^
					 Pos.tostring pos ^ ")")
				end
			    else ()
			| NONE => ())
		val _ = app checksrc desc
	    in	()
	    end

    in
	fun set_desc (descfiles:string list, targets:targets,
		      linking:bool, files:string list) : unit =
	    let val _ = Fs.flush()
		val d = Project.empty {linking=linking}
		val d = foldl (fn (descfile,d) => Project.add_include (d, descfile)) d descfiles
		val d = Project.finish d
		val _ = app (check_file(descfiles,d)) files
		val numEntries = length d
		val TiltExn = Name.unit_label "TiltExn"
		val targets = get_targets(d,targets)
		val targets =
		    if linking then Set.add(targets,TiltExn)
		    else targets
		val d = Compiler.gc_desc(d,targets)
		val numTargets = length d
		val g = Graph.empty dummy
		val ready = READY(Time.now())
		fun add_pdec (pdec : I.pdec) : unit =
		    let val l = I.P.D.name pdec
			val f = I.free_pdec pdec
			val status =
			    if Set.isEmpty f then ready else WAITING
			val attr = {status=ref status, pdec=pdec}
			val _ = Graph.insert_node g (l,attr)
			fun addedge (l' : label) : unit =
			    Graph.insert_edge g (l,l')
			val _ = Set.app addedge f
		    in  ()
		    end
		val _ = app add_pdec d
		val numNodes = Graph.nodecount g
		val numEdges = Graph.edgecount g
		val i2s = Int.toString
		val _ =
		    verbose (concat["Project has ",i2s numEntries,
				    " entries (", i2s numTargets,
				    " relevant).\n\
				    \Dependency graph has ",
				    i2s numNodes, " nodes and ",
				    i2s numEdges, " edges.\n"])
	    in  graph := g;
		desc := d
	    end

	fun sort (nodes : label list) : label list =
	    let val set = Set.addList(Set.empty, nodes)
		val order = map I.P.D.name (!desc)
	    in  List.filter (fn l => Set.member (set,l)) order
	    end

	fun get_nodes () : label list =
	    Graph.nodes (!graph)

	val get_pdec : label -> I.pdec = #pdec o lookup

	fun get_status (node : label) : status =
	    !(#status(lookup node))

	fun set_status (node : label, s : status) : unit =
	    (#status(lookup node)) := s

	val get_dependents : label -> label list = wrap Graph.edgesRev

	val get_prerequisites : label -> label list = wrap Graph.edges

	val get_reachable : label -> label list =
	    (wrap Graph.reachable) o get_prerequisites

	fun get_desc () : I.desc = !desc

	fun get_inputs (node:label) : I.desc * I.pdec =
	    let val desc = get_desc ()
		val inputs = Compiler.get_inputs (desc,node)
	    in	inputs
	    end

    end

    fun badStatus (node : label, status : status, what : string) =
	error (Name.label2longname node ^ " was " ^
	       statusName status ^ "; " ^ what)

    fun getReadyTime (node : label) : time =
	(case get_status node
	   of READY t => t
	    | PENDING (t, _) => t
	    | WORKING t => #1 t
	    | PROCEEDING t => #1 t
	    | PENDING' t => #1 t
	    | WORKING' t => #1 t
	    | status => badStatus (node, status, "getting ready time"))

    fun getWorkingTime (node : label) : time =
	(case get_status node
	   of WORKING t => #2 t
	    | PROCEEDING t => #2 t
	    | status => badStatus (node, status, "getting working time"))

    fun getWorking'Time (node : label) : time =
	(case get_status node
	   of WORKING' t => #3 t
	    | status => badStatus (node, status, "getting working' time"))

    fun getTotalTimes (node : label) : totaltime =
	(case get_status node
	   of DONE t => t
	    | status => badStatus (node, status, "getting total times"))

    fun getSlaveTime (node : label) : time =
	(case get_status node
	   of PENDING' t => #2 t
	    | WORKING' t => #2 t
	    | DONE t => #2 t
	    | status => badStatus (node, status, "getting slave time"))

    val getMasterTime : label -> time = #3 o getTotalTimes

    fun markReady (node : label) : unit =
	(case get_status node
	   of WAITING => set_status (node, READY (Time.now()))
	    | READY _ => ()
	    | status => badStatus (node, status, "making ready"))

    (*
	For every a -> b, mark a READY if for every node c reachable
	from a, c has an up to date interface.
    *)
    fun enable (chatty : bool, b : label) : unit =
	let
	    fun hasInterface' (c' : label) : bool =
		(case get_status c'
		   of WAITING => false
		    | READY _ => false
		    | PENDING (_,interfaces) => interfaces
		    | WORKING _ => false
		    | PROCEEDING _ => true
		    | PENDING' _ => true
		    | WORKING' _ => true
		    | DONE _ => true)
	    fun interface (c : label) : label =
		(case I.P.D.asc' (get_pdec c)
		   of SOME I => I
		    | NONE => c)
	    val hasInterface : label -> bool =
		hasInterface' o interface
	    fun now_ready (a : label) : bool =
		(case get_status a
		   of WAITING =>
			let val reachable = get_reachable a
			in  Listops.andfold hasInterface reachable
			end
		    | _ => false)
	    val enabled = List.filter now_ready (get_dependents b)
	    val _ = app markReady enabled
	in  if chatty andalso (!MasterDiag) andalso not (null enabled)
	    then
		(print "  enabled: ";
		 print_strings 10 (map Name.label2longname enabled);
		 print "\n")
	    else ()
	end

    fun markPending (node : label, interfaces : bool) : unit =
	(case get_status node
	   of READY t =>
		let val _ = set_status (node, PENDING (t, interfaces))
		    val _ = if interfaces then enable(true,node) else ()
		in  ()
		end
	    | status => badStatus (node, status, "making pending"))

    fun markWorking (node : label) : unit =
	(case get_status node
	   of PENDING (t, false) =>
		set_status (node, WORKING (t, Time.now()))
	    | status => badStatus (node, status, "making working"))

    fun markProceeding (node : label) : unit =
	let val wt =
		(case get_status node
		   of PENDING (t,true) => (t,Time.now())
		    | WORKING wt => wt
		    | status => badStatus (node, status, "making proceeding"))
	    val _ = set_status (node, PROCEEDING wt)
	    val _ = enable(true,node)
	in  ()
	end

    fun markPending' (node : label) : unit =
	let fun newT (t,t') = (t, Time.-(Time.now(), t'))
	    val status =
		(case get_status node
		   of READY t => PENDING' (t, Time.zeroTime)
		    | WORKING t  => PENDING' (newT t)
		    | PROCEEDING t => PENDING' (newT t)
		    | status => badStatus (node, status, "making pending'"))
	    val _ = set_status (node, status)
	    val _ = enable(true,node)
	in  ()
	end

    fun markWorking' (node : label) : unit =
	(case get_status node
	   of PENDING' (t, t') =>
		set_status (node, WORKING' (t, t', Time.now()))
	    | status => badStatus (node, status, "making working'"))

    fun markDone (node : label) : unit =
	let
	    val now = Time.now()
	    fun since t = Time.- (now, t)
	    val z = Time.zeroTime
	    val times as (totalTime, slaveTime, masterTime) =
		case get_status node
		  of READY t => (since t, z, z)
		   | WORKING (t,t') => (since t, since t', z)
		   | PROCEEDING (t, t') => (since t, since t', z)
		   | WORKING' (t, t', t'') => (since t, t', since t'')
		   | status => badStatus (node, status, "making done")
	    val _ = set_status (node, DONE times)
	    val _ = enable(false,node)
	in  ()
	end

    local
	val workingLocal : (label * (unit -> bool)) list ref =
	    ref nil
	val idleTimes : time list ref =
	    ref nil
	val workingTimes : time list ref =
	    ref nil

	fun addDuration (r : time list ref)
			(_:Comm.identity, duration : time) : unit =
	    r := duration :: (!r)

	val add_idle_time : Comm.identity * time -> unit =
	    addDuration idleTimes
	val add_working_time : Comm.identity * time -> unit =
	    addDuration workingTimes

	datatype slave_status =
	    WORKING_SLAVE of time	(* Slave is working.  Start time. *)
	  | IDLE_SLAVE of time	(* Slave is idle.  Start time. *)

	type slave_info = {status : slave_status ref,
			   in_channel : Comm.in_channel,
			   out_channel : Comm.out_channel}

	structure SlaveMap = SplayMapFn(type ord_key = Comm.identity
					val compare = Comm.compare)

	val slaveMap : slave_info SlaveMap.map ref =
	    ref SlaveMap.empty

	fun resetSlaveMap () : unit = slaveMap := SlaveMap.empty
	fun add_slave (slave : Comm.identity, info : slave_info) : unit =
	    slaveMap := SlaveMap.insert (!slaveMap, slave, info)
	fun lookup_slave (slave : Comm.identity) : slave_info =
	    (case SlaveMap.find (!slaveMap, slave)
	       of NONE => error ("slave " ^ Comm.name slave ^ " missing")
		| SOME info => info)
	fun slave_known (slave : Comm.identity) : bool =
	    isSome (SlaveMap.find (!slaveMap, slave))
	fun list_slaves () : Comm.identity list =
	    map #1 (SlaveMap.listItemsi (!slaveMap))

	fun get_slave_status (slave : Comm.identity) : slave_status =
	    !(#status(lookup_slave slave))
	fun set_slave_status (slave : Comm.identity,
			      status : slave_status) : unit =
	    (#status(lookup_slave slave) := status)
	fun get_in_channel (slave : Comm.identity) : Comm.in_channel =
	    #in_channel(lookup_slave slave)
	fun get_out_channel (slave : Comm.identity) : Comm.out_channel =
	    #out_channel(lookup_slave slave)

	fun markSlaveIdle (slave : Comm.identity) : unit =
	    let val now = Time.now()
		val workingTime = case get_slave_status slave
				    of WORKING_SLAVE t => t
				     | IDLE_SLAVE _ => error ("slave " ^ Comm.name slave ^
							      " idle - making idle")
		val diff = Time.-(now, workingTime)
		val _ = add_working_time (slave, diff)
		val _ = set_slave_status (slave, IDLE_SLAVE now)
	    in  ()
	    end
	fun markSlaveWorking (slave : Comm.identity) : unit =
	    let val now = Time.now()
		val idleTime = case get_slave_status slave
				 of IDLE_SLAVE t => t
				  | WORKING_SLAVE _ => error ("slave " ^ Comm.name slave ^
							      " working - making working")
		val diff = Time.-(now, idleTime)
		val _ = add_idle_time (slave, diff)
		val _ = set_slave_status (slave, WORKING_SLAVE now)
	    in  ()
	    end

	fun partition_slaves (slaves : Comm.identity list) : Comm.identity list * Comm.identity list =
	    let
		fun folder (slave, (w,i)) =
		    (case (get_slave_status slave)
		       of WORKING_SLAVE _ => (slave::w, i)
			| IDLE_SLAVE _ => (w, slave::i))
		val result = foldl folder (nil, nil) slaves
	    in  result
	    end

	fun findNewSlaves () : unit =
	    let
		fun init() : Comm.message =
		    let val desc = get_desc()
		    in  Comm.INIT desc
		    end
		val init = Util.memoize init
		exception Stop
		fun addSlave slave =
		    let val _ = if slave_known slave then raise Stop
				else ()
			val channel = Comm.toMaster slave
			val in_channel = if not (Comm.hasMsg channel) then raise Stop
					 else Comm.openIn channel
			val name = Comm.name slave
			val _ = msg ("[found slave " ^ name ^ "]\n")
			val out_channel = Comm.openOut (Comm.reverse channel)
			val _ = Comm.send (init()) out_channel
		    in
			add_slave (slave, {status = ref (IDLE_SLAVE (Time.now())),
					   in_channel = in_channel,
					   out_channel = out_channel})
		    end handle Stop => ()
	    in  app addSlave (Comm.findSlaves())
	    end
    in
	fun resetSlaves() : unit =
	    let val _ = resetSlaveMap()
		val _ = workingLocal := nil
		val _ = Comm.destroyAllChannels()
	    in  ()
	    end
	(* Find ready slaves. *)
	fun waitForSlaves () : int * int =
	    let
		val maxWorkingLocal = 2
		val newWorkingLocal = List.filter (fn (job, done) =>
						   if done() then (markDone job; false)
						   else true) (!workingLocal)
		val _ = workingLocal := newWorkingLocal
		val _ = findNewSlaves()
		val slaves = list_slaves()
		val (workingSlaves, _) = partition_slaves slaves
		fun update (slave:Comm.identity, meas:Stats.measurements) : unit =
		    let val _ = Stats.add_measurements meas
			val _ = markSlaveIdle slave
		    in  ()
		    end
		
		val _ =
		    app (fn slave =>
			 case Comm.receive (get_in_channel slave)
			   of NONE => ()
			    | SOME Comm.READY => ()
			    | SOME (Comm.ACK_INTERFACE job) =>
(case (get_status job) of
	PROCEEDING _ => ((*print "XXX: duplicate ACK_INTERFACE message\n"*))
|	_ =>
				markProceeding job)
			    | SOME (Comm.ACK_FINISHED (job, meas)) =>
(case (get_status job) of
	DONE _ => ((*print "XXX: duplicate ACK_FINISHED message\n"*))
|	_ =>
			       (markDone job;
				update(slave,meas)))
			    | SOME (Comm.ACK_UNFINISHED (job, meas)) =>
(case (get_status job) of
	PENDING' _ => ((*print "XXX: duplicate ACK_UNFINISHED message\n"*))
|	_ =>
			       (markPending' job;
				update(slave,meas)))
			    | SOME (Comm.ACK_REJECT (job,msg)) =>
			       (print ("slave " ^ Comm.name slave ^
				       " found an error in " ^
				       Name.label2longname job ^ "\n");
				reject msg)
			    | SOME (Comm.BOMB msg) =>
				let val from = "slave " ^ Comm.name slave
				in  Util.error from msg
				end
			    | SOME _ => error ("slave " ^ (Comm.name slave) ^ " sent a bad message"))
		    workingSlaves
		val (_, idleSlaves) = partition_slaves slaves
		val _ = if null idleSlaves then
		            let val delay = if length slaves > 1 then 1.0 else 0.1
			    in  Platform.sleep delay
			    end
			else ()
	    in  (length idleSlaves, maxWorkingLocal - length (!workingLocal))
	    end
	(* Should only be used when we are below our limit on local processes. *)
	fun useLocal (job : label, f : unit -> unit) : unit =
	    let val newLocal = (job, Util.background f)
		val _ = workingLocal := (newLocal :: (!workingLocal))
	    in  ()
	    end
	(* Works only if there are slaves available *)
	fun useSlave (showSlave : string -> unit,
		      msg:Comm.message) : unit =
	    let val slaves = list_slaves ()
		val (workingSlaves, idleSlaves) = partition_slaves slaves
		val (mySlave, idleSlaves') = if null idleSlaves then error ("useSlave when all slaves working")
					     else (hd idleSlaves, tl idleSlaves)
		val _ = (showSlave (Comm.name mySlave);
			 markSlaveWorking mySlave;
			 Comm.send msg (get_out_channel mySlave))
	    in  ()
	    end
	fun closeSlaves () : unit =
	    let
		val slaves = list_slaves()
		val (working, idling) = partition_slaves slaves
		val _ = if null working then ()
			else error ("slaves still working - closeSlaves")

		fun close slave =
		    let val _ = Comm.closeIn (get_in_channel slave)
			val _ = Comm.closeOut (get_out_channel slave)
			val now = Time.now()
			val (IDLE_SLAVE idleTime) = get_slave_status slave
			val diff = Time.-(now, idleTime)
			val _ = add_idle_time (slave, diff)
		    in  ()
		    end
		val _ = app close slaves
		val _ = resetSlaveMap()
	    in  ()
	    end
	fun slaveTimes() : time list * time list = (!workingTimes, !idleTimes)
    end

    (*
	(waiting, pending, working, proceeding, pending', working')
	Units that are ready and done are not included.  Waiting,
	pending, and pending' are kept sorted so that analysis and
	compilation happens in an order determined by the project.
    *)
    type state = label list * label list * label list * label list * label list * label list
    fun stateSize ((a,b,c,d,e,f) : state) = ((length a) + (length b) + (length c) + (length d) +
					     (length e) + (length f))

    fun stateDone(([],[],[],[],[],[]) : state) = true
      | stateDone _ = false
    fun partition (nodes : label list) =
	let
	    fun folder (node,(wa,r,pe,wo,pr,pe',wo',d)) =
		(case (get_status node) of
		     WAITING => (node::wa,r,pe,wo,pr,pe',wo',d)
		   | READY _ => (wa,node::r,pe,wo,pr,pe',wo',d)
		   | PENDING _ => (wa,r,node::pe,wo,pr,pe',wo',d)
		   | WORKING _ => (wa,r,pe,node::wo,pr,pe',wo',d)
		   | PROCEEDING _ => (wa,r,pe,wo,node::pr,pe',wo',d)
		   | PENDING' _ => (wa,r,pe,wo,pr,node::pe',wo',d)
		   | WORKING' _ => (wa,r,pe,wo,pr,pe',node::wo',d)
		   | DONE _ => (wa,r,pe,wo,pr,pe',wo',node::d))
	    val result = foldr folder ([],[],[],[],[],[],[],[]) nodes
	in  result
	end

    fun analyzeReady (node : label) : unit =
	let val inputs = get_inputs node
	    val status = Update.plan inputs
	in  (case status
	       of Update.EMPTY => markDone node
		| Update.COMPILE {pinterface,tali} =>
		    markPending (node, pinterface andalso tali)
		| Update.ASSEMBLE => markPending' node)
	end

    fun getReady (waiting : label list) : label list * label list * label list =
	let
	    fun loop (waiting, pending, pending', done)  =
		let
		    val (waiting, ready, [], [], [], [], [], []) = partition waiting
		    val _ = app analyzeReady ready
		    val ([], [], pending2, [], [], pending'2, [], done2) = partition ready
		    val pending = pending @ pending2
		    val pending' = pending' @ pending'2
		    val done = done @ done2
		in  if (null ready)				 (* no progress *)
			then (waiting, pending, pending', done)
		    else loop (waiting, pending, pending', done) (* some more may have become ready now *)
		end
	    val (waiting, pending, pending', done) = loop (waiting, [], [], [])
	in  (waiting, pending, pending')
	end
    fun newState (waiting, pending, working, proceeding, pending', working') : state =
	let val ([], [], [], [], [], [], working', _) = partition working'
	    val ([], [], [], [], [], pending', newWorking', []) = partition pending'
	    val ([], [], [], [], proceeding, newPending'1, [], _) = partition proceeding
	    val ([], [], [], working, newProceeding1, newPending'2, [], _) = partition working
	    val ([], [], pending, newWorking, newProceeding2, [], [], []) = partition pending
	    val (waiting, newPending, newPending'3) = getReady waiting
	    val working' = working' @ newWorking'
	    val pending' = List.concat [pending', newPending'1, newPending'2, newPending'3]
	    val proceeding = List.concat [proceeding, newProceeding1, newProceeding2]
	    val working = working @ newWorking
	    val pending = pending @ newPending
	in  (waiting, sort pending, working, proceeding, sort pending', working')
	end

    fun finalReport () : unit =
	let val nodes = get_nodes()
	    fun toString r = Real.toString (Real.realFloor (r * 100.0) / 100.0)
	    val _ = print "------- Times to compile files -------\n"
	    fun stats nil = "unavailable"
	      | stats (L : real list) =
		let val v = Vector.fromList L
		in  String.concat ["min ", toString (Util.min v),
				   " max ", toString (Util.max v),
				   " mean ", toString (Util.mean v),
				   " absdev ", toString (Util.absdev v),
				   " (n=" ^ Int.toString (Vector.length v) ^ ")"]
		end
	    fun mapper node =
		let val (total, slave, master) = getTotalTimes node
		    val total' = Time.+ (slave, master)
		    val idle = Time.- (total, total')
		    val total' = Time.toReal total'
		    val idle = Time.toReal idle
		in  if total' > 0.0
		    then SOME (Name.label2name' node, idle, total')
		    else NONE
		end
	    val unsorted = List.mapPartial mapper nodes
	    val (slaveWork, slaveIdle) = slaveTimes()
	    val slaveWork = map Time.toReal slaveWork
	    val slaveIdle = map Time.toReal slaveIdle
	    val _ = print (Int.toString (length unsorted) ^ " of  " ^
			  Int.toString (length nodes) ^ " files needed compilation.\n" ^
			  "  Unit work times (in seconds): " ^ stats (map #3 unsorted) ^ "\n" ^
			  "  Unit wait times (in seconds): " ^ stats (map #2 unsorted) ^ "\n" ^
			  "  Slave work times (in seconds): " ^ stats slaveWork ^ "\n" ^
			  "  Slave idle times (in seconds): " ^ stats slaveIdle ^ "\n")
	    fun greater ((_,_,x),(_,_,y)) = x > (y : real)
	    val sorted = ListMergeSort.sort greater unsorted
	    val (underOne,overOne) = List.partition (fn (_,_,t) => t < 1.0) sorted
	    val (oneToTen, overTen) = List.partition (fn (_,_,t) => t < 10.0) overOne
	    val (tenToThirty, overThirty) = List.partition (fn (_,_,t) => t < 30.0) overTen
	    val _ = (print (Int.toString (length underOne));
		     print " files under 1.0 second.\n")
	    val _ = (print (Int.toString (length oneToTen));
		     print " files from 1.0 to 10.0 seconds.\n")
	    val _ = (print (Int.toString (length tenToThirty));
		     print " files from 10.0 to 30.0 seconds:  ";
		     print_strings 40 (map #1 tenToThirty); print "\n")
	    val _ = (print (Int.toString (length overThirty));
		     print " files over 30.0 seconds:\n")
	    val _ = app (fn (id,idle,total) =>
			 print ("  " ^ id ^
			       (Util.spaces (20 - (size id))) ^ " took " ^
			       (toString total) ^ " seconds of work and waited for " ^
			       (toString idle) ^ " seconds.\n")) overThirty
	in  ()
	end

    fun useLocals (localsLeft : int, state : state) : int =
	let
	    fun useOne (node : label) =
		let val _ = markWorking' node
		in  useLocal (node, fn () => Compiler.assemble (get_inputs node))
		end
	    fun useSome (0, _) = 0
	      | useSome (n, nil) = n
	      | useSome (n, unit :: rest) = (useOne unit; useSome (n-1, rest))
	    val (_, _, _, _, pending', _) = state
	in  useSome (localsLeft, pending')
	end

    fun useSlaves (slavesLeft : int, state : state) : int =
	let
	    fun useOne node =
		let fun showSlave (name : string) : unit =
			vverbose ("[sending " ^ Name.label2longname node ^
				  " to " ^ name ^ "]\n")
		    val _ =
			(case get_status node
			   of PENDING (_,false) => markWorking node
			    | PENDING (_,true) => markProceeding node
			    | status => badStatus (node, status, "compiling"))
		in  useSlave (showSlave,Comm.COMPILE node)
		end
	    fun useSome (0, _) = 0
	      | useSome (n, nil) = n
	      | useSome (n, unit :: rest) = (useOne unit; useSome (n-1, rest))
	    val (_, pending, _, _, _, _) = state
	in  useSome (slavesLeft, pending)
	end

    local
	val start = ref (NONE : Time.time option)
	val msgs = ref ([] : string list)

	fun showTime (dateStamp : bool, str : string) : unit =
	    let val cur = Time.now()
		val curString = if (dateStamp)
				    then let
					     val temp = (Date.fromTimeLocal cur)
					     val res = Date.toString temp
					 in  res ^ "   "
					 end
				else ""
		val diff = Time.-(cur, (case !start of
					    NONE => error "no start time"
					  | SOME t => t))
		val diff = Time.toReal diff
		val diff = (Real.realFloor(diff * 100.0)) / 100.0
		val msg = concat ["[", str, ": ", curString,
				  Real.toString diff, " sec]\n"]
	    in	msgs := msg :: (!msgs);
		print msg
	    end

	fun startTime (str : string) : unit =
	    (msgs := [];
	     start := SOME(Time.now());
	     showTime (true,str))

	fun reshowTimes () : unit =
	    (print "\n\n";
	     app print (rev (!msgs));
	     msgs := []; start := NONE)

	fun checkpoint (state : state) : unit =
	    let val left = stateSize state
		val msg =
		    if (!CheckpointVerbose) then
			let val (waiting, pending, working, proceeding, pending', working') = state
			    val waiting = length waiting
			    val pending = length pending
			    val working = length working
			    val proceeding = length proceeding
			    val pending' = length pending'
			    val working' = length working'
			in
			    String.concat [Int.toString waiting, " waiting + ",
					   Int.toString pending, " pending + ",
					   Int.toString working, " working + ",
					   Int.toString proceeding, " proceeding + ",
					   Int.toString pending', " pending' + ",
					   Int.toString working', " working' = ",
					   Int.toString left, " files left"]
			end
		    else (Int.toString left ^ " files left")
	    in	showTime (false, msg)
	    end
	fun wrap (f : 'a -> unit) (x : 'a) : unit =
	    if !MasterDiag then f x else ()
    in
	val showTime = wrap showTime
	val startTime = wrap startTime
	val reshowTimes = wrap reshowTimes
	val checkpoint = wrap checkpoint
    end

    fun compile_loop (state : state, lastCpTime : Time.time) : unit =
	let val (slavesLeft, localsLeft) = waitForSlaves()
	    val state = newState state
	    val localsLeft = useLocals (localsLeft, state)
	    val state = newState state
	    val slavesLeft = useSlaves (slavesLeft, state)
	    val state = newState state
	    val time = Time.now()
	    val diff = Time.toReal(Time.-(time,lastCpTime))
	    val lastCpTime =
		if diff > 30.0 then
		    (checkpoint state; time)
		else lastCpTime
	in
	    if stateDone state then ()
	    else compile_loop (state,lastCpTime)
	end

    fun compile () : unit =
	let val _ = if not (Target.native()) then
			(print "Warning: can not compile to binary on this system\n";
			 Compiler.UptoAsm := true)
		    else ()
	    val _ = resetSlaves()
	    val _ = startTime ("started compiling")
	    val nodes = get_nodes ()
	    val nodes = sort nodes	(* make compiler deterministic *)
	    val state : state = (nodes, [], [], [], [], [])
	    val _ = compile_loop (state, Time.now())
	    val _ = showTime (true,"finished compiling")
	    val _ = closeSlaves()
	    val _ = if !MasterVVerbose
		    then (finalReport ();
			  reshowTimes())
		    else ()
	in  ()
	end

    fun make (projects : string list, targets : targets) : unit =
	let val _ = set_desc(projects,targets,false,nil)
	    val _ = compile ()
	in  ()
	end

    fun check_dir (file:string) : unit =
	let val dir = OS.Path.dir file
	in  if dir = "" orelse Fs.exists dir then ()
	    else reject (dir ^ ": does not exist")
	end

    fun make_exe (projects : string list, exe : string,
		  targets : targets) : unit =
	let val _ = check_dir exe
	    val _ = set_desc(projects,targets,true,[exe])
	    val desc = get_desc()
	    fun mapper (pdec : I.pdec) : label option =
		(case pdec
		   of I.SCDEC {name=U,...} => SOME U
		    | _ => NONE)
	    val sc = List.mapPartial mapper desc
	    val _ = compile()
	in  if null sc then
		Compiler.link(desc,exe)
	    else
		(print ("can not link " ^ exe ^
			" because of unimplemented units: ");
		 print_strings 70 (map Name.label2name' sc);
		 print "\n";
		 reject "unable to link")
	end

    fun make_lib (projects : string list, lib : string,
		  targets : targets) : unit =
	let val _ = check_dir lib
	    val _ = set_desc(projects,targets,false,[lib])
	    val desc = get_desc()
	    fun mapper (pdec:I.pdec) : label option =
		(case pdec
		   of I.UDEC {name=U,uexp=I.SRCU _,...} => SOME U
		    | _ => NONE)
	    val inferred = List.mapPartial mapper desc
	    val _ = compile()
	in
	    if null inferred then
		Compiler.pack(desc,lib)
	    else
		(print ("can not pack " ^ lib ^
			" because of units without interfaces: ");
		 print_strings 70 (map Name.label2name' inferred);
		 print "\n";
		 reject "unable to pack")
	end

    local
	type droppings =
	    (I.iexp -> string option) list *
	    (I.uexp -> string option) list *
	    (I.scdec -> string) list

	val empty : droppings = (nil, nil, nil)

	fun append (a:droppings, b:droppings) : droppings =
	    let val (ia,ua,sa) = a
		val (ib,ub,sb) = b
	    in	(ia@ib, ua@ub, sa@sb)
	    end

	val concat : droppings list -> droppings =
	    foldl append empty

	fun purge_help (what : string, droppings : droppings)
		       (projects : string list, targets : targets) : unit =
	    let val _ = set_desc(projects,targets,false,nil)
		val desc = get_desc()
		val (iFiles,uFiles,scFiles) = droppings
		val _ = if !Compiler.CompilerDiag then
			    print ("Purging " ^ what ^ ".\n")
			else ()
		fun rm (fs:('a -> string option) list) (x:'a) : unit =
		    let val files = List.mapPartial (fn f => f x) fs
		    in  app Fs.remove files
		    end
		fun rm' (fs:('a -> string) list) (x:'a) : unit =
		    let val files = map (fn f => f x) fs
		    in  app Fs.remove files
		    end
		val rmi = rm iFiles
		val rmu = rm uFiles
		val rmsc = rm' scFiles
		fun remove (pdec:I.pdec) : unit =
		    if I.P.D.stable pdec then ()
		    else
			(case pdec
			   of I.IDEC {iexp,...} => rmi iexp
			    | I.SCDEC scdec => rmsc scdec
			    | I.UDEC {uexp,...} => rmu uexp)
	    in	app remove desc
	    end

	val meta : droppings =
	    ([I.P.I.info'],
	     [I.P.U.info'],
	     [#info])
	val iface : droppings =
	    ([SOME o I.P.I.pinterface],
	     [I.P.U.pinterface'],
	     nil)
	val obj : droppings =
	    (nil,
	     [SOME o I.P.U.obj,
	      I.P.U.asm',
	      I.P.U.asmz',
	      SOME o I.P.U.using_file,
	      SOME o I.P.U.tali],
	     [#tali])
    in
	val purge : string list * targets -> unit =
	    purge_help ("binaries", obj)

	val purgeAll : string list * targets -> unit =
	    purge_help ("binaries and interfaces",
			concat [meta,iface,obj])
    end
end
