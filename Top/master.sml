(*
	The master builds a dependency graph with a node for each
	unit, interface, and command in the group and an arrow a -> b
	when a depends on b; that is, whenever b must be up to date
	before a can be compiled.  The master uses this graph to bring
	the units and interfaces needed by group commands up to date.

	The master sets up by deleting all channels and then takes
	master steps which are:

	While there are available jobs
		Look for available slave channels and process their
		acknowledgements by marking compilation jobs as done.
		Ready messages require no action.

		Issue available jobs to available slaves.

	XXX: Actual dependencies should be used to build elaboration
	contexts; we probably want to build a dependency graph for this.

	XXX: We should check unit environments when interfaces are
	combined into a context.

	XXX: Add reasonable group file variables.
*)
functor Master (val basis :
		    {group : unit -> string,
		     fixup : ExtSyn.groupfile -> ExtSyn.groupfile} option)
    :> MASTER =
struct
    structure E = ExtSyn
    structure Ue = UnitEnvironment
    structure FileCache = Compiler.FileCache
    structure VarMap = Name.VarMap
    structure StringMap = Util.StringMap
    structure StringOrderedSet = Util.StringOrderedSet
    structure VarSet = Name.VarSet

    val MasterDiag = Stats.ff("MasterDiag")
    fun msg str = if (!MasterDiag) then print str else ()

    fun print_strings (skip : int) (ss : string list) : unit =
	let
	    fun f (str : string,col : int) : int =
		let val cur = 2 + size str
		in  if (col + cur > !Formatter.Pagewidth)
		    then (print "\n        "; print str; 6 + cur)
		    else (print "  "; print str; col + cur)
		end
	in  ignore(foldl f skip ss)
	end

    val Checkpoint = Stats.ff "Checkpoint"
    val CheckpointVerbose = Stats.ff "CheckpointVerbose"
    val ShowEnable = Stats.ff "ShowEnable"
    val ShowUptodate = Stats.ff "ShowUptodate"
    val ShowFinalReport = Stats.ff "ShowFinalReport"

    val error = fn s => Util.error "master.sml" s
    val reject = fn s => raise (Compiler.Reject s)

    (*
	WAITING: prerequisite interfaces not up to date
	READY: waiting for planning
	PENDING: out of date, waiting for compilation
	WORKING: out of date, slave working
	PROCEEDING: interface up to date, slave working
	PENDING': asm up to date, waiting for assembly
	WORKING': assembling
	DONE: up to date

	The status transition graphs are status.dot (for units),
	status-iface.dot (for interfaces), and status-cmd.dot (for
	commands).  We measure the following times:

	total time:  time to go from READY to DONE
	slave time:  time spent in WORKING and PROCEEDING
	master time: time spent in WORKING'

     *)
    type time = Time.time
    type plan = Update.plan
    type readytime = time
    type workingtime = time * time	(* ready, working *)
    type slavetime = time * time	(* ready, slave *)
    type workingtime' = time * time * time (* ready, slave, working' *)
    type totaltime = time * time * time	(* total, slave, master *)

    datatype status =
	WAITING
      | READY of readytime
      | PENDING of readytime * plan
      | WORKING of workingtime * plan
      | PROCEEDING of workingtime * plan
      | PENDING' of slavetime * plan
      | WORKING' of workingtime' * plan
      | DONE of totaltime

    type iface = Paths.iface
    type compunit = Paths.compunit
    type var = Name.var

    datatype node =
	SRCI of Paths.iface * Update.imports
      | COMPI of Paths.iface
      | SRCU of Paths.compunit * var option * Update.imports
      | COMPU of Paths.compunit
      | PRIMU of Paths.compunit * Update.imports
      | IMPORTU of Paths.compunit
      | CHECKU of Paths.compunit * Paths.iface
      | LINK of Paths.exe
      | PACK of Paths.lib * VarSet.set * (E.id * E.exp) list
      | INITIAL

    local
	structure VarNode :> NODE where type node = var =
	struct
	    structure HashKey =
	    struct
		type hash_key = Name.var
		val hashVal = Word.fromInt
		val sameKey = (op= : var * var -> bool)
	    end
	    structure HashTable = HashTableFn (HashKey)
	    type node = var
	    open HashTable
	    exception HashUnknownNode
	    fun make i = HashTable.mkTable(i,HashUnknownNode)
	end
	structure VarOrderedSet = OrderedSet(VarSet)

	structure Dag :> DAG where type node = var =
	    Dag (structure Node = VarNode
		 val dummy = Name.fresh_var()
		 val toString = Name.var2string
		 structure Map = VarMap
		 structure OrderedSet = VarOrderedSet)

	structure Equiv :> EQUIV where type elem = Crc.crc =
	struct
	    structure StringEquiv = Equiv(StringMap)
	    type elem = Crc.crc
	    type equiv = StringEquiv.equiv
	    val empty = StringEquiv.empty
	    fun insert (e,a,b) =
		StringEquiv.insert (e, Crc.toString a, Crc.toString b)
	    fun equiv e =
		let val eq = StringEquiv.equiv e
		in  fn (a,b) => eq (Crc.toString a, Crc.toString b)
		end
	end

	type attr = {status : status ref,
		     node : node}

	type graph = attr Dag.graph

	val equiv : Equiv.equiv ref = ref (Equiv.empty)
	val graph : graph ref = ref (Dag.empty())
	val order : var list ref = ref nil	(* backwards *)
	val pos_ref : (var -> Group.pos) option ref = ref NONE

	fun wrap (f : graph * 'a -> 'b) (arg : 'a) : 'b =
	    (f (!graph,arg) handle Dag.UnknownNode node =>
		error ("node " ^ Name.var2string node ^ " missing"))

	val lookup_node : var -> attr = wrap Dag.nodeAttribute
    in
	fun reset_graph() : unit =
	    (equiv := Equiv.empty;
	     graph := Dag.empty();
	     pos_ref := NONE)

	fun add_eq (crc : Crc.crc, crc' : Crc.crc) : unit =
	    equiv := Equiv.insert (!equiv, crc, crc')

	fun eq (crcs : Crc.crc * Crc.crc) : bool =
	    Equiv.equiv (!equiv) crcs

	fun set_pos (f : var -> Group.pos) : unit = pos_ref := SOME f
	fun get_pos (node : var) : Group.pos = valOf (!pos_ref) node

	fun add_node(v : var, n : node) : unit =
	    let val attr = {status=ref WAITING, node=n}
		val _ = order := (v :: (!order))
	    in  Dag.insert_node(!graph,v,attr)
	    end

	fun add_edge(src : var) (dest : var) : unit =
	    Dag.insert_edge(!graph,src,dest)

	val get_node : var -> node = #node o lookup_node

	fun get_status (node : var) : status = !(#status(lookup_node node))
	fun set_status (node : var, s : status) : unit =
	    (#status(lookup_node node)) := s

	val get_dependent : var -> var list = wrap Dag.parents
	val get_import_direct : var -> var list = wrap Dag.children
	val get_import_transitive : var -> var list =
	    rev o (wrap Dag.descendants)
	fun graphSize () : int * int =
	    (Dag.numNodes (!graph), Dag.numEdges (!graph))
	fun sort (nodes : var list) : var list =
	    let val set = VarSet.addList(VarSet.empty, nodes)
		val order = rev (!order)
	    in  List.filter (fn v => VarSet.member (set,v)) order
	    end
    end

    fun get_iface_paths' (iface : var) : Paths.iface option =
	(case get_node iface
	   of SRCI (p,_) => SOME p
	    | COMPI p => SOME p
	    | _ => NONE)

    fun get_iface_paths (iface : var) : Paths.iface =
	(case get_iface_paths' iface
	   of SOME p => p
	    | NONE => error "get_iface_paths given non-interface")

    fun get_unit_paths (unit : var) : Paths.compunit option =
	(case get_node unit
	   of SRCU (p,_,_) => SOME p
	    | COMPU p => SOME p
	    | PRIMU (p,_) => SOME p
	    | IMPORTU p => SOME p
	    | CHECKU (p,_) => SOME p
	    | _ => NONE)

    fun get_id (v : var) : string =
	(case get_node v
	   of SRCI (p,_) => "*" ^ Paths.ifaceName p
	    | COMPI p => "*" ^ Paths.ifaceName p
	    | SRCU (p,_,_) => Paths.unitName p
	    | COMPU p => Paths.unitName p
	    | PRIMU (p,_) => Paths.unitName p
	    | IMPORTU p => Paths.unitName p
	    | CHECKU (p,_) => "?" ^ Paths.unitName p
	    | LINK p => Paths.exeFile p
	    | PACK (p,_,_) => Paths.libDir p
	    | INITIAL => "end of compilation")

    val nofixup : E.groupfile -> E.groupfile =
	fn g => g

    fun read_group (groupfile : string) : Group.group =
	let val _ = Update.flushAll()
	    (* Eg: target=sparc-8, objtype=sparc, cputype=linux *)
	    val cputype = Platform.platformName (Platform.platform ())
	    val objtype = Target.platformName (Target.getTargetPlatform ())
	    val target = Target.platformString()
	    val littleEndian = !Target.littleEndian
	    val g = Group.empty_group'
	    val g = Group.add_string_value (g, "cputype", cputype)
	    val g = Group.add_string_value (g, "objtype", objtype)
	    val g = Group.add_string_value (g, "target", target)
	    val g = Group.add_bool_value (g, "littleEndian", littleEndian)
	    val g = Group.add_int_value (g, "majorVersion", Version.majorVersion)
	    val g = Group.add_int_value (g, "minorVersion", Version.minorVersion)
	    val g = Group.add_string_value (g, "version", Version.version)
	    val (g,fixup) =
		(case basis
		   of NONE => (g,nofixup)
		    | SOME {group,fixup} =>
			(Group.read (g,group(),nofixup),fixup))
	    val g = Group.read (g, groupfile, fixup)
	    val group = Group.get_group g
	    val _ = msg ("Group has " ^
			 Int.toString (Group.size group) ^
			 " entries.\n")
	in  group
	end

    fun make_imports (imports : var list) : Update.imports =
	let fun unitname (v : var) : string =
		(case get_unit_paths v
		   of SOME p => Paths.unitName p
		    | NONE => error "make_imports given non-unit")
	in  map unitname imports
	end

    fun make_transitive_imports (imports : VarSet.set) : var list =
	let fun add (v,s) = VarSet.addList (s,get_import_transitive v)
	    val set = VarSet.foldl add imports imports
	in  VarSet.listItems set
	end

    fun graphInfo (entry : Group.entry) : node * var list =
	(case entry
	   of Group.IFACE iface =>
		(case iface
		   of Group.SRCI (id,group,file,imports) =>
			let val p = Paths.srci {id=id, group=group, file=file}
			    val i = make_imports imports
			in  (SRCI (p,i), imports)
			end
		    | Group.COMPI (id,iface,ue,parms) =>
			(COMPI (Paths.compi {id=id, file=iface, uefile=ue}),
			 VarSet.listItems parms))
	    | Group.UNIT unit =>
		(case unit
		   of Group.SRCU (id,group,src,imports,iface) =>
			let val p' = Option.map get_iface_paths iface
			    val p = Paths.srcu {id=id,group=group,file=src,
						iface=p'}
			    val i = make_imports imports
			    val edges = (case iface
					   of NONE => imports
					    | SOME I => I :: imports)
			in  (SRCU (p,iface,i), edges)
			end
		    | Group.COMPU (id,obj,ue,parms,iface) =>
			let val p' = get_iface_paths iface
			    val p = Paths.compu {id=id,file=obj,
						 uefile=ue,iface=p'}
			in  (COMPU p, iface :: (VarSet.listItems parms))
			end
		    | Group.PRIMU (id,group,imports) =>
			let val p = Paths.primu {id=id, group=group}
			    val i = make_imports imports
			in  (PRIMU (p,i), imports)
			end
		    | Group.IMPORTU (id,iface) =>
			let val p' = get_iface_paths iface
			    val p = Paths.importu {id=id, iface=p'}
			in  (IMPORTU p, [iface])
			end
		    | Group.CHECKU {U,I} =>
			let val pU = valOf (get_unit_paths U)
			    val pI = get_iface_paths I
			in  (CHECKU (pU,pI), [U,I])
			end)
	    | Group.CMD cmd =>
		(case cmd
		   of Group.LINK (group,exe,targets) =>
			(LINK (Paths.exe {group=group, exe=exe}),
			 make_transitive_imports targets)
		    | Group.PACK {lib, imports, exports, values} =>
			(PACK (Paths.lib {dir=lib}, imports, values),
			 make_transitive_imports exports)))

    fun read_graph (groupfile : string) : var =
	let val group = read_group groupfile
	    val {entries, pos} = Group.list_entries group
	    val _ = reset_graph()
	    val _ = set_pos pos
	    val init = Name.fresh_var()
	    val _ = add_node(init,INITIAL)
	    fun add_edges (v : var, edges : var list) : unit =
		if null edges then set_status (v,READY (Time.now()))
		else app (add_edge v) edges
	    val initial : var -> unit = add_edge init
	    fun cmd_entry (e : Group.entry) : bool =
		(case e of Group.CMD _ => true | _ => false)
	    fun add_entry (v : var, entry : Group.entry) : unit =
		let val (node,edges) = graphInfo entry
		    val _ = add_node (v,node)
		    val _ = if cmd_entry entry then initial v else ()
		    val _ = add_edges(v,edges)
		in  ()
		end
	    val _ = app add_entry entries
	    val (numNodes, numEdges) = graphSize()
	    val _ = msg ("Dependency graph has " ^
			 Int.toString (numNodes) ^ " nodes and " ^
			 Int.toString (numEdges) ^ " edges.\n")
	in  init
	end

    local
	val workingLocal : (Comm.job * (unit -> bool)) list ref =
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
		val platform = Target.getTargetPlatform()
		val flags = Comm.getFlags()
		val flushAll = Comm.FLUSH_ALL (platform, flags)

		exception Stop
		fun addSlave slave =
		    let val _ = if slave_known slave then raise Stop
				else ()
			val channel = Comm.toMaster slave
			val in_channel = if not (Comm.hasMsg channel) then raise Stop
					 else Comm.openIn channel
			val name = Comm.name slave
			val _ = msg ("Noticed slave " ^ name ^ "\n")
			val out_channel = Comm.openOut (Comm.reverse channel)
			val _ = Comm.send flushAll out_channel
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
	fun pollForSlaves (do_ack_interface : string * Comm.job -> unit,
			   do_ack_done : string * Comm.job * Update.plan -> unit,
			   do_ack_local : Comm.job -> unit) : int * int =
	    let
		val maxWorkingLocal = 2
		val newWorkingLocal = List.filter (fn (job, done) =>
						   if done() then (do_ack_local job; false)
						   else true) (!workingLocal)
		val _ = workingLocal := newWorkingLocal
		val _ = findNewSlaves()
		val slaves = list_slaves()
		val (workingSlaves, _) = partition_slaves slaves
		val _ =
		    app (fn slave =>
			 case Comm.receive (get_in_channel slave)
			   of NONE => ()
			    | SOME Comm.READY => ()
			    | SOME (Comm.ACK_INTERFACE job) => do_ack_interface (Comm.name slave, job)
			    | SOME (Comm.ACK_DONE (job, plan)) =>
			       (do_ack_done (Comm.name slave, job, plan);
				markSlaveIdle slave)
			    | SOME (Comm.ACK_ERROR (job,msg)) =>
			       (print ("slave " ^ Comm.name slave ^
				       " signalled an error during job " ^
				       (#2 job) ^ "\n");
				reject msg)
			    | SOME (Comm.FLUSH_ALL _) => error ("slave " ^ (Comm.name slave) ^ " sent flushAll")
			    | SOME (Comm.FLUSH _) => error ("slave " ^ (Comm.name slave) ^ " sent flush")
			    | SOME (Comm.REQUEST _) => error ("slave " ^ (Comm.name slave) ^ " sent request"))
		    workingSlaves
		val (_, idleSlaves) = partition_slaves slaves
	    in  (length idleSlaves, maxWorkingLocal - length (!workingLocal))
	    end
	(* Should only be used when we are below our limit on local processes. *)
	fun useLocal (job : Comm.job, f : unit -> unit) : unit =
	    let val newLocal = (job, Background.background f)
		val _ = workingLocal := (newLocal :: (!workingLocal))
	    in  ()
	    end
	(* Works only if there are slaves available *)
	fun useSlave (showSlave : string -> unit,
		      forMySlave : Comm.message,
		      forOtherSlaves : Comm.message option) : unit =
	    let val slaves = list_slaves ()
		val (workingSlaves, idleSlaves) = partition_slaves slaves
		val (mySlave, idleSlaves') = if null idleSlaves then error ("useSlave when all slaves working")
					     else (hd idleSlaves, tl idleSlaves)
		val _ = (showSlave (Comm.name mySlave);
			 markSlaveWorking mySlave;
			 Comm.send forMySlave (get_out_channel mySlave))
		val _ = case forOtherSlaves
			  of NONE => ()
			   | SOME msg =>
			      let val sendMsg = Comm.send msg
				  val sendTo = sendMsg o get_out_channel
				  val otherSlaves = workingSlaves @ idleSlaves'
			      in  app sendTo otherSlaves
			      end
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

    fun statusName WAITING = "waiting"
      | statusName (READY _) = "ready"
      | statusName (PENDING _) = "pending"
      | statusName (WORKING _) = "working"
      | statusName (PROCEEDING _) = "proceeding"
      | statusName (WORKING' _) = "working'"
      | statusName (PENDING' _) = "pending'"
      | statusName (DONE _) = "done"

    fun badStatus (node : var, status : status, what : string) =
	error (get_id node ^ " was " ^
	       statusName status ^ "; " ^ what)

    fun getReadyTime (node : var) : time =
	(case get_status node
	   of READY t => t
	    | PENDING (t, _) => t
	    | WORKING (t, _) => #1 t
	    | PROCEEDING (t, _) => #1 t
	    | PENDING' (t, _) => #1 t
	    | WORKING' (t, _) => #1 t
	    | status => badStatus (node, status, "getting ready time"))

    fun getWorkingTime (node : var) : time =
	(case get_status node
	   of WORKING (t, _) => #2 t
	    | PROCEEDING (t, _) => #2 t
	    | status => badStatus (node, status, "getting working time"))

    fun getWorking'Time (node : var) : time =
	(case get_status node
	   of WORKING' (t, _) => #3 t
	    | status => badStatus (node, status, "getting working' time"))

    fun getTotalTimes (node : var) : totaltime =
	(case get_status node
	   of DONE t => t
	    | status => badStatus (node, status, "getting total times"))

    fun getSlaveTime (node : var) : time =
	(case get_status node
	   of PENDING' (t, _) => #2 t
	    | WORKING' (t, _) => #2 t
	    | DONE t => #2 t
	    | status => badStatus (node, status, "getting slave time"))

    val getMasterTime : var -> time = #3 o getTotalTimes

    fun getPlan (node : var) : Update.plan =
	(case get_status node
	   of PENDING (_, plan) => plan
	    | WORKING (_, plan) => plan
	    | PROCEEDING (_, plan) => plan
	    | PENDING' (_, plan) => plan
	    | WORKING' (_, plan) => plan
	    | status => badStatus (node, status, "getting plan"))

    fun markReady (node : var) : unit =
	(case get_status node
	   of WAITING => set_status (node, READY (Time.now()))
	    | READY _ => ()
	    | status => badStatus (node, status, "making ready"))

    fun markPending (node : var, plan : Update.plan) : unit =
	(case get_status node
	   of READY t => set_status (node, PENDING (t, plan))
	    | status => badStatus (node, status, "making pending"))

    fun markWorking (node : var) : unit =
	(case get_status node
	   of PENDING (t, plan) =>
		set_status (node, WORKING ((t, Time.now()), plan))
	    | status => badStatus (node, status, "making working"))

    fun markWorking' (node : var) : unit =
	(case get_status node
	   of PENDING' ((t, t'), plan) =>
		set_status (node, WORKING' ((t, t', Time.now()), plan))
	    | status => badStatus (node, status, "making working'"))

    (*
	Find every ready node a such that a -> b.  An interface or
	unit node a is ready if for every node c with a -> c, c has an
	up to date interface.  A LINK, PACK, or INITIAL node a is
	ready if for every node c with a -> c, c is done.
    *)
    fun enable (b : var) : unit =
	let
	    fun hasInterface (c : var) : bool =
	        (case get_status c
		   of PENDING (_, p) => Update.isReady p
		    | WORKING (_, p) => Update.isReady p
		    | PROCEEDING _ => true
		    | PENDING' _ => true
		    | WORKING' _ => true
		    | DONE _ => true
		    | _ => false)
	    fun isDone (c : var) : bool =
		(case get_status c
		   of DONE _ => true
		    | _ => false)
	    fun isReady (a : var) : bool =
		let val imports = get_import_direct a
		    val predicate =
			(case get_node a
			   of LINK _ => isDone
			    | PACK _ => isDone
			    | INITIAL => isDone
			    | _ => hasInterface)
		in  Listops.andfold predicate imports
		end
	    fun enableReady (a : var) : bool =
		(isReady a andalso
		 (case get_status a
		    of WAITING => (markReady a; true)
		     | _ => false)) (* eg, second enable call *)
	    val enabled = List.filter enableReady (get_dependent b)
	in  if (!ShowEnable andalso (not (null enabled)))
	    then
		(print (get_id b ^ " enabled: ");
		 print_strings 20 (map get_id enabled);
		 print "\n")
	    else ()
	end

    fun markPending' (node : var, plan : Update.plan) : unit =
	let fun newT (t,t') = (t, Time.-(Time.now(), t'))
	    val status =
		(case get_status node
		   of READY t =>PENDING' ((t, Time.zeroTime), plan)
		    | WORKING (t, _) => PENDING' (newT t, plan)
		    | PROCEEDING (t, _) => PENDING' (newT t, plan)
		    | status => badStatus (node, status, "making pending'"))
	    val _ = set_status (node, status)
	    val _ = if Update.isReady plan then
			enable node
		    else ()
	in  ()
	end

    fun markProceeding (node : var) : unit =
	let val _ = case get_status node
		      of WORKING arg => set_status (node, PROCEEDING arg)
		       | status => badStatus (node, status, "making proceeding")
	    val _ = enable node
	in  ()
	end

    fun markDone (node : var) : unit =
	let
	    val now = Time.now()
	    fun since t = Time.- (now, t)
	    val z = Time.zeroTime
	    val (times as (totalTime, slaveTime, masterTime)) =
		case get_status node
		  of READY t => (since t, z, z)
		   | WORKING ((t,t'), _) => (since t, since t', z)
		   | PROCEEDING ((t, t'), _) => (since t, since t', z)
		   | WORKING' ((t, t', t''), _) => (since t, t', since t'')
		   | status => badStatus (node, status, "making done")
	    val _ = set_status (node, DONE times)
	    val _ = (case get_node node
		       of CHECKU (U,I) =>
			    let val I = Paths.ifaceFile I
				val U = Paths.ifaceFile (Paths.unitIface U)
			    in  add_eq(FileCache.crc I, FileCache.crc U)
			    end
			| _ => ())
	    val _ = enable node
	in  ()
	end

    (*
	    Invariant: If v in rng(unitmap), then (get_node v) is one
	    of SRCU, COMPU, PRIMU, or IMPORTU; that is, v does not
	    correspond to a CHECKU node.

	    The main dependency graph may have several nodes with the
	    same unit id U, but only because of chains of the form

		    CHECKU -> ...  -> CHECKU ->	node

	    where node is a non-CHECKU unit node.  The invariant
	    assures us that if U in dom(unitmap), then unitmap(U) is
	    the variable for node.
    *)
    fun unit_help (root : var) : Update.unit_help =
	let val imports = get_import_transitive root
	    fun folder (v : var, map) : var StringMap.map =
		(case get_unit_paths v
		   of NONE => map
		    | SOME p => StringMap.insert (map, Paths.unitName p, v))
	    (* Right-to-left establishes the invariant. *)
	    val unitmap = foldr folder StringMap.empty imports
	    fun lookup (U : E.id) : var =
		(case StringMap.find (unitmap, U)
		   of SOME v => v
		    | NONE => error ("unit " ^ U ^ " not in unitmap"))
	    fun readparms (uefile : string) : var list =
		let val ue = FileCache.read_ue uefile
		    val ids = map #1 (Ue.listItemsi ue)
		in  map lookup ids
		end
	in  {parms=readparms, get_id=get_id, get_unit_paths=get_unit_paths}
	end

    (*
	We need fresh interface identifiers because every unit in a
	packed library has an explicit interface and the external
	syntax requires that every interface be named.  get_id has to
	know about these fresh identifiers for debugging messages;
	that is the only reason we need the second componenent of the
	isomorphism.  XXX: We should probably cleanup the external
	syntax to eliminate the need for all this.
    *)
    fun iface_help (root : var) : Update.iface_help =
	let val imports = get_import_transitive root
	    type iso = var StringMap.map * string VarMap.map
	    val empty : iso = (StringMap.empty, VarMap.empty)
	    fun insert ((sm,vm) : iso, v : var, n : string) : iso =
		(StringMap.insert (sm, n, v), VarMap.insert (vm, v, n))
	    fun folder (v : var, iso : iso) : iso =
		(case get_iface_paths' v
		   of NONE => iso
		    | SOME p => insert (iso,v,Paths.ifaceName p))
	    val iso = foldl folder empty imports
	    val r = ref iso
	    fun find (I : E.id) : var =
		(case StringMap.find (#1(!r), I)
		   of SOME v => v
		    | NONE => error ("interface " ^ I ^ " found"))
	    fun fresh' (sm : var StringMap.map, s : string) : string =
		if isSome (StringMap.find (sm, s)) then fresh' (sm, s ^ "'")
		else s
	    fun fresh (s : string) : string =
		let val iso = !r
		    val n = fresh' (#1 iso,s)
		    val _ = r := insert (iso,Name.fresh_var(),n)
		in  n
		end
	    val real_get_id = get_id
	    fun get_id (v : var) : string =
		(case VarMap.find (#2(!r), v)
		   of SOME I => I
		    | NONE => real_get_id v)
	in  {get_id=get_id, fresh=fresh, find=find,
	     get_iface_paths=get_iface_paths'}
	end

    val make_precontext : var list -> Update.precontext =
	List.mapPartial
	(fn v =>
	 (case (get_unit_paths v, get_node v)
	    of (NONE, _) => NONE
	     | (SOME _, CHECKU _) => NONE
	     | (SOME U, _) =>
		let val name = Paths.unitName U
		    val iface = Paths.ifaceFile (Paths.unitIface U)
		in  SOME (name, iface)
		end))

    val get_precontext : var -> Compiler.precontext =
	make_precontext o get_import_transitive

    fun get_ue (node : var) : Ue.ue =
	let val nodes = get_import_direct node
	    fun mapone (U : Paths.compunit) : string * Crc.crc =
		let val name = Paths.unitName U
		    val iface = Paths.ifaceFile (Paths.unitIface U)
		    val crc = FileCache.crc iface
		in  (name, crc)
		end
	    val mapper = Option.compose (mapone, get_unit_paths)
	    val ue = List.mapPartial mapper nodes
	in  (foldl (fn ((u,crc),acc) => Ue.insert (acc,u,crc)) Ue.empty ue)
	end

    fun plan (node : var) : Update.plan =
	(case get_node node
	   of SRCI (iface,imports) =>
		let val precontext = get_precontext node
		in  Update.plan_srci (eq,precontext,imports,iface)
		end
	    | COMPI iface => Update.plan_compi (eq,get_ue node,iface)
	    | SRCU (unit,_,imports) =>
		let val precontext = get_precontext node
		in  Update.plan_compile (eq,precontext,imports,unit)
		end
	    | COMPU unit => Update.plan_compu (eq,get_ue node,unit)
	    | PRIMU (unit,imports) =>
		let val precontext = get_precontext node
		in  Update.plan_compile (eq,precontext,imports,unit)
		end
	    | IMPORTU unit => Update.empty_plan
	    | CHECKU (unit,iface) =>
		let val precontext = get_precontext node
		in  Update.plan_checku (eq,precontext,unit,iface)
		end
	    | LINK exe =>
		let val unit_help = unit_help node
		    val roots = sort(get_import_direct node)
		in  Update.plan_link (eq,unit_help,roots,exe)
		end
	    | PACK (lib,importOnly,values) =>
		let val unit_help = unit_help node
		    val iface_help = iface_help node
		    fun import v = VarSet.member(importOnly,v)
		    val roots = get_import_direct node
		in  Update.plan_pack (eq,unit_help,iface_help,import,roots,lib)
		end
	    | INITIAL => error "plan saw initial")

    fun analyzeReady (node : var) : unit =
	let val plan = plan node
	    val _ =
		(case (Update.isEmpty plan, Update.sendToSlave plan)
		   of (true, _) => markDone node
		    | (false, true) => markPending (node, plan)
		    | (false, false) => markPending' (node, plan))
	    val _ = if Update.isReady plan then
			enable node
		    else ()
	in  ()
	end

    (* waiting, pending, working, proceeding, pending', working' - ready and done not included *)
    type state = var list * var list * var list * var list * var list * var list
    datatype result = PROCESSING of time * state  (* All slaves utilized *)
                    | IDLE of time * state * int  (* Num slaves idle and there are waiting jobs *)
	            | COMPLETE of time
    fun stateSize ((a,b,c,d,e,f) : state) = ((length a) + (length b) + (length c) + (length d) +
					     (length e) + (length f))

    fun resultToTime (PROCESSING (t,_)) = t
      | resultToTime (IDLE (t,_,_)) = t
      | resultToTime (COMPLETE t) = t

    fun stateDone(([],[],[],[],[],[]) : state) = true
      | stateDone _ = false
    fun partition (nodes : var list) =
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
    fun getReady (waiting : var list) : var list * var list * var list =
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
	    val done = List.filter (fn v =>
				    (case get_node v
				       of COMPI _ => false
					| COMPU _ => false
					| _ => true)) done
	    val _ = if !ShowUptodate andalso not (null done) then
			(print "Already up-to-date: ";
			 print_strings 20 (map get_id done);
			 print "\n")
		    else ()
	in  (waiting, pending, pending')
	end
    fun newState (waiting, pending, working, proceeding, pending', working') : state =
	let val ([], [], [], [], [], [], working', _) = partition working'
	    val ([], [], [], [], [], pending', newWorking', []) = partition pending'
	    val ([], [], [], [], proceeding, newPending'1, [], _) = partition proceeding
	    val ([], [], [], working, newProceeding, newPending'2, [], _) = partition working
	    val ([], [], pending, newWorking, [], [], [], []) = partition pending
	    val (waiting, newPending, newPending'3) = getReady waiting
	    val working' = working' @ newWorking'
	    val pending' = List.concat [pending', newPending'1, newPending'2, newPending'3]
	    val proceeding = proceeding @ newProceeding
	    val working = working @ newWorking
	    val pending = pending @ newPending
	in  (waiting, pending, working, proceeding, pending', working')
	end
    fun waitForSlaves () : int * int =
	let
	    fun ack_inter (name : string, (node,id) : Comm.job) : unit =
		markProceeding node
	    fun ack_done (name : string, (node,id) : Comm.job,
			  plan : Update.plan) : unit =
		if Update.isEmpty plan then
		    markDone node
		else
		    markPending' (node, plan)
	    fun ack_local ((node,id) : Comm.job) : unit =
		markDone node
	in  pollForSlaves (ack_inter, ack_done, ack_local)
	end
    fun finalReport (nodes : var list) : unit =
	let
	    fun toString r = Real.toString (Real.realFloor (r * 100.0) / 100.0)
	    val _ = print "------- Times to compile files -------\n"
	    fun stats nil = "unavailable"
	      | stats (L : real list) =
		let val v = Vector.fromList L
		in  String.concat ["min ", toString (VectorStats.min v),
				   " max ", toString (VectorStats.max v),
				   " mean ", toString (VectorStats.mean v),
				   " absdev ", toString (VectorStats.absdev v),
				   " (n=" ^ Int.toString (Vector.length v) ^ ")"]
		end
	    fun mapper node =
		let val (total, slave, master) = getTotalTimes node
		    val total' = Time.+ (slave, master)
		    val idle = Time.- (total, total')
		    val total' = Time.toReal total'
		    val idle = Time.toReal idle
		in  if total' > 0.0 then SOME (get_id node, idle, total')
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
	    fun useOne (node : var) =
		let val plan = getPlan node
		    val _ = markWorking' node
		    val _ = Update.flush plan
		    fun go () =
			if Update.isEmpty (Update.compile plan ())
			then ()
			else error "assembly failed"
		    val job = (node, get_id node)
		in  useLocal (job, go)
		end
	    fun useSome (0, _) = 0
	      | useSome (n, nil) = n
	      | useSome (n, unit :: rest) = (useOne unit;
					     useSome (n-1, rest))
	    val (_, _, _, _, pending', _) = state
	in  useSome (localsLeft, pending')
	end

    fun useSlaves (slavesLeft : int, state : state) : int =
	let
	    fun useOne node =
		let fun showSlave (name : string) : unit =
			msg ("Sending " ^ get_id node ^ " to " ^ name ^ ".\n")
		    val plan = getPlan node
		    val _ = markWorking node
		    val _ = Update.flush plan
		    val job = (node, get_id node)
		in  useSlave (showSlave,
			      Comm.REQUEST (job, plan),
			      SOME (Comm.FLUSH (job,plan)))
		end
	    fun useSome (0, _) = 0
	      | useSome (n, nil) = n
	      | useSome (n, unit :: rest) = (useOne unit;
					     useSome (n-1, rest))
	    val (_, pending, _, _, _, _) = state
	in  useSome (slavesLeft, pending)
	end
    (* Result always contain clean states, we are always invoked on a
     * clean state, and we always call useLocals and useSlaves on
     * clean states.  ("clean" means each unit's state matches the
     * waitlist it occupies.)  *)
    fun step (state : state) : result =
	let val (slavesLeft, localsLeft) = waitForSlaves()
	    val state = newState state
	    val localsLeft = useLocals (localsLeft, state)
	    val state = newState state
	    val slavesLeft = useSlaves (slavesLeft, state)
	    val state = newState state
	    val (waiting, _, _, _, _, _) = state
	in
	    if stateDone state then
		COMPLETE (Time.now())
	    else
		if slavesLeft > 0 then
		    IDLE (Time.now(), state, slavesLeft)
		else
		    PROCESSING (Time.now(), state)
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
					 in  res
					 end
				else ""
		val diff = Time.-(cur, (case !start of
					    NONE => error "no start time"
					  | SOME t => t))
		val diff = Time.toReal diff
		val diff = (Real.realFloor(diff * 100.0)) / 100.0
		val padding = Util.spaces (30 - size str)
		val msg = (str ^ padding ^ ": " ^ curString ^ "   " ^
			   (Real.toString diff) ^ " sec\n")
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
			    String.concat ["CheckPoint (", Int.toString left, " files left = ",
					   Int.toString waiting, " waiting + ",
					   Int.toString pending, " pending + ",
					   Int.toString working, " working + ",
					   Int.toString proceeding, " proceeding + ",
					   Int.toString pending', " pending' + ",
					   Int.toString working', " working')"]
			end
		    else String.concat ["CheckPoint (", Int.toString left, " files left)"]
	    in	showTime (false, msg)
	    end
	fun wrap (f : 'a -> unit) (x : 'a) : unit =
	    if !Checkpoint then f x else ()
    in
	val showTime = wrap showTime
	val startTime = wrap startTime
	val reshowTimes = wrap reshowTimes
	val checkpoint = wrap checkpoint
    end

    fun once (groupfile : string) : {setup : unit -> state,
				     step : state -> result,
				     complete : unit -> unit} =
	let val _ = if not (Target.native()) then
			(print "Warning: target prevents full compile\n";
			 Update.UptoAsm := true)
		    else ()
	    val _ = resetSlaves()
	    val initial = read_graph groupfile
	    val nodes = get_import_transitive initial
	    val _ = startTime ("Started compiling files")
	    val initialState : state = (nodes, [], [], [], [], [])
	in  {setup = fn _ => initialState,
	     step = step,
	     complete = (fn _ =>
			 let val _ = showTime (true,"Finished compiling files")
			     val _ = closeSlaves()
			     val _ = if !ShowFinalReport
					 then finalReport nodes
				     else ()
			     val _ = reshowTimes()
			 in  ()
			 end)}
	end
    fun run (mapfile : string) : unit =
	let val {setup,step,complete} = once mapfile
	    fun loop (lastCpTime : Time.time, last : result option,
		      state : state) : unit =
	      let val cur = step state
		  val thisTime = resultToTime cur
		  val diff = Time.toReal(Time.-(thisTime,lastCpTime))
		  val lastCpTime =
			if (diff > 30.0) then
			    (checkpoint state; thisTime)
			else lastCpTime
	      in
		(case cur of
		    COMPLETE _ => complete()
		  | PROCESSING (t,state) =>
			let val _ = (case last of
					 NONE => ()
				       | SOME (PROCESSING _) => ()
				       | _ => msg "All slaves working.\n")
			in  Platform.sleep 2.0;
			    loop (lastCpTime,SOME cur,state)
			end
		  | IDLE(t, state as (waiting, pending, working, proceeding, pending', working'), n) =>
			let val _ = (case last of
					 NONE => ()
				       | SOME (IDLE _) => ()
				       | _ => msg (Int.toString n ^
						   " idle slaves."))
			in  Platform.sleep 1.0;
			    loop (lastCpTime,SOME cur,state)
			end)
	      end
	in loop (Time.now(), NONE, setup())
	end

    type targets = (Paths.iface -> string) list *
		   (Paths.compunit -> string) list *
		   (Paths.exe -> string) list

    val empty : targets = (nil, nil, nil)

    fun append (t : targets, t' : targets) : targets =
	let val (i,s,e) = t
	    val (i',s',e') = t'
	in  (i@i', s@s', e@e')
	end

    val concat : targets list -> targets =
	foldl append empty

    (*
	XXX: Purge should leave libraries alone.  This works out now
	if pack makes binary-only libraries.
    *)
    fun purge_help (what : string, targets : targets)
		   (groupfile : string) : unit =
	let val (ifacePaths,unitPaths,exePaths) = targets
	    val initial = read_graph groupfile
	    val _ = msg ("Purging " ^ groupfile ^ " (" ^ what ^ ").\n")
	    val nodes = get_import_transitive initial
	    fun remove (paths : ('a -> string) list) (x : 'a) : unit =
		app (fn f => FileCache.remove (f x)) paths
	    fun remove_node (v : var) : unit =
		(case get_node v
		   of SRCI (p,_) => remove ifacePaths p
		    | COMPI _ => ()
		    | SRCU (p,_,_) => remove unitPaths p
		    | COMPU _ => ()
		    | PRIMU (p,_) => remove unitPaths p
		    | IMPORTU _ => ()
		    | CHECKU _ => ()
		    | LINK p => remove exePaths p
		    | PACK _ => ()
		    | INITIAL => error "remove_node saw initial")
	in  app remove_node nodes
	end

    val meta : targets =
	([Paths.ifaceInfoFile], [Paths.infoFile], nil)
    val iface : targets =
	let val i = [Paths.ifaceFile,Paths.ifaceUeFile]
	    val u = map (fn p => p o Paths.unitIface) i
	in  (i, u, nil)
	end
    val asm : targets =
	(nil, [Paths.asmFile, Paths.asmzFile],
	 [Paths.exeAsmFile, Paths.exeAsmzFile])
    val obj : targets =
	(nil, [Paths.objFile,Paths.ueFile], [Paths.exeObjFile])
    val exe : targets = (nil, nil, [Paths.exeFile])

    val clean : string -> unit = purge_help ("object files", obj)
    val purge : string -> unit = purge_help ("binaries", concat [asm,obj,exe])
    val purgeAll : string -> unit =
	purge_help ("binaries and interfaces",
		    concat [meta,iface,asm,obj,exe])

end
