(*
	The master maintains three DAGs, called the compilation graph,
	the unit graph, and the interface graph.  The master could get
	by with a single graph, of course, but it is hoped that a
	clear separation makes the code easier to follow.

	The compilation graph has an initial node, a node for every
	entry in the group, an arrow initial -> b for every executable
	and library b in the group, and an arrow a -> b when neither a
	nor b is initial and b must be up to date before a can be
	compiled.  The compilation graph determines what units,
	interfaces, libraries, and executables need to be brought up
	to date and in what order.  Compilation stops when every node
	reachable from the initial node is up to date.  In contrast to
	the unit and interface graphs, the compilation graph may have
	several nodes for each compilation unit because of
	Group.CHECKU.

	The unit graph has a node for every unit and an arrow a -> b
	when the implementation of a depends on b.  This dependency
	information comes from the group, not from examining compiler
	output so that side effects are not lost.  The unit graph is
	used to link executables and to pack implemented units into
	libraries.

	The interface graph has a node for every unit and an arrow a
	-> b when the interface of a depends on b.  This dependency
	information comes from examining compiler output, not the
	group so it is not cluttered with implementation dependencies.
	The interface graph is used to build elaboration contexts and
	to pack imported units into libraries.

	Other state maintained by the master includes an equivalence
	relation on compiled interface CRCs, group file order
	information, and a list of slaves that have announced
	themselves.
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
    structure StringSet = Util.StringSet
    structure StringMap = Util.StringMap
    structure VarSet = Name.VarSet

    val MasterDiag = Stats.ff("MasterDiag")
    fun msg str = if (!MasterDiag) then print str else ()

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

    val Checkpoint = Stats.ff "Checkpoint"
    val CheckpointVerbose = Stats.ff "CheckpointVerbose"
    val ShowEnable = Stats.ff "ShowEnable"
    val ShowUptodate = Stats.ff "ShowUptodate"
    val ShowFinalReport = Stats.ff "ShowFinalReport"

    val error = fn s => Util.error "master.sml" s
    val reject = fn s => raise (Compiler.Reject s)

    (*
	WAITING: prerequisites not up to date
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

    datatype iface_node =
	SRCI of Paths.iface * Update.imports
      | COMPI of Paths.iface

    fun iface_paths (iface : iface_node) : Paths.iface =
	(case iface
	   of SRCI (p,_) => p
	    | COMPI p => p)

    fun iface_id (n : iface_node) : string =
	(case n
	   of SRCI (p,_) => "source interface " ^ Paths.ifaceName p
	    | COMPI p => "compiled interface " ^ Paths.ifaceName p)

    datatype unit_node =
	SRCU of Paths.compunit * var option * Update.imports
      | COMPU of Paths.compunit * var
      | PRIMU of Paths.compunit * Update.imports
      | IMPORTU of Paths.compunit * var
      | CHECKU of var * Paths.compunit * Paths.iface

    fun unit_paths (unit : unit_node) : Paths.compunit =
	(case unit
	   of SRCU (p,_,_) => p
	    | COMPU (p,_) => p
	    | PRIMU (p,_) => p
	    | IMPORTU (p,_) => p
	    | CHECKU (_,p,_) => p)

    fun unit_id (n : unit_node) : string =
	(case n
	   of SRCU (p,_,_) => "source unit " ^ Paths.unitName p
	    | COMPU (p,_) => "compiled unit " ^ Paths.unitName p
	    | PRIMU (p,_) => "primitive unit " ^ Paths.unitName p
	    | IMPORTU (p,_) => "imported unit " ^ Paths.unitName p
	    | CHECKU (_,p,_) => "checked unit " ^ Paths.unitName p)

    datatype node =
	UNIT of unit_node
      | IFACE of iface_node
      | LINK of Paths.exe
      | PACK of
	{lib:Paths.lib,
	 imports:VarSet.set,
	 units:VarSet.set,
	 ifaces:VarSet.set,
	 values: (E.id * E.exp) list}
      | INITIAL

    fun node_id (n : node) : string =
	(case n
	   of UNIT n' => unit_id n'
	    | IFACE n' => iface_id n'
	    | LINK p => "executable " ^ Paths.exeFile p
	    | PACK {lib,...} => "library " ^ Paths.libDir lib
	    | INITIAL => "end of compilation")

    local
	structure Equiv :> EQUIV where type elem = Crc.crc =
	struct
	    structure StringEquiv = Equiv(StringMap)
	    type elem = Crc.crc
	    type equiv = StringEquiv.equiv
	    val empty = StringEquiv.empty
	    fun insert (e,a,b) =
		let val a = Crc.toString a
		    val b = Crc.toString b
		in  StringEquiv.insert (e, a, b)
		end
	    fun equiv e =
		let val eq = StringEquiv.equiv e
		in  fn (a,b) => eq (Crc.toString a, Crc.toString b)
		end
	end

	structure VarKey =
	struct
	    type hash_key = Name.var
	    val hashVal = Word.fromInt
	    val sameKey = (op= : var * var -> bool)
	end
	structure VarNode = Node(structure Key = VarKey
				 val toString = Name.var2string)
	structure Graph = Graph(VarNode)
	structure Agraph = LabelGraph(SpeedUpGraph(Graph))

	type attr = {status : status ref,
		     node : node}

	type cgraph = attr Agraph.graph
	type ugraph = Graph.graph
	type igraph = Graph.graph

	datatype order =
	    REV of var list	(* backwards *)
	  | BOTH of var list * var list	(* backwards, forwards *)

	val dummy : var = Name.fresh_var()
	val equiv : Equiv.equiv ref = ref (Equiv.empty)
	val cgraph : cgraph ref = ref (Agraph.empty dummy)
	val ugraph : ugraph ref = ref (Graph.empty dummy)
	val igraph : igraph ref = ref (Graph.empty dummy)
	val order : order ref = ref (REV nil)
	val pos_ref : (var -> Group.pos) option ref = ref NONE

	(* These ! at each invocation. *)
	fun cwrap (f : cgraph -> 'a -> 'b) : 'a -> 'b = fn x => f (!cgraph) x
	fun uwrap (f : ugraph -> 'a -> 'b) : 'a -> 'b = fn x => f (!ugraph) x
	fun iwrap (f : igraph -> 'a -> 'b) : 'a -> 'b = fn x => f (!igraph) x

	val lookup_node : var -> attr = cwrap Agraph.attribute

	fun unit_var (v : var) : var option =
	    let val {node,...} = lookup_node v
	    in  (case node
		   of UNIT (CHECKU (U,_,_)) => unit_var U
		    | UNIT _ => SOME v
		    | _ => NONE)
	    end

    in
	fun reset_graph() : unit =
	    (equiv := Equiv.empty;
	     cgraph := Agraph.empty dummy;
	     ugraph := Graph.empty dummy;
	     igraph := Graph.empty dummy;
	     order := REV nil;
	     pos_ref := NONE)

	fun add_eq (crc : Crc.crc, crc' : Crc.crc) : unit =
	    equiv := Equiv.insert (!equiv, crc, crc')

	fun eq (crcs : Crc.crc * Crc.crc) : bool =
	    Equiv.equiv (!equiv) crcs

	fun sort (nodes : var list) : var list =
	    let val set = VarSet.addList(VarSet.empty, nodes)
		val order =
		    (case !order
		       of REV vars => rev vars
			| BOTH (_,vars) => vars)
	    in  List.filter (fn v => VarSet.member (set,v)) order
	    end

	fun add_node (v : var, node : node) : unit =
	    let val revOrder =
		    (case !order
		       of REV vars => vars
			| BOTH (vars,_) => vars)
		val _ = order := REV(v :: revOrder)
		val _ =
		    (case node
		       of UNIT (CHECKU _) => ()
			| UNIT _ =>
			    (Graph.insert_node (!ugraph) v;
			     Graph.insert_node (!igraph) v)
			| _ => ())
		val attr = {status=ref WAITING, node=node}
		val _ = Agraph.insert_node (!cgraph) (v,attr)
	    in  ()
	    end

	fun add_edge (src : var) (dest : var) : unit =
	    (Agraph.insert_edge (!cgraph) (src,dest);
	     (case (unit_var src, unit_var dest)
		of (SOME src', SOME dest') =>
		    Graph.insert_edge (!ugraph) (src',dest')
		 | _ => ()))

	fun add_interface_edge (src : var) (dest : var) : unit =
	    Graph.insert_edge (!igraph) (src,dest)

	fun has_interface_edges (src : var) : bool =
	    not (null (Graph.edges (!igraph) src))

	val get_node : var -> node = #node o lookup_node

	fun get_status (node : var) : status = !(#status(lookup_node node))

	fun set_status (node : var, s : status) : unit =
	    (#status(lookup_node node)) := s

	val get_dependents : var -> var list = cwrap Agraph.edgesRev

	val get_prerequisites : var -> var list = cwrap Agraph.edges

	val units_only : var list -> var list =
	    List.mapPartial unit_var

	fun get_units () : var list =
	    Graph.nodes (!ugraph)

	val get_unit_edges : var -> var list =
	    uwrap Graph.edges

	val get_interface_edges : var -> var list =
	    iwrap Graph.edges

	val get_reachable : var list -> var list =
	    cwrap Agraph.reachable

	val get_unit_reachable : var list -> var list =
	    uwrap Graph.reachable

	val get_interface_reachable : var list -> var list =
	    iwrap Graph.reachable

	fun graphSize () : int * int =
	    (Agraph.nodecount (!cgraph), Agraph.edgecount (!cgraph))

	fun set_pos (f : var -> Group.pos) : unit = pos_ref := SOME f

	fun get_pos (node : var) : Group.pos = valOf (!pos_ref) node

    end

    fun has_ascribed_iface (iface : var) (v : var) : bool =
	(case get_node v
	   of UNIT unit_node =>
		(case unit_node
		   of SRCU (_,SOME i,_) => i = iface
		    | COMPU (_,i) => i = iface
		    | IMPORTU (_,i) => i = iface
		    | _ => false)
	    | _ => false)

    fun ascribed_iface (v : var) : var =
	(case get_node v
	   of UNIT unit_node =>
		(case unit_node
		   of SRCU (_,SOME i,_) => i
		    | COMPU (_,i) => i
		    | IMPORTU (_,i) => i
		    | CHECKU (u,_,_) => ascribed_iface u
		    | _ => error "ascribed_iface saw unit with inferred iface")
	    | _ => error "ascribed_iface saw non-unit")

    fun get_iface_paths' (iface : var) : Paths.iface option =
	(case get_node iface
	   of IFACE iface_node => SOME (iface_paths iface_node)
	    | _ => NONE)

    fun get_iface_paths (iface : var) : Paths.iface =
	(case get_node iface
	   of IFACE iface_node => iface_paths iface_node
	    | _ => error "get_iface_paths given non-interface")

    fun get_unit_paths (unit : var) : Paths.compunit =
	(case get_node unit
	   of UNIT unit_node => unit_paths unit_node
	    | _ => error "get_unit_paths given non-unit")

    val get_id : var -> string = node_id o get_node

    val nofixup : E.groupfile -> E.groupfile =
	fn g => g

    fun read_group (groupfile : string) : Group.group =
	let val _ = Update.flushAll()
	    (* Eg: cputype=linux, objtype=sparc, target=sparc-8 *)
	    val cputype = Platform.platformName (Platform.platform ())
	    val objtype = Target.platformName (Target.getTargetPlatform ())
	    val target = Target.platformString()
	    val littleEndian = !Target.littleEndian
	    val libdir = Dirs.libDir() ^ "/Lib"
	    val g = Group.empty_group'
	    val g = Group.add_string_value (g, "cputype", cputype)
	    val g = Group.add_string_value (g, "objtype", objtype)
	    val g = Group.add_string_value (g, "target", target)
	    val g = Group.add_bool_value (g, "littleEndian", littleEndian)
	    val g = Group.add_int_value (g, "majorVersion", Version.majorVersion)
	    val g = Group.add_int_value (g, "minorVersion", Version.minorVersion)
	    val g = Group.add_string_value (g, "version", Version.version)
	    val g = Group.add_string_value (g, "libdir", libdir)
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

    val make_imports : var list -> Update.imports =
	map (Paths.unitName o get_unit_paths)

    fun graphInfo (entry : Group.entry) : node * var list =
	(case entry
	   of Group.IFACE iface =>
		(case iface
		   of Group.SRCI (id,group,file,imports) =>
			let val p = Paths.srci {id=id, group=group, file=file}
			    val i = make_imports imports
			in  (IFACE (SRCI (p,i)), imports)
			end
		    | Group.COMPI (id,iface,ue,parms) =>
			(IFACE (COMPI (Paths.compi {id=id, file=iface,
						    uefile=ue})),
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
			in  (UNIT (SRCU (p,iface,i)), edges)
			end
		    | Group.COMPU (id,obj,ue,parms,iface) =>
			let val p' = get_iface_paths iface
			    val p = Paths.compu {id=id,file=obj,
						 uefile=ue,iface=p'}
			in  (UNIT (COMPU (p,iface)),
			     iface :: (VarSet.listItems parms))
			end
		    | Group.PRIMU (id,group,imports) =>
			let val p = Paths.primu {id=id, group=group}
			    val i = make_imports imports
			in  (UNIT (PRIMU (p,i)), imports)
			end
		    | Group.IMPORTU (id,iface) =>
			let val p' = get_iface_paths iface
			    val p = Paths.importu {id=id, iface=p'}
			in  (UNIT (IMPORTU (p,iface)), [iface])
			end
		    | Group.CHECKU {U,I} =>
			let val pU = get_unit_paths U
			    val pI = get_iface_paths I
			in  (UNIT (CHECKU (U,pU,pI)), [U,I])
			end)
	    | Group.CMD cmd =>
		(case cmd
		   of Group.LINK (group,exe,targets) =>
			(LINK (Paths.exe {group=group, exe=exe}),
			 get_reachable (VarSet.listItems targets))
		    | Group.PACK {lib, imports, units, ifaces, values} =>
			let val lib = Paths.lib {dir=lib}
			    val pack = PACK {lib=lib, imports=imports,
					     units=units, ifaces=ifaces,
					     values=values}
			    val exports = VarSet.union(units,ifaces)
			    val exports = VarSet.listItems exports
			    val edges = get_reachable exports
			in  (pack,edges)
			end))

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
	We only need to add edges for the compiled interface's
	parameters.  We go beyond this because (a) reading
	parameterized interfaces is a lot slower than reading unit
	environments and (b) the master does not use FileCache.tick so
	whatever gets read into the cache stays there.
    *)
    fun interface_ready (node : var) : unit =
	let fun edges (iface : Paths.iface) : var list =
		let val possible : var list = get_units()
		    val ifaceue = Paths.ifaceUeFile iface
		    val ue = FileCache.read_ue ifaceue
		    fun inUe (v:var) : bool =
			let val U = get_unit_paths v
			    val name = Paths.unitName U
			in  isSome (Ue.find (ue,name))
			end
		    val arrows : var list = List.filter inUe possible
		in  arrows
		end
	    fun add_edges (iface : Paths.iface, units : var list) : unit =
		let val edges : unit -> var list =
			Util.memoize (fn () => edges iface)
		    fun add (u : var) : unit =
			if has_interface_edges u then ()
			else app (add_interface_edge u) (edges())
		in  app add units
		end
	in  (case get_node node
	       of IFACE iface_node =>
		    let val iface = iface_paths iface_node
			val dependent = get_dependents node
			val units =
			    List.filter (has_ascribed_iface node) dependent
		    in	add_edges (iface,units)
		    end
		| UNIT (CHECKU _) => ()
		| UNIT unit_node =>
		    let val iface = Paths.unitIface (unit_paths unit_node)
		    in  add_edges (iface,[node])
		    end
		| _ => ())
	end

    (*
	Find every ready node a such that a -> b.  An interface or
	unit node a is ready if for every node c with a -> c, c has an
	up to date interface.  A LINK, PACK, or INITIAL node a is
	ready if for every node c with a -> c, c is done.
    *)
    fun enable (b : var) : unit =
	let val _ = interface_ready b
	    fun hasInterface' (c : var) : bool =
	        (case get_status c
		   of PENDING (_, p) => Update.isReady p
		    | WORKING (_, p) => Update.isReady p
		    | PROCEEDING _ => true
		    | PENDING' _ => true
		    | WORKING' _ => true
		    | DONE _ => true
		    | _ => false)
	    fun interfaceVar (c : var) : var =
		(case get_node c
		   of UNIT (SRCU (_,SOME I,_)) => I
		    | _ => c)
	    val hasInterface : var -> bool = hasInterface' o interfaceVar
	    fun isDone (c : var) : bool =
		(case get_status c
		   of DONE _ => true
		    | _ => false)
	    fun isReady (a : var) : bool =
		let val prereqs = get_prerequisites a
		    val predicate =
			(case get_node a
			   of LINK _ => isDone
			    | PACK _ => isDone
			    | INITIAL => isDone
			    | _ => hasInterface)
		in  Listops.andfold predicate prereqs
		end
	    fun enableReady (a : var) : bool =
		(case get_status a
		   of WAITING =>
			isReady a andalso (markReady a; true)
		    | _ => false)
	    val enabled = List.filter enableReady (get_dependents b)
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
		       of UNIT (CHECKU (_,U,I)) =>
			    let val I = Paths.ifaceFile I
				val U = Paths.ifaceFile (Paths.unitIface U)
			    in  add_eq(FileCache.crc I, FileCache.crc U)
			    end
			| _ => ())
	    val _ = enable node
	in  ()
	end

    fun get_context' (exclude : VarSet.set, v : var) : Update.context =
	let val prereqs = units_only(get_prerequisites v)
	    val reachable = get_interface_reachable prereqs
	    val reachable = sort reachable
	    fun mapper (v : var) : (string * Paths.iface) option =
		if VarSet.member (exclude,v) then NONE
		else
		    let val U = get_unit_paths v
			val name = Paths.unitName U
			val iface = Paths.unitIface U
		    in	SOME (name,iface)
		    end
	in  List.mapPartial mapper reachable
	end

    fun get_context (v : var) : Update.context = get_context' (VarSet.empty,v)

    (*
	N.B.  COMPU and COMPI nodes have arrows to every unit named in
	their unit environment files.
    *)
    fun get_ue (v : var) : Ue.ue =
	let val prereqs = units_only(get_prerequisites v)
	    fun folder (u:var, ue:Ue.ue) : Ue.ue =
		let val U = get_unit_paths u
		    val name = Paths.unitName U
		    val iface = Paths.ifaceFile (Paths.unitIface U)
		    val crc = FileCache.crc iface
		in  Ue.insert(ue,name,crc)
		end
	in  foldl folder Ue.empty prereqs
	end

    (*
	We need fresh interface names when the group uses the same
	name multiple times and when packing inferred interfaces.  In
	the first case, we want to shadow interface names in group
	file order.  In the second case, we want to ensure that
	inferred interface names never shadow names that were in the
	group.
    *)
    fun make_gensym (reserved : StringSet.set) : string -> string =
	let val used = ref reserved
	    fun find (set : StringSet.set, base : string, n : int) : string =
		let val suffix = if n = 0 then "" else Int.toString n
		    val s = base ^ suffix
		in  if StringSet.member (set, s) then find (set, base, n+1)
		    else s
		end
	    fun gensym (base : string) : string =
		let val set = !used
		    val name = find (set, base, 0)
		in  used := StringSet.add (set, name);
		    name
		end
	in  gensym
	end

    type renaming =
	{shadow : string VarMap.map,
	 gensym : string -> string}

    fun iface_renaming (nodes : var list) : renaming =
	let type acc = string VarMap.map * var StringMap.map * StringSet.set
	    fun folder (v : var, acc as (change, bound, used) : acc) : acc =
		(case get_node v
		   of IFACE iface_node =>
			let val iface = iface_paths iface_node
			    val name = Paths.ifaceName iface
			    val change =
				(case StringMap.find (bound, name)
				   of SOME v' => VarMap.insert (change, v', name)
				    | NONE => change)
			    val bound = StringMap.insert (bound, name, v)
			    val used = StringSet.add (used, name)
			in  (change, bound, used)
			end
		    | _ => acc)
	    val acc = (VarMap.empty, StringMap.empty, StringSet.empty)
	    val (change, bound, used) = foldl folder acc nodes
	    val gensym = make_gensym used
	    fun folder (v : var, name : string,
			shadow : string VarMap.map) : string VarMap.map =
		VarMap.insert (shadow, v, gensym name)
	    val shadow = VarMap.foldli folder VarMap.empty change
	in  {shadow=shadow, gensym=gensym}
	end

    fun named_iface (renaming : renaming) : var -> Paths.iface =
	let val {shadow, ...} = renaming
	    fun get_iface v =
		let val iface = get_iface_paths v
		    val name =
			(case VarMap.find (shadow,v)
			   of SOME name => name
			    | NONE => Paths.ifaceName iface)
		    val file = Paths.ifaceFile iface
		    val uefile = Paths.ifaceUeFile iface
		in  Paths.compi {id=name, file=file, uefile=uefile}
		end
	in  get_iface
	end

    fun inferred_iface (renaming : renaming) : Paths.compunit -> Paths.iface =
	let val {gensym, ...} = renaming
	    fun make_iface unit =
		let val iface = Paths.unitIface unit
		    val name = gensym (Paths.unitName unit)
		    val file = Paths.ifaceFile iface
		    val uefile = Paths.ifaceUeFile iface
		in  Paths.compi {id=name, file=file, uefile=uefile}
		end
	in  make_iface
	end

    type packparms =
	{importOnly : var -> bool,
	 named_iface : var -> Paths.iface,
	 inferred_iface : Paths.compunit -> Paths.iface}

    type packinfo = Update.pack list VarMap.map

    fun packed (info : packinfo, v : var) : bool =
	isSome (VarMap.find (info,v))

    fun pack_unit (parms : packparms) (v : var, info : packinfo) : packinfo =
	if packed(info,v) then info
	else
	    let val U = get_unit_paths v
		val I = Paths.unitIface U
		val import =
		    Paths.isImportUnit U orelse
		    #importOnly parms v
		val get_edges =
		    if import then get_interface_edges else get_unit_edges
		val info = pack_units parms (get_edges v,info)
		val inferred = Paths.isUnitIface I
		val info = if inferred then info
			   else pack_iface parms (ascribed_iface v, info)
		val I' = if inferred then #inferred_iface parms U else I
		val name = Paths.unitName U
		val U' =
		    if import
		    then Paths.importu {id=name, iface=I'}
		    else Paths.compu {id=name, file=Paths.objFile U,
				      uefile=Paths.ueFile U, iface=I'}
		val packlist = [Update.PACKU (U',nil)]
		val packlist =
		    if inferred
		    then (Update.PACKI(I',nil)) :: packlist
		    else packlist
		in  VarMap.insert (info,v,packlist)
		end

    and pack_units (parms : packparms)
		   (vs : var list, info : packinfo) : packinfo =
	foldl (pack_unit parms) info vs

    and pack_iface (parms : packparms) (v : var, info : packinfo) : packinfo =
	if packed(info,v) then info
	else
	    let val prereqs = units_only(get_prerequisites v)
		val info = pack_units parms (prereqs,info)
		val iface = #named_iface parms v
		val pack = Update.PACKI (iface,nil)
	    in	VarMap.insert(info,v,[pack])
	    end

    fun plan (node : var) : Update.plan =
	(case get_node node
	   of IFACE (SRCI (iface,imports)) =>
		let val context = get_context node
		in  Update.plan_srci (eq,context,imports,iface)
		end
	    | IFACE (COMPI iface) => Update.plan_compi (eq,get_ue node,iface)
	    | UNIT (SRCU (unit,_,imports)) =>
		let val context = get_context node
		in  Update.plan_compile (eq,context,imports,unit)
		end
	    | UNIT (COMPU (unit,_)) => Update.plan_compu (eq,get_ue node,unit)
	    | UNIT (PRIMU (unit,imports)) =>
		let val context = get_context node
		in  Update.plan_compile (eq,context,imports,unit)
		end
	    | UNIT (IMPORTU (unit,_)) => Update.empty_plan
	    | UNIT (CHECKU (U,unit,iface)) =>
		let val exclude = VarSet.singleton U
		    val context = get_context' (exclude, node)
		in  Update.plan_checku (eq,context,unit,iface)
		end
	    | LINK exe =>
		let val prereqs = units_only(get_prerequisites node)
		    val reachable = get_unit_reachable prereqs
		    val reachable = sort reachable (* fix order of effects *)
		    val units = map get_unit_paths reachable
		    val (imports,units) = List.partition Paths.isImportUnit units
		in  if null imports then Update.plan_link (eq,units,exe)
		    else
			(print ("Warning: can not link " ^
				Paths.exeFile exe ^
				" because of unimplemented unit(s): ");
			 print_strings 70 (map Paths.unitName imports);
			 print "\n";
			 Update.empty_plan)
		end
	    | PACK {lib,imports,units,ifaces,values} =>
		let val importOnly =
			if !Update.UptoElaborate orelse !Update.UptoAsm then
			    fn v => true
			else
			    fn v => VarSet.member (imports, v)
		    val renaming = iface_renaming (get_prerequisites node)
		    val named_iface = named_iface renaming
		    val inferred_iface = inferred_iface renaming
		    val parms = {importOnly=importOnly, named_iface=named_iface,
				 inferred_iface=inferred_iface}
		    val units = units_only (VarSet.listItems units)
		    val ifaces = VarSet.listItems ifaces
		    val info : packinfo = VarMap.empty
		    val info = foldl (pack_iface parms) info ifaces
		    val info = pack_units parms (units,info)
		    val packlist : var list =
			map #1 (VarMap.listItemsi info)
		    val packlist = sort packlist (* fix order of effects *)
		    val packlist : Update.pack list list =
			map (fn v => valOf (VarMap.find (info,v))) packlist
		    val packlist = List.concat packlist
		in  Update.plan_pack (eq,packlist,lib)
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

    (*
	(waiting, pending, working, proceeding, pending', working')
	Units that are ready and done are not included.  Waiting,
	pending, and pending' are kept sorted so that analysis and
	compilation happens in an order determined by the group.
    *)
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
				       of IFACE (COMPI _) => false
					| UNIT (COMPU _) => false
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
	in  (waiting, sort pending, working, proceeding, sort pending', working')
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
	    val targets = get_reachable (get_prerequisites initial)
	    val targets = sort targets	(* make master more deterministic *)
	    val _ = startTime ("Started compiling files")
	    val initialState : state = (targets, [], [], [], [], [])
	in  {setup = fn _ => initialState,
	     step = step,
	     complete = (fn _ =>
			 let val _ = showTime (true,"Finished compiling files")
			     val _ = closeSlaves()
			     val _ = if !ShowFinalReport
					 then finalReport targets
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
						   " idle slaves.\n"))
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
	    val _ = if !Update.UpdateDiag then
			print ("Purging " ^ groupfile ^ " (" ^ what ^ ").\n")
		    else ()
	    val nodes = get_reachable(get_prerequisites initial)
	    fun remove (paths : ('a -> string) list) (x : 'a) : unit =
		app (fn f => FileCache.remove (f x)) paths
	    fun remove_node (v : var) : unit =
		(case get_node v
		   of IFACE (SRCI (p,_)) => remove ifacePaths p
		    | IFACE (COMPI _) => ()
		    | UNIT (SRCU (p,_,_)) => remove unitPaths p
		    | UNIT (PRIMU (p,_)) => remove unitPaths p
		    | UNIT _ => ()
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
