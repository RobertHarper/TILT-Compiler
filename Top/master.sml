(*$import Prelude TopLevel Util Stats Update UpdateHelp Time Graph Compiler TextIO Real Vector String Char Int Listops SplayMapFn ListMergeSort MASTER Communication TopHelp Prelink Tools Background OS List Platform Dirs Target Paths Statistics *)

(* 
   The master sets up by deleting all channels and then takes master steps which are:
   
     While there are available jobs
         (1) Look for available slave channels and process their acknowledgements
	     by marking compilation jobs as done.  Ready messages require no action.
	 (2) Issue all available jobs to available slaves.
	     (A) If there are more jobs than slaves, then we are PROCESSING fully.
	     (B) Otherwise, we are IDLEing because the slaves are underutilized.
     When there are no more jobs, we are COMPLETEd.
*)


functor Master (val bootstrap : bool
                val basisMapfile : unit -> string option
		val basisImports : string list)
    :> MASTER =
struct
    val error = fn s => Util.error "master.sml" s

    val checkPointVerbose = Stats.tt "CheckPointVerbose"
    val showEnable = Stats.ff "ShowEnable"
	
    structure S = Update
	
    structure Cache = UpdateHelp.Cache
    structure InfoCache = UpdateHelp.InfoCache
    structure StringOrderedSet = Help.StringOrderedSet
    structure StringMap = Help.StringMap
	
    val chat = Help.chat
    val chat_strings = Help.chat_strings
    val chat_verbose = Help.chatVerbose

    (* See status.dot for a view of the status transition graph.  We measure the following times:
     * total time:  time to go from READY to DONE
     * slave time:  time spent in WORKING and PROCEEDING
     * master time: time spent in WORKING'
     *)
    datatype status =
	WAITING							(* Waiting for imports to be up to date. *)
      | READY of Time.time					(* Pending up-to-date check.  Ready time. *)
      | PENDING of Time.time * Update.plan			(* Pending work on slave.  Ready time. *)
      | WORKING of (Time.time * Time.time) * Update.plan	(* Slave working.  Ready time, working time. *)
      | PROCEEDING of (Time.time * Time.time) * Update.plan	(* Slave working; interface up to date.  Ready time, working time. *)
      | PENDING' of (Time.time * Time.time) * Update.plan	(* Pending work on master.  Ready time, slave time. *)
      | WORKING' of (Time.time * Time.time * Time.time) * Update.plan	(* Master working.  Ready time, slave time, working' time. *)
      | DONE of Time.time * Time.time * Time.time		(* Total time, slave time, master time. *)

    datatype compunit =
	UNIT of {paths : Paths.unit_paths,
		 isTarget : bool,
		 isBasis : bool}
      | PRIM

    val primImports = ["Firstlude"]	(* No source code for TiltPrim; we must hard code the imports. *)
    val primUnitName = "TiltPrim"
    fun unitName (UNIT {paths, ...}) = Paths.unitName paths
      | unitName PRIM = primUnitName

    fun unitSize (UNIT {paths, ...}) = Cache.size (Paths.sourceFile paths)
      | unitSize PRIM = 0

    local

	val graph = ref (Dag.empty() : {position : int,
					status : status ref,
					unit : compunit} Dag.graph)
        val units = ref (true, ([] : string list)) (* kept in reverse order if bool is true *)

	fun lookup unitname =
	    ((Dag.nodeAttribute(!graph,unitname))
	     handle Dag.UnknownNode _ => error ("unit " ^ unitname ^ " missing"))
 
    in 


	fun reset_graph() = (graph := Dag.empty(); 
			     units := (true, []))
	fun add_node(node, size, attribute) = 
	    let val u = (case !units of
			     (false, rev_ls) => (false, node :: rev_ls)
			   | (true, ls) => (false, node :: (rev ls)))
		val _ = units := u
		val _ = Dag.insert_node(!graph,node,size,attribute)
	    in  ()
	    end
	fun add_edge(src,dest) = Dag.insert_edge(!graph,src,dest)

	fun list_units() = (case !units of
				(true, ls) => ls
			      | (false, rev_ls) => 
				    let val ls = rev rev_ls
					val _ = units := (true, ls)
				    in ls
				    end)

	fun get_position unit = #position(lookup unit)
	fun get_unit unit = #unit(lookup unit)
	fun get_paths unit = (case get_unit unit
				of UNIT {paths, ...} => SOME paths
				 | PRIM => NONE)
	fun get_isTarget unit = (case get_unit unit
				   of UNIT {isTarget, isBasis, ...} => (not isBasis) andalso isTarget
				    | PRIM => false)
	fun get_status unit = !(#status(lookup unit))
	fun set_status (unit,s) = (#status(lookup unit)) := s

	fun get_import_direct unit = 
	    (Dag.parents(!graph,unit)
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun get_import_transitive unit = 
	    (rev(Dag.ancestors(!graph,unit))
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun get_import unit =
	    let fun isDirect import = if Dag.has_edge (!graph, import, unit)
					  then Compiler.DIRECT
				      else Compiler.INDIRECT
		val imports = get_import_transitive unit
	    in  map (fn import =>
		     (case get_unit import
			of UNIT {paths, ...} => Compiler.FILE (paths, isDirect import)
			 | PRIM => Compiler.PRIM (isDirect import))) imports
	    end
	fun get_dependent_direct unit = 
	    (Dag.children(!graph,unit)
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun get_size unit = 
	    (Dag.nodeWeight(!graph,unit)
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun refreshDag g =
	    Dag.refresh g
	    handle Dag.Cycle nodes =>
		(chat "  Cycle detected in mapfile: ";
		 chat_strings 20 nodes;
		 chat "\n";
		 error "Cycle detected in mapfile")
		 | Dag.UnknownNode node =>
		let val msg = "  " ^ node ^ " not defined in mapfile."
		in
		    chat msg;
		    chat "  Used by ";
		    chat_strings 20 (Dag.children' (g, node));
		    chat "\n";
		    error msg
		end
		  
	type collapse = {maxWeight : int, maxParents : int, maxChildren : int}
	(* makeGraph' : string * {maxWeight:int, maxParents:int, maxChildren:int} option -> string *)
	(* Generate dot(1) representation of current graph, name based on mapfile. *)
	fun makeGraph'(mapfile : string, collapseOpt) = 
	    let
		val start = Time.now() 
		val dot = Paths.mapfileToDot mapfile
		val out = TextIO.openOut dot
		val g = !graph
		val g = (case collapseOpt of
			     NONE => g
			   | SOME collapse => let val g = Dag.collapse(g, collapse)
						  val _ = chat "Collapsed graph.\n"
					      in  g
					      end)
		val _ = Dag.makeDot{out = out, 
				    graph = g,
				    status = (fn n => (case (get_status n) of
							   DONE _ => Dag.Black
							 | WORKING' _ => Dag.Gray
							 | PROCEEDING _ => Dag.Gray
							 | WORKING _ => Dag.Gray
							 | PENDING' _ => Dag.Gray
							 | PENDING _ => Dag.Gray
							 | READY _ => Dag.White
							 | WAITING => Dag.White))}
		val _ = TextIO.closeOut out
		    
		val diff = Time.toReal(Time.-(Time.now(), start))
		val diff = (Real.realFloor(diff * 100.0)) / 100.0
		val _ = if (!chat_verbose)
			    then chat ("Generated " ^ dot ^ " in " ^ (Real.toString diff) ^ " seconds.\n") 
			else ()
	    in  dot
	    end

	fun refreshGraph () = refreshDag (!graph)
	fun reduceGraph () =
	    let
		val reducedGraph = Dag.removeTransitive (!graph)
		val _ = graph := reducedGraph
	    in
		()
	    end
	fun graphSize () = (Dag.numNodes (!graph), Dag.numEdges (!graph))
    end

    (* getLibDir : unit -> string *)
    val getLibDir = Dirs.getLibDir o Dirs.getDirs

    (* createDirectories : unit -> unit *)
    fun createDirectories () =
	let val unit_names = list_units()
	    val unit_paths = List.mapPartial get_paths unit_names
	    val unit_dirs = map Paths.tiltDirs unit_paths
	    val _ = chat "Creating directories.\n"
	    val _ = foldl Dirs.createDirs Dirs.emptyCache unit_dirs
	in
	    ()
	end
    
    fun split_line dropper line = 
	let val fields = String.fields Char.isSpace line
	    fun filter [] = []
	      | filter (x::y) = if (size x = 0)
				    then filter y
				else if dropper x 
					 then []
				     else
					 x :: (filter y)
	in 	filter fields
	end
    
    (* mapfilePath : string -> string list *)
    fun mapfilePath currentDir = [currentDir, getLibDir()]

    (* readAssociation : bool * string -> compunit list *)
    fun readAssociation (isBasis, mapfile) = 
	let
	    val dir = Dirs.dir mapfile
	    val findMapfile = Dirs.accessPath (mapfilePath dir, [OS.FileSys.A_READ])
	    fun relative file = OS.Path.joinDirFile {dir=dir, file=file}
	    fun mkPaths (unit, filebase) = Paths.sourceUnitPaths {unit=unit, file=relative filebase}
	    val is = TextIO.openIn mapfile
	    fun dropper s = let val len = size s
			    in  String.sub(s,0) = #";" orelse
				(len >= 2 andalso String.substring(s,0,2) = "//")
			    end
	    fun compunit (unitname, filebase, isTarget) =
		UNIT {paths = mkPaths (unitname, filebase),
		      isTarget = bootstrap orelse isTarget,
		      isBasis = isBasis}
	    fun fail (n,line,msg) = error ("Line " ^ (Int.toString n) ^ " of " ^ mapfile ^
					   " " ^ msg ^ ": " ^ line ^ "\n")
	    fun loop (n, acc) = 
		if (TextIO.endOfStream is)
		    then rev acc
		else 
		    let val line = TextIO.inputLine is
		    in  (case (split_line dropper line) of
			     ["#include", innerMapfile] => 
				 (case findMapfile innerMapfile
				    of NONE => fail (n,line,"includes an unreadable or non-existent file")
				     | SOME innerMapfile =>
					let
					    val innerAssociation = readAssociation (isBasis, innerMapfile)
					in
					    loop (n+1, (rev innerAssociation) @ acc)
					end)
			   | [unitname] =>
			         if unitname = primUnitName
				     then loop (n+1, PRIM :: acc)
				 else fail (n,line,"specifies no source for a unit other than " ^ primUnitName)
			   | [unitname, filebase] =>
				 if unitname <> primUnitName
				     then loop (n+1, compunit (unitname, filebase, false) :: acc)
				 else fail (n,line,"uses the special unit name " ^ primUnitName)
			   | [unitname, filebase, "TARGET"] =>
				 if unitname <> primUnitName
				     then loop (n+1, compunit (unitname, filebase, true) :: acc)
				 else fail (n,line,"uses the special unit name " ^ primUnitName)
			   | [] => loop (n, acc)
			   | _ => fail (n,line,"is ill-formed"))
		    end
	    val result = loop (0, [])
	    val _ = TextIO.closeIn is
	in  result
	end

    local
	fun parse_depend failure file =
	    let val ins = TextIO.openIn file
		val line = TextIO.inputLine ins
		val sz = size line
		val _ = TextIO.closeIn ins
		val depend_str = "(*$import"
		val depend_str_sz = size depend_str
	    in
		if (sz >= depend_str_sz andalso 
		    String.substring(line,0,depend_str_sz) = depend_str) then
		    let 
			fun dropper s = ("(*"; s = "*)")
		    in  
			split_line dropper 
			(String.substring(line,depend_str_sz,sz-depend_str_sz))
		    end
		else 
		    (print ("Warning: first line of " ^ file ^ 
			    " is not import directive.\n");
		     print "Calling parser to process.\n";
		     failure file)
	    end
    in
	fun parse_impl_import file = 
	    parse_depend Compiler.parse_impl_import file
	fun parse_inter_include file = 
	    parse_depend Compiler.parse_inter_import file
    end

    (* setMapping : string * bool -> unit *)
    (* Build graph from mapFile.  If getImportsAndBasis is true, add (*$import *) edges and
     * basis vertices and edges. *)
    fun setMapping (mapFile, getImportsAndBasis) =
	let val _ = Update.flushAll()
	    val _ = reset_graph()
	    val association = readAssociation (false, mapFile)
	    val association = (case (getImportsAndBasis, basisMapfile())
				 of (true, SOME mapfile) =>
				     readAssociation (true, mapfile) @ association
				  | _ => association)
	    fun mapper (n, compunit) =
		let 
		    val nodeWeight = unitSize compunit
		    val info = 
			{position = n,
			 unit = compunit,
			 status = ref WAITING}
		in  add_node(unitName compunit, nodeWeight, info)
		end
	    val _ = Listops.mapcount mapper association
	    val _ = chat ("Mapfile " ^ mapFile ^ " with " ^ (Int.toString (length association)) 
			  ^ " units processed.\n")
	    fun read_import unit =
		let
		    val imports =
			(case get_unit unit
			   of UNIT {paths, isBasis, ...} =>
			       let val imports = parse_impl_import(Paths.sourceFile paths)
				   val interfaceFile = Paths.interfaceFile paths
				   val includes = if Cache.exists interfaceFile
						      then parse_inter_include interfaceFile
						  else nil
				   val extra = if isBasis then nil else basisImports
			       in  imports @ includes @ extra
			       end
			    | PRIM => primImports)
		    val _ = app (fn import => add_edge(import,unit)) imports
		    val _ = set_status(unit, if (null imports) then READY (Time.now()) else WAITING)
		in  ()
		end
	    fun check_import unit =
		let val import_tr = get_import_transitive unit
		    val next_pos = get_position unit
		    fun check import = if (get_position import < next_pos) then ()
				       else
					   error ("Mapfile file ordering is inconsistent because " ^
						  unit ^ " imports " ^ import ^ " but precedes it.")
		in  app check import_tr
		end
	    
	in
	    if getImportsAndBasis then
		let val units = list_units()
		    val _ = app read_import units
		    val _ = chat ("Imports read.\n")
		    val _ = refreshGraph()
		    val (numNodes, numEdges) = graphSize()
		    val _= (chat "Dependency graph computed: ";
			    chat (Int.toString (numNodes)); chat " nodes and ";
			    chat (Int.toString (numEdges)); chat " edges.\n")
(*			  
		    val _ = reduceGraph()
		    val (numNodes', numEdges') = graphSize()
		    val _ = (chat "Reduced dependency graph computed: ";
			     chat (Int.toString (numNodes')); chat " nodes and ";
			     chat (Int.toString (numEdges')); chat " edges.\n")
*)
		    val _ = chat "Not reducing dependency graph.\n"
		    val _ = app check_import units
		in  ()
		end
	    else ()
	end
    fun list_targets() = let val allUnits = list_units()
			 in  List.filter get_isTarget allUnits
			 end

    (* makeGraph' : string * {maxWeight:int, maxParents:int, maxChildren:int} option -> string *)
    (* Generate dot(1) representation of graph in mapfile. *)
    fun makeGraph(mapfile : string, collapseOpt) = 
	let val _ = setMapping(mapfile, true)
	in  makeGraph'(mapfile, collapseOpt)
	end

    fun makeGraphShow(mapfile : string, collapse) = 
	let val dot = makeGraph (mapfile, collapse)
	    val ps = Paths.mapfileToPs mapfile
	    val _ = OS.Process.system ("dot -Tps " ^ dot ^ " -o " ^ ps)
	    val _ = chat ("Generated " ^ ps ^ ".\n")
	    val _ = OS.Process.system ("gv " ^ ps ^ "&")
	    val _ = chat ("Invoked gv on " ^ ps ^ ".\n")
	in  ps
	end

    local
	val workingLocal = ref ([] : (string * (unit -> bool)) list)
	val idleTimes : Time.time list ref =
	    ref nil
	val workingTimes : Time.time list ref =
	    ref nil
	    
	fun addDuration r (_:Comm.identity, duration) = r := duration :: (!r)
	val add_idle_time = addDuration idleTimes
	val add_working_time = addDuration workingTimes
	    
	datatype slave_status =
	    WORKING_SLAVE of Time.time	(* Slave is working.  Start time. *)
	  | IDLE_SLAVE of Time.time	(* Slave is idle.  Start time. *)

	type slave_info = {status : slave_status ref,
			   in_channel : Comm.in_channel,
			   out_channel : Comm.out_channel}

	structure SlaveMap = SplayMapFn(type ord_key = Comm.identity
					val compare = Comm.compare)
	    
	val slaveMap : slave_info SlaveMap.map ref =
	    ref SlaveMap.empty
	    
	fun resetSlaveMap () = slaveMap := SlaveMap.empty
	fun add_slave (slave, info) = slaveMap := SlaveMap.insert (!slaveMap, slave, info)
	fun lookup_slave slave =
	    (case SlaveMap.find (!slaveMap, slave)
	       of NONE => error ("slave " ^ Comm.name slave ^ " missing")
		| SOME info => info)
	fun slave_known slave = isSome (SlaveMap.find (!slaveMap, slave))
	fun list_slaves () = map #1 (SlaveMap.listItemsi (!slaveMap))
	    
	fun get_slave_status slave = !(#status(lookup_slave slave))
	fun set_slave_status (slave, status) = (#status(lookup_slave slave) := status)
	fun get_in_channel slave = #in_channel(lookup_slave slave)
	fun get_out_channel slave = #out_channel(lookup_slave slave)

	fun markSlaveIdle slave =
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
	fun markSlaveWorking slave =
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

	fun partition_slaves slaves =
	    let
		fun folder (slave, (w,i)) =
		    (case (get_slave_status slave)
		       of WORKING_SLAVE _ => (slave::w, i)
			| IDLE_SLAVE _ => (w, slave::i))
		val result = foldl folder (nil, nil) slaves
	    in  result
	    end

	fun findNewSlaves () =
	    let
		val platform = Target.getTargetPlatform()
		val flags = Comm.getFlags()
		val flushAll = Comm.FLUSH_ALL (platform, flags)
		    
		exception Stop
		fun addSlave slave =
		    let val _ = if slave_known slave then raise Stop
				else ()
			val channel = Comm.toMaster slave
			val in_channel = if not (Comm.exists channel) then raise Stop
					 else Comm.openIn channel
			val name = Comm.name slave
			val _ = chat ("  [Sending FLUSH_ALL to " ^ name ^ "]\n")
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
	(* Kill active slave channels to restart and send flush slave's file caches *)
	fun resetSlaves() = let val _ = resetSlaveMap()
				val _ = workingLocal := nil
				val _ = Comm.destroyAllChannels()
			    in  ()
			    end
	(* Asynchronously ask for whether there are slaves ready *)
	fun pollForSlaves (do_ack_interface, do_ack_done, do_ack_local): int * int= 
	    let
		val maxWorkingLocal = 2
		val newWorkingLocal = List.filter (fn (unit, done) =>
						   if done() then (do_ack_local unit; false)
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
			    | SOME (Comm.ACK_INTERFACE unit) => do_ack_interface (Comm.name slave, unit)
			    | SOME (Comm.ACK_DONE (unit, plan)) =>
			       (do_ack_done (Comm.name slave, unit, plan);
				markSlaveIdle slave)
			    | SOME (Comm.ACK_ERROR unit) =>
			       (chat "\n\nSlave "; chat (Comm.name slave); 
				chat " signalled error during job "; 
				chat unit; chat "\n";
				error "slave signalled error")
			    | SOME (Comm.FLUSH_ALL _) => error ("slave " ^ (Comm.name slave) ^ " sent flushAll")
			    | SOME (Comm.FLUSH _) => error ("slave " ^ (Comm.name slave) ^ " sent flush")
			    | SOME (Comm.REQUEST _) => error ("slave " ^ (Comm.name slave) ^ " sent request"))
		    workingSlaves
		val (_, idleSlaves) = partition_slaves slaves
	    in  (length idleSlaves, maxWorkingLocal - length (!workingLocal))
	    end
	(* Should only be used when we are below our limit on local processes. *)
	fun useLocal (unit, f) =
	    let val newLocal = (unit, Background.background f)
		val _ = workingLocal := (newLocal :: (!workingLocal))
	    in  ()
	    end
	(* Works only if there are slaves available *)
	fun useSlave (showSlave, forMySlave, forOtherSlaves) = 
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
	fun closeSlaves () =
	    let
		val slaves = list_slaves()
		val (working, idling) = partition_slaves slaves
		val _ = if null working then ()
			else (chat "The following slaves are still working: ";
			      chat_strings 40 (map Comm.name working);
			      chat "\n";
			      error ("slaves still working - closeSlaves"))
			    
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
	fun slaveTimes() = (!workingTimes, !idleTimes)
    end

    fun statusName WAITING = "waiting"
      | statusName (READY _) = "ready"
      | statusName (PENDING _) = "pending"
      | statusName (WORKING _) = "working"
      | statusName (PROCEEDING _) = "proceeding"
      | statusName (WORKING' _) = "working'"
      | statusName (PENDING' _) = "pending'"
      | statusName (DONE _) = "done"

    fun badStatus (unit, status, what) =  error ("unit " ^ unit ^ " was " ^ statusName status ^ "; " ^ what)

    fun getReadyTime unit =
	(case get_status unit
	   of READY t => t
	    | PENDING (t, _) => t
	    | WORKING (t, _) => #1 t
	    | PROCEEDING (t, _) => #1 t
	    | PENDING' (t, _) => #1 t
	    | WORKING' (t, _) => #1 t
	    | status => badStatus (unit, status, "getting ready time"))

    fun getWorkingTime unit =
	(case get_status unit
	   of WORKING (t, _) => #2 t
	    | PROCEEDING (t, _) => #2 t
	    | status => badStatus (unit, status, "getting working time"))

    fun getWorking'Time unit =
	(case get_status unit
	   of WORKING' (t, _) => #3 t
	    | status => badStatus (unit, status, "getting working' time"))

    fun getTotalTimes unit =
	(case get_status unit
	   of DONE t => t
	    | status => badStatus (unit, status, "getting total times"))

    fun getSlaveTime unit =
	(case get_status unit
	   of PENDING' (t, _) => #2 t
	    | WORKING' (t, _) => #2 t
	    | DONE t => #2 t
	    | status => badStatus (unit, status, "getting slave time"))

    val getMasterTime = #3 o getTotalTimes
	     
    fun getPlan unit =
	(case get_status unit
	   of PENDING (_, plan) => plan
	    | WORKING (_, plan) => plan
	    | PROCEEDING (_, plan) => plan
	    | PENDING' (_, plan) => plan
	    | WORKING' (_, plan) => plan
	    | status => badStatus (unit, status, "getting plan"))
	     
    fun markReady unit = 
	(case get_status unit
	   of WAITING => set_status (unit, READY (Time.now()))
	    | READY _ => ()
	    | status => badStatus (unit, status, "making ready"))

    fun markPending (unit, plan) =
	(case get_status unit
	   of READY t => set_status (unit, PENDING (t, plan))
	    | status => badStatus (unit, status, "making pending"))
	
    fun markWorking unit =
	(case get_status unit
	   of PENDING (t, plan) => set_status (unit, WORKING ((t, Time.now()), plan))
	    | status => badStatus (unit, status, "making working"))

    fun markWorking' unit =
	(case get_status unit
	   of PENDING' ((t, t'), plan) => set_status (unit, WORKING' ((t, t', Time.now()), plan))
	    | status => badStatus (unit, status, "making working'"))
    
    fun enableChildren parent = 
	let 
	    fun enableReady child =
		let val imports = get_import_direct child (* no need to check transitively *)
		    fun hasInterface unit =
		        unit = parent orelse (case get_status unit
					        of PROCEEDING _ => true
					 	 | PENDING' _ => true
						 | WORKING' _ => true
						 | DONE _ => true
						 | _ => false)
		    val ready = Listops.andfold hasInterface imports
		in  ready andalso 
		    (case get_status child
		       of WAITING => (markReady child; true)
			| _ => false)  (* The child status may be beyond READY since
					  the same parent might call enableChildren twice. *)
		end
	    val enabled = List.filter enableReady (get_dependent_direct parent)
	in  if (!showEnable andalso (not (null enabled)))
		then (chat "  [Interface of "; chat parent;
		      chat " has enabled these units: ";
		      chat_strings 40 enabled; 
		      chat "]\n")
	    else ()
	end

    fun markPending' (unit, plan) =
	let fun newT (t,t') = (t, Time.-(Time.now(), t'))
	    val _ = case get_status unit
		      of READY t => set_status (unit, PENDING' ((t, Time.zeroTime), plan))
		       | WORKING (t, _) => set_status (unit, PENDING' (newT t, plan))
		       | PROCEEDING (t, _) => set_status (unit, PENDING' (newT t, plan))
		       | status => badStatus (unit, status, "making pending'")
	    val _ = enableChildren unit
	in  ()
	end
    
    fun markProceeding unit =
	let val _ = case get_status unit
		      of WORKING arg => set_status (unit, PROCEEDING arg)
		       | status => badStatus (unit, status, "making proceeding")
	    val _ = enableChildren unit
	in  ()
	end

    fun markDone unit =
	let
	    val now = Time.now()
	    fun since t = Time.- (now, t)
	    val z = Time.zeroTime
	    val (times as (totalTime, slaveTime, masterTime)) =
		case get_status unit
		  of READY t => (since t, z, z)
		   | WORKING ((t,t'), _) => (since t, since t', z)
		   | PROCEEDING ((t, t'), _) => (since t, since t', z)
		   | WORKING' ((t, t', t''), _) => (since t, t', since t'')
		   | status => badStatus (unit, status, "making done")
	    val _ = set_status (unit, DONE times)
	    val _ = enableChildren unit
	in  ()
	end

    (* sendToSlave : plan -> bool *)
    fun sendToSlave plan =
	let
	    fun forSlave Update.ELABORATE = true
	      | forSlave Update.GENERATE = true
	      | forSlave _ = false
	in
	    List.exists forSlave plan
	end

    fun analyzeReady unit =		(* Unit is ready so its imports exist *)
	(case get_unit unit
	   of UNIT {paths, ...} =>
	       let
		   val imports = get_import_direct unit (* no need to check transitively *)
		   val import_paths = List.mapPartial get_paths imports
		   val (status, plan) = Update.plan (paths, import_paths)
		   val _ = if Update.interfaceUptodate status
			       then enableChildren unit
			   else ()
		   val _ =
		       if null plan then
			   markDone unit
		       else
			   if sendToSlave plan then
			       markPending (unit, plan)
			   else
			       markPending' (unit, plan)
	       in  ()
	       end
	    | PRIM => (enableChildren unit; markDone unit))
    
    (* waiting, pending, working, proceeding, pending', working' - ready and done not included *)
    type state = string list * string list * string list * string list * string list * string list
    datatype result = PROCESSING of Time.time * state  (* All slaves utilized *)
                    | IDLE of Time.time * state * int  (* Num slaves idle and there are waiting jobs *)
	            | COMPLETE of Time.time 
    fun stateSize ((a,b,c,d,e,f) : state) = ((length a) + (length b) + (length c) + (length d) +
					     (length e) + (length f))
    fun resultToTime (PROCESSING (t,_)) = t
      | resultToTime (IDLE (t,_,_)) = t
      | resultToTime (COMPLETE t) = t
	
    fun stateDone(([],[],[],[],[],[]) : state) = true
      | stateDone _ = false
    fun partition (units : string list) =
	let
	    fun folder (unit,(wa,r,pe,wo,pr,pe',wo',d)) = 
		(case (get_status unit) of
		     WAITING => (unit::wa,r,pe,wo,pr,pe',wo',d)
		   | READY _ => (wa,unit::r,pe,wo,pr,pe',wo',d)
		   | PENDING _ => (wa,r,unit::pe,wo,pr,pe',wo',d)
		   | WORKING _ => (wa,r,pe,unit::wo,pr,pe',wo',d)
		   | PROCEEDING _ => (wa,r,pe,wo,unit::pr,pe',wo',d)
		   | PENDING' _ => (wa,r,pe,wo,pr,unit::pe',wo',d)
		   | WORKING' _ => (wa,r,pe,wo,pr,pe',unit::wo',d)
		   | DONE _ => (wa,r,pe,wo,pr,pe',wo',unit::d))
	    val result = foldl folder ([],[],[],[],[],[],[],[]) units
	in  result
	end
    fun getReady waiting =
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
	    val _ = if (null done) orelse (not (!chat_verbose))
			then ()
		    else (chat "  [These files are up-to-date already:";
			  chat_strings 40 done;
			  chat "]\n")
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
    fun waitForSlaves () = 
	let
	    fun ack_inter (name, unit) =
		let val _ = markProceeding unit
		    val diff = Time.toReal (Time.-(Time.now(), getWorkingTime unit))
		    val diff = (Real.realFloor(diff * 100.0)) / 100.0
		    val _ = chat ("  [" ^ name ^ " compiled interface of " ^ unit ^ " in " ^
				  (Real.toString diff) ^ " seconds]\n")
		in  ()
		end
	    fun ack_done (name, unit, plan) =
		if null plan then
		    let
			val _ = markDone unit
			val diff = Time.toReal (getSlaveTime unit)
			val diff = (Real.realFloor(diff * 100.0)) / 100.0
		    in  chat ("  [" ^ name ^ " compiled " ^ unit ^ " in " ^
			      (Real.toString diff) ^ " seconds]\n")
		    end
		else
		    let val _ = markPending' (unit, plan)
			val diff = Time.toReal(getSlaveTime unit)
			val diff = (Real.realFloor(diff * 100.0)) / 100.0
			val _ = chat ("  [" ^ name ^ " compiled " ^ unit  ^ " to assembly in " ^
				      (Real.toString diff) ^ " seconds]\n")
		    in  ()
		    end
	    fun ack_local unit =
		let val _ = markDone unit
		    val diff = Time.toReal (getMasterTime unit)
		    val diff = (Real.realFloor(diff * 100.0)) / 100.0
		    val _ = Help.chat ("  [Master locally finished " ^ unit ^ " in " ^
				       (Real.toString diff) ^ " seconds]\n")
		in  ()
		end
	    val numSlaves = pollForSlaves (ack_inter, ack_done, ack_local)
	in  numSlaves 
	end
    fun finalReport units =
	let
	    fun toString r = Real.toString (Real.realFloor (r * 100.0) / 100.0)
	    val _ = chat "------- Times to compile files -------\n"
	    fun stats nil = "unavailable"
	      | stats (L : real list) =
		let val v = Vector.fromList L
		in  String.concat ["min ", toString (VectorStats.min v),
				   " max ", toString (VectorStats.max v),
				   " mean ", toString (VectorStats.mean v),
				   " absdev ", toString (VectorStats.absdev v),
				   " (n=" ^ Int.toString (Vector.length v) ^ ")"]
		end
	    fun mapper unit =
		let val (total, slave, master) = getTotalTimes unit
		    val total' = Time.+ (slave, master)
		    val idle = Time.- (total, total')
		    val total' = Time.toReal total'
		    val idle = Time.toReal idle
		in  if total' > 0.0 then SOME (unit, idle, total') else NONE
		end
	    val unsorted = List.mapPartial mapper units
	    val (slaveWork, slaveIdle) = slaveTimes()
	    val slaveWork = map Time.toReal slaveWork
	    val slaveIdle = map Time.toReal slaveIdle
	    val _ = chat (Int.toString (length unsorted) ^ " of  " ^
			  Int.toString (length units) ^ " units needed compilation.\n" ^
			  "  Unit work times (in seconds): " ^ stats (map #3 unsorted) ^ "\n" ^
			  "  Unit wait times (in seconds): " ^ stats (map #2 unsorted) ^ "\n" ^
			  "  Slave work times (in seconds): " ^ stats slaveWork ^ "\n" ^
			  "  Slave idle times (in seconds): " ^ stats slaveIdle ^ "\n")
	    fun greater ((_,_,x),(_,_,y)) = x > (y : real)
	    val sorted = ListMergeSort.sort greater unsorted
	    val (underOne,overOne) = List.partition (fn (_,_,t) => t < 1.0) sorted
	    val (oneToTen, overTen) = List.partition (fn (_,_,t) => t < 10.0) overOne
	    val (tenToThirty, overThirty) = List.partition (fn (_,_,t) => t < 30.0) overTen
	    val _ = (chat (Int.toString (length underOne));
		     chat " files under 1.0 second.\n")
	    val _ = (chat (Int.toString (length oneToTen));
		     chat " files from 1.0 to 10.0 seconds.\n")
	    val _ = (chat (Int.toString (length tenToThirty));
		     chat " files from 10.0 to 30.0 seconds:  ";
		     chat_strings 40 (map #1 tenToThirty); chat "\n")
	    val _ = (chat (Int.toString (length overThirty));
		     chat " files over 30.0 seconds:\n")
	    val _ = app (fn (unit,idle,total) =>
			 chat ("  " ^ unit ^ 
			       (Util.spaces (20 - (size unit))) ^ " took " ^ 
			       (toString total) ^ " seconds of work and waited for " ^
			       (toString idle) ^ " seconds.\n")) overThirty
	in  ()
	end
    fun execute (target, imports, plan) = ignore (foldl Update.execute (Update.init (target, imports)) plan)
    (* Assumes that all units in state's pending' list are PENDING' and not PRIM. *)
    fun useLocals (localsLeft, state : state) : int =
	let
	    fun useOne unit =
		let val target = valOf (get_paths unit)	(* not PRIM unit *)
		    val imports = get_import unit
		    val plan = getPlan unit
		    val _ = markWorking' unit
		    val _ = Update.flush (target, plan)
		in  useLocal (unit, fn () => execute (target, imports, plan))
		end
	    fun useSome (0, _) = 0
	      | useSome (n, nil) = n
	      | useSome (n, unit :: rest) = (useOne unit;
					     useSome (n-1, rest))
	    val (_, _, _, _, pending', _) = state
	in  useSome (localsLeft, pending')
	end
    (* Assumes that all units in state's pending list are PENDING and not PRIM. *)
    fun useSlaves (slavesLeft, state : state) : int =
	let
	    fun useOne unit =
		let fun showSlave name = chat ("  [Calling " ^ name ^ 
					       " to compile " ^ unit ^ "]\n")
		    val target = valOf (get_paths unit)	(* not PRIM unit *)
		    val imports = get_import unit
		    val plan = getPlan unit
		    val _ = markWorking unit
		    val _ = Update.flush (target, plan)
		in  useSlave (showSlave,
			      Comm.REQUEST (target, imports, plan),
			      SOME (Comm.FLUSH (target, plan)))
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
    fun once mapfile = 
	let
	    val _ = case (Target.native(), Help.wantBinaries())
		      of (_, false) => chat "Warning: flags prevent full compile\n"
		       | (false, _) =>
			  if !Help.keepAsm andalso Help.wantAssembler() then
			      (chat "Warning: only compiling to assembly because non-native\n";
			       Help.uptoAsm := true)
			  else
			      (chat "Warning: only compiling to interfaces because non-native\n";
			       if Help.wantAssembler() then Help.uptoElaborate := true
			       else ())
		       | _ => ()
	    val _ = resetSlaves()
	    val _ = setMapping(mapfile, true)
	    val _ = createDirectories()
	    val finalTargets = let val mapfileTargets = list_targets()
			       in  if (null mapfileTargets)
				       then [List.last(list_units())] 
				   else mapfileTargets
			       end
	    fun folder (unit,acc) = 
		let val imports = get_import_transitive unit
		in  StringOrderedSet.cons(unit, foldl StringOrderedSet.cons acc imports)
		end
	    val units = rev (StringOrderedSet.toList (foldl folder StringOrderedSet.empty finalTargets))

	    val _ = if (!chat_verbose)
			then (chat (Int.toString (length units));
			      chat " necessary units: ";
			      chat_strings 20 units; print "\n")
		    else ()
	    val _ = Help.showTime (true,"Start compiling files")
		
            (* make_package : string -> Prelink.package.  Assumes unit is not PRIM. *)
            fun make_package unit =
		let val paths = valOf (get_paths unit) (* not PRIM unit *)
		    val infoFile = Paths.infoFile paths
		    val (_, info) = InfoCache.read infoFile
		in
		    {unit = unit,
		     imports = #imports info,
		     exports = #exports info}
		end
	    
	    (* makeExe : string -> unit *)
	    exception Stop
	    fun makeExe unit =		(* Assumes unit is not PRIM *)
		let val _ = Help.showTime (true, "Start linking on " ^ unit)
		    val requiredUnits = (get_import_transitive unit) @ [unit]
		    val requiredUnits = List.filter (fn u => u <> primUnitName) requiredUnits

		    (* Check unit environments *)
		    val _ = chat ("  [Checking that unit environments match up.]\n")
		    val requiredPackages = map make_package requiredUnits
		    val _ = Prelink.checkTarget (unit, requiredPackages)

		    (* Generate startup code *)
		    val _ = if (not bootstrap) andalso Help.wantAssembler() then () else raise Stop
		    val _ = chat ("  [Generating startup code.]\n")
		    val paths = valOf (get_paths unit) (* not PRIM unit *)
		    val linkAsmFile = Paths.linkAsmFile paths
		    val _ = Compiler.link (linkAsmFile, requiredUnits)

		    (* Assemble startup code *)
		    val _ = if Help.wantBinaries() then () else raise Stop
		    val _ = chat ("  [Assembling startup code.]\n")
		    val linkObjFile = Paths.linkObjFile paths
		    val _ = Tools.assemble (linkAsmFile, linkObjFile);

		    (* Link *)
		    val linkExeFile = if (!Tools.profile)
					  then Paths.linkProfExeFile paths
				      else Paths.linkExeFile paths
		    val objectFiles = (map (Paths.objFile o valOf o get_paths) requiredUnits) @ [linkObjFile]
		    val _ = (chat "Manager calling linker with: ";
			     if (!chat_verbose)
				 then chat_strings 30 requiredUnits
			     else chat_strings 30 ["..." ^ (Int.toString ((length requiredUnits) - 1)) ^ " units...",
						   unit];
			     chat "\n")
		    val _ = Tools.link (objectFiles, linkExeFile)
		in
		    ()
		end handle Stop => ()
	    val initialState : state = (units, [], [], [], [], [])
	in  {setup = fn _ => initialState,
	     step = step, 
	     complete = (fn _ =>
			 let val _ = closeSlaves()
			     val _ = if !chat_verbose
					 then finalReport units
				     else ()
			 in  app makeExe finalTargets
			 end)}
	end
    fun showCheckPoint state =
	let val left = stateSize state
	    val msg =
		if (!checkPointVerbose) then
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
	in  Help.showTime (false, msg)
	end
    fun run mapfile =
	let val {setup,step = step : state -> result,complete} = once mapfile
	    val initialState = setup()
	    val lastGraphTime = ref (Time.now())
	    val lastIdleTime = ref NONE
	    val last = ref (PROCESSING(Time.now(),initialState))
	    fun loop state =
	      let val prev = !last
		  val cur = step state
		  val _ = last := cur
		  val thisTime = resultToTime cur
		  val diff = Time.toReal(Time.-(thisTime,!lastGraphTime))
	      in
		(case cur of
		    COMPLETE _ => complete()
		  | PROCESSING (t,state) => 
			let val _ = (case !lastIdleTime of
					 NONE => ()
				       | SOME t' => let val diff = Time.toReal(Time.-(t,t'))
							val diff = (Real.realFloor(diff * 100.0)) / 100.0
							val _ = lastIdleTime := NONE
						    in  chat ("  [Idled for " ^ (Real.toString diff) ^ 
							      " seconds.]\n")
						    end)
			    val _ = (case prev of
					 PROCESSING _ => ()
				       | _ => chat "  [All processors working!]\n")
			    val _ = if (diff > 30.0)
					then (makeGraph'(mapfile,NONE);
					      showCheckPoint state;
					      lastGraphTime := (Time.now()))
				    else ()
			in  Platform.sleep 0.1;
			    loop state
			end
		  | IDLE(t, state as (waiting, pending, working, proceeding, pending', working'), numIdle) =>
			((case !lastIdleTime of
			      NONE => lastIdleTime := SOME t
			    | _ => ());
			 (case prev of
			      IDLE _ => ()
			    | _ => (chat "  [Idling: ";
				    chat (Int.toString numIdle);
				    chat " ready slaves.  ";
				    chat (Int.toString (length waiting));
				    chat " waiting jobs.\n";
				    chat "     Waiting for interfaces of ";
				    chat_strings 20 (pending @ working); chat ".";
				    chat "]\n";
				    if (diff > 15.0)
					then (makeGraph'(mapfile,NONE);
					      showCheckPoint state;
					      lastGraphTime := (Time.now()))
				    else ();
				    Platform.sleep 0.1));
			   loop state))
	      end
	in loop initialState
	end

    fun purge_help (mapfile : string, what : string, unitFiles : (Paths.unit_paths -> string) list) =
	let
	    val mapfileFiles = [Paths.mapfileToDot, Paths.mapfileToPs]
	    val _ = setMapping(mapfile, false)
	    val units = list_units()
	    val _ = (print "Purging "; print mapfile; print what; print "\n")
	    val dirs = Dirs.getDirs()
	    fun deletable file = not (Dirs.isSystemFile (dirs, file))
	    (* On AFS it is a lot faster to do access() on a non-existent
	     * file than remove().
	     *)
	    fun kill file = if (deletable file andalso
				OS.FileSys.access (file, []) andalso
				OS.FileSys.access (file, [OS.FileSys.A_WRITE]))
				then OS.FileSys.remove file
			    else ()
	    fun remove unit = (case get_paths unit
				 of SOME paths =>
				     let val files = map (fn f => f paths) unitFiles
				     in  app kill files
				     end
				  | NONE => ())
	    val files' = map (fn f => f mapfile) mapfileFiles
	in
	    app remove units;
	    app kill files'
	end
    val any = [Paths.infoFile,
	       Paths.fileToBackup o Paths.infoFile,
	       Paths.ilFile,
	       Paths.ilToUnself o Paths.ilFile,
	       Paths.fileToBackup o Paths.ilFile]
    val target = [Paths.asmFile,
		  Paths.asmzFile,
		  Paths.objFile,
		  Paths.linkAsmFile,
		  Paths.linkObjFile,
		  Paths.linkExeFile,
		  Paths.linkProfExeFile]
    fun purge mapfile = purge_help (mapfile, " (binaries)", target)
    fun purgeAll mapfile = purge_help (mapfile, " (binaries and interfaces)", any @ target)

end
