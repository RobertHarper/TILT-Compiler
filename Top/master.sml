(*$import MASTER Communication TopHelp Prelink LinkParse Tools Background OS List Platform Dirs Target Paths *)

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


structure Master :> MASTER =
struct
    val error = fn s => Util.error "master.sml" s

    val showEnable = Stats.ff "ShowEnable"
	
    structure S = Update
	
    structure Comm = Comm(val slaveTidOpt = NONE)
    structure Cache = UpdateHelp.Cache
    structure InfoCache = UpdateHelp.InfoCache
    structure StringOrderedSet = Help.StringOrderedSet
	
    val chat = Help.chat
    val chat_strings = Help.chat_strings
    val chat_verbose = Help.chatVerbose

    (* We measure the time taken to go from READY to DONE. *)
    datatype status =
	WAITING					(* Waiting for imports to be up to date. *)
      | READY of Time.time			(* Ready for dependency analysis; imports are up to date.  Ready time. *)
      | PENDING of Time.time * Update.plan	(* Pending work on a slave.  Ready time. Work to do.*)
      | WORKING of Time.time * Update.plan	(* Slave working.  Ready time. *)
      | PROCEEDING of Time.time * Update.plan	(* Slave working, but interface up to date.  Ready time. *)
      | PENDING' of Time.time * Update.plan	(* Pending work on the master.  Ready time. Work to do. *)
      | WORKING' of Time.time * Update.plan	(* Master working.  Ready time. *)
      | DONE of Time.time			(* Total compilation time. *)
	
    local

	val graph = ref (Dag.empty() : {position : int,
					paths : Paths.unit_paths,
					status : status ref,
					isTarget : bool} Dag.graph)
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
	fun get_paths unit = #paths(lookup unit)
	fun get_isTarget unit = #isTarget(lookup unit)
	fun get_status unit = !(#status(lookup unit))
	fun set_status (unit,s) = (#status(lookup unit)) := s

	fun get_import_direct unit = 
	    (Dag.parents(!graph,unit)
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
	fun get_import_transitive unit = 
	    (rev(Dag.ancestors(!graph,unit))
	     handle Dag.UnknownNode _ => error ("unit " ^ unit ^ " missing"))
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
	    val unit_paths = map get_paths unit_names
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

    (* readAssociation : string -> (unit_paths * bool) list *)
    fun readAssociation mapfile = 
	let
	    val dir = Dirs.dir mapfile
	    val findMapfile = Dirs.accessPath (mapfilePath dir, [OS.FileSys.A_READ])
	    fun relative file = OS.Path.joinDirFile {dir=dir, file=file}
	    fun mkPaths (unit, filebase) = Paths.sourceUnitPaths {unit=unit, file=relative filebase ^ ".sml"}
	    val is = TextIO.openIn mapfile
	    fun dropper s = let val len = size s
			    in  String.sub(s,0) = #";" orelse
				(len >= 2 andalso String.substring(s,0,2) = "//")
			    end
	    fun loop (n, acc) = 
		if (TextIO.endOfStream is)
		    then rev acc
		else 
		    let val line = TextIO.inputLine is
		    in  (case (split_line dropper line) of
			     ["#include", innerMapfile] => 
				 (case findMapfile innerMapfile
				    of NONE => error ("Line " ^ (Int.toString n) ^ " of " ^ mapfile ^
						      " includes an unreadable or non-existent file: " ^
						      line ^ "\n")
				     | SOME innerMapfile =>
					let
					    val innerAssociation = readAssociation innerMapfile
					in
					    loop (n+1, (rev innerAssociation) @ acc)
					end)
			   | [unitname, filebase] => loop (n+1, (mkPaths (unitname, filebase), false) :: acc)
			   | [unitname, filebase, "TARGET"] => loop (n+1, (mkPaths (unitname, filebase), true) :: acc)
			   | [] => loop (n, acc)
			   | _ => error ("Line " ^ (Int.toString n) ^ 
					 " of " ^ mapfile ^ " is ill-formed: " ^ line ^ "\n"))
		    end
	    val result = loop (0, [])
	    val _ = TextIO.closeIn is
	in  result
	end

    local
	fun parse_depend depend_str failure file =
	    let val ins = TextIO.openIn file
		val line = TextIO.inputLine ins
		val sz = size line
		val _ = TextIO.closeIn ins
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
			    " is not import/include.\n");
		     print "Calling parser to process.\n";
		     failure file)
	    end
    in
	fun parse_impl_import file = 
	    parse_depend "(*$import" (#3 o LinkParse.parse_impl) file
	fun parse_inter_include file = 
	    parse_depend "(*$include" (#3 o LinkParse.parse_inter) file
    end

    (* setMapping : string * bool -> unit *)
    (* Build graph from mapFile.  If getImports is true, add (*$import *) edges. *)
    fun setMapping (mapFile, getImports) =
	let val _ = Update.flushAll()
	    val _ = reset_graph()
	    val association = readAssociation mapFile
	    fun mapper (n, (paths, isTarg)) = 
		let 
		    val nodeWeight = Cache.size(Paths.sourceFile paths)
		    val info = 
			{position = n,
			 paths = paths,
			 status = ref WAITING,
			 isTarget = isTarg}
		in  add_node(Paths.unitName paths, nodeWeight, info)
		end
	    val _ = Listops.mapcount mapper association
	    val _ = chat ("Mapfile " ^ mapFile ^ " with " ^ (Int.toString (length association)) 
			  ^ " units processed.\n")
	    fun read_import unit = 
		let val paths = get_paths unit
		    val imports = parse_impl_import(Paths.sourceFile paths)
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
	    if getImports then
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
	val workingLocal = ref ([] : (string * Time.time * (unit -> bool)) list)
	val readySlaves = ref ([] : Comm.channel list)
	val workingSlaves = ref ([] : Comm.channel list)
    in
	(* Asynchronously ask for whether there are slaves ready *)
	fun pollForSlaves (do_ack_interface, do_ack_done, do_ack_local): int * int= 
	    let
		val maxWorkingLocal = 2
		val newWorkingLocal = List.filter (fn (unit, startTime, done) =>
						   if done() then (do_ack_local (unit, startTime); false)
						   else true) (!workingLocal)
		val _ = workingLocal := newWorkingLocal
		val channels = Comm.findToMasterChannels()
		fun noLongerWorking ch =
		    (workingSlaves := (Listops.list_diff_eq(Comm.eq, !workingSlaves, [Comm.reverse ch])))
		val _ = 
		    app (fn ch =>
			 case Comm.receive ch
			   of NONE => error ("Ready channel became empty: " ^ 
					     (Comm.source ch) ^ " to " ^ (Comm.destination ch))
			    | SOME Comm.READY => ()
			    | SOME (Comm.ACK_INTERFACE unit) => do_ack_interface (Comm.source ch, unit)
			    | SOME (Comm.ACK_DONE (unit, plan)) =>
			       (do_ack_done (Comm.source ch, unit, plan);
				noLongerWorking ch)
			    | SOME (Comm.ACK_ERROR unit) =>
			       (chat "\n\nSlave "; chat (Comm.source ch); 
				chat " signalled error during job "; 
				chat unit; chat "\n";
				error "slave signalled error")
			    | SOME (Comm.FLUSH _) => error ("slave " ^ (Comm.source ch) ^ " sent flush")
			    | SOME (Comm.REQUEST _) => error ("slave " ^ (Comm.source ch) ^ " sent request"))
		    channels
		val potentialNewSlaves = map Comm.reverse channels 
		val _ = readySlaves := (foldl (fn (ch,acc) => 
						 if ((Listops.member_eq(Comm.eq, ch, acc)) orelse
						     (Listops.member_eq(Comm.eq, ch, !workingSlaves)))
						     then acc else ch::acc) 
					  (!readySlaves) potentialNewSlaves)
	    in  (length (!readySlaves), maxWorkingLocal - length (!workingLocal))
	    end
	(* Should only be used when we are below our limit on local processes. *)
	fun useLocal (unit, f) =
	    let val newLocal = (unit, Time.now(), Background.background f)
		val _ = workingLocal := (newLocal :: (!workingLocal))
	    in  ()
	    end
	(* Works only if there are slaves available *)
	fun useSlave (showSlave, job) = 
	    let val (chan::rest) = !readySlaves
		val _ = readySlaves := rest
		val _ = workingSlaves := (chan :: (!workingSlaves))
	    in  showSlave (Comm.destination chan); 
		Comm.send (chan, Comm.REQUEST job)
	    end
	(* Kill active slave channels to restart and send flush slave's file caches *)
	fun resetSlaves() = let val _ = workingLocal := []
				val _ = readySlaves := []
				val _ = workingSlaves := []
				val toMaster = Comm.findToMasterChannels()
				val fromMaster = Comm.findFromMasterChannels()
				val _ = app Comm.erase toMaster
				val _ = app Comm.erase fromMaster
				fun flush toMaster = 
				    let val toSlave = Comm.reverse toMaster
					val platform = Target.getTargetPlatform()
					val flags = Comm.getFlags()
				    in  Comm.send(toSlave, Comm.FLUSH (platform, flags))
				    end
			    in  app flush toMaster
			    end
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
	    | WORKING (t, _) => t
	    | PROCEEDING (t, _) => t
	    | PENDING' (t, _) => t
	    | WORKING' (t, _) => t
	    | status => badStatus (unit, status, "getting ready time"))

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
	    | status => badStatus (unit, status, "maing ready"))

    fun markPending (unit, plan) =
	(case get_status unit
	   of READY t => set_status (unit, PENDING (t, plan))
	    | status => badStatus (unit, status, "making pending"))
	
    fun markWorking unit =
	(case get_status unit
	   of PENDING arg => set_status (unit, WORKING arg)
	    | status => badStatus (unit, status, "making working"))

    fun markWorking' unit =
	(case get_status unit
	   of PENDING' arg => set_status (unit, WORKING' arg)
	    | status => badStatus (unit, status, "making working'"))
	     
    fun enableChildren parent = 
	let 
	    fun enableReady child =
		let val imports = get_import_direct child (* no need to check transitively *)
		    fun hasInterface unit = (case get_status unit
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
	let val _ = case get_status unit
		      of READY t => set_status (unit, PENDING' (t, plan))
		       | WORKING (t, _) => set_status (unit, PENDING' (t, plan))
		       | PROCEEDING (t, _) => set_status (unit, PENDING' (t, plan))
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
	    val readyTime = case get_status unit
			      of READY t => t
			       | WORKING (t,_) => t
			       | PROCEEDING (t,_) => t
			       | WORKING' (t,_) => t
			       | status => badStatus (unit, status, "making done")
	    val elapsedTime = Time.-(Time.now(), readyTime)
	    val _ = set_status (unit, DONE elapsedTime)
	    val _ = enableChildren unit
	in  elapsedTime
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
	let
	    val paths = get_paths unit
	    val imports = get_import_transitive unit (* direct imports insufficient *)
	    val import_paths = map get_paths imports
	    val (status, plan) = Update.plan (paths, import_paths)
	    val _ = if Update.interfaceUptodate status
			then enableChildren unit
		    else ()
	in
	    if null plan then
		ignore (markDone unit)
	    else
		if sendToSlave plan then
		    markPending (unit, plan)
		else
		    markPending' (unit, plan)
	end
    
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
		in  if (null done2)				(* no progress *)
			then (waiting, pending, pending', done)	(* some more may have become ready now *)
		    else loop (waiting, pending, pending', done)
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
    fun waitForSlaves() = 
	let
	    fun ack_inter (name, unit) =
		(markProceeding unit;
		 chat ("  [" ^ name ^ " compiled interface of " ^ unit ^ "]\n"))
	    fun ack_done (name, unit, plan) =
		if null plan then
		    let val diff = Time.toReal (markDone unit)
			val diff = (Real.realFloor(diff * 100.0)) / 100.0
		    in  chat ("  [" ^ name ^ " compiled " ^ unit ^ " in " ^
			      (Real.toString diff) ^ " seconds]\n")
		    end
		else
		    let val _ = markPending' (unit, plan)
			val readyTime = getReadyTime unit
			val diff = Time.toReal(Time.-(Time.now(),readyTime))
			val diff = (Real.realFloor(diff * 100.0)) / 100.0
			val _ = chat ("  [" ^ name ^ " compiled to assembly of " ^ unit ^ " in " ^
				      (Real.toString diff) ^ " seconds]\n")
		    in  ()
		    end
	    fun ack_local (unit, startTime) =
		let val _ = ignore (markDone unit)
		    val diff = Time.toReal(Time.-(Time.now(),startTime))
		    val diff = (Real.realFloor(diff * 100.0)) / 100.0
		    val _ = Help.chat ("  [Master locally finished " ^ unit ^ " in " ^
				       (Real.toString diff) ^ " seconds]\n")
		in  ()
		end
	    val numSlaves = pollForSlaves (ack_inter, ack_done, ack_local)
	in  numSlaves 
	end
    fun finalReport units =
	if (!chat_verbose) then
	    let fun mapper u = 
		(case get_status u of
		     DONE         t => let val t = Time.toReal t
				       in if (t>=1.0) then SOME(u,t) else NONE
				       end
		   | WAITING => error ("Unit "  ^ u ^ " still waiting for compilation!")
		   | status => error ("Unit " ^ u ^ " still has status " ^ statusName status))
		val unsorted = List.mapPartial mapper units
		fun greater ((_,x),(_,y)) = x > (y : real)
		val sorted = ListMergeSort.sort greater unsorted
		val _ = chat "------- Times to compile files in ascending order -------\n"
		val (underOne,overOne) = List.partition (fn (_,t) => t < 1.0) sorted
		val (underTen, overTen) = List.partition (fn (_,t) => t < 10.0) overOne
		val (underThirty, overThirty) = List.partition (fn (_,t) => t < 30.0) overOne
		val _ = (print (Int.toString (length underOne));
			 print " files under 1.0 second.\n")
		val _ = (print (Int.toString (length underTen));
			 print " files from 1.0 to 10.0 seconds.\n")
		val _ = (print (Int.toString (length overTen));
			 print " files from 10.0 to 30.0 seconds:  ";
			 chat_strings 40 (map #1 overThirty); print "\n")
		val _ = (print (Int.toString (length overThirty));
			 print " files over 30.0 seconds:\n")
		val _ = app (fn (unit,t) => 
			     let val t = (Real.realFloor(t * 100.0)) / 100.0
			     in  chat ("  " ^ unit ^ 
				       (Util.spaces (20 - (size unit))) ^
				       " took " ^ 
				       (Real.toString t) ^ " seconds.\n")
			     end) overThirty
	    in  ()
	    end
	else ()
    fun execute (target, imports, plan) = ignore (foldl Update.execute (Update.init (target, imports)) plan)
    (* Assumes that all units in state's pending' list are PENDING'. *)
    fun useLocals (localsLeft, state : state) : int =
	let
	    fun useOne unit =
		let val target = get_paths unit
		    val imports = get_import_transitive unit
		    val import_paths = map get_paths imports
		    val plan = getPlan unit
		    val _ = markWorking' unit
		    val _ = Update.flush (target, plan)
		in  useLocal (unit, fn () => execute (target, import_paths, plan))
		end
	    fun useSome (0, _) = 0
	      | useSome (n, nil) = n
	      | useSome (n, unit :: rest) = (useOne unit;
					     useSome (n-1, rest))
	    val (_, _, _, _, pending', _) = state
	in  useSome (localsLeft, pending')
	end
    (* Assumes that all units in state's pending list are PENDING. *)
    fun useSlaves (slavesLeft, state : state) : int =
	let
	    fun useOne unit =
		let fun showSlave name = chat ("  [Calling " ^ name ^ 
					       " to compile " ^ unit ^ "]\n")
		    val target = get_paths unit
		    val imports = get_import_transitive unit
		    val import_paths = map get_paths imports
		    val plan = getPlan unit
		    val _ = markWorking unit
		    val _ = Update.flush (target, plan)
		in  useSlave (showSlave, (target, import_paths, plan))
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
		if slavesLeft > 0 andalso not (null waiting) then
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
            (* make_package : string -> Prelink.package *)
            fun make_package unit =
		let val paths = get_paths unit
		    val infoFile = Paths.infoFile paths
		    val (_, info) = InfoCache.read infoFile
		in
		    {unit = unit,
		     imports = #imports info,
		     exports = #exports info}
		end
	    
	    (* makeExe : string -> unit *)
	    exception Stop
	    fun makeExe unit =
		let val _ = Help.showTime (true, "Start linking on " ^ unit)
		    val requiredUnits = (get_import_transitive unit) @ [unit]

		    (* Check unit environments *)
		    val requiredPackages = map make_package requiredUnits
		    val _ = Prelink.checkTarget (unit, requiredPackages)

		    (* Generate startup code *)
		    val _ = if Help.wantAssembler() then () else raise Stop
		    val paths = get_paths unit
		    val linkAsmFile = Paths.linkAsmFile paths
		    val _ = Compiler.link (linkAsmFile, requiredUnits)

		    (* Assemble startup code *)
		    val _ = if Help.wantBinaries() then () else raise Stop
		    val linkObjFile = Paths.linkObjFile paths
		    val _ = Tools.assemble (linkAsmFile, linkObjFile);

		    (* Link *)
		    val linkExeFile = Paths.linkExeFile paths
		    val objectFiles = (map (Paths.objFile o get_paths) requiredUnits) @ [linkObjFile]
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
	     complete = fn _ => (finalReport units; app makeExe finalTargets)}
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
					then 
					    let val left = stateSize state
					    in  (makeGraph'(mapfile,NONE); 
						 Help.showTime (false, "CheckPoint (" ^ (Int.toString left) ^ " files left)");
						 lastGraphTime := (Time.now()))
					    end
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
				    if (null proceeding) andalso (null pending') andalso (null working')
					then ()
				    else (chat "\n     Waiting for objects of ";
					  chat_strings 20 (List.concat [proceeding, pending', working']);
					  chat ".");
				    chat "]\n";
				    if (diff > 15.0)
					then 
					    let val left = stateSize state
					    in  (makeGraph'(mapfile,NONE); 
						 Help.showTime (false,"CheckPoint (" ^ (Int.toString left) ^ " files left)");
						 lastGraphTime := (Time.now()))
					    end
				    else ();
				    Platform.sleep 0.1));
			   loop state))
	      end
	in loop initialState
	end

    fun purge(mapfile : string) =
	let val _ = setMapping(mapfile, false)
	    val units = list_units()
	    val _ = (print "Purging "; print mapfile; print "\n")
	    val dirs = Dirs.getDirs()
	    fun deletable file = not (Dirs.isSystemFile (dirs, file))
	    fun kill file = if (deletable file andalso
				OS.FileSys.access(file, []) andalso
				OS.FileSys.access(file, [OS.FileSys.A_READ]))
				then OS.FileSys.remove file
			    else ()
	    fun remove unit = 
		let val paths = get_paths unit
		    val infoFile = Paths.infoFile paths
		    val ilFile = Paths.ilFile paths
		    val ilUnself = Paths.ilToUnself ilFile
		    val ilBackup = Paths.fileToBackup ilFile
		    val asmFile = Paths.asmFile paths
		    val asmzFile = Paths.asmzFile paths
		    val objFile = Paths.objFile paths
		    val linkAsmFile = Paths.linkAsmFile paths
		    val linkAsmzFile = Paths.linkAsmzFile paths
		    val linkObjFile = Paths.linkObjFile paths
		    val linkExeFile = Paths.linkExeFile paths
		in  app kill [infoFile, ilFile, ilUnself, ilBackup, asmFile,
			      asmzFile, objFile, linkAsmFile, linkAsmzFile, linkObjFile, linkExeFile]
		end
	    val dot = Paths.mapfileToDot mapfile
	    val ps = Paths.mapfileToPs mapfile
	in
	    app remove units;
	    app kill [dot, ps]
	end
end
