(*$import MANAGER Communication TopHelp FileCache LinkParse LinkIl Compiler Linker OS List SplayMapFn SplaySetFn Platform Dirs Delay *)

(* 

   The master sets up by deleting all channels and then takes master steps which are:
   
     While there are available jobs
         (1) Look for available slave channels and process their acknowledgements
	     by marking compilation jobs as done.  Ready messages require no action.
	 (2) Issue all available jobs to availalbe slaves.
	     (A) If there are more jobs than slaves, then we are PROCESSING fully.
	     (B) Otherwise, we are IDLEing because the slaves are underutilized.
     When there are no more jobs, we are COMPLETEd.
*)


structure Master =
struct
    val error = fn s => Util.error "manager.sml" s
    open Help

    structure Comm = Comm(val slaveTidOpt = NONE)

    val show_stale = Stats.ff("ShowStale")
    val show_enable = Stats.ff("ShowEnable")
    structure Cache = FileCache(type internal = unit
				val equaler = fn _ => true
				val reader = fn _ => error "Master structure called read"
				val writer = fn _ => error "Master structure called write")

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
    fun parse_depend depend_str failure file =
      let val ins = TextIO.openIn file
	  val line = TextIO.inputLine ins
	  val sz = size line
	  val _ = TextIO.closeIn ins
	  val depend_str_sz = size depend_str
      in  if (sz >= depend_str_sz andalso 
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
    fun parse_impl_import file = 
	parse_depend "(*$import" (#3 o LinkParse.parse_impl) file
    fun parse_inter_include file = 
	parse_depend "(*$include" (#3 o LinkParse.parse_inter) file
    val _ = "*)*)"


    type collapse = {maxWeight : int, maxParents : int, maxChildren : int}
    datatype status = 
	WAITING                 (* Not ready for compilation because some imports are not ready. *)
      | READY of Time.time      (* Ready for compilation. Ready time. *) 
      | PENDING of Time.time    (* Compiling interface. Pending time.*)
      | ASSEMBLING of Time.time (* Compiled interface and assembly.  
				   Locally compiling object file. Pending Time. *)
      | PROCEEDING of Time.time (* Compiled interface.  Compiling object file. Pending Time. *)
      | DONE of Time.time       (* Interface and object are both generated. *)
    local

	val graph = ref (Dag.empty() : {position : int,
					relBase : string,
					absBase : string,
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

	fun get_relBase unit = #relBase(lookup unit)
	fun get_absBase unit = #absBase(lookup unit)
	fun get_position unit = #position(lookup unit)
	fun get_isTarget unit = #isTarget(lookup unit)
	fun get_status unit = !(#status(lookup unit))
	fun set_status (unit,s) = (#status(lookup unit)) := s

	fun list_targets() = let val allUnits = list_units()
			     in  List.filter get_isTarget allUnits
			     end

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

	fun markReady unit = 
	    (case (get_status unit) of
		 WAITING => set_status(unit,READY (Time.now()))
	       | READY _ => ()
	       | PENDING _ => error "unit was pending; making ready\n"
	       | ASSEMBLING _ => error "unit was assembling; making ready\n"
	       | PROCEEDING _ => error "unit was proceeding; making ready\n"
	       | DONE _ => error "unit was done; making ready\n")

	fun markPending unit = 
	    (case (get_status unit) of
		 WAITING => error "markPending: unit was waiting\n"
	       | READY _ => set_status(unit,PENDING (Time.now()))
	       | PENDING _ => ()
	       | ASSEMBLING _ => error "markPending: unit was assembling\n"
	       | PROCEEDING _ => error "markPending: unit was proceeding\n"
	       | DONE _ => error "markPending: unit was done\n")

	fun enableChildren parent = 
	    let 
		fun enableReady child = 
		    let val imports = get_import_direct child (* no need to check transitively *)
			fun atLeastProceeding unit = (case (get_status unit) of
							  PROCEEDING _ => true
							| DONE _ => true
							| _ => false)
			val ready = Listops.andfold atLeastProceeding imports
		    in  ready andalso 
			(case get_status child of
			     WAITING => (markReady child; true)
			   | _ => false)  (* The child status may be beyond READY since
					     the same parent might call enableChildren twice. *)
		    end
		val enabled = List.filter enableReady (get_dependent_direct parent)
	    in  if (!show_enable andalso (not (null enabled) ))
		    then (chat "  [Compilation of "; chat parent;
			  chat " has enabled these units: ";
			  chat_strings 40 enabled; 
			  chat "]\n")
		else ()
	    end

	fun markProceeding unit = 
	    ((case (get_status unit) of
		 WAITING => error "markProceeding: unit was waiting\n"
	       | READY _ => error "markProceeding: unit was ready\n"
	       | PENDING t => set_status(unit,PROCEEDING t)
	       | PROCEEDING _ => ()
	       | ASSEMBLING t => error "markProceeding: unit was assembling\n"
	       | DONE _ => error "markProceeding: unit was done\n");
	      enableChildren unit)

	fun markAssembling unit = 
	    let val startTime = 
		(case (get_status unit) of
		     WAITING => error "markAssembling: unit was waiting\n"
		   | READY _ => error "markAssembling: unit was ready\n"
		   | PENDING t => (set_status(unit, ASSEMBLING t); t)
		   | PROCEEDING t => (set_status(unit, ASSEMBLING t); t)
		   | ASSEMBLING t => t
		   | DONE _ => error "markAssembling: unit was done\n")
		val _ = enableChildren unit
	    in  startTime
	    end

	(* We must call enableChildren here because units may skip through the Proceeding stage. *)
	fun markDone unit : Time.time = 
	    let val now = Time.now()
		val startTime = 
		    (case (get_status unit) of
			 WAITING => error "markDone: unit was waiting\n"
		       (* May not need compile this unit. *)
		       | READY t => t
		       (* Might have missed the proceding step. *)
		       | PENDING t => t
		       | PROCEEDING t => t
		       | ASSEMBLING t => t
		       | DONE _ => error "markDone: unit was already done\n")
		val _ = set_status(unit,DONE(Time.-(now,startTime)))
		val _ = enableChildren unit
	    in  startTime
	    end


	fun partition units = 
	    let fun folder (unit,(w,r,pe,pr,a,d)) = 
		(case (get_status unit) of
		     WAITING => (unit::w, r, pe, pr, a, d)
		   | READY _ => (w, unit::r, pe, pr, a, d)
		   | PENDING _ => (w, r, unit::pe, pr, a, d)
		   | PROCEEDING _ => (w, r, pe, unit::pr, a, d)
		   | ASSEMBLING _ => (w, r, pe, pr, unit::a, d)
		   | DONE _ => (w, r, pe, pr, a, unit::d))
	    in  foldl folder ([],[],[],[],[],[]) units
	    end

	    
	(* getLibDir : unit -> string *)
	val getLibDir = Dirs.getLibDir o Dirs.getDirs
	    
	(* mapfilePath : string -> string list *)
	fun mapfilePath currentDir = [currentDir, getLibDir()]

	(* readAssociation : string -> (string * string * bool) list *)
	fun readAssociation mapfile = 
	    let
		val dir = Dirs.dir mapfile
		val findMapfile = Dirs.accessPath (mapfilePath dir, [OS.FileSys.A_READ])
		fun relative file = Dirs.relative (dir, file)
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
			       | [unitname, filebase] => loop (n+1, (unitname, relative filebase, false) :: acc)
			       | [unitname, filebase, "TARGET"] => loop (n+1, (unitname, relative filebase, true) :: acc)
			       | [] => loop (n, acc)
			       | _ => error ("Line " ^ (Int.toString n) ^ 
					     " of " ^ mapfile ^ " is ill-formed: " ^ line ^ "\n"))
			end
		val result = loop (0, [])
		val _ = TextIO.closeIn is
	    in  result
	    end

	(* setMapping : string * bool -> unit *)
	(* Build graph from mapFile.  If getImports is true, add (*$import *) edges. *)
	fun setMapping (mapFile, getImports) =
	    let val _ = Cache.flushAll()
		val _ = reset_graph()
		val association = readAssociation mapFile
		fun mapper (n, (unitname, filebase, isTarg)) = 
		    let 
			val absBase = OS.Path.mkAbsolute(filebase, OS.FileSys.getDir())
                        val nodeWeight = Cache.size(base2sml filebase)
			val info = 
			    {position = n,
			     relBase = filebase,
			     absBase = absBase,
			     status = ref WAITING,
			     isTarget = isTarg}
		    in  add_node(unitname, nodeWeight, info)
		    end
		val _ = Listops.mapcount mapper association
		val _ = chat ("Mapfile " ^ mapFile ^ " with " ^ (Int.toString (length association)) 
			      ^ " units processed.\n")
		fun read_import unit = 
		    let val filebase = get_relBase unit
			val imports = parse_impl_import(base2sml filebase)
			val _ = app (fn import => add_edge(import,unit)) imports
			val _ = set_status(unit, if (null imports) then READY (Time.now()) else WAITING)
		    in  ()
		    end
	    in  if getImports
		    then 
			let val _ = app read_import (list_units());
			    val _ = chat ("Imports read.\n");
			    val _ = refreshDag (!graph);
			    val _= (chat "Dependency graph computed: ";
				    chat (Int.toString (Dag.numNodes (!graph))); chat " nodes and ";
				    chat (Int.toString (Dag.numEdges (!graph))); chat " edges.\n")
(*
			    val reducedGraph = Dag.removeTransitive (!graph)
			    val _ = graph := reducedGraph
			    val _ = (chat "Reduced dependency graph computed: ";
				     chat (Int.toString (Dag.numNodes (!graph))); chat " nodes and ";
				     chat (Int.toString (Dag.numEdges (!graph))); chat " edges.\n")
*)
			    val _ = chat "Not reducing dependency graph.\n"
			in  ()
			end
			 
		else ()
	    end


	(* makeGraph' : string * {maxWeight:int, maxParents:int, maxChildren:int} option -> string *)
	(* Generate dot(1) representation of current graph, name based on mapfile. *)
	fun makeGraph'(mapfile : string, collapseOpt) = 
	    let
		val start = Time.now() 
		val dot = mapfile ^ ".dot"
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
							   | ASSEMBLING _ => Dag.Gray
							   | PROCEEDING _ => Dag.Gray
							   | PENDING _ => Dag.Gray
							   | WAITING => Dag.White
							   | READY _ => Dag.White))}
		val _ = TextIO.closeOut out

		val diff = Time.toReal(Time.-(Time.now(), start))
		val diff = (Real.realFloor(diff * 100.0)) / 100.0
		val _ = if (!chat_verbose)
			    then chat ("Generated " ^ dot ^ " in " ^ (Real.toString diff) ^ " seconds.\n") 
			else ()
	    in  dot
	    end

	(* makeGraph' : string * {maxWeight:int, maxParents:int, maxChildren:int} option -> string *)
	(* Generate dot(1) representation of graph in mapfile. *)
	fun makeGraph(mapfile : string, collapseOpt) = 
	    let val _ = setMapping(mapfile, true)
	    in  makeGraph'(mapfile, collapseOpt)
	    end
    end


    local
	val workingAsm = ref ([] : (string * Time.time) list)
	val readySlaves = ref ([] : Comm.channel list)
	val workingSlaves = ref ([] : Comm.channel list)
    in  (* Asynchronously ask for whether there are slaves ready *)
	fun pollForSlaves (do_ack_interface, do_ack_assembly, check_asm, do_ack_object): int = 
	    let val newWorkingAsm = List.filter check_asm (!workingAsm)
		val _ = workingAsm := newWorkingAsm
		val channels = Comm.findToMasterChannels()
		val _ = 
		    app (fn ch => 
			 (case Comm.receive ch of
			      NONE => error ("Ready channel became empty: " ^ 
					     (Comm.source ch) ^ " to " ^ (Comm.destination ch))
			    | SOME (Comm.FLUSH _) => error ("slave " ^ (Comm.source ch) ^ " sent flush")
			    | SOME (Comm.REQUEST _) => error "slave sent request"
			    | SOME Comm.READY => ()
			    | SOME (Comm.ACK_ERROR (_::u::imps)) => 
				  (chat "\n\nSlave "; chat (Comm.source ch); 
				   chat " signalled error during job "; 
				   chat u; chat "\n";
				   error "Slave signalled error")
			    | SOME (Comm.ACK_INTERFACE job) => do_ack_interface (Comm.source ch, job)
			    | SOME (Comm.ACK_ASSEMBLY job) => 
				  let val _ = (workingSlaves := 
					       (Listops.list_diff_eq(Comm.eq, !workingSlaves, 
								     [Comm.reverse ch])))
				      val ut = do_ack_assembly (Comm.source ch, job)
				      val _ = workingAsm := ut :: (!workingAsm)
				  in  ()
				  end
			    | SOME (Comm.ACK_OBJECT job) => 
				  (workingSlaves := 
				   (Listops.list_diff_eq(Comm.eq, !workingSlaves, 
							 [Comm.reverse ch]));
				   do_ack_object (Comm.source ch, job)))) 
		     channels
		val potentialNewSlaves = map Comm.reverse channels 
		val _ = readySlaves := (foldl (fn (ch,acc) => 
						 if ((Listops.member_eq(Comm.eq, ch, acc)) orelse
						     (Listops.member_eq(Comm.eq, ch, !workingSlaves)))
						     then acc else ch::acc) 
					  (!readySlaves) potentialNewSlaves)
	    in  length (!readySlaves)
	    end
	(* Works only if there are slaves available *)
	fun useSlave (showSlave, msg) = 
	    let val (chan::rest) = !readySlaves
		val _ = readySlaves := rest
		val _ = workingSlaves := (chan :: (!workingSlaves))
		val platform = case Til.getTargetPlatform() of
		                  Til.TIL_ALPHA => "dunix"
				| Til.TIL_SPARC => "solaris"
				| _ => error "MLRISC not supported"
	    in  showSlave (Comm.destination chan); 
	        print "\nREQUEST = ";
		Comm.send (chan, Comm.REQUEST (platform::msg))
	    end
	(* Kill active slave channels to restart and send flush slave's file caches *)
	fun resetSlaves() = let val _ = workingAsm := []
				val _ = readySlaves := []
				val _ = workingSlaves := []
				val toMaster = Comm.findToMasterChannels()
				val fromMaster = Comm.findFromMasterChannels()
				val _ = app Comm.erase toMaster
				val _ = app Comm.erase fromMaster
				fun flush toMaster = 
				    let val toSlave = Comm.reverse toMaster
				    in  Comm.send(toSlave,Comm.FLUSH(Comm.getFlags()))
				    end
			    in  app flush toMaster
			    end
    end



    (* Compiles the unit either by determining that compilation is not necessary or by calling a slave. 
       This call does not block. *)
    fun needsCompile unitname = (* This unit is in at least a ready state so its imports exist *)
        (case (get_status unitname) of
	      DONE _ => false
	    | _ => 
		  let val sourcebase = get_relBase unitname
		      val intfile = base2int sourcebase
		      val smlfile = base2sml sourcebase
		      val uofile = base2uo sourcebase
		      val ofile = base2o sourcebase
		      val uifile = base2ui sourcebase
			  
		      val smldate = Cache.modTime smlfile
		      val dest_ui_exists = Cache.exists uifile
		      val dest_uo_exists = Cache.exists uofile
		      val dest_o_exists = Cache.exists ofile
			  
		      val direct_imports = get_import_direct unitname
		      val direct_imports_base = map get_absBase direct_imports
		      val direct_imports_ui = map Help.base2ui direct_imports_base
		      val (latest_import_file, latest_import_time) = Cache.lastModTime direct_imports_ui
			       
		      val sml_changed = 
			  (dest_ui_exists andalso
			   dest_uo_exists andalso 
			   dest_o_exists andalso
			   (Time.<(Cache.modTime uofile, smldate) orelse
			    Time.<(Cache.modTime ofile, smldate)))
			  
		      val import_changed = 
			  (dest_uo_exists andalso
			   dest_o_exists andalso
			   (Time.<(Cache.modTime ofile, latest_import_time) orelse
			    Time.<(Cache.modTime uofile, latest_import_time)))
			  
		      val fresh =           
			  if (not dest_ui_exists) then
			      (if (!show_stale)
				   then chat ("      [" ^ sourcebase ^ " is stale: " ^
					      uifile ^ " is missing.]\n")
			       else ();
				   false)
			  else if (not dest_uo_exists) then
			      (if (!show_stale)
				   then chat ("      [" ^ sourcebase ^ " is stale: " ^
					      uofile ^ " is missing.]\n")
			       else ();
				   false)
			       else if (not dest_o_exists) then
				   (if (!show_stale)
					then chat ("      [" ^ sourcebase ^ " is stale: " ^
						   ofile ^ " is missing.]\n")
				    else ();
					false)
				    else if sml_changed then
					(if (!show_stale)
					     then chat ("      [" ^ sourcebase ^ " is stale: " ^
							smlfile ^ " newer than objects or interface.]\n")
					 else ();
		      false)
					 else if import_changed then
					     (if (!show_stale)
						  then chat ("      [" ^ sourcebase ^ " is stale: " ^
							     (valOf latest_import_file) ^ " changed.]\n")
					      else ();
						  false)
					      else 
						  true
						  
		      val _ = if fresh
				  then (
				      (* chat ("  [" ^ sourcebase ^ " is up-to-date.]\n"); *)
					markDone unitname; ())
			      else ()

		  in  not fresh
		  end)

    (* platformName : unit -> string *)
    fun platformName (base, ext) = 
	let val platform = (case Til.getTargetPlatform()
					      of Til.TIL_ALPHA => ".alpha"
					       | Til.TIL_SPARC => ".sparc"
					       | _ => error "MLRISC unsupported")
	    val extra = if !(Stats.bool "MirrorPtrArray")
			    then ".mirror" else ".nomirror"
	in  base ^ extra ^ platform ^ ext
	end
    (* exeName : string * string list -> string *)
    fun exeName ("", units) = platformName (List.last units, ".exe")
      | exeName (exe, _) = exe
    (* exeDroppings : string * string list -> string list *)
    fun exeDroppings (exe, units) =
	let val exeName = exeName (exe, units)
	    fun linkName ext = platformName ("link_" ^ exeName, ext)
	in
	    [exeName, linkName ".o", linkName ".s"]
	end
    (* waiting, ready, pending, assembling, proceeding - done not included *)
    type state = string list * string list * string list * string list * string list
    datatype result = PROCESSING of Time.time * state  (* All slaves utilized *)
                    | IDLE of Time.time * state * int  (* Some slaves idle and there are ready jobs *)
	            | COMPLETE of Time.time 
    fun stateSize ((a,b,c,d,e) : state) = (length a) + (length b) + (length c) + (length d) + (length e)
    fun resultToTime (PROCESSING (t,_)) = t
      | resultToTime (IDLE (t,_,_)) = t
      | resultToTime (COMPLETE t) = t
    fun once (mapfile, doLink) = 
	let val _ = resetSlaves()
	    val _ = setMapping(mapfile, true)
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
	    val _ = showTime (true,"Start compiling files")
	    fun waitForSlaves() = 
		let fun ack_inter (name,(platform::u::_::_)) = 
		          (Cache.flushSome [(base2ui (get_absBase u))];
			   markProceeding u; 
			   chat ("  [" ^ name ^ " compiled interface of " ^ u ^ "]\n"))
		      | ack_inter _ = error "Acknowledgement message not at least three words"
		    fun ack_asm (name,(platform::u::base::importBases)) = 
		          let val pendingTime = markAssembling u
			      val now = Time.now()
			      val diff = Time.toReal(Time.-(now,pendingTime))
			      val diff = (Real.realFloor(diff * 100.0)) / 100.0
			      val _ = chat ("  [" ^ name ^ " compiled to assembly of " ^ u ^ " in " ^
					    (Real.toString diff) ^ " seconds]\n")
			      val _ = Til.assemble_start base
			  in  (u, now)
			  end
		      | ack_asm _ = error "Acknowledgement message not at least three words"
		    fun check_asm (name,asmTime) =
			let val isDone = Til.assemble_done(get_relBase name)
			    val _ = 
				if isDone 
				    then 
					let val _ = markDone name
					    val diff = Time.toReal(Time.-(Time.now(),asmTime))
					    val diff = (Real.realFloor(diff * 100.0)) / 100.0
					in  
					    chat ("  [Master locally assembled " ^ name ^ " to object in " ^
						  (Real.toString diff) ^ " seconds]\n")
					end
				else ()
			in  not isDone
			end
		    fun ack_obj (name,(platform::u::_::_)) = 
		          let val _ = Cache.flushSome [(base2ui (get_absBase u))];
			      val _ = Cache.flushSome [(base2o (get_absBase u))]
			      val _ = Cache.flushSome [(base2uo (get_absBase u))]
			      val pendingTime = markDone u
			      val diff = Time.toReal(Time.-(Time.now(),pendingTime))
			      val diff = (Real.realFloor(diff * 100.0)) / 100.0
			  in  chat ("  [" ^ name ^ " compiled object of " ^ u ^ " in " ^
				 (Real.toString diff) ^ " seconds]\n")
			  end
		      | ack_obj _ = error "Acknowledgement message not at least three words"
		    val numSlaves = pollForSlaves (ack_inter, ack_asm, check_asm, ack_obj)
		in  numSlaves 
		end
	    fun getReady waiting = 
		let fun loop (waiting, ready, done) = 
		    let val (waiting,ready', [], [], [], []) = partition waiting
			val ready = ready @ ready'
			val (ready,done') = List.partition needsCompile ready
			val done = done @ done'
		    in  if (null done') 
			    then  (waiting, ready, done)  (* no progress *)
			else loop (waiting, ready, done)  (* some more may have become ready now *)
		    end
		    val (waiting, ready, done) = loop (waiting, [], [])
		    val _ = if (null done orelse (not (!chat_verbose)))
				then ()
			    else (chat "  [These files are up-to-date already:";
				  chat_strings 40 done;
				  chat "]\n")
		in  (waiting, ready)
		end

	    val idle = ref 0
	    fun newState(waiting, ready, pending, assembling, proceeding) : state =
		let val ([], [], [], [], assembling, _) = partition assembling
		    val ([], [], [], proceeding, newAssembling, _) = partition proceeding
		    val ([],[], pending, newProceeding, _, _) = partition pending
		    val assembling = assembling @ newAssembling
		    val proceeding = proceeding @ newProceeding
		    val (waiting,newReady) = getReady waiting
		    val ready = ready @ newReady
		in  (waiting, ready, pending, assembling, proceeding)
		end
	    fun stateDone(([],[],[],[],[]) : state) = true
	      | stateDone _ = false
	    fun useSlaves slavesLeft state = 
		let val (state as (_, ready, _, _, _)) = newState state
		in  if (stateDone state)
		      then 	
			  let fun mapper u = 
			      (case get_status u of
				   DONE         t => let val t = Time.toReal t
						     in if (t>=1.0) then SOME(u,t) else NONE
						     end
				 | READY        t => (print "Ready\n";     NONE)
				 | PENDING      t => (print "Pending\n";   NONE)
				 | ASSEMBLING   t => (print "Assembling\n"; NONE)
				 | PROCEEDING   t => (print "Proceeding\n"; NONE)
				 | WAITING => error "Unit still waiting for compilation!")
			      val unsorted = List.mapPartial mapper units
			      fun greater ((_,x),(_,y)) = x > (y : real)
			      val sorted = ListMergeSort.sort greater unsorted
			      val _ = 
				  if (!chat_verbose)
				      then let val _ = chat "------- Times to compile files in ascending order -------\n"
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
			    in  COMPLETE (Time.now())
			    end
		    else
			(case (slavesLeft, ready) of
			     (0, _) => PROCESSING (Time.now(),state)
			   | (_, []) => IDLE(Time.now(),state,slavesLeft)
			   | (_, first::rest) =>
				 let fun showSlave name = chat ("  [Calling " ^ name ^ 
								" to compile " ^ first ^ "]\n")
				     val _ = markPending first
				     val absFirstBase = get_absBase first
				     val imports = get_import_transitive first
				     val absImportBases = map get_absBase imports
				     val _ = Cache.flushSome[base2ui absFirstBase,
							     base2s absFirstBase,
							     base2o absFirstBase,
							     base2uo absFirstBase]
				     val (waiting, _, pending, assembling, proceeding) = state
				     val state = (waiting, rest, first::pending, assembling, proceeding)
				 in  useSlave (showSlave, first::absFirstBase::absImportBases);
				     useSlaves (slavesLeft - 1) state
				 end)
		end
	    val initialState : state = (units, [], [], [], [])
	    fun step (state : state) = 
		let val state = newState state
		in  if (stateDone state)
			then COMPLETE(Time.now())
		    else
			let val numSlaves = waitForSlaves()
			in  useSlaves numSlaves state
			end
		end
	    (* makeExe : string -> unit *)
	    fun makeExe(finalTarget) = 
		let val _ = showTime (true,"Start linking on " ^ finalTarget)
		    val exe = exeName ("", [finalTarget])
		    val requiredUnits =
			let val import_tr = get_import_transitive finalTarget
			    val next_pos = get_position finalTarget
			    fun check import = if (get_position import < next_pos) then ()
					       else 
						   error ("Mapfile file ordering is inconsistent because " ^
							  finalTarget ^ " imports " ^ import ^ " but precedes it.")
			    val _ = app check import_tr
			in  import_tr @ [finalTarget]
			end
			
		    fun mapper unit = 
			let val base = get_relBase unit
			in  {unit=unit, base=base,
			     uiFile=base2ui base, uoFile=base2uo base, oFile=base2o base}
			end
		    val packages = map mapper requiredUnits
		in  (chat "Manager calling linker with: ";
		     if (!chat_verbose)
			 then chat_strings 30 requiredUnits
		     else chat_strings 30 ["..." ^ (Int.toString ((length requiredUnits) - 1)) ^ " units...", 
					   (List.last requiredUnits)];
		     chat "\n";
		     Linker.mk_exe {units = packages, exe_result = exe})
		end
	in  {setup = fn _ => initialState,
	     step = step, 
	     complete = fn _ => if doLink then app makeExe finalTargets else ()}
	end
    fun run (args as (mapfile,_)) = 
	let val {setup,step = step : state -> result,complete} = once args
	    val idle = ref 0
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
						 showTime (false, "CheckPoint (" ^ (Int.toString left) ^ " files left)");
						 lastGraphTime := (Time.now()))
					    end
				    else ()
			in  Platform.sleep 0.1;
			    loop state
			end
		  | IDLE(t, state as (waiting, ready, pending, assembling, proceeding), numIdle) =>
			((case !lastIdleTime of
			      NONE => lastIdleTime := SOME t
			    | _ => ());
			 (case prev of
			      IDLE _ => ()
			    | _ => (chat "  [Idling ";
				    chat (Int.toString (!idle));
				    chat ": ";
				    chat (Int.toString numIdle);
				    chat " ready slaves.  ";
				    chat (Int.toString (length waiting));
				    chat " waiting jobs.\n";
				    chat "     Waiting for interfaces of ";
				    chat_strings 20 pending; chat ".";
				    if (null proceeding)
					then ()
				    else (chat "\n     Waiting for objects of ";
					  chat_strings 20 proceeding; chat ".");
				    chat "]\n";
				    if (diff > 15.0)
					then 
					    let val left = stateSize state
					    in  (makeGraph'(mapfile,NONE); 
						 showTime (false,"CheckPoint (" ^ (Int.toString left) ^ " files left)");
						 lastGraphTime := (Time.now()))
					    end
				    else ();
				    Platform.sleep 0.1));
			   loop state))
	      end
	in loop initialState
	end


    fun makeGraphShow(mapfile : string, collapse) = 
	let val dot = makeGraph (mapfile, collapse)
	    val ps = mapfile ^ ".ps"
	    val _ = OS.Process.system ("dot -Tps " ^ dot ^ " -o " ^ ps)
	    val _ = chat ("Generated " ^ ps ^ ".\n")
	    val _ = OS.Process.system ("gv " ^ ps ^ "&")
	    val _ = chat ("Invoked gv on " ^ ps ^ ".\n")
	in  ps
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
		let val base = get_relBase unit
		    val ui = base2ui base
		    val uo = base2uo base
		    val s = base2s base
		    val gz = s ^ ".gz"
		    val obj = base2o base
		in  app kill [ui, ui ^ ".BACKUP", ui ^ ".unself", uo, s, gz, obj]
		end
	    fun purgePlatform platform =
		let val savedPlatform = Til.getTargetPlatform()
		in
		    Til.setTargetPlatform platform;
		    app remove units;
		    Til.setTargetPlatform savedPlatform
		end
	    val dot = mapfile ^ ".dot"
	    val ps = mapfile ^ ".ps"
	in
	    app purgePlatform [Til.TIL_ALPHA, Til.TIL_SPARC];
	    app kill (exeDroppings ("", units));
	    app kill [dot, ps]
	end
end
