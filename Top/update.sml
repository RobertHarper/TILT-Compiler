(*$import Prelude TopLevel UnitEnvironment Stats UPDATE UpdateHelp Paths TopHelp Time Util List Info *)

structure Update
    :> UPDATE
        where type unit_paths = Paths.unit_paths
	where type import = UpdateHelp.import =
struct

    val error = fn s => Util.error "update.sml" s
	
    type unit_paths = Paths.unit_paths
    type import = UpdateHelp.import

    val showStale = Stats.ff "ShowStale"
    val showPlan = Stats.tt "ShowPlan"

    datatype reason =
	Missing
      | Because of string
	
    (* stale : string * reason -> unit *)
    fun stale (what, reason) =
	let val msg = case reason
			of Missing => "missing"
			 | Because why => ("stale: " ^ why)
	in
	    Help.chat "      ["; Help.chat what; Help.chat " is ";
	    Help.chat msg; Help.chat "]\n"
	end
	
    structure Ue = UnitEnvironment
    structure StringSet = Help.StringSet
    structure Cache = UpdateHelp.Cache
    structure InfoCache = UpdateHelp.InfoCache
    structure IlCache = UpdateHelp.IlCache

    (*
	If a unit's source or imported interfaces have changed, we have to
	recompile all the way to object.  If a unit's constraints (.int files)
	have changed we either have to re-elaborate (if constraints don't
	effect object code) or re-compile all the way to object.

	 	Up to date checks
		-----------------
			Source		Constraint	Imported Interface
	  Interface	 (1)		 (1),(2)	 (1)
	  Assembler	 (3)		 (3),(2)	 (3)
	  Object	 (3)		 (3),(2)	 (3)

		(1): lastWritten(col) <= lastChecked(row)
		(2): constraint hasn't appeared or disappeared
		(3): lastWritten(col) <= lastWritten(row)
     *)

    type status = bool * bool * bool	(* interface uptodate, assembler uptodate, object uptodate *)

    (* interfaceUptodate : status -> bool *)
    fun interfaceUptodate (a, _, _) = a
	
    (* modTime : string -> Time.time *)
    fun modTime file = if Cache.exists file then Cache.modTime file else Time.zeroTime

    (* maxTime : Time.time list -> Time.time *)
    val maxTime = List.foldl (fn (a, b) => if Time.>(a, b) then a else b) Time.zeroTime

    (* readInfo : string -> Info.info *)
    fun readInfo infoFile = #2 (InfoCache.read infoFile)
		       
    (* readInfoOption : string -> Info.info option *)
    fun readInfoOption infoFile =
	if InfoCache.exists infoFile
	    then SOME (readInfo infoFile)
	else NONE
		       
    (* ilLastChecked : info option * string -> Time.time *)
    fun ilLastChecked (NONE : Info.info option, ilFile) = if IlCache.exists ilFile
							      then IlCache.modTime ilFile
							  else Time.zeroTime
      | ilLastChecked (SOME {lastChecked, ...}, _) = lastChecked

    (* times : unit_paths * info option -> Time.time * Time.time * Time.time *)
    fun times (target, info) =
	let 
	    val il_time = ilLastChecked (info, Paths.ilFile target)
	    val asm_files = List.filter Cache.exists [Paths.asmFile target, Paths.asmzFile target]
	    val asm_time = #2 (Cache.lastModTime asm_files)
	    val obj_time = modTime (Paths.objFile target)
	in
	    (il_time, asm_time, obj_time)
	end

    (* joinStatus : bool * bool * bool -> bool *)
    fun joinStatus (v_source, v_interface, v_import) =
	v_source andalso v_interface andalso v_import

    (* chatStatus : bool -> string * time * bool * bool * bool -> unit *)
    fun chatStatus interface_same (what, time, v_source, v_interface, v_import) =
	if time = Time.zeroTime then
	    stale (what, Missing)
	else
	    let
		val _ = if v_source then ()
			else stale (what, Because "older than source")
		val _ = if v_interface orelse not interface_same then ()
			else stale (what, Because "older than interface")
		val _ = if v_import then ()
			else stale (what, Because "older than imported interfaces")
	    in  ()
	    end
    
    (* We assmume target .sml and imported .il and .info files exist. *)
    (* fileStatus : unit_paths * unit_paths list -> status *)
    fun fileStatus (target, imports) =
	let
	    val sourceFile = Paths.sourceFile target
	    val source_time = Cache.modTime sourceFile
	    val infoFile = Paths.infoFile target
	    val info = readInfoOption infoFile
		
	    val (il_time, asm_time, obj_time) = times (target, info)
	    val il_v_source = Time.<= (source_time, il_time)
	    val asm_v_source = Time.<= (source_time, asm_time)
	    val obj_v_source = Time.<= (source_time, obj_time)

	    val interfaceFile = Paths.interfaceFile target
	    val interface_time = modTime interfaceFile
	    val interface_same = case info
				   of NONE => false
				    | SOME {constrained,...} => constrained = Cache.exists interfaceFile
	    val v_interface = (if interface_same then
				   fn time => Time.<= (interface_time, il_time)
			       else
				   fn _ => false)
	    val il_v_interface = v_interface il_time
	    val asm_v_interface = v_interface asm_time
	    val obj_v_interface = v_interface obj_time
				       
	    val import_time = maxTime (map ((#lastWritten) o readInfo o Paths.infoFile) imports)
	    val il_v_import = Time.<= (import_time, il_time)
	    val asm_v_import = Time.<= (import_time, asm_time)
	    val obj_v_import = Time.<= (import_time, obj_time)

	    val _ = if not (!showStale) then ()
		    else
			let
			    val info_exists = isSome info
			    val _ = if info_exists then ()
				    else stale (infoFile, Missing)
			    val _ = if interface_same orelse (not info_exists) then ()
				    else stale (sourceFile, Because "interface file has been created or removed")
			    val chatStatus = chatStatus interface_same
				
			    val _ = chatStatus (Paths.ilFile target, il_time, il_v_source, il_v_interface, il_v_import)
			    val _ = if not (Help.wantAssembler()) then ()
				    else chatStatus (Paths.asmFile target, asm_time, asm_v_source, asm_v_interface, asm_v_import)
			    val _ = if not (Help.wantBinaries()) then ()
				    else chatStatus (Paths.objFile target, obj_time, obj_v_source, obj_v_interface, obj_v_import)
		    in ()
		    end
	in
	    (joinStatus (il_v_source,  il_v_interface,  il_v_import),
	     joinStatus (asm_v_source, asm_v_interface, asm_v_import),
	     joinStatus (obj_v_source, obj_v_interface, obj_v_import))
	end

    datatype todo = ELABORATE | GENERATE | PREPARE | ASSEMBLE | CLEANUP
    type plan = todo list

    local 
	val elaborate = "elaborate"
	val generate = "generate"
	val prepare = "prepare"
	val assemble = "assemble"
	val cleanup = "cleanup"
    in
	(* toString : todo -> string *)
	fun toString ELABORATE = elaborate
	  | toString GENERATE = generate
	  | toString PREPARE = prepare
	  | toString ASSEMBLE = assemble
	  | toString CLEANUP = cleanup

	(* fromString : string -> todo *)
	fun fromString s =
	    if s = elaborate then ELABORATE
	    else if s = generate then GENERATE
	    else if s = prepare then PREPARE
	    else if s = assemble then ASSEMBLE
	    else if s = cleanup then CLEANUP
	    else error ("not a valid todo - " ^ s)
    end

    (* Basic Plan
     
	Given the status of a unit's interface, assembler, and object
	files, we can easily make a basic plan of action.
    *)
	
    (* basicPlan : status -> plan *)
    (* Note: we always have uptodate assembler source prior to ASSEMBLE. *)
    fun basicPlan (true, true, true) = nil
      | basicPlan (true, true, false) = [ASSEMBLE]
      | basicPlan (true, false, true) = [GENERATE]
      | basicPlan (true, false, false) = [GENERATE, ASSEMBLE]
      | basicPlan (false, true, true) = [ELABORATE]
      | basicPlan (false, true, false) = [ELABORATE, ASSEMBLE]
      | basicPlan (false, false, true) = [ELABORATE, GENERATE]
      | basicPlan (false, false, false) = [ELABORATE, GENERATE, ASSEMBLE]

    (* keep_asm, compress_asm, and compressed assembler source

	The basic plan ignores some steps for munging different
	versions of assembler source.
     
	We insert a PREPARE step before each ASSEMBLE and a CLEANUP
    	step at the end of every plan.  The former ensures that that
    	the up-to-date assembler is available in uncompressed form and
    	the latter deletes extra assembler files to respect the
    	keep_asm and compress_asm flags.
    *)
	
    (* addTrivialSteps : plan -> plan *)
    fun addTrivialSteps plan =
	let
	    fun folder (ASSEMBLE, acc) = ASSEMBLE :: PREPARE :: acc
	      | folder (x, acc) = x :: acc
	    val result_rev = foldl folder nil plan
	    val result_rev = CLEANUP :: result_rev
	in  rev result_rev
	end

    (* "Upto" flags

        The basic plan ignores the effects of the "Upto" flags.  It assumes
	that we want to compile all the way to object code.

	Most of the effect of the "Upto" flags can be handled by removing
	things from the todo list.  (That is, without complicating the tools.)
	The exception is that GENERATE is aware of these flags and stops
	after the appropriate phase.

 	UptoElaborate: any GENERATE, prepare, ASSEMBLE, and cleanup steps are removed from todo list.
	UptoNil/Rtl: any prepare, ASSEMBLE, and cleanup steps are removed from todo list.
	UptoAsm: any prepare and ASSEMBLE steps are removed from the todo list.
    *)

    (* handleUptoFlags : plan -> plan *)
    fun handleUptoFlags plan =
	let
	    val uptoElaborate = !Help.uptoElaborate
	    val uptoNil = (!Help.uptoPhasesplit orelse !Help.uptoClosureConvert)
	    val uptoRtl = !Help.uptoRtl
	    val uptoAsm = !Help.uptoAsm

	    (*If the interface is up to date and we are only compiling to
	     * NIL or RTL, then don't redo the generation phase
	     *)
	    val elaborating = List.exists (fn todo => todo = ELABORATE) plan
	    val removeGenerate = uptoElaborate orelse  ( not elaborating andalso (uptoNil orelse uptoRtl))
	    val removePrepare = (uptoElaborate orelse uptoNil orelse uptoRtl orelse uptoAsm)
	    val removeAssemble = removePrepare
	    val removeCleanup = (uptoElaborate orelse uptoNil orelse uptoRtl)
	    fun keep (ELABORATE) = true
	      | keep (GENERATE)  = not removeGenerate 
	      | keep (PREPARE)   = not removePrepare
	      | keep (ASSEMBLE)  = not removeAssemble
	      | keep (CLEANUP)   = not removeCleanup
	in List.filter keep plan
	end

    (* The plan generated here is correct but imperfect. *)
    val basicPlan = handleUptoFlags o addTrivialSteps o basicPlan

    (* We eliminate unnecessary CLEANUP actions. *)
    datatype asmfiles = datatype UpdateHelp.asmfiles
    datatype asmstatus =
	UTD of asmfiles
      | OOD of asmfiles
    fun eqAsmFiles (COMPRESSED, COMPRESSED) = true
      | eqAsmFiles (UNCOMPRESSED, UNCOMPRESSED) = true
      | eqAsmFiles (NEITHER, NEITHER) = true
      | eqAsmFiles (BOTH, BOTH) = true
      | eqAsmFiles _ = false
    fun eqAsmStatus (UTD a, UTD b) = eqAsmFiles (a, b)
      | eqAsmStatus (OOD a, OOD b) = eqAsmFiles (a, b)
      | eqAsmStatus _ = false
    (* eliminateCleanup' : asmfiles * asmstatus * plan -> plan *)
    fun eliminateCleanup' (goalFiles, startStatus, plan) =
	let
	    fun simulate (ELABORATE, status) = status
	      | simulate (GENERATE, _) = UTD UNCOMPRESSED
	      | simulate (PREPARE, UTD COMPRESSED) = UTD BOTH
	      | simulate (PREPARE, status) = status
	      | simulate (ASSEMBLE, status) = status
	      | simulate (CLEANUP, _) = UTD goalFiles
	    val simulate : plan -> asmstatus =
		foldl simulate startStatus
	    val plan' = List.filter (fn todo => todo <> CLEANUP) plan
	in
	    if eqAsmStatus (simulate plan, simulate plan') then
		plan'
	    else
		plan
	end
    (* currentAsmFiles : unit_paths -> asmfiles *)
    fun currentAsmFiles target =
	case (Cache.exists (Paths.asmFile target), Cache.exists (Paths.asmzFile target))
	  of (true, true) => BOTH
	   | (true, false) => UNCOMPRESSED
	   | (false, true) => COMPRESSED
	   | (false, false) => NEITHER
    (* eliminateCleanup : bool * unit_paths -> plan -> plan *)
    fun eliminateCleanup (asmUptodate, target) plan =
	let val startStatus = (if asmUptodate then UTD else OOD) (currentAsmFiles target)
	in  eliminateCleanup' (UpdateHelp.goalAsmFiles(), startStatus, plan)
	end

    (* eliminateGenerate' : bool * plan -> plan *)
    (* If assembler is not going to be kept and is not used for
     * assembly, then eliminate any GENERATE/CLEANUP.
     *)
    fun eliminateGenerate' (true, plan) = plan
      | eliminateGenerate' (false, plan) =
	if List.exists (fn todo => todo = ASSEMBLE) plan then plan
	else List.filter (fn GENERATE => false
			   | CLEANUP => false
			   (* PREPARE always lives or dies with ASSEMBLE *)
			   | todo => true) plan
    (* eliminateGenerate : plan -> plan *)
    fun eliminateGenerate plan = eliminateGenerate' (!Help.keepAsm, plan)
	    
    (* plan : unit_paths * unit_paths list -> plan *)
    fun plan (arg as (target, _)) =
	let val status = fileStatus arg
	    val plan = basicPlan status
	    val plan = eliminateCleanup (#2 status, target) plan
	    val plan = eliminateGenerate plan
	    val _ = if null plan orelse not (!showPlan) then ()
		    else (Help.chat ("  [Plan for " ^ Paths.unitName target ^ ": ");
			  Help.chat_strings 40 (map toString plan);
			  Help.chat "]\n")
	in  (status, plan)
	end

    (* flush : unit_paths * plan -> unit *)
    fun flush (paths, plan) =
	let
	    val asmFile = Paths.asmFile paths
	    val asmzFile = Paths.asmzFile paths
		
	    fun flushSome ELABORATE = (IlCache.flushSome [Paths.ilFile paths];
				       InfoCache.flushSome [Paths.infoFile paths])
	      | flushSome GENERATE = (Cache.flushSome [asmFile, asmzFile];
				      InfoCache.flushSome [Paths.infoFile paths])
	      | flushSome PREPARE = Cache.flushSome [asmFile]
	      | flushSome ASSEMBLE = Cache.flushSome [Paths.objFile paths]
	      | flushSome CLEANUP = Cache.flushSome [asmFile, asmzFile]
	in
	    app flushSome plan
	end

    (* flushAll : unit -> unit *)
    fun flushAll () = (Cache.flushAll(); IlCache.flushAll(); InfoCache.flushAll())

    type state = UpdateHelp.state
    val init = UpdateHelp.init

    (* execute : todo * state -> state *)
    fun execute (ELABORATE, state) = UpdateHelp.elaborate state
      | execute (GENERATE, state)  = UpdateHelp.generate state
      | execute (PREPARE, state)   = UpdateHelp.prepare state
      | execute (ASSEMBLE, state)  = UpdateHelp.assemble state
      | execute (CLEANUP, state)   = UpdateHelp.cleanup state
end
