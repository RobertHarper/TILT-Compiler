(*$import UPDATE_HELP Paths FileCache Info LinkIl Util TopHelp Compiler Time UnitEnvironment List OS Tools *)

structure UpdateHelp
    :> UPDATE_HELP
	where type unit_paths = Paths.unit_paths
	where type InfoCache.internal = Info.info =
struct
    val error = fn s => Util.error "updatehelp.sml" s
	
    val compressAsm = Stats.tt "compress_asm"
	
    structure Cache = FileCache (type internal = unit
				 val equaler = fn _ => true
				 val reader = fn _ => error "read from dependency cache"
				 val writer = fn _ => error "write to dependency cache")
    structure IlCache = Compiler.IlCache
    structure InfoCache = FileCache (type internal = Info.info
				     val equaler = Info.equal
				     val reader = Info.read
				     val writer = Info.write)
    structure Ue = UnitEnvironment

    type unit_paths = Paths.unit_paths
    type notes = {unit : string,
		  target : Paths.unit_paths,
		  imports : Paths.unit_paths list,
		  ilFile : string,
		  infoFile : string,
		  asmFile : string,
		  asmzFile : string,
		  objFile : string}
    type state = notes * LinkIl.module option
    datatype asmfiles = COMPRESSED | UNCOMPRESSED | BOTH | NEITHER
	
    (* goalAsmFiles : unit -> asmfiles *)
    fun goalAsmFiles () =
	case (!Help.keepAsm, !compressAsm)
	  of (true, true) => COMPRESSED
	   | (true, false) => UNCOMPRESSED
	   | (false, _) => NEITHER
	
    (* notes : unit_paths * unit_paths list -> notes *)
    fun notes (target, imports) =
	{unit = Paths.unitName target,
	 target = target,
	 imports = imports,
	 ilFile = Paths.ilFile target,
	 infoFile = Paths.infoFile target,
	 asmFile = Paths.asmFile target,
	 asmzFile = Paths.asmzFile target,
	 objFile = Paths.objFile target}
	
    (* init : unit_paths * unit_paths list -> state *)
    fun init arg : state = (notes arg, NONE)

    (* mkUe : unit_paths list -> ue *)
    fun mkUe paths_list = foldl (fn (paths, acc) =>
				 let val ilFile = Paths.ilFile paths
				     val crc = IlCache.crc ilFile
				 in
				     Ue.insert (acc, Paths.unitName paths, crc)
				 end) Ue.empty paths_list
	
    (* elaborate : notes -> LinkIl.module *)
    fun elaborate ({unit, target, imports, ilFile, infoFile, ...} : notes) =
	let val interfaceFile = Paths.interfaceFile target
	    val sourceFile = Paths.sourceFile target
	    val constrained = Cache.exists interfaceFile
	    val (ilModule, ilFileWritten) =
		Compiler.elaborate {unit = unit,
				    smlFile = sourceFile,
				    intFile = (if constrained
						   then SOME interfaceFile
					       else NONE),
				    targetIlFile = ilFile,
				    importIlFiles = map Paths.ilFile imports}
	    val info' = {unit = unit,
			 lastWritten = IlCache.modTime ilFile,
			 lastChecked = Time.now(),
			 constrained = constrained,
			 imports = mkUe imports,
			 exports = mkUe [target]}
	    val _ = InfoCache.write (infoFile, info')
	in  ilModule
	end
    
    (* remove : string -> unit *)
    fun remove file = if Cache.exists file
			  then (Cache.flushSome [file];
				OS.FileSys.remove file)
		      else ()

    (* generate : state -> unit *)
    exception Stop
    fun generate (notes as {unit, target, asmFile, asmzFile, ...} : notes, il_module') =
	let val _ = Help.chat ("  [Compiling to assembly file]\n")
	    val il_module = case il_module'
			      of NONE => elaborate notes
			       | SOME il_module => il_module

		val nil_module = if !Help.uptoElaborate then raise Stop
				 else Compiler.il_to_nil (unit, il_module)

		val rtl_module = if (!Help.uptoPhasesplit) orelse (!Help.uptoClosureConvert) then raise Stop
				 else Compiler.nil_to_rtl (unit, nil_module)

		val _ = if (!Help.uptoRtl) then raise Stop
			else (remove asmzFile; (* In case we fail, avoid inconsistent copies *)
			      Compiler.rtl_to_asm (asmFile, rtl_module))
	    in ()
	    end handle Stop => ()

    (* compress, uncompress, uncompress_copy : string * string -> unit *)
    fun compress (src,dest) =
	let val _ = Help.chat ("  [Compressing " ^ src ^ "]\n")
	    val _ = Cache.flushSome [src,dest]
	    val _ = if Util.system ("gzip -qf9 " ^ src) then ()
		    else error ("compression of " ^ src ^ " failed")
	in
	    ()
	end
    fun uncompress (src,dest) =
	let val _ = Help.chat ("  [Uncompressing " ^ src ^ "]\n")
	    val _ = Cache.flushSome [src,dest]
	    val _ = if Util.system ("gunzip -q " ^ src) then ()
		    else error ("decompression of " ^ src ^ " failed")
	in
	    ()
	end
    fun uncompress_copy (src,dest) =
	let val _ = Help.chat ("  [Uncompressing " ^ src ^ "]\n")
	    val time = Cache.modTime src
	    val _ = Cache.flushSome [dest]
	    val _ = if Util.system ("gunzip -cq <" ^ src ^ " >" ^ dest) then ()
		    else error ("decompression of " ^ src ^ " failed")
	    val _ = OS.FileSys.setTime (dest, SOME time)
	in
	    ()
	end
    
    (* prepare : notes -> unit *)
    fun prepare ({asmFile, asmzFile, ...} : notes) =
	if Cache.exists asmFile then ()
	else uncompress_copy (asmzFile, asmFile)

    (* prepare_start : notes -> unit -> bool *)
    fun prepare_start arg = (prepare arg; fn () => true) (* XXX *)

    (* assemble : notes -> unit *)
    fun assemble ({unit, asmFile, objFile, ...} : notes) =
	let val _ = Help.chat ("  [Assembling " ^ unit ^ " to object file.]\n")
	    val _ = Tools.assemble (asmFile, objFile)
	    val _ = Cache.flushSome [objFile]
	in
	    ()
	end

    (* cleanup : notes -> unit *)
    fun cleanup ({asmFile, asmzFile, ...} : notes) =
	case goalAsmFiles ()
	  of COMPRESSED => if Cache.exists asmzFile then remove asmFile
			   else compress (asmFile, asmzFile)
	   | UNCOMPRESSED => if Cache.exists asmFile then remove asmzFile
			     else uncompress (asmzFile, asmFile)
	   | NEITHER => (remove asmFile; remove asmzFile)
	   | BOTH => error ("cleanup wants both versions of assembler")
				 
    (* elaborate, generate, prepare, assemble, cleanup : state -> state *)
    val elaborate = fn (notes, _)            => (notes, SOME (elaborate notes))
    val generate  = fn state                 => (generate state; state)
    val prepare   = fn (state as (notes, _)) => (prepare notes; state)
    val assemble  = fn (state as (notes, _)) => (assemble notes; state)
    val cleanup   = fn (state as (notes, _)) => (cleanup notes; state)
end

