(*$import Prelude PATHS Target OS *)

(* Thread-safety: We use Target.platformString which depends on the
 * compiler flags; everything else is pure. *)
 
(* Source files without extensions can cause problems.  A.sml and A
 * both map to the interface file A.int.  (Tilt intermediate files are
 * named A.sml.s, A.sml.o, etc.  so if we change the naming convention
 * for interface files, we can completely avoid this problem.)  *)

structure Paths :> PATHS =
struct

    (* joinBaseExt, joinDirFile : string * string -> string *)
    fun joinBaseExt (base, ext) = OS.Path.joinBaseExt {base=base, ext=SOME ext}
    fun joinDirFile (dir, file) = OS.Path.joinDirFile {dir=dir, file=file}
	
    (* asmToObj, ..., fileToBackup : string -> string *)
    fun asmToObj asm = joinBaseExt (OS.Path.base asm, "o")
    fun asmToAsmz asm = joinBaseExt (asm, "gz")
    fun mapfileToDot mapfile = joinBaseExt (mapfile, "dot")
    fun mapfileToPs mapfile = joinBaseExt (mapfile, "ps")
    fun ilToUnself il = joinBaseExt (il, "unself")
    fun fileToBackup file = joinBaseExt (file, "BACKUP")

    datatype unit_paths = SOURCE of {unit : string, file : string}

    (* sourceUnitPaths : {unit:string, file:string} -> unit_paths *)
    val sourceUnitPaths = SOURCE

    (* tiltDir', neutralDir, platformDir : string -> string *)
    fun tiltDir' dir     = joinDirFile (dir, "TM")
    fun tiltDir file     = tiltDir' (OS.Path.dir file)
    fun neutralDir file  = joinDirFile (tiltDir file, "any")
    fun platformDir file = joinDirFile (tiltDir file, Target.platformString())
	
    (* inDir : (string -> string) -> string -> string *)
    fun inDir getDir file = joinDirFile (getDir file, OS.Path.file file)
    
    (* unitName, ..., linkExeFile : unit_paths -> string *)
    fun unitName (SOURCE {unit, ...}) = unit
    fun sourceFile (SOURCE {file, ...}) = file
    fun interfaceFile (SOURCE {file, ...}) = joinBaseExt (OS.Path.base file, "int")
    fun infoFile (SOURCE {file, ...}) = joinBaseExt (inDir neutralDir file, "info")
    fun ilFile (SOURCE {file, ...}) = joinBaseExt (inDir neutralDir file, "il")
    fun asmFile (SOURCE {file, ...}) = joinBaseExt (inDir platformDir file, "s")
    val asmzFile = asmToAsmz o asmFile
    val objFile = asmToObj o asmFile
    fun linkAsmFile (SOURCE {file, ...}) = joinBaseExt (joinBaseExt (inDir platformDir file, "link"), "s")
    val linkAsmzFile = asmToAsmz o linkAsmFile
    val linkObjFile = asmToObj o linkAsmFile
    fun linkExeFile (SOURCE {unit, file}) = joinDirFile (OS.Path.dir file,
							 joinBaseExt (joinBaseExt (unit, Target.platformString()), "exe"))

    (* tiltDirs : unit_paths -> string list *)
    fun tiltDirs (SOURCE {file, ...}) = [tiltDir file, neutralDir file, platformDir file]

    (* commDirs : string -> string list *)
    fun commDirs dir =
	let val tiltDir = tiltDir' dir
	in  [tiltDir, joinDirFile (tiltDir, "TempCommunication")]
	end
	
end
