(*$import Prelude *)

(* Notes:
 * Some paths depend on compiler state via Target.platformString.
 * We don't create any files or directories, we just manipulate paths.
 *)

signature PATHS =
sig

    val asmToObj  : string -> string
    val asmToAsmz : string -> string
    val mapfileToDot : string -> string
    val mapfileToPs  : string -> string
    val ilToUnself : string -> string
    val fileToBackup : string -> string
	
    type unit_paths
    (* Invariant: If p = sourceUnitPaths{unit,file} and file is
     * canonical (absolute) then all paths computed from p are
     * canonical (absolute).
     *)
    val sourceUnitPaths : {unit:string, file:string} -> unit_paths
    val unitName        : unit_paths -> string
    val sourceFile      : unit_paths -> string	(* source file (ext arbitrary but should exist) *)
    val interfaceFile   : unit_paths -> string	(* .int interface file *)
    val infoFile        : unit_paths -> string	(* meta information for manager *)
    val ilFile          : unit_paths -> string	(* IL context *)
    val asmFile         : unit_paths -> string	(* assmebler source *)
    val asmzFile        : unit_paths -> string	(* compressed assembler source *)
    val objFile         : unit_paths -> string	(* object file *)
    val linkAsmFile     : unit_paths -> string	(* linking source file *)
    val linkAsmzFile    : unit_paths -> string	(* compressed assembler source *)
    val linkObjFile     : unit_paths -> string	(* linking object file *)
    val linkExeFile     : unit_paths -> string	(* executable name *)
    val linkProfExeFile : unit_paths -> string  (* profiling executable name *)

    (* TILT intermediate directories related to unit, with
     * parents preceding children. *)
    val tiltDirs : unit_paths -> string list

    (* (commDirs dir) is a list of TILT intermediate directories
     * related to the commDir under dir, with parents preceding
     * children (commDir is last).  *)
    val commDirs : string -> string list
	
end
