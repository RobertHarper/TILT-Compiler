(* 
	Some paths depend on compiler state via Target.platformString.
	We don't create any files or directories, we just manipulate
	paths.
*)
signature PATHS =
sig

    (*
	(commDir dir) is for communication between master and slaves.
    *)
    val commDir : string -> string

    (*
	(tmpFile file) is for atomic file writes.  File should be in a
	directory created by TILT to avoid trashing user files.  There
	is only one temporary file per directory.
    *)
    val tmpFile : string -> string

    type id = string
    type filename = string

    datatype iface =
	SRCI of id * filename * filename	(* dir, source *)
      | COMPI of id * filename * filename	(* iface, ue *)
      | UNITI of filename * filename		(* iface, ue *)

    val blastOutIface : Blaster.outstream -> iface -> unit
    val blastInIface : Blaster.instream -> iface

    val srci : {id:id, group:filename, file:filename} -> iface
    val compi : {id:id, file:filename, uefile:filename} -> iface

    val isSrcIface : iface -> bool
    val isCompIface : iface -> bool
    val isUnitIface : iface -> bool

    val ifaceName : iface -> string
    val ifaceSourceFile : iface -> string	(* source interfaces only *)
    val ifaceInfoFile : iface -> string		(* source interfaces only *)
    val ifaceFile : iface -> string
    val ifaceUeFile : iface -> string
    val ifaceDir : iface -> string		(* source interfaces only *)

    datatype compunit =
	SRCU of id * filename * filename * iface (* dir, source *)
      | COMPU of id * filename * filename * iface (* obj, ue *)
      | PRIMU of id * filename * iface		(* dir *)
      | IMPORTU of id * iface

    val blastOutUnit : Blaster.outstream -> compunit -> unit
    val blastInUnit : Blaster.instream -> compunit

    val srcu : {id:id, group:filename, file:filename,
		iface:iface option} -> compunit
    val compu : {id:id, file:filename, uefile:filename,
		 iface:iface} -> compunit
    val primu : {id:id, group:filename} -> compunit
    val importu : {id:id, iface:iface} -> compunit

    val isSrcUnit : compunit -> bool
    val isCompUnit : compunit -> bool
    val isPrimUnit : compunit -> bool
    val isImportUnit : compunit -> bool

    val unitIface : compunit -> iface
    val unitName : compunit -> string
    val sourceFile : compunit -> string	(* source units only *)
    val infoFile : compunit -> string	(* source and prim units only *)
    val ueFile : compunit -> string
    val asmFile : compunit -> string
    val asmzFile : compunit -> string
    val objFile : compunit -> string
    val unitDir : compunit -> string	(* source and prim units only *)

    type exe

    val blastOutExe : Blaster.outstream -> exe -> unit
    val blastInExe : Blaster.instream -> exe

    val exe : {group:filename, exe:string} -> exe

    val exeAsmFile : exe -> string
    val exeAsmzFile : exe -> string
    val exeObjFile : exe -> string
    val exeFile : exe -> string
    val exeDir : exe -> string

    type lib

    val blastOutLib : Blaster.outstream -> lib -> unit
    val blastInLib : Blaster.instream -> lib

    val lib : {dir:string} -> lib

    val libDir : lib -> string
    val libGroupFile : lib -> string
    val libIface : lib -> iface -> iface
    val libUnit : lib -> compunit -> compunit

end
