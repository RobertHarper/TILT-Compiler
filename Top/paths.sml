structure Paths :> PATHS =
struct
    val error = fn s => Util.error "paths.sml" s

    fun say (s : string) : unit =
	if !Blaster.BlastDebug then (print s; print "\n") else ()

    val op/ : string * string -> string =
	fn (dir,file) => OS.Path.joinDirFile {dir=dir, file=file}

    val dir : string -> string = OS.Path.dir
    val file : string -> string = OS.Path.file

    val TM = "TM"
    val U = "U"		(* units *)
    val I = "I"		(* interfaces *)
    val L = "L"		(* link units *)
    val C = "C"		(* communications *)

    val info = "info"	(* info files *)
    val iface = "iface"	(* parameterized interface files *)
    val ue = "ue"	(* unit environment files *)
    val ifaceue = "ifaceue" (* 2nd unit environment for units w/o interfaces *)
    val asm = "asm"	(* assembler *)
    val asmz = "asmz"	(* compressed assembler *)
    val tmp = "tmp"	(* temporary *)
    val obj = "obj"	(* object files *)
    val group = "group"	(* packed group file *)

    val target : unit -> string = Target.platformString

    fun commDir (dir : string) : string = dir/TM/C
    fun tmpFile (file : string) : string = dir file/tmp

    type id = string
    type filename = string

    datatype iface =
	SRCI of id * filename * filename	(* dir, source *)
      | COMPI of id * filename * filename	(* iface, ue *)
      | UNITI of filename * filename		(* iface, ue *)

    structure B = Blaster

    fun blastOutIface (os : B.outstream) (iface : iface) : unit =
	(say "blastOutIface";
	 (case iface
	    of SRCI (id,dir,src) =>
		(B.blastOutInt os 0; B.blastOutString os id;
		 B.blastOutString os dir; B.blastOutString os src)
	     | COMPI (id,iface,ue) =>
		(B.blastOutInt os 1; B.blastOutString os id;
		 B.blastOutString os iface; B.blastOutString os ue)
	     | UNITI (iface,ue) =>
		(B.blastOutInt os 2; B.blastOutString os iface;
		 B.blastOutString os ue)))
    fun blastInIface (is : B.instream) : iface =
	(say "blastInIface";
	 (case B.blastInInt is
	    of 0 => SRCI (B.blastInString is, B.blastInString is,
			  B.blastInString is)
	     | 1 => COMPI (B.blastInString is, B.blastInString is,
			  B.blastInString is)
	     | 2 => UNITI (B.blastInString is, B.blastInString is)
	     | _ => error "bad iface"))
    val (blastOutIface,blastInIface) =
	B.magic (blastOutIface, blastInIface, "iface $Revision$")

    fun srci {id:id, group:filename, file=src:filename} : iface =
	SRCI (id,dir group/TM/file group/I/id,src)
    fun compi {id : id, file : filename, uefile : filename} : iface =
	COMPI (id,file,uefile)

    fun isSrcIface (iface : iface) : bool =
	(case iface
	   of SRCI _ => true
	    | _ => false)
    fun isCompIface (iface : iface) : bool =
	(case iface
	   of COMPI _ => true
	    | _ => false)
    fun isUnitIface (iface : iface) : bool =
	(case iface
	   of UNITI _ => true
	    | _ => false)

    fun renameIface (iface : iface, name : string) : iface =
	(case iface
	   of SRCI (_,dir,source) => SRCI (name, dir, source)
	    | COMPI (_, iface, ue) => COMPI (name, iface, ue)
	    | UNITI _ => error "renameIface saw UNITI")

    fun ifaceName (iface : iface) : string =
	(case iface
	   of SRCI (id,_,_) => id
	    | COMPI (id,_,_) => id
	    | UNITI _ => error "ifaceName on UNITI")
    fun ifaceSourceFile (iface : iface) : string =
	(case iface
	   of SRCI (_,_,src) => src
	    | COMPI _ => error "ifaceSourceFile on COMPI"
	    | UNITI _ => error "ifaceSourceFile on UNITI")
    fun ifaceInfoFile (iface : iface) : string =
	(case iface
	   of SRCI (_,dir,_) => dir/info
	    | COMPI _ => error "ifaceInfoFile on COMPI"
	    | UNITI _ => error "ifaceInfoFile on UNITI")
    fun ifaceFile (iface' : iface) : string =
	(case iface'
	   of SRCI (_,dir,_) => dir/iface
            | COMPI (_,iface,_) => iface
	    | UNITI (iface,_) => iface)
    fun ifaceUeFile (iface : iface) : string =
	(case iface
	   of SRCI (_,dir,_) => dir/ue
	    | COMPI (_,_,ue) => ue
	    | UNITI (_,ue) => ue)
    fun ifaceDir (iface : iface) : string =
	(case iface
	   of SRCI (_,dir,_) => dir
	    | COMPI _ => error "ifaceDir on COMPI"
	    | UNITI _ => error "ifaceDir on UNITI")

    datatype compunit =
	SRCU of id * filename * filename * iface (* dir, source *)
      | COMPU of id * filename * filename * iface (* obj, ue *)
      | PRIMU of id * filename * iface		(* dir *)
      | IMPORTU of id * iface

    fun blastOutUnit (os : B.outstream) (unit : compunit) : unit =
	(say "blastOutUnit";
	 (case unit
	    of SRCU (id, dir, src, iface) =>
		(B.blastOutInt os 0; B.blastOutString os id;
		 B.blastOutString os dir; B.blastOutString os src;
		 blastOutIface os iface)
	     | COMPU (id, obj, ue, iface) =>
		(B.blastOutInt os 1; B.blastOutString os id;
		 B.blastOutString os obj; B.blastOutString os ue;
		 blastOutIface os iface)
	     | PRIMU (id,dir,iface) =>
		(B.blastOutInt os 2; B.blastOutString os id;
		 B.blastOutString os dir; blastOutIface os iface)
	     | IMPORTU (id,iface) =>
		(B.blastOutInt os 3; B.blastOutString os id;
		 blastOutIface os iface)))
    fun blastInUnit (is : B.instream) : compunit =
	(say "blastInUnit";
	 (case B.blastInInt is
	    of 0 => SRCU (B.blastInString is, B.blastInString is,
			  B.blastInString is, blastInIface is)
	     | 1 => COMPU (B.blastInString is, B.blastInString is,
			   B.blastInString is, blastInIface is)
	     | 2 => PRIMU (B.blastInString is, B.blastInString is,
			   blastInIface is)
	     | 3 => IMPORTU (B.blastInString is, blastInIface is)
	     | _ => error "bad unit"))
    val (blastOutUnit, blastInUnit) =
	B.magic (blastOutUnit, blastInUnit, "unit $Revision$")

    fun uniti (dir : filename) : iface =
	UNITI (dir/iface, dir/ifaceue)

    fun srcu {id:id, group:filename, file=src:filename,
	      iface:iface option} : compunit =
	let val dir = dir group/TM/file group/U/id
	    val iface = (case iface
			   of NONE => uniti dir
			    | SOME iface => iface)
	in  SRCU (id,dir,src,iface)
	end
    fun compu {id : id, file=obj : filename, uefile : filename,
	       iface : iface} : compunit =
	COMPU (id,obj,uefile,iface)
    fun primu {id:id, group:filename} : compunit =
	let val dir = dir group/TM/file group/U/id
	in  PRIMU (id,dir,uniti dir)
        end
    fun importu {id : string, iface : iface} : compunit =
	IMPORTU (id, iface)

    fun isSrcUnit (unit : compunit) : bool =
	(case unit
	   of SRCU _ => true
	    | _ => false)
    fun isCompUnit (unit : compunit) : bool =
	(case unit
	   of COMPU _ => true
	    | _ => false)
    fun isPrimUnit (unit : compunit) : bool =
	(case unit
	   of PRIMU _ => true
	    | _ => false)
    fun isImportUnit (unit : compunit) : bool =
	(case unit
	   of IMPORTU _ => true
	    | _ => false)

    fun unitIface (unit : compunit) : iface =
	(case unit
	   of SRCU (_,_,_,iface) => iface
	    | COMPU (_,_,_,iface) => iface
	    | PRIMU (_,_,iface) => iface
	    | IMPORTU (_,iface) => iface)
    fun unitName (unit : compunit) : string =
	(case unit
	   of SRCU (id,_,_,_) => id
	    | COMPU (id,_,_,_) => id
	    | PRIMU (id,_,_) => id
	    | IMPORTU (id,_) => id)
    fun sourceFile (unit : compunit) : string =
	(case unit
	   of SRCU (_,_,src,_) => src
	    | COMPU _ => error "sourceFile on COMPU"
	    | PRIMU _ => error "sourceFile on PRIMU"
	    | IMPORTU _ => error "sourceFile on IMPORTU")
    fun infoFile (unit : compunit) : string =
	(case unit
	   of SRCU (_,dir,_,_) => dir/info
	    | COMPU _ => error "infoFile on COMPU"
	    | PRIMU (_,dir,_) => dir/info
	    | IMPORTU _ => error "infoFile on IMPORTU")
    fun ueFile (unit : compunit) : string =
	(case unit
	   of SRCU (_,dir,_,_) => dir/ue
	    | COMPU (_,_,ue,_) => ue
	    | PRIMU (_,dir,_) => dir/ue
	    | IMPORTU _ => error "ueFile on IMPORTU")
    fun asmFile (unit : compunit) : string =
	(case unit
	   of SRCU (_,dir,_,_) => dir/target()/asm
	    | COMPU _ => error "asmFile on COMPU"
	    | PRIMU (_,dir,_) => dir/target()/asm
	    | IMPORTU _ => error "asmFile on IMPORTU")
    fun asmzFile (unit : compunit) : string =
	(case unit
	   of SRCU (_,dir,_,_) => dir/target()/asmz
	    | COMPU _ => error "asmFile on COMPU"
	    | PRIMU (_,dir,_) => dir/target()/asmz
	    | IMPORTU _ => error "asmzFile on IMPORTU")
    fun objFile (unit : compunit) : string =
	(case unit
	   of SRCU (_,dir,_,_) => dir/target()/obj
	    | COMPU (_,obj,_,_) => obj
	    | PRIMU (_,dir,_) => dir/target()/obj
	    | IMPORTU _ => error "objFile on IMPORTU")
    fun unitDir (unit : compunit) : string =
	(case unit
	   of SRCU (_,dir,_,_) => dir
	    | COMPU _ => error "unitDir on COMPU"
	    | PRIMU (_,dir,_) => dir
	    | IMPORTU _ => error "objFile on IMPORTU")

    type exe = string * string	(* dir, exe *)

    fun exe {group:filename, exe:string} : exe =
	(dir group/TM/file group/L/file exe, exe)

    fun blastOutExe (os : B.outstream) ((dir,exe) : exe) : unit =
	(say "blastOutExe"; B.blastOutString os dir; B.blastOutString os exe)
    fun blastInExe (is : B.instream) : exe =
	(say "blastInExe"; (B.blastInString is, B.blastInString is))
    val (blastOutExe, blastInExe) =
	B.magic (blastOutExe, blastInExe, "exe $Revision$")

    fun exeAsmFile ((dir,_) : exe) : string = dir/target()/asm
    fun exeAsmzFile ((dir,_) : exe) : string = dir/target()/asmz
    fun exeObjFile ((dir,_) : exe) : string = dir/target()/obj
    fun exeFile ((_,exe) : exe) : string = exe
    fun exeDir ((dir,_) : exe) : string = dir

    type lib = string

    fun blastOutLib (os : B.outstream) (dir : lib) : unit =
	(say "blastOutLib"; B.blastOutString os dir)
    fun blastInLib (is : B.instream) : lib =
	(say "blastInLib"; B.blastInString is)
    val (blastOutLib, blastInLib) =
	B.magic (blastOutLib, blastInLib, "lib $Revision$")

    fun lib {dir:string} : lib = dir

    fun libDir (lib : lib) : string = lib
    fun libGroupFile (lib : lib) : string = lib/group
    fun libIface (lib : lib) (iface' : iface) : iface =
	(case iface'
	   of SRCI (id,_,_) => SRCI (id,lib/TM/group/I/id,lib/I/id)
	    | COMPI (id,_,_) =>
		let val dir = lib/TM/group/I/id
		in  COMPI (id,dir/iface,dir/ue)
		end
	    | UNITI _ => error "libIface on UNITI")
    fun libUnit (lib : lib) (unit : compunit) : compunit =
	(case unit
	   of SRCU (id,_,_,UNITI _) =>
		let val dir = lib/TM/group/U/id
		in  SRCU (id,dir,lib/U/id,uniti dir)
		end
	    | SRCU (id,_,_,iface) =>
		SRCU (id,lib/TM/group/U/id,lib/U/id,libIface lib iface)
	    | COMPU (id,_,_,iface) =>
		let val dir = lib/TM/group/U/id
		in  COMPU (id,dir/target()/obj,dir/ue,libIface lib iface)
		end
	    | PRIMU (id,_,_) =>
		let val dir = lib/TM/group/U/id
		in  PRIMU (id,dir,uniti dir)
		end
	    | IMPORTU (id,iface) => IMPORTU (id,libIface lib iface))

end
