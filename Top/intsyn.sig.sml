(* Internal syntax for project description files. *)
(*
	You will notice that unit and interface expressions are
	decorated with a lot of file names that are not part of the
	abstract syntax.  What are all these files?

	A pinterface file contains the sdecs from elaborating an
	interface or unit (along with some other information).  A
	pinterface file is associated with every interface in the
	project description and with every source unit with an
	inferred interface.  A pinterface file is closed by rooting
	paths that refer to other units with unit names (parameters)
	rather than variables.  A list of parameters can be read from
	the front of a pinterface file without reading in the sdecs.
	This is how the manager knows what units a pinterface uses.

	The manager uses info files to implement (cut-off)
	recompilation.  An info file is associated with source and
	precompiled interfaces and source, precompiled, and primitive
	units.  They store a unit's or interface's view of the project
	description at the time of the most recent compile.  Source
	and precompiled info files contain a summary of the
	elaboration context, an ordered list of open units, and a hash
	of the source file.  Unit info files indicate whether or not
	the unit was sealed to an interface and, if so, they contain a
	hash of the interface's pinterface file.  If any information
	in an info file changes between compilations, then the manager
	recompiles the unit or interface.

	A unit's asm, asmz, and obj files are what you expect.  They
	are target specific.  A unit's parm file is not target
	specific; it records the units that the other three files
	refer to so that the manager can determine what units an
	object file uses without parsing the binary.  When performing
	IR, flags govern whether the manager generates/keeps
	assembler, compressed assembler, and object files.  The
	manager only packs object and parm files into libraries and it
	will not delete assembler or compressed assembler files
	associated with precompiled and compiled units.

	Invariant: If desc:desc, then |- desc ok.  In other words, the
	manager only uses well-formed project descriptions.

	Invariant: If desc:desc and pos:Pos.pos in desc, then
	(Pos.file pos) terminates.

	The positions carried by pdecs name the project description
	file and line number where the declaration comes from.  They
	are used for error messages and to (re-)infer file names when
	the master sends a description to a slave.

	A unit environment summarizes a well-formed elaboration context
		U_1:pinterface_1, ..., U_n:pinterface_n
	as
		U_1:CRC(pinterface_1), ..., U_n:CRC(pinterface_n)

	A value of type info summarizes compiler inputs and are used
	to implement cut-off recompilation.  Possible forms are

		ue |- OPEN opened IN CRC(topspec)
		ue |- OPEN opened IN CRC(topdec)
		ue |- (OPEN opened IN CRC(topdec)) : CRC(pinterface)
		* |- PRIMITIVE : CRC(pinterface)
*)
signature INTSYN =
sig

    type file = string
    type pos = Pos.pos

    type label = Name.label
    type set = Name.LabelSet.set

    (* Files names. *)
    structure F :
    sig
	structure I :
	sig
	    type src = {src:file, info:file, pinterface:file}
	    type prim = {pinterface:file}
	    type precomp = src
	    type comp = prim

	    (* The first argument is the project description file. *)
	    val precomp : file * label * file -> precomp
	    val comp : file * label -> comp
	end

	structure U :
	sig
	    type src = {src:file, info:file, pinterface:file, obj:file,
			asm:file, asmz:file, parm:file}
	    type ssrc = {src:file, info:file, obj:file,
			 asm:file, asmz:file, parm:file}
	    type prim = {info:file, obj:file, asm:file,
			 asmz:file, parm:file}
	    type precomp = {obj:file, src:file, info:file,
			    parm:file}
	    type comp = {obj:file, parm:file}

	    val precomp : file * label * file -> precomp
	    val comp : file * label -> comp
	end

	type link = {exe:file, asm:file, asmz:file, obj:file}
	val link : file * file -> link	(* desc, exe *)

	type pack = {libdir:file, src:label -> file}
	val inter : file
	val impl : file
	val desc : file
	val pack : file -> pack		(* libdir *)

	val commdir : unit -> file
	val libdir : unit -> file
	val basisdesc : unit -> file
	val is_basisdesc : file -> bool
	val runtimedir : unit -> file
	val til_slave : unit -> file
    end

    type opened = label list
    type using = label list

    datatype iexp =
	SRCI of opened * F.I.src
      | PRIMI of F.I.prim
      | PRECOMPI of using * opened * F.I.precomp
      | COMPI of using * F.I.comp

    datatype uexp =
	SRCU of opened * F.U.src
      | SSRCU of opened * label * F.U.ssrc
      | PRIMU of label * F.U.prim
      | PRECOMPU of using * opened * label * F.U.precomp
      | COMPU of using * label * F.U.comp

    datatype pdec =
	IDEC of label * iexp * pos
      | SCDEC of label * label * pos
      | UDEC of label * uexp * pos

    type desc = pdec list

    type crc = Crc.crc
    type ue = (label * crc) list

    datatype info =
	INFO_SRCI of ue * opened * crc
      | INFO_SRCU of ue * opened * crc
      | INFO_SSRCU of ue * opened * crc * crc
      | INFO_PRIMU of crc

    (* Projections. *)
    structure P :
    sig

	structure I :
	sig
	    val src : iexp -> file
	    val src' : iexp -> file option
	    val info : iexp -> file
	    val info' : iexp -> file option
	    val pinterface : iexp -> file
	    val using' : iexp -> using option
	    val using : iexp -> using
	end

	structure U :
	sig
	    val src : uexp -> file
	    val src' : uexp -> file option
	    val info : uexp -> file
	    val info' : uexp -> file option
	    val pinterface : uexp -> file
	    val pinterface' : uexp -> file option
	    val obj : uexp -> file
	    val asm : uexp -> file
	    val asm' : uexp -> file option
	    val asmz : uexp -> file
	    val asmz' : uexp -> file option
	    val parm : uexp -> file
	    val using' : uexp -> using option
	    val using : uexp -> using
	    val asc : uexp -> label
	    val asc' : uexp -> label option
	end

	val label : pdec -> label
	val pos : pdec -> pos
	val info : pdec -> file
	val info' : pdec -> file option
    end

    (* Constructors. *)
    structure C :
    sig
	structure I :
	sig
	    val src : file * label * opened * file -> iexp
	    val prim : file * label -> iexp
	    val precomp : file * label * opened * file -> iexp
	    val comp : file * label -> iexp
	end

	structure U :
	sig
	    val src : file * label * opened * file -> uexp
	    val ssrc : file * label * label * opened * file -> uexp
	    val prim : file * label * label -> uexp
	    val precomp : file * label * label * opened * file -> uexp
	    val comp : file * label * label -> uexp
	end
    end

    (* Destructors. *)
    structure D :
    sig

	structure I :
	sig
	    val src : iexp -> opened * F.I.src
	    val src' : iexp -> (opened * F.I.src) option
	    val prim : iexp -> F.I.prim
	    val prim' : iexp -> F.I.prim option
	    val precomp : iexp -> using * opened * F.I.precomp
	    val precomp' : iexp -> (using * opened * F.I.precomp) option
	    val comp : iexp -> using * F.I.comp
	    val comp' : iexp -> (using * F.I.comp) option
	end

	structure U :
	sig
	    val src : uexp -> opened * F.U.src
	    val src' : uexp -> (opened * F.U.src) option
	    val ssrc : uexp -> opened * label * F.U.ssrc
	    val ssrc' : uexp -> (opened * label * F.U.ssrc) option
	    val prim : uexp -> label * F.U.prim
	    val prim' : uexp -> (label * F.U.prim) option
	    val precomp : uexp -> using * opened * label * F.U.precomp
	    val precomp' : uexp -> (using * opened * label * F.U.precomp) option
	    val comp : uexp -> using * label * F.U.comp
	    val comp' : uexp -> (using * label * F.U.comp) option
	end

	structure Pdec :
	sig
	    val idec : pdec -> label * iexp * pos
	    val idec' : pdec -> (label * iexp * pos) option
	    val scdec : pdec -> label * label * pos
	    val scdec' : pdec -> (label * label * pos) option
	    val udec : pdec -> label * uexp * pos
	    val udec' : pdec -> (label * uexp * pos) option
	end

    end

    val free_pdec : pdec -> set

    (*
	If check_desc desc = (),
	then |- desc ok.
    *)
    val check_desc : desc -> unit

    val blastOutDesc : Blaster.outstream -> desc -> unit
    val blastInDesc : Blaster.instream -> desc

    val blastOutParm : Blaster.outstream -> using -> unit
    val blastInParm : Blaster.instream -> using

    val blastOutInfo : Blaster.outstream -> info -> unit
    val blastInInfo : Blaster.instream -> info

    val pp_pdec : pdec -> Formatter.format
    val pp_desc : desc -> Formatter.format
    val pp_parm : using -> Formatter.format
    val pp_info : info -> Formatter.format

end
