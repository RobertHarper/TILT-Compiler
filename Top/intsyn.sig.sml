(* Internal syntax for project description files. *)
(*
    The main difference between the implementation and the abstract
    syntax is that unit expressions, interface expressions, and
    project declarations are decorated with file information.  We
    record the project description file and line number where an entry
    is defined and the names of any user-provided and
    compiler-generated files.  A brief description of these files
    follows.

    A pinterface file contains the sdecs from elaborating an
    interface or unit (along with some other information).  A
    pinterface file is associated with every interface in the
    project description and with every source unit with an
    inferred interface.	 A pinterface file is closed by rooting
    paths that refer to other units with unit names (parameters)
    rather than variables.  A list of parameters can be read from
    the front of a pinterface file without reading in the sdecs.
    This is how the manager knows what units a pinterface uses.

    Info files store values of type summary (see below) and support
    incremental recompilation and sanity checking pre-compiled
    interfaces and units.  When a unit or interface is compiled, the
    compiler stores a summary of its inputs in the corresponding info
    file.  At a future time, the manager has new inputs and would like
    to know if the compiled files from the earlier compile are
    suitable.  To decide this question, the new inputs are compared to
    the saved summary.

    A unit's asm, asmz, and obj files are what you expect.  A unit's
    tali file contains a TAL interface for its object file.  All four
    are target specific.  A unit's using file is not target specific;
    it records the units that the other four files refer to so that
    the manager can determine what units an object file uses without
    parsing the binary.	 When performing IR, flags and the target
    platform govern whether the manager generates/keeps assembler,
    compressed assembler, object, and tali files.  The manager only
    packs object, using, and tali files into libraries and it will not
    delete assembler or compressed assembler files associated with
    precompiled and compiled units.

    In the talx86 backend, asm files refer to tali files by name.
    Tali files are moved when they are packed into libraries.  To
    avoid reassembly, we use relative file names in asm files and send
    a suitable search path to the TAL linker and assembler.  A unit's
    tali_rel file is the relative version of its tali filename.	 The
    function F.tal_include takes a project description file position
    to an include directory.
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
	type link = {exe:file, asm:file, asmz:file, obj:file, tobj:file}
	val link : file -> link (* exe *)

	type pack =
	    {src:label -> file,
	     src_rel:label -> file,
	     libdir:file, inter:file, impl:file, desc:file}
	val inter : file
	val impl : file
	val desc : file
	val TM : file
	val pack : file -> pack (* libdir *)

	val commdir : unit -> file
	val libdir : unit -> file
	val basisdesc : unit -> file
	val is_basisdesc : file -> bool
	val runtimedir : unit -> file
	val bindir : unit -> file

	val tal_include : pos -> file
    end

    type units = label list

    type srci =
	{pos:pos, opened:units, src:file,
	 info:file, pinterface:file}
    type primi =
	{pos:pos,
	 pinterface:file}
    type precompi =
	{pos:pos, pinterface:file, using:units, opened:units, src:file,
	 info:file}
    type compi =
	{pos:pos, pinterface:file, using:units}

    datatype iexp =
	SRCI of srci
    |	PRIMI of primi
    |	PRECOMPI of precompi
    |	COMPI of compi

    type srcu =
	{pos:pos, opened:units, src:file,
	 info:file, pinterface:file, obj:file, tobj:file,
	 asm:file, asmz:file, using_file:file,
	 asme:file,asme_rel:file,
	 asmi:file,asmi_rel:file,
	 tali:file, tali_rel:file}
    type ssrcu =
	{pos:pos, opened:units, src:file, asc:label,
	 info:file, obj:file, tobj:file, asm:file, asmz:file,
	 asme:file,asme_rel:file,
	 asmi:file,asmi_rel:file,
	 using_file:file, tali:file, tali_rel:file}
    type primu =
	{pos:pos, asc:label,
	 obj:file, tobj:file, asm:file, asmz:file,
	 asmi:file,asmi_rel:file,
	 asme:file,asme_rel:file,
	 using_file:file, tali:file, tali_rel:file}
    type precompu =
	{pos:pos, obj:file, tobj:file, using:units, opened:units, src:file, asc:label,
	 asmi:file,asmi_rel:file,
	 asme:file,asme_rel:file,
	 info:file, using_file:file, tali:file, tali_rel:file}
    type compu =
	{pos:pos, obj:file, tobj:file, using:units, opened:units, asc:label,
	 asmi:file,asmi_rel:file,
	 asme:file,asme_rel:file,
	 using_file:file, tali:file, tali_rel:file}

    datatype uexp =
	SRCU of srcu
    |	SSRCU of ssrcu
    |	PRIMU of primu
    |	PRECOMPU of precompu
    |	COMPU of compu

    type idec = {name:label, iexp:iexp}
    type scdec =
	{pos:pos, name:label, asc:label,
	 stable:bool, info:file, tali:file, tali_rel:file}
    type udec = {name:label, uexp:uexp}

    datatype pdec =
	IDEC of idec
    |	SCDEC of scdec
    |	UDEC of udec

    (*
	Invariant: If desc:desc, then |- desc ok.  In other words, the
	manager only uses well-formed project descriptions.

	Invariant: If desc:desc and pos:Pos.pos in desc, then
	(Pos.file pos) terminates.
    *)
    type desc = pdec list

    (* Constructors. *)
    structure C :
    sig
	structure I :
	sig
	    val src : pos * label * units * file -> iexp
	    val prim : pos * label -> iexp
	    val precomp : pos * label * units * file -> iexp
	    val precomp' : pos * label * units * units * file -> iexp	(* using, opened *)
	    val comp : pos * label -> iexp
	    val comp' : pos * label * units -> iexp
	end

	structure U :
	sig
	    val src : pos * label * units * file -> uexp
	    val ssrc : pos * label * label * units * file -> uexp
	    val prim : pos * label * label -> uexp
	    val precomp : pos * label * label * units * file -> uexp	(* opened *)
	    val precomp' : pos * label * units * label * units * file -> uexp	(* using, opened *)
	    val comp : pos * label * units * label -> uexp
	    val comp' : pos * label * units * units * label -> uexp (* using, opened *)
	end

	structure D :
	sig
	    val i : label * iexp -> pdec
	    val sc : pos * label * label * bool -> pdec
	    val u : label * uexp -> pdec
	end

    end

    (* Deconstructors. *)
    structure D :
    sig

	structure I :
	sig
	    val src : iexp -> srci
	    val src' : iexp -> srci option
	    val prim : iexp -> primi
	    val prim' : iexp -> primi option
	    val precomp : iexp -> precompi
	    val precomp' : iexp -> precompi option
	    val comp : iexp -> compi
	    val comp' : iexp -> compi option
	end

	structure U :
	sig
	    val src : uexp -> srcu
	    val src' : uexp -> srcu option
	    val ssrc : uexp -> ssrcu
	    val ssrc' : uexp -> ssrcu option
	    val prim : uexp -> primu
	    val prim' : uexp -> primu option
	    val precomp : uexp -> precompu
	    val precomp' : uexp -> precompu option
	    val comp : uexp -> compu
	    val comp' : uexp -> compu option
	end

	structure D :
	sig
	    val i : pdec -> idec
	    val i' : pdec -> idec option
	    val sc : pdec -> scdec
	    val sc' : pdec -> scdec option
	    val u : pdec -> udec
	    val u' : pdec -> udec option
	end

    end

    (* Projections. *)
    structure P :
    sig

	structure I :
	sig
	    val source : iexp -> units * file
	    val source' : iexp -> (units * file) option
	    val opened : iexp -> units
	    val opened' : iexp -> units option
	    val src : iexp -> file
	    val src' : iexp -> file option
	    val pos : iexp -> pos
	    val info : iexp -> file
	    val info' : iexp -> file option
	    val pinterface : iexp -> file
	    val using : iexp -> units
	    val using' : iexp -> units option
	    val stable : iexp -> bool
	end

	structure U :
	sig
	    val source : uexp -> units * file
	    val source' : uexp -> (units * file) option
	    val opened : uexp -> units
	    val opened' : uexp -> units option
	    val src : uexp -> file
	    val src' : uexp -> file option
	    val pos : uexp -> pos
	    val asc : uexp -> label
	    val asc' : uexp -> label option
	    val using : uexp -> units
	    val using' : uexp -> units option
	    val using_file : uexp -> file
	    val asm : uexp -> file
	    val asm' : uexp -> file option
	    val asmz : uexp -> file
	    val asmz' : uexp -> file option
	    val info : uexp -> file
	    val info' : uexp -> file option
	    val obj : uexp -> file
	    val tobj : uexp -> file
	    val pinterface : uexp -> file
	    val pinterface' : uexp -> file option
	    val tali : uexp -> file
	    val tali_rel : uexp -> file
	    val asme : uexp -> file
	    val asme_rel : uexp -> file
	    val asmi : uexp -> file
	    val asmi_rel : uexp -> file
	    val stable : uexp -> bool
	end

	structure D :
	sig
	    val name : pdec -> label
	    val pos : pdec -> pos
	    val iexp : pdec -> iexp
	    val iexp' : pdec -> iexp option
	    val uexp : pdec -> uexp
	    val uexp' : pdec -> uexp option
	    val asc : pdec -> label
	    val asc' : pdec -> label option
	    val stable : pdec -> bool
	    val src : pdec -> file
	    val src' : pdec -> file option
	    val tali : pdec -> file
	    val tali' : pdec -> file option
	    val tali_rel : pdec -> file
	    val tali_rel' : pdec -> file option
	    val info : pdec -> file
	    val info' : pdec -> file option
	end
    end

    val blastOutUnits : Blaster.outstream -> units -> unit
    val blastInUnits : Blaster.instream -> units

    val blastOutDesc : Blaster.outstream -> desc -> unit
    val blastInDesc : Blaster.instream -> desc

    val pp_label : label -> Formatter.format
    val pp_units : units -> Formatter.format

    val pp_pdec : pdec -> Formatter.format
    val pp_desc : desc -> Formatter.format

    val free_pdec : pdec -> set

    (*
	If check_desc (_,desc) = (),
	then |- desc ok.
    *)
    val check_desc : string * desc -> unit  (* message in case of errors *)

    (*
	Compiler inputs comprise an up-to-date project description and
	a to-be-compiled project declaration.

	Invariant:

	If (desc,pdec) : inputs,
	then desc defines only (pre-)compiled interfaces and
	separately compiled units,
	and pdec is not a (pre-)compiled interface or unit,
	and desc |- pdec ok,
	and support(pdec,desc) = dom(desc).
    *)
    type inputs = desc * pdec

    (*
	A summary of compiler inputs.  Precompiled interfaces are
	summarized by the CRC of their source and their list of opened
	units.	Compiled interfaces using no units (corresponding to
	the primitive interface) are summarized by the CRC of their
	pinterface.
    *)
    structure S :
    sig
	type crc = Crc.crc

	structure D :
	sig
	    datatype iexp =
		PRECOMPI of units * crc (* opened, source *)
	    |	COMPI of crc	(* pinterface *)

	    datatype pdec =
		IDEC of label * iexp	(* I = iexp *)
	    |	SCDEC of label * label	(* U : I *)

	    type desc = pdec list
	end

	structure P :
	sig
	    datatype iexp =
		SRCI of units * crc (* opened, source *)
		(* PRIMI always out of date *)

	    datatype uexp =
		SRCU of units * crc (* opened, source *)
	    |	SSRCU of label * units * crc	(* ascription, opened, source *)
		(* PRIMU always out of date *)

	    datatype pdec =
		IDEC of label * iexp
	    |	SCDEC of label * label
	    |	UDEC of label * uexp
	end

	type summary = D.desc * P.pdec

	val blastOutSummary : Blaster.outstream -> summary -> unit
	val blastInSummary : Blaster.instream -> summary

	val pp_summary : summary -> Formatter.format

    end

end
