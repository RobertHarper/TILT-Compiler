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

    The manager uses info files to implement incremental, cut-off
    recompilation and to check that (pre-)compiled units and
    interfaces are consistent with the rest of the project
    description.  An info files contains a value of type info (see
    below) that summarizes the project description at the time of the
    most recent compile.  If any information in an info file changes
    between compilations, then the manager recompiles the unit or
    interface.	Even separately compiled units like "unit U : I" must
    be recompiled to generate a correct TAL interface (which depends
    on the label U as well as the interface I). Recompiling a
    (pre-)compiled unit or interface means signalling an error.

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

    Invariant: If desc:desc, then |- desc ok.  In other words, the
    manager only uses well-formed project descriptions.

    Invariant: If desc:desc and pos:Pos.pos in desc, then
    (Pos.file pos) terminates.

    A unit environment summarizes a well-formed elaboration context
	U_1:pinterface_1, ..., U_n:pinterface_n
    as
	U_1:CRC(pinterface_1), ..., U_n:CRC(pinterface_n)

    A value of type info summarizes compiler inputs.  All info files
    summarize the elaboration context.	Info files for source and
    precompiled units and interfaces summarize the source code.	 Info
    files for units with ascribed interfaces include summarize the
    ascribed pinterface.
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
	type link = {exe:file, asm:file, asmz:file, obj:file}
	val link : file * file -> link	(* desc, exe *)

	type pack =
	    {src:label -> file,
	     src_rel:label -> file,
	     libdir:file, inter:file, impl:file, desc:file}
	val inter : file
	val impl : file
	val desc : file
	val pack : file -> pack (* libdir *)

	val commdir : unit -> file
	val libdir : unit -> file
	val basisdesc : unit -> file
	val is_basisdesc : file -> bool
	val runtimedir : unit -> file
	val til_slave : unit -> file

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
	 info:file, pinterface:file, obj:file,
	 asm:file, asmz:file, using_file:file,
	 tali:file, tali_rel:file}
    type ssrcu =
	{pos:pos, opened:units, src:file, asc:label,
	 info:file, obj:file, asm:file, asmz:file,
	 using_file:file, tali:file, tali_rel:file}
    type primu =
	{pos:pos, asc:label,
	 obj:file, asm:file, asmz:file,
	 using_file:file, tali:file, tali_rel:file}
    type precompu =
	{pos:pos, obj:file, using:units, opened:units, src:file, asc:label,
	 info:file, using_file:file, tali:file, tali_rel:file}
    type compu =
	{pos:pos, obj:file, using:units, opened:units, asc:label,
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
	    val pinterface : uexp -> file
	    val pinterface' : uexp -> file option
	    val tali : uexp -> file
	    val tali_rel : uexp -> file
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

    val free_pdec : pdec -> set

    (*
	If check_desc desc = (),
	then |- desc ok.
    *)
    val check_desc : desc -> unit

    val blastOutUnits : Blaster.outstream -> units -> unit
    val blastInUnits : Blaster.instream -> units

    val blastOutDesc : Blaster.outstream -> desc -> unit
    val blastInDesc : Blaster.instream -> desc

    val pp_units : units -> Formatter.format

    val pp_pdec : pdec -> Formatter.format
    val pp_desc : desc -> Formatter.format

    (*
	Info files
    *)
    type crc = Crc.crc
    type ue = (label * crc) list

    datatype info =
	INFO_I of {ue:ue, src:(units * crc) option}
    |	INFO_U of {ue:ue, src:(units * crc) option, pinterface:crc option}

    val blastOutInfo : Blaster.outstream -> info -> unit
    val blastInInfo : Blaster.instream -> info

    val pp_info : info -> Formatter.format

end
