(* Internal syntax for project description files. *)

structure IntSyn :> INTSYN =
struct

    val error = fn s => Util.error "intsyn.sml" s
    val reject = Util.reject

    type file = string
    type pos = Pos.pos

    type label = Name.label
    type set = Name.LabelSet.set

    structure S = Name.LabelSet
    structure M = Name.LabelMap
    structure B = Blaster
    structure NB = NameBlast
    structure Fmt = Formatter

    type units = label list

    (* Files names. *)
    structure F =
    struct
	val op/ : string * string -> string =
	    fn (dir,file) => OS.Path.joinDirFile {dir=dir, file=file}

	val dir : string -> string = OS.Path.dir
	val file : string -> string = OS.Path.file

	val TM = "TM2"
	val U = "U"
	val I = "I"
	val L = "L"
	val C = "C"
	val P = "P"
	val Info = "info"
	val Context = "context"
	val Pinterface = "pinterface"
	val Using = "using"
	val Asm = "asm"
	val Asmtal = "asm.tal"
	val Asmetali = "asm_e.tali"
	val Asmitali = "asm_i.tali"
	val Asmz = "asmz"
	val Obj = "obj.o"
	val Tobj = "obj.to"
	val Tali = "tali"
	val Tmp = "tmp"
	val Lib = "Lib"
	val Bin = "Bin"
	val Runtime = "Runtime"
	val Basis = "basis"
	val Impl = "implementation"
	val Inter = "interface"
	val Desc = "project"

	val Target : unit -> string = Target.targetString

	fun pdir' (desc:file) : file =
	    dir desc/TM/P/file desc

	val pdir : pos -> file = pdir' o Pos.file

	fun i (pos:pos, l:label) : {info:file, pinterface:file} =
	    let val root = pdir pos/I/Name.label2name' l
	    in	{info=root/Info, pinterface=root/Pinterface}
	    end

	fun sc (pos:pos, l:label) : {info:file, tali:file, tali_rel:file} =
	    let val l = Name.label2name' l
		val t = Target()
		val root = pdir pos/U/l
		val root' = root/t
		val relroot' = l/t
		val info = root/Info
		val tali_rel = relroot'/Tali
		val tali = root'/Tali
	    in	{info=info, tali=tali, tali_rel=tali_rel}
	    end

	fun u (pos:pos, l:label) : {info:file, pinterface:file, obj:file, tobj:file,
				    asm:file, asmz:file, 
				    asme:file, asme_rel:file, asmi:file, asmi_rel:file, 
				    using_file:file, tali:file, tali_rel:file} =
	    let val l = Name.label2name' l
		val target = Target()
		val root = pdir pos/U/l
		val root' = root/target
	    in	{info=root/Info, pinterface=root/Pinterface,
		 obj=root'/Obj, 
		 tobj = root'/Tobj,
		 asm=if Target.tal() then root'/Asmtal else root'/Asm, 
		 asme=root'/Asmetali, asme_rel=l/target/Asmetali,
		 asmi=root'/Asmitali, asmi_rel=l/target/Asmitali,
		 asmz=root'/Asmz,
		 using_file=root/Using, tali=root'/Tali, tali_rel=l/target/Tali}
	    end

	val cwd : unit -> string =
	    Util.memoize OS.FileSys.getDir

	type link = {exe:file, asm:file, asmz:file, obj:file, tobj:file}

	fun link (exe:file) : link =
	    let val root = cwd()/TM/L/file exe
	    in	{exe=exe, 
		 asm=if Target.tal() then root/Asmtal else root/Asm, 
		 asmz=root/Asmz, obj=root/Obj, tobj=root/Tobj}
	    end

	type pack =
	    {src:label -> file,
	     src_rel:label -> file,
	     libdir:file, inter:file, impl:file, desc:file}
	val inter = Inter
	val impl = Impl
	val desc = Desc
	fun pack (libdir : file) : pack =
	    let fun src_rel (l:label) : file =
		    let val space =
			    if Name.is_unit l then U
			    else if Name.is_interface l then I
			    else error "pack saw unexpected label"
			val name = Name.label2name' l
		    in	space/name
		    end
		fun src (l:label) : file = libdir/src_rel l
		val inter = libdir/inter
		val impl = libdir/impl
		val desc = libdir/desc
	    in	{src=src, src_rel=src_rel, libdir=libdir, inter=inter,
		 impl=impl, desc=desc}
	    end

	val cwd : unit -> string =
	    Util.memoize OS.FileSys.getDir

	fun commdir () : file =
	    let val r = cwd()/TM/C
		val _ = Fs.mkdirs r
	    in	r
	    end
	val commdir = Util.memoize commdir

	fun full_path (path:file) : file =
	    OS.FileSys.fullPath path

	fun tiltroot () : file =
	    let val Env = "TILTROOT"
		val root =
		    (case (OS.Process.getEnv Env) of
			NONE => ""
		    |	SOME dir => dir)
		val root = full_path root
		val rx = [OS.FileSys.A_READ, OS.FileSys.A_EXEC]
		val have_root =
		    (OS.FileSys.isDir root andalso
		     OS.FileSys.access(root,rx))
		    handle _ => false
		val _ = if have_root then ()
			else reject (Env ^ ": " ^ root ^ " inaccessible")
	    in root
	    end
	val tiltroot = Util.memoize tiltroot
	val libdir = (fn () => tiltroot()/Lib)
	val basisdir = Util.memoize (fn () => libdir()/Basis)
	val basisdesc = Util.memoize (fn () => basisdir()/Desc)
	fun is_basisdesc (file:file) : bool =
	    let val file = full_path file
		val dir = OS.Path.dir file
	    in	dir = basisdir()
	    end
	val runtimedir = (fn () => tiltroot()/Runtime)
	val bindir = (fn () => tiltroot()/Bin)

	fun tal_include (pos:pos) : file =
	    pdir pos/U

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
	 asme:file, asme_rel:file,
	 asmi:file, asmi_rel:file,
	 tali:file, tali_rel:file}
    type ssrcu =
	{pos:pos, opened:units, src:file, asc:label,
	 info:file, obj:file, tobj:file, asm:file, asmz:file,
	 asme:file, asme_rel:file,
	 asmi:file, asmi_rel:file,
	 using_file:file, tali:file, tali_rel:file}
    type primu =
	{pos:pos, asc:label,
	 obj:file, tobj:file, asm:file, asmz:file,
	 asme:file, asme_rel:file,
	 asmi:file, asmi_rel:file,
	 using_file:file, tali:file, tali_rel:file}
    type precompu =
	{pos:pos, obj:file, tobj:file, using:units, opened:units, src:file, asc:label,
	 asme:file, asme_rel:file,
	 asmi:file, asmi_rel:file,
	 info:file, using_file:file, tali:file, tali_rel:file}
    type compu =
	{pos:pos, obj:file, tobj:file, using:units, opened:units, asc:label,
	 asme:file, asme_rel:file,
	 asmi:file, asmi_rel:file,
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

    val blastOutLabel : B.outstream -> label -> unit = NB.blastOutLabel
    val blastInLabel : B.instream -> label = NB.blastInLabel

    val blastOutUnits' : B.outstream -> units -> unit = B.blastOutList blastOutLabel
    val blastInUnits' : B.instream -> units = B.blastInList blastInLabel

    val (blastOutUnits, blastInUnits) =
	B.magic (blastOutUnits',blastInUnits',"units $Revision$")

    fun opt_out (what:string) (f:'a -> 'b option) : 'a -> 'b =
	(fn x =>
	 (case (f x) of
	    SOME y => y
	 |  NONE => error (what ^ " failed")))

    (* Constructors. *)
    structure C =
    struct
	structure I =
	struct

	    fun src (pos:pos, I:label, opened:units, src:file) : iexp =
		let val {info,pinterface} = F.i(pos,I)
		in  SRCI
			{pos=pos, opened=opened, src=src,
			 info=info, pinterface=pinterface}
		end

	    fun prim (pos:pos, I:label) : iexp =
		let val {pinterface,...} = F.i(pos,I)
		in  PRIMI
			{pos=pos, pinterface=pinterface}
		end

	    fun precomp (pos:pos, I:label, opened:units, src:file) : iexp =
		let val {info,pinterface} = F.i(pos,I)
		    val using = Fs.read_pinterface_parm pinterface
		in  PRECOMPI
			{pos=pos, pinterface=pinterface, using=using,
			 opened=opened, src=src, info=info}
		end

	    fun precomp' (pos:pos, I:label, using:units, opened:units, src:file) : iexp =
		let val {info,pinterface} = F.i(pos,I)
		in  PRECOMPI
			{pos=pos, pinterface=pinterface, using=using,
			 opened=opened, src=src, info=info}
		end

	    fun comp (pos:pos, I:label) : iexp =
		let val {pinterface,...} = F.i(pos,I)
		    val using = Fs.read_pinterface_parm pinterface
		in  COMPI
			{pos=pos, pinterface=pinterface, using=using}
		end

	    fun comp' (pos:pos, I:label, using:units) : iexp =
		let val {pinterface,...} = F.i(pos,I)
		in  COMPI
			{pos=pos, pinterface=pinterface, using=using}
		end
	end

	structure U =
	struct

	    fun src (pos:pos, U:label, opened:units, src:file) : uexp =
		let val {info,pinterface,obj,tobj,asm,asmz,
			 asme,asme_rel,asmi,asmi_rel,
			 using_file,tali,tali_rel,...} =
			F.u(pos,U)
		in  SRCU
			{pos=pos, opened=opened, src=src, info=info,
			 pinterface=pinterface, obj=obj, tobj=tobj, asm=asm,
			 asme=asme, asme_rel=asme_rel,asmi=asmi, asmi_rel=asmi_rel,
			 asmz=asmz, using_file=using_file, tali=tali,
			 tali_rel=tali_rel}
		end

	    fun ssrc (pos:pos, U:label, I:label, opened:units, src:file) : uexp =
		let val {info,obj,tobj,asm,asme,asme_rel,asmi,asmi_rel,
			 asmz,using_file,tali,tali_rel,...} = F.u(pos,U)
		in  SSRCU
			{pos=pos, opened=opened, src=src, asc=I, info=info,
			 obj=obj, tobj=tobj, asm=asm, asmz=asmz, using_file=using_file,
			 asmi=asmi, asmi_rel=asmi_rel,
			 asme=asme, asme_rel=asme_rel,
			 tali=tali, tali_rel=tali_rel}
		end

	    fun prim (pos:pos, U:label, I:label) : uexp =
		let val {obj,tobj,asm,asmz,asme,asme_rel,asmi,asmi_rel,using_file,tali,tali_rel,...} = F.u(pos,U)
		in  PRIMU
			{pos=pos, asc=I, obj=obj, tobj=tobj, asm=asm, asmz=asmz,
			 asme=asme, asme_rel=asme_rel,asmi=asmi, asmi_rel=asmi_rel,
			 using_file=using_file, tali=tali, tali_rel=tali_rel}
		end

	    fun precomp (pos:pos, U:label, I:label, opened:units, src:file) : uexp =
		let val {obj,tobj,info,using_file,asmi,asmi_rel,asme,asme_rel,tali,tali_rel,...} = F.u(pos,U)
		    val using = Fs.read blastInUnits using_file
		in  PRECOMPU
			{pos=pos, obj=obj, tobj=tobj, using=using, opened=opened,
			 src=src, asc=I, info=info, using_file=using_file,
			 asme=asme, asme_rel=asme_rel,
			 asmi=asmi, asmi_rel=asmi_rel,
			 tali=tali, tali_rel=tali_rel}
		end

	    fun precomp' (pos:pos, U:label, using:units, I:label, opened:units, src:file) : uexp =
		let val {obj,tobj,info,using_file,asme,asme_rel,asmi,asmi_rel,tali,tali_rel,...} = F.u(pos,U)
		in  PRECOMPU
			{pos=pos, obj=obj, tobj=tobj, using=using, opened=opened,
			 src=src, asc=I, info=info, using_file=using_file,
			 asme=asme, asme_rel=asme_rel,
			 asmi=asmi, asmi_rel=asmi_rel,
			 tali=tali, tali_rel=tali_rel}
		end

	    fun comp (pos:pos, U:label, opened:units, I:label) : uexp =
		let val {obj,tobj,using_file,asme,asme_rel,asmi,asmi_rel,tali,tali_rel,...} = F.u(pos,U)
		    val using = Fs.read blastInUnits using_file
		in  COMPU
			{pos=pos, obj=obj, tobj=tobj, using=using, opened=opened,
			 asc=I,using_file=using_file,
			 asme=asme,asme_rel=asme_rel,
			 asmi=asmi,asmi_rel=asmi_rel,
			 tali=tali, tali_rel=tali_rel}
		end

	    fun comp' (pos:pos, U:label, using:units, opened:units, I:label) : uexp =
		let val {obj,tobj,using_file,asme,asme_rel,asmi,asmi_rel,tali,tali_rel,...} = F.u(pos,U)
		in  COMPU
			{pos=pos, obj=obj, tobj=tobj, using=using, opened=opened,
			 asc=I,using_file=using_file, 
			 asme=asme, asme_rel=asme_rel,
			 asmi=asmi, asmi_rel=asmi_rel,
			 tali=tali, tali_rel=tali_rel}
		end
	end

	structure D =
	struct

	    fun i (I:label, iexp:iexp) : pdec =
		IDEC {name=I, iexp=iexp}

	    fun sc (pos:pos, U:label, I:label, stable:bool) : pdec =
		let val {info, tali, tali_rel} = F.sc(pos,U)
		in  SCDEC
			{pos=pos, name=U, asc=I, stable=stable,
			 info=info, tali=tali, tali_rel=tali_rel}
		end

	    fun u (U:label, uexp:uexp) : pdec =
		UDEC {name=U, uexp=uexp}
	end

    end

    (* Deconstructors. *)
    structure D =
    struct

	structure I =
	struct
	    fun src' (SRCI a) = SOME a | src' _ = NONE
	    val src = opt_out "D.I.src" src'

	    fun prim' (PRIMI a) = SOME a | prim' _ = NONE
	    val prim = opt_out "primitive interface" prim'

	    fun precomp' (PRECOMPI a) = SOME a | precomp' _ = NONE
	    val precomp = opt_out "D.I.precomp" precomp'

	    fun comp' (COMPI a) = SOME a | comp' _ = NONE
	    val comp = opt_out "D.I.comp" comp'
	end

	structure U =
	struct
	    fun src' (SRCU a) = SOME a | src' _ = NONE
	    val src = opt_out "D.U.src" src'

	    fun ssrc' (SSRCU a) = SOME a | ssrc' _ = NONE
	    val ssrc = opt_out "D.U.ssrc" ssrc'

	    fun prim' (PRIMU a) = SOME a | prim' _ = NONE
	    val prim = opt_out "D.U.prim" prim'

	    fun precomp' (PRECOMPU a) = SOME a | precomp' _ = NONE
	    val precomp = opt_out "D.U.precomp" precomp'

	    fun comp' (COMPU a) = SOME a | comp' _ = NONE
	    val comp = opt_out "D.U.comp" comp'
	end

	structure D =
	struct
	    fun i' (IDEC a) = SOME a | i' _ = NONE
	    val i = opt_out "D.D.i" i'

	    fun sc' (SCDEC a) = SOME a | sc' _ = NONE
	    val sc = opt_out "D.D.sc" sc'

	    fun u' (UDEC a) = SOME a | u' _ = NONE
	    val u = opt_out "D.D.u" u'
	end

    end

    (* Projections. *)
    structure P =
    struct

	structure I =
	struct
	    fun source' (iexp : iexp) : (units * file) option =
		(case iexp of
		    SRCI {opened,src,...} => SOME (opened,src)
		|   PRECOMPI {opened,src,...} => SOME (opened,src)
		|   _ => NONE)
	    val source = opt_out "P.I.source" source'

	    val opened = #1 o source
	    val opened' = Option.compose(#1,source')

	    val src = #2 o source
	    val src' = Option.compose(#2,source')

	    fun pos (iexp:iexp) : pos =
		(case iexp of
		    SRCI {pos,...} => pos
		|   PRIMI {pos,...} => pos
		|   PRECOMPI {pos,...} => pos
		|   COMPI {pos,...} => pos)

	    fun info' (iexp : iexp) : file option =
		(case iexp of
		    SRCI {info,...} => SOME info
		|   PRECOMPI {info,...} => SOME info
		|   _ => NONE)
	    val info = opt_out "P.I.info" info'

	    fun pinterface (iexp : iexp) : file =
		(case iexp of
		    SRCI {pinterface,...} => pinterface
		|   PRIMI {pinterface,...} => pinterface
		|   PRECOMPI {pinterface,...} => pinterface
		|   COMPI {pinterface,...} => pinterface)

	    fun using' (iexp : iexp) : units option =
		(case iexp of
		    PRECOMPI {using,...} => SOME using
		|   COMPI {using,...} => SOME using
		|   _ => NONE)
	    val using = opt_out "P.I.using" using'

	    fun stable (iexp:iexp) : bool =
		(case iexp of
		    PRECOMPI _ => true
		|   COMPI _ => true
		|   _ => false)
	end

	structure U =
	struct
	    fun source' (uexp : uexp) : (units * file) option =
		(case uexp of
		    SRCU {opened,src,...} => SOME (opened,src)
		|   SSRCU {opened,src,...} => SOME (opened,src)
		|   PRECOMPU {opened,src,...} => SOME (opened,src)
		|   _ => NONE)
	    val source = opt_out "P.U.source" source'

	    val opened = #1 o source
	    val opened' = Option.compose(#1,source')

	    val src = #2 o source
	    val src' = Option.compose(#2,source')

	    fun pos (uexp:uexp) : pos =
		(case uexp of
		    SRCU {pos,...} => pos
		|   SSRCU {pos,...} => pos
		|   PRIMU {pos,...} => pos
		|   PRECOMPU {pos,...} => pos
		|   COMPU {pos,...} => pos)

	    fun asc' (uexp : uexp) : label option =
		(case uexp of
		    SRCU _ => NONE
		|   SSRCU {asc,...} => SOME asc
		|   PRIMU {asc,...} => SOME asc
		|   PRECOMPU {asc,...} => SOME asc
		|   COMPU {asc,...} => SOME asc)
	    val asc = opt_out "P.U.asc" asc'

	    fun using' (uexp : uexp) : units option =
		(case uexp of
		    PRECOMPU {using,...} => SOME using
		|   COMPU {using,...} => SOME using
		|   _ => NONE)
	    val using = opt_out "P.U.using" using'

	    fun using_file (uexp : uexp) : file =
		(case uexp of
		    SRCU {using_file,...} => using_file
		|   SSRCU {using_file,...} => using_file
		|   PRIMU {using_file,...} => using_file
		|   PRECOMPU {using_file,...} => using_file
		|   COMPU {using_file,...} => using_file)

	    fun asm' (uexp : uexp) : file option =
		(case uexp of
		    SRCU {asm,...} => SOME asm
		|   SSRCU {asm,...} => SOME asm
		|   PRIMU {asm,...} => SOME asm
		|   _ => NONE)
	    val asm = opt_out "P.U.asm" asm'

	    fun asmz' (uexp : uexp) : file option =
		(case uexp of
		    SRCU {asmz,...} => SOME asmz
		|   SSRCU {asmz,...} => SOME asmz
		|   PRIMU {asmz,...} => SOME asmz
		|   _ => NONE)
	    val asmz = opt_out "P.U.asmz" asmz'

	    fun info' (uexp : uexp) : file option =
		(case uexp of
		    SRCU {info,...} => SOME info
		|   SSRCU {info,...} => SOME info
		|   PRECOMPU {info,...} => SOME info
		|   _ => NONE)
	    val info = opt_out "P.U.info" info'

	    fun obj (uexp : uexp) : file =
		(case uexp of
		    SRCU {obj,...} => obj
		|   SSRCU {obj,...} => obj
		|   PRIMU {obj,...} => obj
		|   PRECOMPU {obj,...} => obj
		|   COMPU {obj,...} => obj)

	    fun tobj (uexp : uexp) : file =
		(case uexp of
		    SRCU {tobj,...} => tobj
		|   SSRCU {tobj,...} => tobj
		|   PRIMU {tobj,...} => tobj
		|   PRECOMPU {tobj,...} => tobj
		|   COMPU {tobj,...} => tobj)

	    fun pinterface' (uexp : uexp) : file option =
		(case uexp of
		    SRCU {pinterface,...} => SOME pinterface
		|   _ => NONE)
	    val pinterface = opt_out "P.U.pinterface" pinterface'

	    fun tali (uexp : uexp) : file =
		(case uexp of
		    SRCU {tali,...} => tali
		|   SSRCU {tali,...} => tali
		|   PRIMU {tali,...} => tali
		|   PRECOMPU {tali,...} => tali
		|   COMPU {tali,...} => tali)

	    fun tali_rel (uexp : uexp) : file =
		(case uexp of
		    SRCU {tali_rel,...} => tali_rel
		|   SSRCU {tali_rel,...} => tali_rel
		|   PRIMU {tali_rel,...} => tali_rel
		|   PRECOMPU {tali_rel,...} => tali_rel
		|   COMPU {tali_rel,...} => tali_rel)


	    fun asme (uexp : uexp) : file =
		(case uexp of
		    SRCU {asme,...} => asme
		|   SSRCU {asme,...} => asme
		|   PRIMU {asme,...} => asme
		|   PRECOMPU {asme,...} => asme
		|   COMPU {asme,...} => asme)

	    fun asme_rel (uexp : uexp) : file =
		(case uexp of
		    SRCU {asme_rel,...} => asme_rel
		|   SSRCU {asme_rel,...} => asme_rel
		|   PRIMU {asme_rel,...} => asme_rel
		|   PRECOMPU {asme_rel,...} => asme_rel
		|   COMPU {asme_rel,...} => asme_rel)

	    fun asmi (uexp : uexp) : file =
		(case uexp of
		    SRCU {asmi,...} => asmi
		|   SSRCU {asmi,...} => asmi
		|   PRIMU {asmi,...} => asmi
		|   PRECOMPU {asmi,...} => asmi
		|   COMPU {asmi,...} => asmi)

	    fun asmi_rel (uexp : uexp) : file =
		(case uexp of
		    SRCU {asmi_rel,...} => asmi_rel
		|   SSRCU {asmi_rel,...} => asmi_rel
		|   PRIMU {asmi_rel,...} => asmi_rel
		|   PRECOMPU {asmi_rel,...} => asmi_rel
		|   COMPU {asmi_rel,...} => asmi_rel)

	    fun stable (uexp:uexp) : bool =
		(case uexp of
		    PRECOMPU _ => true
		|   COMPU _ => true
		|   _ => false)
	end

	structure D =
	struct

	    fun name (pdec : pdec) : label =
		(case pdec of
		    IDEC {name,...} => name
		|   SCDEC {name,...} => name
		|   UDEC {name,...} => name)

	    fun pos (pdec : pdec) : pos =
		(case pdec of
		    IDEC {iexp,...} => I.pos iexp
		|   SCDEC {pos,...} => pos
		|   UDEC {uexp,...} => U.pos uexp)

	    fun iexp' (pdec:pdec) : iexp option =
		(case pdec of
		    IDEC {iexp,...} => SOME iexp
		|   _ => NONE)
	    val iexp = opt_out "P.D.iexp" iexp'

	    fun uexp' (pdec:pdec) : uexp option =
		(case pdec of
		    UDEC {uexp,...} => SOME uexp
		|   _ => NONE)
	    val uexp = opt_out "P.D.uexp" uexp'

	    fun asc' (pdec:pdec) : label option =
		(case pdec of
		    SCDEC {asc,...} => SOME asc
		|   UDEC {uexp,...} => U.asc' uexp
		|   _ => NONE)
	    val asc = opt_out "P.D.asc" asc'

	    fun stable (pdec:pdec) : bool =
		(case pdec of
		    IDEC {iexp,...} => I.stable iexp
		|   SCDEC {stable,...} => stable
		|   UDEC {uexp,...} => U.stable uexp)

	    fun src' (pdec:pdec) : file option =
		(case pdec of
		    IDEC {iexp,...} => I.src' iexp
		|   SCDEC _ => NONE
		|   UDEC {uexp,...} => U.src' uexp)
	    val src = opt_out "P.D.src" src'

	    fun tali' (pdec:pdec) : file option =
		(case pdec of
		    IDEC _ => NONE
		|   SCDEC {tali,...} => SOME tali
		|   UDEC {uexp,...} => SOME (U.tali uexp))
	    val tali = opt_out "P.D.tali" tali'

	    fun tali_rel' (pdec:pdec) : file option =
		(case pdec of
		    IDEC _ => NONE
		|   SCDEC {tali_rel,...} => SOME tali_rel
		|   UDEC {uexp,...} => SOME (U.tali_rel uexp))
	    val tali_rel = opt_out "P.D.tali_rel" tali_rel'

	    fun info' (pdec : pdec) : file option =
		(case pdec of
		    IDEC {iexp,...} => I.info' iexp
		|   SCDEC {info,...} => SOME info
		|   UDEC {uexp,...} => U.info' uexp)
	    val info = opt_out "P.D.info" info'
	end
    end

    fun blastOutIexp (os:B.outstream) (iexp:iexp) : unit =
	(case iexp of
	    SRCI {pos,opened,src,...} =>
		(B.blastOutInt os 0; Pos.blastOutPos os pos;
		 blastOutUnits' os opened; B.blastOutString os src)
	|   PRIMI {pos,...} =>
		(B.blastOutInt os 1; Pos.blastOutPos os pos)
	|   PRECOMPI {pos,opened,src,...} =>
		(B.blastOutInt os 2; Pos.blastOutPos os pos;
		 blastOutUnits' os opened; B.blastOutString os src)
	|   COMPI {pos,...} =>
		(B.blastOutInt os 3; Pos.blastOutPos os pos))

    fun blastInIexp (is:B.instream) (I:label) : iexp =
	(case (B.blastInInt is) of
	    0 => C.I.src(Pos.blastInPos is,I,blastInUnits' is,B.blastInString is)
	|   1 => C.I.prim(Pos.blastInPos is,I)
	|   2 => C.I.precomp(Pos.blastInPos is,I,blastInUnits' is,B.blastInString is)
	|   3 => C.I.comp(Pos.blastInPos is,I)
	|   _ => error "blastInIexp")

    fun blastOutUexp (os:B.outstream) (uexp:uexp) : unit =
	(case uexp of
	    SRCU {pos,opened,src,...} =>
		(B.blastOutInt os 0; Pos.blastOutPos os pos;
		 blastOutUnits' os opened; B.blastOutString os src)
	|   SSRCU {pos,asc=I,opened,src,...} =>
		(B.blastOutInt os 1; Pos.blastOutPos os pos;
		 blastOutLabel os I; blastOutUnits' os opened;
		 B.blastOutString os src)
	|   PRIMU {pos,asc=I,...} =>
		(B.blastOutInt os 2; Pos.blastOutPos os pos;
		 blastOutLabel os I)
	|   PRECOMPU {pos,asc=I,opened,src,...} =>
		(B.blastOutInt os 3; Pos.blastOutPos os pos;
		 blastOutLabel os I; blastOutUnits' os opened;
		 B.blastOutString os src)
	|   COMPU {pos,opened,asc=I,...} =>
		(B.blastOutInt os 4; Pos.blastOutPos os pos;
		 blastOutUnits' os opened; blastOutLabel os I))

    fun blastInUexp (is:B.instream) (U:label) : uexp =
	(case (B.blastInInt is) of
	    0 => C.U.src(Pos.blastInPos is,U,blastInUnits' is,B.blastInString is)
	|   1 =>
		C.U.ssrc(Pos.blastInPos is,U,blastInLabel is,blastInUnits' is,
		    B.blastInString is)
	|   2 => C.U.prim(Pos.blastInPos is,U,blastInLabel is)
	|   3 =>
		C.U.precomp(Pos.blastInPos is,U,blastInLabel is,
		    blastInUnits' is,B.blastInString is)
	|   4 => C.U.comp(Pos.blastInPos is,U,blastInUnits' is,blastInLabel is)
	|   _ => error "blastInUexp")

    fun blastOutPdec (os:B.outstream) (pdec:pdec) : unit =
	(case pdec of
	    IDEC {name=I,iexp,...} =>
		(B.blastOutInt os 0; blastOutLabel os I; blastOutIexp os iexp)
	|   SCDEC {pos,name=U,asc=I,stable,...} =>
		(B.blastOutInt os 1; Pos.blastOutPos os pos; blastOutLabel os U;
		 blastOutLabel os I; B.blastOutBool os stable)
	|   UDEC {name=U,uexp=uexp,...} =>
		(B.blastOutInt os 2; blastOutLabel os U; blastOutUexp os uexp))

    fun blastInPdec (is:B.instream) : pdec =
	(case (B.blastInInt is) of
	    0 =>
		let val I = blastInLabel is
		in  C.D.i(I,blastInIexp is I)
		end
	|   1 =>
		C.D.sc(Pos.blastInPos is, blastInLabel is,
		    blastInLabel is, B.blastInBool is)
	|   2 =>
		let val U = blastInLabel is
		in  C.D.u(U,blastInUexp is U)
		end
	|   _ => error "blastInPdec")

    val blastOutDesc : B.outstream -> desc -> unit =
	B.blastOutList blastOutPdec

    val blastInDesc : B.instream -> desc =
	B.blastInList blastInPdec

    val (blastOutDesc, blastInDesc) =
	B.magic (blastOutDesc, blastInDesc, "desc $Revision$")

    type format = Fmt.format
    val String = Fmt.String
    val Break = Fmt.Break
    val Eq = String " = "
    val Has = String " : "
    val Box : format list -> format = Fmt.HOVbox

    val pp_label = String o Name.label2name'
    val pp_pos = Pos.pp_pos

    fun pp_units (units:units) : format =
	Fmt.pp_list pp_label units

    fun pp_iexp (iexp : iexp) : format =
	(case iexp of
	    SRCI {pos,opened,src,info,pinterface} =>
		Box[String "SRCI", Break,
		    String "pos = ", pp_pos pos, Break,
		    String "opened = ", pp_units opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "pinterface = ", String pinterface]
	|   PRIMI {pos,pinterface} =>
		Box[String "PRIMI", Break,
		    String "pos = ", pp_pos pos, Break,
		    String "pinterface = ", String pinterface]
	|   PRECOMPI {pos,pinterface,using,opened,src,info} =>
		Box[String "PRECOMPI", Break,
		    String "pos = ", pp_pos pos, Break,
		    String "pinterface = ", String pinterface, Break,
		    String "using = ", pp_units using, Break,
		    String "opened = ", pp_units opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info]
	|   COMPI {pos,pinterface,using} =>
		Box[String "COMPI", Break,
		    String "pos = ", pp_pos pos, Break,
		    String "pinterface = ", String pinterface, Break,
		    String "using = ", pp_units using])

    fun pp_uexp (uexp : uexp) : format =
	(case uexp of
	    SRCU {pos,opened,src,info,pinterface,obj,tobj,asm,asmz,using_file,asme,asme_rel,asmi,asmi_rel,tali,tali_rel} =>
		Box[String "SRCU", Break,
		    String "pos = ", pp_pos pos, Break,
		    String "opened = ", pp_units opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "pinterface = ", String pinterface, Break,
		    String "obj = ", String obj, Break,
		    String "tobj = ", String tobj, Break,
		    String "asm = ", String asm, Break,
		    String "asmz = ", String asmz, Break,
		    String "using_file = ", String using_file, Break,
		    String "tali = ", String tali, Break,
		    String "tali_rel = ", String tali_rel, Break,
		    String "asme = ", String asme, Break,
		    String "asme_rel = ", String asme_rel, Break,
		    String "asmi = ", String asmi, Break,
		    String "asmi_rel = ", String asmi_rel]
	|   SSRCU {pos,opened,src,asc=I,info,obj,tobj,asm,asmz,using_file,tali,tali_rel,asme,asme_rel,asmi,asmi_rel} =>
		Box[String "SSRCU", Has, pp_label I, Break,
		    String "pos = ", pp_pos pos, Break,
		    String "opened = ", pp_units opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "obj = ", String obj, Break,
		    String "tobj = ", String tobj, Break,
		    String "asm = ", String asm, Break,
		    String "asmz = ", String asmz, Break,
		    String "using_file = ", String using_file, Break,
		    String "tali = ", String tali, Break,
		    String "tali_rel = ", String tali_rel, Break,
		    String "asme = ", String asme, Break,
		    String "asme_rel = ", String asme_rel, Break,
		    String "asmi = ", String asmi, Break,
		    String "asmi_rel = ", String asmi_rel]
	|   PRIMU {pos,asc=I,obj,tobj,asm,asmz,using_file,asme,asme_rel,asmi,asmi_rel,tali,tali_rel} =>
		Box[String "PRIMU", Has, pp_label I, Break,
		    String "pos = ", pp_pos pos, Break,
		    String "obj = ", String obj, Break,
		    String "tobj = ", String tobj, Break,
		    String "asm = ", String asm, Break,
		    String "asmz = ", String asmz, Break,
		    String "using_file = ", String using_file, Break,
		    String "tali = ", String tali, Break,
		    String "tali_rel = ", String tali_rel, Break,
		    String "asme = ", String asme, Break,
		    String "asme_rel = ", String asme_rel, Break,
		    String "asmi = ", String asmi, Break,
		    String "asmi_rel = ", String asmi_rel]
	|   PRECOMPU {pos,obj,tobj,using,opened,src,asc=I,info,using_file,tali,tali_rel,asme,asme_rel,asmi,asmi_rel} =>
		Box[String "PRECOMPU", Has, pp_label I, Break,
		    String "pos = ", pp_pos pos, Break,
		    String "obj = ", String obj, Break,
		    String "tobj = ", String tobj, Break,
		    String "using = ", pp_units using, Break,
		    String "opened = ", pp_units opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "using_file = ", String using_file, Break,
		    String "tali = ", String tali, Break,
		    String "tali_rel = ", String tali_rel, Break,
		    String "asme = ", String asme, Break,
		    String "asme_rel = ", String asme_rel, Break,
		    String "asmi = ", String asmi, Break,
		    String "asmi_rel = ", String asmi_rel]
	|   COMPU {pos,obj,tobj,using,opened,asc=I,using_file,tali,tali_rel,asme,asme_rel,asmi,asmi_rel} =>
		Box[String "COMPU", Has, pp_label I, Break,
		    String "pos = ", pp_pos pos, Break,
		    String "obj = ", String obj, Break,
		    String "tobj = ", String tobj, Break,
		    String "using = ", pp_units using, Break,
		    String "opened = ", pp_units opened, Break,
		    String "using_file = ", String using_file, Break,
		    String "tali = ", String tali, Break,
		    String "tali_rel = ", String tali_rel, Break,
		    String "asme = ", String asme, Break,
		    String "asme_rel = ", String asme_rel, Break,
		    String "asmi = ", String asmi, Break,
		    String "asmi_rel = ", String asmi_rel])

    fun pp_pdec (pdec:pdec) : format =
	(case pdec of
	    IDEC {name=I,iexp} => Box[pp_label I, Eq, pp_iexp iexp]
	|   SCDEC {pos,name=U,asc=I,stable,info,tali,tali_rel} =>
		Box[pp_label U, Has, pp_label I, Break,
		    String "pos = ", pp_pos pos, Break,
		    String "stable = ", String (Bool.toString stable), Break,
		    String "info = ", String info, Break,
		    String "tali = ", String tali, Break,
		    String "tali_rel = ", String tali_rel]
	|   UDEC {name=U,uexp} => Box[pp_label U, Eq, pp_uexp uexp])

    fun pp_desc (desc:desc) : format =
	let val formats = map pp_pdec desc
	    val formats = Listops.join Break formats
	in  Fmt.Vbox formats
	end

    (*
	Syntactic definitions and well-formedness checks.
    *)

    fun free_units (units : units) : set = S.addList (S.empty, units)

    fun free_iexp (iexp : iexp) : set =
	(case iexp of
	    SRCI {opened,...}=> free_units opened
	|   PRIMI _ => S.empty
	|   PRECOMPI {using,opened,...} =>
		S.union (free_units using, free_units opened)
	|   COMPI {using,...} => free_units using)

    fun free_uexp (uexp : uexp) : set =
	(case uexp of
	    SRCU {opened,...}=> free_units opened
	|   SSRCU {opened,asc=I,...} => S.add(free_units opened, I)
	|   PRIMU {asc=I,...} => S.singleton I
	|   PRECOMPU {using,opened,asc=I,...} =>
		S.add(S.union(free_units using, free_units opened), I)
	|   COMPU {using,opened,asc=I,...} =>
		S.add(S.union(free_units using, free_units opened),I))

    fun free_pdec (pdec : pdec) : set =
	(case pdec of
	    IDEC {iexp,...} => free_iexp iexp
	|   SCDEC {asc=I,...} => S.singleton I
	|   UDEC {uexp,...} => free_uexp uexp)

    fun pdec_ok (pdec : pdec, dom : set) : set =
	let val l = P.D.name pdec
	    val redefined = S.member (dom,l)
	    val missing = S.difference (free_pdec pdec, dom)
	    fun fail msg =
		error (concat[Pos.tostring (P.D.pos pdec), ": ",
		    Name.label2longname l, " ", msg])
	in
	    (case (redefined, not (S.isEmpty missing)) of
		(false, false) => S.add(dom,l)
	    |	(true, _) => fail "redefined"
	    |	(_, true) =>
		    let val labels = S.listItems missing
			val strings = map Name.label2longname labels
			val missing = Listops.concatWith " " strings
		    in	fail ("names undefined units/interfaces: " ^ missing)
		    end)
	end

    fun check_desc (what:string, desc : desc) : unit =
	(ignore (foldl pdec_ok S.empty desc)
	 handle e =>
	    (print (what ^ ": project description not well-formed\n");
	     Fmt.print_fmt(pp_desc desc); print "\n";
	     raise e))

    (*
	Compiler inputs and summary.
    *)

    type inputs = desc * pdec

    structure S =
    struct

	type crc = Crc.crc

	structure D =
	struct

	    datatype iexp =
		PRECOMPI of units * crc (* opened, source *)
	    |	COMPI of crc	(* pinterface *)

	    datatype pdec =
		IDEC of label * iexp	(* I = iexp *)
	    |	SCDEC of label * label	(* U : I *)

	    type desc = pdec list

	    fun blastOutIexp (os:B.outstream) (iexp:iexp) : unit =
		(case iexp of
		    PRECOMPI (units, src) =>
			(B.blastOutInt os 0; blastOutUnits' os units;
			 Crc.blastOutCrc os src)
		|   COMPI pinterface =>
			(B.blastOutInt os 1; Crc.blastOutCrc os pinterface))

	    fun blastInIexp (is:B.instream) : iexp =
		(case (B.blastInInt is) of
		    0 => PRECOMPI (blastInUnits' is, Crc.blastInCrc is)
		|   1 => COMPI (Crc.blastInCrc is)
		|   _ => error "S.D.blastInIexp")

	    fun blastOutPdec (os:B.outstream) (pdec:pdec) : unit =
		(case pdec of
		    IDEC (I,iexp) =>
			(B.blastOutInt os 0; blastOutLabel os I;
			 blastOutIexp os iexp)
		|   SCDEC (U,I) =>
			(B.blastOutInt os 1; blastOutLabel os U;
			 blastOutLabel os I))

	    fun blastInPdec (is:B.instream) : pdec =
		(case (B.blastInInt is) of
		    0 => IDEC (blastInLabel is, blastInIexp is)
		|   1 => SCDEC (blastInLabel is, blastInLabel is)
		|   _ => error "S.D.blastInPdec")

	    val blastOutDesc : B.outstream -> desc -> unit =
		B.blastOutList blastOutPdec

	    val blastInDesc : B.instream -> desc =
		B.blastInList blastInPdec

	    fun pp_iexp (iexp : iexp) : format =
		(case iexp of
		    PRECOMPI (opened,src) =>
			Box[String "PRECOMPI", Break,
			    String "opened = ", pp_units opened, Break,
			    String "src = ", Crc.pp_crc src]
		|   COMPI pinterface =>
			Box[String "COMPI", Break,
			    String "pinterface = ", Crc.pp_crc pinterface])

	    fun pp_pdec (pdec:pdec) : format =
		(case pdec of
		    IDEC (I,iexp) => Box[pp_label I, Eq, pp_iexp iexp]
		|   SCDEC (U,I) => Box[pp_label U, Has, pp_label I])

	    fun pp_desc (desc:desc) : format =
		let val formats = map pp_pdec desc
		    val formats = Listops.join Break formats
		in  Fmt.Vbox formats
		end

	end

	structure P =
	struct

	    datatype iexp =
		SRCI of units * crc (* opened, source *)

	    datatype uexp =
		SRCU of units * crc (* opened, source *)
	    |	SSRCU of label * units * crc	(* ascription, opened, source *)

	    datatype pdec =
		IDEC of label * iexp
	    |	SCDEC of label * label
	    |	UDEC of label * uexp

	    fun blastOutIexp (os:B.outstream) (iexp:iexp) : unit =
		let val SRCI (opened,src) = iexp
		in  blastOutUnits' os opened; Crc.blastOutCrc os src
		end

	    fun blastInIexp (is:B.instream) : iexp =
		SRCI(blastInUnits' is,Crc.blastInCrc is)

	    fun blastOutUexp (os:B.outstream) (uexp:uexp) : unit =
		(case uexp of
		    SRCU (opened,src) =>
			(B.blastOutInt os 0; blastOutUnits' os opened;
			 Crc.blastOutCrc os src)
		|   SSRCU (I,opened,src) =>
			(B.blastOutInt os 1; blastOutLabel os I;
			 blastOutUnits' os opened; Crc.blastOutCrc os src))

	    fun blastInUexp (is:B.instream) : uexp =
		(case (B.blastInInt is) of
		    0 => SRCU(blastInUnits' is,Crc.blastInCrc is)
		|   1 => SSRCU(blastInLabel is,blastInUnits' is,Crc.blastInCrc is)
		|   _ => error "S.P.blastInUexp")

	    fun blastOutPdec (os:B.outstream) (pdec:pdec) : unit =
		(case pdec of
		    IDEC (I,iexp) =>
			(B.blastOutInt os 0; blastOutLabel os I;
			 blastOutIexp os iexp)
		|   SCDEC (U,I) =>
			(B.blastOutInt os 1; blastOutLabel os U;
			 blastOutLabel os I)
		|   UDEC (U,uexp) =>
			(B.blastOutInt os 2; blastOutLabel os U;
			 blastOutUexp os uexp))

	    fun blastInPdec (is:B.instream) : pdec =
		(case (B.blastInInt is) of
		    0 => IDEC(blastInLabel is,blastInIexp is)
		|   1 => SCDEC(blastInLabel is,blastInLabel is)
		|   2 => UDEC(blastInLabel is,blastInUexp is)
		|   _ => error "S.P.blastInPdec")

	    fun pp_iexp (iexp:iexp) : format =
		let val SRCI(opened,src) = iexp
		in  Box[String "SRCI", Break,
			String "opened = ", pp_units opened, Break,
			String "src = ", Crc.pp_crc src]
		end

	    fun pp_uexp (uexp : uexp) : format =
		(case uexp of
		    SRCU (opened,src) =>
			Box[String "SRCU", Break,
			    String "opened = ", pp_units opened, Break,
			    String "src = ", Crc.pp_crc src]
		|   SSRCU (I,opened,src) =>
			Box[String "SSRCU", Has, pp_label I, Break,
			    String "opened = ", pp_units opened, Break,
			    String "src = ", Crc.pp_crc src])

	    fun pp_pdec (pdec:pdec) : format =
		(case pdec of
		    IDEC (I,iexp) => Box[pp_label I, Eq, pp_iexp iexp]
		|   SCDEC (U,I) => Box[pp_label U, Has, pp_label I]
		|   UDEC (U,uexp) => Box[pp_label U, Eq, pp_uexp uexp])

	end

	type summary = D.desc * P.pdec

	val blastOutSummary : Blaster.outstream -> summary -> unit =
	    B.blastOutPair D.blastOutDesc P.blastOutPdec

	val blastInSummary : Blaster.instream -> summary =
	    B.blastInPair D.blastInDesc P.blastInPdec

	val (blastOutSummary, blastInSummary) =
	    B.magic (blastOutSummary, blastInSummary, "summary $Revision$")

	fun pp_summary ((desc,pdec):summary) : format =
	    Box [String "SUMMARY", Break,
		String "desc = ", D.pp_desc desc, Break,
		String "pdec = ", P.pp_pdec pdec]

    end

end
