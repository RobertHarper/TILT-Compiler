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

	val TM = "TM"
	val U = "U"
	val I = "I"
	val L = "L"
	val C = "C"
	val Info = "info"
	val Context = "context"
	val Pinterface = "pinterface"
	val Parm = "parm"
	val Asm = "asm"
	val Asmz = "asmz"
	val Obj = "obj"
	val Tmp = "tmp"
	val Lib = "Lib"
	val Bin = "Bin"
	val Runtime = "Runtime"
	val Tilslave = "til_slave"
	val Basis = "basis"
	val Impl = "impl"
	val Inter = "inter"
	val Desc = "desc"

	val Target : unit -> string = Target.targetString

	fun pdir (desc:file) : file =
	    dir desc/TM/file desc

	structure I =
	struct
	    type src = {src:file, info:file, pinterface:file}
	    type prim = {pinterface:file}
	    type precomp = src
	    type comp = prim

	    fun idir (desc:file, l:label) : file =
		pdir desc/I/Name.label2name' l

	    fun src (desc:file, I:label, src:file) : src =
		let val root = idir (desc,I)
		in  {src=src, info=root/Info,
		     pinterface=root/Pinterface}
		end

	    fun prim (desc:file, I:label) : prim =
		let val root = idir (desc,I)
		in  {pinterface=root/Pinterface}
		end

	    val precomp = src
	    val comp = prim
	end

	structure U =
	struct

	    type src = {src:file, info:file, pinterface:file, obj:file,
			asm:file, asmz:file, parm:file}
	    type ssrc = {src:file, info:file, obj:file,
			 asm:file, asmz:file, parm:file}
	    type prim = {info:file, obj:file, asm:file,
			 asmz:file, parm:file}
	    type precomp = {obj:file, src:file, info:file,
			    parm:file}
	    type comp = {obj:file, parm:file}

	    fun udir (desc:file, l:label) : file =
		pdir desc/U/Name.label2name' l

	    fun src (desc:file, U:label, src:file) : src =
		let val root = udir (desc,U)
		    val root' = root/Target()
		in  {src=src, info=root/Info,
		     pinterface=root/Pinterface, obj=root'/Obj,
		     asm=root'/Asm, asmz=root'/Asmz, parm=root/Parm}
		end

	    fun ssrc (desc:file, U:label, src:file) : ssrc =
		let val root = udir (desc,U)
		    val root' = root/Target()
		in  {src=src, info=root/Info, obj=root'/Obj,
		     asm=root'/Asm, asmz=root'/Asmz, parm=root/Parm}
		end

	    fun prim (desc:file, U:label) : prim =
		let val root = udir (desc,U)
		    val root' = root/Target()
		in  {info=root/Info, obj=root'/Obj,
		     asm=root'/Asm, asmz=root'/Asmz, parm=root/Parm}
		end

	    fun precomp (desc:file, U:label, src:file) : precomp =
		let val root = udir (desc,U)
		    val root' = root/Target()
		in  {obj=root'/Obj, src=src, info=root/Info,
		     parm=root/Parm}
		end

	    fun comp (desc:file, U:label) : comp =
		let val root = udir (desc,U)
		    val root' = root/Target()
		in  {obj=root'/Obj, parm=root/Parm}
		end
	end

	type link = {exe:file, asm:file, asmz:file, obj:file}

	fun link (project:file, exe:file) : link =
	    let val root = pdir project/L/file exe
	    in  {exe=exe, asm=root/Asm, asmz=root/Asmz, obj=root/Obj}
	    end

        type pack = {libdir:file, src:label -> file}
	val inter = Inter
	val impl = Impl
	val desc = Desc
	fun pack (libdir : file) : pack =
	    let fun src (l:label) : file =
		    let val space =
			    if Name.is_unit l then U
			    else if Name.is_interface l then I
			    else error "pack saw unexpected label"
			val name = Name.label2name' l
		    in	space/name
		    end
	    in  {libdir=libdir, src=src}
	    end

	val cwd : unit -> string =
	    Util.memoize OS.FileSys.getDir

	fun commdir () : file =
	    let val r = cwd()/TM/C
		val _ = Fs.mkdirs r
	    in  r
	    end
	val commdir = Util.memoize commdir

	fun full_path (path:file) : file =
	    OS.FileSys.fullPath path

	fun tiltroot () : file =
	    let val Env = "TILTROOT"
		val root =
		    (case OS.Process.getEnv Env
		       of NONE => ""
			| SOME dir => dir)
		val root = full_path root
		val have_root =
		    (OS.FileSys.isDir root andalso
		     OS.FileSys.access(root, [OS.FileSys.A_READ,
					      OS.FileSys.A_EXEC]))
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
	    in  dir = basisdir()
	    end
	val runtimedir = (fn () => tiltroot()/Runtime)
	val til_slave = (fn () => tiltroot()/Bin/Tilslave)

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

    val (blastOutParm, blastInParm) =
	B.magic (B.blastOutList NameBlast.blastOutLabel,
		 B.blastInList NameBlast.blastInLabel,
		 "using $Revision$")

    fun opt_out (what:string) (f:'a -> 'b option) : 'a -> 'b =
	(fn x =>
	 (case f x
	    of SOME y => y
	     | NONE => error (what ^ " failed")))

    (* Projections. *)
    structure P =
    struct

	structure I =
	struct
	    fun src' (iexp : iexp) : file option =
		(case iexp
		   of SRCI (_,files) => SOME (#src files)
		    | PRECOMPI (_,_,files) => SOME (#src files)
		    | _ => NONE)
	    val src = opt_out "P.I.src" src'

	    fun info' (iexp : iexp) : file option =
		(case iexp
		   of SRCI (_,files) => SOME (#info files)
		    | PRECOMPI (_,_,files) => SOME (#info files)
		    | _ => NONE)
	    val info = opt_out "P.I.info" info'

	    fun pinterface (iexp : iexp) : file =
		(case iexp
		   of SRCI (_,files) => #pinterface files
		    | PRIMI files => #pinterface files
		    | PRECOMPI (_,_,files) => #pinterface files
		    | COMPI (_,files) => #pinterface files)

	    fun using' (iexp : iexp) : using option =
		(case iexp
		   of PRECOMPI (using,_,_) => SOME using
		    | _ => NONE)
	    val using = opt_out "P.I.using" using'
	end

	structure U =
	struct
	    fun src' (uexp : uexp) : file option =
		(case uexp
		   of SRCU (_,files) => SOME (#src files)
		    | SSRCU (_,_,files) => SOME (#src files)
		    | PRECOMPU (_,_,_,files) => SOME (#src files)
		    | _ => NONE)
	    val src = opt_out "P.U.src" src'

	    fun info' (uexp : uexp) : file option =
		(case uexp
		   of SRCU (_,files) => SOME (#info files)
		    | SSRCU (_,_,files) => SOME (#info files)
		    | PRIMU (_,files) => SOME (#info files)
		    | PRECOMPU (_,_,_,files) => SOME (#info files)
		    | _ => NONE)
	    val info = opt_out "P.U.info" info'

	    fun pinterface' (uexp : uexp) : file option =
		(case uexp
		   of SRCU (_,files) => SOME (#pinterface files)
		    | _ => NONE)
	    val pinterface = opt_out "P.U.pinterface" pinterface'

	    fun obj (uexp : uexp) : file =
		(case uexp
		   of SRCU (_,files) => #obj files
		    | SSRCU (_,_,files) => #obj files
		    | PRIMU (_,files) => #obj files
		    | PRECOMPU (_,_,_,files) => #obj files
		    | COMPU (_,_,files) => #obj files)

	    fun asm' (uexp : uexp) : file option =
		(case uexp
		   of SRCU (_,files) => SOME (#asm files)
		    | SSRCU (_,_,files) => SOME (#asm files)
		    | PRIMU (_,files) => SOME (#asm files)
		    | _ => NONE)
	    val asm = opt_out "P.U.asm" asm'

	    fun asmz' (uexp : uexp) : file option =
		(case uexp
		   of SRCU (_,files) => SOME (#asmz files)
		    | SSRCU (_,_,files) => SOME (#asmz files)
		    | PRIMU (_,files) => SOME (#asmz files)
		    | _ => NONE)
	    val asmz = opt_out "P.U.asmz" asmz'

	    fun parm (uexp : uexp) : file =
		(case uexp
		   of SRCU (_,files) => #parm files
		    | SSRCU (_,_,files) => #parm files
		    | PRIMU (_,files) => #parm files
		    | PRECOMPU (_,_,_,files) => #parm files
		    | COMPU (_,_,files) => #parm files)

	    fun using' (uexp : uexp) : using option =
		(case uexp
		   of PRECOMPU (using,_,_,_) => SOME using
		    | COMPU (using,_,_) => SOME using
		    | _ => NONE)
	    val using = opt_out "P.U.using" using'

	    fun asc' (uexp : uexp) : label option =
		(case uexp
		   of SRCU _ => NONE
		    | SSRCU (_,I,_) => SOME I
		    | PRIMU (I,_) => SOME I
		    | PRECOMPU (_,_,I,_) => SOME I
		    | COMPU (_,I,_) => SOME I)
	    val asc = opt_out "P.U.asc" asc'
	end

	fun label (pdec : pdec) : label =
	    (case pdec
	       of IDEC (I,_,_) => I
		| SCDEC (U,_,_) => U
		| UDEC (U,_,_) => U)

	fun pos (pdec : pdec) : pos =
	    (case pdec
	       of IDEC (_,_,pos) => pos
		| SCDEC (_,_,pos) => pos
		| UDEC (_,_,pos) => pos)

	fun info' (pdec : pdec) : file option =
	    (case pdec
	       of IDEC (_,iexp,_) => I.info' iexp
		| SCDEC _ => NONE
		| UDEC (_,uexp,_) => U.info' uexp)

	val info = opt_out "P.info" info'
    end

    structure C =
    struct
	structure I =
	struct

	    fun src (desc:file, I:label, opened:opened, src:file) : iexp =
		SRCI(opened, F.I.src (desc,I,src))

	    fun prim (desc:file, I:label) : iexp =
		PRIMI (F.I.prim (desc,I))

	    fun precomp (desc:file, I:label, opened:opened, src:file) : iexp =
		let val files = F.I.precomp (desc,I,src)
		    val pi = #pinterface files
		    val using = Fs.read_pinterface_parm pi
		in  PRECOMPI(using,opened,files)
		end

	    fun comp (desc:file, I:label) : iexp =
		let val files = F.I.comp (desc,I)
		    val pi = #pinterface files
		    val using = Fs.read_pinterface_parm pi
		in  COMPI (using,files)
		end
	end

	structure U =
	struct

	    val read_using : file -> using =
		Fs.read blastInParm

	    fun src (desc:file, U:label, opened:opened, src:file) : uexp =
		SRCU(opened,F.U.src (desc,U,src))

	    fun ssrc (desc:file, U:label, I:label,
		      opened:opened, src:file) : uexp =
		SSRCU(opened,I,F.U.ssrc (desc,U,src))

	    fun prim (desc:file, U:label, I:label) : uexp =
		PRIMU(I,F.U.prim (desc,U))

	    fun precomp (desc:file, U:label, I:label,
			  opened:opened, src:file) : uexp =
		let val files = F.U.precomp (desc,U,src)
		    val using = read_using (#parm files)
		in  PRECOMPU(using,opened,I,files)
		end

	    fun comp (desc:file, U:label, I:label) : uexp =
		let val files = F.U.comp (desc,U)
		    val using = read_using (#parm files)
		in  COMPU(using,I,files)
		end
	end
    end

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

	structure Pdec =
	struct
	    fun idec' (IDEC a) = SOME a | idec' _ = NONE
	    val idec = opt_out "D.Pdec.idec" idec'

	    fun scdec' (SCDEC a) = SOME a | scdec' _ = NONE
	    val scdec = opt_out "D.Pdec.scdec" scdec'

	    fun udec' (UDEC a) = SOME a | udec' _ = NONE
	    val udec = opt_out "D.Pdec.udec" udec'
	end

    end

    (*
	Syntactic definitions and well-formedness checks.
    *)

    fun free_opened (opened : opened) : set = S.addList (S.empty, opened)

    val free_using = free_opened

    fun free_iexp (iexp : iexp) : set =
	(case iexp
	   of SRCI (opened,_) => free_opened opened
	    | PRIMI _ => S.empty
	    | PRECOMPI (using,opened,_) =>
		S.union (free_using using, free_opened opened)
	    | COMPI (using,_) => free_using using)

    fun free_uexp (uexp : uexp) : set =
	(case uexp
	   of SRCU (opened,_) => free_opened opened
	    | SSRCU (opened,I,_) => S.add(free_opened opened, I)
	    | PRIMU (I,_) => S.singleton I
	    | PRECOMPU (using, opened, I, _) =>
		S.add(S.union(free_using using, free_opened opened), I)
	    | COMPU (using,I,_) => S.add(free_using using,I))

    fun free_pdec (pdec : pdec) : set =
	(case pdec
	   of IDEC (_,iexp,_) => free_iexp iexp
	    | SCDEC (_,I,_) => S.singleton I
	    | UDEC (_,uexp,_) => free_uexp uexp)

    fun pdec_ok (pdec : pdec, dom : set) : set =
	let val l = P.label pdec
	    val redefined = S.member (dom,l)
	    val missing = S.difference (free_pdec pdec, dom)
	    fun fail msg =
		error (concat[Pos.tostring (P.pos pdec), ": ",
			      Name.label2longname (P.label pdec),
			      " ", msg])
	in
	    (case (redefined, not (S.isEmpty missing))
	       of (false, false) => S.add(dom,l)
		| (true, _) => fail "redefined"
		| (_, true) =>
		    let val labels = S.listItems missing
			val strings = map Name.label2longname labels
			val missing = Listops.concatWith "    " strings
		    in  fail ("names undefined units/interfaces: " ^ missing)
		    end)
	end

    fun check_desc (desc : desc) : unit =
	ignore (foldl pdec_ok S.empty desc)

    fun say (s : string) : unit =
	if !B.BlastDebug then (print s; print "\n") else ()

    val blastInLabel = NB.blastInLabel
    val blastOutLabel = NB.blastOutLabel

    val blastOutOpened : B.outstream -> opened -> unit =
	B.blastOutList blastOutLabel
    val blastInOpened : B.instream -> opened =
	B.blastInList blastInLabel

    fun blastOutPdec (os:B.outstream) (pdec:pdec) : unit =
	(case pdec
	   of IDEC(I,SRCI(opened,{src,...}),pos) =>
		(B.blastOutInt os 0; Pos.blastOutPos os pos;
		 blastOutLabel os I; blastOutOpened os opened;
		 B.blastOutString os src)
	    | IDEC(I,PRIMI _,pos) =>
		(B.blastOutInt os 1; Pos.blastOutPos os pos; blastOutLabel os I)
	    | IDEC(I,PRECOMPI (_,opened,{src,...}),pos) =>
		(B.blastOutInt os 2; Pos.blastOutPos os pos;
		 blastOutLabel os I; blastOutOpened os opened;
		 B.blastOutString os src)
	    | IDEC(I,COMPI _,pos) =>
		(B.blastOutInt os 3; Pos.blastOutPos os pos; blastOutLabel os I)
	    | UDEC(U,SRCU(opened,{src,...}),pos) =>
		(B.blastOutInt os 4; Pos.blastOutPos os pos;
		 blastOutLabel os U; blastOutOpened os opened;
		 B.blastOutString os src)
	    | UDEC(U,SSRCU(opened,I,{src,...}),pos) =>
		(B.blastOutInt os 5; Pos.blastOutPos os pos;
		 blastOutLabel os U; blastOutLabel os I;
		 blastOutOpened os opened; B.blastOutString os src)
	    | UDEC(U,PRIMU(I,_),pos) =>
		(B.blastOutInt os 6; Pos.blastOutPos os pos; blastOutLabel os U;
		 blastOutLabel os I)
	    | UDEC(U,PRECOMPU(_,opened,I,{src,...}),pos) =>
		(B.blastOutInt os 7; Pos.blastOutPos os pos;
		 blastOutLabel os U; blastOutLabel os I;
		 blastOutOpened os opened; B.blastOutString os src)
	    | UDEC(U,COMPU(_,I,_),pos) =>
		(B.blastOutInt os 8; Pos.blastOutPos os pos; blastOutLabel os U;
		 blastOutLabel os I)
	    | SCDEC(U,I,pos) =>
		(B.blastOutInt os 9; blastOutLabel os U; blastOutLabel os I;
		 Pos.blastOutPos os pos))

    fun blastInPdec (is:B.instream) : pdec =
	(case B.blastInInt is
	   of 0 =>
		let val pos = Pos.blastInPos is
		    val I = blastInLabel is
		    val opened = blastInOpened is
		    val src = B.blastInString is
		    val iexp = C.I.src (Pos.file pos,I,opened,src)
		in  IDEC (I,iexp,pos)
		end
	    | 1 =>
		let val pos = Pos.blastInPos is
		    val I = blastInLabel is
		    val iexp = C.I.prim (Pos.file pos,I)
		in  IDEC (I,iexp,pos)
		end
	    | 2 =>
		let val pos = Pos.blastInPos is
		    val I = blastInLabel is
		    val opened = blastInOpened is
		    val src = B.blastInString is
		    val iexp = C.I.precomp (Pos.file pos,I,opened,src)
		in  IDEC (I,iexp,pos)
		end
	    | 3 =>
		let val pos = Pos.blastInPos is
		    val I = blastInLabel is
		    val iexp = C.I.comp (Pos.file pos,I)
		in  IDEC (I,iexp,pos)
		end
	    | 4 =>
		let val pos = Pos.blastInPos is
		    val U = blastInLabel is
		    val opened = blastInOpened is
		    val src = B.blastInString is
		    val uexp = C.U.src (Pos.file pos,U,opened,src)
		in  UDEC (U,uexp,pos)
		end
	    | 5 =>
		let val pos = Pos.blastInPos is
		    val U = blastInLabel is
		    val I = blastInLabel is
		    val opened = blastInOpened is
		    val src = B.blastInString is
		    val uexp = C.U.ssrc (Pos.file pos,U,I,opened,src)
		in  UDEC (U,uexp,pos)
		end
	    | 6 =>
		let val pos = Pos.blastInPos is
		    val U = blastInLabel is
		    val I = blastInLabel is
		    val uexp = C.U.prim (Pos.file pos,U,I)
		in  UDEC (U,uexp,pos)
		end
	    | 7 =>
		let val pos = Pos.blastInPos is
		    val U = blastInLabel is
		    val I = blastInLabel is
		    val opened = blastInOpened is
		    val src = B.blastInString is
		    val uexp = C.U.precomp (Pos.file pos,U,I,opened,src)
		in  UDEC (U,uexp,pos)
		end
	    | 8 =>
		let val pos = Pos.blastInPos is
		    val U = blastInLabel is
		    val I = blastInLabel is
		    val uexp = C.U.comp (Pos.file pos,U,I)
		in  UDEC (U,uexp,pos)
		end
	    | 9 => SCDEC (blastInLabel is, blastInLabel is,
			  Pos.blastInPos is)
	    | _ => error "bad pdec")

    val blastOutDesc : B.outstream -> desc -> unit =
	B.blastOutList blastOutPdec

    val blastInDesc : B.instream -> desc =
	B.blastInList blastInPdec

    val (blastOutDesc, blastInDesc) =
	B.magic (blastOutDesc, blastInDesc, "desc $Revision$")

    fun blastOutEntry (os : B.outstream) (e : label * Crc.crc) : unit =
	(say "blastOutEntry";
	 B.blastOutPair NB.blastOutLabel Crc.blastOutCrc os e)
    fun blastInEntry (is : B.instream) : label * Crc.crc =
	(say "blastInEntry";
	 B.blastInPair NB.blastInLabel Crc.blastInCrc is)

    fun blastOutUe (os : B.outstream) (ue : ue) : unit =
	(say "blastOutUe"; B.blastOutList blastOutEntry os ue)
    fun blastInUe (is : B.instream) : ue =
	(say "blastInUe"; B.blastInList blastInEntry is)

    fun blastOutInfo (os : B.outstream) (info : info) : unit =
	(say "blastOutInfo";
	 (case info
	    of INFO_SRCI (ue, opened, src) =>
		(B.blastOutInt os 0; blastOutUe os ue;
		 blastOutOpened os opened; Crc.blastOutCrc os src)
	     | INFO_SRCU (ue, opened, src) =>
		(B.blastOutInt os 1; blastOutUe os ue;
		 blastOutOpened os opened; Crc.blastOutCrc os src)
	     | INFO_SSRCU (ue, opened, src, pi) =>
		(B.blastOutInt os 2; blastOutUe os ue;
		 blastOutOpened os opened; Crc.blastOutCrc os src;
		 Crc.blastOutCrc os pi)
	     | INFO_PRIMU pi =>
		(B.blastOutInt os 3; Crc.blastOutCrc os pi)))

    fun blastInInfo (is : B.instream) : info =
	(say "blastInInfo";
	 (case B.blastInInt is
	    of 0 => INFO_SRCI (blastInUe is, blastInOpened is,
			       Crc.blastInCrc is)
	     | 1 => INFO_SRCU (blastInUe is, blastInOpened is,
			       Crc.blastInCrc is)
	     | 2 => INFO_SSRCU (blastInUe is, blastInOpened is,
				Crc.blastInCrc is, Crc.blastInCrc is)
	     | 3 => INFO_PRIMU (Crc.blastInCrc is)
	     | _ => error "bad info"))

    val (blastOutInfo, blastInInfo) =
	B.magic (blastOutInfo, blastInInfo, "info $Revision$")

    type format = Fmt.format
    val String = Fmt.String
    val Break = Fmt.Break
    val Eq = String " = "
    val Has = String " : "
    val Box : format list -> format = Fmt.HOVbox

    val pp_label = Ppil.pp_label'
    val pp_pos = Pos.pp_pos

    fun pp_parm (using:using) : format =
	Fmt.pp_list pp_label using

    fun pp_opened (opened:opened) : format =
	Fmt.pp_list pp_label opened

    fun pp_iexp (iexp : iexp) : format =
	(case iexp
	   of SRCI (opened,{src,info,pinterface}) =>
		Box[String "SRCI", Break,
		    String "opened = ", pp_opened opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "pinterface = ", String pinterface]
	    | PRIMI {pinterface} =>
	    	Box[String "PRIMI", Break,
		    String "pinterface = ", String pinterface]
	    | PRECOMPI (using,opened,{pinterface,src,info}) =>
		Box[String "PRECOMPI", Break,
		    String "using = ", pp_parm using, Break,
		    String "opened = ", pp_opened opened, Break,
		    String "pinterface = ", String pinterface, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info]
	    | COMPI (using,{pinterface}) =>
		Box[String "COMPI", Break,
		    String "using = ", pp_parm using, Break,
		    String "pinterface = ", String pinterface])

    fun pp_uexp (uexp : uexp) : format =
	(case uexp
	   of SRCU (opened,{src,info,pinterface,obj,asm,asmz,parm}) =>
		Box[String "SRCU", Break,
		    String "opened = ", pp_opened opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "pinterface = ", String pinterface, Break,
		    String "obj = ", String obj, Break,
		    String "asm = ", String asm, Break,
		    String "asmz = ", String asmz, Break,
		    String "parm = ", String parm]
	    | SSRCU (opened,I,{src,info,obj,asm,asmz,parm}) =>
		Box[String "SSRCU", Has, pp_label I, Break,
		    String "opened = ", pp_opened opened, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "obj = ", String obj, Break,
		    String "asm = ", String asm, Break,
		    String "asmz = ", String asmz, Break,
		    String "parm = ", String parm]
	    | PRIMU (I,{info,obj,asm,asmz,parm}) =>
		Box[String "PRIMU", Has, pp_label I, Break,
		    String "info = ", String info, Break,
		    String "obj = ", String obj, Break,
		    String "asm = ", String asm, Break,
		    String "asmz = ", String asmz, Break,
		    String "parm = ", String parm]
	    | PRECOMPU (using,opened,I,{obj,src,info,parm}) =>
		Box[String "PRECOMPU", Has, pp_label I, Break,
		    String "using = ", pp_parm using, Break,
		    String "opened = ", pp_opened opened, Break,
		    String "obj = ", String obj, Break,
		    String "src = ", String src, Break,
		    String "info = ", String info, Break,
		    String "parm = ", String parm]
	    | COMPU (using,I,{obj,parm}) =>
		Box[String "COMPU", Has, pp_label I, Break,
		    String "using = ", pp_parm using, Break,
		    String "obj = ", String obj, Break,
		    String "parm = ", String parm])

    fun pp_pdec (pdec:pdec) : format =
	(case pdec
	   of IDEC (I,iexp,pos) =>
		Box[pp_label I, Eq, pp_iexp iexp, Break,
		    String "pos = ", pp_pos pos]
	    | SCDEC (U,I,pos) =>
		Box[pp_label U, Has, pp_label I, Break,
		    String "pos = ", pp_pos pos]
	    | UDEC (U,uexp,pos) =>
		Box[pp_label U, Eq, pp_uexp uexp, Break,
		    String "pos = ", pp_pos pos])

    fun pp_desc (desc:desc) : format =
	let val formats = map pp_pdec desc
	    val formats = Listops.join Break formats
	in  Fmt.Vbox formats
	end

    fun pp_entry (name : label, crc : Crc.crc) : format =
	Fmt.Hbox [String (Name.label2string name), Has, Crc.pp_crc crc]

    fun pp_ue (ue : ue) : format = Fmt.pp_list pp_entry ue

    val pp_opened : opened -> format =
	Fmt.pp_list' Fmt.Hbox (String o Name.label2string)

    fun pp_info (info : info) : format =
	(case info
	   of INFO_SRCI (ue,opened,src) =>
		Box[String "INFO_SRCI", Break,
		    String "ue = ", pp_ue ue, Break,
		    String "opened = ", pp_opened opened, Break,
		    String "source = ", Crc.pp_crc src]
	    | INFO_SRCU (ue,opened,src) =>
		Box[String "INFO_SRCU", Break,
		    String "ue = ", pp_ue ue, Break,
		    String "opened = ", pp_opened opened, Break,
		    String "source = ", Crc.pp_crc src]
	    | INFO_SSRCU (ue, opened, src, pi) =>
		Box[String "INFO_SSRCU", Break,
		    String "ue = ", pp_ue ue, Break,
		    String "opened = ", pp_opened opened, Break,
		    String "source = ", Crc.pp_crc src, Break,
		    String "pinterface = ", Crc.pp_crc pi]
	    | INFO_PRIMU pi =>
		Box[String "INFO_PRIMU", Break,
		    String "pinterface = ", Crc.pp_crc pi])

end
