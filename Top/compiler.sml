(* Interface to the compiler. *)
(*
	NB we reset the label counter to make the elaborator more
	deterministic.  Fresh labels must be chosen to account for SML
	shadowing and these sometimes make it into compilation unit
	interfaces.  We want to choose the same labels each time to
	ensure that if a unit's source code and evaluation context
	does not change, then the compiler will infer the same
	interface every time it elaborates the unit.  We also reset
	the label counter prior to generating the link unit so that
	diffing that assembler is easier.
*)
structure Compiler :> COMPILER =
struct

    structure I = IntSyn
    structure E = ExtSyn
    structure P = LinkParse
    structure Il = LinkIl
    structure Nil = Linknil
    structure Sparc = Linksparc
    structure Alpha = Linkalpha
    structure F = Formatter

    type label = Name.label
    type desc = IntSyn.desc
    type pdec = IntSyn.pdec
    type file = IntSyn.file

    structure Map = Name.LabelMap
    structure Set = Name.LabelSet

    val timestamp = Timestamp.timestamp

    val error = fn s => Util.error "compiler.sml" s
    val reject = Util.reject

    exception Fail of string	(* local *)
    fun fail (msg:string) : 'a =
	raise (Fail msg)

    val CompilerDebug = Stats.ff "CompilerDebug"
    fun debugdo (f : unit -> 'a) : unit =
	if (!CompilerDebug) then (f(); ()) else ()

    val CompilerDiag = Stats.ff "CompilerDiag"
    fun msg str = if (!CompilerDiag) then print str else ()

    val CompilerVerbose = Stats.ff "CompilerVerbose"
    fun verbose str = if (!CompilerVerbose) then print str else ()

    val UptoElaborate = Stats.ff "UptoElaborate"
    val UptoAsm = Stats.ff "UptoAsm"
    val KeepAsm = Stats.tt "KeepAsm"
    val CompressAsm = Stats.ff "CompressAsm"
    val PackUnitSource = Stats.ff "PackUnitSource"

    fun lookup (desc:desc) : label -> I.pdec =
	let fun add (pdec:pdec, map:I.pdec Map.map) : I.pdec Map.map =
		let val l = I.P.label pdec
		in  Map.insert(map,l,pdec)
		end
	    val map = foldl add Map.empty desc
	    fun look (l:label) : I.pdec =
		(case Map.find (map,l)
		   of SOME pdec => pdec
		    | NONE => error (Name.label2longname l ^ " not in desc"))
	in  look
	end

    fun lookup' (desc:desc, l:label) : I.pdec =
	(case desc
	   of pdec :: desc' =>
		if Name.eq_label(l,I.P.label pdec) then pdec
		else lookup' (desc', l)
	    | nil => error (Name.label2longname l ^ " not in desc"))

    fun pinterface (pdec:I.pdec) : file =
	let val (_,iexp,_) = I.D.Pdec.idec pdec
	    val pinterface = I.P.I.pinterface iexp
	in  pinterface
	end

    fun reachable (desc:desc, roots:Set.set) : Set.set =
	let fun add_pdec (pdec:I.pdec, map:Set.set Map.map) : Set.set Map.map =
		Map.insert(map, I.P.label pdec, I.free_pdec pdec)
	    val freemap = foldl add_pdec Map.empty desc
	    fun reachable (black:Set.set, gray:Set.set) : Set.set =
		if Set.isEmpty gray then black
		else
		    let val black = Set.union(black,gray)
			fun folder (l:label, s:Set.set) : Set.set =
			    (case Map.find(freemap,l)
			       of SOME s' => Set.union(s,s')
				| NONE =>
				    error ("reachable: " ^
					   Name.label2longname l ^
					   " not in desc"))
			val free = Set.foldl folder Set.empty gray
			val gray = Set.difference(free,black)
		    in	reachable(black,gray)
		    end
	in  reachable(Set.empty,roots)
	end

    fun gc_desc' (desc:desc, roots:label list) : desc =
	let val keep = reachable(desc,Set.addList(Set.empty,roots))
	    fun keep_pdec (pdec:I.pdec) : bool =
		Set.member(keep, I.P.label pdec)
	    val desc' = List.filter keep_pdec desc
	in  desc'
	end

    fun gc_desc (desc:desc, roots:label list) : desc =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val l = F.pp_list Ppil.pp_label' roots
		    val fmt = F.HOVbox [F.String"Compiler.gc_desc", F.Break,
					F.String"desc = ", d, F.Break,
					F.String"roots = ", l, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc desc
		in  ()
		end
	    val _ = debugdo report
	in  gc_desc' (desc,roots)
	end

    (*
	A project description has more information than necessary to
	compile a pdec (as opposed to, say, packing a library).  To
	reduce compilation overhead, get_inputs computes a skeletal
	description (skel) which is a syntactic subcategory of desc:

	skel ::= * | skel, I = pinterface USING units | skel, U : I

	A skel is obtained from a desc for a pdec by:

	1.  Converting every interface definition to the form I =
	(pinterface USING units); these contain all the information
	needed for elaboration and no spurious dependencies.

	2.  Converting every unit definition to the form U : I; this
	requires making up interface definitions to name any inferred
	interfaces.

	3.  Deleting units and interfaces that are not relevant to
	pdec.

	For example, consider the project description

		interface I1 = "I1.int" {}
		unit A : I1 = "A.sml" {}
		interface I2 = "I2.sml" {}
		unit B : I2 = "B.sml" {}
		unit C = "C.sml" {A}
		unit D = "D.sml" {C}

	and suppose that everything but D is up to date.  A skeletal
	project description for compiling D would look like:

		interface I1 = "I1.pinterface" USING []
		unit A : I1
		interface FRESH = "C.pinterface" USING [A]
		unit C : FRESH

	NB a library's interface project description can not be
	skeletal.  Each project description for a library must define
	the same set of units to avoid the confusing scenario where
	"tilt desc" works but "tilt -o exe desc" fails due to a
	conflict between a unit U defined by desc and also a library's
	implementation but not the library's interface.
    *)

    fun compi (I:label, using:I.using, pinterface:file, pos:Pos.pos) : I.pdec =
	let val files : I.F.I.comp = {pinterface=pinterface}
	    val iexp = I.COMPI (using,files)
	    val pdec = I.IDEC (I,iexp,pos)
	in  pdec
	end

    fun rewritei (pdec:I.pdec, revdesc:I.pdec list) : I.pdec list =
	let val (I,iexp,pos) = I.D.Pdec.idec pdec
	    fun rewrite (using, pinterface) =
		compi (I,using,pinterface,pos) :: revdesc
	in  (case iexp
	       of I.SRCI (_,{pinterface,...}) =>
		    let val using = Fs.read_pinterface_parm pinterface
		    in	rewrite(using,pinterface)
		    end
		| I.PRIMI {pinterface,...} => rewrite(nil,pinterface)
		| I.PRECOMPI (using,_,{pinterface,...}) =>
		    rewrite(using,pinterface)
		| I.COMPI _ => pdec::revdesc)
	end

    fun rewriteu (pdec:I.pdec, revdesc:I.pdec list) : I.pdec list =
	let val (U,uexp,pos) = I.D.Pdec.udec pdec
	    fun simple I = I.SCDEC (U,I,pos) :: revdesc
	in  (case uexp
	       of I.SRCU (_,{pinterface,...}) =>
		    let val I = Name.fresh_internal_label "inferred"
			val using = Fs.read_pinterface_parm pinterface
			val idec = compi (I,using,pinterface,pos)
			val udec = I.SCDEC(U,I,pos)
		    in  udec :: idec :: revdesc
		    end
		| I.SSRCU (_,I,_) => simple I
		| I.PRIMU (I,_) => simple I
		| I.PRECOMPU (_,_,I,_) => simple I
		| I.COMPU (_,I,_) => simple I)
	end

    fun skel_desc (desc:desc, keep:label) : desc =
	let fun folder (pdec:I.pdec, revdesc:I.pdec list) : I.pdec list =
		let val l = I.P.label pdec
		in  if Name.eq_label(l,keep) then pdec::revdesc
		    else
			(case pdec
			   of I.IDEC _ => rewritei (pdec,revdesc)
			    | I.SCDEC _ => pdec :: revdesc
			    | I.UDEC _ => rewriteu (pdec,revdesc))
		end
	    val desc = foldr folder nil desc
	in  desc
	end

    fun get_inputs (desc:I.desc, l:label) : I.desc * I.pdec =
	let (*
		GC according to project description.  Everything
		but the last pdec (defining l) is up to date.
	    *)
	    val desc = gc_desc' (desc, [l])
	    (*
		GC according to compiled interface dependencies.
	    *)
	    val desc = skel_desc (desc,l)
	    val desc = gc_desc' (desc, [l])
	in
	    (case rev desc
	       of nil => error "empty description"
		| pdec::revdesc => (rev revdesc, pdec))
	end

    (*
	If desc |- pdec ready, then (context desc) is a well-formed
	elaboration context that can be used to compile a pdec.
    *)
    fun precontext' (desc:desc) : (label * file) list =
	let val look : label -> file = pinterface o (lookup desc)
	    fun mapper (pdec:I.pdec) : (label * file) option =
		(case pdec
		   of I.SCDEC (U,I,_) => SOME (U,look I)
		    | I.UDEC(U,uexp,_) =>
			(case uexp
			   of I.SRCU (_,{pinterface,...}) =>
				SOME (U,pinterface)
			    | _ =>
				let val I = I.P.U.asc uexp
				in  SOME (U,look I)
				end)
		    | I.IDEC _ => NONE)
	    val precontext' = List.mapPartial mapper desc
	in  precontext'
	end

    fun precontext (desc:desc) : Il.precontext =
	let val precontext' = precontext' desc
	    fun mapper (U,file) = (U,Fs.read_pinterface file)
	    val precontext = map mapper precontext'
	in  precontext
	end

    fun info' (desc:desc, pdec:pdec) : I.info =
	let val crc : string -> Crc.crc = Fs.crc
	    val look : label -> file = pinterface o (lookup desc)
	    val precontext' = precontext' desc
	    fun mapper (U,file) = (U,crc file)
	    val ue : I.ue = map mapper precontext'
	    val info =
		(case pdec
		   of I.IDEC(I,iexp,_) =>
			(case iexp
			   of I.SRCI (opened,{src,...}) =>
				I.INFO_SRCI (ue,opened,crc src)
			    | I.PRECOMPI (_,opened,{src,...}) =>
				I.INFO_SRCI (ue,opened,crc src)
			    | _ => error "info on non-source interface")
		    | I.SCDEC _ => error "info on separately compiled unit"
		    | I.UDEC (U,uexp,_) =>
			(case uexp
			   of I.SRCU (opened,{src,...}) =>
				I.INFO_SRCU (ue,opened,crc src)
			    | I.SSRCU (opened,I,{src,...}) =>
				I.INFO_SSRCU (ue,opened,crc src,crc (look I))
			    | I.PRIMU (I,_) =>
				I.INFO_PRIMU (crc (look I))
			    | I.PRECOMPU (_,opened,I,{src,...}) =>
				I.INFO_SSRCU (ue,opened,crc src, crc (look I))
			    | _ => error "info on non-source unit"))
	in  info
	end

    fun info (desc:desc, pdec:pdec) : I.info =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val p = I.pp_pdec pdec
		    val fmt = F.HOVbox [F.String"Compiler.info", F.Break,
					F.String"desc = ", d, F.Break,
					F.String"pdec = ", p, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc (desc @ [pdec])
		in  ()
		end
	    val _ = debugdo report
	in  info' (desc,pdec)
	end

    fun wrap_parse (f:'a P.parser) (l:label,src:file) : P.filepos * 'a =
	(case f(Name.label2longname l, src)
	   of SOME r => r
	    | NONE => fail ("could not parse " ^ src))

    val parse_topspec : label * file -> P.filepos * Ast.topspec =
	wrap_parse P.parse_topspec

    val parse_topdec : label * file -> P.filepos * Ast.dec =
	wrap_parse P.parse_topdec

    fun elab_opt (src:file, x:'a option) : 'a =
	(case x
	   of SOME r => r
	    | NONE => fail ("could not elaborate " ^ src))

    fun write_pinterface (pctx:Il.precontext, pinterface:file,
			  pi:Il.pinterface) : unit =
	let val unchanged =
		Fs.exists pinterface andalso
		(case Fs.read_pinterface' pinterface
		   of NONE => false
		    | SOME pi' => LinkIl.eq (pctx, pi, pi'))
	in  if unchanged then ()
	    else
		(verbose "  Writing pinterface\n";
		 Fs.write_pinterface (pinterface,pi))
	end

    fun elaborating (l:label) : unit =
	verbose (concat["===== Elaborating ", Name.label2longname l," =====\n"])

    datatype module = RTL of Rtl.module | LIL of Lil.module

    (*
	RTL parameters are more specific than unit names.
    *)
    fun unitnames (desc : I.desc, parms : Set.set) : I.using =
	let
	    fun unit (U:label) : label option =
		let val (c,r) = Name.make_cr_labels U
		in  if (Set.member(parms,U) orelse
			Set.member(parms,c) orelse
			Set.member(parms,r))
		    then SOME U
		    else NONE
		end
	    fun mapper (pdec:I.pdec) : label option =
		(case pdec
		   of I.IDEC _ => NONE
		    | I.SCDEC (U,_,_) => unit U
		    | I.UDEC (U,_,_) => unit U)
	in  List.mapPartial mapper desc
	end

    fun generate (desc:I.desc, U:label, asm:file, parm:file,
		  ilmod:Il.module) : unit =
	let val name = Name.label2name' U
	    val nilmod = Nil.il_to_nil (name,ilmod)
	    val _ = timestamp()
	    val tomod =
		(case Target.getTarget()
		   of Platform.ALPHA => RTL o Linkrtl.nil_to_rtl
		    | Platform.SPARC => RTL o Linkrtl.nil_to_rtl
		    | Platform.TALx86 => LIL o Linklil.nil_to_lil)
	    val module = tomod(name,nilmod)
	    val using =
		(case module
		   of RTL (Rtl.MODULE {parms,...}) => parms
		    | LIL _ => Set.empty)	(* XXX *)
	    val using = unitnames (desc,using)
	    val _ = Fs.write I.blastOutParm parm using
	    fun rtlwrap f (s,module) = (case module of RTL rmod => f (s,rmod)  | _ => error "Can't translate LIL to sparc/alpha")
	    fun lilwrap f (s,module) = (case module of LIL rmod => f (s,rmod)  | _ => error "Can't translate RTL to TAL")
	    val toasm =
		(case Target.getTarget()
		   of Platform.ALPHA => rtlwrap Linkalpha.rtl_to_asm
		    | Platform.SPARC => rtlwrap Linksparc.rtl_to_asm
		    | Platform.TALx86 => lilwrap LinkTAL.lil_to_asm)
	    val _ = Fs.write' (fn tmp => toasm(tmp,module)) asm
	in  ()
	end

    (*
	Convert an up to date asm or asmz file to asm, asmz and obj
	files according to flags, returing false if we are not native
	and assembly is required.
     *)
    fun assemble'' (asm:file, asmz:file, obj:file) : bool =
	let val haveasm = Fs.exists asm
	    val haveasmz = Fs.exists asmz
	    val wantasm = !KeepAsm andalso not (!CompressAsm)
	    val wantasmz = !KeepAsm andalso !CompressAsm
	    val wantobj = not (!UptoAsm)
	    val native = Target.native()
	    val uncompress = (wantobj orelse wantasm) andalso (not haveasm)
	    val assemble = wantobj andalso native
	    val finished = (not wantobj orelse native)
	    val compress = wantasmz andalso (not haveasmz)
	    val removeasm = (not wantasm) andalso finished
	    val removeasmz = not wantasmz andalso haveasmz
	    val _ = Timestamp.timestamp()
	in
	    if uncompress then Tools.uncompress{src=asmz, dest=asm} else ();
	    if assemble then Tools.assemble (asm,obj) else ();
	    if compress then Tools.compress{src=asm, dest=asmz} else ();
	    if removeasm then Fs.remove asm else ();
	    if removeasmz then Fs.remove asmz else ();
	    finished
	end

    fun finish_compile (desc:I.desc, U:label, ilmod:Il.module, asm:file,
			asmz:file, obj:file, parm:file) : bool =
	let val elab_only =
		!UptoElaborate orelse
		(!UptoAsm andalso not (!KeepAsm))
	in  elab_only orelse
	    (generate(desc,U,asm,parm,ilmod); assemble''(asm,asmz,obj))
	end

    fun compile_srci (desc:I.desc, pdec:I.pdec) : bool =
	let val (I,iexp,_) = I.D.Pdec.idec pdec
	    val (opened,files) = I.D.I.src iexp
	    val {src,pinterface,...} = files
	    val (fp,topspec) = parse_topspec (I,src)
	    val _ = elaborating I
	    val pctx = precontext desc
	    val opt = Il.elab_topspec (pctx,opened,fp,topspec)
	    val pi = elab_opt (src,opt)
	    val _ = write_pinterface (pctx,pinterface,pi)
	in  true
	end

    fun compile_primi (_:I.desc, pdec:I.pdec) : bool =
	let val (I,iexp,_) = I.D.Pdec.idec pdec
	    val {pinterface,...} = I.D.I.prim iexp
	    val _ = elaborating I
	    val pi = Il.elab_primspec()
	    val _ = write_pinterface (nil,pinterface,pi)
	in  true
	end

    fun compile_srcu (desc:I.desc, pdec:I.pdec,
		      ack_inter:unit -> unit) : bool =
	let val (U,uexp,_) = I.D.Pdec.udec pdec
	    val (opened,files) = I.D.U.src uexp
	    val {pinterface,src,asm,parm,asmz,obj,...} = files
	    val (fp,topdec) = parse_topdec (U,src)
	    val _ = elaborating U
	    val pctx = precontext desc
	    val opt = Il.elab_topdec (pctx,U,opened,fp,topdec)
	    val (ilmod,pi) = elab_opt (src,opt)
	    val _ = write_pinterface (pctx,pinterface,pi)
	    val _ = ack_inter()
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,parm)
	end

    fun compile_ssrcu (desc:I.desc, pdec:I.pdec) : bool =
	let val (U,uexp,_) = I.D.Pdec.udec pdec
	    val (opened,I,files) = I.D.U.ssrc uexp
	    val {src,asm,asmz,obj,parm,...} = files
	    val (fp,topdec) = parse_topdec (U,src)
	    val _ = elaborating U
	    val pctx = precontext desc
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val opt = Il.elab_sealed_topdec (pctx,U,opened,fp,topdec,pi)
	    val ilmod = elab_opt (src,opt)
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,parm)
	end

    fun compile_primu (desc:I.desc, pdec:I.pdec) : bool =
	let val (U,uexp,_) = I.D.Pdec.udec pdec
	    val (I,files) = I.D.U.prim uexp
	    val {asm,asmz,obj,parm,...} = files
	    val _ = elaborating U
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val opt = Il.elab_primdec (U,pi)
	    val ilmod = elab_opt ("primitive unit",opt)
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,parm)
	end

    fun compile' (desc:I.desc, pdec:I.pdec, f:unit -> unit) : bool =
	let val _ = timestamp()
	    val l = I.P.label pdec
	    val _ = msg("[compiling " ^ Name.label2longname l ^ "]\n")
	    val _ = Name.reset_label_counter()
	    val _ = Name.reset_varmap()
	    fun primwrap f x =
		(Il.compiling_tiltprim := true;
		 f x before (Il.compiling_tiltprim := false)
		 handle e => (Il.compiling_tiltprim := false; raise e))
	    val finished =
		(case pdec
		   of I.IDEC(_,I.SRCI _,_) => compile_srci (desc,pdec)
		    | I.IDEC(_,I.PRIMI _,_) => primwrap compile_primi (desc,pdec)
		    | I.UDEC(_,I.SRCU _,_) => compile_srcu (desc,pdec,f)
		    | I.UDEC(_,I.SSRCU _,_) => compile_ssrcu (desc,pdec)
		    | I.UDEC(_,I.PRIMU _,_) => primwrap compile_primu (desc,pdec)
		    | _ => error "compile can't work with pdec")
	    val _ =
		(case I.P.info' pdec
		   of SOME file =>
			let val info = info' (desc,pdec)
			in  Fs.write I.blastOutInfo file info
			end
		    | NONE => ())
	in  finished
	end handle Fail msg =>
		let val pos = I.P.pos pdec
		    val msg = concat[Pos.tostring pos, ": ", msg]
		in  reject msg
		end

    fun compile (desc:I.desc, pdec:I.pdec, f:unit -> unit) : bool =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val p = I.pp_pdec pdec
		    val fmt = F.HOVbox [F.String"Compiler.compile", F.Break,
					F.String"desc = ", d, F.Break,
					F.String"pdec = ", p, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc (desc @ [pdec])
		in  ()
		end
	    val _ = debugdo report
	in  compile' (desc,pdec,f)
	end

    fun assemble' (pdec:I.pdec) : unit =
	let val (U,uexp,_) = I.D.Pdec.udec pdec
	    val asm = I.P.U.asm uexp
	    val asmz = I.P.U.asmz uexp
	    val obj = I.P.U.obj uexp
	in  if assemble'' (asm,asmz,obj) then ()
	    else error "unable to assemble (not native)"
	end

    fun assemble (pdec:I.pdec) : unit =
	let fun report () : unit =
		let val p = I.pp_pdec pdec
		    val fmt = F.HOVbox [F.String"Compiler.assemble", F.Break,
					F.String"pdec = ", p, F.Newline()]
		    val _ = F.print_fmt fmt
		in  ()
		end
	    val _ = debugdo report
	in  assemble' pdec
	end

    fun link' (desc:I.desc, files:I.F.link) : unit =
	let val {exe,asm,asmz,obj} = files
	    val _ = msg ("[linking " ^ exe ^ "]\n")
	    fun unit_info (pdec:I.pdec) : (string * file) option =
		(case pdec
		   of I.IDEC _ => NONE
		    | I.SCDEC _ => error "link saw unimplementd unit"
		    | I.UDEC (U,uexp,_) =>
			let val name = Name.label2name' U
			    val obj = I.P.U.obj uexp
			in  SOME (name,obj)
			end)
	    val (units,objs) = Listops.unzip(List.mapPartial unit_info desc)
	    val _ = timestamp()
	    val generate =
		(case Target.getTarget()
		   of Platform.ALPHA => Linkalpha.link
		    | Platform.SPARC => Linksparc.link
		    | Platform.TALx86 => error "no TAL linker yet")
	    fun writer tmp = generate {asmFile=tmp, units=units}
	    val _ = Fs.write' writer asm
	in  if assemble'' (asm,asmz,obj) then
		Tools.link (objs @ [obj], exe)
	    else ()
	end

    fun link (desc:I.desc, files:I.F.link) : unit =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val fmt = F.HOVbox [F.String"Compiler.link", F.Break,
					F.String"desc = ", d, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc desc
		in  ()
		end
	    val _ = debugdo report
	in  link' (desc,files)
	end

    (*
	Packing libraries.  The three project description files
	constructed for a library define exactly the same unit and
	interface names; the differ only in whether or not unit
	implementations are available.
    *)

    type target =
	{libdir:file,
	 desc:file,
	 src:label -> file,
	 copyshared:bool}

    val op/ =
	fn (dir,file) => OS.Path.joinDirFile {dir=dir, file=file}

    fun libsrc (target:target, l:label) : file * file =
	let val relative = #src target l
	    val dir = #libdir target
	    val absolute = dir/relative
	in  (relative,absolute)
	end

    fun copy_precompi (target:target, I:label, opened:I.opened,
		       files:I.F.I.precomp) : E.ent =
	let val {desc,copyshared,...} = target
	    val (relsrc,abssrc) = libsrc (target,I)
	    val {src,pinterface,info} = files
	    val targetfiles = I.F.I.precomp (desc,I,abssrc)
	    val _ = if copyshared then Fs.copy (src,abssrc) else ()
	    val _ = Fs.copy (pinterface, #pinterface targetfiles)
	    val _ = Fs.copy (info, #info targetfiles)
	    val ent = E.INTERFACE(I, E.PRECOMPI (E.EXP_STR relsrc, opened))
	in  ent
	end

    fun copy_compi (target:target, I:label, files:I.F.I.comp) : E.ent =
	let val {desc,...} = target
	    val {pinterface} = files
	    val targetfiles = I.F.I.comp (desc,I)
	    val _ = Fs.copy (pinterface, #pinterface targetfiles)
	    val ent = E.INTERFACE(I,E.COMPI)
	in  ent
	end

    (*
	"extra" units are dependencies imposed by the user in the
	project description.  They may have effects.
    *)
    fun copy_compu (target:target, U:label, extra:label list,
		    I:label, files:I.F.U.comp) : E.ent =
	let val {desc,...} = target
	    val {obj,parm} = files
	    val targetfiles = I.F.U.comp (desc,U)
	    val _ = Fs.copy (obj, #obj targetfiles)
	    val parms = Fs.read I.blastInParm parm
	    val parms = Listops.list_sum_eq (Name.eq_label, parms, extra)
	    val _ = Fs.write I.blastOutParm (#parm targetfiles) parms
	    val ent = E.UNIT (U,E.COMPU I)
	in  ent
	end

    fun copy_precompu (target:target, U:label, opened:I.opened, I:label,
		       files:I.F.U.precomp) : E.ent =
	if !PackUnitSource then
	    let val {desc,...} = target
		val (relsrc,abssrc) = libsrc (target,U)
		val {src,obj,info,parm} = files
		val targetfiles = I.F.U.precomp (desc,U,abssrc)
		val _ = Fs.copy (src, abssrc)
		val _ = Fs.copy (obj, #obj targetfiles)
		val _ = Fs.copy (info, #info targetfiles)
		val _ = Fs.copy (parm, #parm targetfiles)
		val ent = E.UNIT (U,E.PRECOMPU (E.EXP_STR relsrc,opened,I))
	    in	ent
	    end
	else
	    let val {obj,parm,...} = files
		val files : I.F.U.comp = {obj=obj, parm=parm}
	    in	copy_compu (target,U,opened,I,files)
	    end


    fun copy_iexp (target:target, I:label, iexp:I.iexp) : E.ent =
	(case iexp
	   of I.SRCI (opened,files) => copy_precompi (target,I,opened,files)
	    | I.PRIMI files => copy_compi (target,I,files)
	    | I.PRECOMPI (_,opened,files) => 
		copy_precompi (target,I,opened,files)
	    | I.COMPI (_,files) => copy_compi (target,I,files))

    fun copy_uexp (target:target, U:label, uexp:I.uexp) : E.ent =
	(case uexp
	   of I.SRCU _ => error "can not pack inferred interfaces"
	    | I.SSRCU (opened,I,{obj,src,info,parm,...}) =>
		let val files = {obj=obj,src=src,info=info,parm=parm}
		in  copy_precompu (target,U,opened,I,files)
		end
	    | I.PRIMU (I,{obj,parm,...}) =>
		let val files = {obj=obj, parm=parm}
		in  copy_compu (target,U,nil,I,files)
		end
	    | I.PRECOMPU (_,opened,I,files) =>
		copy_precompu (target,U,opened,I,files)
	    | I.COMPU (_,I,files) => copy_compu (target,U,nil,I,files))

    fun copy_pdec (target:target, pdec:I.pdec) : E.ent =
	(case pdec
	   of I.IDEC (I,iexp,_) => copy_iexp (target,I,iexp)
	    | I.SCDEC (U,I,_) => E.SC (U,I)
	    | I.UDEC (U,uexp,_) => copy_uexp (target,U,uexp))

    fun copy_desc (target:target, desc:I.desc) : E.ents =
	map (fn pdec => copy_pdec (target,pdec)) desc

    fun write_ents (pack:I.F.pack, desc:file, ents:E.ents) : unit =
	let val {libdir, ...} = pack
	    val desc = libdir/desc
	    val os = TextIO.openOut desc
	    val _ = Project.print_ents (os,ents)
	    val _ = TextIO.closeOut os
	in  ()
	end

    fun interface (desc:I.desc) : I.desc =
	let fun mapper (pdec:I.pdec) : I.pdec =
		(case pdec
		   of I.IDEC _ => pdec
		    | I.SCDEC _ => pdec
		    | I.UDEC (U,uexp,pos) => I.SCDEC (U,I.P.U.asc uexp,pos))
	in  map mapper desc
	end

    fun pack_inter (desc:I.desc, pack:I.F.pack) : unit =
	let val {libdir,src,...} = pack
	    val target = {libdir=libdir,desc=libdir/I.F.inter,
			  src=src,copyshared=true}
	    val desc = interface desc
	    val ents = copy_desc (target, desc)
	    val _ = write_ents (pack,I.F.inter,ents)
	in  ()
	end

    fun pack_impl (desc:I.desc, pack:I.F.pack) : unit =
	let val {libdir,src,...} = pack
	    val target = {libdir=libdir,desc=libdir/I.F.impl,
			  src=src,copyshared=false}
	    val ents = copy_desc (target, desc)
	    val _ = write_ents (pack,I.F.impl,ents)
	in  ()
	end

    fun pack_fake_impl (pack:I.F.pack) : unit =
	let val ents = [E.INCLUDE (E.EXP_STR I.F.inter)]
	    val _ = write_ents (pack,I.F.impl,ents)
	in  ()
	end

    fun pack_desc (pack:I.F.pack) : unit =
	let val linking = Name.symbol_label(Symbol.varSymbol "linking")
	    val exp = E.EXP_IF (E.EXP_VAR linking,
				E.EXP_STR I.F.impl,
				E.EXP_STR I.F.inter)
	    val ents = [E.INCLUDE exp]
	    val _ = write_ents (pack,I.F.desc,ents)
	in  ()
	end

    fun pack' (desc:I.desc, libdir:file) : unit =
	let val _ = msg ("[packing " ^ libdir ^ "]\n")
	    val pack = I.F.pack libdir
	    val _ = pack_inter (desc,pack)
	    val _ =
		if !UptoElaborate orelse !UptoAsm
		then pack_fake_impl pack
		else pack_impl (desc,pack)
	    val _ = pack_desc pack
	in  ()
	end

    fun pack (desc:I.desc, libdir:file) : unit =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val fmt = F.HOVbox [F.String"Compiler.pack", F.Break,
					F.String"desc = ", d, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc desc
		in  ()
		end
	    val _ = debugdo report
	in  pack' (desc,libdir)
	end

end
