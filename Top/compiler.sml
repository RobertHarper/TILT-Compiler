(* Interface to the compiler. *)
(*
    NB prior to elaborating the primitive interface or a source unit
    with an inferred interface, we reset the label counter to make the
    elaborator more deterministic.  Fresh labels must be chosen to
    account for SML shadowing and these sometimes make it into
    compiled interfaces.  We want to choose the same labels each time
    to ensure that if the compilers inputs do not change, then the
    resulting pinterface files will not change.	 CRCs of these
    pinterface files are used in project description summaries.	 If
    these change between compiles, then the manager will perform
    unnecessary recompilation or incorrectly complain that a compiled
    unit or interface is incompatible with the rest of the project.

    We also reset the label counter prior to generating other units
    (including the link unit) so that diffing assembler is easier.
*)
(*
    XXX: The parameters of a compiled unit should be read from the Nil
    module rather than from the Rtl and Lil modules so that they are
    not potentially platform specific.
*)
structure Compiler :> COMPILER =
struct

    structure I = IntSyn
    structure E = ExtSyn

    type label = Name.label
    type desc = IntSyn.desc
    type pdec = IntSyn.pdec
    type inputs = IntSyn.inputs
    type file = IntSyn.file

    structure Map = Name.LabelMap
    structure Set = Name.LabelSet
    structure StringSet = Util.StringSet

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

    fun reset () : unit =
	(Name.reset_label_counter();
	 Name.reset_varmap())

    fun summarize (inputs:inputs) : I.S.summary =
	let fun sum_iexp (iexp:I.iexp) : I.S.D.iexp =
		(case iexp of
		    I.PRECOMPI {opened,src,...} =>
			I.S.D.PRECOMPI(opened,Fs.crc src)
		|   I.COMPI {pinterface,...} => I.S.D.COMPI(Fs.crc pinterface)
		|   _ => error "sum_iexp")

	    fun sum_pdec (pdec:I.pdec) : I.S.D.pdec =
		(case pdec of
		    I.IDEC {name=I,iexp,...} => I.S.D.IDEC(I,sum_iexp iexp)
		|   I.SCDEC {name=U,asc=I,...} => I.S.D.SCDEC(U,I)
		|   _ => error "sum_pdec")

	    val sum_desc : IntSyn.desc -> I.S.D.desc =
		map sum_pdec

	    fun sum_iexp (iexp:I.iexp) : I.S.P.iexp =
		(case iexp of
		    I.SRCI {opened,src,...} => I.S.P.SRCI(opened,Fs.crc src)
		|   I.PRECOMPI {opened,src,...} => I.S.P.SRCI(opened,Fs.crc src)
		|   _ => error "sum_iexp")

	    fun sum_uexp (uexp:I.uexp) : I.S.P.uexp =
		(case uexp of
		    I.SRCU {opened,src,...} => I.S.P.SRCU(opened,Fs.crc src)
		|   I.SSRCU {asc=I,opened,src,...} =>
			I.S.P.SSRCU(I,opened,Fs.crc src)
		|   I.PRECOMPU {asc=I,opened,src,...} =>
			I.S.P.SSRCU(I,opened,Fs.crc src)
		|   _ => error "sum_uexp")

	    fun sum_pdec (pdec:I.pdec) : I.S.P.pdec =
		(case pdec of
		    I.IDEC {name=I,iexp,...} => I.S.P.IDEC(I,sum_iexp iexp)
		|   I.SCDEC {name=U,asc=I,...} => I.S.P.SCDEC(U,I)
		|   I.UDEC {name=U,uexp,...} => I.S.P.UDEC(U,sum_uexp uexp))
	    val (desc,pdec) = inputs
	    val sdesc = sum_desc desc
	    val spdec = sum_pdec pdec
	in  (sdesc,spdec)
	end

    fun write_summary (inputs:inputs) : unit =
	let val (_,pdec) = inputs
	in  (case (I.P.D.info' pdec) of
		SOME info =>
		    let val summary = summarize inputs
		    in	Fs.write I.S.blastOutSummary info summary
		    end
	    |	NONE => ())
	end

    fun lookup (desc:desc) : label -> I.pdec =
	let fun add (pdec:pdec, map:I.pdec Map.map) : I.pdec Map.map =
		let val l = I.P.D.name pdec
		in  Map.insert(map,l,pdec)
		end
	    val map = foldl add Map.empty desc
	    fun look (l:label) : I.pdec =
		(case (Map.find (map,l)) of
		    SOME pdec => pdec
		|   NONE => error (Name.label2longname l ^ " not in desc"))
	in  look
	end

    fun lookup' (desc:desc, l:label) : I.pdec =
	(case desc of
	    pdec :: desc' =>
		if Name.eq_label(l,I.P.D.name pdec) then pdec
		else lookup' (desc', l)
	|   nil => error (Name.label2longname l ^ " not in desc"))

    fun reachable (desc:desc, roots:Set.set) : Set.set =
	let fun add_pdec (pdec:I.pdec, map:Set.set Map.map) : Set.set Map.map =
		Map.insert(map, I.P.D.name pdec, I.free_pdec pdec)
	    val freemap = foldl add_pdec Map.empty desc
	    fun reachable (black:Set.set, gray:Set.set) : Set.set =
		if Set.isEmpty gray then black
		else
		    let val black = Set.union(black,gray)
			fun folder (l:label, s:Set.set) : Set.set =
			    (case (Map.find(freemap,l)) of
				SOME s' => Set.union(s,s')
			    |	NONE =>
				    error ("reachable: " ^
					Name.label2longname l ^
					" not in desc"))
			val free = Set.foldl folder Set.empty gray
			val gray = Set.difference(free,black)
		    in	reachable(black,gray)
		    end
	in  reachable(Set.empty,roots)
	end

    fun gc_desc (desc:desc, roots:Set.set) : desc =
	let val keep = reachable(desc,roots)
	    fun keep_pdec (pdec:I.pdec) : bool =
		Set.member(keep, I.P.D.name pdec)
	    val desc' = List.filter keep_pdec desc
	in  desc'
	end

    fun support (desc:desc, pdec:pdec) : desc =
	let val roots = I.free_pdec pdec
	    val desc = gc_desc(desc,roots)
	in  desc
	end

    (*
	Get_inputs looks up the pdec to be compiled and computes a
	skeletal project description to support it.  A skeletal
	project description (skel) syntactic subcategory of desc:

	skel	::= *
	    |	skel, I = (pinterface USING units) FROM isrc
	    |	skel, I = pinterface USING units
	    |	skel, U : I

	A skel is obtained from a desc for a pdec by:

	1.  Deleting interfaces and units that are not relevant to
	pdec; by assumption, everything left is up to date.

	2.  Converting every interface definition to a pre-compiled
	interface (preferred) or a compiled interface.

	3.  Converting every unit definition to the form U : I; this
	requires making up interface definitions to name any inferred
	interfaces.

	NB a library's interface project description can not be
	skeletal.  Each project description for a library must define
	the same set of units to avoid the confusing scenario where
	"tilt desc" works but "tilt -o exe desc" fails due to a
	conflict between a unit U defined by desc and also a library's
	implementation but not the library's interface.
    *)

    fun skel_desc (desc:desc) : desc =
	let
	    fun compi (using:I.units, pinterface:file, pos:Pos.pos) : I.iexp =
		let val compi : I.compi =
			{pos=pos,pinterface=pinterface,using=using}
		    val iexp = I.COMPI compi
		in  iexp
		end

	    fun rewrite_iexp (iexp:I.iexp) : I.iexp option =
		(case iexp of
		    I.SRCI {pinterface,pos,opened,src,info,...} =>
			let val using = Fs.read_pinterface_parm pinterface
			    val precompi : I.precompi =
				{pos=pos, pinterface=pinterface, using=using,
				 opened=opened, src=src, info=info}
			    val iexp = I.PRECOMPI precompi
			in  SOME iexp
			end
		|   I.PRIMI {pinterface,pos,...} =>
			let val iexp = compi(nil,pinterface,pos)
			in  SOME iexp
			end
		|   I.PRECOMPI _=> NONE
		|   I.COMPI _ => NONE)

	    fun rewrite_idec (pdec:I.pdec, desc:I.pdec list) : I.pdec list =
		let val {name=I,iexp,...} = I.D.D.i pdec
		    val pdec =
			(case (rewrite_iexp iexp) of
			    SOME iexp => I.C.D.i(I,iexp)
			|   NONE => pdec)
		in  pdec :: desc
		end

	    fun rewrite_udec (pdec:I.pdec, desc:I.pdec list) : I.pdec list =
		let val {name=U,uexp,...} = I.D.D.u pdec
		    val pos = I.P.U.pos uexp
		in  (case (I.P.U.asc' uexp) of
			SOME I =>
			    let val pdec = I.C.D.sc(pos,U,I,false)
			    in	pdec :: desc
			    end
		    |	NONE =>
			    let val pinterface = I.P.U.pinterface uexp
				val I = Name.fresh_internal_label "inferred"
				val using = Fs.read_pinterface_parm pinterface
				val iexp = compi(using,pinterface,pos)
				val idec = I.C.D.i(I,iexp)
				val udec = I.C.D.sc(pos,U,I,false)
			    in	idec :: udec :: desc
			    end)
		end

	    fun folder (pdec:I.pdec, desc:I.pdec list) : I.pdec list =
		(case pdec of
		    I.IDEC _ => rewrite_idec(pdec,desc)
		|   I.SCDEC _ => pdec :: desc
		|   I.UDEC _ => rewrite_udec(pdec,desc))
	in  foldr folder nil desc
	end

    fun get_inputs (desc:I.desc, l:label) : inputs =
	let val pdec = lookup'(desc,l)
	    val desc = support(desc,pdec)
	    val desc = skel_desc desc
	    val desc = support(desc,pdec)
	in  (desc,pdec)
	end

    (*
	Get_inputs retains interface sources so that info files may
	summarize interfaces properly.	Simplify_inputs discards these
	sources (and any supporting units that are otherwise
	unnecessary) so that the elaboration context can be as small
	as possible.
    *)
    fun simplify_inputs (inputs:inputs) : inputs =
	let val (desc,pdec) = inputs
	    fun mapper (pdec:I.pdec) : pdec =
		(case pdec of
		    I.IDEC {name=I,iexp=I.PRECOMPI {pos,pinterface,using,...},...} =>
			let val compi : I.compi =
				{pos=pos, pinterface=pinterface, using=using}
			    val iexp = I.COMPI compi
			    val pdec = I.C.D.i(I,iexp)
			in  pdec
			end
		|   _ => pdec)
	    val desc = map mapper desc
	    val desc = support(desc,pdec)
	in  (desc,pdec)
	end

    val pinterface : I.pdec -> file =
	I.P.I.pinterface o I.P.D.iexp

    fun precontext' (desc:desc) : (label * file) list =
	let val look : label -> file = pinterface o (lookup desc)
	    fun mapper (pdec:I.pdec) : (label * file) option =
		(case pdec of
		    I.SCDEC {name=U,asc=I,...} => SOME (U,look I)
		|   I.UDEC {name=U,uexp,...} =>
			(case (I.P.U.asc' uexp) of
			    SOME I => SOME(U,look I)
			|   NONE => SOME(U,I.P.U.pinterface uexp))
		|   I.IDEC _ => NONE)
	    val precontext' = List.mapPartial mapper desc
	in  precontext'
	end

    fun precontext (desc:desc) : LinkIl.precontext =
	let val precontext' = precontext' desc
	    fun mapper (U,file) = (U,Fs.read_pinterface file)
	    val precontext = map mapper precontext'
	in  precontext
	end

    fun compile_tali (pctx:LinkIl.precontext, U:label, pi:LinkIl.pinterface, tali:file) : unit =
	let val _ = verbose(concat
		["===== Making interface for ",
		 Name.label2longname U, " =====\n"])
	    val name = Name.label2name' U
	    val _ = reset() (* not essential *)
	    val ilint = LinkIl.sc_module (pctx,U,pi)
	    val _ = timestamp()
	    val nilint = Linknil.ilint_to_nilint (name,ilint)
	    val _ = timestamp()
	    val lilint = Linklil.nilint_to_lilint (name,nilint)
	    val _ = Fs.write' (LinkTAL.to_tal_interface lilint) tali
	in  ()
	end

    fun compile_int (inputs:inputs) : unit =
	let val _ = timestamp()
	    val (desc,pdec) = simplify_inputs inputs
	    val U = I.P.D.name pdec
	    val _ = msg("[compiling TAL interface for " ^ Name.label2longname U ^ "]\n")
	    val I = I.P.D.asc pdec
	    val tali = I.P.D.tali pdec
	    val name = Name.label2name' U
	    val pctx = precontext desc
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val _ = compile_tali(pctx,U,pi,tali)
	    val needinfo =
		(case pdec of
		    I.SCDEC _ => true
		|   _ => false) (* will be written by compile *)
	    val _ = if needinfo then write_summary inputs else ()
	in  ()
	end handle Fail msg =>
	    let val (_,pdec) = inputs
		val pos = I.P.D.pos pdec
		val msg = concat[Pos.tostring pos, ": ", msg]
	    in	reject msg
	    end

    fun wrap_parse (f:'a LinkParse.parser) (l:label,src:file) : LinkParse.filepos * 'a =
	(case (f(Name.label2longname l, src)) of
	    SOME r => r
	|   NONE => fail ("could not parse " ^ src))

    val parse_topspec : label * file -> LinkParse.filepos * Ast.topspec =
	wrap_parse LinkParse.parse_topspec

    val parse_topdec : label * file -> LinkParse.filepos * Ast.dec =
	wrap_parse LinkParse.parse_topdec

    fun elab_opt (src:file, x:'a option) : 'a =
	(case x of
	    SOME r => r
	|   NONE => fail ("could not elaborate " ^ src))

    fun write_pinterface (pctx:LinkIl.precontext, pinterface:file, pi:LinkIl.pinterface) : bool =
	let val unchanged =
		Fs.exists pinterface andalso
		(case (Fs.read_pinterface' pinterface) of
		    NONE => false
		|   SOME pi' => LinkIl.eq (pctx, pi, pi'))
	    val _ =
		if unchanged then ()
		else
		    (verbose "  Writing pinterface\n";
		     Fs.write_pinterface (pinterface,pi))
	in  unchanged
	end

    fun elaborating (l:label) : unit =
	verbose (concat["===== Elaborating ", Name.label2longname l," =====\n"])

    datatype module = RTL of Rtl.module | LIL of Lil.module

    (*
	RTL parameters are more specific than unit names.
	LIL modules do not specify their parameters.
    *)
    fun get_using (desc : I.desc, module:module) : I.units =
	let fun isrtlparm (parms : Set.set) (U:label) : bool =
		Set.member (parms,U) orelse
		let val (c,r) = Name.make_cr_labels U
		in  Set.member(parms,c) orelse
		    Set.member(parms,r)
		end
	    val isparm : label -> bool =
		(case module of
		    RTL (Rtl.MODULE {parms,...}) => isrtlparm parms
		|   LIL (Lil.MODULE {parms,...}) => isrtlparm parms)
	    fun unit (U:label) : label option =
		if isparm U then SOME U else NONE
	    fun mapper (pdec:I.pdec) : label option =
		(case pdec of
		    I.IDEC _ => NONE
		|   I.SCDEC {name=U,...} => unit U
		|   I.UDEC {name=U,...} => unit U)
	in  List.mapPartial mapper desc
	end

    fun generate (desc:I.desc, U:label, asm:file, using_file:file, tali_rel:file, ilmod:LinkIl.module) : unit =
	let val name = Name.label2name' U
	    val nilmod = Linknil.il_to_nil (name,ilmod)
	    val _ = timestamp()
	    val tomod =
		(case (Target.getTarget()) of
		    Platform.ALPHA => RTL o Linkrtl.nil_to_rtl
		|   Platform.SPARC => RTL o Linkrtl.nil_to_rtl
		|   Platform.TALx86 => LIL o Linklil.nil_to_lil)
	    val module = tomod(name,nilmod)
	    val using = get_using (desc,module)
	    val _ = Fs.write I.blastOutUnits using_file using
	    fun rtlwrap f module s =
		(case module of
		    RTL rmod => f (s,rmod)
		|   _ => error "Can't translate LIL to sparc/alpha")
	    fun lilwrap f module s =
		(case module of
		    LIL rmod =>
			let val look : label -> file =
				I.P.D.tali_rel o (lookup desc)
			    val imports = map look using
			    val exports = [tali_rel]
			in  f (s,name,imports,exports,rmod)
			end
		|   _ => error "Can't translate RTL to TAL")
	    val toasm =
		(case (Target.getTarget()) of
		    Platform.ALPHA => rtlwrap Linkalpha.rtl_to_asm
		|   Platform.SPARC => rtlwrap Linksparc.rtl_to_asm
		|   Platform.TALx86 => lilwrap LinkTAL.lil_to_asm)
	    val _ = Fs.write' (toasm module) asm
	in  ()
	end

    (*
	Convert an up to date asm or asmz file to asm, asmz and obj
	files according to flags, returing false if we are not native
	and assembly is required.
    *)
    fun tal_includes (desc:I.desc) : string list =
	let fun folder (pdec:I.pdec, acc:(StringSet.set * string list))
		: StringSet.set * string list =
		let val pos = I.P.D.pos pdec
		    val dir = I.F.tal_include pos
		    val (set,list) = acc
		in  if StringSet.member(set,dir) then
			acc
		    else
			let val set = StringSet.add(set,dir)
			    val list = "-I"::dir::list
			in  (set,list)
			end
		end
	in  if Target.tal() then
		#2(foldr folder (StringSet.empty,nil) desc)
	    else
		nil
	end

    fun assemble'' (desc:I.desc, asm:file, asmz:file, obj:file) : bool =
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
	    if assemble then Tools.assemble (tal_includes desc,asm,obj) else ();
	    if compress then Tools.compress{src=asm, dest=asmz} else ();
	    if removeasm then Fs.remove asm else ();
	    if removeasmz then Fs.remove asmz else ();
	    finished
	end

    fun finish_compile (desc:I.desc, U:label, ilmod:LinkIl.module, asm:file,
	    asmz:file, obj:file, using:file, tali_rel:file) : bool =
	let val elab_only =
		!UptoElaborate orelse
		(!UptoAsm andalso not (!KeepAsm))
	in  elab_only orelse
	    (generate(desc,U,asm,using,tali_rel,ilmod);
	     assemble''(desc,asm,asmz,obj))
	end

    fun compile_srci ((desc,pdec):inputs) : bool =
	let val {name=I,iexp,...} = I.D.D.i pdec
	    val {opened,src,pinterface,...} = I.D.I.src iexp
	    val (fp,topspec) = parse_topspec (I,src)
	    val _ = elaborating I
	    val pctx = precontext desc
	    val _ = reset()
	    val opt = LinkIl.elab_topspec (pctx,opened,fp,topspec)
	    val pi = elab_opt (src,opt)
	    val _ = ignore(write_pinterface (pctx,pinterface,pi))
	in  true
	end

    fun compile_primi ((_,pdec):inputs) : bool =
	let val {name=I,iexp,...} = I.D.D.i pdec
	    val {pinterface,...} = I.D.I.prim iexp
	    val _ = elaborating I
	    val _ = reset()
	    val pi = LinkIl.elab_primspec()
	    val _ = ignore(write_pinterface (nil,pinterface,pi))
	in  true
	end

    fun compile_sc (tali:bool) ((desc,pdec):inputs) : bool =
	(tali orelse
	let val {name=U, asc=I, tali, ...} = I.D.D.sc pdec
	    val pctx = precontext desc
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val _ = compile_tali(pctx,U,pi,tali)
	in  true
	end)

    fun compile_srcu (tali_ready:bool, ack_inter:unit -> unit)
	    ((desc,pdec):inputs) : bool =
	let val {name=U,uexp,...} = I.D.D.u pdec
	    val {opened,pinterface,src,asm,using_file,asmz,obj,tali,tali_rel,...} = I.D.U.src uexp
	    val (fp,topdec) = parse_topdec (U,src)
	    val _ = elaborating U
	    val pctx = precontext desc
	    val _ = reset()
	    val opt = LinkIl.elab_topdec (pctx,U,opened,fp,topdec)
	    val (ilmod,pi) = elab_opt (src,opt)
	    val unchanged = write_pinterface (pctx,pinterface,pi)
	    val _ =
		if unchanged orelse tali_ready then ()
		else compile_tali (pctx,U,pi,tali)
	    val _ = ack_inter()
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,using_file,tali_rel)
	end

    fun compile_ssrcu (tali_ready:bool) ((desc,pdec):inputs) : bool =
	let val {name=U,uexp,...} = I.D.D.u pdec
	    val {opened,asc=I,src,asm,asmz,obj,using_file,tali,tali_rel,...} = I.D.U.ssrc uexp
	    val (fp,topdec) = parse_topdec (U,src)
	    val _ = elaborating U
	    val pctx = precontext desc
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val _ =
		if tali_ready then ()
		else compile_tali(pctx,U,pi,tali)
	    val _ = reset() (* not essential *)
	    val opt = LinkIl.elab_sealed_topdec (pctx,U,opened,fp,topdec,pi)
	    val ilmod = elab_opt (src,opt)
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,using_file,tali_rel)
	end

    fun compile_primu (tali_ready:bool) ((desc,pdec):inputs) : bool =
	let val {name=U,uexp,...} = I.D.D.u pdec
	    val {asc=I,asm,asmz,obj,using_file,tali,tali_rel,...} = I.D.U.prim uexp
	    val _ = elaborating U
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val _ =
		if tali_ready then ()
		else compile_tali(nil,U,pi,tali)
	    val _ = reset() (* not essential *)
	    val opt = LinkIl.elab_primdec (U,pi)
	    val ilmod = elab_opt ("primitive unit",opt)
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,using_file,tali_rel)
	end

    fun compile (inputs:inputs, f:unit -> unit) : bool =
	let val _ = timestamp()
	    val (desc,pdec) = inputs
	    val l = I.P.D.name pdec
	    val _ = msg("[compiling " ^ Name.label2longname l ^ "]\n")
	    fun primwrap f x =
		let val _ = LinkIl.compiling_tiltprim := true
		    val r = Util.apply(f,x)
		    val _ = LinkIl.compiling_tiltprim := false
		in  r()
		end
	    (* The updater guarantees that if it exists, then it is up to date. *)
	    val tali_ready =
		not (Target.tal()) orelse
		not (isSome (I.P.D.tali' pdec)) orelse
		Fs.exists(I.P.D.tali pdec)
	    val compile =
		(case pdec of
		    I.IDEC {iexp=I.SRCI _,...} => compile_srci
		|   I.IDEC {iexp=I.PRIMI _,...} => primwrap compile_primi
		|   I.SCDEC {stable=false, ...} => compile_sc tali_ready
		|   I.UDEC {uexp=I.SRCU _,...} => compile_srcu (tali_ready,f)
		|   I.UDEC {uexp=I.SSRCU _,...} => compile_ssrcu tali_ready
		|   I.UDEC {uexp=I.PRIMU _,...} =>
			primwrap (compile_primu tali_ready)
		|   _ => error "compile can't work with pdec")
	    val finished = compile(simplify_inputs inputs)
	    val needinfo =
		(case pdec of
		    I.SCDEC _ => not tali_ready
		|   _ => true)
	    val _ = if needinfo then write_summary inputs else ()
	in  finished
	end handle Fail msg =>
	    let val (_,pdec) = inputs
		val pos = I.P.D.pos pdec
		val msg = concat[Pos.tostring pos, ": ", msg]
	    in	reject msg
	    end

    fun assemble (inputs:inputs) : unit =
	let val (desc,pdec) = simplify_inputs inputs
	    val {name=U,uexp,...} = I.D.D.u pdec
	    val _ = msg("[assembling " ^ Name.label2longname U ^ "]\n")
	    val asm = I.P.U.asm uexp
	    val asmz = I.P.U.asmz uexp
	    val obj = I.P.U.obj uexp
	    (*
		Update may have brought information about these
		files into the cache which a slave subsequently made
		out of date.
	    *)
	    val _ = Fs.flush_some [asm,asmz,obj]
	in  if assemble'' (desc,asm,asmz,obj) then ()
	    else error "unable to assemble (not native)"
	end

    fun uexp_using (uexp:I.uexp) : I.units =
	(case uexp of
	    I.PRECOMPU {using,...} => using
	|   I.COMPU {using,...} => using
	|   _ =>
		let val using_file = I.P.U.using_file uexp
		    val using = Fs.read I.blastInUnits using_file
		in  using
		end)

    fun link (desc:I.desc, exe:file) : unit =
	let val {exe,asm,asmz,obj} = I.F.link exe
	    val _ = msg ("[linking " ^ exe ^ "]\n")
	    fun unitname (pdec:I.pdec) : label option =
		(case pdec of
		    I.IDEC _ => NONE
		|   I.SCDEC _ => error "link saw unimplemented unit"
		|   I.UDEC {name=U,...} => SOME U)
	    val units : label list = List.mapPartial unitname desc
	    val lookup : label -> I.uexp = I.P.D.uexp o (lookup desc)
	    fun untyped (f:{asmFile:string, units:string list} -> unit) (tmp:string) : unit =
		let val units = map Name.label2name' units
		in  f{asmFile=tmp, units=units}
		end
	    fun typed (f:{asmFile:string, units:{name:string, imports:string list}list} -> unit) (tmp:string) : unit =
		let fun mapper (U:label) : {name:string, imports:string list} =
			let val name = Name.label2name' U
			    val uexp = lookup U
			    val imports : label list = uexp_using uexp
			    val imports : string list =
				map Name.label2name' imports
			in  {name=name, imports=imports}
			end
		    val units = map mapper units
		in  f{asmFile=tmp, units=units}
		end
	    val _ = timestamp()
	    val generate : string -> unit =
		(case (Target.getTarget()) of
		    Platform.ALPHA => untyped Linkalpha.link
		|   Platform.SPARC => untyped Linksparc.link
		|   Platform.TALx86 => typed (error "no TAL linker yet"))
	    val _ = reset() (* not essential *)
	    val _ = Fs.write' generate asm
	in  if assemble'' (desc,asm,asmz,obj) then
		let val objs = map (I.P.U.obj o lookup) units
		    val includes = tal_includes desc
		in  Tools.link (includes, objs @ [obj], exe)
		end
	    else ()
	end

    (*
	Packing libraries.  The three project description files
	constructed for a library define exactly the same unit and
	interface names; they differ only in whether or not unit
	implementations are available.	Doing it any other way is
	likely to surprise the user.

	We pack a library by constructing internal language project
	descriptions and using these to copy compiled files and to
	generate external language project descriptions.  There are a
	few wrinkles.  If we have not compiled to binary, then the
	packed impl description just includes the packed inter
	description.  We do not copy tali files unless we have
	compiled to binary on talx86.
    *)

    type constructparms =
	{libsrc:label -> file,
	 libpos:Pos.pos}

    fun iexp_using (iexp:I.iexp) : I.units =
	(case iexp of
	    I.PRECOMPI {using,...} => using
	|   I.COMPI {using,...} => using
	|   _ =>
		let val pinterface = I.P.I.pinterface iexp
		    val using = Fs.read_pinterface_parm pinterface
		in  using
		end)

    fun uexp_opened (uexp:I.uexp) : I.units =
	(case (I.P.U.opened' uexp) of
	    SOME opened => opened
	|   NONE => nil)

    fun construct_iexp (constructparms:constructparms) (I:label, iexp:I.iexp) : I.pdec =
	let val {libsrc,libpos,...} = constructparms
	    val using = iexp_using iexp
	    val iexp =
		(case (I.P.I.source' iexp) of
		    SOME (opened,_) =>
			I.C.I.precomp'(libpos,I,using,opened,libsrc I)
		|   NONE =>
			I.C.I.comp'(libpos,I,using))
	    val pdec = I.C.D.i(I,iexp)
	in  pdec
	end

    fun construct_uexp_noimpl (parms:constructparms) (U:label, uexp:I.uexp) : I.pdec =
	let val {libpos,...} = parms
	    val I = I.P.U.asc uexp
	    val pdec = I.C.D.sc(libpos,I,U,true)
	in  pdec
	end

    fun construct_uexp_nosrc (parms:constructparms) (U:label, uexp:I.uexp) : I.pdec =
	let val {libpos,...} = parms
	    val I = I.P.U.asc uexp
	    val using = uexp_using uexp
	    val opened = uexp_opened uexp
	    val uexp = I.C.U.comp'(libpos,U,using,opened,I)
	    val pdec = I.C.D.u(U,uexp)
	in  pdec
	end

    fun construct_uexp_src (parms:constructparms) (U:label, uexp:I.uexp) : I.pdec =
	(case (I.P.U.source' uexp) of
	    SOME(opened,_) =>
		let val {libsrc,libpos,...} = parms
		    val src = libsrc U
		    val using = uexp_using uexp
		    val I = I.P.U.asc uexp
		    val uexp = I.C.U.precomp'(libpos,U,using,I,opened,src)
		    val pdec = I.C.D.u(U,uexp)
		in  pdec
		end
	|   NONE => construct_uexp_nosrc parms (U,uexp))

    fun construct_desc (pack:I.F.pack, impl:bool, srcdesc:I.desc) : I.desc =
	let val libsrc = #src pack
	    val desc = if impl then #impl pack else #inter pack
	    val libpos = Pos.pos' desc
	    val parms : constructparms = {libsrc=libsrc, libpos=libpos}
	    val construct_iexp : label * I.iexp -> I.pdec = construct_iexp parms
	    val construct_uexp : label * I.uexp -> I.pdec =
		(case (!PackUnitSource, impl) of
		    (_,false) => construct_uexp_noimpl
		|   (true,_) => construct_uexp_src
		|   (false,_) => construct_uexp_nosrc) parms
	    fun mapper (pdec:I.pdec) : I.pdec =
		(case pdec of
		    I.IDEC {name=I,iexp,...} => construct_iexp(I,iexp)
		|   I.SCDEC {name=U,asc=I,...} => I.C.D.sc(libpos,U,I,true)
		|   I.UDEC {name=U,uexp,...} => construct_uexp(U,uexp))
	in  map mapper srcdesc
	end

    type copyparms =
	{copy:file * file -> unit,
	 copysrc:file * file -> unit,
	 copytali:file * file -> unit,
	 writeusing:file * I.units -> unit}

    fun copy_iexp (parms:copyparms, iexp:I.iexp, libiexp:I.iexp) : unit =
	(case libiexp of
	    I.PRECOMPI {src,info,pinterface,...} =>
		let val {copysrc,copy,...} = parms
		    val _ = copysrc(I.P.I.src iexp,src)
		    val _ = copy(I.P.I.info iexp,info)
		    val _ = copy(I.P.I.pinterface iexp,pinterface)
		in  ()
		end
	|   I.COMPI {pinterface,...} =>
		let val {copy,...} = parms
		    val _ = copy(I.P.I.pinterface iexp,pinterface)
		in  ()
		end
	|   _ => error "copy_iexp")

    fun copy_uexp (parms:copyparms, uexp:I.uexp, libuexp:I.uexp) : unit =
	(case libuexp of
	    I.PRECOMPU {using,obj,src,info,using_file,tali,...} =>
		let val {copysrc,copy,writeusing,copytali,...} = parms
		    val _ = copysrc(I.P.U.src uexp,src)
		    val _ = copy(I.P.U.info uexp,info)
		    val _ = copy(I.P.U.obj uexp,obj)
		    val _ = writeusing(using_file,using)
		    val _ = copytali(I.P.U.tali uexp,tali)
		in  ()
		end
	|   I.COMPU {using,obj,using_file,tali,...} =>
		let val {copy,writeusing,copytali,...} = parms
		    val _ = copy(I.P.U.obj uexp,obj)
		    val _ = writeusing(using_file,using)
		    val _ = copytali(I.P.U.tali uexp,tali)
		in  ()
		end
	|   _ => error "copy_uexp")

    fun copy_pdec (parms:copyparms) (pdec:I.pdec, libpdec:I.pdec) : unit =
	(case libpdec of
	    I.IDEC {iexp=libiexp,...} =>
		let val iexp = I.P.D.iexp pdec
		in  copy_iexp (parms,iexp,libiexp)
		end
	|   I.SCDEC {stable=true,tali,...} =>
		let val {copytali,...} = parms
		    val _ = copytali(I.P.D.tali pdec,tali)
		in  ()
		end
	|   I.SCDEC _ => error "copy_pdec"
	|   I.UDEC {uexp=libuexp,...} =>
		let val uexp = I.P.D.uexp pdec
		in  copy_uexp (parms,uexp,libuexp)
		end)

    fun copy_files (impl:bool, srcdesc:I.desc, libdesc:I.desc) : unit =
	let val dontcopy : file * file -> unit = fn _ => ()
	    val copy : file * file -> unit = Fs.copy
	    val copysrc : file * file -> unit =
		if impl then dontcopy else copy
	    val copytali : file * file -> unit =
		if !UptoElaborate orelse not(Target.tal())
		then dontcopy
		else copy
	    fun writeusing (file:string, using:I.units) : unit =
		Fs.write I.blastOutUnits file using
	    val parms : copyparms =
		{copy=copy, copysrc=copysrc, copytali=copytali,
		 writeusing=writeusing}
	in  app (copy_pdec parms) (Listops.zip srcdesc libdesc)
	end

    type externparms =
	{src:label -> E.exp}

    fun extern_iexp (parms:externparms, I:label, iexp:I.iexp) : E.ie =
	(case iexp of
	    I.PRECOMPI {opened,...} =>
		let val {src,...} = parms
		in  E.PRECOMPI(src I, opened)
		end
	|   I.COMPI _ => E.COMPI
	|   _ => error "extern_iexp")

    fun extern_uexp (parms:externparms, U:label, uexp:I.uexp) : E.ue =
	(case uexp of
	    I.PRECOMPU {opened,asc=I,...} =>
		let val {src,...} = parms
		in  E.PRECOMPU(src U,opened,I)
		end
	|   I.COMPU {opened,asc=I,...} => E.COMPU(opened,I)
	|   _ => error "extern_uexp")

    fun extern_pdec (parms:externparms) (pdec:I.pdec) : E.ent =
	(case pdec of
	    I.IDEC {name=I,iexp,...} =>
		let val ie = extern_iexp (parms,I,iexp)
		in  E.INTERFACE(I,ie)
		end
	|   I.SCDEC {name=U,asc=I,stable,...} => E.SC(U,I,stable)
	|   I.UDEC {name=U,uexp,...} =>
		let val ue = extern_uexp (parms,U,uexp)
		in  E.UNIT(U,ue)
		end)

    fun extern_desc (pack:I.F.pack, desc:I.desc) : E.ents =
	let val {src_rel,...} = pack
	    val src : label -> E.exp = E.EXP_STR o src_rel
	    val parms : externparms = {src=src}
	    val ents = map (extern_pdec parms) desc
	in  ents
	end

    fun write_ents (desc:file, ents:E.ents) : unit =
	let val os = TextIO.openOut desc
	    val r = Util.apply (Project.print_ents, (os,ents))
	    val _ = TextIO.closeOut os
	in  r()
	end

    fun pack_real (pack:I.F.pack, impl:bool, desc:I.desc) : unit =
	let val descfile = if impl then #impl pack else #inter pack
	    val libdesc = construct_desc(pack,impl,desc)
	    val _ = copy_files(impl,desc,libdesc)
	    val ents = extern_desc(pack,libdesc)
	    val _ = write_ents(descfile,ents)
	in  ()
	end

    fun pack_fake_impl (pack:I.F.pack) : unit =
	let val descfile = #impl pack
	    val ents = [E.INCLUDE (E.EXP_STR I.F.inter)]
	    val _ = write_ents (descfile,ents)
	in  ()
	end

    fun pack_desc (pack:I.F.pack) : unit =
	let val descfile = #desc pack
	    val linking = Name.symbol_label(Symbol.varSymbol "linking")
	    val exp =
		E.EXP_IF (E.EXP_VAR linking,
		    E.EXP_STR I.F.impl,
		    E.EXP_STR I.F.inter)
	    val ents = [E.INCLUDE exp]
	    val _ = write_ents (descfile,ents)
	in  ()
	end

    fun pack (desc:I.desc, libdir:file) : unit =
	let val _ = msg ("[packing " ^ libdir ^ "]\n")
	    val pack = I.F.pack libdir
	    val _ = pack_real(pack,false,desc)
	    val _ =
		if !UptoElaborate orelse !UptoAsm
		then pack_fake_impl pack
		else pack_real(pack,true,desc)
	    val _ = pack_desc pack
	in  ()
	end

    val pack = Stats.timer("packing libraries",pack)

    fun wrap (what:string, f:'a -> 'b, proj:'a -> desc) : 'a -> 'b =
	fn x =>
	let val _ = debugdo (fn () => I.check_desc("Compiler."^what,proj x))
	in  f x
	end

    val summarize = wrap("summarize",summarize,#1)
    val gc_desc = wrap("gc_desc",gc_desc,#1)
    val get_inputs = wrap("get_inputs",get_inputs,#1)
    val compile_int = wrap("compile_int",compile_int,#1)
    val compile = wrap("compile",compile,(#1) o (#1))
    val assemble = wrap("assemble",assemble,#1)
    val link = wrap("link",link,#1)
    val pack = wrap("pack",pack,#1)

end
