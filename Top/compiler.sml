(* Interface to the compiler. *)
(*
    NB we reset the label counter to make the elaborator more
    deterministic.  Fresh labels must be chosen to account for SML
    shadowing and these sometimes make it into compilation unit
    interfaces.	 We want to choose the same labels each time to ensure
    that if a unit's source code and evaluation context does not
    change, then the compiler will infer the same interface every time
    it elaborates the unit.  We also reset the label counter prior to
    generating the link unit so that diffing that assembler is easier.
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
    structure P = LinkParse
    structure F = Formatter

    type label = Name.label
    type desc = IntSyn.desc
    type pdec = IntSyn.pdec
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

    val pinterface : I.pdec -> file =
	I.P.I.pinterface o I.P.D.iexp

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

    fun gc_desc' (desc:desc, roots:label list) : desc =
	let val keep = reachable(desc,Set.addList(Set.empty,roots))
	    fun keep_pdec (pdec:I.pdec) : bool =
		Set.member(keep, I.P.D.name pdec)
	    val desc' = List.filter keep_pdec desc
	in  desc'
	end

    fun gc_desc (desc:desc, roots:label list) : desc =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val l = F.pp_list Ppil.pp_label' roots
		    val fmt = F.HOVbox
			[F.String "Compiler.gc_desc", F.Break,
			 F.String "desc = ", d, F.Break,
			 F.String "roots = ", l, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc desc
		in  ()
		end
	    val _ = debugdo report
	in  gc_desc' (desc,roots)
	end

    (*
	A project description has more information than necessary to
	compile a pdec (as opposed to, say, packing a library).	 To
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

    fun compi (I:label, using:I.units, pinterface:file, pos:Pos.pos) : I.pdec =
	let val compi : I.compi =
		{pos=pos, pinterface=pinterface, using=using}
	    val iexp = I.COMPI compi
	    val pdec = I.C.D.i(I,iexp)
	in  pdec
	end

    fun rewritei (pdec:I.pdec, revdesc:I.pdec list) : I.pdec list =
	let val {name=I,iexp,...} = I.D.D.i pdec
	    fun rewrite (using, pinterface, pos) =
		compi (I,using,pinterface,pos) :: revdesc
	in  (case iexp of
		I.SRCI {pinterface,pos,...} =>
		    let val using = Fs.read_pinterface_parm pinterface
		    in	rewrite(using,pinterface,pos)
		    end
	    |	I.PRIMI {pinterface,pos,...} => rewrite(nil,pinterface,pos)
	    |	I.PRECOMPI {using,pinterface,pos,...} =>
		    rewrite(using,pinterface,pos)
	    |	I.COMPI _ => pdec::revdesc)
	end

    fun rewriteu (pdec:I.pdec, revdesc:I.pdec list) : I.pdec list =
	let val {name=U,uexp,...} = I.D.D.u pdec
	    val pos = I.P.U.pos uexp
	    fun simple I = I.C.D.sc (pos,U,I,false) :: revdesc
	in  (case (I.P.U.asc' uexp) of
		SOME I => simple I
	    |	NONE =>
		    let val pinterface = I.P.U.pinterface uexp
			val I = Name.fresh_internal_label "inferred"
			val using = Fs.read_pinterface_parm pinterface
			val idec = compi (I,using,pinterface,pos)
			val udec = I.C.D.sc(pos,U,I,false)
		    in	udec :: idec :: revdesc
		    end)
	end

    fun skel_desc (desc:desc, keep:label) : desc =
	let fun folder (pdec:I.pdec, revdesc:I.pdec list) : I.pdec list =
		let val l = I.P.D.name pdec
		in  if Name.eq_label(l,keep) then pdec::revdesc
		    else
			(case pdec of
			    I.IDEC _ => rewritei (pdec,revdesc)
			|   I.SCDEC _ => pdec :: revdesc
			|   I.UDEC _ => rewriteu (pdec,revdesc))
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
	    (case (rev desc) of
		nil => error "empty description"
	    |	pdec::revdesc => (rev revdesc, pdec))
	end

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

    fun info' (desc:desc, pdec:pdec) : I.info =
	let val crc : string -> Crc.crc = Fs.crc
	    val look : label -> file = pinterface o (lookup desc)
	    val precontext' = precontext' desc
	    fun mapper (U,file) = (U,crc file)
	    val ue : I.ue = map mapper precontext'
	    fun src_i (opened:I.units, src:file) : I.info =
		I.INFO_I {ue=ue, src=SOME(opened,crc src)}
	    fun nosrc_i () : I.info = I.INFO_I {ue=ue, src=NONE}
	    fun nosrc_u (I:label) : I.info =
		I.INFO_U {ue=ue, src=NONE, pinterface=SOME(crc(look I))}
	    fun src_u (Iopt:label option, opened:I.units, src:file) : I.info =
		let val src = SOME(opened,crc src)
		    val pinterface = Option.map (crc o look) Iopt
		in  I.INFO_U {ue=ue, src=src, pinterface=pinterface}
		end
	    fun interface (iexp:I.iexp) : I.info =
		(case (I.P.I.source' iexp) of
		    SOME opened_src => src_i opened_src
		|   NONE => nosrc_i())
	    fun unit (uexp:I.uexp) : I.info =
		(case (I.P.U.source' uexp) of
		    SOME (opened,src) => src_u(I.P.U.asc' uexp,opened,src)
		|   NONE => nosrc_u (I.P.U.asc uexp))
	    val info =
		(case pdec of
		    I.IDEC {iexp,...} => interface iexp
		|   I.SCDEC {name=I,...} => nosrc_u I
		|   I.UDEC {uexp,...} => unit uexp)
	in  info
	end

    fun info (desc:desc, pdec:pdec) : I.info =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val p = I.pp_pdec pdec
		    val fmt = F.HOVbox
			[F.String "Compiler.info", F.Break,
			 F.String "desc = ", d, F.Break,
			 F.String "pdec = ", p, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc (desc @ [pdec])
		in  ()
		end
	    val _ = debugdo report
	in  info' (desc,pdec)
	end

    fun write_info (desc:desc, pdec:pdec) : unit =
	(case (I.P.D.info' pdec) of
	    SOME info_file =>
		let val info = info'(desc,pdec)
		in  Fs.write I.blastOutInfo info_file info
		end
	|   NONE => ())

    fun compile_tali (pctx:LinkIl.precontext, U:label, pi:LinkIl.pinterface, tali:file) : unit =
	let val _ = verbose(concat
		["===== Making interface for ",
		 Name.label2longname U, " =====\n"])
	    val name = Name.label2name' U
	    val ilint = LinkIl.sc_module (pctx,U,pi)
	    val _ = timestamp()
	    val nilint = Linknil.ilint_to_nilint (name,ilint)
	    val _ = timestamp()
	    val lilint = Linklil.nilint_to_lilint (name,nilint)
	    val _ = Fs.write' (LinkTAL.to_tal_interface lilint) tali
	in  ()
	end

    fun compile_int' (desc:I.desc, pdec:pdec) : unit =
	let val _ = timestamp()
	    val U = I.P.D.name pdec
	    val _ = msg("[compiling TAL interface for " ^ Name.label2longname U ^ "]\n")
	    val _ = Name.reset_label_counter()
	    val _ = Name.reset_varmap()
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
	    val _ = if needinfo then write_info(desc,pdec) else ()
	in  ()
	end handle Fail msg =>
	    let val pos = I.P.D.pos pdec
		val msg = concat[Pos.tostring pos, ": ", msg]
	    in	reject msg
	    end

    fun compile_int (desc:I.desc, pdec:pdec) : unit =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val p = I.pp_pdec pdec
		    val fmt = F.HOVbox
			[F.String "Compiler.compile_int", F.Break,
			 F.String "desc = ", d, F.Break,
			 F.String "pdec = ", p, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc (desc @ [pdec])
		in  ()
		end
	    val _ = debugdo report
	in  compile_int' (desc,pdec)
	end

    fun wrap_parse (f:'a P.parser) (l:label,src:file) : P.filepos * 'a =
	(case (f(Name.label2longname l, src)) of
	    SOME r => r
	|   NONE => fail ("could not parse " ^ src))

    val parse_topspec : label * file -> P.filepos * Ast.topspec =
	wrap_parse P.parse_topspec

    val parse_topdec : label * file -> P.filepos * Ast.dec =
	wrap_parse P.parse_topdec

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

    fun compile_srci (desc:I.desc, pdec:I.pdec) : bool =
	let val {name=I,iexp,...} = I.D.D.i pdec
	    val {opened,src,pinterface,...} = I.D.I.src iexp
	    val (fp,topspec) = parse_topspec (I,src)
	    val _ = elaborating I
	    val pctx = precontext desc
	    val opt = LinkIl.elab_topspec (pctx,opened,fp,topspec)
	    val pi = elab_opt (src,opt)
	    val _ = ignore(write_pinterface (pctx,pinterface,pi))
	in  true
	end

    fun compile_primi (_:I.desc, pdec:I.pdec) : bool =
	let val {name=I,iexp,...} = I.D.D.i pdec
	    val {pinterface,...} = I.D.I.prim iexp
	    val _ = elaborating I
	    val pi = LinkIl.elab_primspec()
	    val _ = ignore(write_pinterface (nil,pinterface,pi))
	in  true
	end

    fun compile_sc (tali:bool) (desc:I.desc, pdec:I.pdec) : bool =
	(tali orelse
	let val {name=U, asc=I, tali, ...} = I.D.D.sc pdec
	    val pctx = precontext desc
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val _ = compile_tali(pctx,U,pi,tali)
	in  true
	end)

    fun compile_srcu (tali_ready:bool, ack_inter:unit -> unit)
	    (desc:I.desc, pdec:I.pdec) : bool =
	let val {name=U,uexp,...} = I.D.D.u pdec
	    val {opened,pinterface,src,asm,using_file,asmz,obj,tali,tali_rel,...} = I.D.U.src uexp
	    val (fp,topdec) = parse_topdec (U,src)
	    val _ = elaborating U
	    val pctx = precontext desc
	    val opt = LinkIl.elab_topdec (pctx,U,opened,fp,topdec)
	    val (ilmod,pi) = elab_opt (src,opt)
	    val unchanged = write_pinterface (pctx,pinterface,pi)
	    val _ =
		if unchanged orelse tali_ready then ()
		else compile_tali (pctx,U,pi,tali)
	    val _ = ack_inter()
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,using_file,tali_rel)
	end

    fun compile_ssrcu (tali_ready:bool) (desc:I.desc, pdec:I.pdec) : bool =
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
	    val opt = LinkIl.elab_sealed_topdec (pctx,U,opened,fp,topdec,pi)
	    val ilmod = elab_opt (src,opt)
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,using_file,tali_rel)
	end

    fun compile_primu (tali_ready:bool) (desc:I.desc, pdec:I.pdec) : bool =
	let val {name=U,uexp,...} = I.D.D.u pdec
	    val {asc=I,asm,asmz,obj,using_file,tali,tali_rel,...} = I.D.U.prim uexp
	    val _ = elaborating U
	    val pinterface = pinterface(lookup'(desc,I))
	    val pi = Fs.read_pinterface pinterface
	    val _ =
		if tali_ready then ()
		else compile_tali(nil,U,pi,tali)
	    val opt = LinkIl.elab_primdec (U,pi)
	    val ilmod = elab_opt ("primitive unit",opt)
	in  finish_compile (desc,U,ilmod,asm,asmz,obj,using_file,tali_rel)
	end

    fun compile' (desc:I.desc, pdec:I.pdec, f:unit -> unit) : bool =
	let val _ = timestamp()
	    val l = I.P.D.name pdec
	    val _ = msg("[compiling " ^ Name.label2longname l ^ "]\n")
	    val _ = Name.reset_label_counter()
	    val _ = Name.reset_varmap()
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
	    val finished = compile(desc,pdec)
	    val needinfo =
		(case pdec of
		    I.SCDEC _ => not tali_ready
		|   _ => true)
	    val _ = if needinfo then write_info(desc,pdec) else ()
	in  finished
	end handle Fail msg =>
	    let val pos = I.P.D.pos pdec
		val msg = concat[Pos.tostring pos, ": ", msg]
	    in	reject msg
	    end

    fun compile (desc:I.desc, pdec:I.pdec, f:unit -> unit) : bool =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val p = I.pp_pdec pdec
		    val fmt = F.HOVbox
			[F.String "Compiler.compile", F.Break,
			 F.String "desc = ", d, F.Break,
			 F.String "pdec = ", p, F.Break,
			 F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc (desc @ [pdec])
		in  ()
		end
	    val _ = debugdo report
	in  compile' (desc,pdec,f)
	end

    fun assemble' (desc:I.desc, pdec:I.pdec) : unit =
	let val {name=U,uexp,...} = I.D.D.u pdec
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

    fun assemble (desc:I.desc, pdec:I.pdec) : unit =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val p = I.pp_pdec pdec
		    val fmt = F.HOVbox
			[F.String "Compiler.assemble", F.Break,
			 F.String "desc = ", d, F.Break,
			 F.String "pdec = ", p, F.Newline()]
		    val _ = F.print_fmt fmt
		in  ()
		end
	    val _ = debugdo report
	in  assemble' (desc,pdec)
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

    fun link' (desc:I.desc, files:I.F.link) : unit =
	let val {exe,asm,asmz,obj} = files
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
	    val _ = Fs.write' generate asm
	in  if assemble'' (desc,asm,asmz,obj) then
		let val objs = map (I.P.U.obj o lookup) units
		    val includes = tal_includes desc
		in  Tools.link (includes, objs @ [obj], exe)
		end
	    else ()
	end

    fun link (desc:I.desc, files:I.F.link) : unit =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val fmt = F.HOVbox
			[F.String "Compiler.link", F.Break,
			 F.String "desc = ", d, F.Newline()]
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

    fun pack' (desc:I.desc, libdir:file) : unit =
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

    fun pack (desc:I.desc, libdir:file) : unit =
	let fun report () : unit =
		let val d = I.pp_desc desc
		    val fmt = F.HOVbox
			[F.String "Compiler.pack", F.Break,
			 F.String "desc = ", d, F.Newline()]
		    val _ = F.print_fmt fmt
		    val _ = I.check_desc desc
		in  ()
		end
	    val _ = debugdo report
	in  pack' (desc,libdir)
	end
    val pack = Stats.timer("packing libraries",pack)
end
