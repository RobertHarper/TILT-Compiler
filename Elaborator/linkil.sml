structure LinkIl :> LINKIL  =
struct

    (* Resolve mutual dependencies by installing functions here. *)
    val _ = IlStatic.installHelpers {eq_compile = Toil.xeq}
    val _ = IlContext.installHelpers {eq_con = IlStatic.eq_con}
    val _ = Equal.installHelpers {add_eq_entry = Toil.add_eq_entry}
    val _ = Pat.installHelpers {typecompile = Toil.typecompile,
				expcompile = Toil.expcompile,
				polyinst = Toil.polyinst}
    val _ = Signature.installHelpers {polyinst = Toil.polyinst}
    val _ = IlUtil.installHelpers {Context_Lookup_Labels = IlStatic.Context_Lookup_Labels}
    val _ = IlPrimUtilParam.installHelpers {con_bool = IlUtil.con_bool,
					    true_exp = IlUtil.true_exp,
					    false_exp = IlUtil.false_exp}

    type filepos = LinkParse.filepos
    type dec = Ast.dec
    type specs = Ast.spec list
    open Il

    val LinkIlDiag = Stats.ff("LinkIlDiag")
    fun msg str = if (!LinkIlDiag) then print str else ()

    val ShowHIL = Stats.ff("ShowHIL")
    val ShowInterface = Stats.ff("ShowInterface")

    val error = fn s => Util.error "linkil.sml" s

    val ShowContext = Stats.ff "ShowContext"
    val LinkIlDebug = Stats.ff "LinkIlDebug"
    fun debugdo (f : unit -> 'a) : unit =
	if !LinkIlDebug then (f(); ()) else ()

    exception Fail of string
    fun fail (s : string) : 'a =
	if !LinkIlDebug then error s
	else raise Fail s

    structure F = Formatter
    structure U = IlUtil
    structure C = IlContext
    structure S = IlStatic
    structure VarMap = Name.VarMap
    structure VarSet = Name.VarSet
    structure LabelMap = Name.LabelMap
    structure LabelSet = Name.LabelSet

    (*
	Variable Invariant: The top-level variables are fresh; that
	is, they are not bound in the ambient context and they are not
	used by any other interface that may be on hand.

	Main Invariant: Main is well-formed in the ambient context.

	Signat Invariant: Each signature definition is well-formed in
	a context in which mainvar : mainsignat is bound.

	Other Invariant: Each overload and extern is well-formed in a
	context in which mainvar : mainsignat is bound.
    *)

    type signats = (var * signat) LabelMap.map
    type fixity = (label * Fixity.fixity) list
    type ovlds = (label * ovld) list
    type externs = (var * con) LabelMap.map
    type other = {fixity : fixity, ovlds : ovlds, externs : externs}
    type interface = {main : var * signat, signats : signats, other : other}

    val Com = F.String ","
    fun pp_signats (s : signats) : F.format =
	let val items = LabelMap.listItemsi s
	    fun pp (l,(v,s)) = F.Hbox [Ppil.pp_label' l, F.String " > ",
				       Ppil.pp_var' v, F.String " = ",
				       Ppil.pp_signat' s]
	in  F.pp_list pp items
	end

    fun pp_ovlds (ovlds : ovlds) : F.format =
	let fun pp (l,ovld) = F.Hbox [Ppil.pp_label' l, F.String " : ",
				      Ppil.pp_ovld' ovld]
	in  F.pp_list pp ovlds
	end

    fun pp_externs (e : externs) : F.format =
	let val items = LabelMap.listItemsi e
	    fun pp (l,(v,c)) = F.Hbox [Ppil.pp_label' l, F.String " > ",
				       Ppil.pp_var' v, F.String " : ",
				       Ppil.pp_con' c]
	in  F.pp_list pp items
	end

    fun pp_interface (i : interface) : F.format =
	let val {main=(v,s),signats,other={fixity,ovlds,externs}} = i
	in  F.HOVbox [F.String "interface", F.Break,
		      F.String "main = ", Ppil.pp_var' v, F.String " : ",
		      Ppil.pp_signat' s, Com, F.Break,
		      F.String "signats = ", pp_signats signats,
		      Com, F.Break,
		      F.String "ovlds = ", pp_ovlds ovlds, Com, F.Break,
		      F.String "externs = ", pp_externs externs]
	end

    val pp_interface : interface -> unit =
	F.print_fmt o pp_interface

    (*
	A context's second component is necessary to resolve signature
	parameters and is used to open directly imported units prior
	to elaboration.
    *)

    type context = Il.context * interface LabelMap.map

    type pinterface = Il.pinterface

    val blastOutPinterface = IlBlast.blastOutPinterface
    val blastInPinterface = IlBlast.blastInPinterface

    fun unitname (p : parm) : label =
	(case p
	   of PARM l => l
	    | PARM_SIG (l,_) => l
	    | PARM_EXT (l,_) => l)

    fun parameters (i : pinterface) : LabelSet.set =
	let fun folder (p, acc) = LabelSet.add(acc, unitname p)
	in  VarMap.foldl folder LabelSet.empty (#parms i)
	end

    fun slice (ctxt : context, parms : LabelSet.set) : LabelSet.set =
	let val (ilctxt,im) = ctxt
	    fun addvar (l:label, s:VarSet.set) : VarSet.set =
		(case LabelMap.find (im,l)
		   of NONE => error "parameter not in context"
		    | SOME {main=(v,_),...} => VarSet.add(s,v))
	    val roots = LabelSet.foldl addvar VarSet.empty parms
	    val reachable = C.reachable (ilctxt, roots)
	    fun isReachable' (v : var) : bool = VarSet.member (reachable, v)
	    val exists : ('a -> bool) -> 'a LabelMap.map -> bool =
		fn f => fn lm => List.exists f (LabelMap.listItems lm)
	    fun isReachable (i : interface) : bool =
		let val {main=(main,_),signats,other={externs,...},...} = i
		in  isReachable' main orelse
		    exists (isReachable' o #1) signats orelse
		    exists (isReachable' o #1) externs
		end
	    fun addunit (l : label, i : interface,
			 s : LabelSet.set) : LabelSet.set =
		if isReachable i then LabelSet.add (s,l) else s
	in  LabelMap.foldli addunit LabelSet.empty im
	end
    val empty : context = (C.empty_context, LabelMap.empty)

    (* Compare to open_interface. *)
    fun add_unit (ctxt : context, l : label, i : interface) : context =
	let val (ilctxt,im) = ctxt
	    val {main=(v,s),signats,other={externs,...},...} = i
	    fun add_sig ((v,s), ilctxt) = C.add_context_sig' (ilctxt,v,s)
	    fun add_extern (l,(v,c), ilctxt) =
		C.add_context_extern' (ilctxt,v,l,c)
	    val ilctxt = C.add_context_mod (ilctxt,l,v,s)
	    val ilctxt = LabelMap.foldl add_sig ilctxt signats
	    val ilctxt = LabelMap.foldli add_extern ilctxt externs
	    val im = LabelMap.insert (im,l,i)
	in  (ilctxt,im)
	end

    fun interface_free (i : interface) : VarSet.set =
	let val {main=(mainvar,mainsig),signats,other,...} = i
	    val {fixity,ovlds,externs} = other
	    fun add_sig ((_,s), fv) = VarSet.union (fv, U.sig_free s)
	    fun add_ovld ((_,ov), fv) = VarSet.union (fv, U.ovld_free ov)
	    fun add_extern ((_,c), fv) = VarSet.union (fv, U.con_free c)
	    val fv = U.sig_free mainsig
	    val fv = LabelMap.foldl add_sig fv signats
	    val fv = foldl add_ovld fv ovlds
	    val fv = LabelMap.foldl add_extern fv externs
	    val fv = VarSet.difference (fv,VarSet.singleton mainvar)
	in  fv
	end

    fun add_parms (unitlab : label, i : interface, parms : parms) : parms =
	let val {main=(v,_),signats,other={externs,...}} = i
	    fun add_sig (siglab,(v,_),parms) =
		VarMap.insert (parms,v,PARM_SIG (unitlab,siglab))
	    fun add_extern (externlab,(v,_),parms) =
		VarMap.insert (parms,v,PARM_EXT (unitlab,externlab))
	    val parms = VarMap.insert(parms,v,PARM unitlab)
	    val parms = LabelMap.foldli add_sig parms signats
	    val parms = LabelMap.foldli add_extern parms externs
	in  parms
	end

    fun parms (ctxt : context, entries) : parms =
	let val vars = U.entries_free entries
	    fun keep (v,_) = VarSet.member (vars,v)
	    val all_parms = LabelMap.foldli add_parms VarMap.empty (#2 ctxt)
	in  VarMap.filteri keep all_parms
	end

    fun parameterize (arg : context * entries) : pinterface =
	{parms = parms arg,
	 entries = #2 arg}

    (*
	Factoring pulls all SDECs and SBNDs into a single main
	signature and structure, breaking any dependence on new
	signature definitions.
    *)

    (*
	Extend substitution to eliminate references to new signature
	definitions.
    *)

    fun sigdef_subst (entries, subst) : U.subst =
	let
	    fun folder (entry : context_entry, subst : U.subst)=
		(case entry
		   of CONTEXT_SIGNAT (l,v,s) =>
		       let val s = U.sig_subst(s, subst)
		       in  U.subst_add_sigvar(subst, v, s)
		       end
		    | _ => subst)
	in  foldl folder subst entries
	end

    val sdec_entries : entries -> sdecs
	= List.mapPartial (fn CONTEXT_SDEC sdec => SOME sdec | _ => NONE)

    fun factor_decresult (mainlab : label,
			  decresult : decresult) : sbnd * sdec =
	let val entries = map #2 decresult
	    val sbnds = List.mapPartial #1 decresult
	    val sdecs = sdec_entries entries
	    val subst = sigdef_subst (entries,U.empty_subst)
	    val mainsdecs = U.sdecs_subst (sdecs,subst)
	    val mainsbnds = U.sbnds_subst (sbnds,subst)
	    val mainvar = Name.fresh_var()
	    val sbnd = SBND(mainlab,
			    BND_MOD(mainvar,false,MOD_STRUCTURE mainsbnds))
	    val sdec = SDEC(mainlab,
			    DEC_MOD(mainvar,false,SIGNAT_STRUCTURE mainsdecs))
	in  (sbnd,sdec)
	end

    (* Extend substitution to fix up references to SDECs. *)

    fun main_subst (mainvar : var, mainsdecs : sdecs, subst) : U.subst =
	let val m = MOD_VAR mainvar
	    fun folder (sdec, subst) =
		let val SDEC(l,dec) = sdec
		    val p = (m,l)
		in  (case dec
		       of DEC_EXP (v,_,_,_) =>
			   U.subst_add_expvar(subst, v, MODULE_PROJECT p)
			| DEC_CON (v,_,_,_) =>
			   U.subst_add_convar(subst, v, CON_MODULE_PROJECT p)
			| DEC_MOD (v,_,_) =>
			   U.subst_add_modvar(subst, v, MOD_PROJECT p))
		end
	in  foldl folder subst mainsdecs
	end

    fun factor_entries (entries, subst) : var * signat * entries =
	let val sdecs = sdec_entries entries
	    val subst = sigdef_subst (entries,subst)
	    val mainsdecs = U.sdecs_subst (sdecs,subst)
	    val mainvar = Name.fresh_var()
	    val mainsig = SIGNAT_STRUCTURE mainsdecs
	    val subst = main_subst (mainvar,mainsdecs,subst)
	    val entries = (List.filter
			   (fn CONTEXT_SDEC _ => false | _ => true)
			   entries)
	    (*
		U.entries_subst does not touch bound sigvars so we
		apply the substitution one entry at at time.
	    *)
	    val entries = map (fn e => U.entry_subst (e,subst)) entries
	in  (mainvar,mainsig,entries)
	end

    (*
	Instantiate must instantiate parameters, factor the context
	entries, and establish the variable invariant.  The first two
	steps require a substitution; the third does not.
    *)

    (* Extend substitution to instantiate a single parameter. *)

    fun lookup (ctxt : context) (unitname : label) : interface =
	(case LabelMap.find (#2 ctxt, unitname)
	   of SOME i => i
	    | NONE => fail "unit not found")

    fun parm_subst (ctxt : context) (var,parm,subst) : U.subst =
	(case parm
	   of PARM unitlab =>
	       let val (ilctxt,_) = ctxt
	       in  (case C.Context_Lookup_Label (ilctxt,unitlab)
		      of SOME (PATH(v,nil)) =>
			  U.subst_add_modvar (subst,var,MOD_VAR v)
		       | _ => fail "PARM unit not a top-level structure")
	       end
	    | PARM_SIG (unitlab,siglab) =>
	       let val {signats, ...} = lookup ctxt unitlab
	       in  (case LabelMap.find (signats,siglab)
		      of SOME (v,_) =>
			  U.subst_add_sigvar (subst,var,SIGNAT_VAR v)
		       | NONE => fail "PARM_SIG signat not found")
	       end
	    | PARM_EXT (unitlab,extlab) =>
	       let val {other={externs,...}, ...} = lookup ctxt unitlab
	       in  (case LabelMap.find (externs,extlab)
		      of SOME (v,_) =>
			  U.subst_add_expvar (subst,var,VAR v)
		       | NONE => fail "PARM_EXT extern not found")
	       end)

    fun classify (entries, main : var * signat, signats : signats,
		  externs : externs, fixity : fixity,
		  ovlds : ovlds) : interface =
	(case entries
	   of nil => {main=main,signats=signats,
		      other={fixity=rev fixity,ovlds=rev ovlds,
			     externs=externs}}
	    | (entry :: rest) =>
	       (case entry
		  of CONTEXT_SDEC _ => fail "classify saw an SDEC"
		   | CONTEXT_SIGNAT (l,_,s) =>
		      let val v = Name.fresh_var()
			  val signats = LabelMap.insert (signats,l,(v,s))
		      in  classify (rest,main,signats,externs,fixity,ovlds)
		      end
		   | CONTEXT_EXTERN (_,_,l,c) =>
		      let val v = Name.fresh_var()
			  val externs = LabelMap.insert (externs,l,(v,c))
		      in  classify (rest,main,signats,externs,fixity,ovlds)
		      end
		   | CONTEXT_FIXITY x =>
		      classify (rest,main,signats,externs,x::fixity,ovlds)
		   | CONTEXT_OVEREXP x =>
		      classify (rest,main,signats,externs,fixity,x::ovlds)))

    fun instantiate (ctxt : context, i : pinterface) : interface option =
	let val _ = debugdo
		    (fn () =>
		     (print "---- LinkIl.instantiate called\n";
		      print "i = "; Ppil.pp_pinterface i; print "\n"))
	    val {entries,parms,...} = i
	    val subst = VarMap.foldli (parm_subst ctxt) U.empty_subst parms
	    val (mainvar,mainsig,entries) = factor_entries (entries,subst)
	    val i = classify (entries, (mainvar,mainsig), LabelMap.empty,
			      LabelMap.empty, nil, nil)
	in  SOME i
	end handle Fail _ => NONE

    fun wrap (what : string) (f : 'a -> bool) (x : 'a) : bool =
	(f x orelse
	 (debugdo (fn () => (print (what ^ " returning false\n")));
	  false))

    val eq_label = wrap "eq_label" Name.eq_label
    val Sig_IsEqual = wrap "Sig_IsEqual" S.Sig_IsEqual
    val soft_eq_con = wrap "soft_eq_con" S.soft_eq_con

    fun eq_labelmap (eq : 'a * 'a -> bool, map1 : 'a LabelMap.map,
		     map2 : 'a LabelMap.map) : bool =
	let fun eq' ((l1,a1), (l2,a2)) = eq_label(l1,l2) andalso eq(a1,a2)
	in  Listops.eq_list (eq',
			     LabelMap.listItemsi map1,
			     LabelMap.listItemsi map2)
	end

    fun eq_signats (ilctxt : Il.context, a : signats, b : signats) : bool =
	eq_labelmap ((fn ((_,s1), (_,s2)) => Sig_IsEqual (ilctxt,s1,s2)),a,b)
    val eq_signats = wrap "eq_signats" eq_signats

    fun eq_externs (ilctxt : Il.context, a : externs, b : externs) : bool =
	eq_labelmap ((fn ((_,c1), (_,c2)) => soft_eq_con (ilctxt,c1,c2)),a,b)
    val eq_externs = wrap "eq_externs" eq_externs

    fun compare (a : label * 'a, b : label * 'a) : order =
	Name.compare_label (#1 a, #1 b)
    val sort : (label * 'a) list -> (label * 'a) list =
	fn x => Listops.insertion_sort compare x

    fun eq_labelled (eq : 'a * 'a -> bool, a : (label * 'a) list,
		     b : (label * 'a) list) : bool =
	let fun eq' ((l1,x1),(l2,x2)) = eq_label(l1,l2) andalso eq(x1,x2)
	in  Listops.eq_list (eq',sort a,sort b)
	end

    fun eq_fix (a : fixity, b : fixity) : bool =
	eq_labelled (op= : Fixity.fixity * Fixity.fixity -> bool, a, b)
    val eq_fix = wrap "eq_fix" eq_fix

    (* Imperfect. *)
    fun eq_ovld (ilctxt : Il.context, a : ovlds, b : ovlds) : bool =
	let fun eq_conexp ((c1,e1),(c2,e2)) = soft_eq_con (ilctxt,c1,c2)
	    fun eq_overexp (OVLD(ce1,d1),OVLD(ce2,d2)) =
		(Listops.eq_list (eq_conexp,ce1,ce2) andalso
		 d1 = d2)
	in  eq_labelled (eq_overexp, a, b)
	end
    val eq_ovld = wrap "eq_ovld" eq_ovld

    fun eq_other (ilctxt : Il.context, a : other, b : other) : bool =
	(eq_fix (#fixity a, #fixity b) andalso
	 eq_ovld (ilctxt, #ovlds a, #ovlds b) andalso
	 eq_externs (ilctxt, #externs a, #externs b))
    val eq_other = wrap "eq_other" eq_other

    fun eq (ctxt : context, i1 : interface, i2 : interface) : bool =
	let val _ = debugdo
		    (fn () =>
		     (print "---- LinkIl.eq called\n";
		      print "i1 = "; pp_interface i1; print "\n";
		      print "i2 = "; pp_interface i2; print "\n"))
	    val {main=(v1,s1),signats=signats1,other=other1} = i1
	    val {main=(v2,s2),signats=signats2,other=other2} = i2
	    val ilctxt = #1 ctxt
	in  Sig_IsEqual (ilctxt,s1,s2) andalso
	    let val ilctxt = C.add_context_mod' (ilctxt,v1,s1)
		val s2 = SIGNAT_OF(PATH (v1,nil))
		val ilctxt = C.add_context_mod' (ilctxt,v2,s2)
	    in  eq_signats(ilctxt,signats1,signats2) andalso
		eq_other(ilctxt,other1,other2)
	    end
	end
    val eq = wrap "eq" eq

    fun seal (ctxt : context, m : module, i : interface) : module option =
	let val _ = debugdo
		    (fn () =>
		     (print "---- LinkIl.seal called\n";
		      print "m = "; Ppil.pp_module m; print "\n";
		      print "i = "; pp_interface i; print "\n"))
	    val (_, sbnd, sdec) = m
	    val {main=(v,sig_target),...} = i
	    val (mainlab,mod_actual,sig_actual) =
		(case (sbnd,sdec)
		   of (SBND (_,BND_MOD(_,_,m)),
		       SDEC (l,DEC_MOD(_,_,s))) => (l,m,s)
		    | _ => error "seal given malformed module")
	    val ilctxt = #1 ctxt
	    val _ = msg "  Sealing\n"
	in  Option.map
	    (fn m =>
	     let val sbnd = SBND(mainlab,BND_MOD(v,false,m))
		 val sdec = SDEC(mainlab,DEC_MOD(v,false,sig_target))
		 val module = (ilctxt,sbnd,sdec)
		 val _ =
		     if !ShowHIL then
			 (print "\nsealed module:\n"; Ppil.pp_module module;
			  print "\n\n")
		     else ()
	     in  module
	     end)
	    (Toil.seal (ilctxt, Error.nofilepos, mod_actual,
			sig_actual, sig_target))
	end

    fun unitlabel (unit : string) : label =
	Name.to_unit(Name.internal_label unit)

    type imports = labels

    (*
	We open the interfaces of direct imports in preparation for
	elaboration.  The idea is to elaborate in context "local open
	imports in [-] end".  To open an interface, we open its main
	structure; we add its signatures and externs to the context,
	this time with their labels; and we add its fixity and
	overload information to the context.  Compare to add_unit.

	We deviate from HS: Rather than preceding the elaboration
	result with an sbnd/sdec to handle the locally opened imports,
	we apply a substitution to the elaboration result.  This has
	three benefits.  First, the same mechanism handles a unit's
	signature definitions as well as its sdecs.  Second, unit
	interfaces are not polluted with open labels.  Third, we can
	continue to handle TiltPrim without object code.
    *)

    fun open_main (mainvar : var, (ilctxt,subst)) : Il.context * U.subst =
	let val open_lab = Name.to_open (Name.fresh_internal_label "import")
	    val v = Name.fresh_var()
	    val s = SIGNAT_OF (PATH (mainvar,nil))
	    val ilctxt = C.add_context_mod (ilctxt, open_lab, v, s)
	    val subst = U.subst_add_modvar(subst,v,MOD_VAR mainvar)
	in  (ilctxt,subst)
	end

    fun open_signats (signats : signats,
		      acc : Il.context * U.subst) : Il.context * U.subst =
	let fun add_sig (siglab, (sigvar,_), (ilctxt,subst)) =
		let val v = Name.fresh_var()
		    val s = SIGNAT_VAR sigvar
		    val ilctxt = C.add_context_sig (ilctxt, siglab, v, s)
		    val subst = U.subst_add_sigvar(subst,v,s)
		in  (ilctxt,subst)
		end
	in  LabelMap.foldli add_sig acc signats
	end

    fun open_externs (externs : externs,
		      acc : Il.context * U.subst) : Il.context * U.subst =
	let fun add_extern (l, (extvar,c), (ilctxt,subst)) =
		let val v = Name.fresh_var()
		    val ilctxt = C.add_context_extern (ilctxt, l, v, l, c)
		    val subst = U.subst_add_expvar(subst,v,VAR extvar)
		in  (ilctxt,subst)
		end
	in  LabelMap.foldli add_extern acc externs
	end

    fun open_fixity (fixity : fixity, (ilctxt,subst) : Il.context * U.subst)
	    : Il.context * U.subst =
	let val entries = map CONTEXT_FIXITY fixity
	    val ilctxt = C.add_context_entries (ilctxt, entries)
	in  (ilctxt,subst)
	end

    fun open_ovlds (ovlds : ovlds, (ilctxt,subst) : Il.context * U.subst)
	    : Il.context * U.subst =
	let val entries = map CONTEXT_OVEREXP ovlds
	    val ilctxt = C.add_context_entries (ilctxt, entries)
	in  (ilctxt,subst)
	end

    fun open_interface (i : interface,
			acc : Il.context * U.subst) : Il.context * U.subst =
	let val {main=(mainvar,_),signats,other} = i
	    val {fixity,ovlds,externs} = other
	    val acc = open_main (mainvar,acc)
	    val acc = open_signats (signats, acc)
	    val acc = open_externs (externs, acc)
	    val acc = open_fixity (fixity, acc)
	    val acc = open_ovlds (ovlds, acc)
	in  acc
	end

    fun open_imports (ctxt : context, imports) : Il.context * U.subst =
	let val _ = msg "  Opening imports\n"
	    val interfaces = map (lookup ctxt) imports
	    val r = foldl open_interface (#1 ctxt, U.empty_subst) interfaces
	    val _ = if (!ShowContext)
			then (print "\nIL context:\n";
			      Ppil.pp_context (#1 r);
			      print "\n\n")
		    else ()
	in  r
	end handle Fail s => error s

    fun wrap_sdecsopt (ctxt : context, imports,
		       f : Il.context -> sdecs option) : pinterface option =
	let val (localctxt,subst) = open_imports (ctxt, imports)
	    val _ = msg "  Elaborating\n"
	in  Option.map
	    (fn sdecs =>
	     let val sdecs = U.sdecs_subst (sdecs, subst)
		 val entries = map CONTEXT_SDEC sdecs
		 val pinterface = parameterize (ctxt, entries)
		 val _ =
		     if !ShowInterface then
			 (print "\ninterface:\n"; Ppil.pp_pinterface pinterface;
			  print "\n\n")
		     else ()
	     in  pinterface
	     end)
	    (f localctxt)
	end

    fun wrap_decresultopt (ctxt : context, mainlab : label, imports,
			   f : Il.context -> decresult option)
	: (module * pinterface) option =
	let val (localctxt,subst) = open_imports (ctxt, imports)
	    val _ = msg "  Elaborating\n"
	in  Option.map
	    (fn decresult =>
	     let val decresult = U.decresult_subst (decresult, subst)
		 val entries = map #2 decresult
		 val pinterface = parameterize (ctxt, entries)
		 val (sbnd,sdec) = factor_decresult (mainlab,decresult)
		 val module = (#1 ctxt, sbnd, sdec)
		 val _ =
		     if !ShowInterface then
			 (print "\ninterface:\n"; Ppil.pp_pinterface pinterface;
			  print "\n\n")
		     else ()
		 val _ =
		     if !ShowHIL then
			 (print "\nmodule:\n"; Ppil.pp_module module;
			  print "\n\n")
		     else ()
	     in  (module, pinterface)
	     end)
	    (f localctxt)
	end

    fun decresult (sbnds, sdecs) : decresult =
	Listops.zip (map SOME sbnds) (map CONTEXT_SDEC sdecs)

    fun wrap_decresult (ctxt : context, mainlab : label, imports,
			f : Il.context -> sbnds * sdecs) : module * pinterface =
	valOf (wrap_decresultopt (ctxt, mainlab, imports, SOME o decresult o f))

    fun tiltprim (ctxt : context, mainlab : label,
		  imports) : module * pinterface =
	wrap_decresult (ctxt, mainlab, imports, Basis.tiltprim)

    fun elab_dec (ctxt : context, mainlab : label, imports, filepos,
		  dec : Ast.dec) : (module * pinterface) option =
	wrap_decresultopt (ctxt, mainlab, imports,
			   fn ilctxt =>
			   Toil.xdec (ilctxt, filepos, dec))

    fun elab_specs (ctxt : context, imports, filepos,
		    specs : Ast.spec list) : pinterface option =
	wrap_sdecsopt (ctxt, imports,
		       fn ilctxt =>
		       Toil.xspecs (ilctxt, filepos, specs))

    val add_unit    = Stats.timer("Elaboration:add_unit",add_unit)
    val instantiate = Stats.timer("Elaboration:instantiate",instantiate)
    val eq          = Stats.timer("Elaboration:eq",eq)
    val seal        = Stats.timer("Elaboration:seal",seal)
    val elab_dec    = Stats.timer("Elaboration:elab_dec",elab_dec)
    val elab_specs  = Stats.timer("Elaboration:elab_specs",elab_specs)
end
