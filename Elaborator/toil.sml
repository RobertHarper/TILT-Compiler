(* todo : LetExp and CaseExp: valuability coputation too conservative
          optimize coercion functors to recognize when it is entirely unndeeded 
          optimize coercion functions of polymorphic values to be identity by normalizing type argument positions
*)

(* The translation from EL to IL *)
functor Toil(structure Il : IL
	     structure IlStatic : ILSTATIC
	     structure IlUtil : ILUTIL
	     structure Ppil : PPIL
	     structure IlContext : ILCONTEXT
	     structure Pat : PAT
	     structure AstHelp : ASTHELP
	     structure InfixParse : INFIXPARSE
	     structure Datatype : DATATYPE
	     sharing Datatype.IlContext = IlContext
	     sharing IlContext.Il = InfixParse.Il = Pat.Il = Ppil.Il 
	       = IlUtil.Il = IlStatic.Il = Il)
   : TOIL =  
  struct

    structure Il = Il
    open AstHelp Il IlStatic IlUtil Ppil Pat
    open Util Listops Name IlContext Tyvar
    open Prim

fun Sig_IsSub arg = IlStatic.Sig_IsSub arg handle e => false
fun sub_con arg = IlStatic.sub_con arg handle e => false
fun eq_con arg = IlStatic.eq_con arg handle e => false
fun soft_eq_con arg = IlStatic.soft_eq_con arg handle e => false
fun con_normalize (arg as (ctxt,con)) = IlStatic.con_normalize arg handle e => con
fun con_head_normalize (arg as (ctxt,con)) = IlStatic.con_head_normalize arg handle e => con
	    fun eq_modproj (MOD_VAR v, MOD_VAR v') = eq_var (v,v')
	      | eq_modproj (MOD_PROJECT (m,l), MOD_PROJECT (m',l')) = eq_label(l,l') andalso eq_modproj(m,m')
	      | eq_modproj _ = false
	    fun eq_conproj (CON_VAR v, CON_VAR v') = eq_var (v,v')
	      | eq_conproj (CON_MODULE_PROJECT (m,l), 
			    CON_MODULE_PROJECT (m',l')) = eq_label(l,l') andalso eq_modproj(m,m')
	      | eq_conproj _ = false

    val parse_error = fn s => error "toil.sml: parse impossibility" s
    val pat_error = fn s => error "toil.sml: pattern impossibility" s
    val elab_error = fn s => error "toil.sml: elaborator impossibility" s
    val error = fn s => error "toil.sml" s
    val debug = ref false
    val debug_coerce = ref false
    val debug_coerce_full = ref false
    fun debugdo t = if (!debug) then (t(); ()) else ()
    fun debugdo' t = (t(); ())
    fun nada() = ()

    val tyvar_counter = ref (Stats.counter "Elaborator Tyvars")
    fun reset_counters() = (tyvar_counter := Stats.counter "toil.fresh_tyvar")
    val fresh_con = fn ctxt => ((!tyvar_counter)(); fresh_con ctxt)
    val fresh_tyvar = fn ctxt => ((!tyvar_counter)(); fresh_tyvar ctxt)
    val fresh_named_tyvar = fn arg => ((!tyvar_counter)(); fresh_named_tyvar arg)
    fun mk_eq_con c = CON_ARROW([con_tuple[c,c]],
				con_bool,false, oneshot_init PARTIAL)

    type tyvar = (context,con) Tyvar.tyvar
    type ocon = (context,con) Tyvar.ocon
    type decresult = (sbnd option * context_entry) list
    type filepos = Ast.srcpos -> string * int * int
    exception NoEqExp





    (*  --------- Elaboration state contains: -------------------------
        overload table, eq table, flex tables, 
	source position stack, error state
        -------------------------------------------------------------- *)
    datatype ErrorLevel = NoError | Warn | Error
    type region = Ast.srcpos * Ast.srcpos
    local 
      val tyvar_table = ref([] : tyvar list)
      val overload_table = ref ([] : (region * ocon) list)
      val eq_table = ref ([] : (region * context * tyvar * exp Util.oneshot) list)
      val eq_stack = ref ([] : (region * dec list * context * tyvar * exp Util.oneshot) list list)
      val flex_table = ref ([] : (label * flexinfo ref * con * exp Util.oneshot) list)
      val error_level = ref NoError
      val src_region = ref ([] : region list)
      fun nofilepos (_ : SourceMap.charpos) = ("nofilepos",0,0)
      val filepos = ref nofilepos
      fun error_max (Error,_) = Error
	| error_max (_,Error) = Error
	| error_max (Warn,_) = Warn
	| error_max (_,Warn) = Warn
	| error_max (NoError,NoError) = NoError
    in
	fun reset_eq() = (eq_table := []; eq_stack := [])
	fun reset_elaboration fp = (reset_counters();
				    tyvar_table := [];
				    overload_table := [];
				    flex_table := [];
				    reset_eq();
				    error_level := NoError;
				    src_region := [];
				    filepos := fp)
	    
	fun get_elaboration_error() = !error_level

	fun push_region p = src_region := (p :: (!src_region))
	fun pop_region () = src_region := (tl(!src_region))
	fun peek_region () = (hd(!src_region))

	fun peek_region_str () : string = let val (p1,p2) = (hd(!src_region))
					  val fp = !filepos
					  val (f1,r1,c1) = fp p1
					  val (f2,r2,c2) = fp p2
				      in f1 ^ ":" ^ 
					  ((Int.toString r1) ^ "." ^ (Int.toString c1)) ^ "-" ^ 
					  ((Int.toString r2) ^ "." ^ (Int.toString c2))
				      end handle _ => "unknown"
	fun error_region_string() = ("Error at " ^ (peek_region_str()) ^ ", ")
	fun warn_region_string() = ("Warning at " ^ (peek_region_str()) ^ ", ")
	fun error_region() = (error_level := error_max(!error_level,Error);
			      print (error_region_string()))
	fun warn_region() = (error_level := error_max(!error_level,Warn);
			      print (warn_region_string()))
	fun spaces n = 
	    let fun loop 0 = []
		  | loop n = #" "::(loop (n-1))
	    in String.implode(loop n)
	    end
	val tab = spaces 10
	fun tab_region() = print tab

	fun get_tyvar_table () = !tyvar_table
	fun add_tyvar_table tv = (tyvar_table := tv :: (!tyvar_table))

	fun get_overload_table () = !overload_table
	fun add_overload_entry ocon = (overload_table := (peek_region(),ocon)::(!overload_table))

	fun get_flex_table () = !flex_table
	fun add_flex_entry (l,rc,fc,e) = flex_table := (l,rc,fc,e)::(!flex_table)
	    
	fun get_eq_table () = (case !eq_stack of
				   [] => !eq_table
				 | _ => error "get_eq_table called when eq_stack non-empty")
	fun add_eq_entry (tyvar,expos) = 
	    (case (!eq_stack) of
		 [] => elab_error "cannot add entry: empty eq_stack"
	       | (first::rest) => let val ctxt = (case (tyvar_getctxts tyvar) of
						      [] => elab_error "tyvar must have some context"
						    | a::_ => a)
				      val first' = (peek_region(),[],ctxt,tyvar,expos)::first
				      val _ = debugdo (fn () => 
						       (print "add_eq_entry called with length first = ";
							print (Int.toString (length first));
							print "\n"))
				  in (eq_stack := first'::rest)
				  end)
	fun eq_table_push() = (debugdo (fn () => (print "EQ_PUSHING depth = "; 
						  print (Int.toString (length (!eq_stack)));
						  print "\n"));
			       eq_stack := [] :: (!eq_stack))
	fun eq_table_pop dec = 
	    let val _ = debugdo (fn () => (print "EQ_POPPING depth = "; 
					    print (Int.toString (length (!eq_stack)));
					    print " with dec = ";
					    pp_dec dec; print "\n"))
		val stack = !eq_stack
		fun help dec (reg, extra_decs,context,tyvar,expos) = 
		    (reg, dec :: extra_decs, context, tyvar,expos)
		val stack' = mapmap (help dec) stack
		fun help2 (reg,extra_decs,context,tyvar,expos) = 
		    (reg,foldr (fn (dec,ctxt) => add_context_dec(ctxt,SelfifyDec dec)) context extra_decs,
		     tyvar,expos)
	    in case stack' of
		[] => elab_error "cannot pop: empty eq_stack"
	      | (first :: rest) => 
		    let val _ = (debugdo (fn () => (print "EQ-There are "; 
						    print (Int.toString (length first));
						    print " items in first\n")))
			val first' = map help2 first
		    in  (case rest of
			     [] => (eq_stack := [];
				    eq_table := (map help2 first) @ (!eq_table))
			   | (second::rest') => (eq_stack := (first @ second) :: rest'))
		    end
	    end
    end

    val fresh_tyvar' = fresh_tyvar
    fun fresh_tyvar ctxt = let val tv = fresh_tyvar' ctxt
				val _ = add_tyvar_table tv
			    in  tv
			    end
    fun fresh_con ctxt = CON_TYVAR(fresh_tyvar ctxt)

    fun kind_eq_shape (KIND_INLINE(k1,_),k2) = kind_eq_shape(k1,k2)
      | kind_eq_shape (k1,KIND_INLINE(k2,_)) = kind_eq_shape(k1,k2)
      | kind_eq_shape (k1,k2) = k1 = k2

     (* ----------------- overload_resolver ----------------------- *)
    fun overload_help warn (region,ocon) =
	let val helpers = {hard = fn (c1,c2) => eq_con(empty_context,c1,c2),
			   soft = fn (c1,c2) => soft_eq_con(empty_context,c1,c2)}
	    val _ = push_region region
	val res =  (
(*
	    print "overload_help: internal type is: ";
	    Ppil.pp_con (CON_TYVAR (ocon_deref ocon));
	    print "\n";
*)
	    case (ocon_constrain ocon) of
		[] => (error_region();
		       print "overloaded type: none of the constraints are satisfied\n";
		       true)
	      | [pos] => true
	      | pos::_ => (if warn 
			       then 
				   (error_region(); 
				    print "Warning: more than one constraint satisfied by overloaded type")
			   else ();
			       false))
	    val _ = pop_region()
	in res
	end


     (* ----------------- Substiution Helper Functions ----------------------- *)
    local
	type mapping = (con * con) list * (mod * mod) list * 
	    ((con * con) list Name.LabelMap.map) * (con Name.VarMap.map) *
	    ((mod * mod) list Name.LabelMap.map) * (mod Name.VarMap.map)

	fun chandle (_,_,clmap,cvmap,_,_) (CON_VAR v) = Name.VarMap.find(cvmap,v) 
	  | chandle (_,_,clmap,cvmap,_,_) (c as CON_MODULE_PROJECT(_,l)) = 
	    (case Name.LabelMap.find(clmap,l) of
		 NONE => NONE
	       | SOME (cclist) => assoc_eq(eq_conproj, c, cclist))
	  | chandle _ _ = NONE

	fun mhandle (_,_,_,_,mlmap,mvmap) (MOD_VAR v) = Name.VarMap.find(mvmap,v) 
	  | mhandle (_,_,_,_,mlmap,mvmap) (m as MOD_PROJECT(_,l)) = 
	    (case Name.LabelMap.find(mlmap,l) of
		 NONE => NONE
	       | SOME (mmlist) => assoc_eq(eq_modproj, m, mmlist))
	  | mhandle _ _ = NONE

	fun sdechandle mapping (SDEC(l,DEC_CON(v,k,SOME c))) =
	    (case chandle mapping c of
		 NONE => NONE
	       | SOME (CON_VAR v') => 
		     if (Name.eq_var(v,v'))
			 then SOME(SDEC(l,DEC_CON(v,kind_substconmod(k,mapping),SOME c)))
		     else NONE
	       | _ => NONE)
	  | sdechandle _ _ = NONE

	and kind_substconmod(KIND_INLINE(k,c),mapping) = KIND_INLINE(k,con_substconmod(c,mapping))
	  | kind_substconmod(k,_) = k
	and con_substconmod(c,mapping) = con_all_handle(c, fn _ => NONE, chandle mapping, mhandle mapping,
							sdechandle mapping)
	and sig_substconmod(s,mapping) = sig_all_handle(s, fn _ => NONE, chandle mapping, mhandle mapping,
							sdechandle mapping)
    in
	type mapping = mapping
	val empty_mapping = ([],[],Name.LabelMap.empty,Name.VarMap.empty,
			     Name.LabelMap.empty,Name.VarMap.empty)
	fun getcclist((cclist,_,_,_,_,_) : mapping) = cclist
	fun getmmlist((_,mmlist,_,_,_,_) : mapping) = mmlist
	fun join_maps((cclist1,mmlist1,cllist1,cvlist1,mllist1,mvlist1) : mapping,
		      (cclist2,mmlist2,cllist2,cvlist2,mllist2,mvlist2) : mapping) : mapping = 
	    (cclist1 @ cclist2, mmlist1 @ mmlist2,
	     Name.LabelMap.unionWith (op @) (cllist1,cllist2),
	     Name.VarMap.unionWith (fn (x,y) => x) (cvlist1,cvlist2),
	     Name.LabelMap.unionWith (op @) (mllist1,mllist2),
	     Name.VarMap.unionWith (fn (x,y) => y) (mvlist1,mvlist2))
	fun convar_addmap(v,c,(cclist,mmlist,clmap,cvmap,mlmap,mvmap)) = 
	    let val cclist = (CON_VAR v, c)::cclist
		val cvmap = Name.VarMap.insert(cvmap,v,c)
	    in  (cclist,mmlist,clmap,cvmap,mlmap,mvmap)
	    end
	fun modvar_addmap(v,m,(cclist,mmlist,clmap,cvmap,mlmap,mvmap)) = 
	    let val mmlist = (MOD_VAR v, m)::mmlist
		val mvmap = Name.VarMap.insert(mvmap,v,m)
	    in  (cclist,mmlist,clmap,cvmap,mlmap,mvmap)
	    end
	fun conproj_addmap(CON_MODULE_PROJECT(m,l),c,(cclist,mmlist,clmap,cvmap,mlmap,mvmap)) = 
	    let val pair = (CON_MODULE_PROJECT(m,l), c)
		val cclist = pair::cclist
		val temp = pair::(case Name.LabelMap.find(clmap,l) of
				      NONE => []
				    | SOME ccs => ccs)
		val clmap = Name.LabelMap.insert(clmap,l,temp)
	    in  (cclist,mmlist,clmap,cvmap,mlmap,mvmap)
	    end
	  | conproj_addmap _ = error "conproj_addmap not given a CON_MODULE_PROJECT"
	fun modproj_addmap(MOD_PROJECT(m,l),m',(cclist,mmlist,clmap,cvmap,mlmap,mvmap)) = 
	    let val pair = (MOD_PROJECT(m,l), m')
		val mmlist = pair::mmlist
		val temp = pair::(case Name.LabelMap.find(mlmap,l) of
				      NONE => []
				    | SOME mms => mms)
		val mlmap = Name.LabelMap.insert(mlmap,l,temp)
	    in  (cclist,mmlist,clmap,cvmap,mlmap,mvmap)
	    end
	  | modproj_addmap _ = error "modproj_addmap not given a MOD_PROJECT"
	val kind_substconmod = kind_substconmod
	val con_substconmod = con_substconmod
	val sig_substconmod = sig_substconmod
    end

     (* ----------------- Helper Functions ----------------------- *)
    fun is_non_const context (syms : Symbol.symbol list) = 
	(syms = [Symbol.varSymbol "ref"]) orelse
	(case (Datatype.constr_lookup context syms) of
	     NONE => (case Datatype.exn_lookup context syms of
			  NONE => false
			| SOME {stamp,carried_type=NONE} => false
			| SOME {stamp,carried_type=SOME _} => true)
	   | (SOME {name,datatype_path,is_const,datatype_sig}) => not is_const)
    fun parse_pats context pats = 
	(case InfixParse.parse_pat(fixity context,is_non_const context, pats) of
	     SOME result => result
	   | NONE => (error_region();
		      print "cannot parse pattern\n";
		      error "cannot parse pattern\n"))
    fun parse_pat context pat = (case parse_pats context [pat] of
				     [pat] => pat
				   | _ => error "parse_pat getting back more than 1 pat")


    fun dummy_type(context,str) = fresh_named_con(context,str)
    fun dummy_exp (context,str) =
	let val c = fresh_named_con(context,str)
	    val e = RAISE(c,EXN_INJECT("elab_fail",NEW_STAMP con_unit,RECORD[]))
	in (e,c,true)
	end


     fun add_inline_module (context,label,var,module,signat) =
	 case (make_inline_module (context,module,NONE,true)) of
	     SOME norm_mod =>
		 let
		     val _ = (print "original module is:\n";
			      pp_mod module;
			      print "result of inlinemodule is:\n";
			      pp_mod norm_mod;
			      print "\n")
		     val signat = GetModSig(context,norm_mod)
		     val inline = INLINE_MODSIG(norm_mod, SelfifySig(SIMPLE_PATH var,signat))
		 in  add_context_inline(context,label,var,inline)
		 end
	   | NONE => add_context_mod(context,label,var,SelfifySig(SIMPLE_PATH var, signat))


     fun sbnd_ctxt_list2modsig (sbnd_ctxt_list : (sbnd option * context_entry) list) 
	 : (mod * signat) = 
	 let fun loop (acc1,acc2) arg = 
	     (case arg of 
		  [] => (acc1,acc2)
		| ((NONE,_)::rest) => loop (acc1,acc2) rest
		| ((SOME sbnd,CONTEXT_SDEC sdec)::rest) => loop (sbnd::acc1,sdec::acc2) rest
		| ((SOME sbnd,_)::_) => elab_error "sbnd_ctxt_list2modsig: sbnd without sdec")
	     val (sbnds,sdecs) = loop ([],[]) sbnd_ctxt_list
	 in (MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE (NONE,sdecs))
	 end

     and boolsbnd_ctxt_list2modsig (boolsbnd_ctxt_list : ((bool * sbnd) option * context_entry) list) 
	 : (mod * signat) = sbnd_ctxt_list2modsig(map (fn (SOME(_,sbnd),ctxt) => (SOME sbnd,ctxt)
                                                        | (NONE,ctxt) => (NONE,ctxt)) boolsbnd_ctxt_list)

     and add_context_sbnd_ctxts(context,sbnd_ce_list) : sbnd list * context = 
	 add_context_boolsbnd_ctxts(context,map 
				    (fn (NONE,ce) => (NONE,ce)
				  | (SOME sbnd,ce) => (SOME(false,sbnd),ce)) sbnd_ce_list)

     and add_context_boolsbnd_ctxts 
	 (context : context,
	  boolsbnd_ctxt : ((bool * sbnd) option * context_entry) list) : sbnd list * context = 
	 let 
	     fun loop (sbnds,ctxt) arg = 
		 case arg of
		     [] => (rev sbnds,ctxt)
		   | ((NONE,CONTEXT_SIGNAT(l,v,s))::rest) => 
			 loop (sbnds,add_context_sig(ctxt,l,v,s)) rest
		   | ((NONE,CONTEXT_SDEC (SDEC(l,dec)))::rest) => 
			 loop (sbnds,add_context_sdec(ctxt,SDEC(l,SelfifyDec dec))) rest
		   | ((NONE,ce as CONTEXT_INLINE _)::rest) => 
			 loop (sbnds, add_context_entries(context, [ce])) rest
		   | ((NONE,cf as (CONTEXT_FIXITY _))::rest) => 
			 loop (sbnds,add_context_entries(ctxt,[cf])) rest
		   | ((SOME (flag,sbnd as SBND(l,bnd)), CONTEXT_SDEC (sdec as SDEC(l',dec)))::rest) => 
			 let 
			     val sbnds' = sbnd::sbnds
			     val ctxt' = 
				 (case (flag,bnd,dec) of
				      (true,BND_MOD(v,m),DEC_MOD(v',s)) => 
					  if (eq_label(l,l') andalso (eq_var(v,v')))
					      then add_inline_module(ctxt,l,v,m,
								     SelfifySig(SIMPLE_PATH v,s))
					  else elab_error "add_context_boolsbnd_ctxts: inconsistent sbnd_sdeclist"
				    | _ => add_context_sdec(ctxt,SDEC(l',SelfifyDec dec)))
			 in
			     loop (sbnds',ctxt') rest 
			 end
		   | ((SOME _,_)::rest) => elab_error "add_context_boolsbnd_ctxts: cannot have sbnd without sdec"
	 in loop ([],context) boolsbnd_ctxt
	 end

     (* -- translate and package a list of objects using an object translator 
           inputs  : a translator that takes objects to a list of
	               (optional) bindings and context-entries
		     the bool in the binding pair indicates whether
		       it should be inlined
                     a context
                     a list of objects
           outputs : a list of (optional) bindings and context-entries
     *)
     and packagedecs (xobj : context * 'a -> ((bool * sbnd) option * context_entry) list)
         context (objs : 'a list) : (sbnd option * context_entry) list = 
         let 
             fun loop context [] = []
               | loop context [obj] = xobj(context,obj)
               | loop context (obj::rest) =
                 let 
                     val boolsbnd_ctxt_list = xobj(context,obj)
                     val (_,context') = add_context_boolsbnd_ctxts(context,boolsbnd_ctxt_list)
                     val boolsbnd_ctxt_restlist = loop context' rest
                 in boolsbnd_ctxt_list @ boolsbnd_ctxt_restlist
                 end
             val boolsbnd_ctxt_list = loop context objs
             fun maxmap_folder((NONE,_),map) = map
               | maxmap_folder((SOME(_,SBND(l,_)),_),map) = 
                 (case Name.LabelMap.find(map,l) of
                     SOME r => (r := 1 + (!r);
                                map)
                   | NONE => Name.LabelMap.insert(map,l,ref 1))
             val maxmap = foldl maxmap_folder Name.LabelMap.empty boolsbnd_ctxt_list 
             fun uniquify [] = []
               | uniquify (boolsbnd_ctxt::rest) = 
                 (case boolsbnd_ctxt of
                      (NONE,ce) => (NONE,ce) :: (uniquify rest)
                    | (SOME(_,sbnd as (SBND(l,bnd))),
                       ce as (CONTEXT_SDEC(SDEC(_,dec)))) => 
                          (case (Name.LabelMap.find(maxmap,l)) of
                              NONE => elab_error "maxmap must have entry at this point"
                            | SOME r =>
                                  (r := (!r) - 1;
                                   if (!r = 0)
                                       then (SOME sbnd, ce) :: (uniquify rest)
                                   else if (!r < 0)
                                            then elab_error "maxmap count inconsistency"
                                        else (* rename case *)
                                            let val l' = fresh_internal_label "hidden"
                                            in (SOME (SBND(l',bnd)), 
                                                CONTEXT_SDEC(SDEC(l',dec)))
                                                :: (uniquify rest)
                                            end))
                    | (SOME(_,sbnd), _) => elab_error "packagedec: got sbnd without CONTEXT_SDEC")
         in uniquify boolsbnd_ctxt_list 
         end



    (* --------------------------------------------------------- 
      ------------------ EXPRESSIONS --------------------------
      --------------------------------------------------------- *)
    (* ----- Polymorphic (sdec) Instantiation ------------------------ 
       given a list of sdecs, return a triple consisting of
      (1) sbnds with the fresh types or already available types
      (2) the same sdecs except replace each opaque type with a fresh type
      (3) a list of the the fresh or already present types 
      ---------------------------------------------------------------- *)
     fun poly_inst (ctxt,sdecs) : sbnds * sdecs * con list = 
       let
	   fun help sdecs =
	       (case sdecs of
		    [] => ([],[],[])
		  | (SDEC(l1,DEC_CON(v1,k1,copt)) :: SDEC(l2,DEC_EXP(v2,c2)) :: rest) =>
			let 
			    val (con,eq_con,e2) = 
				(case copt of
				     NONE => let 
						 val tyvar = fresh_named_tyvar (ctxt,"eq_tyvar")
						 val _ = tyvar_use_equal tyvar
						 val con = CON_TYVAR tyvar
						 val exp_os = oneshot()
						 val eq_con = CON_ARROW([con_tuple[con,con]],
									con_bool,
									false,
									oneshot_init PARTIAL)
						 val _ = add_eq_entry(tyvar,exp_os)
						 val e2 = OVEREXP(eq_con,true,exp_os)
					     in (con,eq_con,e2)
					     end
				   | SOME con => let val e2 = xeq(ctxt,con)
						     val eq_con = CON_ARROW([con_tuple[con,con]],
									    con_bool,
									    false,
									    oneshot_init PARTIAL)
						 in (con,eq_con,e2)
						 end)

			    val (sbnds,sdecs,cons) = help rest
			    val sbnds' = (SBND(l1,BND_CON(v1,con))) ::
					  (SBND(l2,BND_EXP(v2,e2))) :: sbnds
			    val sdecs' = (SDEC(l1,DEC_CON(v1,k1,SOME con)))::
					  (SDEC(l2,DEC_EXP(v2,c2))) :: sdecs
			    val cons' = con :: cons
			in (sbnds',sdecs',cons')
			end
		  | (SDEC(l,DEC_CON(v,k,NONE))::rest) =>
			let val c = fresh_con ctxt
			    val (sbnds,sdecs,cons) = help rest
			in ((SBND(l,BND_CON(v,c)))::sbnds,
			    (SDEC(l,DEC_CON(v,k,SOME c)))::sdecs,
			    c::cons)
			end
		  | (SDEC(l,DEC_CON(v,k,SOME c))::rest) =>
			let val (sbnds,sdecs,cons) = help rest
			in ((SBND(l,BND_CON(v,c)))::sbnds,
			    (SDEC(l,DEC_CON(v,k,SOME c)))::sdecs,
			    c::cons)
			end
		  | _ => (print "poly_inst got strange sdecs:\n";
			  pp_sdecs sdecs;
			  elab_error "poly_inst received strange sdecs"))
       in help sdecs
       end


    (* ---------- Polymorphic function Instantiation ------------------ *)
    and polyfun_inst (context, module : mod, s : signat) : exp * con = 
       let 
	 val _ = debugdo (fn () => (print "polyfun_inst called with module:\n";
				     pp_mod module; print "\nand sig = \n";
				     pp_signat s; print "\n\n"))
	 val (e,c) = polyfun_inst' (context,module,s)
	 val _ = debugdo (fn () => (print "and returning e =\n";
				    pp_exp e; print "\nand c =\n";
				    pp_con c; print "\n"))
       in (e,c)
       end
    and polyfun_inst' (context, module : mod, s : signat) : exp * con = 
       (case s of 
	  (SIGNAT_FUNCTOR(v,SIGNAT_STRUCTURE (_,arg_sdecs), 
			  SIGNAT_STRUCTURE (_,[SDEC(_,DEC_EXP(resv,resc))]),_)) =>
	  let 
	      val _ = debugdo (fn () => (print "polyfun_inst' got module of:\n";
					 Ppil.pp_mod module;
					 print "\n\n  and signature of:\n";
					 Ppil.pp_signat s;
					 print "\n\n"))
	    local 
		fun dotype l v = let val tv = fresh_tyvar context
				     val c = CON_TYVAR tv
				 in (tv,(SBND(l,BND_CON(v,c)),
					 SDEC(l,DEC_CON(v,KIND_TUPLE 1, SOME c)))
(*					 SDEC(l,DEC_CON(v,KIND_TUPLE 1, NONE)), *)
				     )
				 end
		fun help ([] : sdecs) = []
	          | help [SDEC(l,DEC_CON(v,_,_))] = [#2(dotype l v)]
		  | help (SDEC(l,DEC_CON(v,_,_)) :: 
			  (rest as (SDEC(_,(DEC_CON _)) :: _))) = (#2(dotype l v))::(help rest)
		  | help (SDEC(l1,DEC_CON(v1,_,_)) :: SDEC(l2,DEC_EXP(v2,c2)) :: rest) =
		    let 
			val (tyvar,(sbnd1,sdec1)) = dotype l1 v1
			val _ = tyvar_use_equal tyvar
			val con = CON_TYVAR tyvar
			val eq_con = CON_ARROW([con_tuple[con,con]],con_bool,false,oneshot_init PARTIAL)
			val exp_os = oneshot()
			val _ = add_eq_entry(tyvar,exp_os)
			val eqexp = OVEREXP(eq_con,true,exp_os)
			val sbnd2 = SBND(l2,BND_EXP(v2,eqexp))
			val sdec2 = SDEC(l2,DEC_EXP(v2,c2))
		    in (sbnd1,sdec1) :: (sbnd2,sdec2) :: (help rest)
		    end
		  | help _ = elab_error "unexpected sig to arg struct of polymorphic fun"
		val temp = help arg_sdecs
	    in 
	      val mod_poly = MOD_STRUCTURE(map #1 temp)
	      val signat_poly_sdecs = map #2 temp
	      val signat_poly = SIGNAT_STRUCTURE (NONE,signat_poly_sdecs)
	    end
	    val new_rescon : con = remove_modvar_type(resc,v,signat_poly_sdecs)
	    val _ = debugdo (fn () => (print "\n*********\nabout to make_non_dependent_type with resc = \n";
				       pp_con resc;
				       print "\n"))
	    val signat_temp = SIGNAT_STRUCTURE(NONE,[SDEC
						     (it_lab,
						      DEC_EXP(resv,new_rescon))])
	    val _ = debugdo (fn () => (print "\n*********\ndone make_non_dependent_type with signat_temp:\n";
				       pp_signat signat_temp; print "\n\n"))
	    val signat'= SIGNAT_FUNCTOR(v,signat_poly,signat_temp,PARTIAL)
	    val _ = if (Sig_IsSub(context,s,signat')) then ()
		    else (print "s is\n";
			  pp_signat s; print "\nsignat' is \n";
			  pp_signat signat'; print "\n";
			  elab_error "Failed rule 24 Sig_IsSub")
	    val exp = (case (mod_poly,module) of
			   (MOD_STRUCTURE sbnds,
			    MOD_FUNCTOR(v,SIGNAT_STRUCTURE sdecs,MOD_STRUCTURE[SBND(it_maybe,BND_EXP(_,e))])) =>
			       if (eq_label(it_lab,it_maybe))
				   then
				       let fun table_entry (SBND(l,BND_CON(_,c))) = (l,c)
					     | table_entry _ = error "bad functor application"
					   val table = map table_entry sbnds
					   fun eproj _ = NONE
					   fun cproj (MOD_VAR v', l) = 
					       if (eq_var(v,v')) 
						   then assoc_eq(eq_label,l,table)
					       else NONE
					     | cproj _ = NONE
				       in  exp_subst_proj(e,eproj,cproj)
				       end
			       else MODULE_PROJECT(MOD_APP(module,mod_poly), it_lab)
			 | _ => MODULE_PROJECT(MOD_APP(module,mod_poly), it_lab))
	    val _ = debugdo (fn () => (print "polyfun_inst returning exp :\n";
				       pp_exp exp; print "\n\n  and rescon:\n";
				       pp_con new_rescon; print "\n\n"))
	  in (exp,new_rescon)
	  end
       |  _ =>  (print "polyfun_inst received s of\n";
		 pp_signat s;
		 print "\n";
		 elab_error "rule 224 or 226 not applicable in polyfun_inst"))

	  
     and xexp (context : context, exp : Ast.exp) : (exp * con * bool) = (* returns valuablilty *)
      (case exp of
	 Ast.IntExp lit => (SCON(int(W32,lit)), CON_INT W32, true)
       | Ast.WordExp lit => (SCON(uint(W32,lit)), CON_UINT W32, true)
       | Ast.RealExp s => (SCON(float(F64,s)), CON_FLOAT F64, true)
       | Ast.StringExp s => 
	     (SCON(vector (CON_UINT W8,
			   Array.fromList
			   (map (fn c => SCON(uint(W8,TilWord64.fromInt (ord c))))
			    (explode s)))), con_string, true)
       | Ast.CharExp s =>   
	     (SCON(uint(W8,
			(case (explode s) of
			     [c] => TilWord64.fromInt (ord c)
			   | _ => parse_error "Ast.CharExp carries non-charcter string"))),
	      CON_UINT W8, true)
       | (Ast.TupleExp _ | Ast.RecordExp _) =>
	     let val (sorted, sym_expr_list) = 
		 (case exp of
		      Ast.TupleExp exps => 
			  (length exps < 10,
			   mapcount (fn(n,a) => (generate_tuple_symbol (n+1),a)) exps)
		    | Ast.RecordExp sym_exps => (false,sym_exps)
		    | _ => pat_error "must have a TupleExp or RecordExp here")
		 fun doer((sym,expr),(acc,va_acc)) = 
		     let val label = symbol_label sym
			 val (exp,con,va) = xexp(context,expr)
		     in ((label,(label,exp),(label,con))::acc,va andalso va_acc)
		     end
		 val (label_rbnd_rdec,va) = foldr doer ([],true) sym_expr_list 
		 val sorted = sorted orelse (label_issorted (map #1 label_rbnd_rdec))
	     in
		 if sorted 
		     then (RECORD(map #2 label_rbnd_rdec), CON_RECORD(map #3 label_rbnd_rdec),va)
		 else
		     let
			 fun make_var(l,rb,rd) = (l,(fresh_named_var (label2string l),rb,rd))
			 val label_var_rbnd_rdec = map make_var label_rbnd_rdec
			 val bnds = map (fn (_,(v,(_,e),_)) => BND_EXP(v,e)) label_var_rbnd_rdec
			 val label_var_rbnd_rdec = sort_labelpair label_var_rbnd_rdec
			 val con = CON_RECORD(map (fn (_,(_,_,rd)) => rd) label_var_rbnd_rdec)
			 val body = RECORD(map (fn (l,(v,_,_)) => (l,VAR v)) label_var_rbnd_rdec)
		     in (LET(bnds,body), con, va)
		     end
	     end
       | Ast.ListExp args => 
	     let fun loop [] = AstHelp.nil_exp
		   | loop (a::b) = Ast.AppExp{function=AstHelp.cons_exp,
					      argument=Ast.TupleExp[a,loop b]}
	     in xexp(context,loop args)
	     end
       | Ast.SelectorExp s => 
     (* for the record, i'd like to say that this construct is a pain in the butt - Perry *)
	     let 
		 val label = symbol_label s
		 val stamp = get_stamp()
		 val fieldcon = fresh_con context
		 val the_ref = ref(FLEXINFO(stamp,false,[(label,fieldcon)]))
		 val rescon = CON_ARROW([CON_FLEXRECORD the_ref],
					fieldcon, false, oneshot_init PARTIAL)
		 val eshot : exp Util.oneshot = oneshot()
		 val exp = OVEREXP(rescon,true,eshot)
		 val _ = add_flex_entry(label,the_ref,fieldcon,eshot)
	     in (exp,rescon,true)
	     end
       | Ast.VarExp path => 
	     let fun unbound() = 
		 let val _ = (error_region(); 
			      print "unbound variable or constructor: ";
			      AstHelp.pp_path path;
			      print "\n")
		 in dummy_exp(context,"unbound_var")
		 end
		 fun eqcase iseq = 
		     let val exp_os = oneshot()
			 val tyvar = fresh_named_tyvar (context,"teq")
			 val _ = tyvar_use_equal tyvar
			 val con = CON_TYVAR tyvar
			 val arg_con = con_tuple[con,con]
			 val eq_con = CON_ARROW([arg_con],con_bool,
						false,oneshot_init PARTIAL)
			 val _ = add_eq_entry(tyvar,exp_os)
			 val eqexp = OVEREXP(eq_con,true,exp_os)
			 val res = if iseq
				       then eqexp
				   else let val v = fresh_named_var "neq_arg"
					in  #1(make_lambda(v,arg_con, con_bool,
							   make_ifthenelse(APP(eqexp,[VAR v]),
									   false_exp,true_exp,con_bool)))
					end
		     in (res,eq_con,true)
		     end
	     in
		 (case (Context_Lookup(context,map symbol_label path)) of
		      SOME(_,PHRASE_CLASS_EXP (e,c)) => (e,c,Exp_IsValuable(context,e))
		    | SOME (_, PHRASE_CLASS_OVEREXP constraint_result) =>
			  let 
			      fun mk_constraint (c,res) (tyvar, is_hard) = 
				  let val c' = CON_TYVAR tyvar
				      val match = if is_hard 
						      then eq_con(context,c,CON_TYVAR tyvar)
						  else soft_eq_con(context,c,CON_TYVAR tyvar)
				      val res = if match
						    then Tyvar.MATCH res
						else Tyvar.FAIL
				  in res
				  end
			      val constraints = Listops.mapcount (fn (n,(con,_)) => mk_constraint (con,n)) constraint_result
			      val results = map #2 constraint_result
			      fun con_thunk exp_oneshot results (index : int) = 
				  let val result : exp = List.nth(results,index)
				  in  (case (oneshot_deref exp_oneshot) of
					   (SOME _) => ()
					 | NONE => oneshot_set(exp_oneshot,result))
				  end
			      val eshot = oneshot()
			      val ocon = Tyvar.uocon_inst (empty_context,
							   Tyvar.fresh_uocon constraints, 
							   con_thunk eshot results)
			      val con = CON_OVAR ocon
			      val exp = OVEREXP(con,true,eshot)
			      val _ = add_overload_entry ocon
			  in (exp,CON_OVAR ocon,Exp_IsValuable(context,exp))
			  end
		      
		      
		    | SOME(_,PHRASE_CLASS_MOD (m,s as SIGNAT_FUNCTOR _)) => 
			  let val (e,c) = polyfun_inst (context,m,s)
			  in  (e,c,true)
			  end
		    | SOME(_,PHRASE_CLASS_MOD (m,(SIGNAT_STRUCTURE(_,([sdec] | [_,sdec]))))) =>
			  (case sdec of
			       SDEC(l,DEC_EXP(_,c)) =>
				   if (eq_label (l,mk_lab))
				       then (MODULE_PROJECT(m,mk_lab),c,true)
				   else unbound()
			     | SDEC(l,DEC_MOD(_,s)) =>
				       if (eq_label(l,mk_lab))
					   then 
					       let val mk_mod = MOD_PROJECT(m,mk_lab)
						   val (e,c) = polyfun_inst(context,mk_mod,s)
					       in  (e,c,true)
					       end
				       else unbound()
			     | _ => unbound())
		    | NONE => if (path = [Symbol.varSymbol "="])
				  then eqcase true
			      else if (path = [Symbol.varSymbol "<>"])
				       then eqcase false
				   else unbound())
	     end
       | Ast.DelayExp expr =>
	     (case (Context_Lookup(context,[symbol_label (Symbol.varSymbol "Susp")]),
		   Context_Lookup(context,[symbol_label (Symbol.tycSymbol "susp")])) of
		 (SOME (_,PHRASE_CLASS_MOD(sm,SIGNAT_FUNCTOR(_,SIGNAT_STRUCTURE(_,[sdec]),_,_))), 
		  SOME(_,PHRASE_CLASS_CON(sc,sk))) =>  
		 (let 
		      val (e,c,va) = xexp(context,expr)
		      local
			  val (type_sbnds,_,new_cons) = poly_inst(context,[sdec])
			  val _ = (case new_cons of
				       [new_con] => if (eq_con(context,new_con,c))
							then ()
						    else elab_error "new_con mus be unset"
				     | _ => elab_error "poly_inst returned diff length list")
			  val type_mod = MOD_STRUCTURE type_sbnds
		      in
			  val wrapper_exp = MODULE_PROJECT(MOD_APP(sm,type_mod),it_lab)
		      end
		      fun make_thunk(c,e) = make_lambda(fresh_named_var "dummy_var",con_unit, c, e) 
		      val ref_arg = fresh_named_var "delay_ref"
		      val value_arg = fresh_named_var "delay_value"
		      val thunk_c = CON_ARROW([con_unit],c,false,oneshot_init PARTIAL)
		      val dummy_fun = #1(make_thunk(c, RAISE(c,bindexn_exp)))
		      val bnd = BND_EXP(ref_arg,PRIM(mk_ref,[thunk_c], [dummy_fun]))
		      val thunk_e = #1(make_thunk(c, APP(PRIM(deref,[thunk_c],[VAR ref_arg]),
							 [unit_exp])))
		      val wrapped_exp = APP(wrapper_exp,[thunk_e])
		      val final_c = CON_APP(sc, c)
		      val inner_body = #1(make_seq[(PRIM(setref,[thunk_c],
						    [VAR ref_arg,
						     #1(make_thunk(c, VAR value_arg))]), con_unit),
						   (VAR value_arg, c)])
		      val assign_exp = PRIM(setref,[thunk_c],
					    [VAR ref_arg,
					     #1(make_thunk(c, LET([BND_EXP(value_arg,e)], inner_body)))])
		      val body = #1(make_seq[(assign_exp, con_unit),(wrapped_exp,final_c)])
		  in  (LET([bnd],body), final_c, va)
		  end)
		| _ => elab_error "constructor #Susp or type #susp not defined in initial basis\n")
       | Ast.LetExp {dec,expr} => 
	     let val boolsbnd_ctxt_list = xdec'(context, dec)
		 val (sbnds,context') = add_context_boolsbnd_ctxts(context,boolsbnd_ctxt_list)
		 val (e,c,va) = xexp(context',expr)
		 val bnds = map (fn (SBND(_,bnd)) => bnd) sbnds
	     in  (LET(bnds,e),c,false) 
	     end
       | Ast.FlatAppExp _ => (case InfixParse.parse_exp(fixity context, exp) of
				  SOME exp' => xexp(context,exp')
				| NONE => (error_region();
					   print "cannot parse FlatAppExp\n";
					   dummy_exp(context,"bad_application")))

       | Ast.CcallExp (function,arguments) => 
	     let val (e,con,va) = xexp(context,function)
		 val e_con_va_args = map (fn e => xexp(context,e)) arguments
		 val arrow_oe =
		     (case con of
			  CON_ARROW(_,_,true,arrow) => arrow
			| _ => (case (con_normalize(context,con)) of
				    CON_ARROW(_,_,true,arrow) => arrow
				  | _ => oneshot()))
		 val spec_rescon = fresh_con context
		 val exp_args = map #1 e_con_va_args
		 val con_args = map #2 e_con_va_args
		 val va_args = map #3 e_con_va_args
		 val spec_funcon = CON_ARROW(con_args,spec_rescon,
					     true,arrow_oe)
	     in
		 if (sub_con(context,spec_funcon,con))
		     then (let val va3 = 
			       (case oneshot_deref arrow_oe of
				      NONE => (oneshot_set(arrow_oe,PARTIAL); false)
				    | SOME PARTIAL => false
				    | SOME TOTAL => true)
			   in  (APP(e,exp_args),
				con_deref spec_rescon,
				va andalso (List.all (fn x => x) va_args) andalso va3)
			   end)
		 else
		     (case (con_normalize(context,con)) of
			 CON_ARROW(argcons,rescon,_,_) => 
			     (error_region(); print " application is ill-typed.\n";
			      print "  Function domain: "; 
			      app (fn c => (pp_con c; print "; ")) argcons;
			      print "\n  Argument type: "; 
			      app (fn c => (pp_con c; print "; ")) con_args;
			      print "\n";
			      dummy_exp(context,"bad_application"))
		       | nonarrow => (error_region(); 
				      print " operator is not a function. Has type:\n";
				      pp_con nonarrow;
				      print "\n";
				      dummy_exp(context,"bad_application")))
	     end

       | Ast.AppExp {argument,function} => 
	     let val (e1',con1,va1) = xexp(context,function)
		 val (e2',con2,va2) = xexp(context,argument)
		 val (closed,arrow_oe) = 
		     (case con1 of
			  CON_ARROW(_,_,closed,arrow) => (closed,arrow)
			| _ => (case (con_normalize(context,con1)) of
				    CON_ARROW(_,_,closed,arrow) => (closed,arrow)
				  | _ => (false,oneshot())))
		 val spec_rescon = fresh_con context
		 val spec_funcon = CON_ARROW([con2],spec_rescon,closed,arrow_oe)
		 fun reduce(x,y) = 
		     (case (IlUtil.beta_reduce(x,y)) of
			 NONE => APP(x,[y])
		       | SOME e => e)
		 fun red (exp as (OVEREXP (c,_,oe))) = 
		     ((case c of 
			   CON_OVAR ocon => overload_help false (peek_region(),ocon)
			 | _ => false);
		      (case (oneshot_deref oe) of
			   SOME exp => exp
			 | NONE => exp))
		   | red exp = exp
	     in
		 if (sub_con(context,spec_funcon,con1))
		     then (let val va3 = (case oneshot_deref arrow_oe of
					      NONE => (oneshot_set(arrow_oe,PARTIAL); false)
					    | SOME PARTIAL => false
					    | SOME TOTAL => true)
			   in  (reduce(red e1',red e2'),con_deref spec_rescon,
				va1 andalso va2 andalso va3)
			   end)
		 else
		     (case (con_normalize(context,con1)) of
			 CON_ARROW([argcon],rescon,_,_) => 
			     (error_region(); print " application is ill-typed.\n";
			      print "  Function domain: "; pp_con argcon;
			      print "\n  Argument type: "; pp_con con2;
			      print "\n";
			      dummy_exp(context,"bad_application"))
		       | nonarrow => (error_region(); 
				      print " operator is not a function. Has type:\n";
				      pp_con nonarrow;
				      print "\n";
				      dummy_exp(context,"bad_application")))
	     end
       | Ast.AndalsoExp (e1,e2) => 
	     xexp(context,Ast.IfExp{test=e1,thenCase=e2,elseCase=AstHelp.false_exp})
       | Ast.OrelseExp (e1,e2) => 
	     xexp(context,Ast.IfExp{test=e1,thenCase=AstHelp.true_exp,elseCase=e2})
       | Ast.IfExp {test,thenCase,elseCase} => 
	     let val pr1 = {pat=true_pat,exp=thenCase}
		 val pr2 = {pat=false_pat,exp=elseCase}
	     in xexp(context,Ast.CaseExp {expr=test, 
					  rules=[Ast.Rule pr1, Ast.Rule pr2]})
	     end
       | Ast.ConstraintExp {expr, constraint} => 
	     let val (exp,con,va) = xexp(context,expr)
		 val con' = xty(context,constraint)
	     in if (sub_con(context,con,con'))
		    then (SEAL(exp,con'),con',va)
		else let val (e,c,_) = dummy_exp(context,"badseal")
		     in  error_region();
			 print "constraint does not match expression type\n";
			 tab_region();
			 print "Expression type:\n"; pp_con con;
			 tab_region();
			 print "\nConstraint type:\n"; pp_con con';
			 print "\n";
			 (SEAL(e,con'),con',va)
		     end
	     end		    
       | Ast.VectorExp expr_list => 
	     let val c = fresh_con context
		 val ecv_list = map (fn e => xexp(context,e)) expr_list
		 val elist = map #1 ecv_list
		 fun folder (_,c',_) = (eq_con(context,c,c') orelse
					(error_region();
					 print "ill-typed vector expression\n";
					 false))
	     in if (andfold folder ecv_list)
		    then (SCON(vector(c,Array.fromList elist)), CON_VECTOR c,
			  andfold (fn (_,_,va) => va) ecv_list)
		else dummy_exp(context, "bad_vector")
	     end
       | Ast.WhileExp {test,expr} => 
	     let 
		 val (teste,testc,_) = xexp(context,test)
		 val (be,bc,_) = xexp(context,expr)
		 val body_ec = (be,bc)
	     in  if (eq_con(context,testc,con_bool)) 
		     then 
			 let val loop_var = fresh_named_var "loop"
			     val arg_var = fresh_named_var "loop_arg"
			     val (then_exp,_) = make_seq[body_ec, (APP(VAR loop_var, [unit_exp]),con_unit)]
			     val loop_body = make_ifthenelse(teste,then_exp,unit_exp,con_unit)
			     val loop_fun = FIX(true,PARTIAL,[FBND(loop_var,arg_var,con_unit,con_unit,loop_body)])
			 in (LET([BND_EXP(loop_var,loop_fun)],APP(VAR loop_var, [unit_exp])),
			     con_unit, false)
			 end
		 else (error_region();
		       print "while construct given a test clause not of boolean type\n";
		       dummy_exp(context,"badwhile"))
	     end
       | Ast.HandleExp {expr,rules} => (* almost same as CaseExp except need to wrap with HANDLE *)
	     let 
		 val (exp',rescon,va) = xexp(context,expr)
		 val v = fresh_named_var "handle_exn"
		 val patarg = {context = context,
			       typecompile = xty,
			       expcompile = xexp,
			       polyinst = poly_inst,
			       error_region = error_region,
			       fresh_con = fresh_con}
		 val arms = map (fn (Ast.Rule{pat,exp})=>(parse_pat context pat,exp)) rules
		 val (hbe,hbc) = caseCompile{patarg = patarg,
					     arms = arms,
					     arg = (v,CON_ANY)}
		 val (he,hc) = make_lambda(v,CON_ANY,hbc,hbe)
	     in if (eq_con(context,rescon,hbc))
		    then (HANDLE(exp',he),rescon,va)
		else (error_region();
		      print "mismatch between handle body and handler\n";
		      dummy_exp(context,"bad_handle"))
	     end
       | Ast.RaiseExp e => 
	     let val (exp,con,_) = xexp(context,e)
		 val c = fresh_con context
	     in if (eq_con(context,con,CON_ANY)) 
		    then (RAISE (c,exp),c,false)
		else (error_region(); print "raise not given an expression of exn type\n";
		      dummy_exp(context,"badraise"))
	     end
       | Ast.SeqExp elist => 
	     let val ecvlist = map (fn e => xexp(context,e)) elist
		 val eclist = map (fn (e,c,v) => (e,c)) ecvlist
		 val (e,c) = make_seq eclist
	     in  (e,c,andfold (fn (e,c,v) => v) ecvlist)
	     end
       | Ast.FnExp [] => parse_error "Ast.FnExp with empty list"
       | Ast.FnExp rules => 
	     let val patarg = {context = context, 
			       typecompile = xty, 
			       expcompile = xexp, 
			       polyinst = poly_inst,
			       error_region = error_region,
			       fresh_con = fresh_con}
		 val arms = map (fn (Ast.Rule{pat,exp}) => (parse_pats context [pat],exp)) rules
		 val {arglist,body} = funCompile{patarg = patarg,
						 rules = arms,
						 reraise = false}
		 fun help ((v,c),(e,resc)) = make_lambda(v,c,resc,e)
		 val (e,c) = foldr help body arglist
	     in  (e,c,true)
	     end
       | Ast.CaseExp {expr,rules} =>  
	     let fun getarm (Ast.Rule{pat,exp}) = (parse_pat context pat,exp)
		 val arms = map getarm rules
		 val (arge,argc,_) = xexp(context,expr)
		 val (context,wrap,v) = (case arge of 
					     VAR v => (context,fn e => e, v)
					   | _ => let val v = fresh_named_var "casearg"
						      fun wrap e = LET([BND_EXP(v,arge)],e)
						  in  (add_context_exp'(context,v,argc),wrap,v)
						  end)
		 val patarg = {context = context,
			       typecompile = xty,
			       expcompile = xexp,
			       polyinst = poly_inst,
			       error_region = error_region,
			       fresh_con = fresh_con}
		 val (e,c) = caseCompile{patarg = patarg,
					 arms = arms,
					 arg = (v,argc)}
	     in (wrap e,c,false)
	     end
       | Ast.MarkExp(exp,region) => 
	     let val _ = push_region region
		 val res = xexp(context,exp)
		 val _ = pop_region()
	     in res 
	     end)


    (* --------------------------------------------------------- 
      ------------------ DECLARATIONS --------------------------
      --------------------------------------------------------- *)
      and make_typearg_sdec [] = []
        | make_typearg_sdec ((type_lab,is_eq) :: more) = 
	 let 
	     val rest = make_typearg_sdec more
	     val type_str = label2string type_lab
	     val type_var = fresh_named_var type_str 
	     val type_sdec = SDEC(type_lab,DEC_CON(type_var, KIND_TUPLE 1, NONE))
	     val eq_lab = to_eq_lab type_lab
	     val eq_str = label2string eq_lab
	     val eq_var = fresh_named_var eq_str
	     val eq_con =  CON_ARROW([con_tuple[CON_VAR type_var, CON_VAR type_var]],
				     con_bool, false, oneshot_init PARTIAL)
	     val eq_sdec = SDEC(eq_lab,DEC_EXP(eq_var, eq_con))
	 in  if (is_eq) then (type_sdec :: eq_sdec :: rest) else type_sdec :: rest
	 end
	
     and xdec' (context : context, d : Ast.dec) : ((bool * sbnd) option * context_entry) list = 
       let 
	   fun strip (Ast.MarkDec(d,r)) = strip d
	     | strip d = d
	   val sbndsdec_list = xdec(context,d)
	   val inlineflag = (case (strip d) of
				 (Ast.DatatypeDec {datatycs,withtycs}) => false
			       | _ => false)
	   fun help (SOME b,d) = (SOME(inlineflag,b),d)
	     | help (NONE, d) = (NONE,d)
       in  map help sbndsdec_list
       end


   and xdatatype (context,datatycs) : (sbnd * sdec) list =
       let val sbnd_sdecs = Datatype.compile{context=context,
					     typecompile=xty,
					     datatycs=datatycs,
					     eq_compile=xeqopt}
	   (* we want to eventually expose all the types;
	    we want to inline all the structures now though *)
	   fun revise ((sbnd as SBND(l,bnd),sdec as SDEC(_,dec)), (context,acc)) = 
	       let val (dec,dec_local) = 
		   (case (bnd,dec) of
			       (BND_CON(v,c),DEC_CON(_,k,copt)) => 
				   let val k' = KIND_INLINE(k,c)
				   in  (DEC_CON(v,k',copt), DEC_CON(v,k',SOME c))
				   end
			     | (BND_MOD(v,MOD_STRUCTURE sbnds),
				   DEC_MOD(_,SIGNAT_STRUCTURE(_,sdecs))) =>
				   let val imp_sdecs = IlStatic.GetSbndsSdecs(context,sbnds)
				       val s' = SIGNAT_INLINE_STRUCTURE{self = NONE,
									code = sbnds,
									abs_sig = sdecs,
									imp_sig = imp_sdecs}
				   in  (DEC_MOD(v,s'), DEC_MOD(v,s'))
				   end
				 | _ => (dec,dec))
		   val context = add_context_sdec(context,SDEC(l,SelfifyDec dec_local))
	       in  (context,(sbnd,SDEC(l,dec))::acc)
	       end
(*
	   val _ = (print "calling revise on sbnd_sdecs. sbnds :\n";
		    Ppil.pp_sbnds (map #1 sbnd_sdecs); print "\n\nsdecs:\n";
		    Ppil.pp_sdecs (map #2 sbnd_sdecs); print "\n\n")
*)
	   val (_,rev_sbnd_sdecs) = foldl revise (context,[]) sbnd_sdecs
       in rev rev_sbnd_sdecs
       end
	   
     and xdec (context : context, d : Ast.dec) : (sbnd option * context_entry) list =
       (case d of
          (* --- The tricky thing about this value declarations is figuring 
	     --- out what type variables need to be generalized.  There are two
             --- sources: those arising explicitly from the program text and those
             --- arising from the type inference process.  
	     --- (1) Source-level Type Variables:
             ---     In the former case, these explicit type variables must be put into
             ---     the translation context before encountering.  This can be done by
             ---     a prepass over the program to explicitly scope these user-level type varaibles.
             ---     In ML96, these there is a syntax for the programmer to give these type variables
             ---     explicit scope.  Nonetheless, since this is optional, we must create the
             ---     (narrowest) scope for those variables that the user did not explicitly scope.
             ---     As a matter of efficiency, it would be better to compute the scope of user-level
             ---     type variables in a prepass rather than repeatedly scan the expression for them.
             --- (2) Unresolved meta-variables(unless constrained) are also eligible for type generalization.
             ---     If a meta-variable is to be generalized, it is resolved to a type variables which is
             ---     then generalized.  Unresolved meta-variables can either appear in the type of the
             ---     expression being bound.  These must be generalized.  Meta-variables that are generated
             ---     and appear in the translated expression but not the translated type may be generalized
             ---     or not.  If it is not generalized however, it MUST be resolved at some point to some      
             ---     well-formed type. 
             --- (3) Note that until pattern matching is done, metavariables that seem to be generalizable
             ---     may not be yet. *)
	  Ast.ValDec ([],_) => []
	| Ast.ValDec (vblist,ref tyvars) => (* non-recursive bindings *)
	    let 
		local
		  val pe_list = map vb_strip vblist
		in
		  val (pat,expr) = (case pe_list of
				      [] => pat_error "let with nothing should not get here"
				    | [(p,e)] => (p,e)
				    | _ => (Ast.TuplePat(map #1 pe_list),
					    Ast.TupleExp(map #2 pe_list)))
		end

		val tyvars = map tyvar_strip tyvars
		val tyvar_stamp = get_stamp()
		val lbl = fresh_open_internal_label "varpoly"
		val var_poly = fresh_named_var "varpoly"
		val lbl' = fresh_internal_label "valbind"
		val var' = fresh_named_var "valbind"
		val cons1 = map (fn _ => fresh_con context) tyvars
		local
		    val labs1 = map (fn s => symbol_label s) tyvars
		    val vars1 = map (fn s => gen_var_from_symbol s) tyvars
		in val temp_sdecs = (map3 (fn (v,l,c) => SDEC(l,DEC_CON(v,KIND_TUPLE 1,SOME c))) 
				     (vars1,labs1,cons1))
		end
		val context' = add_context_mod(context,lbl,var_poly,
						  SelfifySig(SIMPLE_PATH var_poly,
							     SIGNAT_STRUCTURE (NONE,temp_sdecs)))
		val _ = eq_table_push()
		val lbl = fresh_internal_label "bindarg"
		val v = fresh_named_var "bindarg"
		val (e,con,va) = xexp(context',expr)
		val sbnd_sdec = (SBND(lbl,BND_EXP(v,e)),SDEC(lbl,DEC_EXP(v,con)))
		val context' = add_context_exp'(context',v,con)
		val patarg = {context = context', 
			      typecompile = xty, 
			      expcompile = xexp, 
			      polyinst = poly_inst,
			      error_region = error_region,
			      fresh_con = fresh_con}
                val parsed_pat = parse_pat context pat
		val bind_sbnd_sdec = (bindCompile{patarg = patarg,
						  bindpat = parsed_pat,
						  arg = (v,con)})
		val sbnd_sdec_list = sbnd_sdec::bind_sbnd_sdec
		val is_irrefutable = va andalso Sbnds_IsValuable(context', map #1 bind_sbnd_sdec)
		fun refutable_case () = 
		    let val _ = eq_table_pop (DEC_MOD(fresh_named_var "dummy", SIGNAT_STRUCTURE(NONE,[])))
		    in  map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) sbnd_sdec_list
		    end
		and irrefutable_case () = 
		    let
			val _ = debugdo (fn () => (print "about to call rebind_free_type_var:  var_poly = ";
						   pp_var var_poly; print "  stamp = ";
						   print (Int.toString (stamp2int tyvar_stamp));
						   print "\nand con = \n";
						   pp_con con; print"\n\n"))
			    
			    
			val tyvar_lbls'_useeq = rebind_free_type_var(tyvar_stamp,con,context,var_poly)
			val _ = debugdo (fn () => (print "done calling rebind_free_type_var:  var_poly = ";
						   pp_var var_poly; print "\nand con = \n";
						   pp_con con; print"\n\n"))
			val poly_sdecs = make_typearg_sdec(map (fn (_,l,f) => (l,f)) tyvar_lbls'_useeq)
			val (sbnds,sdecs) = (map #1 sbnd_sdec_list, map #2 sbnd_sdec_list)
			val _ = 
			    let val extra_dec = DEC_MOD(var_poly,SIGNAT_STRUCTURE (NONE,poly_sdecs))
			    in  eq_table_pop extra_dec
			    end
		    in
			(case poly_sdecs of
			     [] => map2 (fn (sbnd,sdec) => (SOME sbnd, CONTEXT_SDEC sdec)) (sbnds,sdecs)
			   | _ =>
				 let 
				     val sig_poly = SIGNAT_STRUCTURE (NONE,poly_sdecs)
				     val labs = map (fn SDEC (l,_) => l) sdecs
				     val cons = map (fn SDEC(l,DEC_EXP(_,c)) => c | _ => elab_error "Rule 237") sdecs
				     fun mod_sig_help (l,c) =
					 let val modapp = MOD_APP(MOD_VAR var',MOD_VAR var_poly)
					     val inner_var = fresh_named_var "inner_valbind"
					     val outer_var = fresh_named_var "outer_valbind"
						 
					     val temp_mod = MOD_STRUCTURE[SBND(it_lab,
									       BND_EXP(inner_var,
										       MODULE_PROJECT(modapp,l)))]
					     val temp_sig = SIGNAT_STRUCTURE(NONE,
									     [SDEC(it_lab,
										   DEC_EXP(inner_var,c))])
					     val bnd = BND_MOD(outer_var,MOD_FUNCTOR(var_poly,sig_poly,temp_mod))
					     val dec = DEC_MOD(outer_var,
							       SIGNAT_FUNCTOR(var_poly,sig_poly,
									      temp_sig,
									      	   if is_irrefutable 
										       then TOTAL else PARTIAL))
					 in (SBND(l,bnd),SDEC(l,dec))
					 end
				     val temp_mod = MOD_FUNCTOR(var_poly,sig_poly,MOD_STRUCTURE sbnds)
				     val temp_sig = SIGNAT_FUNCTOR(var_poly,sig_poly,
								   SIGNAT_STRUCTURE(NONE, sdecs),
								   if is_irrefutable 
								       then TOTAL else PARTIAL)
				     val rest_sbnds_sdecs = map2 mod_sig_help (labs,cons)
				     val final_sbnds = ((SBND(lbl',BND_MOD(var',temp_mod)))::
							(map #1 rest_sbnds_sdecs))
				     val final_sdecs = ((SDEC(lbl',DEC_MOD(var',temp_sig))) ::
							(map #2 rest_sbnds_sdecs))
				 in map2 (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) (final_sbnds, final_sdecs)
				 end)
		    end
	    in if (is_irrefutable)
		then irrefutable_case()
	       else refutable_case()
	    end
	| binddec as ((Ast.ValrecDec (_,ref tyvars)) | 
		      (Ast.FunDec (_,ref tyvars))) => (* recursive value dec: i.e. functions *)
	      let
                  (* We must discover all the variables to generalize over.  For the user-level variables,
		     we must also compile in a context with these user-level variables bound.  As a first
		     step, we find all the user-level variables in the program. 
		     At some future point when the syntax includes explicit scoping, 
		     we must include those too *)
		  val tyvar_stamp = get_stamp()
		  val tyvars = map tyvar_strip tyvars
		  local
		      fun help tyvar = 
			  let val type_str = Symbol.name tyvar
			      val type_lab = symbol_label tyvar
			      val is_eq =  ((size type_str > 1) andalso 
					    (String.substring(type_str,0,2) = "''"))
			  in  (type_lab,is_eq)
			  end
		      val temp = map help tyvars
		  in  val sdecs1 = make_typearg_sdec temp
		  end

		  val var_poly = fresh_named_var "var_poly"
		  val open_lbl = fresh_open_internal_label "lbl"
		  val context' = add_context_mod(context,open_lbl,var_poly,
						    SelfifySig(SIMPLE_PATH var_poly,
							       SIGNAT_STRUCTURE(NONE, sdecs1)))


		  local (* ----- we cannonicalize into one format by introducing meta-variables *)
		      fun fb_help clause_list =
			  let 
			      val fun_con = fresh_named_con (context',"fun_con")
			      val body_con = fresh_named_con (context',"body_con")
			      fun help (Ast.Clause{pats = {item=Ast.VarPat[s],
							   fixity=NONE,...}::rest, resultty,exp}) =
				  (symbol_label s, (parse_pats context' (map #item rest),exp))
				| help (Ast.Clause{pats : Ast.pat Ast.fixitem list, resultty,exp}) =
				  (case (parse_pats context' (map #item pats)) of
				       (Ast.VarPat[s])::rest => (symbol_label s, (rest, exp))
				     | (Ast.AppPat{constr = Ast.VarPat[s],
						   argument}::rest) => (symbol_label s, (argument::rest, exp))
				     | _ => error "illegal pattern for function declaraion")
			      fun getid [] = parse_error "no ids"
				| getid [a] = a
				| getid (a::(rest as b::_)) = 
				  (if eq_label(a,b) 
				       then ()
				   else (error_region();
					 print "clauses don't all have same function name\n");
				   getid rest)
			      val temp = map help clause_list
			      val id = getid (map #1 temp)
			      val matches = map #2 temp
			  in (id, (fun_con,body_con), matches)
			  end
		      fun rvb_help {var:Ast.symbol, fixity: (Ast.symbol * 'a) option,
				    exp:Ast.exp, resultty: Ast.ty option} = 
			let 
			    val body_con = fresh_named_con (context',"body_con")
			    val fun_con = (case resultty of
					       NONE => fresh_named_con (context',"fun_con")
					     | SOME ty => xty(context',ty))
			    fun help (Ast.Rule{pat,exp}) = 
				(case pat of
				     Ast.ConstraintPat{constraint=ty,pattern} =>  
					 let val given_res_con = xty(context',ty)
					 in  if (eq_con(context',given_res_con,body_con))
						 then ()
					     else (error_region();
						   print "fn matches have conflicting constraints,";
						   print " using the first one\n");
					     (parse_pats context' [pattern],exp)
					 end
				   | _ => (parse_pats context' [pat],exp))
			    val matches = 
				(case exp of 
				     Ast.FnExp rules => map help rules
				   | _ => parse_error "val rec requires an fn expression")
			in  (symbol_label var, (fun_con, body_con), matches)
			end
		  in
		      val dec_list : (label * 
				      (con * con) *
				      (Ast.pat list * Ast.exp) list) list =
			  (case d of
			       Ast.ValrecDec (rvblist,_) => map (rvb_help o rvb_strip) rvblist
			     | Ast.FunDec (fblist,_) => map (fb_help o fb_strip) fblist
			     | _ => pat_error "must have ValrecDec or FunDec here")
		  end (* local ------ ValRecDec and FunDec are assimilated now ---- *)

		  val fun_ids = map #1 dec_list
		  val fun_cons = map (#1 o #2) dec_list
		  val body_cons = map (#2 o #2) dec_list
		  val matches_list = map #3 dec_list
		  val fun_labs = map (fn l => internal_label (label2string l)) fun_ids
		  val fun_vars = map (fn l => fresh_named_var (label2string l)) fun_ids

                  (* --- create the context with all the fun_ids typed --- *)
		  val context_fun_ids = 
		      let fun help ((id,var',funcon),ctxt) = 
			  add_context_exp(ctxt,id,var',funcon)
		      in foldl help context (zip3 fun_ids fun_vars fun_cons)
		      end

		  val context'' = add_context_mod(context_fun_ids,open_lbl,var_poly,
						     SelfifySig(SIMPLE_PATH var_poly,
								SIGNAT_STRUCTURE(NONE, sdecs1)))
			   

		  val _ = eq_table_push()
		  val fbnd_con_list = 
		      (map4 (fn (matches,fun_con,body_con,var') => 
			     let 
				 val patarg = {context = context'', 
					       typecompile = xty, 
					       expcompile = xexp, 
					       polyinst = poly_inst,
					       error_region = error_region,
					       fresh_con = fresh_con}
				 val {body = (bodye,bodyc), 
				      arglist} = funCompile{patarg = patarg,
							    rules = matches,
							    reraise = false}
				 fun con_folder ((_,c),acc) = CON_ARROW([c],acc,false,oneshot_init PARTIAL)
				 val func = foldr con_folder bodyc arglist
				 val _ = if (eq_con(context'',body_con,bodyc) andalso
					     eq_con(context'',fun_con,func))
					     then ()
					 else (error_region();
					       print "function constraint does not match body type\n")
				 local 
				     fun help ((v,c),(e,resc)) = make_lambda(v,c,resc,e)
				 in 
				     val (var1,con1) = hd arglist
				     val (bigbodyc,bigbodye) = foldr help (bodye,bodyc) (tl arglist)
				 end 
			     in (FBND(var',var1,con1,bigbodye,bigbodyc),func)
			     end)
		      (matches_list, fun_cons, body_cons, fun_vars))


		  val fbnds = map #1 fbnd_con_list
		  val fbnd_cons : con list = map #2 fbnd_con_list
		  val top_label = fresh_internal_label "polyfuns"
		  val top_var = fresh_named_var "polyfuns"
		  val top_exp_con = (FIX(true,PARTIAL,fbnds),
				     case fbnd_cons of
					 [c] => c
				       | _ => con_tuple fbnd_cons)

		  local 
		      val tyvar_lbls'_useeq = 
			  ((fn (_,c) => 
			    (debugdo (fn () => (print "about to call rebind_free_type_var:";
						print "var_poly = ";
						pp_var var_poly; print "\nand c = \n";
						pp_con c; print"\n\n"));
			     rebind_free_type_var(tyvar_stamp,c,
						  context_fun_ids,var_poly)))
			  top_exp_con)
		      fun help(_,tlab,iseq) = (tlab,iseq)
		      val temp = map help tyvar_lbls'_useeq
		  in
		      val sdecs2 = make_typearg_sdec temp
		      val sdecs = sdecs1 @ sdecs2
		  end


		  val temp_exp = (case sdecs of
				      [] => VAR top_var
				    | _ => let val s = MOD_APP(MOD_VAR top_var,
							       MOD_VAR var_poly)
					   in MODULE_PROJECT(s,it_lab)
					   end)
					  
		  val exp_con_list = 
		      case fbnd_cons of
			  [c] => [(temp_exp,c)]
			| _ => mapcount (fn (i,c) => (RECORD_PROJECT(temp_exp, 
								     generate_tuple_label (i+1),
								     #2 top_exp_con), c))
			  fbnd_cons
(*
		  val exp_con_list = map2count 
		      (fn (i,v',c) => (let val f = FIX(PARTIAL,fbnds)
				       in case fbnd_con_list of
					   [_] => f
					 | _ => RECORD_PROJECT(f,generate_tuple_label(i+1),
							       #2 top_exp_con)
				       end,
					   c))
		      (fun_vars,map #2 fbnd_con_list)
*) 


		  val _ = 
		      let val extra_dec = DEC_MOD(var_poly,SIGNAT_STRUCTURE(NONE, sdecs))
		      in  eq_table_pop extra_dec
		      end


		  fun modsig_helper nameopt (id,(exp,con)) = 
		      let val v1 = fresh_named_var "fixexp"
			  val v2 = (case nameopt of
					NONE => fresh_named_var (label2string id)
				      | SOME v => v)
			  fun poly_case () = 
			      let 
				  val sig_poly = SIGNAT_STRUCTURE (NONE,sdecs)
				  val sbnd = SBND(it_lab, BND_EXP(v1,exp))
				  val sdec = SDEC(it_lab, DEC_EXP(v1,con))
				  val functor_mod = MOD_FUNCTOR(var_poly,sig_poly,
								MOD_STRUCTURE[sbnd])
				  val functor_sig = 
				      SIGNAT_FUNCTOR(var_poly,sig_poly,
						     SIGNAT_STRUCTURE(NONE, [sdec]),
						     TOTAL)
			      in  (SBND(id,BND_MOD(v2,functor_mod)),
				   SDEC(id,DEC_MOD(v2,functor_sig)))
			      end
		      in
			  (case sdecs of
			       [] => (SBND(id,BND_EXP(v2,exp)),
				      SDEC(id,DEC_EXP(v2,con)))
			     | _ => poly_case())
		      end

		  val (top_sbnd,top_sdec) = modsig_helper (SOME top_var) (top_label, top_exp_con)
		  val top_sbnd_entry = (SOME top_sbnd, CONTEXT_SDEC top_sdec)
		  val sbnds_sdecs = map2 (modsig_helper NONE) (fun_ids,exp_con_list)
		  val sbnds_entries = (map (fn (sbnd,sdec) => (SOME sbnd,CONTEXT_SDEC sdec)) 
				       sbnds_sdecs)
	    in	top_sbnd_entry :: sbnds_entries
	    end

	| Ast.ExternDec (sym,ty) =>
	    let val var = gen_var_from_symbol sym
		val lab = symbol_label sym
		val con = xty(context,ty)
	    in  [(NONE, CONTEXT_SDEC(SDEC(lab,DEC_EXP(var,con))))]
	    end
	| Ast.SeqDec decs => packagedecs xdec' context decs
	| Ast.OpenDec pathlist => 
	      let fun help (i,path) = 
		  (case (Context_Lookup(context,map symbol_label path)) of
		       SOME(_,PHRASE_CLASS_MOD(m,s)) => 
			   let val l = fresh_open_internal_label ("openlbl" ^ (Int.toString i))
			       val v = fresh_named_var "openvar"
			   in  SOME(SOME (SBND(l,BND_MOD(v,m))), CONTEXT_SDEC(SDEC(l,DEC_MOD(v,s))))
			   end
		     | _ => (error_region(); print "unbound structure: ???\n";
			     NONE))
	      in List.mapPartial (fn x => x) (mapcount help pathlist)
	      end
	| Ast.TypeDec tblist => 
	      let val typeresult = xtybind(context,tblist) 
		  fun make_eq_bnddec(l,c,k) = 
		      let val is_poly = (case k of
					     KIND_TUPLE _ => NONE
					   | KIND_ARROW(m,_) => SOME m)
			  val vp = fresh_named_var "varpoly"
			  val (ctxt',c',sigpoly) = 
			      case is_poly of 
				  NONE => (context,c,SIGNAT_STRUCTURE(NONE,[]))
				| SOME m => 
				      let
					  fun mapper n = internal_label ("tv" ^ (Int.toString n)) 
					  val lbls = Listops.map0count mapper m
					  fun mapper l = 
					      let val eql = to_eq_lab l
						  val v = fresh_var()
						  val sdec1 = SDEC(l,DEC_CON(v,KIND_TUPLE 1, NONE))
						  val sdec2 = SDEC(eql,DEC_EXP(fresh_var(),
									       mk_eq_con(CON_VAR v)))
					      in  [sdec1,sdec2]
					      end
					  val sdecs = List.concat (map mapper lbls)
					  val sp = SIGNAT_STRUCTURE(NONE,sdecs)
					  val ctxt' = add_context_dec(context,SelfifyDec(DEC_MOD(vp,sp)))
					  val arg_cons = map (fn l => CON_MODULE_PROJECT(MOD_VAR vp,l)) lbls
					  val arg_con = con_tuple_inject arg_cons
					  val c' = CON_APP(c,arg_con)
				      in  (ctxt',c',sp)
				      end
			  val eqlab = to_eq_lab l
			  val eq_con = mk_eq_con c'
		    in case (xeqopt(ctxt',c')) of
			SOME eq_exp =>
			    let
			  val v1 = fresh_var()
			  val (bnd,dec) = 
			      case is_poly of
				  NONE => (BND_EXP(v1, eq_exp), 
					   DEC_EXP(v1, eq_con))
				| SOME _ => 
				      let val v2 = fresh_var()
					  val innermod = 
					      MOD_FUNCTOR(vp,sigpoly,
							  MOD_STRUCTURE[SBND(it_lab,BND_EXP(v2,eq_exp))])
					  val innersig = 
					      SIGNAT_FUNCTOR(vp,sigpoly,
							     SIGNAT_STRUCTURE(NONE,
									      [SDEC(it_lab,
									       DEC_EXP(v2,eq_con))]),
							      TOTAL)
					      
				      in  (BND_MOD(v1,innermod), DEC_MOD(v1, innersig))
				      end
			    in  [(SOME(SBND(eqlab,bnd)),CONTEXT_SDEC(SDEC(eqlab,dec)))]
			    end
			    | NONE => []
			end
		  val eqresult = 
		      case typeresult of
			  [(SOME(SBND(l,BND_CON(_,c))), CONTEXT_SDEC(SDEC(_,DEC_CON(_,k,_))))] =>
			      make_eq_bnddec(l,c,k)
			| _ => []
	      in  typeresult @ eqresult
	      end
	| Ast.DatatypeDec {datatycs,withtycs=[]} => 
	      let val sbnd_sdecs = xdatatype(context,datatycs)
	      in  map (fn (sb,sd) => (SOME sb, CONTEXT_SDEC sd)) sbnd_sdecs 
	      end
	| Ast.DatatypeDec {datatycs,withtycs} => 
	      let val (dt,wt) = (case InfixParse.parse_datbind(datatycs,withtycs) of
				     SOME result => result
				   | NONE => (error_region();
					      print "cannot parse datbind\n";
					      error "cannot parse datbind"))
		  val dec = Ast.SeqDec[Ast.DatatypeDec{datatycs=dt,withtycs=[]},
				       Ast.TypeDec wt]
	      in  xdec(context,dec)
	      end
	| Ast.StrDec strblist => xstrbinds(context,strblist) 
 	| Ast.FctDec fctblist => xfctbind(context,fctblist) 

	| Ast.ExceptionDec [] => parse_error "ExceptionDec []"
	| Ast.ExceptionDec [Ast.MarkEb (eb,r)] => 
	      let val _ = push_region r
		  val res = xdec(context, Ast.ExceptionDec [eb])
		  val _ = pop_region()
	      in res
	      end
	| Ast.ExceptionDec [Ast.EbGen {exn,etype}] =>
		let 
		  val exn_str = Symbol.symbolToString exn
		  val id_bar = symbol_label exn
		  val var = fresh_named_var "exn_stamp"
		  val mkvar = fresh_named_var "mk"
		  val exnmodvar = fresh_named_var "exnmod"
		  val v = fresh_named_var "injectee"
		  val con = (case etype of
			       NONE => con_unit
			     | SOME ty => xty(context,ty))
		  val (mk_exp,mk_con) = 
		      (case etype of
			   NONE => (EXN_INJECT(exn_str,VAR var,unit_exp), CON_ANY)
			 | SOME ty => (#1 (make_total_lambda(v,con,CON_ANY,
							     EXN_INJECT(exn_str, VAR var,VAR v))),
				       CON_ARROW([con], CON_ANY, false, oneshot_init TOTAL)))
		  val inner_mod = MOD_STRUCTURE[SBND(stamp_lab, BND_EXP(var,NEW_STAMP con)),
						SBND(mk_lab, BND_EXP(mkvar,mk_exp))]
		  val inner_sig = SIGNAT_STRUCTURE(NONE,
						   [SDEC(stamp_lab,DEC_EXP(var,CON_TAG con)),
						    SDEC(mk_lab,DEC_EXP(mkvar,mk_con))])
		in [(SOME(SBND(id_bar,BND_MOD(exnmodvar,inner_mod))),
		     CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(exnmodvar,inner_sig))))]
		end
	| Ast.ExceptionDec [Ast.EbDef {exn: Symbol.symbol, edef: Ast.path}] => 
	      (case (Context_Lookup(context,map symbol_label edef)) of
		   SOME(_,PHRASE_CLASS_MOD(m,s)) => 
		       let val id_bar = symbol_label exn
			   val path_mk_exp = MODULE_PROJECT(m,mk_lab)
			   val path_stamp_exp = MODULE_PROJECT(m,stamp_lab)
			   val path_mk_con = GetExpCon(context,path_mk_exp)
			   val path_stamp_con = GetExpCon(context,path_stamp_exp)
			   val itvar = fresh_named_var "exn_tag"
			   val mkvar = fresh_named_var "exn_injector"
			   val modvar = fresh_named_var "exn_structure"
			   val inner_mod = MOD_STRUCTURE[SBND(stamp_lab, BND_EXP(itvar,path_stamp_exp)),
							 SBND(mk_lab, BND_EXP(mkvar,path_mk_exp))]

			   val inner_sig = SIGNAT_STRUCTURE(NONE,
							    [SDEC(stamp_lab, DEC_EXP(itvar,path_stamp_con)),
							     SDEC(mk_lab, DEC_EXP(mkvar,path_mk_con))])
		       in [(SOME(SBND(id_bar,BND_MOD(modvar,inner_mod))),
			    CONTEXT_SDEC(SDEC(id_bar,DEC_MOD(modvar,inner_sig))))]
		       end
		 | _ => (error_region(); print "unbound exception: ???\n";
			 []))
	| Ast.ExceptionDec eblist => xdec(context,Ast.SeqDec(map (fn eb => Ast.ExceptionDec [eb]) eblist))

        (* Rule 244 *)
	| Ast.LocalDec (dec1,dec2) => 
	      let 
		  val boolsbnd_ctxt_list1 = xdec'(context,dec1)
		  val (_,context') = add_context_boolsbnd_ctxts(context,boolsbnd_ctxt_list1)
		  val boolsbnd_ctxt_list2 = xdec'(context',dec2)
		  fun temp (opt : (bool * sbnd) option,ce) = (mapopt #2 opt,ce)
		  fun rename(opt,CONTEXT_SDEC(SDEC(_,dec))) = 
		      let val lbl = fresh_internal_label "local"
			  val ce' = CONTEXT_SDEC(SDEC(lbl,dec))
		      in case opt of
			  NONE => (NONE,ce')
			| SOME (SBND(_,bnd)) => (SOME (SBND(lbl,bnd)), ce')
		      end
		    | rename arg = arg
		  val sbnd_ctxt_list1 = map (rename o temp) boolsbnd_ctxt_list1
		  val sbnd_ctxt_list2 = map temp boolsbnd_ctxt_list2
	      in sbnd_ctxt_list1 @ sbnd_ctxt_list2
	      end

        (* Must augment translation context with fixity information *)
	| Ast.FixDec {fixity,ops} => let (* given symbol is in FixSymbol space *)
				       fun helper sym = 
					   let val sym' = Symbol.varSymbol(Symbol.name sym)
					   in (symbol_label sym', fixity)
					   end
				       val vf_list = map helper ops
				     in [(NONE,CONTEXT_FIXITY vf_list)]
				     end

	(* These cases are unhandled *)
	| Ast.SigDec [] => []
	| Ast.SigDec (sigb::rest) => 
	      let val ctxt : context_entry = xsigb(context,sigb)
		  (* CONTEXT_SIGNATs do not need to be selfified *)
		  val context' = add_context_entries(context,[ctxt])
		  val sbnd_ctxt_rest = xdec(context',Ast.SigDec rest)
	      in (NONE,ctxt)::sbnd_ctxt_rest
	      end
	| Ast.AbstypeDec {abstycs,withtycs,body} => 
	      let val ldec = Ast.DatatypeDec{datatycs = abstycs, withtycs = withtycs}
		  fun get_dec(Ast.MarkDb(db,_)) = get_dec db
		    | get_dec(Ast.Db{tyc,tyvars,...}) = 
		      let val ty = Ast.ConTy([tyc],map Ast.VarTy tyvars)
		      in Ast.TypeDec[Ast.Tb{tyc=tyc,tyvars=tyvars,def=ty}]
		      end
		  val bdec = Ast.SeqDec((map get_dec abstycs) @ [body])
		  val desugared_dec = Ast.LocalDec(ldec,bdec)
	      in xdec(context, desugared_dec)
	      end
	| Ast.FsigDec fsiglist => parse_error "functor signature declaration not handled"
	| Ast.AbsDec strblist => parse_error "abstract structure not handled"
	| Ast.OvldDec (sym,ignored_type,exp_list) =>
	      let val con_exp_list = map (fn e => let val (e,c,_) = xexp(context,e)
						  in (c,e)
						  end) exp_list
		  val l = symbol_label sym
		  val v = gen_var_from_symbol sym
		  val inline = INLINE_OVER con_exp_list
		  val ce = CONTEXT_INLINE(l,v,inline)
	      in  [(NONE,ce)]
	      end
	| Ast.ImportDec strlist => parse_error "import declaration not handled"

        (* translate declaration by dropping region information *)
	| Ast.MarkDec (dec,region) => let val _ = push_region region
					  val res = xdec(context,dec)
					  val _ = pop_region()
				      in res
				      end)

	    
    (* --------------------------------------------------------- 
      ------------------ TYPE EXPRESSIONS----------------------
      --------------------------------------------------------- *)
    and xty (context, ty) : con = 
      (case ty of
	 Ast.VarTy tyvar => 
	     let val sym = AstHelp.tyvar_strip tyvar
	     in (case (Context_Lookup(context,[symbol_label sym])) of
		     SOME(_,PHRASE_CLASS_CON (c,_)) => c
		   | _ => (error_region(); print "unbound type constructor: ";
			   AstHelp.pp_sym sym; print "\n";
			   fresh_named_con(context,"unbound_type")))
	     end
       | Ast.MarkTy (ty,r) => 
	     let val _ = push_region r
		 val res = xty(context,ty)
		 val _ = pop_region()
	     in res
	     end
       | Ast.TupleTy(tys) => 
	     let fun loop _ [] = []
		   | loop n (a::rest) = (generate_tuple_symbol n,a)::(loop (n+1) rest)
	     in xty(context, Ast.RecordTy(loop 1 tys))
	     end
       | Ast.RecordTy(sym_ty_list) => let val lab_ty_list = map (fn (s,t) => (symbol_label s,t)) sym_ty_list
					  val sorted_lab_ty_list = sort_labelpair lab_ty_list
					  fun doer (lab,ty) = (lab,xty(context,ty))
				      in CON_RECORD(map doer sorted_lab_ty_list)
				      end
       | Ast.ConTy (syms,ty_list) => 
	     let 
		 val _ = debugdo (fn () => (print "xty: Ast.Conty(["; 
					    pp_list AstHelp.pp_sym' syms ("","",".",false); print "])"))
		 val con_list = map (fn t => xty(context,t)) ty_list
	     in
		 (case (Context_Lookup(context, map symbol_label syms)) of
		      SOME(_,PHRASE_CLASS_CON(con,k)) =>
			  (case (con_list,(case k of 
					      KIND_INLINE(k,_) => k
					    | _ => k)) of
			      ([],KIND_TUPLE 1) => con
			    | (_,KIND_ARROW(n,1)) => 
				  if (n = length con_list) 
				      then ConApply(true,con,con_tuple_inject con_list)
				  else (error_region();
					tab_region();
					print "type constructor wants ";
					print (Int.toString n);
					print "arguments, given ";
					print (Int.toString (length con_list));
					fresh_named_con(context,"badarity_type"))
			     | _ => (pp_kind k; print "\nand c = "; 
				      pp_con con;
				      elab_error "external_label mapped to type with KIND_ARROW(_,!= 1)"))
		    | _ => 
		      if (length syms = 1 andalso
			     hd syms = Symbol.tycSymbol "-->")
		      then let fun split acc [] = error "need at least result type"
				 | split acc [c] = (rev acc,c)
			         | split acc (c::d) = split (c::acc) d
			       val (arg_cons,res_con) = split [] con_list
			   in  CON_ARROW(arg_cons,res_con,true,
					 oneshot_init PARTIAL)
			   end
		      else
			       (error_region();
			       print "unbound type constructor: ";
			       pp_pathlist AstHelp.pp_sym' syms; print "\n";
			       fresh_named_con(context,"unbound_type")))
	     end)

				  
    (* --------------------------------------------------------- 
      ------------------ TYPE DEFINITIONS ---------------------
      --------------------------------------------------------- *)
    and xtybind (context : context, tblist : Ast.tb list) : (sbnd option * context_entry) list =
       let 
	 fun doer tb = 
	   let 
	     val (tyc,tyvars,def) = tb_strip tb
	     val tyvars = map tyvar_strip tyvars
	     val vars = map (fn s => gen_var_from_symbol s) tyvars
	     val tyvars_bar = map (fn s => symbol_label s) tyvars
	     val context' = (foldl (fn ((v,tv),c) => 
				    add_context_con(c,tv,v,KIND_TUPLE 1,NONE))
			     context (zip vars tyvars_bar))
	     val con' = xty(context',def)
	     val n = length tyvars
	     val (con,kind) = (case tyvars of
				 [] => (con',KIND_TUPLE 1)
			       | _ => (CON_FUN(vars,con'),KIND_ARROW(n,1)))
	     val var = gen_var_from_symbol tyc
	     val tyc_bar = symbol_label tyc
	   in (SOME(SBND(tyc_bar,BND_CON(var,con))),
	       CONTEXT_SDEC(SDEC(tyc_bar,DEC_CON(var,kind,SOME con))))
	   end
       in map doer tblist
       end
     
    (* --------------------------------------------------------- 
      ------------------ TYPE DESCRIPTIONS ---------------------
      --------------------------------------------------------- *)
   and xtypedesc (context : context, sym_tyvar_tyopt_list, is_eq) : sdecs = 
       let 
	   fun loop [] = []
	     | loop ((sym,tyvars : Ast.tyvar list, tyopt)::rest) =
		   let 
		       val type_label = symbol_label sym
		       val type_var = gen_var_from_symbol sym
		       val eq_label = to_eq_lab type_label
		       val eq_var = fresh_named_var (label2string eq_label)
		       val kind = 
			   (case tyvars of
				[] => KIND_TUPLE 1
			      | _ => KIND_ARROW(length tyvars,1))
		       fun doty ty =
			   let 
			       val vars = map (fn _ => fresh_var()) tyvars
			       val tyvars_bar = map (fn s => symbol_label (tyvar_strip s)) tyvars
			       val context' = (foldl (fn ((v,tv),c) => 
						      add_context_con(c,tv,v,KIND_TUPLE 1,NONE))
					       context (zip vars tyvars_bar))
			       val con' = xty(context',ty)
			   in  (case tyvars of
				    [] => con'
				  | _ => (CON_FUN(vars,con')))
			   end
		       val conopt = mapopt doty tyopt
		       val type_sdec = SDEC(type_label, DEC_CON(type_var,kind,conopt))
		       val eq_dec = 
			   (case tyvars of
				[] => DEC_EXP(eq_var,mk_eq_con(CON_VAR type_var))
			      | _ => 
				let val vpoly = fresh_named_var "vpoly"
				    fun mapper (n,_) = internal_label ("tv" ^ (Int.toString n)) 
				    val lbls = Listops.mapcount mapper tyvars
				    fun mapper l = 
					let val eql = to_eq_lab l
					    val v = fresh_var()
					    val sdec1 = SDEC(l,DEC_CON(v,KIND_TUPLE 1, NONE))
					    val sdec2 = SDEC(eql,DEC_EXP(fresh_var(),
									 mk_eq_con(CON_VAR v)))
					in  [sdec1,sdec2]
					end
				    val sdecs = List.concat (map mapper lbls)
				    val sigpoly = SIGNAT_STRUCTURE(NONE,sdecs)
				    val args = map (fn l => CON_MODULE_PROJECT(MOD_VAR vpoly,l)) lbls
				    val eq_con = mk_eq_con(CON_APP(CON_VAR type_var, 
								   con_tuple_inject args))
				    val innersig = SIGNAT_STRUCTURE(NONE,
								    [SDEC(it_lab,
									  DEC_EXP(fresh_var(),eq_con))])
				in  DEC_MOD(eq_var,SIGNAT_FUNCTOR(vpoly,sigpoly,innersig,PARTIAL))
				end)
					    
		       val eq_sdec = SDEC(eq_label, eq_dec)
		   in if (is_eq) then type_sdec :: eq_sdec:: (loop rest)
		      else type_sdec :: (loop rest)
		   end
       in loop sym_tyvar_tyopt_list
       end

    (* --------------------------------------------------------- 
      ------------ Signature bindings and expressions ----------
      --------------------------------------------------------- *)
     and xsigexp(context,sigexp) : signat =
       (case sigexp of
	  Ast.VarSig s => (case (Context_Lookup(context,[symbol_label s])) of
			       SOME(_,PHRASE_CLASS_SIG s) => s
			     | _ => (error_region();
				     print "unbound signature: "; 
				     AstHelp.pp_sym s;
				     print "\n";
				     SIGNAT_STRUCTURE(NONE,[])))
	| Ast.SigSig speclist => SIGNAT_STRUCTURE(NONE,xspec(context,speclist))
	| Ast.MarkSig (s,r) => let val _ = push_region r
				   val res = xsigexp(context,s)
				   val _ = pop_region()
			       in res 
			       end
	| Ast.AugSig (s, []) => xsigexp(context,s)
	| Ast.AugSig (s, ((Ast.WhStruct (syms1,syms2))::rest)) => 
	      let val mjunk = MOD_VAR(fresh_named_var "mjunk")
	      in  (case xsigexp(context,Ast.AugSig(s,rest)) of
		   s as SIGNAT_STRUCTURE (popt,sdecs) => 
		       (case (Sdecs_Lookup'(mjunk,sdecs,map symbol_label syms1),
			      xstrexp(context,Ast.VarStr syms2, Ast.NoSig)) of
			    (SOME(labels1,PHRASE_CLASS_MOD(_,s1)),
			     (_,_,SIGNAT_STRUCTURE(_,sdecs2))) =>
				let fun find_labels' path (SDEC(l,DEC_CON (_,k,_)),acc) = (k,l::path)::acc
				      | find_labels' path (SDEC(l,DEC_MOD (_,s)),acc) = 
				                       (find_labels (l::path) s)@acc
				      | find_labels' path (_,acc) = acc
				    and find_labels path (SIGNAT_STRUCTURE (_,sdecs)) = 
					foldl (find_labels' path) [] sdecs
				      | find_labels _ _ = []
				    val kind_rlabels = find_labels [] s1
				    fun constrain((k,rlbls),sdecs) = 
					let val lbls = rev rlbls
					    val lbls_all = labels1 @ lbls
					    val c = case (Sdecs_Lookup'(mjunk,sdecs2,lbls)) of
						SOME(_,PHRASE_CLASS_CON(c,_)) => c
					      | _ => (error_region();
						      print "where's rhs structure missing components\n";
						      dummy_type(context,"badwhere"))
					in  xsig_wheretype(context,sdecs,lbls_all,c,k)
					end
				in  SIGNAT_STRUCTURE(popt,foldl constrain sdecs kind_rlabels)
				end
			   | _ => (error_region();
				   print "can't where non-existent or non-structure component\n";
				   s))
		 | s => (error_region();
			 print "can't where a non-structure signature\n";
			 s))
	      end
	| Ast.AugSig (s, ((Ast.WhType(syms, tyvars, ty))::rest)) =>
	      (case xsigexp(context,Ast.AugSig(s,rest)) of
		   s as SIGNAT_STRUCTURE (popt,sdecs) => 
		       let val mjunk = MOD_VAR(fresh_named_var "mjunk")
		       in (case (Sdecs_Lookup'(mjunk,sdecs,map symbol_label syms)) of
			       SOME(labels,PHRASE_CLASS_CON(_,k)) => 
				   let val sym_vars = map (fn tv => 
							   let val sym = AstHelp.tyvar_strip tv
							   in  (sym, gen_var_from_symbol sym)
							   end) tyvars
				       fun folder ((sym,var),context) = 
				        add_context_sdec(context,SDEC(symbol_label sym, 
								      DEC_CON(var,
									      KIND_TUPLE 1, NONE)))
				       val context = foldl folder context sym_vars
				       val c = xty(context,ty)
				       val c = (case sym_vars of
						    [] => c
						  | _ => CON_FUN(map #2 sym_vars, c))
				   in SIGNAT_STRUCTURE(popt,xsig_wheretype(context,sdecs,labels,c,k))
				   end
			     | _ => (error_region();
				     print "can't where type a non-type component";
				     s))
		       end
		 | s => (error_region();
			 print "can't where type a non-structure signature\n";
			 s)))


     and xsigb(context,Ast.MarkSigb(sigb,r)) : context_entry = 
	 let val _ = push_region r
	     val res = xsigb(context,sigb)
	     val _ = pop_region()
	 in res
	 end
       | xsigb(context,Ast.Sigb{name,def}) = 
	 let val v = gen_var_from_symbol name
	 in CONTEXT_SIGNAT(symbol_label name,v,
			   xsigexp(context,def))

	 end

    (* --------------------------------------------------------- 
      ------------------ SIGNATURE SPECIFICTIONS --------------
      --------------------------------------------------------- *)
     and xspec(orig_ctxt, specs : Ast.spec list) : sdecs = 
       let 
	 datatype sdecs_tag = ADDITIONAL of sdecs | ALL_NEW of sdecs
	 fun xspec1 context prev_sdecs spec : sdecs_tag =
	     case spec of
		 (Ast.ValSpec vtlist) => (* Rules 257 - 258 *)
	       let 
		 fun doer (sym,ty) = 
		   (case (free_tyvar_ty(ty,fn _ => false)) of
			[] => SDEC(symbol_label sym, 
				   DEC_EXP(gen_var_from_symbol sym,xty(context,ty)))
		    | ftv_sym => 
			let 
			    val varpoly = fresh_named_var "var_poly"
			    fun help tv_sym = 
				let val type_lab = symbol_label tv_sym
				    val eq_lab = to_eq_lab type_lab
				    val type_var = fresh_var()
				    val eq_var = fresh_var()
				    val type_str = Symbol.name tv_sym
				    val is_eq =  ((size type_str > 1) andalso 
						  (String.substring(type_str,0,2) = "''"))
				    val eq_con = CON_ARROW([con_tuple[CON_VAR type_var, CON_VAR type_var]],
							   con_bool,false, oneshot_init PARTIAL)
				    val type_sdec = SDEC(type_lab,DEC_CON(type_var,KIND_TUPLE 1, NONE))
				    val eq_sdec = SDEC(eq_lab, DEC_EXP(eq_var,eq_con))
				in if is_eq
				    then [type_sdec,eq_sdec]
				   else [type_sdec]
				end
			    val sigpoly = SIGNAT_STRUCTURE(NONE,flatten(map help ftv_sym))
			    val context' = add_context_mod(context,
							      fresh_open_internal_label "lbl",
							      varpoly, 
							      SelfifySig(SIMPLE_PATH varpoly,sigpoly))
			    val con = xty(context',ty)
			    val fsig = SIGNAT_FUNCTOR(varpoly,sigpoly,
						      SIGNAT_STRUCTURE(NONE,
								       [SDEC(it_lab,
									     DEC_EXP(fresh_var(),con))]),
						      TOTAL)
			in SDEC(symbol_label sym, DEC_MOD(fresh_named_var "unused",fsig))
			end)
	       in ADDITIONAL(map doer vtlist )
	       end
	   | (Ast.StrSpec (sym_sigexp_path_list)) =>
	       let 
		   fun doer(sym,SOME sigexp, _) = 
		       let val s = xsigexp(context,sigexp)
		       in SDEC(symbol_label sym,DEC_MOD(fresh_var(),s))
		       end
		     | doer(sym,_,_) = error "structure path spec not imped"
	       in ADDITIONAL(map doer sym_sigexp_path_list)
	       end
	   | (Ast.IncludeSpec sym) =>
	       (case (xsigexp(context,Ast.VarSig sym)) of
		  SIGNAT_STRUCTURE (NONE,sdecs) => ADDITIONAL sdecs
		| SIGNAT_STRUCTURE (SOME _, _) => elab_error "xsigexp compiled to selfified signature"
		| _ => elab_error "xsigexp compiled to non-structure signature")
	   | (Ast.FctSpec sym_fsigexp_list) =>
		  let 
		    fun doer (funid,fsig) = 
		      let 
			val var = fresh_var()
			fun help (Ast.VarFsig _) = parse_error "Ast.VarFsig encountered"
			  | help (Ast.MarkFsig (fs,r)) = let val _ = push_region r
							     val res = help fs
							     val _ = pop_region ()
							 in res
							 end
			  | help (Ast.FsigFsig {param,def=sigexp'}) = 
			     let (* this is a derived form *)
			       val strid = functor_arg_lab
			       val sym_sigexp_path_list = 
				   (map (fn (SOME s,se) => (s,SOME se,NONE)
				          | (NONE, _) => elab_error "functor arg unnamed")
				    param)
			       val sigexp = Ast.SigSig[Ast.StrSpec sym_sigexp_path_list]
			       val signat = xsigexp(context,sigexp)
			       val context' = add_context_mod(context,strid,var,
								 SelfifySig(SIMPLE_PATH var,signat))
			       val signat' = xsigexp(context',sigexp')
			     in SIGNAT_FUNCTOR(var,signat,signat',PARTIAL)
			     end
		      in SDEC(symbol_label funid, DEC_MOD(var,help fsig))
		      end
		  in ADDITIONAL(map doer sym_fsigexp_list)
		  end
	   | (Ast.TycSpec (typdesc_list,is_eq)) => ADDITIONAL(xtypedesc(context,typdesc_list,is_eq))
	   | (Ast.ExceSpec exlist) => (* Rules 260 - 261 *)
		      let fun doer (sym,tyopt) = 
			let val (mk_con,stamp_con) = 
			  (case tyopt of
			     NONE => (CON_ANY, CON_TAG con_unit)
			   | (SOME ty) => let val con = xty(context,ty)
					  in (CON_ARROW([con],CON_ANY,false,oneshot_init TOTAL),
					      CON_TAG con)
					  end)
			    val inner_sig = 
			      SIGNAT_STRUCTURE(NONE,
					       [SDEC(stamp_lab,
						    DEC_EXP(fresh_var(),stamp_con)),
					       SDEC(mk_lab,
						    DEC_EXP(fresh_var(),mk_con))])
			in SDEC(symbol_label sym, DEC_MOD(fresh_var(),inner_sig))
			end
		      in ADDITIONAL(map doer exlist)
		      end
	   | (Ast.DataSpec {datatycs, withtycs = []}) =>
	        let val sbnd_sdecs = xdatatype(context,datatycs)
		    val sdecs = map #2 sbnd_sdecs
		in  ADDITIONAL sdecs
		end
	   | (Ast.ShareTycSpec paths) => ALL_NEW(xsig_sharing_type(context,prev_sdecs,paths))
	   | (Ast.ShareStrSpec paths) => ALL_NEW(xsig_sharing_structure(context,prev_sdecs,paths))
	   | (Ast.FixSpec _) => parse_error "fixity specs not supported"
	   | (Ast.MarkSpec (s,r)) => let val _ = push_region r
					 val res = xspec1 context prev_sdecs s
					 val _ = pop_region ()
				     in res
				     end
         fun loop ctxt prev_sdecs [] = prev_sdecs
           | loop ctxt prev_sdecs ((Ast.DataSpec{datatycs,withtycs=wt as (_::_)})::specrest) =
		let val (dt,wt) = (case InfixParse.parse_datbind(datatycs,wt) of
				       SOME result => result
				     | NONE => (error_region();
						print "cannot parse datspec\n";
						error "cannot parse datspec"))
		    val dspec = Ast.DataSpec{datatycs=dt,withtycs=[]}
		    fun strip tb = let val (s,tvs,ty) = AstHelp.tb_strip tb
				   in  (s,tvs,SOME ty) 
				   end
		    val wspec = Ast.TycSpec (map strip wt,false)
		in  loop ctxt prev_sdecs (dspec::wspec::specrest)
		end
           | loop ctxt prev_sdecs (spec::specrest) =
	     (case (xspec1 ctxt prev_sdecs spec) of
		  ADDITIONAL sdecs' =>
		      let val sdecs'' = map (fn (SDEC(l,dec)) => SDEC(l,SelfifyDec dec)) sdecs'
			  val ctxt' = add_context_sdecs(ctxt,sdecs'')
		      in loop ctxt' (prev_sdecs @ sdecs') specrest
		      end
		| ALL_NEW sdecs' => 
		      let val sdecs'' = map (fn (SDEC(l,dec)) => SDEC(l,SelfifyDec dec)) sdecs'
			  val ctxt' = add_context_sdecs(orig_ctxt,sdecs'')
		      in loop ctxt' sdecs' specrest
		      end)
       in loop orig_ctxt [] specs
       end


    (* --------------------------------------------------------- 
      ------------------ FUNCTOR BINDINDS ---------------------
      --------------------------------------------------------- *)
     and xfctbind (context : context, fctbs : Ast.fctb list) : (sbnd option * context_entry) list =
	 let 
	     fun help (context,(name,def)) : ((bool * sbnd) option * context_entry) list = 
		 (case def of
		      (Ast.VarFct (path,Ast.NoSig)) => 
			  (case (Context_Lookup(context,map symbol_label path)) of
			       SOME(path,PHRASE_CLASS_MOD(m,s as (SIGNAT_FUNCTOR _))) => 
				   let val l = symbol_label name
				       val v = fresh_named_var "functor_var"
				   in [(SOME(false,SBND(l,BND_MOD(v,m))),
					CONTEXT_SDEC(SDEC(l,DEC_MOD(v,s))))]
				   end
			     | _ => (error_region();
				    print "unbound functor: ";
				    AstHelp.pp_path path;
				    print "\n"; []))
		    | (Ast.VarFct (path,_)) => parse_error "functor signatures not handled"
		    | (Ast.FctFct {params=[(argnameopt,sigexp)],body,constraint}) =>
			  let 
			      val arglabel = (case argnameopt of
						  NONE => fresh_open_internal_label "FunctorArg"
						| SOME s => symbol_label s)
			      val funid = symbol_label name
			      val argvar = fresh_named_var "functor_arg_var"
			      val signat = xsigexp(context,sigexp)
			      val context' = add_context_mod(context,arglabel,argvar,
							     SelfifySig (SIMPLE_PATH argvar, 
									 signat))
			      val (sbnd_ce_list,m',s') = xstrexp(context',body,constraint)
			      fun addbool(NONE,ce) = (NONE,ce)
				| addbool(SOME sbnd,ce) = (SOME(false,sbnd), ce)
			      val sbnd_ce_list' = map addbool sbnd_ce_list
			      val v = fresh_named_var "functor_var"
			      val sbnd = SBND(funid,BND_MOD(v,MOD_FUNCTOR(argvar,signat,m')))
			      val sdec = SDEC(funid,DEC_MOD(v,SIGNAT_FUNCTOR(argvar,signat,s',
									     PARTIAL)))
			  in sbnd_ce_list' @ [(SOME(false,sbnd), CONTEXT_SDEC sdec)]
			  end
		    | (Ast.FctFct {params=[],body,constraint}) => parse_error "Functor of order 0"
		    | (Ast.FctFct _) => parse_error "No higher order functors"
		    | (Ast.LetFct _) => parse_error "No lets in functor bindings"
		    | (Ast.AppFct _) => parse_error "No higher order functors"
		    | (Ast.MarkFct (f,r)) => let val _ = push_region r
						 val res = help (context,(name,f))
						 val _ = pop_region()
					     in res
					     end)
	 in  packagedecs help context (map fctb_strip fctbs)
	 end

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE EXPRESSION -----------------
      --------------------------------------------------------- *)

     and xstrexp (context : context, strb : Ast.strexp,  Ast.Opaque sigexp) 
	 : (decresult * mod * signat) = 
	 let 
	     val (sbnd_ce_list,module,sig_actual) = xstrexp(context,strb,Ast.NoSig)
	     val (_,context') = add_context_sbnd_ctxts(context,sbnd_ce_list)
	     val sig_target = xsigexp(context,sigexp)
	     val mod_var = fresh_named_var "inner_mod"
	 in  if (Sig_IsSub(context,sig_target,sig_actual))
		 then
		     (sbnd_ce_list,module, sig_actual)
	     else let val (mod'_body,sig_ret') = xcoerce_seal(context',mod_var,sig_actual,sig_target)
		      val resmod =  MOD_LET(mod_var,module, MOD_SEAL(mod'_body, sig_target))
		  in  (sbnd_ce_list,resmod, sig_target)
		  end
	 end
      | xstrexp (context, strb, Ast.Transparent sigexp) = 
	let 
	    val (sbnd_ce_list,module,signat) = xstrexp(context,strb,Ast.NoSig)
	    val (_,context') = add_context_sbnd_ctxts(context,sbnd_ce_list)
	    val sig' = xsigexp(context,sigexp) 
	    val orig_var = fresh_named_var "orig_var"

	    (* --- coerced_sig should not contain references to orig_var *)
	    val (coerced_mod,coerced_sig) = xcoerce_transparent (context',orig_var,signat,sig')
	    val resmod = MOD_LET(orig_var, module, 
				 MOD_SEAL(coerced_mod,
					  coerced_sig))
	in  (sbnd_ce_list,resmod, coerced_sig)
	end

     | xstrexp (context, strb, Ast.NoSig) =
	(case strb of
	     Ast.VarStr path => 
			 (case Context_Lookup(context,map symbol_label path) of
			      SOME (path,PHRASE_CLASS_MOD(m,s as (SIGNAT_STRUCTURE _))) => ([],m,s)
			    | _ => (error_region();
				    print "unbound structure: ";
				    AstHelp.pp_path path;
				    print "\n";
				    ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[]))))
	   | Ast.AppStr (_,[]) => parse_error "AppStr with no arguments"
	   | Ast.AppStr (f,[(Ast.MarkStr (se,r),flag)]) =>
		 xstrexp(context, Ast.AppStr(f,[(se,flag)]), Ast.NoSig)
	   | Ast.AppStr (funpath,[(strexp as (Ast.VarStr argpath),_)]) =>
		 (case (Context_Lookup(context,map symbol_label funpath)) of
		      SOME(_,PHRASE_CLASS_MOD(m,s as (SIGNAT_FUNCTOR(var1,sig1,sig2,_)))) => 
			  let 
			      val (sbnd_ce_list,argmod,signat) = xstrexp(context,strexp,Ast.NoSig)
			      val argmod = (mod2path argmod; argmod)
				  handle _ => elab_error "xstrexp: str path looked up to non-path"

			      val modc_v0 = fresh_named_var "v0_xcoerce"
			      val (modc_body,temp_sig1') = xcoerce_seal(context,modc_v0,signat,sig1)
			      val sig1' = sig_subst_modvar(temp_sig1',[(modc_v0, argmod)])

			      val (_, context') = add_context_sbnd_ctxts(context, sbnd_ce_list)
			      val _ = if Sig_IsSub(context',sig1',sig1)
				  then ()
				      else (error_region();  
					    print "functor application failed\n";
					    tab_region();
					    print "Expected signature";
					    pp_signat sig1;
					    print "\n";
					    tab_region();
					    print "Actual (original) signature";
					    pp_signat signat;
					    print "Actual (almost_coerced) signature";
					    pp_signat temp_sig1';
					    print "Actual (coerced) signature";
					    pp_signat sig1';
					    print "\n")

			      val newvar = fresh_named_var "coerced_structure"
			      val sig2' = sig_subst_modvar(sig2,[(var1,MOD_VAR newvar)])
			      val fsig = SIGNAT_FUNCTOR(var1,sig1',sig2,PARTIAL)
			      val fsig' = SIGNAT_FUNCTOR(var1,sig1',sig2',PARTIAL)
			      val context' = add_context_mod'(context,newvar,
							      SelfifySig(SIMPLE_PATH newvar, sig1'))
(*
			      val _ = if Sig_IsSub(context',fsig,fsig')
					  then ()
				      else (error_region();  
					    print "functor application failed\n";
					    tab_region();
					    print "Expected signature";
					    pp_signat fsig';
					    print "\n";
					    tab_region();
					    print "Actual signature";
					    pp_signat fsig;
					    print "\n")

*)

			      val temp = mod_subst_modvar(modc_body,
							  [(modc_v0,argmod)])
			      val sealed = MOD_SEAL(temp,sig1')
			      val lbl = to_export_lab(fresh_internal_label "coerced")
			      val sbnd_ce = 
				  (SOME(SBND(lbl,BND_MOD(newvar,sealed))),
				   CONTEXT_SDEC(SDEC(lbl,DEC_MOD(newvar,sig1'))))
			  in (sbnd_ce_list @ [sbnd_ce],
			      MOD_APP(m,MOD_VAR newvar),
			      sig2')
			  end

(*
		   print "sig_issub failed: want fsig < fsig'\n  fsig = ";
		   pp_signat fsig; print "\n  fsig' = ";
		   pp_signat fsig';
		   print "\n\ncontext' is\n";
		   pp_context context';
		   print "\n";
		   print "sig1' < sig1 is ";
		   print (Bool.toString (Sig_IsSub(context',sig1',sig1)));
		   print "\n";
		   print "sig2 < sig2' is ";
		   print (Bool.toString (Sig_IsSub(add_context_mod'(context',var1,
								   SelfifySig(SIMPLE_PATH var1, sig1')),
						   sig2,sig2')));
		   print "\n";
*)

	            | SOME _ => (error_region();
			    print "cannot apply a non-functor\n";
			    ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[])))
	            | NONE => (error_region();
			    print "identifier not bound";
			       AstHelp.pp_path funpath;
			       print "\n";
			    ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[]))))
	   | Ast.AppStr (_,[(strexp,_)]) => parse_error "AppStr applied to a non-path: we should be in named form"
	   | Ast.AppStr (_,_) => (error_region();
				  print "higher order functors not supported\n";
				  ([],MOD_STRUCTURE[],SIGNAT_STRUCTURE(NONE,[])))
	   | Ast.LetStr (dec,strexp) => (* rule 254 *) 
		 let val (var1,var2) = (fresh_var(), fresh_var())
		     val (lbl1,lbl2) = (fresh_open_internal_label "lbl1",
					fresh_open_internal_label "lbl2")
		     val boolsbnd_sdec_list = xdec'(context,dec)
		     val (mod1,sig1) = boolsbnd_ctxt_list2modsig boolsbnd_sdec_list
		     val context' = add_context_mod(context,lbl1,var1,
						    SelfifySig(SIMPLE_PATH var1, sig1)) (* <-- inline ? *)
		     val (sbnd_ce_list,mod2,sig2) = xstrexp(context',strexp,Ast.NoSig)
		     val final_mod = MOD_STRUCTURE [SBND(lbl1,BND_MOD(var1,mod1)),
						    SBND(lbl2,BND_MOD(var2,mod2))]
		     val final_sig = SIGNAT_STRUCTURE(NONE,[SDEC(lbl1,DEC_MOD(var1,sig1)),
							    SDEC(lbl2,DEC_MOD(var2,sig2))])
(*
		     val sig2' = sig2
		     val final_mod = MOD_PROJECT(MOD_STRUCTURE [SBND(lbl1,BND_MOD(var1,mod1)),
								SBND(lbl2,BND_MOD(var2,mod2))],
						 lbl2)
		     val final_sig = SIGNAT_STRUCTURE(NONE, [SDEC(lbl2,DEC_MOD(var2,sig2'))])
*)
		 in (sbnd_ce_list,final_mod, final_sig)
		 (*
		     if (Sig_IsSub(context',sig2,sig2'))
			then (sbnd_ce_list,final_mod, final_sig)
		    else old_err_or "Rule 254 failed" *)
		 end
	   | Ast.StructStr dec => 
		 let 
		     val sbnd_ctxt_list = xdec(context,dec)
		     val sbnds = List.mapPartial #1 sbnd_ctxt_list
		     val sdecs = List.mapPartial (fn (_,CONTEXT_SDEC sdec) => SOME sdec
		   | _ => NONE) sbnd_ctxt_list
		 in ([], MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE(NONE, sdecs))
		 end
	   | Ast.MarkStr (strexp,r) => let val _ = push_region r
					   val res = xstrexp(context,strexp,Ast.NoSig)
					   val _ = pop_region()
				       in res
				       end)
	     
	     

    (* --------------------------------------------------------- 
      ------------------ STRUCTURE BINDINGS --------------------
      --------------------------------------------------------- *)
     and xstrbinds (context : context, strbs : Ast.strb list) 
	 : (sbnd option * context_entry) list =
       let val strbs = map strb_strip strbs
	   fun help (n,(d,c)) = let val (sbnd_ce_list,m,s) = xstrexp(context,d,c)
				    val v = fresh_named_var "strbindvar"
				    val l = symbol_label n
				in  sbnd_ce_list @ [(SOME(SBND(l,BND_MOD(v,m))),
						     CONTEXT_SDEC(SDEC(l,DEC_MOD(v,s))))]
				end
       in flatten(map help strbs)
       end

    (* --------------------------------------------------------- 
      ------------------ SIGNATURE PATCHING -------------------
      --------------------------------------------------------- *)
    and xsig_wheretype(ctxt,orig_sdecs,lbls, con, kind) : sdecs =
      let 
	  local val fv = con_free_convar con
	  in    fun bound v = if (member_eq(eq_var,v,fv)) 
				  then (error_region();
					print "signature wheretype leads to variable capture\n")
			      else ()
	  end
	  fun docon curl sdecs : sdecs =
	      (case sdecs of
		   [] => (error_region();
			  print "signature wheretype could not find specificed component\n";
			  tab_region();
			  pp_pathlist pp_label' lbls;
			  print "\n";
			  [])
		 | ((sdec as SDEC(l,dec))::rest) => 
		       (case dec of
			    DEC_CON(v,k,NONE) => 
				(bound v; 
				 if eq_label(l,curl)
				     then if (kind_eq_shape(k,kind))
					      then (SDEC(l,DEC_CON(v,k,SOME con)))::rest
					  else (error_region();
						print "signature wheretype failed due to constructor arity\n";
						print "\nExpected kind: ";
						pp_kind k;
						print "\nActual kind: ";
						pp_kind kind; print "\n";
						sdecs)
				 else sdec::(docon curl rest))
			  | (DEC_EXP(v,_) | DEC_MOD(v,_) | DEC_CON(v,_,_)) => (bound v; sdec::(docon curl rest))
			  | DEC_EXCEPTION _ => sdec::(docon curl rest)))
	  fun dosig [] sdecs = elab_error "xsig_wheretype got empty lbls"
	    | dosig [l] sdecs = docon l sdecs
	    | dosig (curl::restl) sdecs = 
	      let 
		  fun loop [] : sdecs = (error_region();
					 print "signature wheretype could not find specificed component\n";
					 [])
		    | loop (sdec::rest) = 
		      (case sdec of
			   (SDEC(l,DEC_MOD(v,SIGNAT_STRUCTURE(NONE, sdecs)))) => 
			       (bound v; if eq_label(l, curl) 
					     then (SDEC(l,DEC_MOD(v,SIGNAT_STRUCTURE
								  (NONE, dosig restl sdecs))))::rest
					 else sdec::(loop rest))
			 | (SDEC(l,DEC_MOD(v,SIGNAT_STRUCTURE (SOME _,sdecs)))) =>
			       loop ((SDEC(l,DEC_MOD(v,SIGNAT_STRUCTURE (NONE, sdecs))))::rest)
			 | SDEC(l,(DEC_EXP(v,_) | DEC_MOD(v,_) | DEC_CON(v,_,_))) => 
			       (bound v; sdec::(loop rest))
			 | SDEC(l,DEC_EXCEPTION _) => sdec::(loop rest))
	      in loop sdecs
	      end		
      in case (follow_labels (orig_sdecs,ctxt) lbls) of 
	  SOME lbls => dosig lbls orig_sdecs
	| NONE => (error_region();
		   print "cannot where type bad sig\n";
		   orig_sdecs)
      end


  and type_is_abstract(v,CON_MODULE_PROJECT(m,l)) = 
	let fun loop acc (MOD_VAR v') = if eq_var(v,v') then SOME acc else NONE
              | loop acc (MOD_PROJECT(m,l)) = loop (l::acc) m
              | loop _ _ = NONE
        in loop [l] m
        end
    | type_is_abstract _ = NONE

  and xsig_sharing_structure(ctxt,sdecs,paths) : sdecs = 
      let exception LocalError
      in
	  let
	      type lpath = label list
	      val mjunk = MOD_VAR(fresh_named_var "mjunk")
	      fun path2sdecs p = (case (Sdecs_Lookup'(mjunk,sdecs,map symbol_label p)) of
				      SOME(l,PHRASE_CLASS_MOD(_,SIGNAT_STRUCTURE(_,sd))) => (l,sd)
				    | _ => (error_region();
					    print "structure sharing given a non-structure component\n";
					    raise LocalError))
	      val lpath_sdecs_list : (lpath * sdecs) list = map path2sdecs paths
	      fun getcomponents (lpath,sdecs) : (lpath * lpath list) = 
		  let
		      fun traverse (SDEC(l,DEC_CON _)) = [[l]]
			| traverse (SDEC(l,DEC_MOD (v,SIGNAT_STRUCTURE(_,sdecs)))) = 
			  let val lpaths = List.concat (map traverse sdecs)
			  in map (fn lpath => (l :: lpath)) lpaths
			  end
			| traverse _ = []
		  in (lpath, List.concat (map traverse sdecs))
		  end
	      val lpath_lpaths_list : (lpath * lpath list) list = map getcomponents lpath_sdecs_list
	      val lpaths = #2 (hd lpath_lpaths_list)
	      val num_types = length lpaths
	      val _ = if (andfold (fn (_,lpaths) => length lpaths = num_types) lpath_lpaths_list)
			  then ()
		      else (error_region();
			    print "structure sharing failed\n";
			    raise LocalError)
	      val labels : lpath list list = (map (fn (lpath,lpaths) => map (fn lps => lpath @ lps) lpaths) 
					      lpath_lpaths_list)
	      val labels_list : lpath list list = mapmap (fn l => case follow_labels (sdecs,ctxt) l of
							  SOME lbls => lbls
							| NONE => raise LocalError) labels
	      val labels_list = Listops.transpose labels_list
	      fun folder (labels : label list list,sdecs) = xsig_sharing_rewrite(sdecs,labels)
	  in (foldl folder sdecs labels_list)
	  end
      handle LocalError => sdecs
      end

  and follow_labels (sdecs,ctxt) =
      let val v = fresh_named_var "modtemp"
	  val s = SIGNAT_STRUCTURE(NONE, sdecs)
	  val ctxt = add_context_mod'(ctxt,v,SelfifySig(SIMPLE_PATH v, s))
	  fun result labels : label list option =
	      let val c = path2con(COMPOUND_PATH(v,labels))
		  val c' = con_normalize(ctxt,c)
	      in  case (type_is_abstract(v,c')) of
		  SOME lbls => SOME lbls
		| NONE => (error_region();
			   print "can't share non-abstact type component";
			   pp_con c'; print "\n";
			   NONE)
	      end
      in result
      end
  and xsig_sharing_type(ctxt,sdecs,path) : sdecs = 
      let exception LocalError
	  val mjunk = MOD_VAR(fresh_named_var "mjunk")
	  fun path2label p = (case (Sdecs_Lookup'(mjunk,sdecs,map symbol_label p)) of
				  SOME(l,_) => l
				| NONE => (error_region();
					   print "sharing type got a non-existent component\n";
					   raise LocalError))
      in  
	  let val labels = map path2label path
	      val labels = map (fn l => case (follow_labels (sdecs,ctxt) l) of 
				SOME lbls => lbls
			      | NONE => raise LocalError) labels
	  in xsig_sharing_rewrite(sdecs,labels)
	  end
      handle LocalError => sdecs
      end

      (* labels is a list of paths (relative to the sdecs) to opaque type
	 components;  we search for the one that occurs first and
         then transparently type-abbreviate all of the rest to the first one *)
      and xsig_sharing_rewrite(sdecs,labels) : sdecs = 
        let local val firstpath = ref NONE
            in fun transparent (curpath : (var * label) list) = 
		case (!firstpath) of
		  SOME firstpath => 
		      let fun vlpath2con [] = elab_error "transparent got empty path"
			    | vlpath2con ((v,_)::vlrest) = 
				    let val p = COMPOUND_PATH(v,map #2 vlrest)
				    in  SOME(path2con p)
				    end
			  fun combine_path first [] = vlpath2con first
			    | combine_path [] _ = error "bad paths"
			    | combine_path (first as ((v,_)::firstrest))
			      ((v',_)::secondrest) = 
			      if (eq_var(v,v')) 
				  then combine_path firstrest secondrest
			      else vlpath2con first
		      in  combine_path firstpath curpath
		      end
                | NONE => (firstpath := SOME curpath; NONE)

	    end
            fun match l labels = 
	    let fun folder(lab::labs,(match,mismatch)) = 
			if (eq_label(l,lab))
				then (labs::match,mismatch)
			else (match,(lab::labs)::mismatch)
	          | folder([],_) = elab_error "xsig_sdecs_rewrite: match failed"
            in foldl folder ([],[]) labels
	    end
	    fun traverse _ [] = [] 
	      | traverse (_,[]) sdecs = sdecs
              | traverse (cur_path,labels) ((SDEC(l,dec))::rest) =
		let val (match_lab,labels) = match l labels
                    val dec' = 
			if (length match_lab = 0) 
			then dec 
			else case dec of
		      DEC_CON(v,k,copt) => 
			DEC_CON(v,k,transparent (cur_path @ [(v,l)]))
                    | DEC_MOD(v,SIGNAT_STRUCTURE(popt,sdecs)) =>
			let val sdecs' = traverse(cur_path@[(v,l)],match_lab) sdecs
			in DEC_MOD(v,SIGNAT_STRUCTURE(popt,sdecs'))
			end
                    | _ => dec
                in (SDEC(l,dec')) :: (traverse (cur_path,labels) rest)
	        end
	in traverse ([],labels) sdecs
        end

    (* --------------------------------------------------------- 
      ------------------ COERCION COMPILATION-------------------
      --------------------------------------------------------- *)

    and xcoerce_seal (context : context,
		      var_actual : var,
		      sig_actual : signat,
		      sig_target : signat) : Il.mod * Il.signat =
	    let val (m,s,_) = xcoerce_help(empty_mapping,
					   true,
					   add_context_mod'(context,var_actual,
							SelfifySig(SIMPLE_PATH var_actual, sig_actual)),
					   SIMPLE_PATH var_actual,
					   sig_actual,sig_target)
	    in  (m,s)
	    end


    (* Augment coerced_mod and coerced_sig so that all references to
      var_actual are removed from coerced_sig.  *)
    and extract_hidden(coerced_mod, coerced_sig, var_actual, sig_actual) : mod * signat = 
	let 
	    val var_local = Name.fresh_named_var "hidden_module"
	    val label_local = Name.fresh_internal_label "hidden_module"
	    fun labels2path_local labs = join_path_labels(SIMPLE_PATH var_local, labs)
	    fun labels2path_actual labs = join_path_labels(SIMPLE_PATH var_actual, labs)
	    (* first, we compute the components that we need to extract and 
		convert the projections to use var_local instead of var_actual *)
	    local 
		val neededpaths = ref ([] : (label list * con) list)
		fun eq_labs(labs1,labs2) = Listops.eq_list(eq_label,labs1,labs2)
		fun add_path p = (case (assoc_eq(eq_labs, p, !neededpaths)) of
				      SOME c => c
				    | NONE => let val c = path2con(labels2path_local p)
					      in neededpaths := (p,c) :: (!neededpaths); c
					      end)
		fun chandle (CON_MODULE_PROJECT (m,l)) = 
		    let fun loop labs (MOD_VAR v) = if (eq_var(v,var_actual))
							then SOME(add_path labs)
						    else NONE
			  | loop labs (MOD_PROJECT(m,l)) = loop (l::labs) m 
			  | loop labs _ = NONE
		    in  loop [l] m
		    end
		  | chandle _ = NONE
	    in
		val coerced_sig = sig_all_handle(coerced_sig, fn _ => NONE, chandle, fn _ => NONE, fn _ => NONE)
	        val neededpaths = !neededpaths
	    end (* local *)


            (* we create the augmentation module and name it var_local; 
	      note that only the label of the var_local must be hidden;
	      for now, we copy all type components *)
	    fun dosdec (p,mapping : mapping, sdec) = 
		case sdec of
		    SDEC(l,DEC_CON(v,k,copt)) => 
			let val v' = Name.derived_var v
			    val k' = kind_substconmod(k,mapping)
			    val copt' = Util.mapopt (fn c => con_substconmod(c,mapping)) copt
			    val c' = path2con(labels2path_actual(rev(l::p)))
			    val mapping = convar_addmap(v,CON_VAR v',mapping)
			    val mapping = (case p of
					       [] => conproj_addmap(c',CON_VAR v',mapping)
					     | _ => mapping)
			in  SOME(mapping,
				 SBND(l,BND_CON(v',c')),
				 SDEC(l,DEC_CON(v',k',copt')))
			end
		  | SDEC(l,DEC_EXP(v,c)) => 
			if (is_eq_lab l)
			    then
				let val v' = Name.derived_var v
				    val e = path2exp(labels2path_actual(rev(l::p)))
				    val c' = con_substconmod(c,mapping)
				in  SOME(mapping,
					 SBND(l,BND_EXP(v',e)),
					 SDEC(l,DEC_EXP(v',c')))
				end
			else NONE
		  | SDEC(l,DEC_MOD(v,s)) => 
			(case dosig(l::p, mapping, s) of
			     SOME(_,m,s) => let val v' = Name.derived_var v
					      val mapping = modvar_addmap(v, MOD_VAR v', mapping)
					      val m' = path2mod(labels2path_actual[l])
					      val mapping = 
						  (case p of
						       [] => modproj_addmap(m',MOD_VAR v',mapping)
						     | _ => mapping)
					  in  SOME(mapping,
						   SBND(l,BND_MOD(v',m)), 
						   SDEC(l,DEC_MOD(v',s)))
					  end
			   | NONE => NONE)
		  | _ => NONE
	    and dosdecs (p,mapping,[]) = (mapping,[],[])
	      | dosdecs (p,mapping,sdec::rest) = 
		let val (mapping,sbsdopt) = (case dosdec(p,mapping,sdec) of
					   NONE => (mapping, NONE)
					 | SOME(mapping,sbnd,sdec) => (mapping,SOME(sbnd,sdec)))
		    val (mapping,restsbnds,restsdecs) = dosdecs(p,mapping,rest)
		in  (case sbsdopt of
			 NONE => (mapping,restsbnds,restsdecs)
		       | SOME(sbnd,sdec) => (mapping,sbnd::restsbnds, sdec::restsdecs))
		end
            and dosig (p, mapping, (SIGNAT_STRUCTURE(_,sdecs)
	                 | SIGNAT_INLINE_STRUCTURE{imp_sig=sdecs,...})) =  
		let val (mapping,sbnds,sdecs) = dosdecs(p,mapping,sdecs)
		in  SOME(mapping,MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE(NONE,sdecs))
		end
	      | dosig _ = NONE
	    val (mapping,augment_m,augment_s) = (case dosig([],empty_mapping,sig_actual) of
						     SOME mapmodsig => mapmodsig
						   | NONE => (print "sig_actual is not a structure\n";
							      elab_error "sig_actual is not a structure"))
	    val coerced_sig = sig_substconmod(coerced_sig,mapping)
	    val sbnd_augment = SBND(label_local,BND_MOD(var_local, augment_m))
	    val sdec_augment = SDEC(label_local,DEC_MOD(var_local, augment_s))
	    val res as (m,s) = case (coerced_mod, coerced_sig) of
		(MOD_STRUCTURE sbnds, SIGNAT_STRUCTURE (popt,sdecs)) =>
		    (MOD_STRUCTURE (sbnd_augment::sbnds), SIGNAT_STRUCTURE (popt,sdec_augment::sdecs))
	      | _ => error "extract_hidden given coerced_mod/sig not strutures"

	in  res
	end

    and xcoerce_transparent (context : context,
			     var_actual : var,
			     sig_actual : signat,
			     sig_target : signat) : Il.mod * Il.signat =
	let val _ = if (!debug_coerce)
			then (print "xcoerce_transparent just started... sig_actual = \n";
			      pp_signat sig_actual; 
			      if (!debug_coerce_full)
				  then (print "\nand sig_target = \n";
					pp_signat sig_target)
			      else ();
			      print "\n\n")
		    else ()
	    (* first call perform an opaque sealing for type-checking reasons *)
	    val (coerced_mod,coerced_sig,mapping : mapping) = 
		xcoerce_help(empty_mapping,
			     true,
			     add_context_mod'(context,var_actual,
					      SelfifySig(SIMPLE_PATH var_actual, sig_actual)),
			     SIMPLE_PATH var_actual,
			     sig_actual,sig_target)
(*
	    (* second call performs transparent sealing using the obtained coerced_sig *)
	    val (m,s,mapping) = xcoerce_help(empty_mapping,
					     false,
					     add_context_mod'(context,var_actual,
							      SelfifySig(SIMPLE_PATH var_actual, sig_actual)),
					     SIMPLE_PATH var_actual,
					     sig_actual,coerced_sig)
	    (* now we need to get rid of references to var_actual *)
	    val s' = sig_substcon(s,mapping)
*)
	    (* first we get rid of some references to var_actual with internal variables *)
	    val coerced_sig' = sig_substconmod(coerced_sig,mapping)
	    val _ = if (!debug_coerce)
			then (print "xcoerce_transparent about to call extract_hidden..\n";
			      if (!debug_coerce_full)
			      then (print "csubst =";
				    app (fn (c1,c2) => (pp_con c1; print "  --> "; pp_con c2; print "\n")) 
				    (getcclist mapping))
			      else ();
			      print "\nand coerced_sig' = \n";
			      pp_signat coerced_sig'; 
			      print "\n\n")
		    else ()
	    (* now, we must augment with additional hidden types and modules;
	       note that coerced_sig' does not contain internal variables of sig_actual;
	       but has projections from var_actual *)
	    val (m,s) = extract_hidden(coerced_mod, coerced_sig', var_actual, sig_actual)
	in  (m, s)
	end

    (* The resulting module will involve var_actual while the signature references var_actual only
        if opqaue is false.  The resulting signature will be ordered as sig_target
        if opaque is true; otherwise; the resulting signature will be ordered as in
	 sig_actual and contain components mentioned in sig_target and hidden components. 
	 The main algorithm maintains:
	       The current context to perform typecheck signature matching.
	       The signature whose order we are using to traverse and create
	         the final module and signature.
              The current list of labels indicating our current position.
	          This list allows us to look up components in the 
		    sig_actual and sig_target to type-check. *)
    and xcoerce_help (mapping : mapping,
		      opaque : bool,
		      context : context,
		      path_actual : path,
		      sig_actual : signat,
		      sig_target : signat) : Il.mod * Il.signat * mapping =
      let 


	  fun makecon(path,labs) = path2con(join_path_labels(path,labs))

	  val _ =  if (!debug_coerce)
		       then (print "trying to xcoerce with signat_actual = \n";
			     pp_signat sig_actual; 
			     if (!debug_coerce_full)
				 then (print "\nand signat_target = \n";
				       pp_signat sig_target)
			     else ();
(*			     print "\nand ctxt = \n"; pp_context context;  *)
			     print "\n")
		   else ()

	  local
	      fun sig_lookup (p,s) lbls : (label list * phrase_class) option = 
		  (case s of
		       SIGNAT_STRUCTURE (_,self_sdecs) =>
			   Sdecs_Lookup'(path2mod p, self_sdecs, lbls)
		     | SIGNAT_INLINE_STRUCTURE {abs_sig = self_sdecs,...} =>
			   Sdecs_Lookup'(path2mod p, self_sdecs, lbls)
		     | SIGNAT_FUNCTOR _ => NONE)
	      fun find lbl [] : dec option = NONE
		| find lbl (SDEC(l,d)::rest) = 
		  if (eq_label(l,lbl))
		      then SOME d
		  else (case (is_label_open l, d) of
			    (true, DEC_MOD(_,(SIGNAT_STRUCTURE(_,sdecs) 
		                             | SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...}))) =>
			    (case find lbl sdecs of
				 NONE => find lbl rest
			       | decopt => decopt)
			  | _ => find lbl rest)
 
          fun dec_lookup (SIGNAT_FUNCTOR _) _ = NONE
	    | dec_lookup (SIGNAT_STRUCTURE (_,sdecs) | 
			  SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...}) lbls = 
	      (case lbls of
		   [] => NONE
		 | (lbl::rest) => let val decopt = find lbl sdecs
				  in  case (rest,decopt) of
				      ([],SOME dec) => SOME dec
				    | (_,SOME(DEC_MOD(_,s))) => dec_lookup s rest
				    | _ => NONE
				  end)
	      val var_target = Name.fresh_named_var "xcoerce_var_target"
	      val sig_actual_self = SelfifySig (path_actual, sig_actual)
	  in  val dec_lookup = dec_lookup
	      val sig_actual_lookup = sig_lookup (path_actual,sig_actual_self)
	      val dec_actual_lookup = dec_lookup sig_actual 
	      val dec_target_lookup = dec_lookup sig_target
	  end

        (* ---- coercion of a polymorphic component to a mono or polymorphic specification --- *)
	fun polyval_case mapping (ctxt : context) (labs,v,con : con,varsig_option) 
	    : (bnd * dec * context * mapping) option = 
	    let 
		exception Bad of int
		(* dispatch is passed the actual signature *)
		 fun local_error(expected_con,actual_con) = 
		     (error_region();
		      print "Coercion of a polymorphic value component to a\n";
		      error_region();
		      print "  monomorphic/polymorphic specification failed at ";
		      pp_pathlist pp_label' labs;
		      print "\n";
		      error_region();
		      print "Expected type: ";
		      pp_con expected_con;
		      print "\n";
		      error_region();
		      print "Actual type: ";
		      pp_con actual_con;
		      print "\n";
		      NONE)
		 fun dispatch(path,s as SIGNAT_FUNCTOR(var_poly,sig_poly as SIGNAT_STRUCTURE (NONE,sig_poly_sdecs),
						      SIGNAT_STRUCTURE(NONE,[SDEC(maybe_it_lab,DEC_EXP(vp,con'))]),_)) = 
		    let val v' = Name.derived_var v
			val ctxt' =  add_context_mod'(ctxt,var_poly,SelfifySig(SIMPLE_PATH var_poly,sig_poly))
			val ctxt' = (case varsig_option of
					 NONE => ctxt'
				       | SOME(v1,s1) => add_context_mod'(ctxt',v1,
									 SelfifySig(SIMPLE_PATH v1, s1)))
			val (sbnds_poly,sdecs_poly,_) = poly_inst(ctxt',sig_poly_sdecs)
			(* can't use con instead of con'' *)
			fun folder (SBND(l,BND_CON(v,c)),csubst) =
			    ((CON_MODULE_PROJECT(MOD_VAR var_poly, l), c)::csubst)
			  | folder (_,csubst) = csubst
			val csubst = foldl folder [] sbnds_poly
			fun chandle c = assoc_eq(eq_conproj,c,csubst)
			val con'' = con_all_handle(con', fn _ => NONE,
						   chandle, fn _ => NONE,
						   fn _ => NONE)
		    in  case varsig_option of
			NONE => let val mtemp = MOD_APP(path2mod path,MOD_STRUCTURE sbnds_poly)
				in if (not opaque orelse
				       sub_con(ctxt',con'',con_substconmod(con,mapping)))
				       then 
					   let val mapping = convar_addmap(v,CON_VAR v', mapping)
					       val mapping = conproj_addmap(makecon(path_actual,labs), 
									    CON_VAR v', mapping)
					   in  SOME(BND_EXP(v',MODULE_PROJECT(mtemp,it_lab)),
						 DEC_EXP(v',con''),
						 ctxt, mapping)
					   end
				   else (local_error(con_substconmod(con,mapping),con'');
					 raise (Bad 4))
				end
		      | SOME (v1,s1) => 
				let val mtemp = MOD_APP(path2mod path,MOD_STRUCTURE sbnds_poly)
				    val s2 = SIGNAT_FUNCTOR(v1,s1,
							    SIGNAT_STRUCTURE(NONE,[SDEC(it_lab,
											DEC_EXP(fresh_var(),con''))]),
							    TOTAL)
				in  if (not opaque orelse
					sub_con(ctxt',con'',con_substconmod (con,mapping)))
					then 
					    let val mapping = convar_addmap(v,CON_VAR v', mapping)
						val mapping = conproj_addmap(makecon(path_actual,labs), 
									     CON_VAR v', mapping)
					    in  SOME(BND_MOD(v',MOD_FUNCTOR(v1,s1,mtemp)),
						  DEC_MOD(v', s2),
						  ctxt, mapping)
					    end
				    else (local_error(con_substconmod(con,mapping),con'');
					  raise (Bad 5))
				end
		    end
		  | dispatch _ = (error_region(); raise (Bad 6))
	    in  (case (sig_actual_lookup labs) of
			 SOME (lbls,PHRASE_CLASS_MOD (_,s)) => 
			     (case s of
				  SIGNAT_FUNCTOR _ => dispatch(join_path_labels(path_actual,lbls),s)
				| SIGNAT_STRUCTURE(_,[SDEC(mk_lab,dec),_]) => (* exceptions *)
				      (case dec of
					   DEC_MOD(v,s) => dispatch(join_path_labels(path_actual,lbls @ [mk_lab]), s)
					 | _ => (error_region(); raise (Bad 1)))
				| _ => (error_region(); raise (Bad 2)))
		       | _ => (error_region(); raise (Bad 3)))
		handle Bad i => NONE
	    end (* polyval_case *)

		  
	fun doit mapping ctxt labs : (bnd * dec * context * mapping) option = 
	    let 
		val _ = if (!debug_coerce)
			    then (print "\n\ndoit called with labs = ";
				  pp_pathlist pp_label' labs; 
				  if (!debug_coerce_full)
				      then (print "\nand mapping = \n";
					    app (fn (c1,c2) => (print "  "; pp_con c1;
								print "  -->  "; pp_con c2;
								print "\n")) (getcclist mapping))
				  else ();
				  print "\n\n\n")
			else ()
		fun general_mod v =
		(case (sig_actual_lookup labs) of
		     SOME(lbls,PHRASE_CLASS_MOD (_,s1)) =>
		     let 
			 val s = (case (is_label_open (hd lbls), dec_target_lookup labs, sig_target) of
				      (_,SOME(DEC_MOD (_,s)),_) =>  s
				    | (true,_,SIGNAT_STRUCTURE(popt,sdecs1)) =>
					  SIGNAT_STRUCTURE(popt,
					  List.filter (fn(SDEC(l,_)) =>
						 case dec_lookup s1 [l] of
						       NONE => false
						     | SOME _ => true) sdecs1)
				    | _ => SIGNAT_STRUCTURE(NONE,[]))
			 val (mbody,sig_ret,mapping') = 
			     xcoerce_help(mapping,opaque,ctxt,join_path_labels(path_actual,lbls),s1,s)
(*			 val mapping = join_maps(mapping, mapping') *)
			 val mapping = mapping'
			 val v1 = fresh_var()
			 val v' = Name.derived_var v
			 val bnd = BND_MOD(v',mbody)
			 val dec = DEC_MOD(v',sig_ret)
(*			 val ctxt = add_context_dec(ctxt,SelfifyDec(DEC_MOD(v,s1))) *)
			 val ctxt = add_context_dec(ctxt,SelfifyDec(DEC_MOD(v,sig_ret)))
		     in SOME(bnd,dec,ctxt,mapping)
		     end
		   | _ => (error_region();
			   print "coercion of a non-structure component to a ";
			   print "structure specification failed: ";
			   pp_pathlist pp_label' labs;
			   print "\n";
			   NONE))
	    in
		(case (dec_target_lookup labs, dec_actual_lookup labs) of
                   (* ------- coercion to a monomorphic value specificiation ---- *)
		     (SOME(DEC_EXP(v,c)),_) =>
			 (case (sig_actual_lookup labs) of
			      SOME(lbls,PHRASE_CLASS_EXP (_,con)) => 
				  let val _ = (debugdo (fn () => 
					    (print "Looking up with path_actual = "; pp_path path_actual; 
					     print " with labs = "; pp_pathlist pp_label' labs;
					     print "\nand sig_actual = "; pp_signat sig_actual;
					     print "\ngot back "; pp_con con; print "\n")))
				      val c' = con_substconmod(c,mapping)
				  in
				      if (not opaque orelse
					  sub_con(ctxt,con,c'))
				      then
					  let 
					      val v' = Name.derived_var v
					      val bnd = BND_EXP(v',path2exp(join_path_labels(path_actual,lbls)))
					      val dec = DEC_EXP(v',con_substconmod(con,mapping))
					      val extdec = DEC_EXP(v,con)
					  in SOME(bnd,dec,ctxt,mapping)
					  end 
				      else (error_region();
					    print "coercion of a monomorphic value component to a\n";
					    error_region();
					    print "monomorphic value specification failed\n";
					    print "Component name: ";
					    pp_pathlist pp_label' labs;
					    print "\nExpected type:\n";
					    pp_con c;
					    print "\nFound type:\n";
					    pp_con con;
					    print "\nExpected expanded type:\n";
					    pp_con (con_normalize(ctxt,c));
					    print "\nFound expanded type:\n";
					    pp_con (con_normalize(ctxt,con));
					    print "\n";
					    NONE)
				  end
			    | SOME(lbls,PHRASE_CLASS_MOD (_,s)) => 
				  polyval_case mapping ctxt (labs,v,c,NONE)  (* rule 250 *)
			    | SOME(_,PHRASE_CLASS_CON _) => 
				  (error_region();
				   print "value specification but type component";
				   NONE)
			    | SOME(_,PHRASE_CLASS_SIG _) => 
				  (error_region();
				   print "value specification but signature component";
				   NONE)
			    | SOME(_,PHRASE_CLASS_OVEREXP _) => 
				  (error_region();
				   print "value specification but OVEREXP";
				    NONE)
			    | NONE => (error_region();
				       pp_pathlist pp_label' labs;
				       print " component in signature not in structure\n";
				       NONE))
		   | (_, SOME(DEC_EXP _)) => NONE
	           (* ----- check for polymorphic specification case first ---- *)
		   | (SOME(DEC_MOD(v,s as (SIGNAT_FUNCTOR(v1,s1,SIGNAT_STRUCTURE (NONE,
					 [SDEC(maybe_it,DEC_EXP(_,c))]),_)))),_) =>
		     if (eq_label(maybe_it,it_lab))
			 then polyval_case mapping ctxt (labs,v,c,SOME(v1,s1))
		     else general_mod v 
                   (* ---------- coercion of non-polyfun module component ---------------- *)
         	   | (SOME(DEC_MOD(v,s)),_) => general_mod v 
                   (* ---------- must keep non-polyfun module components for transparent ascription *) 
         	   | (_,SOME(DEC_MOD(v,s))) =>
			 if (not opaque andalso is_label_internal(List.last labs)
			     andalso not (is_label_open(List.last labs)))
			     orelse
			     (case s of
				  (SIGNAT_FUNCTOR(v1,s1,SIGNAT_STRUCTURE (NONE,
				     [SDEC(maybe_it,DEC_EXP(_,c))]),_)) => eq_label(maybe_it,it_lab)
				       | _ => false)
			     then NONE
			 else general_mod v
		   (* ------- coercion of a type component to a type spec ---- *)
		   | (SOME(DEC_CON(v,tk,copt)),actual_decopt) =>
			 (case (sig_actual_lookup labs) of
			      SOME(lbls,PHRASE_CLASS_CON (con,k)) => 
				  (* con is typically a path from var_actual *)
				  let (* val con' = path2con(join_path_labels(path_actual,lbls)) *)
				      val v' = Name.derived_var v
				      val ctxt = add_context_dec(ctxt,DEC_CON(v,k,SOME con))
				      val ctxt = add_context_dec(ctxt,DEC_CON(v',k,SOME con))
				      val mapping = convar_addmap(v,con,mapping)
				      val mapping = conproj_addmap(makecon(path_actual,labs), 
								   CON_VAR v', mapping)
				      val (mapping,actual_con) = 
					  (case actual_decopt of
					       SOME(DEC_CON(va,_,copt)) => 
						   (convar_addmap(va, CON_VAR v',mapping), copt)
					     | _ => (mapping, NONE))
				      val _ = (case copt of 
						   NONE => ()
						 | SOME spec_con => 
						       if (not opaque orelse
							   sub_con(ctxt,con,spec_con))
							   then ()
						       else (let val con' = con_normalize(ctxt,con)
								 val spec_con' = con_normalize(ctxt,spec_con)
							     in  error_region();
								 print "coercion of a type component to a ";
								 print "type specification failed at ";
								 pp_pathlist pp_label' labs;
								 print "\nExpected type: ";
								 pp_con spec_con;
								 print "\nActual type: ";
								 pp_con con;
								 print "\nReduced Expected type: ";
								 pp_con spec_con';
								 print "\nReduced Actual type: ";
								 pp_con con';
								 print "\n"
							     end))
				      val k' = kind_substconmod(k,mapping)
				      val bnd = BND_CON(v',con)
				      val dec = DEC_CON(v',k',SOME con)
				  in SOME(bnd,dec,ctxt,mapping)
				  end
			    | _ => (error_region();
				    print "coercion of a non-type or non-existent component to a ";
				    print "type specification failed at ";
				    pp_pathlist pp_label' labs;
				    print "\n";
				    NONE))
		   (* ------- must keep type components for transparent ascription *)
		   | (_,SOME(DEC_CON(v,k,copt))) =>
			      (case (sig_actual_lookup labs) of
				   SOME(lbls,PHRASE_CLASS_CON (_,knd)) => 
				       let val con' = path2con(join_path_labels(path_actual,lbls))
					   val v' = Name.derived_var v
					   val bnd = BND_CON(v',con')
					   val dec = DEC_CON(v',k,copt)
					   val ctxt = add_context_dec(ctxt,DEC_CON(v,k,copt))
					   val mapping = convar_addmap(v,con',mapping)
					   val mapping = conproj_addmap(makecon(path_actual,labs), CON_VAR v',
									mapping)
				       in  SOME(bnd,dec,ctxt,mapping)
				       end
				 | _ => error "traversing according to sig_actual but lookup failed")
		   | _ => elab_error "ill-formed specification")
	    end

	fun sdecs_loop lbls (mapping,ctxt,sdecs) : sbnd list * sdec list * mapping =
	    let fun loop (m,ctxt,[]) = ([],[],m)
		  | loop (m,ctxt,(SDEC(l,_))::rest) =
		     (case (doit m ctxt (lbls @ [l])) of
			  SOME (resbnd,resdec,ctxt',mapping) =>
			      let val (sbnds,sdecs,mapping2) = loop(mapping,ctxt',rest)
			      in ((SBND(l,resbnd))::sbnds, 
				  (SDEC(l,resdec))::sdecs,
				  mapping2)
			      end
			| NONE => loop(m,ctxt,rest))
	    in  loop (mapping,ctxt,sdecs)
	    end

	fun sbnds_loop lbls (mapping,ctxt,sdecs) (sbnds_code,sdecs_imp) = 
	    let val (sbnds,sdecs,mapping) = sdecs_loop lbls (mapping,ctxt,sdecs)
		fun sbnd_find lbl [] = error "sbnd_find failed"
		  | sbnd_find lbl ((sbnd as SBND(l,_))::rest) = if (Name.eq_label(l,lbl)) then sbnd 
								else sbnd_find lbl rest
		fun sdec_find lbl [] = error "sdec_find failed"
		  | sdec_find lbl ((sdec as SDEC(l,_))::rest) = if (Name.eq_label(l,lbl)) then sdec
								else sdec_find lbl rest
		val sbnds_code = map (fn (SDEC(l,_)) => sbnd_find l sbnds_code) sdecs
		val sdecs_imp = map (fn (SDEC(l,_)) => sdec_find l sdecs_imp) sdecs
	    in  (sbnds,sbnds_code,sdecs_imp,sdecs,mapping)
	    end

	val (m,s,mapping : mapping) = 
	    (case (opaque,sig_actual,sig_target) of
		       (_,SIGNAT_FUNCTOR(v1,s1,s1',a1), SIGNAT_FUNCTOR(v2,s2,s2',a2)) =>
			 let 
			   val _ = if (a1 = a2) then () 
				   else raise (FAILURE "arrow mismatch in xcoerce")
			   val (m3body,_,_) = xcoerce_help(mapping,
							   true,
							   add_context_mod'(context,v2,
									    SelfifySig(SIMPLE_PATH v2, s2)),
							   SIMPLE_PATH v2,s2,s1)
			   val m4_arg = MOD_APP(path2mod path_actual, m3body)
			   val m4var = fresh_named_var "var_actual_xcoerce"
			   val (m4body,_,_) = xcoerce_help(mapping,
							   true,
							   add_context_mod'(context,m4var,
									    SelfifySig(SIMPLE_PATH m4var,s1')),
							   SIMPLE_PATH m4var,s1',s2')
			   val m4body = mod_subst_modvar(m4body,[(m4var,m4_arg)])
			   val context' = add_context_mod'(context,v2,(SelfifySig(SIMPLE_PATH v2,s2)))
			   val s = GetModSig(context',m4body)
			 in (MOD_FUNCTOR(v2,s2,m4body),
			     SIGNAT_FUNCTOR(v2,s2,s,a1),
			     empty_mapping) (* XXX empty mapping ? *)
			 end
		   | (true,_,SIGNAT_STRUCTURE (_,sdecs)) =>
			 let val (sbnds,sdecs,mapping) = sdecs_loop [] (mapping,context,sdecs)
			 in (MOD_STRUCTURE sbnds,
			     SIGNAT_STRUCTURE (NONE, sdecs),
			     mapping)
			 end
		   | (false,SIGNAT_STRUCTURE (_,sdecs),_) =>
			 let val (sbnds,sdecs,mapping) = sdecs_loop [] (mapping,context,sdecs)
			 in (MOD_STRUCTURE sbnds,
			     SIGNAT_STRUCTURE (NONE, sdecs),
			     mapping)
			 end
		   | (true,((SIGNAT_STRUCTURE (NONE,sdecs)) | (SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...})),
			 SIGNAT_INLINE_STRUCTURE {self,code,imp_sig,abs_sig}) =>
			   let val (sbnds,sbnds_code,imp_sdecs,abs_sdecs,mapping) =
			       sbnds_loop [] (mapping,context,sdecs) (code,imp_sig)
			   in (MOD_STRUCTURE sbnds,
			       SIGNAT_INLINE_STRUCTURE {self = self,
							code = sbnds_code,
							imp_sig = imp_sdecs,
							abs_sig = abs_sdecs},
			       mapping)
			   end
		   | (false,SIGNAT_INLINE_STRUCTURE {self,code,imp_sig,abs_sig},
			   ((SIGNAT_STRUCTURE (NONE,sdecs)) | (SIGNAT_INLINE_STRUCTURE{abs_sig=sdecs,...}))) =>
			   let val (sbnds,sbnds_code,
				    imp_sdecs,abs_sdecs,mapping) = sbnds_loop [] (mapping,context,sdecs) (code,imp_sig)
			   in (MOD_STRUCTURE sbnds,
			       SIGNAT_INLINE_STRUCTURE {self = self,
							code = sbnds_code,
							imp_sig = imp_sdecs,
							abs_sig = abs_sdecs},
			       mapping)
			   end
		   | (_,SIGNAT_FUNCTOR _, ((SIGNAT_STRUCTURE _) | (SIGNAT_INLINE_STRUCTURE _))) => 
			   (error_region();
			    print "cannot coerce a functor to a structure\n";
			    (MOD_STRUCTURE [], SIGNAT_STRUCTURE(NONE,[]),empty_mapping))
		  | (_,((SIGNAT_STRUCTURE _) | (SIGNAT_INLINE_STRUCTURE _)),SIGNAT_FUNCTOR _) => 
			   (error_region();
			    print "cannot coerce a structure to a functor\n";
			    (MOD_STRUCTURE [], SIGNAT_STRUCTURE(NONE,[]),empty_mapping)))

	val _ = if (!debug_coerce) 
		    then (print "\n\nxcoerce result:\n";
			  if (!debug_coerce_full)
			      then (print "\nmodule:\n";
				    pp_mod m;
				    print "\nsig:\n";
				    pp_signat s)
			  else ();
			  print "\n")
		else ()

      in (m,s,mapping)
      end


    and xeqopt (ctxt : context, argcon : con) : exp option = 
	SOME(xeq_hidden(ctxt,argcon)) handle _ => NONE

    and xeq (ctxt : context, argcon : con) : exp = 
	(xeq_hidden (ctxt, argcon))
	handle NoEqExp => (debugdo (fn () => (print "no equality at this type:\n";
					      pp_con (con_normalize(ctxt,argcon))));
			   raise NoEqExp)

    and xeq_hidden (ctxt : context, argcon : con) : exp = 
      let
	  val _ = debugdo (fn () => (print "CALLED xeq with con = ";
				     pp_con argcon; print "\nand ctxt = \n";
				     pp_context ctxt))
	  val con' = con_normalize(ctxt,argcon) 
	  val _ = debugdo (fn () => (print "NORMALIZE to con = ";
				     pp_con con'; print "\n"))
	  fun self c = xeq_hidden(ctxt,c)
	  open Prim
      in (case con' of
	    CON_TYVAR tyvar => (case (tyvar_deref tyvar) of
				NONE => elab_error "unresolved type does not permit equailty"
			      | SOME c => self c)
	  | CON_VAR v => (let val type_label = (case (Context_Lookup'(ctxt,v)) of
						    SOME(l,_) => l
						  | _ => raise NoEqExp)
			      val eq_label = to_eq_lab type_label
			  in (case (Context_Lookup(ctxt,[eq_label])) of
				  SOME(_,PHRASE_CLASS_EXP(e,_)) => e
				| _ => raise NoEqExp)
			  end)
	  | CON_OVAR ocon => self (CON_TYVAR (ocon_deref ocon))
	  | CON_INT is => ETAPRIM(eq_int is,[])
	  | CON_UINT is => ETAILPRIM(eq_uint is,[])
(*	  | CON_FLOAT fs => ETAPRIM(eq_float fs,[]) *)
	  | CON_FLOAT fs => raise NoEqExp
	  | CON_RECORD fields => 
		let 
		    val v = fresh_var()
		    val v1 = fresh_var()
		    val v2 = fresh_var()
		    val paircon = con_tuple[con',con']
		    val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
		    val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
		    fun help (lbl,fieldcon) = 
			let 
			    val eqexp = self fieldcon
			    val e1 = RECORD_PROJECT(VAR v1,lbl,con')
			    val e2 = RECORD_PROJECT(VAR v2,lbl,con')
			in APP(eqexp,[exp_tuple[e1,e2]])
			end
		    fun folder (rdec,exp) = 
			let val exp' = help rdec
			in make_ifthenelse(exp,exp',false_exp,con_bool)
			end
		    val body = (case fields of
				    [] => true_exp
				  | (fst::rest) => foldl folder (help fst) rest)
		in #1(make_lambda(v,paircon,con_bool,
				  make_let([(v1,e1),(v2,e2)],body)))
		end
	  | CON_SUM {carriers,noncarriers,special} =>
		let 
		    val v = fresh_named_var "eqargpair"
		    val v1 = fresh_named_var "eqarg1"
		    val v2 = fresh_named_var "eqarg2"
		    val paircon = con_tuple[con',con']
		    val e1 = RECORD_PROJECT(VAR v,generate_tuple_label 1,paircon)
		    val e2 = RECORD_PROJECT(VAR v,generate_tuple_label 2,paircon)
		    val totalcount = (noncarriers + length carriers)
		    fun help i = let val var' = fresh_named_var "eqarg1"
				     val var'' = fresh_named_var "eqarg2"
				     val is_carrier = i >= noncarriers
				     val sumc = CON_SUM{carriers=carriers,
							noncarriers=noncarriers,
							special = SOME i}
				     val armbody = if is_carrier
						       then APP(self(List.nth(carriers,i-noncarriers)),
								[exp_tuple[SUM_TAIL(sumc,VAR var'),
									  SUM_TAIL(sumc,VAR var'')]])
						   else true_exp
				     val arms2 = map0count 
					 (fn j =>
					  if (i=j) 
					      then 
						  SOME (if is_carrier
							    then (#1(make_lambda(var'',
										 sumc,
										 con_bool,
										 armbody)))
							else armbody)
					  else NONE) totalcount
				     val switch = CASE{noncarriers = noncarriers,
						       carriers = carriers,
						       arg = VAR v2,
						       arms = arms2,
						       default = SOME false_exp,
						       tipe = con_bool}
				 in SOME (if is_carrier
					      then #1(make_lambda(var', sumc,
								  con_bool,
								  switch))
					  else switch)
				 end
		    val arms1 = map0count help totalcount
		    val body = CASE{noncarriers = noncarriers,
					  carriers = carriers,
					  arg = VAR v1,
					  arms = arms1,
					  default = NONE,
					  tipe = con_bool}
(*		    val body = make_catch(inner_body,con_bool,match_exp,con_unit,false_exp) *)
		in #1(make_lambda(v,paircon,con_bool,
				  make_let([(v1,e1),(v2,e2)],body)))
		end
	  | CON_ARRAY c => ETAPRIM(equal_table WordArray,[c])
(*	  | CON_VECTOR c => APP(ETAPRIM(equal_table WordVector,[c]),self c) *)
	  | CON_VECTOR c => 
		let val (e,vc,_) = xexp(ctxt,Ast.VarExp[Symbol.varSymbol "vector_eq"])
		    val ac = CON_ARROW([CON_ARROW([con_tuple[c,c]],con_bool,false,oneshot())],
				       CON_ARROW([con_tuple[CON_VECTOR c,CON_VECTOR c]],con_bool,false,oneshot()),
				       false, oneshot())
		    val _ = if (eq_con(ctxt,vc,ac))
				then ()
			    else (elab_error "Prelude vector_eq is bad")
		in  APP(e,[self c]) 
		end
	  | CON_REF c => ETAPRIM(eq_ref,[c])
	  | CON_MODULE_PROJECT(m,l) => 
		let val e = MODULE_PROJECT(m,to_eq_lab l)
		in (GetExpCon(ctxt,e) 
			handle _ => raise NoEqExp);
		  	e
	       end
	  | CON_APP(c,tuple) => 
		let val meq = (case c of
				   CON_MODULE_PROJECT(m,l) => MOD_PROJECT(m,to_eq_lab l)
				 | CON_VAR v => 
				       (let val type_label = (case (Context_Lookup'(ctxt,v)) of
								  SOME(l,_) => l
								| _ => (print "noeq 1\n"; raise NoEqExp))
					    val eq_label = to_eq_lab type_label
					in (case (Context_Lookup(ctxt,[eq_label])) of
						SOME(_,PHRASE_CLASS_MOD(m,_)) => m
					      | _ => (print "noeq 2\n";
						      raise NoEqExp))
					end))
		in case (GetModSig(ctxt,meq)
			 handle _ => (print "noeq3\n"; raise NoEqExp)) of
		    SIGNAT_FUNCTOR(_,SIGNAT_STRUCTURE (NONE, sdecs),
				   SIGNAT_STRUCTURE(NONE, [res_sdec]),_) => 
			let 
			    val types = (case tuple of
					     CON_TUPLE_INJECT cons => cons
					   | c => [c])
			    fun translucentfy [] [] = []
			      | translucentfy [] _ = elab_error "arity mismatch in eq compiler"
			      | translucentfy ((SDEC(l,DEC_CON(v,k,NONE)))::
					       (sdec2 as (SDEC(_,DEC_EXP _))) :: rest) (c::crest) =
				((SDEC(l,DEC_CON(v,k,SOME c)))::sdec2::
				 (translucentfy rest crest))
			      | translucentfy ((SDEC(l,DEC_CON(v,k,NONE)))::rest) (c::crest) = 
				((SDEC(l,DEC_CON(v,k,SOME c)))::(translucentfy rest crest))
			      | translucentfy _ _ = elab_error "got strange sdec in eq compiler"
			    val sdecs = translucentfy sdecs types
			    val (new_sbnds,new_sdecs,new_types) = poly_inst(ctxt,sdecs)
			in MODULE_PROJECT(MOD_APP(meq,MOD_STRUCTURE new_sbnds),it_lab)
			end
		  | _ => (print "noeq 4\n"; raise NoEqExp)
		end
	  | CON_MUPROJECT (j,con') => 
		(case GetConKind(ctxt,con') of
		     KIND_TUPLE _ => elab_error "cannot perform equality on con tuples"
		   | KIND_ARROW(n,m) => 
			 let
			     val _ = if (m=n) then () else elab_error "datatype constructor must have kind n=>n"
			     val mu_cons = map0count (fn i => CON_MUPROJECT(i,con')) n
			     val vars_eq = map0count (fn i => fresh_named_var ("vars_eq_" ^ (Int.toString i))) n
			     val type_lbls = map0count (fn i => fresh_internal_label("lbl" ^ (Int.toString i))) n
			     val eq_lbls = map to_eq_lab type_lbls
			     val evars = map0count (fn i => fresh_named_var ("evar" ^ (Int.toString i))) n
			     val cvars = map0count (fn i => fresh_named_var ("cvar" ^ (Int.toString i))) n
			     val elist = zip evars (map VAR vars_eq)
			     val clist = zip cvars mu_cons
			     fun cfolder ((cvar,cl),ctxt) = 
				 let val dec = DEC_CON(cvar,KIND_TUPLE 1,NONE)
				 in add_context_sdec(ctxt,SDEC(cl,SelfifyDec dec))
				 end
			     fun efolder ((evar,cvar,el),ctxt) = 
				 let 
				     val con = CON_ARROW([con_tuple[CON_VAR cvar, CON_VAR cvar]],
							 con_bool,false,oneshot_init PARTIAL)
				     val dec = DEC_EXP(evar,con)
				 in add_context_sdec(ctxt,SDEC(el,SelfifyDec dec))
				 end
			     val ctxt = foldl cfolder ctxt (zip cvars type_lbls)
			     val ctxt = foldl efolder ctxt (zip3 evars cvars eq_lbls)

			     local
				 val applied = ConApply(false,con',con_tuple_inject (map CON_VAR cvars))
			     in
				 val reduced_cons = (case applied of
							 CON_TUPLE_INJECT conlist => conlist
						       | c => [c])
			     end
			     val exps_v = map (fn c => xeq_hidden(ctxt,c)) reduced_cons

			     fun make_expeq (mu_con,expv) = 
				 let
				     val var = fresh_named_var "arg_pair"
				     val var_con = con_tuple[mu_con,mu_con]
				     val expv' = exp_subst_expconmodvar(expv,elist,clist,[])
				     val e1 = RECORD_PROJECT(VAR var,generate_tuple_label 1,var_con)
				     val e2 = RECORD_PROJECT(VAR var,generate_tuple_label 2,var_con)
				     val e1' = UNROLL(mu_con,e1)
				     val e2' = UNROLL(mu_con,e2)
				 in (var,APP(expv',[exp_tuple[e1',e2']]))
				 end
			     val exps_eq = map2 make_expeq (mu_cons,exps_v)
			     val fbnds = map3 (fn (mu_con,vareq,(vararg,expeq)) =>
					       FBND(vareq,vararg,con_tuple[mu_con,mu_con],
						    con_bool,expeq))
				 (mu_cons,vars_eq,exps_eq)
			     val fbnd_types = 
				 map (fn mu_con => CON_ARROW([con_tuple[mu_con,mu_con]],
							     con_bool,false,
							     oneshot_init PARTIAL))
				 mu_cons
			     val fix_exp = FIX(true,PARTIAL,fbnds)
			 in case fbnds of
			     [fbnd] => fix_exp
			   | _ => RECORD_PROJECT(fix_exp, generate_tuple_label j,
						 con_tuple fbnd_types)
			 end)
	  | _ => raise NoEqExp)
      end

    (* ------------ Exported interface to resolve overloading ------------ *)
    fun overload_wrap fp xobj arg = 
      let 
	fun flex_help (l,ref(FLEXINFO(stamp,false,rdecs)),fieldc,eshot) = 
	    (error_region();
	     print "Unresolved flex record with label: ";
	     pp_label l; print "\nrdecs are:";
	     pp_con (CON_RECORD rdecs); print "\nfieldc is:";
	     pp_con fieldc; print "\n")
	  | flex_help (l,ref(FLEXINFO(_,true,rdecs)),fieldc,eshot) = 
	    let val v = fresh_named_var "flex_eta_var"
		val recc = CON_RECORD rdecs
		val body = RECORD_PROJECT(VAR v,l,recc)
		val (e,c) = make_lambda(v,recc,fieldc,body)
	    in oneshot_set(eshot,e)
	    end
	  | flex_help (l,ref(INDIRECT_FLEXINFO fr),fieldc,eshot) = flex_help (l,fr,fieldc,eshot)
	fun eq_help (reg,decs,tyvar,exp_oneshot) = 
	  let val _ = push_region reg
	      val con = CON_TYVAR tyvar
	      val res = (case xeqopt(decs,con) of
			     SOME e => oneshot_set(exp_oneshot,e)
			   | NONE => (error_region();
				      print "no equality at type: ";
				      pp_con con; print "\n"))
	      val _ = pop_region()
	  in res
	  end

	fun tyvar_help tv = 
	(case (Tyvar.tyvar_deref tv) of
			SOME _ => ()
		      | NONE => (print "Warning: top-level unresolved tyvar -- setting to type unit\n"; 
				 Stats.counter "toil.unresolved_tyvar" ();
				 Tyvar.tyvar_set(tv,con_unit)))
			
	fun overload_loop warn overload_table = 
	    let fun folder (entry,(rest,change)) = 
		if (overload_help false entry)
		    then (rest,true)
		else (entry::rest, change)
		val (rest,change) = foldl folder ([],warn) overload_table
	    in  if warn
		    then ()
		else (if change 
			  then overload_loop warn rest
		      else overload_loop true rest)
	    end

	val _ = reset_elaboration fp
	val _ = push_region(0,1000000)
	val _ = eq_table_push()
	val res = xobj arg
	val _ = eq_table_pop (DEC_MOD(fresh_named_var "dummy", SIGNAT_STRUCTURE(NONE,[])))
        val tyvar_table = get_tyvar_table()
	val overload_table = get_overload_table()
	val flex_table = get_flex_table()
        val _ = overload_loop false overload_table 
	val _ = app flex_help flex_table 
        val _ = app tyvar_help tyvar_table

	fun eq_loop 10 = elab_error "eq_loop reached 10"
	  | eq_loop n = 
	    let val eq_table = get_eq_table()
	    in case eq_table of
		[] => ()
	      | _ => let val _ = debugdo (fn() =>
					  (print "eq_table had "; print (Int.toString (length eq_table));
					   print " things\n"))
(*			 val _ = reset_elaboration fp *)
			 val _ = reset_eq()
			 val _ = eq_table_push()
			 val _ = app eq_help eq_table 
			 val _ = eq_table_pop (DEC_MOD(fresh_named_var "dummy", 
						       SIGNAT_STRUCTURE(NONE,[])))
		     in eq_loop (n+1)
		     end
	    end
	val _ = eq_loop 1
      in (case (get_elaboration_error()) of
	      NoError => SOME res
	    | Warn => SOME res
	    | Error => NONE)
      end
    
    val xdec = fn (ctxt,fp,dec) => overload_wrap fp xdec (ctxt,dec)
    val xexp = fn (ctxt,fp,exp) => overload_wrap fp xexp (ctxt,exp)
    val xstrexp = fn (ctxt,fp,strexp,sigc) => overload_wrap fp xstrexp (ctxt,strexp,sigc)
    val xspec = fn (ctxt,fp,specs) => overload_wrap fp xspec (ctxt,specs)
    val xsigexp = fn (ctxt,fp,se) => overload_wrap fp xsigexp (ctxt,se)
    val xty = fn (ctxt,fp,ty) => overload_wrap fp xty (ctxt,ty)
    val xtybind = fn (ctxt,fp,tyb) => overload_wrap fp xtybind (ctxt,tyb)
    val xeq = xeqopt

    fun zfp (_ : Ast.srcpos) = ("zfp",0,0)
    val xsig_sharing_type = fn arg => overload_wrap zfp xsig_sharing_type arg
    val xsig_sharing_structure = fn arg => overload_wrap zfp xsig_sharing_structure arg
    val xsig_wheretype = fn arg => overload_wrap zfp xsig_wheretype arg

  end;



