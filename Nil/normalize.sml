structure Normalize :> NORMALIZE =
struct

  structure NilContext = NilContextPre


  val number_flatten = Nil.flattenThreshold

  val profile       = Stats.ff "nil_profile"
  val local_profile = Stats.ff "normalize_profile"

  val SpecializeArrayofChar = CompilerControl.SpecializeArrayofChar

  val subtimer = fn args => fn args2 => if !profile orelse !local_profile then Stats.subtimer args args2 else #2 args args2

  open Nil
  open Prim

  type con_subst = NilSubst.con_subst

  val substConInKind= fn s => subtimer("Norm:substConInKind",NilSubst.substConInKind s)
  val substConInExp = fn s => subtimer("Norm:substConInExp",NilSubst.substConInExp s)
  val substConInCon = fn s => subtimer("Norm:substConInCon",NilSubst.substConInCon s)
  val substExpInCon = fn s => subtimer("Norm:substExpInCon",NilSubst.substExpInCon s)

  val empty         = NilSubst.C.empty
  val add           = NilSubst.C.sim_add
  val addr          = NilSubst.C.addr
  val substitute    = NilSubst.C.substitute
  val fromList      = NilSubst.C.simFromList
  val printConSubst = NilSubst.C.print

  val makeLetC             = NilUtil.makeLetC
  val is_var_c             = NilUtil.is_var_c
  val strip_var            = NilUtil.strip_var
  val strip_crecord        = NilUtil.strip_crecord
  val strip_proj           = NilUtil.strip_proj
  val strip_prim           = NilUtil.strip_prim
  val strip_app            = NilUtil.strip_app
  val generate_tuple_label = NilUtil.generate_tuple_label
  val primequiv            = NilUtil.primequiv

  (*From NilRename*)
  val renameExp         = NilRename.renameExp
  val alphaCRenameExp   = NilRename.alphaCRenameExp
  val alphaCRenameCon   = NilRename.alphaCRenameCon
  val alphaCRenameKind  = NilRename.alphaCRenameKind
  val alphaECRenameCon  = NilRename.alphaECRenameCon
  val alphaECRenameKind = NilRename.alphaECRenameKind

  (*From Name*)
  val eq_var          = Name.eq_var
  val eq_var2         = Name.eq_var2
  val eq_label        = Name.eq_label
  val fresh_named_var = Name.fresh_named_var
  fun fresh_var ()    = fresh_named_var "normalize"
  val derived_var     = Name.derived_var
  val label2string    = Name.label2string
  val var2string      = Name.var2string

  (*From Listops*)
  val map_second = Listops.map_second
  val foldl_acc  = Listops.foldl_acc
  val foldl2     = Listops.foldl2
  val map        = Listops.map
  val map2       = Listops.map2
  val zip        = Listops.zip
  val unzip      = Listops.unzip
  val all        = Listops.all
  val all2       = Listops.all2

  (*From Util *)
  val eq_opt  = Util.eq_opt
  val map_opt = Util.mapopt
  val printl  = Util.printl
  val lprintl = Util.lprintl

  (* NilContext *)
  type context   = NilContext.context
  val find_kind  = NilContext.find_kind
  val kind_of    = NilContext.kind_of
  val find_con   = NilContext.find_con
  val insert_con = NilContext.insert_con

  val find_kind_equation = NilContext.find_kind_equation
  val print_context      = NilContext.print_context

  val insert_kind          = NilContext.insert_kind
  val insert_kind_equation = NilContext.insert_kind_equation
  val insert_equation      = NilContext.insert_equation

  fun error' s = Util.error "normalize.sml" s
  fun error s s' = Util.error s s'

  val assert   = NilError.assert
  val locate   = NilError.locate "Normalize"
  val perr_k_k = NilError.perr_k_k


  val NormalizeDiag = Stats.ff "NormalizeDiag"
  fun msg str = if (!NormalizeDiag) then print str else ()

  val debug = Stats.ff "normalize_debug"
  val show_calls = Stats.ff "normalize_show_calls"
  val show_context = Stats.ff "normalize_show_context"

  val warnDepth = 1000
  val maxDepth = 10000


  local
      datatype entry =
	EXP of exp * (NilContext.context * (con_subst))
      | CON of con * (NilContext.context  * (con_subst))
      | KIND of kind * (NilContext.context * (con_subst))
      | BND of bnd * (NilContext.context * (con_subst))
      | MODULE of module * (NilContext.context * (con_subst))
      val stack = ref ([] : entry list)

      val depth = ref 0
      fun push e = (depth := !depth + 1;
		    stack := (e :: (!stack));
		    if (!depth mod 20 = 0)
			then msg ("  *** stack depth = " ^
				  Int.toString (!depth) ^ "\n")
		    else ();
		    if (!depth) > maxDepth
			then error' "stack depth exceeded"
		    else ())
  in
    fun clear_stack() = (depth := 0; stack := [])
    fun push_exp (e,context) = push (EXP(e,context))
    fun push_con(c,context) = push(CON(c,context))
    fun push_kind(k,context) = push(KIND(k,context))
    fun push_bnd(b,context) = push(BND(b,context))
    fun push_mod(m,context) = push(MODULE(m,context))
    fun pop() = (depth := !depth - 1;
		 stack := (tl (!stack)))
    fun show_stack() = let val st = !stack
			   val _ = clear_stack()
			   fun show (EXP(e,(context,s))) =
			     (print "exp_normalize called with expression =\n";
			      Ppnil.pp_exp e;
			      print "\nand context";
			      NilContext.print_context context;
			      print "\n and subst";
			      printConSubst s;
			      print "\n\n")
			     | show (CON(c,(context,s))) =
				     (print "con_normalize called with constructor =\n";
				      Ppnil.pp_con c;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
			     | show (KIND(k,(context,s))) =
				     (print "kind_normalize called with kind =\n";
				      Ppnil.pp_kind k;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
			     | show (BND(b,(context,s))) =
				     (print "bnd_normalize called with bound =\n";
				      Ppnil.pp_bnd b;
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
			     | show (MODULE(m,(context,s))) =
				     (print "module_normalize called with module =\n";
				      Ppnil.pp_module
                                        {module = m,
                                         header = "",
                                         name = "",
                                         pass = ""};
				      print "\nand context"; NilContext.print_context context;
				      print "\n and subst";  printConSubst s;
				      print "\n\n")
		       in  app show (rev st)
		       end
    fun wrap str f =
	(clear_stack(); f())
	handle e => (print "\n ------ ERROR in "; print str; print " ---------\n";
		     show_stack(); raise e)
    fun wrap1 str f arg1 = wrap str (fn () => f arg1)
    fun wrap2 str f arg1 arg2 = wrap str (fn () => f arg1 arg2)
  end


  fun beta_conrecord' (proj as Proj_c (con,label)) =
	(case strip_crecord con
	   of SOME entries =>
	     (case (List.find (fn ((l,_)) => eq_label (l,label))
		    entries )
		of SOME (l,c) => (true,c)
		 | NONE => (error' "Field not in record" handle e => raise e))
	    | NONE => (false,proj))
    | beta_conrecord' proj =
	   (Ppnil.pp_con proj;
	    (error' "beta_conrecord called on non-projection" handle e => raise e))

  fun beta_conrecord proj = #2(beta_conrecord' proj)

  val beta_conrecord = subtimer("Norm:beta_conrecord",beta_conrecord)

  fun eta_confun lambda =
    let
	fun help(var, formals,body) =
	    (case strip_app body of
		 SOME (con,actuals) =>
		     let
			 val (vars,_) = unzip formals
			 fun eq (var,con) = eq_opt (eq_var,SOME var,strip_var con)
		     in
			 if (all2 eq (vars,actuals)) andalso
			     (let
				  val fvs = NilUtil.freeConVarInCon(true,0,con)
			      in  all (fn v => not(Name.VarSet.member(fvs,v))) vars
			      end)
			     then
				 con
			 else
			     lambda
		     end
	       | NONE => lambda)
      fun eta_confun' (Let_c (sort,[Open_cb (var,formals,body)],Var_c var')) =
	  if (eq_var(var,var')) then help(var,formals,body) else lambda
        | eta_confun' (Let_c (sort,[Code_cb (var,formals,body)],Var_c var')) =
	  if (eq_var(var,var')) then help(var,formals,body) else lambda
	| eta_confun' _ = lambda
    in
      eta_confun' lambda
    end

  (*and beta_typecase D typecase =
    let
      fun beta_typecase'
	(Typecase_c {arg,arms,default,kind}) =
	(case strip_prim arg
	   of SOME (pcon,args) =>
	     (case List.find (fn (pcon',formals,body) => primequiv (pcon,pcon')) arms
		of SOME (_,formals,body) =>
		  let
		    val (vars,_) = unzip formals
		    val subst = fromList (zip vars args)
		  in con_normalize' (D,subst) body
		  end
		 | NONE => default)
	    | _ => typecase)
	| beta_typecase' _ =
	   (Ppnil.pp_con typecase;
	    (error' "beta_typecase called on non type case" handle e => raise e))
    in
      map beta_typecase' typecase
    end*)


  and beta_confun once D app = #2(beta_confun' once D app)

  and beta_confun' once D (app as (App_c (con,actuals))) =
    let

      exception NOT_A_LAMBDA

      fun strip (Open_cb (var,formals,body)) = (var,formals,body)
	| strip (Code_cb (var,formals,body)) = (var,formals,body)
	| strip _ = raise NOT_A_LAMBDA

      fun get_lambda (lambda,name) =
	let
	  val (var,formals,body) = strip lambda
	in
	  (case name
	     of Var_c var' =>
	       if eq_var (var,var') then
		 (formals,body)
	       else raise NOT_A_LAMBDA
	      | _ => raise NOT_A_LAMBDA)
	end

      fun lambda_or_closure (Let_c (_,[lambda],name)) = (get_lambda (lambda,name),NONE)
	| lambda_or_closure (Closure_c(code,env)) =
	let val (args,_) = lambda_or_closure code
	in  (args,SOME env) end
	(*| lambda_or_closure (Annotate_c(_,con)) = lambda_or_closure (con)*)
	| lambda_or_closure _ = raise NOT_A_LAMBDA


      fun open_lambda cfun = (SOME (lambda_or_closure cfun)) handle NOT_A_LAMBDA => NONE

      fun reduce actuals (formals,body) =
	(true,
	 let
	   val (vars,_) = unzip formals
	   val subst = fromList (zip vars actuals)
	 in if once
	      then substConInCon subst body
	    else (con_normalize' (D,subst) body)
	 end)


    in
      (case open_lambda con
	 of SOME(args,SOME env) =>
	       reduce (env::actuals) args
	  | SOME(args,NONE)     =>
	       reduce actuals args
	  | NONE => (false,app))
    end
    | beta_confun' _ _ con =
    (Ppnil.pp_con con;
     (error' "beta_confun called on non-application" handle e => raise e))


  and insert_kind (D,var,kind) = NilContext.insert_kind (D,var,kind)
  and bind_at_kind ((var,kind),(D,subst)) =
    let
      val kind = kind_normalize' (D,subst) kind
      val var' = if ((find_kind(D,var); true)
			handle NilContext.Unbound => false)
		     then derived_var var
		 else var
      val D = insert_kind (D,var',kind)
      val subst = add subst (var,Var_c var')
    in ((var',kind),(D,subst))
    end

  and bind_at_kinds state vklist = foldl_acc bind_at_kind state vklist

  and bind_at_con ((var,con),(D,subst)) =
    let
      val con = con_normalize' (D,subst) con
      val var' = if ((find_con(D,var); true)
			handle NilContext.Unbound => false)
		     then derived_var var
		 else var
      val D = insert_con (D,var',con)
      val subst = add subst (var,Var_c var')
    in ((var',con),(D,subst))
    end

  and bind_at_tracecon ((var,trace,con),state) =
      let val ((v,c),state) = bind_at_con ((var,con),state)
      in ((v,trace,c),state)
      end

  and bind_at_cons state vclist = foldl_acc bind_at_con state vclist
  and bind_at_tracecons state vtclist = foldl_acc bind_at_tracecon state vtclist

  and kind_normalize' (state as (D,subst)) (kind : kind) : kind =
    if !debug then
      let
	val _ = push_kind(kind,state)
	val _ = if (!show_calls)
		  then (print "kind_normalize called with kind =\n";
			Ppnil.pp_kind kind;
			(if (!show_context)
			   then (print "\nand context"; NilContext.print_context D;
				 print "\n and subst";  printConSubst subst)
			 else ());
			 print "\n\n")
		else ()
	val res = kind_normalize state kind
	val _ = pop()
      in  res
      end
    else
      kind_normalize state kind
  and kind_normalize state (kind : kind) : kind =
    (case kind of
          Type_k => kind
	| SingleType_k con => SingleType_k(con_normalize' state con)
	| Single_k con => Single_k(con_normalize' state con)
	| Record_k elts =>
	 let
	   val elt_list = Sequence.toList elts
	   val (labels,vars_and_kinds) = unzip (map (fn ((l,v),k) => (l,(v,k))) elt_list)
	   val (vars_and_kinds,state) =  bind_at_kinds state vars_and_kinds
	   val elts = map2 (fn (l,(v,k)) => ((l,v),k)) (labels,vars_and_kinds)
	 in
	   Record_k (Sequence.fromList elts)
	 end
	| Arrow_k (openness, formals, return) =>
	 let
	   val (formals,state) = bind_at_kinds state formals
	   val return = kind_normalize' state return
	 in
	   (Arrow_k (openness, formals,return))
	 end)

  and con_normalize' (state as (D,subst)) (con : con) : con =
    if !debug then
      let
	val _ = push_con(con,state)
	val _ = if (!show_calls)
		  then (print "con_normalize called with con =\n";
			Ppnil.pp_con con;
			if (!show_context)
			    then (print "\nand context"; NilContext.print_context D)
			else ();
			print "\n and subst";  printConSubst subst;
			print "\n\n")
		else ()
	val res = con_normalize state con
	val _ = pop()
      in  res
      end
    else
      con_normalize state con

  and con_normalize_letfun state (sort,constructor,var,formals,body,rest,letbody) =
	    let
	      val old_state = state


	    in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var letbody)
		   then
		       let val (formals,state) = bind_at_kinds state formals
			   val body = con_normalize' state body
			   val lambda = (Let_c (sort,[constructor (var,formals,body)],
						Var_c var))
			   val lambda = eta_confun lambda
		       in  lambda
		       end
	       else
		   let

		       val lambda = (Let_c (sort,[constructor (var,formals,body)],
					    Var_c var))
		       val _ =
			   (case (substitute (#2 old_state) var)  of
				SOME c => error' "XXX var already in subst"
			      | _ => ())
		       val lambda = substConInCon (#2 old_state) lambda
		       val state =
			   let val (D,subst) = old_state
			   in (D,add subst (var,lambda))
			   end
		       val res = con_normalize' state (Let_c (sort,rest,letbody))

		   in   res
		   end
	    end

  (* This code does NOT appear to produce normal forms in general.
     Specifically, it only looks up equations for variables in the Var_c case.
     So for example, if v is bound with kind T * S(int), 
     then v.2 will not get normalized to int, because v does not have an equation.
     The reduce_hnf code gets it right, see below.
     Since normalize is only used by specialize.sml for an optimization, this perhaps
     doesn't matter, but eventually it should either be fixed or removed.

       -Derek
   *)

  and con_normalize state (constructor : con) : con  =
    ((*print "con_normalize ";
     Ppnil.pp_con constructor;
     print "\n";*)
     (case constructor of
          (Prim_c (pcon,args)) =>
	      let val args = map (con_normalize' state) args
	      in (Prim_c (pcon,args))
	      end
	| (Mu_c (recur,defs)) =>
	 let
	   val def_list = Sequence.toList defs
	   val (vars,cons) = unzip def_list
	   val (vars_kinds,state') =
	     bind_at_kinds state (map (fn v => (v,Type_k)) vars)
	   val (vars,_) = unzip vars_kinds
	   val cons = if recur then map (con_normalize' state') cons
		      else map (con_normalize' state) cons
	   val defs = Sequence.fromList (zip vars cons)
	 in Mu_c (recur,defs)
	 end
	| (Nurec_c (v,k,c)) => 
         let
	     val ((v,k),state) = bind_at_kind((v,k),state)
	     val c = con_normalize' state c
	 in
	     Nurec_c (v,k,c)
	 end
	| (AllArrow_c {openness,effect,
		       tFormals,eFormals,fFormals,body_type}) =>
	 let
	   val (tFormals,state) = bind_at_kinds state tFormals
	   (*fun folder ((vopt,c),state) =
	       (case vopt of
		    NONE => ((vopt, con_normalize' state c), state)
		  | SOME v =>
			let val ((v,c),state) = bind_at_con ((v,c),state)
			in  ((SOME v, c), state)
			end)
	   val (eFormals,state) = foldl_acc folder state eFormals*)
	   val eFormals = map (con_normalize' state) eFormals
	   val body_type = con_normalize' state body_type
	 in AllArrow_c{openness = openness, effect = effect,
		       tFormals = tFormals, eFormals = eFormals,
		       fFormals = fFormals, body_type = body_type}
	 end
	| ExternArrow_c (args,body) =>
	 let
	   val args = map (con_normalize' state) args
	   val body = con_normalize' state body
	 in
	   ExternArrow_c (args,body)
	 end
	| (Var_c var) =>
	 let
	   val (D,subst) = state
	   val con =
	     (case (substitute subst var) of
		   SOME c => c
		 | NONE =>
	           (case NilContext.find_kind_equation  (D,Var_c var) of
			SOME c => con_normalize' state c
		      | NONE => Var_c var)) handle e => (NilContext.print_context D; raise e)
	 in con
	 end
        | (Let_c (sort,((cbnd as Open_cb (var,formals,body))::rest),letbody)) =>
	 con_normalize_letfun state (sort,Open_cb,var,formals,body,rest,letbody)
        | (Let_c (sort,((cbnd as Code_cb (var,formals,body))::rest),letbody)) =>
	 con_normalize_letfun state (sort,Code_cb,var,formals,body,rest,letbody)

	| (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body)) =>
	    let
	      val con = con_normalize' state con
	      val state =
		let val (D,subst) = state
		in (D,add subst (var,con))
		end
	    in con_normalize' state (Let_c (sort,rest,body))
	    end
	| (Let_c (sort,[],body)) => con_normalize' state body
	| (Closure_c (code,env)) =>
	    let
	      val env = con_normalize' state env
	      val code =  con_normalize' state code
	    in Closure_c (code,env)
	    end
	| (Crecord_c (entries as orig_entries)) =>
	    let
	      val (labels,cons) = unzip entries
	      val cons = map (con_normalize' state) cons
	      val entries = zip labels cons
	      val con = Crecord_c entries
	    in con
	    end
        (*| Typeof_c e => error' "typeof encountered in con_normalize"*)
	| (Proj_c (rvals,label)) =>
	    let
	      val rvals = con_normalize' state rvals
	      val con = Proj_c (rvals,label)
	      val con = beta_conrecord con
	    in con
	    end
	| (App_c (cfun,actuals)) =>
	    let
		val (D,subst) = state
	      val cfun = con_normalize' state cfun
	      val actuals = map (con_normalize' state) actuals
	      val con = App_c (cfun,actuals)
	      val con = beta_confun false D con
	    in con
	    end
	| (Coercion_c {vars, from, to}) =>
	    let
		val (D, subst) = state
		val D = foldl (fn (v, D) => insert_kind(D, v, Type_k)) D vars
		val state = (D, subst)
		val from = con_normalize' state from
		val to = con_normalize' state to
	    in
		Coercion_c {vars = vars, from = from, to = to}
	    end
(*
	| (Typecase_c {arg,arms,default,kind}) =>
	    let
	      val kind = kind_normalize' state kind
	      fun doarm (pcon,args,body) =
		let
		  val (args,state) = bind_at_kinds state args
		  val body = con_normalize' state body
		in (pcon,args,body)
		end
	      val arg = con_normalize' state arg
	      val default = con_normalize' state default
	      val arms = map doarm arms
	      val con = Typecase_c {arg=arg,arms=arms,
				    default=default,kind=kind}
	      val con = beta_typecase (#1 state) con
	    in con
	    end
	| (Annotate_c (annot,con)) => Annotate_c (annot,con_normalize' state con)
*)
	    ) (*before print "/con_normalize\n"*))

  fun kind_normalize D = kind_normalize' (D,empty())
  fun con_normalize D = con_normalize' (D,empty())

  val beta_confun = beta_confun false

(*  val kind_normalize = wrap "kind_normalize" kind_normalize
  val con_normalize = wrap "con_normalize"  con_normalize
  val con_reduce_once = wrap "con_reduce_once" con_reduce
  val exp_normalize = wrap "exp_normalize" exp_normalize
  val module_normalize = wrap "mod_normalize" module_normalize

  val kind_normalize' = wrap "kind_normalize'" kind_normalize'
  val con_normalize' = wrap "con_normalize'"  con_normalize'
  val exp_normalize' = wrap "exp_normalize'" exp_normalize'
*)
    fun lab2int l ~1 = error' "lab2int failed"
      | lab2int l n = if (eq_label(l,NilUtil.generate_tuple_label n))
			  then n else lab2int l (n-1)



  (* ------ Reduce one step if not in head-normal-form; return whether progress was made  ------ *)
  datatype progress = PROGRESS | HNF | IRREDUCIBLE
  datatype 'a ReduceResult = REDUCED of 'a | UNREDUCED of con

    (* With the addition of Nurec_c, this should really be called "is_maybe_hnf",
       because a path starting at a Nurec_c may be reducible if it has a "natural"
       singleton kind.  However, given that is_hnf is only applied to the result of
       a call to reduce_hnf, we can be sure that the input does not have a singleton
       kind, or reduce_hnf would have reduced further.  -Derek *)
    fun is_hnf c : bool =
        (case c of
             Prim_c _ => true
           | AllArrow_c _ => true
           | ExternArrow_c _ => true
           | Var_c _ => false
           | Let_c _ => false
           | Mu_c _ => true
	   | Nurec_c _ => true
           | Proj_c (c,_) => is_hnf c
	   (*| Typeof_c _ => false*)
           | App_c (c,_) => is_hnf c
           | Crecord_c _ => true
           | Closure_c _ => error' "Closure_c not a type"
	   | Coercion_c _ => true
           (*| Typecase_c _ => false
           | Annotate_c (_,c) => false)*))


    (* Paths are not stable under substitution.
     * A return of (SOME subst,c) indicates that continuing may be fruitful
     * A return of (NONE,c) indicates that c is as far as it can be reduced
     * This process terminates because either this function tells you to stop,
     * or it empties the subst.  So if no further progress can be made,
     * the subst will be empty next time around and we will return NONE
     *)
    fun find_kind_equation_subst (D,subst,c) =
      let
	(*Since we are under a subst here, we may have an unbound variable in the path*)
	val eqnopt = (find_kind_equation(D,c)) handle Unbound => NONE
      in
	case eqnopt
	  of SOME c => (SOME subst,c)                     (*We found an equation *)
	   | NONE =>                                      (*No equation *)
	    if NilSubst.C.is_empty subst then                  (*No equation, no subst, no further progress is possible*)
	      (NONE,c)
	    else                                          (*No equation, but carrying out subst may enable reductions *)
	      let val c = substConInCon subst c
	      in
		case find_kind_equation(D,c)
		  of SOME c => (SOME (empty()),c)         (*Result of substitution had an equation, continue with empty subst*)
		   | NONE => (SOME (empty()), c)          (*Result of substitution did not have an equation, but try to
							   * beta reduce further before giving up*)
	      end
      end


    fun expandMuType(D:context, mu_con:con) =
	let

          (* 
	     This function takes as input a constructor of the form
                E[nurec v:k. c]
             that is, a path starting at a non-uniform mu-constructor.
             It returns the context extended with v':k (where v' is fresh)
             the con-binding "v' = mu v:k. c", and the constructor E[c[v'/v]].  
	     The body of expandMuType below will first call
	     this function, then reduce E[c[v'/v]] to a type of the form
             E[mu (x1,...,xn). (c1,...cn)], then expandMuType that type,
             and finally wrap the resulting type with
             "let v' = nurec v:k.c in ...".
                                            -Derek
           *)
	  fun deconstructNurecPath (con : con) : ((context * conbnd) * con) =
	      (case con of
		   Proj_c (con,lbl) => 
		       let val (res,c) = deconstructNurecPath con
		       in  (res,Proj_c(c,lbl))
		       end
		 | App_c (confun,conarg) =>
		       let val (res,c) = deconstructNurecPath confun
		       in  (res,App_c(c,conarg))
		       end
		 | Nurec_c (v,k,c) => 
		       let 
			   val nurecvar = Name.fresh_named_var "nurec_bnd"
			   val c' = substConInCon (add (empty()) (v,Var_c nurecvar)) c
			   val D = insert_kind (D,nurecvar,k)
			   val cbnd = Con_cb (nurecvar, (* NilRename.renameCon *) con)
		       in
			   ((D,cbnd),c')
		       end
		 | _ => (print "expandMuType given type not reducible to projection from mu:\n";
			 Ppnil.pp_con con;
			 error' "expandMuType given type not reducible to projection from mu"))


	in
	  case #2(reduce_hnf(D,mu_con)) of
	    Proj_c (mu_tuple as Mu_c (_,defs), l) =>
	      let 
		  val muvar = Name.fresh_named_var "mu_bnd"
		  val cbnd_mu = Con_cb(muvar,mu_tuple)
		  val defs = Sequence.toList defs
		  val which = lab2int l (length defs)
		  fun mapper (n,(v,_)) =
		      Con_cb (v,Proj_c(Var_c muvar,NilUtil.generate_tuple_label(n+1)))
		  val cbnds_projs = Listops.mapcount mapper defs
		  val (_,c) = List.nth(defs,which-1)
	      in  makeLetC (cbnd_mu::cbnds_projs) c
	      end

	  | c => 
	      let
		  val ((D,cbnd_nurec),c) = deconstructNurecPath c
	      in
		  case #2(reduce_hnf(D,c)) of
		      Proj_c(Mu_c (_,defs),l) =>
			let
			    val defs = Sequence.toList defs
			    val which = lab2int l (length defs)
                            (* This works based on the invariant that Mu's appearing inside
			       Nurec's do not actually bind any local variables, i.e. all recursion
                               inside a Nurec goes through the Nurec-bound variable.
			       So the Mu here behaves more like a boxing operator 
			       than like a recursion operator. *)
			    val (_,the_lth_projection) = List.nth(defs,which-1)
			in
			    makeLetC [cbnd_nurec] the_lth_projection
			end
		    | _ => error' "expandMuType given type not reducible to projection from mu"
	      end
	end


  and con_reduce_letfun state (sort,coder,var,formals,body,rest,con) =
	    let
	      val (D,subst) = state
	      val lambda = (Let_c (sort,[coder (var,formals,body)],Var_c var))
	    in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con)
		   then (HNF, subst, lambda)
	       else
		   let
		       val _ = if (!debug)
				   then (case (substitute subst var)  of
					SOME c => error' "XXX var already in subst"
				      | _ => ())
				else ()
		       val subst = addr (subst,var,lambda)
		   in  (PROGRESS,subst,Let_c(sort,rest,con))
		   end
	    end

  and con_reduce state (constructor : con) : progress * con_subst * con  =
    (case constructor of
	 (Prim_c (Vararg_c (openness,effect),[argc,resc])) =>
	 let
	   val irreducible = Prim_c(Vararg_c(openness,effect),[argc,resc])
	   val no_flatten = AllArrow_c{openness=openness,effect=effect,
				       tFormals=[],eFormals=[argc],fFormals=0w0,
				       body_type=resc}
	 in
	   (case con_reduce state argc of
	      (PROGRESS,subst,argc) => (PROGRESS,subst,Prim_c (Vararg_c (openness,effect),[argc,resc]))
	    | (HNF,_,Prim_c(Record_c labs, cons)) =>
		(HNF,#2 state,
		 if (length labs > !number_flatten) then no_flatten
		 else AllArrow_c{openness=openness,effect=effect,
				 tFormals=[], eFormals=cons,
				 fFormals=0w0, body_type=resc})
	    | (HNF,_,c)         => (HNF,#2 state,no_flatten)
	    | (IRREDUCIBLE,_,c) =>
		(case find_kind_equation_subst(#1 state,#2state,argc)
		   of (SOME subst,argc) => (PROGRESS,subst,Prim_c (Vararg_c (openness,effect),[argc,resc]))
		    | (NONE,_)      => (HNF,#2 state,irreducible)))
	 end
       | (Prim_c (Array_c,[c])) =>
	 let
	   val irreducible = Prim_c(Array_c,[c])
	 in
	   (case con_reduce state c of
	      (PROGRESS,subst,c) => (PROGRESS,subst,Prim_c (Array_c,[c]))
	    | (HNF,_,Prim_c(BoxFloat_c Prim.F64,[])) =>
		(HNF,#2 state,Prim_c(FloatArray_c Prim.F64,[]))
	    | (HNF,_,Prim_c(Int_c Prim.W32,[])) =>
		(HNF,#2 state,Prim_c(IntArray_c Prim.W32,[]))
	    | (HNF,_,Prim_c(Int_c Prim.W8,[])) =>
		if !SpecializeArrayofChar then
		  (HNF,#2 state,Prim_c(IntArray_c Prim.W8,[]))
		else
		  (HNF,#2 state,irreducible)
	    | (HNF,_,c)         => (HNF,#2 state,irreducible)
	    | (IRREDUCIBLE,_,_) =>
	       (case find_kind_equation_subst(#1 state,#2 state,c)
		  of (SOME subst,c) => (PROGRESS,subst,Prim_c (Array_c,[c]))
		   | (NONE,_)      => (HNF,#2 state,irreducible)))
	 end
       | (Prim_c (Vector_c,[c])) =>
	 let
	   val irreducible = Prim_c(Vector_c,[c])
	 in
	   (case con_reduce state c of
	      (PROGRESS,subst,c) => (PROGRESS,subst,Prim_c (Vector_c,[c]))
	    | (HNF,_,Prim_c(BoxFloat_c Prim.F64,[])) =>
		(HNF,#2 state,Prim_c(FloatVector_c Prim.F64,[]))
	    | (HNF,_,Prim_c(Int_c Prim.W32,[])) =>
		(HNF,#2 state,Prim_c(IntVector_c Prim.W32,[]))
	    | (HNF,_,Prim_c(Int_c Prim.W8,[])) =>
		if !SpecializeArrayofChar then
		  (HNF,#2 state,Prim_c(IntVector_c Prim.W8,[]))
		else
		  (HNF,#2 state,irreducible)
	    | (HNF,_,c)         => (HNF,#2 state,irreducible)
	    | (IRREDUCIBLE,_,_) =>
	       (case find_kind_equation_subst(#1 state,#2state,c)
		  of (SOME subst,c) => (PROGRESS,subst,Prim_c (Vector_c,[c]))
		   | (NONE,_)      => (HNF,#2 state,irreducible)))
	 end
	| (Prim_c _) => (HNF, #2 state, constructor)
	| (Mu_c _) => (HNF, #2 state, constructor)
	| (Nurec_c _) => (IRREDUCIBLE, #2 state, constructor)
	| (AllArrow_c _) => (HNF, #2 state, constructor)
	| (ExternArrow_c _) => (HNF, #2 state, constructor)
	| (Var_c var) =>
	 let val (D,subst) = state
	 in  (case (substitute subst var) of
		   SOME c => (PROGRESS, subst, c)
		 | NONE => (IRREDUCIBLE, subst, constructor))
	 end

	| Let_c (sort,[],body) => (PROGRESS,#2 state,body)
        | Let_c (sort,cbnds,letbody) =>
	 let val cbnd::rest = NilUtil.flattenCbnds cbnds
	 in  (case cbnd of
		  Open_cb (var,formals,funbody) => con_reduce_letfun state (sort,Open_cb,var,formals,funbody,rest,letbody)
		| Code_cb (var,formals,funbody) => con_reduce_letfun state (sort,Code_cb,var,formals,funbody,rest,letbody)
		| Con_cb(var,con) => let val (D,subst) = state
					 val subst = addr (subst,var,con)
				     in  (PROGRESS,subst,Let_c(sort,rest,letbody))
				     end)
	 end
	| (Closure_c (c1,c2)) =>
	    let val (progress,subst,c1) = con_reduce state c1
	    in  (progress,subst,Closure_c(c1,c2))
	    end
	| (Crecord_c _) => (HNF,#2 state,constructor)
	(*| Typeof_c e => (PROGRESS, #2 state, type_of(#1 state,e))*)
	| (Proj_c (Mu_c _,lab)) => (HNF,#2 state,constructor)
	| (Proj_c (c,lab)) =>
	      (case con_reduce state c of
		   (PROGRESS,subst,c) => (PROGRESS,subst,Proj_c(c,lab))
		 | (_,_,c) =>
		       let val (D,subst) = state
			   val (progress,con) = beta_conrecord' (Proj_c (c,lab))
		       in  if progress
			       then (PROGRESS,subst,con)
			   else (IRREDUCIBLE,subst,con)
		       end)
	| (App_c (cfun,actuals)) =>
	       (case con_reduce state cfun of
		    (PROGRESS,subst,c) => (PROGRESS,subst,App_c(c,actuals))
		  | (_,_,c) =>
			let val (D,subst) = state
			    val (progress,con) = beta_confun' true D (App_c(c,actuals))
			in  if progress
				then (PROGRESS,subst,con)
			    else (IRREDUCIBLE,subst,con)
			end)
	| (Coercion_c _) => (HNF,#2 state,constructor)
	(*| (Typecase_c {arg,arms,default,kind}) => error' "typecase not done yet"
	| (Annotate_c (annot,con)) => con_reduce state con)*)
	 )

(*
    and reduce_once (D,con) = let val (progress,subst,c) = con_reduce(D,empty()) con
			      in  (case progress
				     of IRREDUCIBLE => (case find_kind_equation_subst (D,subst,c)
							  of (SOME subst,c) => substConInCon subst c
							   | (NONE,c) => c)
				      | _ => substConInCon subst c)
			      end
*)

    and reduce_until (D,pred,con) =
        let
	  fun loop n (subst,c : con) =
            let val _ = if (n=warnDepth)
			    then (print "Warning: reduce_until exceeded ";
				  print (Int.toString warnDepth);
				  print " iterations\n")
			else ()
		val _ = if (n = maxDepth)
			    then (print "Warning: reduce_until exceeded ";
				  print (Int.toString maxDepth);
				  print " iterations.  Showing 10 iterations.\n")
			else ()
		val _ = if (n >= maxDepth)
			    then (print "Error: reduce_until iteration #";
				   print (Int.toString n); print ": "; Ppnil.pp_con c; print "\n")
			else ()
		val _ = if (n >= maxDepth + 10) then error' "reduce_until too many iterations" else ()

		fun step (subst,c) =
		  let val (progress,subst,c) = con_reduce(D,subst) c
		  in  case progress of
		    PROGRESS => loop (n+1) (subst,c)
		  | HNF =>
		      let val c = substConInCon subst c
		      in (case pred c of
			    SOME info => REDUCED info
			  | NONE => UNREDUCED c)
		      end
		  | IRREDUCIBLE =>
		      (case find_kind_equation_subst(D,subst,c)
			 of (SOME subst,c) => loop (n+1) (subst,c)
			  | (NONE,c) => UNREDUCED c)
		  end
	    in  case (pred c) of
                SOME info =>
		  let val c = substConInCon subst c
		  in case pred c of  (*Predicate may not be invariant under substitution*)
		    SOME info => REDUCED info
		  | NONE => step(empty(),c)
		  end
	      | NONE => step(subst,c)
	    end
        in  loop 0 (empty(),con)
        end
    and reduce_hnf_list' keep_paths (D,con) =
        let fun loop n (subst,c,acc) =
            let val _ = if (n>maxDepth)
			    then (print "reduce_hnf exceeded max reductions\n";
				  Ppnil.pp_con (substConInCon subst c); print "\n\n";
				  error' "reduce_hnf exceeded max reductions")
			 else ()
		val _ = if (n > 0 andalso n mod warnDepth = 0)
			    then msg ("  reduce_hnf at iteration #" ^
				      Int.toString n ^ "\n")
			else ()
	        val (progress,subst,c) = con_reduce(D,subst) c
	    in  case progress of
			 PROGRESS => loop (n+1) (subst,c,acc)
		       | HNF => (true, substConInCon subst c,acc)  (*HNF is invariant under substitution*)
		       | IRREDUCIBLE =>
			   (case find_kind_equation_subst(D,subst,c)
			      of (SOME subst',c') => loop (n+1) (subst',c',if keep_paths then (substConInCon subst c)::acc else acc)
			       | (NONE,c) => (false,c,acc))
	    end
        in  loop 0 (empty(),con,[])
        end
    and reduce_hnf_list arg = reduce_hnf_list' true arg
    and reduce_hnf (D,con) = let val (hnf,c,_) = reduce_hnf_list' false (D,con) in (hnf,c) end

(*
    and reduce_hnf' (D,con,subst) =
      let
	fun loop (subst,c) =
	  let
	    val (progress,subst,c) = con_reduce(D,subst) c
	  in  case progress of
	    PROGRESS => loop (subst,c)
	  | HNF => (subst,c)
	  | IRREDUCIBLE =>
	      (case find_kind_equation_subst(D,subst,c) of
		 (SOME subst,c) => loop (subst,c)
	       | (NONE,c) => (subst,c))
	  end
      in  loop (subst,con)
      end


    and reduce(D,con) = con_normalize D con
    and reduce_beta_hnf con = 
        let 
	  fun loop n (subst,c) = 
	    let 
	      val (progress,subst,c) = con_reduce(NilContext.empty(),subst) c  
	    in  
	      case progress of
		PROGRESS => loop (n+1) (subst,c) 
	      | HNF => (true, substConInCon subst c)  (*HNF is invariant under substitution*)
	      | IRREDUCIBLE => 
		  if NilSubst.C.is_empty subst then                  (*No subst, no further progress is possible*)
		    (false,c)
		  else                                          (*Carrying out subst may enable reductions *)
		    loop (n+1) (empty(),  substConInCon subst c)          (*Try to beta reduce further before giving up*)
	    end
        in  loop 0 (empty(),con)
        end
*)

    and projectTuple(D:context, c:con, l:label) =
	(case (reduce_hnf(D,Proj_c(c,l))) of
	   (true,c)  => c
	 | (false,c) => c)

    (*and removeDependence vclist c =
	let fun loop subst [] = substExpInCon subst c
	      | loop subst ((v,c)::rest) =
	           let val e = Raise_e(NilDefs.internal_match_exn,c)
		   in  loop (NilSubst.E.addr (subst,v,e)) rest
		   end
	in  loop (NilSubst.E.empty()) vclist
	end*)

    and projectRecordType(D:context, c:con, l:label) =
      let
	fun err c' =
	  (
	   print "projectRecordType could not find field ";Ppnil.pp_label l;print " in constructor: \n\t";
	   Ppnil.pp_con c;print "\n";
	   print " with HNF: \n\t";
	   Ppnil.pp_con c';print "\n";
	   error' "Error in record projection"
	   )
      in
	case #2(reduce_hnf(D,c)) 
	  of c as (Prim_c(Record_c labs, cons)) =>
	    (case (Listops.assoc_eq(eq_label,l,Listops.zip labs cons)) of
	       NONE => err c
	     | SOME c => c)
	   | c => (print "projectRecordType reduced to non-record type = \n";
		   Ppnil.pp_con c; print "\n";
		   error' "projectRecordType reduced to non-record type")
      end

    and reduceToSumtype(D: context, c:con) =
	(case #2(reduce_hnf(D,c)) of
	     Prim_c(Sum_c {tagcount,totalcount,known}, [carrier]) =>
	       if (TilWord32.equal(totalcount,TilWord32.uplus(tagcount,0w1)))
		  then (tagcount,known,[carrier])
	       else (case #2(reduce_hnf(D,carrier)) of
		      Crecord_c lcons => (tagcount,known,map #2 lcons)
		    | _ => error' "reduceToSumtype failed to reduced carrier to a crecord")
           | c => (print "reduceToSumtype failed to reduced argument to sumtype\n";
		   Ppnil.pp_con c; print "\n";
		   error' "reduceToSumtype failed to reduced argument to sumtype"))

    and projectSumType(D:context, c:con, s:TilWord32.word) =
	(case #2(reduce_hnf(D,c)) of
	     Prim_c(Sum_c {tagcount,totalcount,known}, cons) =>
		 if (TilWord32.ult(s,tagcount))
		     then error' "projectSumType: asking for tag fields"
		 else
		     let val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
			 val which = TilWord32.toInt(TilWord32.uminus(s,tagcount))
		     in  case (nontagcount,which) of
			 (0,_) => error' "projectSumType: only tag fields"
		       | (1,0) => hd cons
		       | _ => (projectTuple(D,hd cons, NilUtil.generate_tuple_label (which + 1))
			       handle e => (print "projectSumtype - unable to reduce Tuple\n";
					    NilContext.print_context D;
					    raise e))
		     end
	   | c => (print "projectSumType reduced to non-sum type = \n";
		   Ppnil.pp_con c; print "\n";
		   error' "projectSumType reduced to non-sum type"))

   and reduce_vararg(D:context,openness,effect,argc,resc) =
       let val irreducible = Prim_c(Vararg_c(openness,effect),[argc,resc])
	   val no_flatten = AllArrow_c{openness=openness,effect=effect,
				       tFormals=[],eFormals=[argc],fFormals=0w0,
				       body_type=resc}
       in  case (reduce_hnf(D,argc)) of
	   (_,Prim_c(Record_c labs, cons)) =>
	       if (length labs > !number_flatten) then no_flatten
	       else AllArrow_c{openness=openness,effect=effect,
			       tFormals=[], eFormals=cons,
			       fFormals=0w0, body_type=resc}
	 | (true,_) => no_flatten
	 | _ => irreducible
       end


   and type_of_switch (D:context,switch:switch):con  =
     (case switch of
	   Intsw_e {result_type,...} => result_type
	 | Sumsw_e {result_type,...} => result_type
	 | Exncase_e {result_type,...} => result_type
         | Typecase_e {result_type,...} => result_type
	 | Ifthenelse_e {result_type,...} => result_type)

(**
           Intsw_e {default=SOME def,...} => type_of(D,def)
	 | Intsw_e {arms,...} => type_of(D,#2(hd arms))
	 | Sumsw_e {default=SOME def,...} => type_of(D,def)
	 | Sumsw_e {arms,bound,sumtype,...} =>
	       let val (tagcount,_,carriers) = reduceToSumtype(D,sumtype)
		   val ssumcon = Prim_c(Sum_c {tagcount=tagcount,
					       totalcount=TilWord32.uplus(tagcount,
									  TilWord32.fromInt
									  (length carriers)),
					       known=SOME 0w0},
					case carriers of
					    [_] => carriers
					  | _ => [NilUtil.con_tuple carriers])
		   val D = NilContext.insert_con(D,bound,ssumcon)
	       in  type_of(D,#2(hd arms))
	       end
	 | Exncase_e {default=SOME def,...} => type_of(D,def)
	 | Exncase_e {arms,bound,...} =>
	       let val (tage,body) = hd arms
		   val tagcon = type_of(D,tage)
		   val Prim_c(Exntag_c, [con]) = #2(reduce_hnf(D,tagcon))
		   val D = NilContext.insert_con(D,bound,con)
	       in  type_of(D,#2(hd arms))
	       end
	 | Typecase_e _ => error' "typecase_e not done")
**)

   and type_of_value (D,value) =
     (case value
	of int (intsize,_) => Prim_c (Int_c intsize,[])
	 | uint (intsize,_) => Prim_c (Int_c intsize,[])
	 | float (floatsize,string) => Prim_c (Float_c floatsize,[])
	 | array (con,arr) => Prim_c (Array_c,[con])
	 | vector (con,vec) => Prim_c (Vector_c,[con])
	 | intarray (sz,arr) => Prim_c (IntArray_c sz,[])
	 | intvector (sz,vec) => Prim_c (IntVector_c sz,[])
	 | floatarray (sz,arr) => Prim_c (FloatArray_c sz,[])
	 | floatvector (sz,vec) => Prim_c (FloatVector_c sz,[])
	 | refcell expref => Prim_c (Array_c,[type_of(D,!expref)])
	 | tag (atag,con) => Prim_c (Exntag_c,[con]))

   and type_of_fbnd (D,openness,constructor,defs) =
     let
       val def_list = Sequence.toList defs
       val (bnd_types,functions) = unzip def_list
       (*val declared_c = map (NilUtil.function_type openness) functions
       val bnd_types = zip vars declared_c*)
       val D = NilContext.insert_con_list (D,bnd_types)
     in
       ((map (fn (x, y) => (x, SOME y)) bnd_types,[]),D)
     end
   and type_of_bnds (D,bnds) =
     let
       fun folder (bnd,D) =
	 (case bnd of
	    Con_b (phase, cbnd) =>
	      let val (v,c) =
		(case cbnd of
		   Con_cb (v,c) => (v,c)
		 | Open_cb(v,vklist,c) => (v,Let_c(Sequential,[cbnd],Var_c v))
		 | Code_cb(v,vklist,c) => (v,Let_c(Sequential,[cbnd],Var_c v)))
		  val D = NilContext.insert_equation(D,v,c)
	      in  (([],[cbnd]),D)
	      end
	  | Exp_b (var, _, exp) =>
	      let
		  val con = type_of (D, exp)
		  val D = NilContext.insert_con(D,var,con)
	      in
		  (([(var,SOME con)],[]),D)
	      end
	  | (Fixopen_b defs) => (type_of_fbnd(D,Open,Fixopen_b,defs))
	  | (Fixcode_b defs) => (type_of_fbnd(D,Code,Fixcode_b,defs))
	  | Fixclosure_b (is_recur,defs_l) =>
	      let
		(*val defs_l = Sequence.toList defs
		val defs_l = Listops.map_second (fn cl => #tipe cl) defs_l*)
		val defs_l = map #1 defs_l
		val D = NilContext.insert_con_list (D,defs_l)
	      in
		((map (fn (x, y) => (x, SOME y)) defs_l,[]),D)
	      end)
       val (bnds,D) = foldl_acc folder D bnds
       val (etypes_l,cbnds_l) = unzip bnds
       val cbnds = List.concat cbnds_l
       val etypes = List.concat etypes_l
     in
       (D,etypes,cbnds)
     end
   and type_of_prim (D,prim,cons,exps) =
        let
	   fun specialize_sumtype (known,sumcon) =
	       let val (_,sumcon_hnf) = reduce_hnf(D,sumcon)
	       in  NilUtil.convert_sum_to_special(sumcon_hnf,known)
	       end
	in
	 (case prim of
	    record [] => NilDefs.unit_con
	  | record labs => Prim_c(Record_c labs, map (fn e => type_of(D,e)) (tl exps))
	  | select lab => projectRecordType(D,type_of(D,hd exps),lab)
	  | inject s => specialize_sumtype(s,hd cons)
	  | inject_known s => specialize_sumtype(s,hd cons)
	  | project s => projectSumType(D,hd cons, s)
	  | project_known s => projectSumType(D,hd cons, s)
	  | box_float fs => Prim_c(BoxFloat_c fs,[])
	  | unbox_float fs => Prim_c(Float_c fs,[])
	  | make_exntag => Prim_c(Exntag_c, cons)
	  | inj_exn _ => Prim_c(Exn_c, [])
	  | make_vararg (openness,effect) =>
	       let val [argc,resc] = cons
	       in  reduce_vararg(D,openness,effect,argc,resc)
	       end
	  | make_onearg (openness,effect) =>
	       let val [argc,resc] = cons
	       in  AllArrow_c{openness=openness,effect=effect,
			      tFormals=[],eFormals=[argc],fFormals=0w0,body_type=resc}
	       end
	  | mk_record_gctag => Prim_c(GCTag_c,[hd cons])
	  | mk_sum_known_gctag => Prim_c(GCTag_c,[hd cons])
	  | partialRecord _ => error' "No clue."
)
	end

   and type_of (D : context,exp : exp) : con =
     let val _ = if (!debug)
		     then (print "XXX type_of on ";
			   Ppnil.pp_exp exp;
			   print "\n")
		 else ()
     in
       (case exp
	  of Var_e var =>
	    (NilContext.find_con (D,var)
	     handle NilContext.Unbound =>
		 let val _ = if (!debug)
				 then (print "XXX type_of context:\n";
				       NilContext.print_context D;
				       print "\n")
			     else ()
		 in
		     error' ("Encountered undefined variable " ^
			     (Name.var2string var) ^ " in type_of")
		 end)
	   | Const_e value => type_of_value (D,value)
	   | Let_e (letsort,bnds,exp) =>
	    let
	      val (D,etypes,cbnds) = type_of_bnds (D,bnds)
	      val c = type_of (D,exp)
	    in
	      NilUtil.makeLetC cbnds c
	    end
	   | Prim_e (NilPrimOp prim,_,cons,exps) => type_of_prim (D,prim,cons,exps)
	   | Prim_e (PrimOp prim,_,cons,exps) =>
	    let
	      val (total,arg_types,return_type) = NilPrimUtil.get_type D prim cons
	    in
	      return_type
	    end
	   | Switch_e switch => type_of_switch (D,switch)
	   | ExternApp_e (app,texps) =>
	    let
	      val app_con : con = type_of (D,app)
	    in  (case #2(reduce_hnf(D,app_con)) of
		     ExternArrow_c(_,c) => c
		   | c => (print "Ill Typed expression - not an arrow type. c = \n";
			      Ppnil.pp_con app_con;
			      print "\nreduce to = \n";
			      Ppnil.pp_con c;
			      print "\nexp = \n";
			      Ppnil.pp_exp app;
			      print "\n";
			      error' "Ill Typed expression - not an arrow"))
	    end
	   | (App_e (openness,app,cons,texps,fexps)) =>
	    let
	      val app_con : con = type_of (D,app)
	      val  (tformals,eformals,body) =
		(case #2(reduce_hnf(D,app_con)) of
		     AllArrow_c{tFormals,eFormals,body_type,...} => (tFormals,eFormals,body_type)
		   | Prim_c(Vararg_c _, [_,c]) => ([],[],c)
		   | c => (print "Ill Typed expression - not an arrow type.\n app_con = \n";
			      Ppnil.pp_con app_con;
			      print "\nreduce to = \n";
			      Ppnil.pp_con c;
			      print "\nexp = \n";
			      Ppnil.pp_exp app;
			      print "\n";
			      error' "Ill Typed expression - not an arrow")) handle ex => (print_context D; raise ex)

	      val subst = fromList (zip (#1 (unzip tformals)) cons)
	      val con = substConInCon subst body

	    in  (*removeDependence
		  (map (fn (SOME v,c) => (v,substConInCon subst c)
		         | (NONE, c) => (fresh_var(), c)) eformals)*)
		  con
	    end

	   | Raise_e (exp,con) => con
	   | Handle_e {result_type,...} => result_type
	   | ForgetKnown_e (sumcon,field) => 
	     let
	       val (_,sumcon_hnf) = reduce_hnf(D,sumcon)
	       val ksumcon = NilUtil.convert_sum_to_special(sumcon_hnf,field)
	     in
	       Coercion_c {vars = [],from = ksumcon,to = sumcon}
	     end
	   | Fold_e (vars,from,to) => Coercion_c {vars=vars,from=from,to=to}
	   | Unfold_e (vars,from,to) => Coercion_c {vars=vars,from=from,to=to}
	   | (exp as (Coerce_e (coercion,cargs,_))) =>
	     let
		 val coerce_con = type_of (D,coercion)
		 val (vars,to) = case #2 (reduce_hnf(D,coerce_con)) of
				     Coercion_c {vars,to,...} => (vars,to)
				   | c => (print "Ill Typed expression - coercion has type: \n";
					   Ppnil.pp_con coerce_con;
					   print "\nwhich reduces to:";
					   Ppnil.pp_con c;
					   print "\nexp = \n";
					   Ppnil.pp_exp exp;
					   print "\n";
					   error' "Ill Typed expression - not a coercion type")
		 val subst = fromList (zip vars cargs)
		 val to = substConInCon subst to
	     in to
	     end)
     end

    local
      fun bind_kind_eqn'((D,alpha),var,con,kind) =
	if NilContext.bound_con (D,var) then
	  let
	    val vnew = Name.derived_var var
	    val D = insert_kind_equation(D,vnew,con,kind)
	    val alpha = Alpha.rename(alpha,var,vnew)
	  in (D,alpha)
	  end
	else (insert_kind_equation(D,var,con,kind),alpha)

      fun bind_kind_eqn(state as (D,alpha),var,con,kind) =
	bind_kind_eqn'(state,var,alphaCRenameCon alpha con,alphaCRenameKind alpha kind)

      fun bind_eqn(state as (D,alpha),var,con) =
	let val con = alphaCRenameCon alpha con
	in
	  if NilContext.bound_con (D,var) then
	    let
	      val vnew = Name.derived_var var
	      val D = insert_equation(D,vnew,con)
	      val alpha = Alpha.rename(alpha,var,vnew)
	    in (D,alpha)
	    end
	  else (insert_equation(D,var,con),alpha)
	end

      fun instantiate_formals(state,vklist,actuals) =
	let
	  fun folder ((var,k),actual,state) = bind_kind_eqn(state,var,actual,k)
	in foldl2 folder state (vklist,actuals)
	end
    in
      fun context_beta_reduce_letfun (state,sort,coder,var,formals,body,rest,con) =
	let
	  val lambda = (Let_c (sort,[coder (var,formals,body)],Var_c var))
	in if (null rest) andalso eq_opt (eq_var,SOME var,strip_var con)
	     then (state,lambda,false)
	   else
	     context_beta_reduce(bind_eqn(state,var,lambda),Let_c(sort,rest,con))
	end

      and context_beta_reduce (state : (context * Alpha.alpha_context),constructor : con)
	: ((context * Alpha.alpha_context) * con * bool)  =
	(case constructor of
	   (Prim_c (Vararg_c (openness,effect),[argc,resc])) =>
	     let
	       val (D,alpha) = state

	       (* Alpha context may be zeroed out on return from recursive call *)
	       val argc = alphaCRenameCon alpha argc
	       val resc = alphaCRenameCon alpha resc
	       val state = (D,Alpha.empty_context())
	       val (state,argc,path) = context_reduce_hnf''(state,argc)

	       val irreducible = Prim_c(Vararg_c(openness,effect),[argc,resc])
	       val no_flatten  = AllArrow_c{openness=openness,effect=effect,
					    tFormals=[],eFormals=[argc],fFormals=0w0,
					    body_type=resc}
	       val res =
		 (case argc of
			Prim_c(Record_c labs, cons) =>
			  if (length labs > !number_flatten) then no_flatten
			  else AllArrow_c{openness=openness,effect=effect,
					  tFormals=[], eFormals=cons,
					  fFormals=0w0, body_type=resc}
		      | _ => if path then irreducible
			     else no_flatten)
	     in (state,res,false)
	     end
	 | (Prim_c (Array_c,[c])) =>
	     let
	       val (state,c,path) = context_reduce_hnf'' (state,c)
	       val irreducible = Prim_c(Array_c,[c])
	       val res = 
		 (case c 
		    of Prim_c(BoxFloat_c Prim.F64,[]) => Prim_c(FloatArray_c Prim.F64,[])
		     | Prim_c(Int_c Prim.W32,[]) => Prim_c(IntArray_c Prim.W32,[])
		     | Prim_c(Int_c Prim.W8,[]) =>
		      if !SpecializeArrayofChar then
			(Prim_c(IntArray_c Prim.W8,[]))
		      else irreducible
		     | _ => irreducible)
	     in (state,res,false)
	     end
	 | (Prim_c (Vector_c,[c])) =>
	     let
	       val (state,c,path) = context_reduce_hnf'' (state,c)
	       val irreducible = Prim_c(Vector_c,[c])
	       val res = 
		 (case c 
		    of Prim_c(BoxFloat_c Prim.F64,[]) => Prim_c(FloatVector_c Prim.F64,[])
		     | Prim_c(Int_c Prim.W32,[]) => Prim_c(IntVector_c Prim.W32,[])
		     | Prim_c(Int_c Prim.W8,[]) =>
		      if !SpecializeArrayofChar then
			(Prim_c(IntVector_c Prim.W8,[]))
		      else irreducible
		     | _ => irreducible)
	     in (state,res,false)
	     end

	 | (Prim_c _)            => (state,constructor,false)
	 | (Mu_c _)              => (state,constructor,false)
	 | (Nurec_c _)           => (state,constructor,true)
	 | (AllArrow_c _)        => (state,constructor,false)
	 | (Coercion_c _)        => (state,constructor,false)
	 | (ExternArrow_c _)     => (state,constructor,false)
	 | (Crecord_c _)         => (state,constructor,false)
	 | (Var_c var)           => (state,constructor,true)

	 | (Let_c (sort,((cbnd as Open_cb (var,formals,body))::rest),con)) =>
	     context_beta_reduce_letfun (state,sort,Open_cb,var,formals,body,rest,con)

	 | (Let_c (sort,((cbnd as Code_cb (var,formals,body))::rest),con)) =>
	     context_beta_reduce_letfun (state,sort,Code_cb,var,formals,body,rest,con)

	 | (Let_c (sort,cbnd as (Con_cb(var,con)::rest),body))             =>
	     context_beta_reduce(bind_eqn(state,var,con),Let_c(sort,rest,body))

	 | (Let_c (sort,[],body)) => context_beta_reduce(state,body)

	 | (Closure_c (c1,c2)) =>
	     let val (state,c1,path) = context_beta_reduce (state,c1)
	     in  (state,Closure_c(c1,c2),path)
	     end

	 | (Proj_c (c,lab)) =>
	     (case context_beta_reduce (state,c) of
		(state,constructor,true)  => (state,Proj_c(constructor,lab),true)
	      | (state,constructor,false) =>
		  (case strip_crecord constructor
		     of SOME entries =>
		       (case (List.find (fn ((l,_)) => eq_label (l,lab))
			      entries )
			  of SOME (l,c) => context_beta_reduce(state,c)
			   | NONE => error (locate "context_beta_reduce") "Field not in record")
		      | NONE => (state,Proj_c(constructor,lab),false))) (* constructor is a mu *)
	 | (App_c (cfun,actuals)) =>
	      (case context_beta_reduce (state,cfun) of
		 (state,constructor,true)  => (state,App_c(constructor,actuals),true)
	       | (state,constructor,false) =>
		   let
		     exception NOT_A_LAMBDA

		     fun strip (Open_cb (var,formals,body)) = (var,formals,body)
		       | strip (Code_cb (var,formals,body)) = (var,formals,body)
		       | strip _ = raise NOT_A_LAMBDA

		     fun get_lambda (lambda,name) =
		       let
			 val (var,formals,body) = strip lambda
		       in
			 (case name
			    of Var_c var' =>
			      if eq_var (var,var') then
				(formals,body)
			      else raise NOT_A_LAMBDA
			     | _ => raise NOT_A_LAMBDA)
		       end

		     fun lambda_or_closure (Let_c (_,[lambda],name)) = (get_lambda (lambda,name),NONE)
		       | lambda_or_closure (Closure_c(code,env)) =
		       let val (args,_) = lambda_or_closure code
		       in  (args,SOME env) end
		       (*| lambda_or_closure (Annotate_c(_,con)) = lambda_or_closure (con)*)
		       | lambda_or_closure _ = raise NOT_A_LAMBDA


		     fun open_lambda cfun = (SOME (lambda_or_closure cfun)) handle NOT_A_LAMBDA => NONE

		   in
		     (case open_lambda constructor
			of SOME((formals,body),SOME env) =>
			  context_beta_reduce(instantiate_formals(state,formals,env::actuals),body)
			 | SOME((formals,body),NONE)     =>
			  context_beta_reduce(instantiate_formals(state,formals,actuals),body)
			 | NONE => (Ppnil.pp_con constructor;
				    error (locate "context_beta_reduce") "redex not in HNF"))
		   end)

(*
	     | (Typecase_c {arg,arms,default,kind}) =>
		 let
		   val (state,arg)   = context_reduce_hnf'(state,arg)
		 in
		   (case strip_prim arg
		      of SOME (pcon,args) =>
			(case List.find (fn (pcon',formals,body) => primequiv (pcon,pcon')) arms
			   of SOME (_,formals,body) => context_beta_reduce(instantiate_formals(state,formals,args),body)
			    | NONE => (state,default,false))
		       | _ => (state,Typecase_c{arg=arg,arms=arms,default=default,kind=kind},false))
		 end
	     | (Annotate_c (annot,con)) => context_beta_reduce (state,con)*)
		   )
      and context_reduce_hnf'' (state,constructor) =
	let val ((D,alpha),con,path) = context_beta_reduce (state,constructor)
	in
	  if path then
	    let val con = alphaCRenameCon alpha con
	    in
	      (case find_kind_equation (D,con)
		 of SOME con => context_reduce_hnf'' ((D,Alpha.empty_context()),con)
		  | NONE => ((D,Alpha.empty_context()),con,true))
	    end
	  else
	    ((D,alpha),con,false)
	end

      and context_reduce_hnf' args =
	let val (state,con,path) = context_reduce_hnf'' args
	in (state,con)
	end
      fun context_reduce_hnf(D,constructor) =
	let val ((D,alpha),con) = context_reduce_hnf'((D,Alpha.empty_context()),constructor)
	in (D,alphaCRenameCon alpha con)
	end
    end

  val kind_normalize = wrap2 "kind_normalize" kind_normalize
  val con_normalize = wrap2 "con_normalize"  con_normalize

  val kind_normalize' = wrap2 "kind_normalize'" kind_normalize'
  val con_normalize' = wrap2 "con_normalize'"  con_normalize'

  val reduce_hnf = wrap1 "reduce_hnf" reduce_hnf
  val reduce_until = fn arg => wrap "reduce_until" (fn () => reduce_until arg)
(*
  val reduce_once = wrap1 "reduce_once" reduce_once
*)

  val beta_conrecord = wrap1 "beta_conrecord" beta_conrecord
  val beta_confun = wrap2 "beta_confun" beta_confun
  val eta_confun = wrap1 "eta_confun" eta_confun
  (*val beta_typecase = wrap2 "beta_typecase" beta_typecase*)

  val reduceToSumtype = wrap1 "reduceToSumType" reduceToSumtype
  val type_of = fn arg => (wrap1 "type_of" type_of arg
			   handle e => (print "type_of failed on ";
					Ppnil.pp_exp (#2 arg);
					print "\n"; raise e))

  fun strip_arrow_norm D con =
       (case NilUtil.strip_arrow(#2(reduce_hnf (D, con))) of
	   NONE => (print "Got: ";
		    Ppnil.pp_con con;
		    print "\n";
		    error "strip_arrow" "Expected arrow type")
	 | SOME r => r)

end




