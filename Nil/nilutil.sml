(*
 This structure is a repository for useful functions for operating on the NIL syntax.
 It should not rely on the NIL contexts, static semantics, or the normalizer: conceptually
 it exists at a level just above the datatype itself.

*)

structure NilUtil
  :> NILUTIL =
struct
  (* IMPORTS *)
  open Nil
  open Name

  val error = fn s => Util.error "nilutil.sml" s

  val printl  = Util.printl
  val lprintl = Util.lprintl

  type arrow = {openness : openness, effect : effect,
		tFormals : (var*kind) list,
		eFormals : con list,
		fFormals : w32,
		body_type : con}

  val debug = Stats.ff"NilUtilDebug"

  fun error s = Util.error "nilutil.sml" s

  val arrow_is_taglike = CompilerControl.ArrowIsTaglike
  val array_is_taglike = CompilerControl.ArrayIsTaglike

  val profile       = Stats.ff "nil_profile"
  val local_profile = Stats.ff "nilutil_profile"
  val _ = Stats.ff "closure_omit_coercions"

  val subtimer = fn args => fn args2 => if !profile orelse !local_profile then Stats.subtimer args args2 else #2 args args2

  val unzip     = Listops.unzip
  val zip       = Listops.zip

  val foldl_acc = Listops.foldl_acc
  val foldl2    = Listops.foldl2
  val foldl4    = Listops.foldl4
  val eq_list   = Listops.eq_list

  (* This code was just copied from IlUtil so that this module can be independent from the Il. *)
  local
	val size = 20
	fun make i = let val s = Symbol.labSymbol(Int.toString i)
		     in  (s, Name.symbol_label s)
		     end
	val table = Array.tabulate(size, make)
  in
	fun generate_tuple_label 0 = error "generate_tuple_label called with 0"
	  | generate_tuple_label i = #2(if (i<size)
					    then Array.sub(table,i)
					else make i)
  end

  val fresh_named_var = Name.fresh_named_var

  val allprim_uses_carg = NilDefs.allprim_uses_carg
  val path2con          = NilDefs.path2con
  val con2path          = NilDefs.con2path
  val covariant_prim    = NilDefs.covariant_prim
  val small_con         = NilDefs.small_con
  (* END IMPORTS *)


  (******Functions for destructing NIL terms*********)

  fun extractCbnd (Con_cb(v,c)) = (v,c)
    | extractCbnd (Open_cb(v,vklist,c)) =
    let val v' = Name.derived_var v
    in  (v,Let_c(Sequential,
		 [Open_cb(v',vklist,c)],
		 Var_c v'))
    end
    | extractCbnd (Code_cb(v,vklist,c)) =
    let val v' = Name.derived_var v
    in  (v,Let_c(Sequential,
		 [Code_cb(v',vklist,c)],
		 Var_c v'))
    end

  fun makeConb cbnd = Con_b (Runtime,cbnd)
  fun cbnds2bnds cbnds = map makeConb cbnds

  local
    fun cbnd2importbnd cbnd = ImportBnd(Runtime,cbnd)
    fun bnd2importbnd bnd = 
      (case bnd
	 of Con_b pc => ImportBnd pc
	  | _ => error "Only con bnds can be used as ImportBnds")
  in
    val cbnds2importbnds = map cbnd2importbnd

    val bnds2importbnds  = map bnd2importbnd
  end

  (* Loses Parallel information 
   * Return bounds in reverse order 
   *)
  fun flattenCbnds' cbnds =
      let 
	fun loop [] acc = acc
	  | loop (cbnd::rest) acc =
	  let
	    val acc =
	      (case cbnd of
		 Con_cb(v,c) => 
		   let
		     val (cbnds,c) = flattenCLet' c
		   in Con_cb(v,c)::cbnds@acc
		   end
	       | _ => cbnd::acc)
	  in loop rest acc
	  end
      in loop cbnds []
      end
  and flattenCLet' c = 
    (case c
       of Let_c (_,cbnds,body) => 
	 let
	   val rcbnds = flattenCbnds' cbnds
	   val (brcbnds,body) = flattenCLet' body
	 in (brcbnds@rcbnds,body)
	 end
	| _ => ([],c))

  fun flattenCbnds cbnds = rev (flattenCbnds' cbnds)
  fun flattenCLet c = 
    (case flattenCLet' c
       of ([],body) => body
	| (rcbnds,body) => Let_c(Sequential,rev rcbnds,body))


  fun project_from_kind(lvk_seq,con,label) =
    let
      val lvk_list = Sequence.toList lvk_seq
      fun loop (subst,[]) = error ("project_kind: Missing label in record kind:  "^(Name.label2string label))
	| loop (subst,((l,v),k)::rest) =
	if (Name.eq_label(label,l))
	  then NilSubst.substConInKind subst k
	else loop (NilSubst.C.sim_add subst (v,Proj_c(con,l)),rest)
    in  loop (NilSubst.C.empty(),lvk_list)
    end

  fun project_from_kind_nondep (rkind,label) =
    case rkind
      of Record_k lvk_seq =>
	(case Sequence.find (fn (l,_) => (Name.eq_label(label,l))) lvk_seq
	   of SOME k => k
	    | NONE => error "project_from_kind_nondep: Field not in record kind")
       | other =>
	   (Ppnil.pp_kind other; print "\n";
	    error  "project_from_kind_nondep: Trying to project from non-record kind ")


    fun convert_sum_to_special
      (Prim_c(Sum_c {tagcount,totalcount,known},carriers), w) =
      Prim_c(Sum_c {tagcount=tagcount, totalcount=totalcount,
		    known = SOME w}, carriers)

    fun sum_project_carrier_type (Prim_c(Sum_c {known=SOME sumtype,tagcount,totalcount},[carrier])) =
      let
	val nontagcount = TilWord32.toInt(TilWord32.uminus(totalcount,tagcount))
	val which = TilWord32.toInt(TilWord32.uminus(sumtype,tagcount))
      in
	case (Int.compare (which,0),Int.compare (nontagcount,1)) of
	  (LESS,_) => error ("Injecting value into non tag field")
	| (_,LESS) => error("Illegal injection - no non value fields!")
	| (EQUAL,EQUAL) => carrier
	| _ => Proj_c(carrier,generate_tuple_label (which + 1))
      end
      | sum_project_carrier_type _ = error "projecting carrier from non-sum"



    (* Functions for dealing with annotations
     *)

  local
    fun strip_var' (Var_c var) = SOME var
      | strip_var' _ = NONE
    fun strip_exntag' (Prim_c (Exntag_c,[con])) = SOME con
      | strip_exntag' _ = NONE
    fun strip_recursive' (Mu_c (flag,set)) = SOME (flag,set)
      | strip_recursive' _ = NONE
    fun strip_boxfloat' (Prim_c (BoxFloat_c floatsize,[])) = SOME floatsize
      | strip_boxfloat' _ = NONE
    fun strip_float' (Prim_c (Float_c floatsize,[])) = SOME floatsize
      | strip_float' _ = NONE
    fun strip_int' (Prim_c (Int_c intsize,[])) = SOME intsize
      | strip_int' _ = NONE
    fun strip_sum' (Prim_c (Sum_c {tagcount,totalcount,known},cons)) =
      (case cons
	 of [con] => SOME (tagcount,totalcount,known,con)
	  | _ => error "strip_sum given sum not carrying exactly one type argument")
      | strip_sum' _ = NONE
    fun strip_arrow' (AllArrow_c arg) = SOME arg
      | strip_arrow' _ = NONE
    fun strip_externarrow' (ExternArrow_c arg) = SOME arg
      | strip_externarrow' _ = NONE
    fun strip_record' (Prim_c (Record_c labs, cons)) = SOME (labs,cons)
      | strip_record' _ = NONE
    fun strip_crecord' (Crecord_c entries) = SOME entries
      | strip_crecord' _ = NONE
    fun strip_proj' (Proj_c (con,label)) = SOME (con,label)
      | strip_proj' _ = NONE
    fun strip_prim' (Prim_c (pcon,args)) = SOME (pcon,args)
      | strip_prim' _ = NONE
    fun strip_app' (App_c (con,actuals)) = SOME (con,actuals)
      | strip_app' _ = NONE
    fun strip_coercion' (Coercion_c stuff) = SOME stuff
      | strip_coercion' _ = NONE

    fun is_exn_con' (Prim_c (Exn_c,[])) = true
      | is_exn_con' _ = false

    fun is_unit_c' (Prim_c(Record_c _,[])) = true
      | is_unit_c' _ = false

    fun is_mu_c' (Mu_c _) = true
      | is_mu_c' _ = false

  in
    val strip_var = strip_var'
    val strip_exntag = strip_exntag'
    val strip_recursive = strip_recursive'
    val strip_boxfloat = strip_boxfloat'
    val strip_float = strip_float'
    val strip_int = strip_int'
    val strip_sum = strip_sum'
    val strip_arrow = strip_arrow'
    val strip_externarrow = strip_externarrow'
    val strip_record = strip_record'
    val strip_crecord = strip_crecord'
    val strip_proj = strip_proj'
    val strip_prim = strip_prim'
    val strip_app = strip_app'
    val strip_coercion = strip_coercion'

    val is_exn_con = is_exn_con'
    val is_unit_c = is_unit_c'
    val is_var_c = Option.isSome o strip_var
    val is_float_c = Option.isSome o strip_float
    val is_mu_c = is_mu_c'
  end

  fun is_var_e (Var_e v) = true
    | is_var_e _ = false
  fun is_taglike c =
      (case c
	of Prim_c(Int_c _, _)  => true
	 | Prim_c(Sum_c _,_)   => true
	 | Mu_c _              => true
	 | Proj_c(Mu_c _,_)    => true
	 | Prim_c(Exntag_c, _) => true
	 | AllArrow_c _        => !arrow_is_taglike
	 | Prim_c(Array_c,_)           => !array_is_taglike
	 | Prim_c(Vector_c,_)         => !array_is_taglike
	 | Prim_c(IntArray_c _ ,_)       => !array_is_taglike
	 | Prim_c(IntVector_c _ ,_)      => !array_is_taglike
	 | Prim_c(FloatArray_c _,_)      => !array_is_taglike
	 | Prim_c(FloatVector_c _ ,_)    => !array_is_taglike
	 | _                   => false)


  fun get_arrow_return con =
    case strip_arrow con of
	SOME {body_type,...} => SOME body_type
       | NONE => NONE


  (* Local rebindings from imported structures *)


  fun sub_phase (Compiletime, Runtime) = false
    | sub_phase _ = true




  fun selfify'(con,kind,subst) =
    (case kind of
       Type_k => SingleType_k con
     | SingleType_k _ => SingleType_k(con)
     | Single_k _ => Single_k con
     | Record_k entries =>
	 let
	   fun folder (((l,v),k),subst) =
	     let
	       val proj = Proj_c (con,l)
	       val kres = selfify' (proj,k,subst)
	       val subst = NilSubst.C.sim_add subst (v,proj)
	     in
	       (((l,v),kres),subst)
	     end
	   val (entries,subst) = Sequence.foldl_acc folder subst entries
	 in Record_k entries
	 end
     | Arrow_k (openness,args,return) =>
	 let
	   val (formal_vars,_) = unzip args
	   val args = Listops.map_second (NilSubst.substConInKind subst) args
	   val actuals = List.map Var_c formal_vars
	 in
	   Arrow_k (openness,args,selfify'(App_c (con,actuals),return,subst))
	 end)

  fun selfify (con,kind) = selfify'(con,kind,NilSubst.C.empty())

  local
    structure A :
      sig
	type alpha_context
	val substitute : alpha_context * var -> var
	val empty_context : unit -> alpha_context
	val alpha_bind : (alpha_context * var) -> (alpha_context * var)
	val alpha_bind_list : (alpha_context * (var list)) -> (alpha_context * (var list))
	val alpha_equate_pair : (alpha_context * alpha_context) * (var * var)
	  -> alpha_context * alpha_context
	val alpha_equate_pairs :
	  (alpha_context * alpha_context) * (var list * var list)
	  -> alpha_context * alpha_context
	val alpha_pair_eq : (alpha_context * alpha_context) * (var * var) -> bool
      end = Alpha
  in
    open A
    val eq_var = Name.eq_var
    val eq_label = Name.eq_label
    val map_second = Listops.map_second
    val foldl_acc = Listops.foldl_acc
    val eq_len = Listops.eq_len
    val member_eq = Listops.member_eq
    val same_intsize = Prim.same_intsize
    val same_floatsize = Prim.same_floatsize
    val eq_var2 = Name.eq_var2
  end
  (**)


    type bound = {isConstr : bool,
		  level : int,   (* Initially zero and increases by one past each lambda *)
		  boundcvars : Name.VarSet.set,
		  boundevars : Name.VarSet.set}

    datatype 'a changeopt = NOCHANGE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a

    type handlers =
	{exphandler : bound * Nil.exp -> Nil.exp changeopt,
	 bndhandler : bound * Nil.bnd -> (Nil.bnd list) changeopt,
	 conhandler : bound * Nil.con -> Nil.con changeopt,
	 cbndhandler : bound * Nil.conbnd -> (Nil.conbnd list) changeopt,
	 kindhandler : bound * Nil.kind -> Nil.kind changeopt}

    datatype state =
	STATE of {bound : bound,
		  handlers : handlers}

    fun add_convars (STATE{bound={isConstr,level,boundcvars,boundevars},handlers},vs) =
	(STATE{bound = {level = level,
			isConstr=isConstr,
			boundcvars = Name.VarSet.addList(boundcvars,vs),
			boundevars = boundevars},
	       handlers = handlers})

    fun add_convar (h : state ,v) = add_convars(h,[v])

    fun add_var (STATE{bound={isConstr,level,boundevars,boundcvars},handlers=handlers}, v) =
	(STATE{bound = {level=level,
			isConstr = isConstr,
			boundcvars = boundcvars,
			boundevars = Name.VarSet.add(boundevars, v)},
	       handlers = handlers})

    fun add_level (STATE{bound={isConstr,level,boundevars,boundcvars},handlers=handlers}) =
	(STATE{bound = {level = level + 1,
			isConstr = isConstr,
			boundcvars = boundcvars,
			boundevars = boundevars},
	       handlers = handlers})

    fun type_state(STATE{bound={isConstr,level,boundevars,boundcvars},handlers=handlers}) =
	(STATE{bound = {level = level,
			isConstr = false,
			boundcvars = boundcvars,
			boundevars = boundevars},
	       handlers = handlers})
    fun constr_state(STATE{bound={isConstr,level,boundevars,boundcvars},handlers=handlers}) =
	(STATE{bound = {level = level,
			isConstr = true,
			boundcvars = boundcvars,
			boundevars = boundevars},
	       handlers = handlers})

    fun f_cbnd (state : state)  (cbnd : conbnd) : (conbnd list * state) =
	let
	    val (STATE{bound,handlers={cbndhandler,...},...}) = state
	    fun cbnd_help wrap (var, vklist, c) state openness =
		let val state' = add_level state
		    val (vklist',state') = f_vklist state' vklist
		    val c' = f_con state'  c
		in (wrap(var, vklist', c'), add_convar(state,var))
		end
	    fun do_cbnd (Con_cb(var, con),state) =
		let val con' = f_con state  con
		in (Con_cb(var, con'), add_convar(state,var))
		end
	      | do_cbnd (Open_cb args,state) = cbnd_help Open_cb args state Open
	      | do_cbnd (Code_cb args,state) = cbnd_help Code_cb args state Code
	in (case (cbndhandler (bound,cbnd)) of
		CHANGE_NORECURSE cbs => (cbs, state)  (* is this right? *)
	      | CHANGE_RECURSE cbs =>
		    let
			fun folder(cb,(cbs,s)) =
			    let val (cb',s') = do_cbnd(cb,s)
			    in  (cb'::cbs,s')
			    end
			val (cbs_rev',s) = foldl folder ([],state) cbs
		    in  (rev cbs_rev', s)
		    end
	      | NOCHANGE => let val (cb,s) = do_cbnd(cbnd,state)
			    in ([cb],s)
			    end)
	end

  and f_con (state : state)  (con : con) : con =
    let
      val self = f_con state
      val (STATE{bound,handlers={conhandler,...},...}) = state
      fun docon con =
	  (case con of
	       (Prim_c (pcon,args)) => (Prim_c (pcon,map self args))

	     | (Mu_c (flag,defs)) =>
		   let
		       val (con_vars,cons) = unzip (Sequence.toList defs)
		       val state' = add_convars (state,con_vars)
		       val cons' = List.map (f_con state' ) cons
		       val defs' = Sequence.fromList (zip con_vars cons')
		   in  Mu_c (flag,defs')
		   end

	     | (Nurec_c (var,kind,conbody)) => 
		   let
		       val kind' = f_kind state kind
		       val state' = add_convar(state,var)
		       val conbody' = f_con state' conbody
		   in
		       Nurec_c (var,kind',conbody')
		   end

	     | (AllArrow_c confun) => AllArrow_c (f_arrow state confun)
	     | (ExternArrow_c (cons,c)) => ExternArrow_c(map self cons, self c)

	     | (Var_c var) => con

	     | (Let_c (letsort, cbnds, body)) =>
		   let
		       fun folder(cbnd,(rev_cbnds,accstate)) =
			   let val s = (case letsort of
					    Parallel => state
					  | Sequential => accstate)
			       val (cbnds',state') = f_cbnd s  cbnd
			   in  (cbnds'@rev_cbnds, state')
			   end
		       val (rev_cbnds',state') = foldl folder ([],state) cbnds
		       val cbnds' = rev rev_cbnds'
		       val body' = f_con state'  body
		   in
		       Let_c (letsort, cbnds', body')
		   end

	     | (Closure_c (code,env)) =>
		   let
		       val code' = self code
		       val env' = self env
		   in
		       Closure_c(code', env')
		   end

	     | (Crecord_c entries) =>
		   let
		       val entries' = map_second self entries
		   in
		       Crecord_c entries'
		   end

	     | (Proj_c (con,lbl)) =>
		   let
		       val con' = self con
		   in
		       Proj_c (con', lbl)
		   end

	     | (App_c (cfun,actuals)) =>
		   let
		       val cfun' = self cfun
		       val actuals' = map self actuals
		   in
		       App_c (cfun', actuals')
		   end
	     | (Coercion_c {vars,from,to}) =>
		   let
		     val state' = foldl (fn (v,s) => add_convar (s,v)) state vars
		   in
		     Coercion_c {vars=vars,from=f_con state' from,to=f_con state' to}
		   end)
    in
	case (conhandler (bound,con)) of
	    CHANGE_NORECURSE c => c
	  | CHANGE_RECURSE c => docon c
	  | NOCHANGE => docon con
    end

  and f_arrow (state : state) {openness, effect,
			       tFormals, eFormals, fFormals, body_type} =
    let
      val (tFormals,state) = f_vklist state tFormals
      val eFormals = map (f_con state) eFormals
      val body_type = f_con state body_type
    in   {openness = openness,
	  effect =  effect,
	  tFormals = tFormals,
	  eFormals = eFormals,
	  fFormals = fFormals,
	  body_type = body_type}
    end

  and f_kind (state : state) (arg_kind : kind) =
    let
      val self = f_kind state
      val (STATE{bound,handlers={kindhandler,...},...}) = state
      fun dokind kind =
	  (case kind
	     of Type_k => kind
	     | (SingleType_k con) => SingleType_k(f_con state con)
	     | (Single_k con) => Single_k(f_con state con)
	     | (Record_k fieldseq) =>
	      let
		fun fold_one (((lbl,var),kind),state) =
		  let
		    val kind'  = f_kind state kind
		    val state' = add_convar (state,var)
		  in
		    (((lbl, var), kind'),state')
		  end
		val (fieldseq',state') = Sequence.foldl_acc fold_one state fieldseq
	      in
		Record_k fieldseq'
	      end

	     | (Arrow_k (openness, args, result)) =>
	      let
		val (args', result') = f_arrow_kind state (args, result)
	      in
		Arrow_k (openness, args', result')
	      end)

    in
      case (kindhandler (bound,arg_kind)) of
	CHANGE_NORECURSE k => k
      | CHANGE_RECURSE k => dokind k
      | NOCHANGE => dokind arg_kind
    end

  and f_vklist state vklist =
    let	(* val state = type_state state *)
	fun fold_one ((var,knd),state) =
	    let
		val knd' = f_kind state knd
		val state' = add_convar (state, var)
	    in
		((var,knd'),state')
	    end
    in
	foldl_acc fold_one state vklist
    end

  and f_type state c = f_con (type_state state) c

  and f_vtclist state vtclist =
    let fun fold_one ((var,trace,con),state) =
	    let
		val con' = f_type state con
		val trace' = f_niltrace state trace
		val state' = add_var (state, var)
	    in
		((var,trace,con'),state')
	    end
    in
	foldl_acc fold_one state vtclist
    end

  and f_vtlist state vtlist =
    let fun fold_one ((var,trace),state) =
	    let
		val trace' = f_niltrace state trace
		val state' = add_var (state, var)
	    in
		((var,trace),state')
	    end
    in
	foldl_acc fold_one state vtlist
    end

  and f_arrow_kind state (args, result) =
    let
      val (args',state') = f_vklist state args
      val result' = f_kind state' result
    in
      (args', result')
    end


  and dofun (state : state) (Function{effect,recursive,
				      tFormals,eFormals,fFormals,body}) =
      let val state' = add_level state
	  val (eFormals', state') = f_vtlist state' eFormals
	  val body' = f_exp state' body
      in
	  Function{effect=effect, recursive=recursive,
		   tFormals = tFormals, eFormals = eFormals, fFormals = fFormals,
		   body = body'}
      end

    and f_niltrace (state : state) (nt : Nil.niltrace) : Nil.niltrace =
      let val state = constr_state state
      in
	case nt of
	  TraceCompute v =>
	    (case (f_con state (Var_c v)) of
	       Var_c v' => TraceCompute v'
	     | _ => TraceUnknown)
	| TraceKnown (TraceInfo.Compute(v,ls)) =>
	       let
		 val con = f_con state (path2con (v,ls))
	       in
		 case (con2path con) of
		   SOME (v', ls') =>
		     TraceKnown (TraceInfo.Compute(v',ls'))
		 | _ => TraceUnknown
	       end
	| _ => nt
      end

  and f_bnd (state : state) (bnd : bnd) : bnd list * state =
      let val (STATE{bound,handlers={bndhandler,exphandler,...},...}) = state
	  fun do_bnd (bnd,s) : bnd list * state =
	      case bnd of
		  Con_b(p,cb) => let val state = (case p of
						      Runtime => state
						    | Compiletime => type_state state)
				     val (cbnds,state) = f_cbnd state cb
				     val state = constr_state state
				 in  (map (fn cb => Con_b(p,cb)) cbnds, state)
				 end
		| Exp_b(v,nt,e) => ([Exp_b(v, f_niltrace state nt, f_exp state e)],
				       add_var(state,v))
		| Fixopen_b vfset =>
		      let val s' = Sequence.foldl (fn (((v,c),f),s) => add_var(s,v)) s vfset
			  fun doer(v,f) = (v,dofun s' f)
		      in  ([Fixopen_b(Sequence.map doer vfset)], s')
		      end
		| Fixcode_b vfset =>
		      let val s' = Sequence.foldl (fn (((v,c),f),s) => add_var(s,v)) s vfset
			  fun doer(v,f) = (v,dofun s' f)
		      in  ([Fixcode_b(Sequence.map doer vfset)], s')
		      end
		| Fixclosure_b (is_recur,vcset) =>
		      let val s' = Sequence.foldl (fn (((v,c),{...}),s) => add_var(s,v)) s vcset
			  fun doer(v,{code,cenv,venv}) =
			  (v,{code = (case (exphandler (bound,Var_e code)) of
					  NOCHANGE => code
					| (CHANGE_RECURSE (Var_e v')) => v'
					| (CHANGE_NORECURSE (Var_e v')) => v'
					| _ => error "can't have non-var in cllosure code comp"),
			  cenv = f_con s' cenv,
			  venv = f_exp s' venv})
		      in  ([Fixclosure_b(is_recur, Sequence.map doer vcset)], s')
		      end
      in  case (bndhandler (bound,bnd)) of
	  CHANGE_NORECURSE bs => (bs, state)  (* is this right? *)
	| CHANGE_RECURSE bs => let val (bs_list,s) = foldl_acc do_bnd state bs
			       in  (List.concat bs_list, s)
			       end
	| NOCHANGE => do_bnd(bnd,state)
      end

  and f_switch state sw =
      (case sw of
	   Intsw_e {arg, size, arms, default, result_type} =>
	       Intsw_e {arg = f_exp state arg,
			size = size,
			arms = map (fn (t,e) => (t,f_exp state e)) arms,
			default = Util.mapopt (f_exp state) default,
			result_type = f_type state result_type}
	 | Sumsw_e {arg, sumtype, bound, arms, default, result_type} =>
	       let val state' = add_var(state,bound)
	       in  Sumsw_e {arg = f_exp state arg,
			    sumtype = f_type state sumtype,
			    bound = bound,
			    arms = map (fn (t,tr,e) => (t,f_niltrace state tr,f_exp state' e)) arms,
			    default = Util.mapopt (f_exp state) default,
			    result_type = f_type state result_type}
	       end
	 | Exncase_e {arg, bound, arms, default, result_type} =>
	       let val state' = add_var(state,bound)
	       in  Exncase_e {arg = f_exp state arg,
			      bound = bound,
			      arms = map (fn (t,tr,e) => (f_exp state t,f_niltrace state tr,f_exp state' e)) arms,
			      default = Util.mapopt (f_exp state) default,
			      result_type = f_type state result_type}
	       end
	 | Ifthenelse_e {arg, thenArm, elseArm, result_type} =>
	       Ifthenelse_e {arg = f_ccode state arg,
			     thenArm = f_exp state thenArm,
			     elseArm = f_exp state elseArm,
			     result_type = f_type state result_type}
	 | Typecase_e {arg, arms, default, result_type} =>
	       let val arg = f_con state arg
		   val default = f_exp state default
		   val arms = map (fn (pc,vklist,e) =>
				   let val (vklist,state') = f_vklist state vklist
				   in  (pc, vklist, f_exp state' e)
				   end) arms
	       in  Typecase_e{arg=arg, default=default,arms=arms, result_type = f_type state result_type}
	       end)

  and f_ccode state (Exp_cc e) = Exp_cc (f_exp state e)
    | f_ccode state (And_cc (c1, c2)) = And_cc (f_ccode state c1, f_ccode state c2)
    | f_ccode state (Or_cc (c1, c2)) = Or_cc (f_ccode state c1, f_ccode state c2)
    | f_ccode state (Not_cc c) = Not_cc (f_ccode state c)

  and f_exp state (exp : exp) : exp =
      let val self = f_exp state
	  val (STATE{bound,handlers={exphandler,...},...}) = state
	  fun doexp e =
	      case e of
		  (Var_e _) => e
		| (Const_e v) =>
		    Const_e
		    (case v of
			 (Prim.int _) => v
                       | (Prim.uint _) => v
                       | (Prim.float _) => v
		       | (Prim.array (c,array)) =>
			     (Array.modify self array;
			      Prim.array(f_con state c,array))
		       | (Prim.vector (c,array)) =>
			     (Array.modify self array;
			      Prim.vector(f_con state c,array))
		       | (Prim.intarray (sz,array)) =>
			     (Array.modify self array;
			      Prim.intarray(sz,array))
		       | (Prim.intvector (sz,array)) =>
			     (Array.modify self array;
			      Prim.intvector(sz,array))
		       | (Prim.floatarray (sz,array)) =>
			     (Array.modify self array;
			      Prim.floatarray(sz,array))
		       | (Prim.floatvector (sz,array)) =>
			     (Array.modify self array;
			      Prim.floatvector(sz,array))
		       | Prim.refcell (r as (ref e)) => (r := self e; v)
		       | Prim.tag (t,c) => Prim.tag(t,f_con state c))
		| (Let_e (sort,bnds,body)) =>
		      let fun folder (bnd,(bnds_list,s)) =
			    let val (bnds,s') = f_bnd s bnd
			    in  (bnds :: bnds_list,s')
			    end
			  val (rev_bndslist,state') = foldl folder ([],state) bnds
			  val bnds' = List.concat(rev rev_bndslist)
			  val body' = f_exp state' body
		      in Let_e(sort,bnds',body')
		      end
		| (Prim_e (ap,trlist,clist,elist)) =>
		      let val state = if (allprim_uses_carg ap) then state else type_state state
		      in  Prim_e(ap,map (f_niltrace state) trlist,map (f_con state) clist, map self elist)
		      end
		| (Switch_e switch) => Switch_e(f_switch state switch)
		| ExternApp_e (func,elist) =>
		      ExternApp_e(self func, map self elist)
		| (App_e (openness,func,clist,elist,eflist)) =>
		      App_e(openness,
			    self func,
			    map (f_con state) clist,
			    map self elist,
			    map self eflist)
		| Raise_e (e,c) => Raise_e(self e, f_type state c)
		| Handle_e {body,bound,handler,result_type} =>
		      let val state' = add_var(state,bound)
		      in  Handle_e{body = self body, bound=bound,
				   handler = f_exp state' handler,
				   result_type = f_type state result_type}
		      end
		| ForgetKnown_e (sumcon,which) =>
		    ForgetKnown_e (f_con state sumcon,which)

		| Fold_e (vars,from,to) =>
		  let
		      val state' = foldl (fn (v,s) => add_var (s,v)) state vars
		  in
		      Fold_e (vars, f_con state' from, f_con state' to)
		  end
		| Unfold_e (vars,from,to) =>
		  let
		      val state' = foldl (fn (v,s) => add_var (s,v)) state vars
		  in
		      Unfold_e (vars, f_con state' from, f_con state' to)
		  end
		| Coerce_e (coercion,cargs,exp) =>
		  Coerce_e (self coercion, map (f_con state) cargs, self exp)
      in case (exphandler (bound,exp)) of
	  CHANGE_NORECURSE e => e
	| CHANGE_RECURSE e => doexp e
	| NOCHANGE => doexp exp
      end

  val default_bound : bound = {isConstr = true, level = 0,
			       boundcvars = Name.VarSet.empty, boundevars = Name.VarSet.empty}
  fun default_bndhandler _ = NOCHANGE
  fun default_cbndhandler _ = NOCHANGE
  fun default_exphandler _ = NOCHANGE
  fun default_conhandler _ = NOCHANGE
  fun default_kindhandler _ = NOCHANGE

  (* --------------- Exported Functions -------------------------- *)

  fun to_handlers handlers =
      STATE{bound = default_bound,
	    handlers = handlers}

  fun exp_rewrite h e = f_exp (to_handlers h) e
  fun bnd_rewrite h b = #1(f_bnd (to_handlers h) b)
  fun kind_rewrite h k = f_kind(to_handlers h) k
  fun con_rewrite h c = f_con(to_handlers h) c
  fun cbnd_rewrite h cb = hd (#1(f_cbnd (to_handlers h) cb))


  local
      fun count_handler() =
	  let val count = ref 0
	      fun con_handler _ = (count := (!count) + 1; NOCHANGE)
	      fun exp_handler _ = (count := (!count) + 1; NOCHANGE)
	      fun kind_handler _ = (count := (!count) + 1; NOCHANGE)
	  in
	      (count,
	       STATE{bound = default_bound,
		     handlers = {bndhandler = default_bndhandler,
				 cbndhandler = default_cbndhandler,
				 exphandler = exp_handler,
				 conhandler = con_handler,
				 kindhandler = kind_handler}})
	  end
  in  fun bnd_size argbnd = let val (count,handlers) = count_handler()
			    in  (f_bnd handlers argbnd; !count)
			    end
      fun exp_size argexp = let val (count,handlers) = count_handler()
			    in  (f_exp handlers argexp; !count)
			    end
      fun con_size argcon = let val (count,handlers) = count_handler()
			    in  (f_con handlers argcon; !count)
			    end
      fun cbnd_size argbnd = let val (count,handlers) = count_handler()
			     in  (f_cbnd handlers argbnd; !count)
			     end
      fun kind_size argkind = let val (count,handlers) = count_handler()
			     in  (f_kind handlers argkind; !count)
			     end
      fun import_size (ImportValue (_,_,_,c)) = 1 + con_size c
	| import_size (ImportType (_,_,k)) = 1 + kind_size k
	| import_size (ImportBnd (_, cb)) = 1 + cbnd_size cb
      fun export_size (ExportValue (_,v)) = 2
	| export_size (ExportType (_,v)) = 2
      fun module_size (MODULE{bnds,imports,exports,exports_int}) =
	  let val count = foldl (fn (bnd,acc) => acc + (bnd_size bnd))    0 bnds
	      val count = foldl (fn (imp,acc) => acc + (import_size imp)) count imports
	      val count = foldl (fn (exp,acc) => acc + (export_size exp)) count exports
	      val count = 
		(case exports_int
		   of SOME ei => foldl (fn (exp,acc) => acc + (import_size exp)) count ei
		    | NONE => count)
	  in  count
	  end
  end

  local

    (* var_set_handler creates a set of handlers to pass to the rewriter defined above
     for the purpose of finding free variables.  This is presumably used for countless
     purposes; one very special one is in Tortl for the translation of exception handlers.
     Since the saving that goes on there has to be compatible with the "free variable"
     computation in the closure converter wrt coercion variables, we have a for_exnhandler
     flag here. *)

    fun var_set_handler (for_exnhandler,look_in_kind, minLevel) =
	let val free_evars : (Name.VarSet.set ref) = ref Name.VarSet.empty
	    val free_cvars : (Name.VarSet.set ref) = ref Name.VarSet.empty
	    fun normal_exp_handler ({level,boundevars,boundcvars,...}:bound,Var_e v) =
		(if (not (Name.VarSet.member(boundevars,v))
		     andalso (level >= minLevel))
		     then free_evars := Name.VarSet.add(!free_evars, v)
		 else ();
		     NOCHANGE)
	      | normal_exp_handler _ = NOCHANGE
	    val ignoring_coercions = !(Stats.bool "closure_omit_coercions")
	    fun coercion_ignoring_exp_handler ({level,boundevars,boundcvars,...}:bound,Var_e v) =
		(if (not (Name.VarSet.member(boundevars,v))
		     andalso (level >= minLevel))
		     then free_evars := Name.VarSet.add(!free_evars, v)
		 else ();
		     NOCHANGE)
	      (* NOTE: Never ever use the rewritten expression for anything if we are *)
	      (* ignoring coercions, because of the hacky way we make that happen.    *)
	      | coercion_ignoring_exp_handler (_,Coerce_e(Var_e v,cargs,e')) =
		   if ignoring_coercions then
		     CHANGE_RECURSE (Coerce_e( Const_e(Prim.int(Prim.W8,TilWord64.zero)) ,cargs,e'))
		   else NOCHANGE
	      | coercion_ignoring_exp_handler _ = NOCHANGE
	    fun kind_handler (_,k) = CHANGE_NORECURSE k
	    fun con_handler ({level,boundevars,boundcvars,...}:bound,Var_c v) =
		(if (not (Name.VarSet.member(boundcvars,v))
		     andalso (level >= minLevel))
		     then free_cvars := Name.VarSet.add(!free_cvars,v)
		 else ();
		     NOCHANGE)
	      | con_handler _ = NOCHANGE
	in
	    (free_evars,
	     free_cvars,
	     (STATE{bound = default_bound,
		    handlers = {bndhandler = default_bndhandler,
				cbndhandler = default_cbndhandler,
				exphandler = if for_exnhandler then coercion_ignoring_exp_handler
					     else normal_exp_handler,
				conhandler = con_handler,
				kindhandler = if (look_in_kind)
						  then default_kindhandler
					      else kind_handler}}))
	end

    fun free_handler (lookInKind,minLevel) = var_set_handler(false,lookInKind,minLevel)

  in

    fun freeExpConVarInExnHandler(lookInKind,minLevel,e) =
      let val (evars_ref,cvars_ref,handler) = var_set_handler(true,lookInKind,minLevel)
      in f_exp handler e;
	(!evars_ref,!cvars_ref)
      end

    fun freeExpConVarInExp(lookInKind,minLevel,e) =
	let val (evars_ref,cvars_ref,handler) = free_handler (lookInKind,minLevel)
	in  f_exp handler e;
	    (!evars_ref, !cvars_ref)
	end

    fun freeExpConVarInCon(lookInKind,minLevel,c) =
	let val (evars_ref,cvars_ref,handler) = free_handler (lookInKind,minLevel)
	in  f_con handler c;
	    (!evars_ref, !cvars_ref)
	end

    fun freeExpConVarInKind(lookInKind,minLevel,k) =
	let val (evars_ref,cvars_ref,handler) = free_handler (lookInKind,minLevel)
	in  f_kind handler k;
	    (!evars_ref, !cvars_ref)
	end

    fun freeExpConVarInBnd (lookInKind,minLevel, eb) =
	let val (evars_ref,cvars_ref,handler) = free_handler (lookInKind,minLevel)
	in
	    f_bnd handler eb;
	    (! evars_ref, !cvars_ref)
	end

    fun freeConVarInCon(lookInKind,minLevel,c) =
	let val (evars_ref,cvars_ref,handler) = free_handler (lookInKind,minLevel)
	in  f_con handler c;
	    !cvars_ref
	end

    fun freeConVarInCbnd(lookInKind,minLevel,c) =
	let val (evars_ref,cvars_ref,handler) = free_handler (lookInKind,minLevel)
	in  f_cbnd handler c;
	  !cvars_ref
	end

    fun freeConVarInKind (minLevel, k) =
	let val (evars_ref,cvars_ref,handler) = free_handler (true, minLevel)
	in  f_kind handler k;
	    !cvars_ref
	end

    fun freeVarInKind (minLevel, k) =
	let val (evars_ref,cvars_ref,handler) = free_handler (true, minLevel)
	in  f_kind handler k;
	    Name.VarSet.union(!evars_ref,!cvars_ref)
	end

  end


    (* get the bound var from a conbnd *)
    fun varBoundByCbnd (Con_cb (v,c)) = v
      | varBoundByCbnd (Open_cb (v,_,_)) = v
      | varBoundByCbnd (Code_cb (v,_,_)) = v

    val varsBoundByCbnds = map varBoundByCbnd

    (* get the bound vars from a list of bnds *)
    fun varsBoundByBnds bnds =
	let
	    fun gv ([],l) = rev l
	      | gv (Con_b (p,cb)::rest,l) = gv(rest,(varBoundByCbnd cb)::l)
	      | gv (Exp_b (v,_,e)::rest,l) = gv(rest,v::l)
	      | gv (Fixopen_b(vfs)::rest,l) = gv(rest,(map (#1 o #1) (Sequence.toList vfs))@l)
	      | gv (Fixcode_b(vfs)::rest,l) = gv(rest,(map (#1 o #1) (Sequence.toList vfs))@l)
	      | gv (Fixclosure_b(_,vfs)::rest,l) = gv(rest,(map (#1 o #1) (Sequence.toList vfs))@l)
	in
	    gv (bnds,[])
	end


  fun same_openness (Open,Open) = true
    | same_openness (Closure,Closure) = true
    | same_openness (Code,Code) = true
    | same_openness _ = false

  fun same_effect (Total,Total) = true
    | same_effect (Partial,Partial) = true
    | same_effect _ = false

  fun sub_effect (sk,Total,Total) = true
    | sub_effect (sk,Partial,Partial) = true
    | sub_effect (sk,Total,Partial) = sk
    | sub_effect (sk,_,_) = false

  (*Pre: Records are sorted by label *)
  fun primequiv (pcon1,pcon2) =
    let
    in
      case (pcon1,pcon2) of
	  (Int_c size1,Int_c size2) => same_intsize (size1,size2)
	| (Float_c size1,Float_c size2) => same_floatsize (size1,size2)
	| (BoxFloat_c size1,BoxFloat_c size2) => same_floatsize (size1,size2)
	| (Exn_c,Exn_c) => true
	| (Array_c,Array_c) => true
	| (Vector_c,Vector_c) => true
	| (IntArray_c sz1,IntArray_c sz2) => same_intsize (sz1,sz2)
	| (IntVector_c sz1,IntVector_c sz2) => same_intsize (sz1,sz2)
	| (FloatArray_c sz1,FloatArray_c sz2) => same_floatsize (sz1,sz2)
	| (FloatVector_c sz1,FloatVector_c sz2) => same_floatsize (sz1,sz2)
	| (Ref_c,Ref_c) => true
	| (Loc_c,Loc_c) => true
	| (Exntag_c,Exntag_c) => true
	 | (Sum_c {known=k1,tagcount=t1,totalcount=to1},
	    Sum_c {known=k2,tagcount=t2,totalcount=to2}) =>
	    Util.eq_opt (op =,k1,k2)
	    andalso (to1 = to2)
	    andalso (t1 = t2)

	 | (Record_c labs1,Record_c labs2) =>
	      Listops.eq_list (eq_label,labs1,labs2)
	 | (Vararg_c (openness1,effect1),Vararg_c (openness2,effect2)) =>
	  (same_openness (openness1,openness2) andalso
	   same_effect (effect1,effect2))
	 | (GCTag_c,GCTag_c) => true
	 | _  => false
    end


  fun same_phase (Compiletime, Compiletime) = true
    | same_phase (Runtime, Runtime) = true
    | same_phase _ = false

  fun alpha_equiv_kind' (context) (kind1,kind2) =
    let
      val recur = alpha_equiv_kind' context
    in
      (case (kind1,kind2) of
	    (Type_k, Type_k) => true
          | (Single_k con1, Single_k con2) =>
	       alpha_equiv_con' context (con1,con2)
	  | (SingleType_k con1,SingleType_k con2) =>
               alpha_equiv_con' context (con1,con2)
	  | (Record_k elts1_seq,Record_k elts2_seq) =>
	   let
	     val conref = ref context
	     val elts1 = Sequence.toList elts1_seq
	     val elts2 = Sequence.toList elts2_seq
	     fun equiv_one (((lbl1,var1),kind1),((lbl2,var2),kind2)) =
	       let
		 val kind_equiv = alpha_equiv_kind' (!conref) (kind1,kind2)
		 val _ = conref := alpha_equate_pair (!conref,(var1,var2))
	       in
		 kind_equiv andalso (eq_label (lbl1,lbl2))
	       end
	   in
	     (eq_len(elts1,elts2)) andalso
	     (ListPair.all equiv_one (elts1,elts2))
	   end

	  | (Arrow_k (openness1, formals1, return1),
	     (Arrow_k (openness2, formals2, return2))) =>
	   let
	     val conref = ref context
	     fun equiv_one ((var1,kind1),(var2,kind2)) =
	       (alpha_equiv_kind' (!conref) (kind1,kind2))
	       before (conref := alpha_equate_pair (!conref,(var1,var2)))
	   in
	     same_openness (openness1,openness2) andalso
	     eq_len (formals1,formals2) andalso
	     (ListPair.all equiv_one (formals1,formals2)) andalso
	     alpha_equiv_kind' (!conref) (return1,return2)
	   end
	  | _ => false)
    end

  and alpha_equiv_con' context args = alpha_subequiv_con' false context args
  and alpha_subequiv_con_list st context (cl1,cl2) =
    Listops.eq_list (fn (c1,c2) => alpha_subequiv_con' st context (c1,c2),cl1,cl2)
  and alpha_equiv_vk_list context (vk1,vk2) =
    let
      fun folder ((var1,kind1),(var2,kind2),(context,equal)) =
	if equal then
	  (alpha_equate_pair (context,(var1,var2)),alpha_equiv_kind' context (kind1,kind2))
	else (context,false)
    in foldl2 folder (context,true) (vk1,vk2)
    end
  and alpha_subequiv_con' st context (con1,con2) =
    let
      val res =
	(case (con1,con2)
	   of (Prim_c (pcon1,args1),Prim_c (pcon2,args2)) =>
	     let
	       val res1 =
		 (case (pcon1,pcon2) of
		    (Record_c labs1,
		     Record_c labs2) => Listops.eq_list(eq_label,labs1,labs2)
		  | (Sum_c{tagcount=tagcount1,totalcount=totalcount1,
			   known=known1},
		     Sum_c{tagcount=tagcount2,totalcount=totalcount2,
			   known=known2}) =>
		    (
		     (tagcount1 = tagcount2)     
		     andalso (totalcount1 = totalcount2) 
		     andalso (known1 = known2)
		     )
		  | (Vararg_c(o1,eff1),Vararg_c(o2,eff2)) =>
		    o1 = o2 andalso (sub_effect(st,eff1,eff2)) andalso
		    (case (args1,args2)
		       of ([argc1,resc1],[argc2,resc2]) =>
			 alpha_subequiv_con' st context (argc2,argc1) andalso
			 alpha_subequiv_con' st context (resc1,resc2)
			| _ => false)
		  | (_,_) => primequiv(pcon1,pcon2))
	       val st' = st andalso (covariant_prim pcon1)
	     in res1 andalso alpha_subequiv_con_list st' context (args1,args2)
	     end
	    | (Mu_c (flag1,defs1),Mu_c (flag2,defs2)) =>
		flag1 = flag2 andalso
		let
		  val def_list1 = Sequence.toList defs1
		  val def_list2 = Sequence.toList defs2
		  val (var_list1,con_list1) = unzip def_list1
		  val (var_list2,con_list2) = unzip def_list2
		  val context = if flag1 then alpha_equate_pairs (context,(var_list1,var_list2)) else context
		in alpha_subequiv_con_list false context (con_list1,con_list2)
		end
	    | (Nurec_c (var1,kind1,con1), Nurec_c (var2,kind2,con2)) => 
		alpha_equiv_kind' context (kind1,kind2) andalso
		let
		    val context = alpha_equate_pair(context,(var1,var2))
		in
		    alpha_subequiv_con' false context (con1,con2)
		end
	    | (AllArrow_c {openness = o1, effect = eff1,
			   tFormals = t1, eFormals = e1, fFormals = f1, body_type = b1},
	       AllArrow_c {openness = o2, effect = eff2,
			   tFormals = t2, eFormals = e2, fFormals = f2, body_type = b2}) =>

		(same_openness(o1,o2)
		 andalso (f1 = f2)
		 andalso (sub_effect (st,eff1,eff2))
		 andalso (List.length t1 = List.length t2)
		 andalso
		 let val (context,equal) = alpha_equiv_vk_list context (t1,t2)
		 in (equal 
		     andalso alpha_subequiv_con_list st context (e2, e1)
		     andalso alpha_subequiv_con' st context (b1,b2))
		 end )

			 (*Common case?*)

	  | (Var_c var1,Var_c var2) => alpha_pair_eq(context,(var1,var2))

	  (* Note - can't subtype lets without recording variance info,
	   * so set subtyping to false here.
	   *)
	  | (Let_c (sort1, binds1,con1),Let_c (sort2, binds2,con2)) =>
	   let
	     fun equate_fun context ((var1,formals1,con1),(var2,formals2,con2)) =
	       let val (context',equal) = alpha_equiv_vk_list context (formals1,formals2)
	       in if equal then (alpha_equate_pair(context,(var1,var2)),
				 alpha_equiv_con' context' (con1,con2))
		  else (context,false)
	       end

	     fun equiv_one (_,_,(context,false))   = (context,false)
	       | equiv_one (bnd1,bnd2,(context,_)) =
	       (case (bnd1,bnd2)
		  of (Con_cb(var1,con1),Con_cb(var2,con2)) =>
		    (alpha_equate_pair(context,(var1,var2)),alpha_equiv_con' context (con1,con2))
		   | (Open_cb args1,Open_cb args2) => equate_fun context (args1,args2)
		   | (Code_cb args1,Code_cb args2) => equate_fun context (args1,args2)
		   | _ => (context,false))
	   in
	     (eq_len(binds1,binds2) andalso
	      let val (context,equal) = foldl2 equiv_one (context,true) (binds1,binds2)
	      in equal andalso alpha_subequiv_con' st context (con1,con2)
	      end)
	   end
	  | (Closure_c (code1,env1),Closure_c (code2,env2)) =>
	   alpha_subequiv_con' st context (code1,code2) andalso alpha_subequiv_con' st context (env1,env2)

	  | (Crecord_c entries1,Crecord_c entries2) =>
	   let
	     fun equiv_each ((lbl1,con1),(lbl2,con2)) =
	       (eq_label (lbl1,lbl2)
		andalso	alpha_subequiv_con' st context (con1,con2))
	   in eq_list (equiv_each,entries1,entries2)
	   end

	  | (Proj_c (crec1,label1),Proj_c (crec2,label2)) =>
	   eq_label (label1,label2) andalso
	   alpha_subequiv_con' st context (crec1,crec2)

	  | (App_c (cfun1,actuals1),App_c (cfun2,actuals2)) =>
	   alpha_subequiv_con' st context (cfun1,cfun2)
	   andalso alpha_subequiv_con_list st context (actuals1,actuals2)

	  | _ => false)

      val _ =
	if !debug andalso not res then
	  (lprintl "alpha_equiv_con failed!";
	   printl "Constructor:";
	   Ppnil.pp_con con1;
	   lprintl "Not equivalent to :";
	   Ppnil.pp_con con2;
	   printl "")
	else ()

    in res
    end

  fun alpha_subequiv_con st args = alpha_subequiv_con' st (empty_context (),empty_context ()) args

  val alpha_equiv_con    = alpha_subequiv_con false
  val alpha_equiv_kind   = alpha_equiv_kind' (empty_context (),empty_context ())


  fun alpha_mu is_bound (vclist) =
      let fun folder((v,_),subst) = if (is_bound v)
				       then NilSubst.C.sim_add subst (v,Var_c(Name.derived_var v))
				    else subst
	  val subst = foldl folder (NilSubst.C.empty()) vclist
	  fun substcon c = NilSubst.substConInCon subst c
	  fun lookup var = (case (substcon (Var_c var)) of
				(Var_c v) => v
			      | _ => error "substcon returned non Var_c")
      in  if (NilSubst.C.is_empty subst)
	      then (vclist)
	  else (map (fn (v,c) => (lookup v, substcon c)) vclist)
      end

   (* makeLetC.
         Creates a constructor-level sequential let given bindings and
         a body.  Optimizes the let when the bindings or the body are
         especially simple:

         LET_C [] IN con                 ==>  con
         LET_C cvar = con IN cvar        ==>  con
         LET_C cvar' = con IN cvar       ==>  cvar  [where cvar != cvar']
         LET_C cbnds, cvar = con IN cvar ==>  makeLetC cbnds con
         LET_C cbnds IN (LET_C cbnds' IN con) ==> makeLetC (cbnds@cbnds') con
    *)
   fun makeLetC nil body = body
     | makeLetC [conbnd as Con_cb(var,con)] (cv as Var_c var') =
       if (Name.eq_var(var,var')) then
	 if small_con con then con else Let_c (Sequential,[conbnd],cv)
       else cv
     | makeLetC cbnds (cv as Var_c var') =
       (case (List.rev cbnds) of
	    Con_cb(var,con)::rest =>
	      if (Name.eq_var(var,var')) andalso small_con con then
		makeLetC (rev rest) con
	      else
		Let_c (Sequential, cbnds, cv)
	  | _ => Let_c (Sequential, cbnds, cv))
     | makeLetC cbnds (Let_c(Sequential, cbnds', body)) =
          makeLetC (cbnds @ cbnds') body
     | makeLetC cbnds body = Let_c (Sequential, cbnds, body)



   fun createBindings (vklist, con_args, vtclist, exp_args, fplist, fp_args) =
       let
	   val con_bnds = map (fn(cvar, con) => Con_b (Runtime, Con_cb(cvar, con)))
                              (Listops.zip vklist con_args)
           val exp_bnds = map (fn((evar, trace), exp) => Exp_b (evar, trace, exp))
	                      (Listops.zip vtclist exp_args)
	   val float_type = Prim_c (Float_c Prim.F64, [])
           val fp_bnds = map (fn (evar, exp) => Exp_b (evar, TraceUnknown, exp))
	                     (Listops.zip fplist fp_args)
       in
           con_bnds @ exp_bnds @ fp_bnds
       end

   fun makeExpB (v,nt,exp) =
       (case exp of
	    Let_e (Sequential, let_bnds, body as Var_e v') => let_bnds @ [Exp_b(v,nt,body)]
	  | _ => [Exp_b(v,nt,exp)])

   (* makeLetE.
         Creates a term-level sequential let given bindings and
         a body.  Optimizes the let when the bindings or the body are
         especially simple:

         LET_E [] IN exp                    ==> exp
         LET_E bnds, evar = exp IN evar     ==> makeLetE bnds exp
         LET_E bnds IN (LET_E bnds' in EXP) ==> makeLetE (bnds@bnds') exp
         LET_E bnds, fixopen-LEAF foo formal = fnbody IN foo arg ==>
                 makeLetE (bnds, formal = arg) IN fnbody
    *)

   fun makeLetE Parallel ebnds let_body = Let_e(Parallel, ebnds, let_body)
     | makeLetE _ nil let_body = let_body
     | makeLetE _ ebnds (Let_e(Sequential, ebnds', let_body)) =
          makeLetE Sequential (ebnds @ ebnds') let_body
     | makeLetE _ ebnds let_body =
       (case (List.rev ebnds, let_body) of
           (Fixopen_b fset :: rest,
	    App_e(_,Var_e evar, con_args, exp_args, fp_args))=>
	       (case (Sequence.toList fset) of
		    [((evar', econ),
		      Function{recursive = Leaf,
			       tFormals = vklist, eFormals = vclist, fFormals = fplist,
			       body=fun_body,...})] =>
 		       if (Name.eq_var(evar',evar)) then
			   makeLetE Sequential
			   ((List.rev rest) @
			    (createBindings(vklist, con_args,
					    vclist, exp_args,
					    fplist, fp_args)))
			   fun_body
		       else
			   Let_e(Sequential, ebnds, let_body)
		    | _ => Let_e(Sequential, ebnds, let_body))
	 | _ => Let_e(Sequential, ebnds, let_body))

   (* makeApply
         Creates an application.  Optimizes (beta-reduces)
         certain simple cases:

         APP(LET bnds IN body, arg) ==> LET bnds IN (APP(body, arg))
           [when FV(args) and BV(bnds) are disjoint]
    *)
    fun makeAppE (fn_exp as Let_e (Sequential, bnds, exp)) cargs eargs fargs =
	let
	    fun addFree((fvTerm,fvType), fv) = Name.VarSet.union(fv,Name.VarSet.union(fvTerm,fvType))
            fun addExp(e,fv) = addFree(freeExpConVarInExp(true,0,e), fv)
            fun addCon(c,fv) = addFree(freeExpConVarInCon(true,0,c), fv)

	    val fv = foldl addExp VarSet.empty eargs
	    val fv = foldl addExp fv fargs
	    val fv = foldl addCon  fv cargs

	    fun vf_mem(v,_) = VarSet.member(fv,v)

	    fun bnd_check (Con_b(_,Con_cb(v,_))) = VarSet.member(fv,v)
	      | bnd_check (Con_b(_,Open_cb(v,_,_))) = VarSet.member(fv,v)
	      | bnd_check (Con_b(_,Code_cb(v,_,_))) = VarSet.member(fv,v)
	      | bnd_check (Exp_b(v,_,_)) = VarSet.member(fv,v)
	      | bnd_check (Fixopen_b vfset) = Listops.orfold vf_mem (map #1 (Sequence.toList vfset))
	      | bnd_check (Fixcode_b vfset) = Listops.orfold vf_mem (map #1 (Sequence.toList vfset))
	      | bnd_check (Fixclosure_b (_,vfset)) =
		Listops.orfold vf_mem (map #1 (Sequence.toList vfset))

	    val intersect = Listops.orfold bnd_check bnds
	in
	    if (intersect) then
		App_e(Open, fn_exp, cargs, eargs, fargs)
	    else
		makeLetE Sequential bnds (makeAppE exp cargs eargs fargs)
	end
      | makeAppE fn_exp cargs eargs fargs =
            App_e(Open, fn_exp, cargs, eargs, fargs)

    fun makeNamedRecordType rname labels cons = 
      let

	fun folder ((l,c),rbnds) =
	  if small_con c then (c,rbnds)
	  else
	    let
	      val tvar = Name.fresh_named_var ((label2name l)^"_type")
	      val bnd = Con_cb(tvar,c)
	    in (Var_c tvar,bnd::rbnds)
	    end
	val (cons,rbnds) = Listops.foldl_acc folder [] (zip labels cons)
	val tvar = Name.fresh_named_var rname
	val rtype = Prim_c( Record_c labels,cons)
	val bnds = rev (Con_cb(tvar,rtype)::rbnds)
      in (bnds,Var_c tvar)
      end

    fun nameType s c = 
      if small_con c then ([],c) 
      else 
	let
	  val cname = Name.fresh_named_var s
	in ([Con_cb(cname,c)],Var_c cname)
	end

    fun makeNamedArrowType rname totality from to = 
      let
	val (fbnds,from) = nameType "from_type" from
	val (tbnds,to)   = nameType "to_type" to
	val arrow = 
	  AllArrow_c{effect = totality, openness = Open,
		     tFormals = [], eFormals = [from], fFormals = 0w0,
		     body_type = to}
	val (abnds,arrow) = nameType rname arrow
      in (fbnds@tbnds@abnds,arrow)
      end

    (* makeSelect.  Given a record expression r and a list lbls
     * of labels, produce the term corresponding to r.lbls
     *)
    fun makeSelect exp [] = exp
      | makeSelect exp (lbl::lbls) =
      makeSelect (Prim_e(NilPrimOp(select lbl), [], [], [exp])) lbls

    (* Rename the constructor parameters in an arrow type *)
    fun rename_arrow ({openness, effect, tFormals, eFormals, fFormals, body_type}, tFormals') =
	let
	  val tfps = (zip tFormals tFormals') handle _ => error "rename_arrow got lists of different length"
	  val (tFormals, ac) = foldl_acc (fn (((v, k), v'), ac) => ((v', NilRename.alphaCRenameKind ac k), Alpha.rename (ac, v, v')))
	    (Alpha.empty_context()) tfps
	  val ren = NilRename.alphaCRenameCon ac
	  val eFormals = map ren eFormals
	  val body_type = ren body_type
	in
	  {openness = openness, effect = effect, tFormals = tFormals, eFormals = eFormals,
	   fFormals = fFormals, body_type = body_type}
	end

    (* Find the strongly connected components of the graph.
     * Result is a list of list of vars and their info.
     * Each list is a scc.  The sccs are ordered such
     * that there are no edges from an earlier component 
     * to a later one (the opposite of the ordering returned 
     * by GraphUtil).
     *)
    fun scc (vars : ((var * 'a) * VarSet.set) list) : (var * 'a) list list = 
      let
	val var2i = 
	  let
	    val map = foldl (fn ((v,i),m) => VarMap.insert(m,v,i)) VarMap.empty (Listops.mapcount (fn (i,((v,a),vars)) => (v,i)) vars)
	  in fn v => valOf (VarMap.find (map,v))
	  end
	val imap = Array.fromList vars
	fun i2va i = #1 (Array.sub(imap,i))
	val nodes = Array.length imap
	fun get_edge (i,(_,edgeset),edges) = VarSet.foldl (fn (v,edges) => (i,var2i v) :: edges) edges edgeset
	val edges = Array.foldli get_edge [] (imap,0,NONE)
	val components = GraphUtil.scc nodes edges
	val components = map (fn l => map i2va l) (rev components)
      in components
      end

    fun mark_non_recur (Function {recursive, effect, tFormals, eFormals, fFormals, body}) =
      (Function{recursive = NonRecursive, effect=effect,
		tFormals=tFormals, eFormals=eFormals, fFormals=fFormals,
		body=body})
      
    (* Break a fix binding into its strongly connected components *)
    fun break_fix [((v,c),f)] = 
      let
	val f = if VarSet.member(Frees.E.freeInFunction f,v)then f else mark_non_recur f
      in [[((v,c),f)]]
      end
      | break_fix (vcfs : ((var * con) * function) list) : ((var * con) * function) list list = 
      let
	(* The set of functions in the nest*)
	val fset = VarSet.addList(VarSet.empty,map (#1 o #1) vcfs)
	  
	(* For each function, get the set of functions from the nest to 
	 * which it refers.  These comprise edges in a graph where the nodes
	 * are the functions.
	 *)
	fun get_frees ((v,c),f) = 
	  let
	    val frees = VarSet.intersection(fset,Frees.E.freeInFunction f)
	    val f = if VarSet.isEmpty frees then mark_non_recur f else f
	  in ((v,(c,f)),frees)
	  end

	val vifrees = map get_frees vcfs

	(* Get the strongly connected components of the graph *)
	val components = scc vifrees

      in map (fn l => map (fn (v,(c,f)) => ((v,c),f)) l) components
      end

end
