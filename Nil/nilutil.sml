(*$import NIL PRIMUTIL PRIM ILUTIL ALPHA NILSUBST PPNIL NILUTIL *)
(* -------------------------------------------------------- *)
functor NilUtilFn(structure ArgNil : NIL
		  structure PrimUtil : PRIMUTIL
		  structure ArgPrim : PRIM
		  structure IlUtil : ILUTIL
		  structure Alpha : ALPHA
		  structure Subst : NILSUBST
		  structure PpNil : PPNIL
		  sharing ArgNil = Alpha.Nil = PpNil.Nil
		  and ArgPrim = PrimUtil.Prim = ArgNil.Prim
		  and type ArgNil.exp = Subst.exp
		  and type ArgNil.con = Subst.con
		  and type ArgNil.kind = Subst.kind) 
  :> NILUTIL where Nil = ArgNil 
	     and type alpha_context = Alpha.alpha_context =
struct

  structure Nil = ArgNil
  structure Prim = ArgPrim
  open Nil Util 

  val debug = ref false
  fun error s = Util.error "nilutil.sml" s

  val unzip = ListPair.unzip
  val zip = ListPair.zip
  val foldl_acc = Listops.foldl_acc

  fun generate_tuple_symbol (i : int) = Symbol.labSymbol(Int.toString i)
  fun generate_tuple_label (i : int) = Name.symbol_label(generate_tuple_symbol i)
  fun exp_tuple (eclist : (exp * con) list) = 
      let val clist = map #2 eclist
	  val elist = map #1 eclist
	  val labels = Listops.mapcount (fn (i,_) => generate_tuple_label(i+1)) elist
      in Prim_e(NilPrimOp(record labels),clist,elist)
      end
  fun con_tuple clist = 
      let val labels = Listops.mapcount (fn (i,_) => generate_tuple_label(i+1)) clist
      in Prim_c(Record_c labels,clist)
      end
  fun con_tuple_inject clist = 
      let val lc_list = Listops.mapcount (fn (i,c) => (generate_tuple_label(i+1),c)) clist
      in  Crecord_c lc_list
      end
  fun kind_tuple klist = let fun doer(i,k) = ((generate_tuple_label(i+1),Name.fresh_var()),k)
			     val lvk_list = Listops.mapcount doer klist
			 in  Record_k(Util.list2sequence lvk_list)
			 end
  val unit_con = con_tuple []
  val unit_exp = exp_tuple []
  val bool_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[con_tuple_inject[]])
  val false_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w0},[con_tuple_inject[]])
  val true_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w1},[con_tuple_inject[]])
  val string_con = Prim_c(Vector_c,[Prim_c(Int_c Prim.W8,[])])
  val match_tag = Const_e(Prim.tag(IlUtil.match_tag,unit_con))
  val match_exn = Prim_e(NilPrimOp (inj_exn "match"),[unit_con],[match_tag,unit_exp])
  val false_exp = Prim_e(NilPrimOp inject,[false_con],[])
  val true_exp = Prim_e(NilPrimOp inject,[true_con],[])
  val int_con = Prim_c(Int_c Prim.W32,[])
  val char_con = Prim_c(Int_c Prim.W8,[])

  fun effect (e : exp) = true (* very conservative *)

   fun letc ([],c) = c
     | letc (cbnds,c) = Let_c(Sequential,cbnds,c)
   fun lete ([],e) = e
     | lete (ebnds,e) = Let_e(Sequential,ebnds,e)

  fun cbnd2bnd (Con_cb vc) = Con_b vc
    | cbnd2bnd (Open_cb (confuns as (v,vklist,_,resk))) = 
      let val v' = Name.derived_var v
      in  Con_b(v',Let_c(Sequential,[Open_cb confuns], Var_c v))
      end
    | cbnd2bnd (Code_cb (confuns as (v,vklist,_,resk))) = 
      let val v' = Name.derived_var v
      in  Con_b(v',Let_c(Sequential,[Code_cb confuns], Var_c v))
      end

  fun is_var_e (Var_e v) = true
    | is_var_e _ = false

  fun map_annotate f = 
    let
      fun map (Annotate_c (note,con)) =  (Annotate_c (note,map con))
	| map con = f con
    in
      map
    end
  
  fun strip_annotate f = 
    let
      fun strip (Annotate_c (annotate,con)) =  strip con
	| strip con = f con
    in
      strip
    end
  
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
	if (length cons = 1) then SOME (tagcount,totalcount,known,hd cons)
	    else error "strip_sum given sum not carrying exactly one type argument"
      | strip_sum' _ = NONE
    fun strip_arrow' (AllArrow_c body) = SOME body
      | strip_arrow' _ = NONE
    fun strip_record' (Prim_c (Record_c labels,cons)) = SOME (labels,cons)
      | strip_record' _ = NONE
    fun strip_crecord' (Crecord_c entries) = SOME entries
      | strip_crecord' _ = NONE
    fun strip_proj' (Proj_c (con,label)) = SOME (con,label)
      | strip_proj' _ = NONE
    fun strip_prim' (Prim_c (pcon,args)) = SOME (pcon,args)
      | strip_prim' _ = NONE
    fun strip_app' (App_c (con,actuals)) = SOME (con,actuals)
      | strip_app' _ = NONE

    fun is_exn_con' (Prim_c (Exn_c,[])) = true
      | is_exn_con' _ = false

    fun is_unit_c' (Prim_c(Record_c _,[])) = true
      | is_unit_c' _ = false
  in
    val strip_var = strip_annotate strip_var'
    val strip_exntag = strip_annotate strip_exntag'
    val strip_recursive = strip_annotate strip_recursive'
    val strip_boxfloat = strip_annotate strip_boxfloat'
    val strip_float = strip_annotate strip_float'
    val strip_int = strip_annotate strip_int'
    val strip_sum = strip_annotate strip_sum'
    val strip_arrow = strip_annotate strip_arrow'
    val strip_record = strip_annotate strip_record'
    val strip_crecord = strip_annotate strip_crecord'
    val strip_proj = strip_annotate strip_proj'
    val strip_prim = strip_annotate strip_prim'
    val strip_app = strip_annotate strip_app'

    val is_exn_con = strip_annotate is_exn_con'
    val is_unit_c = strip_annotate is_unit_c'
    val is_var_c = Option.isSome o strip_var
    val is_float_c = Option.isSome o strip_float
  end

  fun strip_singleton (Singleton_k(_,k,_)) = strip_singleton k
    | strip_singleton k = k

  fun get_arrow_return con = 
    case strip_arrow con
      of SOME (_,_,_,_,_,body_c) => SOME body_c
       | NONE => NONE

  (* Local rebindings from imported structures *)


  fun sub_phase (Compiletime, Runtime) = false
    | sub_phase _ = true

  fun get_phase kind = 
    (case kind of
          Type_k p => p
        | Word_k p => p
        | Singleton_k (p,_,_) => p
	| Record_k entries => 
	 if allsequence (fn ((l,v),k) => sub_phase (get_phase k,Runtime)) entries then
	   Runtime
	 else
	   Compiletime
       | Arrow_k (openness,args,result) => 
	   if List.all (fn (v,k) => sub_phase (get_phase k,Runtime)) args 
	     andalso sub_phase(get_phase result,Runtime) then
	     Runtime
	   else
	     Compiletime)

  val fresh_named_var = Name.fresh_named_var


  fun selfify (con,kind) =
    (case kind of
          Type_k phase => Singleton_k(phase,Type_k phase,con)
	| Word_k phase => Singleton_k(phase,Word_k phase,con)
	| Singleton_k(_) => kind
	| Record_k entries => 
	 Record_k (mapsequence (fn ((l,v),k) => ((l,v),selfify (Proj_c (con,l),k))) entries)
	| Arrow_k (openness,args,return) => 
	 let
	   val (formal_vars,_) = ListPair.unzip args
	   val actuals = List.map Var_c formal_vars
	 in
	   Arrow_k (openness,args,selfify(App_c (con,actuals),return))
	 end)

  fun singletonize (kind as Singleton_k(_,_,_),con) = kind
    | singletonize (kind,con) = selfify(con,kind)

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
    val same_intsize = PrimUtil.same_intsize
    val same_floatsize = PrimUtil.same_floatsize
    val eq_var2 = Name.eq_var2
  end
  (**)
    

    (* collections *)
 
    type bound = {boundcvars : Name.VarSet.set,
		  boundevars : con Name.VarMap.map}
    val emptyCollection = Name.VarMap.empty
    fun collInsert (set, (key,value)) = Name.VarMap.insert(set,key,value)
    fun collMerge set [] = set
      | collMerge set (a::rest) = collMerge (collInsert(set,a)) rest
    fun collMember (set,k) = (case Name.VarMap.find(set,k) of
				  NONE => false
				| SOME _ => true)
    datatype 'a changeopt = NOCHANGE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
    datatype state =
	STATE of {bound : bound,
		  bndhandler : bound * bnd -> (bnd list) changeopt,
		  cbndhandler : bound * conbnd -> (conbnd list) changeopt,
		  conhandler : bound * con -> con changeopt,
                  exphandler : bound * exp -> exp changeopt,
		  kindhandler : bound * kind -> kind changeopt}

    fun add_convars (STATE{bound={boundcvars,boundevars},
			   bndhandler,cbndhandler,conhandler,exphandler,kindhandler},vs) =
	(STATE{bound = {boundcvars = Name.VarSet.addList(boundcvars,vs),
			boundevars = boundevars},
	       bndhandler = bndhandler,
	       cbndhandler = cbndhandler,
	       conhandler = conhandler,
	       exphandler = exphandler,
	       kindhandler = kindhandler})

    fun add_convar (h : state ,v) = add_convars(h,[v])

    fun add_var (STATE{bound={boundevars,boundcvars},
		       cbndhandler,bndhandler,conhandler,exphandler,kindhandler}, v, c) =
	(STATE{bound = {boundcvars = boundcvars,
			boundevars = collInsert (boundevars, (v,c))},
	       bndhandler = bndhandler,
	       cbndhandler = cbndhandler,
	       conhandler = conhandler,
	       exphandler = exphandler,
	       kindhandler = kindhandler})

    

    fun f_cbnd (state : state) (cbnd : conbnd) : (conbnd list * state) = 
	let 
	    val (STATE{bound,cbndhandler,...}) = state
	    fun cbnd_help wrap (var, vklist, c, k) state openness = 
		let fun folder((v,k),(vklist,s)) = 
		    let val k' = f_kind state k
			val s' = add_convar (s,v)
		    in  ((v,k')::vklist,s')
		    end
		    val (rev_vklist',state') = foldl folder ([],state) vklist
		    val c' = f_con state' c
		    val k' = f_kind state' k
		    val vklist' = rev rev_vklist'
		    val funk = Arrow_k(openness,vklist',k')
		in (wrap(var, vklist', c', k'), add_convar(state,var))
		end
	    fun do_cbnd (Con_cb(var, con),state) = 
		let val con' = f_con state con
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
		
  and f_con (state : state) (con : con) : con = 
    let 
      val self = f_con state
      val (STATE{bound,conhandler,...}) = state
      fun docon con = 
	  (case con of
	       (Prim_c (pcon,args)) => (Prim_c (pcon,map self args))
	       
	     | (Mu_c (flag,defs)) =>
		   let
		       val (con_vars,cons) = ListPair.unzip (Util.set2list defs)
		       val state' = add_convars (state,con_vars)
		       val cons' = List.map (f_con state') cons
		       val defs' = Util.list2set (ListPair.zip (con_vars,cons'))
		   in  Mu_c (flag,defs')
		   end
	       
	     | (AllArrow_c confun) => AllArrow_c (f_arrow state confun)
		   
	     | (Var_c var) => con

	     | (Let_c (letsort, cbnds, body)) => 
		   let
		       fun folder(cbnd,(rev_cbnds,accstate)) = 
			   let val s = (case letsort of
					    Parallel => state
					  | Sequential => accstate)
			       val (cbnds',state') = f_cbnd s cbnd
			   in  (cbnds'@rev_cbnds, state')
			   end
		       val (rev_cbnds',state') = foldl folder ([],state) cbnds
		       val cbnds' = rev rev_cbnds'
		       val body' = f_con state' body
		   in
		       Let_c (letsort, cbnds', body')
		   end
	   
	     | (Closure_c (code,env)) =>
		   let
		       val code' = f_con state code
		       val env' = f_con state env
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
	       
             | Typecase_c {arg, arms, default, kind} => 
                   let fun doarm(pc,vklist,c) =   
                       let fun folder((v,k),(vklist,s)) = 
                           let val k' = f_kind state k
                               val s' = add_convar (s,v)
                           in  ((v,k')::vklist,s')
                           end
                           val (rev_vklist',state') = foldl folder ([],state) vklist
                       in  (pc, rev rev_vklist', f_con state' c)
                       end
                       val arms' = map doarm arms
                       val kind' = f_kind state kind
                   in  Typecase_c{arg = self arg,
                                  arms = arms',
                                  default = self default,
                                  kind = kind'}
                   end

	     | (Annotate_c (annot,con)) => 
		   let
		       val con' = self con
		   in
		       Annotate_c (annot, con')
		   end)
    in
	case (conhandler (bound,con)) of
	    CHANGE_NORECURSE c => c
	  | CHANGE_RECURSE c => docon c
	  | NOCHANGE => docon con
    end

  and f_arrow (state : state) (openness, effect, knds, cons, numfloat, result) =
    let
      val knds' = map (fn (var,kind) => 
		       (var, f_kind state kind)) knds
      val state' = add_convars (state,map #1 knds')
      val cons'  = map (f_con state') cons
      val result' = f_con state' result
    in
	(openness, effect, knds', cons', numfloat, result')
    end

  and f_kind (state : state) (arg_kind : kind) = 
    let 
      val self = f_kind state
      val (STATE{bound,kindhandler,...}) = state
      fun dokind kind = 
	  (case kind of
	       Type_k _ => kind

	     | Word_k _ => kind

	     | (Singleton_k(p,kind, con)) =>
	      let
		val kind' = self kind
		val con' = f_con state con
	      in
		Singleton_k(p,kind', con')
	      end

	     | (Record_k fieldseq) =>
	      let
		fun fold_one (((lbl,var),kind),state) = 
		  let
		    val kind'  = f_kind state kind
		    val state' = add_convar (state,var)
		  in
		    (((lbl, var), kind'),state')
		  end
		val field_list = Util.sequence2list fieldseq
		val (field_list',state') = 
		  foldl_acc fold_one state field_list
	      in
		Record_k (Util.list2sequence field_list')
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

  and f_arrow_kind state (args, result) =
    let
      val (args',state') =
	let 
	  fun fold_one ((var,knd),state) = 
	    let
	      val knd' = f_kind state knd
	      val state' = add_convar (state, var)
	    in
	      ((var,knd'),state')
	    end
	in
	  foldl_acc fold_one state args
	end
      val result' = f_kind state' result
    in
      (args', result')
    end


  fun dofun (state : state) (Function(effect,recur,
				      vklist,vclist,vflist,body,con)) = 
      let 
	  fun vkfolder((v,k),(vklist,s)) = let val k' = f_kind s k
					   in ((v,k')::vklist, add_convar(s,v))
					   end
	  fun vcfolder((v,c),(vclist,s)) = let val c' = f_con s c
					   in  ((v,c')::vclist, add_var(s,v,c'))
					   end
	  val (rev_vklist', state) = foldl vkfolder ([],state) vklist
	  val (rev_vclist', state) = foldl vcfolder ([],state) vclist
      in
	  Function(effect,recur,rev rev_vklist', rev rev_vclist',
		   vflist, f_exp state body,
		   f_con state con)
      end

  and f_bnd (state : state) (bnd : bnd) : bnd list * state = 
      let val (STATE{bound,bndhandler,exphandler,...}) = state
	  fun funtype openness (Function(effect,recur,vklist,vclist,vflist,body,c)) = 
	      AllArrow_c(openness,effect,vklist,(map #2 vclist),TilWord32.fromInt(length vflist),c)
	  fun do_bnd (bnd,s) : bnd * state = 
	      case bnd of
		  Con_b(v,c) => (Con_b(v,f_con state c), add_convar(state,v))
		| Exp_b(v,c,e) => (Exp_b(v,f_con state c, f_exp state e), add_var(state,v,c))
		| Fixopen_b vfset => 
		      let val s' = foldset (fn ((v,f),s) => add_var(s,v,funtype Open f)) s vfset
			  fun doer(v,f) = (v,dofun s' f)
		      in  (Fixopen_b(Util.mapset doer vfset), s')
		      end
		| Fixcode_b vfset => 
		      let val s' = foldset (fn ((v,f),s) => add_var(s,v,funtype Code f)) s vfset
			  fun doer(v,f) = (v,dofun s' f)
		      in  (Fixcode_b(Util.mapset doer vfset), s')
		      end
		| Fixclosure_b (is_recur,vcset) => 
		      let val s' = foldset (fn ((v,{tipe,...}),s) => add_var(s,v,tipe)) s vcset 
			  fun doer(v,{code,cenv,venv,tipe}) = 
			  (v,{code = (case (exphandler (bound,Var_e code)) of
					  NOCHANGE => code
					| (CHANGE_RECURSE (Var_e v')) => v'
					| (CHANGE_NORECURSE (Var_e v')) => v'
					| _ => error "can't have non-var in cllosure code comp"),
			  cenv = f_con s' cenv,
			  venv = f_exp s' venv,
			  tipe = f_con s' tipe})
		      in  (Fixclosure_b(is_recur,Util.mapset doer vcset), s')
		      end
      in  case (bndhandler (bound,bnd)) of
	  CHANGE_NORECURSE bs => (bs, state)  (* is this right? *)
	| CHANGE_RECURSE bs =>  let 
				   fun folder(b,(bs,s)) = 
				       let val (b',s') = do_bnd(b,s)
				       in  (b'::bs,s')
				       end
				   val (bs_rev',s) = foldl folder ([],state) bs
			       in  (rev bs_rev', s)
			       end
	| NOCHANGE => let val (b,s) = do_bnd(bnd,state)
		      in ([b],s)
		      end
      end

  and f_switch state sw = 
      (case sw of
	   Intsw_e {result_type, arg, size, arms, default} =>
	       Intsw_e {result_type = f_con state result_type, 
			arg = f_exp state arg,
			size = size, 
			arms = map (fn (t,e) => (t,f_exp state e)) arms,
			default = Util.mapopt (f_exp state) default}
	 | Sumsw_e {result_type, arg, sumtype, bound, arms, default} =>
	       Sumsw_e {result_type = f_con state result_type, 
			arg = f_exp state arg,
			sumtype = f_con state sumtype,
			bound = bound,
			arms = map (fn (t,e) => (t,f_exp state e)) arms,
			default = Util.mapopt (f_exp state) default}
	 | Exncase_e {result_type, arg, bound, arms, default} =>
	       Exncase_e {result_type = f_con state result_type, 
			arg = f_exp state arg,
			bound = bound,
			arms = map (fn (t,e) => (f_exp state t,f_exp state e)) arms,
			default = Util.mapopt (f_exp state) default}
	 | Typecase_e _ => error "typecase not handled")

  and f_exp state (exp : exp) : exp = 
      let val self = f_exp state
	  val (STATE{bound,exphandler,...}) = state
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
		| (Prim_e (ap,clist,elist)) => Prim_e(ap,map (f_con state) clist, map self elist)
		| (Switch_e switch) => Switch_e(f_switch state switch)
		| (App_e (openness,func,clist,elist,eflist)) => 
		      App_e(openness,
			    self func,
			    map (f_con state) clist,
			    map self elist, 
			    map self eflist)
		| Raise_e (e,c) => Raise_e(self e, f_con state c)
		| Handle_e (e,v,h,c) => Handle_e(self e, v, self h, f_con state c)
      in case (exphandler (bound,exp)) of
	  CHANGE_NORECURSE e => e
	| CHANGE_RECURSE e => doexp e
	| NOCHANGE => doexp exp
      end

  val default_bound = {boundcvars = Name.VarSet.empty, boundevars = emptyCollection}
  fun default_bnd_handler _ = NOCHANGE
  fun default_cbnd_handler _ = NOCHANGE
  fun default_exp_handler _ = NOCHANGE
  fun default_con_handler _ = NOCHANGE
  fun default_kind_handler _ = NOCHANGE

  (* --------------- Exported Functions -------------------------- *) 

  type handlers = ((bound * Nil.exp -> Nil.exp changeopt) *
		   (bound * Nil.bnd -> (Nil.bnd list) changeopt) *
		   (bound * Nil.con -> Nil.con changeopt) *
		   (bound * Nil.conbnd -> (Nil.conbnd list) changeopt) *
		   (bound * Nil.kind -> Nil.kind changeopt))
  fun to_handlers (eh,bh,ch,cbh,kh) =
      (STATE{bound = default_bound,
	     bndhandler = bh,
	     cbndhandler = cbh,
	     exphandler = eh,
	     conhandler = ch,
	     kindhandler = kh})
  fun exp_rewrite h e = f_exp (to_handlers h) e
  fun bnd_rewrite h b = #1(f_bnd (to_handlers h) b)
  fun kind_rewrite h k = f_kind(to_handlers h) k
  fun con_rewrite h c = f_con(to_handlers h) c

  fun con_free_convar (argcon : con) : var list = 
    let 
      val free : var list ref = ref []
      fun con_handler ({boundcvars,boundevars},Var_c v) = 
	let
	  val _ = if (Name.VarSet.member(boundcvars,v) orelse
		      member_eq(eq_var,v,!free))
		    then ()
		  else free := (v::(!free))
	in 
	  NOCHANGE
	end
      
	| con_handler _ = NOCHANGE

      val handlers = (STATE{bound = default_bound,
			    bndhandler = default_bnd_handler,
			    cbndhandler = default_cbnd_handler,
			    exphandler = default_exp_handler,
			    conhandler = con_handler,
			    kindhandler = default_kind_handler})
      val _ = f_con handlers argcon
    in !free
    end

  fun convar_occurs_free (var : var, con : con) : bool = 
    let
      val free_vars = con_free_convar con
    in
      List.exists (fn v => eq_var (v,var)) free_vars
    end

  fun kill_singleton k = 
      let fun remove_single (Singleton_k(_,k,_)) = remove_single k
	    | remove_single k = CHANGE_RECURSE k
	  fun kind_handler (_,Singleton_k(_,k,_)) = remove_single k
	    | kind_handler _ = NOCHANGE
	  val handlers = STATE{bound = default_bound,
			       bndhandler = default_bnd_handler,
			       cbndhandler = default_cbnd_handler,
			       exphandler = default_exp_handler,
			       conhandler = default_con_handler,
			       kindhandler = kind_handler}
      in  f_kind handlers k
      end

  local
      fun count_handler() = 
	  let val count = ref 0 
	      fun con_handler _ = (count := (!count) + 1; NOCHANGE)
	      fun exp_handler _ = (count := (!count) + 1; NOCHANGE)
	      fun kind_handler _ = (count := (!count) + 1; NOCHANGE)
	  in 
	      (count,
	       STATE{bound = default_bound,
		     bndhandler = default_bnd_handler,
		     cbndhandler = default_cbnd_handler,
		     exphandler = exp_handler,
		     conhandler = con_handler,
		     kindhandler = kind_handler})
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
      fun kind_size argkind = let val (count,handlers) = count_handler()
			     in  (f_kind handlers argkind; !count)
			     end
      fun import_size (ImportValue (_,_,c)) = 1 + con_size c
	| import_size (ImportType (_,_,k)) = 1 + kind_size k
      fun export_size (ExportValue (_,e,c)) = exp_size e + con_size c
	| export_size (ExportType (_,c,k)) = con_size c + kind_size k
      fun module_size (MODULE{bnds,imports,exports}) =
	  let val count = foldl (fn (bnd,acc) => acc + (bnd_size bnd))    0 bnds
	      val count = foldl (fn (imp,acc) => acc + (import_size imp)) count imports
	      val count = foldl (fn (exp,acc) => acc + (export_size exp)) count exports
	  in  count
	  end
  end

  local 
    fun con_handler conmap ({boundcvars,boundevars},Var_c var) = 
	 if (Name.VarSet.member(boundcvars,var)) 
	   then NOCHANGE
	 else (case (conmap var) of
		 NONE => NOCHANGE
	       | SOME c => CHANGE_NORECURSE c)
      | con_handler _ _ = NOCHANGE
	   
    fun exp_handler expmap ({boundevars,boundcvars},Var_e var) = 
      if (collMember(boundevars,var)) 
	then NOCHANGE
      else (case (expmap var) of
		NONE => NOCHANGE
	      | SOME e => CHANGE_NORECURSE e)
      | exp_handler _ _ = NOCHANGE

    fun free_handler look_in_kind =
	let val free_evars : (var list ref) = ref []
	    val free_cvars : (var list ref) = ref []
	    fun exp_handler ({boundevars,boundcvars},Var_e v) = 
		(if (not (collMember(boundevars,v)) andalso not (member_eq(eq_var,v,!free_evars)))
		     then free_evars := v :: (!free_evars)
		 else ();
		     NOCHANGE)
	      | exp_handler _ = NOCHANGE
	    fun kind_handler (_,k) = CHANGE_NORECURSE k
	    fun con_handler ({boundevars,boundcvars},Var_c v) = 
		(if (not (Name.VarSet.member(boundcvars,v)) 
		     andalso not (member_eq(eq_var,v,!free_cvars)))
		     then free_cvars := v :: (!free_cvars)
		 else ();
		     NOCHANGE)
	      | con_handler _ = NOCHANGE
	in
	    (free_evars,
	     free_cvars,
	     (STATE{bound = default_bound,
		    bndhandler = default_bnd_handler,
		    cbndhandler = default_cbnd_handler,
		    exphandler = exp_handler,
		    conhandler = con_handler,
		    kindhandler = if (look_in_kind)
				      then default_kind_handler
				  else kind_handler}))
	end
      
  in

    fun freeExpConVarInExp(look_in_kind,e) = 
	let val (evars_ref,cvars_ref,handler) = free_handler look_in_kind
	in  f_exp handler e;
	    (!evars_ref, !cvars_ref)
	end
    fun freeConVarInCon(look_in_kind,c) =
	let val (evars_ref,cvars_ref,handler) = free_handler look_in_kind
	in  f_con handler c;
	    !cvars_ref
	end
  end

  fun expvars_occur_free (vars : var list, exp : exp) : bool = 
    let
      val (free_expvars,_) = freeExpConVarInExp(true,exp)
    in
      case (Listops.list_inter_eq (eq_var, vars, free_expvars)) of
	nil => true
      | _ => false
    end

  fun muExpand (flag,vcseq,v) = 
      let val vc_list = sequence2list vcseq
	  val mu_con = Mu_c(flag,vcseq)
	  fun mapper (which,(v,_)) = (v,if (length vc_list = 1)
					    then mu_con
					else Proj_c(mu_con,generate_tuple_label(which+1)))
	  val vc_list' = Listops.mapcount mapper vc_list
	  val conmap = Subst.fromList vc_list'
	  val c = (case (Listops.assoc_eq(eq_var,v,vc_list)) of
		     SOME c => c | NONE => error "bad mu type")
      in  Subst.substConInCon conmap c
      end

  fun same_openness (Open,Open) = true
    | same_openness (Closure,Closure) = true
    | same_openness (Code,Code) = true
    | same_openness (ExternCode,ExternCode) = true
    | same_openness _ = false

  fun same_effect (Total,Total) = true
    | same_effect (Partial,Partial) = true
    | same_effect _ = false


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
	| (Ref_c,Ref_c) => true
	| (Exntag_c,Exntag_c) => true
	 | (Sum_c {known=k1,tagcount=t1,totalcount=to1},
	    Sum_c {known=k2,tagcount=t2,totalcount=to2}) => (Util.eq_opt (op =,k1,k2)
							     andalso (to1 = to2)
							     andalso (t1 = t2))
	 | (Record_c fields1,Record_c fields2) => 
	  (List.length fields1 = List.length fields2) andalso
	  ListPair.all eq_label (fields1,fields2)
	 | (Vararg_c (openness1,effect1),Vararg_c (openness2,effect2)) => 
	  (same_openness (openness1,openness2) andalso
	   same_effect (effect1,effect2))
	 | _  => false
    end
  
  fun same_phase (Compiletime, Compiletime) = true
    | same_phase (Runtime, Runtime) = true
    | same_phase _ = false

  fun alpha_equiv_kind' (context) (kind1,kind2) = 
    let
      val recur = alpha_equiv_kind' context
    in
      (case (kind1,kind2)
	 of (Type_k p1, Type_k p2) => same_phase(p1,p2)
	  | (Word_k p1, Word_k p2) => same_phase(p1,p2)
	  | (Singleton_k (p1,kind1,con1),Singleton_k (p2,kind2,con2)) => 
	   same_phase(p1,p2) andalso
	   recur (kind1,kind2) andalso
	   alpha_equiv_con' context (con1,con2)
	   
	  | (Record_k elts1,Record_k elts2) => 
	   let
	     val conref = ref context
	     fun equiv_one (((lbl1,var1),kind1),((lbl2,var2),kind2)) = 
	       let
		 val kind_equiv = alpha_equiv_kind' (!conref) (kind1,kind2)
		 val _ = conref := alpha_equate_pair (!conref,(var1,var2))
	       in
		 eq_label (lbl1,lbl2) andalso kind_equiv
	       end
	   in
	     eq_len (elts1,elts2) andalso 
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
  and alpha_equiv_con' context (con1,con2) = 
    let
      val recur = alpha_equiv_con' context
    in
      (case (con1,con2)
	 of (Prim_c (pcon1,args1),Prim_c (pcon2,args2)) => 
	   primequiv (pcon1,pcon2) andalso
	   alpha_equiv_con_list context (args1,args2)
	   
	  (*Assume - sets must maintain ordering!  We only judge*)
	  (* mus with the same ordering to be equiv *) 
	  | (Mu_c (flag1,defs1),Mu_c (flag2,defs2)) =>
	   let
	     val def_list1 = Util.set2list defs1
	     val def_list2 = Util.set2list defs2
	     val (var_list1,con_list1) = ListPair.unzip def_list1
	     val (var_list2,con_list2) = ListPair.unzip def_list2
	     val context' = alpha_equate_pairs (context,(var_list1,var_list2))
	   in
	     flag1 = flag2 andalso
	     alpha_equiv_con_list context' (con_list1,con_list2)
	   end

	  | (AllArrow_c (openness1,effect1,tformals1,formals1,flength1,return1),
	     AllArrow_c (openness2,effect2,tformals2,formals2,flength2,return2)) =>
	   let
	     val conref = ref context
	     fun tformal_equiv ((var1,kind1),(var2,kind2)) = 
	       (alpha_equiv_kind' (!conref) (kind1,kind2))
	       before (conref :=  alpha_equate_pair (!conref,(var1,var2)))
	   in
	     same_openness(openness1,openness2) andalso
	     (flength1 = flength2) andalso
	     eq_len (tformals1,tformals2) andalso 
	     ListPair.all tformal_equiv (tformals1,tformals2) andalso 
	     alpha_equiv_con_list (!conref) (formals1,formals2) andalso
	     alpha_equiv_con' (!conref) (return1,return2)
	   end 

	  | (Var_c var1,Var_c var2) => 
	   alpha_pair_eq(context,(var1,var2))

	  | (Let_c (sort1, binds1,con1),Let_c (sort2, binds2,con2)) => 
	   let 
	     val conref = ref context
	     fun equiv_one (Con_cb(var1,con1),Con_cb(var2,con2)) = 
	       (alpha_equiv_con' (!conref) (con1,con2))
	       before (conref := alpha_equate_pair(!conref,(var1,var2)))
	       
	       | equiv_one (Open_cb(var1,formals1,con1,k1),
			     Open_cb(var2,formals2,con2,k2)) =
	       let
		 val conref' = ref (!conref)
		 fun equiv_one ((var1,kind1),(var2,kind2))= 
		   (alpha_equiv_kind' (!conref') (kind1,kind2))
		   before (conref' := alpha_equate_pair(!conref',(var1,var2)))
	       in
		 ((ListPair.all equiv_one (formals1,formals2))
		  andalso alpha_equiv_con' (!conref') (con1,con2))
		 before (conref := alpha_equate_pair(!conref,(var1,var2)))
	       end
	       | equiv_one (Code_cb(var1,formals1,con1,k1),
			    Code_cb(var2,formals2,con2,k2)) =
	       let
		 val conref' = ref (!conref)
		 fun equiv_one ((var1,kind1),(var2,kind2))= 
		   (alpha_equiv_kind' (!conref') (kind1,kind2))
		   before (conref' := alpha_equate_pair(!conref',(var1,var2)))
	       in
		 ((ListPair.all equiv_one (formals1,formals2))
		  andalso alpha_equiv_con' (!conref') (con1,con2))
		 before (conref := alpha_equate_pair(!conref,(var1,var2)))
	       end
	       | equiv_one _ = false
	   in
	     (ListPair.all equiv_one (binds1,binds2))
	     andalso alpha_equiv_con' (!conref) (con1,con2)
	   end
	 
	  | (Closure_c (code1,env1),Closure_c (code2,env2)) => 
	   recur (code1,code2) andalso recur (env1,env2)
	  
	  (* Cannot be dependent.  Note Precondition is sorted labels*)
	  | (Crecord_c entries1,Crecord_c entries2) => 
	   let
	     fun equiv_each ((lbl1,con1),(lbl2,con2)) = 
	       (eq_label (lbl1,lbl2) andalso
		recur (con1,con2))
	   in
	     eq_len (entries1,entries2) andalso 
	     (ListPair.all equiv_each (entries1,entries2))
	   end
	 
	  | (Proj_c (crec1,label1),Proj_c (crec2,label2)) => 
	   eq_label (label1,label2) andalso
	   recur (crec1,crec2)
	   
	  | (App_c (cfun1,actuals1),App_c (cfun2,actuals2)) => 
	   recur (cfun1,cfun2) andalso
	   (ListPair.all recur (actuals1,actuals2))
	   
	  | (Annotate_c (annot1,con1),con2) => 
	   recur (con1,con2)
	  | (con1,Annotate_c (annot1,con2)) => 
	   recur (con1,con2)
	  | _ => false)
	 orelse (if !debug then
		   (lprintl "alpha_equiv_con failed!";
		    printl "Constructor:";
		    PpNil.pp_con con1;
		    lprintl "Not equivalent to :";
		    PpNil.pp_con con2;
		    printl "")
		 else ();
		   false)
		   
    end
  
  and alpha_equiv_con_list context list_pair = 
    eq_len list_pair andalso
    ListPair.all (alpha_equiv_con' context) list_pair

  fun alpha_sub_kind' context (k1,k2) = 
    (case (k1,k2)
       of (Word_k p1, Word_k p2) => sub_phase(p1,p2)
	| (Type_k p1, Type_k p2) => sub_phase(p1,p2)
	| (Word_k p1, Type_k p2) => sub_phase(p1,p2)
	| (Singleton_k (p1,k1,c1),Singleton_k (p2,k2,c2)) => 
	 sub_phase(p1,p2) andalso alpha_equiv_con' context (c1,c2)
	| (Singleton_k (p1,k1,c1),k2) => 
	 sub_phase(p1,get_phase k2) andalso alpha_sub_kind' context (k1,k2)
	| (Arrow_k (openness1, formals1, return1), Arrow_k (openness2, formals2, return2)) => 
	 let
	   val conref = ref context
	   fun sub_one ((var1,kind1),(var2,kind2)) = 
	     (alpha_sub_kind' (!conref) (kind1,kind2))
	     before (conref := alpha_equate_pair (!conref,(var1,var2)))
	 in
	   (same_openness (openness1,openness2) andalso 
	    eq_len (formals1,formals2) andalso 
	    ListPair.all sub_one (formals1,formals2) andalso
	    alpha_sub_kind' (!conref) (return1,return2))
	 end
       
	| (Record_k elts1,Record_k elts2) => 
	 let
	   val conref = ref context
	   fun sub_one (((lbl1,var1),kind1),((lbl2,var2),kind2)) = 
	     (eq_label (lbl1,lbl2) andalso
	      alpha_sub_kind' (!conref) (kind1,kind2))
	     before (conref := (alpha_equate_pair (!conref,(var1,var2))))
	 in
	   eq_len (elts1,elts2) andalso 
	   (ListPair.all sub_one (elts1,elts2))
	 end
	| (_,_) => false)
       
  val alpha_equiv_con = alpha_equiv_con' (empty_context (),empty_context ())

  val alpha_equiv_kind = alpha_equiv_kind' (empty_context (),empty_context ())

  val alpha_sub_kind = alpha_sub_kind' (empty_context (),empty_context ())

  val alpha_sub_kind = alpha_sub_kind' (empty_context (),empty_context ())

(* End exported functions *)

    fun alpha_normalize_con' (context:alpha_context) (con:con) = 
      (case con 
	 of (Prim_c (pcon,args)) => 
	   (Prim_c (pcon,map (alpha_normalize_con' context) args))
	  | (Mu_c (flag, defs)) =>
	   let
	     val (con_vars,cons) = ListPair.unzip (Util.set2list defs)
	     val (context',con_vars') = alpha_bind_list (context,con_vars)
	     val cons' = List.map (alpha_normalize_con' context') cons
	     val defs' = Util.list2set (ListPair.zip (con_vars',cons'))
	   in
	     (Mu_c (flag, defs'))
	   end
	 
	  | (AllArrow_c (openness,effect,tformals,formals,flength,return)) =>
	   let
	     fun fold_one ((var,kind),context) =
	       let
		 val (context',var') = alpha_bind (context,var)
		 val kind' = alpha_normalize_kind' context' kind
	       in
		 ((var',kind'),context')
	       end
	     
	     val (tformals',context') = foldl_acc fold_one context tformals
	     val formals' = map (alpha_normalize_con' context') formals
	     val return' = alpha_normalize_con' context' return
	   in
	     AllArrow_c (openness,effect,tformals',formals',flength,return')
	   end

	  | (Var_c var) => Var_c (substitute (context,var))

	  | (Let_c (letsort, cbnds, body)) => 
	   let
	     
	     fun do_confun Con (var,formals,body,kind) = 
	       let
		 fun fold_one ((var,kind),context) =
		   let
		     val (context',var') = alpha_bind (context,var)
		     val kind' = alpha_normalize_kind' context' kind
		   in
		     ((var',kind'),context')
		   end
		 val (formals',context') = foldl_acc fold_one context formals
		 val body' = alpha_normalize_con' context' body
		 val kind' = alpha_normalize_kind' context' kind
		 val (context'',var') = alpha_bind (context,var) (*Not context'!!*)
	       in
		 (Con (var',formals',body',kind'),context'')
	       end
	     fun folder (cbnd,context) = 
	       case cbnd 
		 of Con_cb (var,con) =>
		   let 
		     val (context',var') = alpha_bind (context,var)
		     val con' = alpha_normalize_con' context' con
		     val cbnd' = Con_cb (var',con')
		   in  
		     (cbnd',context')
		   end
		  | Open_cb body => do_confun Open_cb body
		  | Code_cb body => do_confun Code_cb body

	     val (cbnds',context') = foldl_acc folder context cbnds
	     val body' = alpha_normalize_con' context' body
	   in
	     Let_c (letsort, cbnds', body')
	   end
	 
	  | (Closure_c (code,env)) =>
	   let
	     val code' = alpha_normalize_con' context code
	     val env' = alpha_normalize_con' context env
	   in
	     Closure_c(code', env')
	   end
	 
	  | (Crecord_c entries) =>
	   Crecord_c (map (fn (l,c) => (l,alpha_normalize_con' context c)) entries)
	   
	  | (Proj_c (con,lbl)) =>
	   let
	     val con' = alpha_normalize_con' context con
	   in
	     Proj_c (con', lbl)
	   end
	 
	  | (App_c (cfun,actuals)) =>
	   let
	     val cfun' = alpha_normalize_con' context cfun
	     val actuals' = map (alpha_normalize_con' context) actuals
	   in
	     App_c (cfun', actuals')
	   end
	 
	  | Typecase_c {arg, arms, default, kind} => 
	   let 
	     val arg' = alpha_normalize_con' context arg
	     fun doarm ((pcon,args,body),arms) = 
	       let
		 val (vars,kinds) = ListPair.unzip args
		 val kinds' = map (alpha_normalize_kind' context) kinds
		 val (context',vars') = alpha_bind_list (context,vars)
		 val body' = alpha_normalize_con' context' body
		 val args' = ListPair.zip (vars',kinds')
	       in
		 (pcon,args',body')::arms
	       end
	     val arms' =  List.foldr doarm [] arms
	   in  Typecase_c{arg = arg',
			  arms = arms',
			  default = alpha_normalize_con' context default,
			  kind = alpha_normalize_kind' context kind}
	   end

	  | (Annotate_c (annot,con)) => 
	   let
	     val con' = alpha_normalize_con' context con
	   in
	     Annotate_c (annot, con')
	   end)
    and alpha_normalize_kind' (context:alpha_context) (kind:kind) : kind =
      (case kind of
	 Type_k _ => kind
       | Word_k _ => kind
       | (Singleton_k(p,kind, con)) =>
	   let
	     val kind' = alpha_normalize_kind' context kind
	     val con' = alpha_normalize_con' context con
	   in
	     Singleton_k(p,kind', con')
	   end
	 
       | (Record_k fieldseq) =>
	   let
	     fun fold_one (((lbl,var),knd),context) = 
	       let
		 val kind'  = alpha_normalize_kind' context kind
		 val (context',var') = alpha_bind (context,var)
	       in
		 (((lbl, var'), kind'),context')
	       end
	     val field_list = Util.sequence2list fieldseq
	     val (field_list',context') = 
	       foldl_acc fold_one context field_list
	   in
	     Record_k (Util.list2sequence field_list')
	   end

       | (Arrow_k (openness, args, result)) =>
	   let
	     fun fold_one ((var,kind),context) = 
	       let
		 val kind' = alpha_normalize_kind' context kind
		 val (context',var') = alpha_bind(context, var)
	       in
		 ((var',kind'),context')
	       end
	     val (args',context') = foldl_acc fold_one context args
	     val result' = alpha_normalize_kind' context' result
	   in
	     Arrow_k (openness,args', result')
	   end)

    fun alpha_normalize_exp' 
      (contexts as (e_context : alpha_context, c_context : alpha_context))
      (exp : exp) : exp = 
      (case exp
	 of Var_e var => Var_e (substitute (e_context,var))
	  | Const_e value => 
	   Const_e (alpha_normalize_value' contexts value)
	  | Let_e (letsort,bnds,exp) => 
	   let
	     val (bnds',contexts') = 
	       alpha_normalize_bnds' contexts bnds
	     val exp' = alpha_normalize_exp' contexts' exp
	   in
	     (Let_e (letsort,bnds',exp'))
	   end
	  | Prim_e (allprim,cons,exps) => 
	   let
	     val cons' = map (alpha_normalize_con' c_context) cons
	     val exps' = map (alpha_normalize_exp' contexts) exps
	   in
	     (Prim_e (allprim,cons',exps'))
	   end
	  | Switch_e switch => Switch_e (alpha_normalize_switch' contexts switch)
	  | App_e (openness,exp,cons,exps,floats) =>
	   let
	     val exp' = alpha_normalize_exp' contexts exp
	     val cons' = map (alpha_normalize_con' c_context) cons
	     val exps' = map (alpha_normalize_exp' contexts) exps
	     val floats' = map (alpha_normalize_exp' contexts) floats
	   in
	     App_e (openness,exp',cons',exps',floats')
	   end
	  | Raise_e (exp,con) =>
	   let 
	     val con' = alpha_normalize_con' c_context con
	     val exp' = alpha_normalize_exp' contexts exp
	   in
	     Raise_e (exp',con')
	   end
	  | Handle_e (exp,v,handler,result_type) =>
	   let
	     val exp' = alpha_normalize_exp' contexts exp
	     val function = Function(Partial,Nonleaf,[],[(v,Prim_c(Exn_c,[]))],[],handler,result_type)
	     val Function(_,_,_,[(v',_)],_,handler',result_type') = 
				     alpha_normalize_function' contexts function
	   in
	     Handle_e (exp',v',handler', result_type')
	   end)
    and alpha_normalize_value' 
      (contexts as (e_context : alpha_context, c_context : alpha_context)) value = 
      (case value of
	    (Prim.int _) => value
          | (Prim.uint _) => value
          | (Prim.float _) => value
	  | Prim.array (con,arr) => 
	   let
	     val con' = alpha_normalize_con' c_context con
	   in
	     Array.modify (alpha_normalize_exp' contexts) arr;
	     Prim.array (con',arr)
	   end
	| Prim.vector (con,vec) => 
	   let
	     val con' = alpha_normalize_con' c_context con
	   in
	     Array.modify (alpha_normalize_exp' contexts) vec;
	     Prim.vector (con',vec)
	   end
	| Prim.refcell expref =>
	 let
	   val exp' = alpha_normalize_exp' contexts (!expref)
	 in
	   expref := exp';
	   Prim.refcell expref
	 end
	| Prim.tag (atag,con) => 
	 let
	   val con' = alpha_normalize_con' c_context con
	 in
	   Prim.tag (atag,con')
	 end)
    and alpha_normalize_bnds' contexts bnds =
      foldl_acc alpha_normalize_bnd' contexts bnds
    and alpha_normalize_bnd' (bnd,(e_context,c_context)) = 
	let fun do_funs (wrapper,defs) = 
	    let
		val (vars,functions) = unzip (set2list defs)
		val (e_context',vars') = alpha_bind_list (e_context,vars)
		val functions' = 
		    map (alpha_normalize_function' (e_context',c_context)) functions
		val defs' = list2set (zip (vars',functions'))
	     in (wrapper defs',(e_context',c_context))
	     end
	in
	    (case bnd of
		 Con_b (var, con) =>
		     let
			 val con' = alpha_normalize_con' c_context con
			 val (c_context',var') = alpha_bind (c_context,var)
			 val bnd' = (Con_b (var',con'))
		     in
			 (bnd',(e_context,c_context'))
		     end
	       | Exp_b (var, con, exp) =>
		     let
			 val con' = alpha_normalize_con' c_context con
			 val exp' = alpha_normalize_exp' (e_context,c_context) exp
			 val (e_context',var') = alpha_bind (e_context,var)
			 val bnd' = (Exp_b (var',con',exp'))
		     in
			 (bnd',(e_context',c_context))
		     end
	  | (Fixopen_b defs) => do_funs(Fixopen_b,defs)
	  | (Fixcode_b defs) => do_funs(Fixcode_b,defs)

	 | Fixclosure_b (is_recur,defs) => 
	     let
	       val (vars,closures) = unzip (set2list defs)
	       val (e_context',vars') = alpha_bind_list (e_context,vars)
	       val closures' = map (alpha_normalize_closure' (e_context',c_context)) closures
	       val defs' = list2set (zip (vars',closures'))
	       val bnd' = Fixclosure_b (is_recur, defs)
	     in
	       (bnd',(e_context',c_context))
	     end)
	end

    and alpha_normalize_switch'
      (contexts as (e_context : alpha_context, c_context : alpha_context)) switch = 
      (case switch
	 of Intsw_e {size,arg,result_type,arms,default} =>
	   let
	     val arg' = alpha_normalize_exp' contexts arg
	     val arms' = map_second (alpha_normalize_exp' contexts) arms
	     val result_type' = alpha_normalize_con' c_context result_type
	     val default' = alpha_normalize_exp_opt' contexts default
	   in
	     Intsw_e {size=size,arg=arg',result_type=result_type',
		      arms=arms',default=default'}
	   end
	  | Sumsw_e {sumtype,arg,result_type,bound,arms,default} => 
	   let
	     val arg' = alpha_normalize_exp' contexts arg
	     val sumtype' = alpha_normalize_con' c_context sumtype
	     val result_type' = alpha_normalize_con' c_context result_type
	     val default' = alpha_normalize_exp_opt' contexts default
	     val (e_context', bound') = alpha_bind(e_context,bound)
	     val contexts' = (e_context,c_context)
	     val arms' = map_second (alpha_normalize_exp' contexts') arms
	   in
	     Sumsw_e {sumtype=sumtype',arg=arg',bound=bound',
		      result_type=result_type',
		      arms=arms',default=default'}
	   end
	  | Exncase_e {result_type,arg,bound,arms,default} =>
	   let
	     val arg' = alpha_normalize_exp' contexts arg
	     val default' = alpha_normalize_exp_opt' contexts default
	     val result_type' = alpha_normalize_con' c_context result_type
	     val (e_context', bound') = alpha_bind(e_context,bound)
	     val contexts' = (e_context,c_context)
	     val arms' = map (fn (e1,e2) => (alpha_normalize_exp' contexts' e1,
					     alpha_normalize_exp' contexts' e2)) arms
	   in Exncase_e {result_type=result_type',arg=arg',
			 bound=bound',
			 arms=arms',default=default'}
	   end
	  | Typecase_e _ => error "typecase not handled")

    and alpha_normalize_function'
      (contexts as (e_context : alpha_context, c_context : alpha_context)) 
      (Function (effect,recursive,tformals,formals,fformals,body,return)) = 
      let
	fun folder f ((var,elt),context) = 
	  let
	    val elt' = f context elt
	    val (context',var') = alpha_bind (context,var)
	  in
	    ((var',elt'),context')
	  end
	val (tformals',c_context') = 
	  foldl_acc (folder alpha_normalize_kind') c_context tformals
	val (formals',e_context') = 
	  foldl_acc (folder alpha_normalize_con') e_context formals
	val (e_context'',fformals') = alpha_bind_list (e_context,fformals)
	val body' = alpha_normalize_exp' (e_context',c_context') body
	val return' = alpha_normalize_con' c_context' return
      in
	Function (effect,recursive,tformals',formals',fformals',body',return')
      end
    and alpha_normalize_closure'
      (contexts as (e_context : alpha_context, c_context : alpha_context)) 
      {code:var, cenv:con, venv:exp, tipe:con} = 
      let
	val code' = substitute (e_context,code)
	val cenv' = alpha_normalize_con' c_context cenv
	val venv' = alpha_normalize_exp' (c_context,e_context) venv
	val tipe' = alpha_normalize_con' c_context tipe
      in
	{code=code',cenv=cenv',venv=venv',tipe=tipe'}
      end

    and alpha_normalize_exp_opt' contexts opt = 
      mapopt (alpha_normalize_exp' contexts) opt



    val alpha_normalize_con = 
	 alpha_normalize_con' (empty_context()) 

    val alpha_normalize_kind = 
	 alpha_normalize_kind' (empty_context()) 

    val alpha_normalize_exp = 
	 alpha_normalize_exp' (empty_context(),empty_context())
(*
    fun rename_mu (is_bound,defs,var) = 
	let
	    val defs = sequence2list defs
	    fun make_entry (v,_) = if (is_bound v)
				       then SOME(v,Var_c(Name.derived_var v))
				   else NONE
	    val table = List.mapPartial make_entry defs
	    fun subster v = Listops.assoc_eq(eq_var,v,table)
	    fun find_var v = (case Listops.assoc_eq(eq_var,v,table) of
				  SOME (Var_c v) => v 
				| SOME _ => error "table has only Var_c's"
				| NONE => v)
	    fun rebind [] pair = pair
	      | rebind table (v,c) = (find_var v,substConInCon subster c)
	    val defs' = list2sequence (map (rebind table) defs)
	    val var' = find_var var
	in  Mu_c(defs',var')
	end

*)

  fun type_or_word T = alpha_sub_kind (T,Type_k Runtime)
  fun is_word T = alpha_sub_kind (T,Word_k Runtime)

  fun get_function_type openness (Function(effect, recur, vklist, vclist, vlist, _, con)) =
      AllArrow_c(openness, effect, vklist, map #2 vclist, 
		 Word32.fromInt (List.length vlist), con)

  fun alpha_mu is_bound (vclist) = 
      let fun folder((v,_),subst) = if (is_bound v)
				       then Subst.add subst (v,Var_c(Name.derived_var v))
				    else subst
	  val subst = foldl folder (Subst.empty()) vclist
	  fun substcon c = Subst.substConInCon subst c
	  fun lookup var = (case (substcon (Var_c var)) of
				(Var_c v) => v
			      | _ => error "substcon returned non Var_c")
      in  if (Subst.is_empty subst)
	      then (vclist)
	  else (map (fn (v,c) => (lookup v, substcon c)) vclist)
      end

end
