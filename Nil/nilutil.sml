(*$import Nil PrimUtil IlUtil NilSubst Ppnil NILUTIL NilSubst Alpha Option ListPair List NilPrimUtilParam TraceInfo *)

structure NilPrimUtil :> PRIMUTIL where type con = Nil.con
                                 where type exp = Nil.exp = PrimUtil(structure PrimUtilParam = NilPrimUtilParam)
structure NilUtil
  :> NILUTIL =
struct

  open Nil Util 
  open Name

  val debug = ref false
  fun error s = Util.error "nilutil.sml" s

  fun fresh_var() = Name.fresh_named_var "nilutil"
  val unzip = ListPair.unzip
  val zip = ListPair.zip
  val foldl_acc = Listops.foldl_acc

   fun extractCbnd (Con_cb(v,c)) = (v,c)
     | extractCbnd (Open_cb(v,vklist,c,k)) = 
       let val v' = Name.derived_var v
       in  (v,Let_c(Sequential,
		    [Open_cb(v',vklist,c,k)],
		    Var_c v'))
       end
     | extractCbnd (Code_cb(v,vklist,c,k)) = 
       let val v' = Name.derived_var v
       in  (v,Let_c(Sequential,
		    [Code_cb(v',vklist,c,k)],
		    Var_c v'))
       end

  val generate_tuple_symbol = IlUtil.generate_tuple_symbol
  val generate_tuple_label = IlUtil.generate_tuple_label
  fun exp_tuple (elist : exp list) = 
      let val labels = Listops.mapcount (fn (i,_) => generate_tuple_label(i+1)) elist
      in Prim_e(NilPrimOp(record labels),[],elist)
      end
  fun con_tuple clist = 
      let fun mapper(i,_) = generate_tuple_label(i+1)
	  val labs = Listops.mapcount mapper clist
      in Prim_c(Record_c (labs,NONE),clist)
      end
  fun con_tuple_inject clist = 
      let val lc_list = Listops.mapcount (fn (i,c) => (generate_tuple_label(i+1),c)) clist
      in  Crecord_c lc_list
      end
  fun kind_tuple klist = let fun doer(i,k) = ((generate_tuple_label(i+1),fresh_var()),k)
			     val lvk_list = Listops.mapcount doer klist
			 in  Record_k(Sequence.fromList lvk_list)
			 end

  val unit_con = con_tuple []
  val unit_exp = exp_tuple []
  val bool_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[con_tuple_inject[]])
  val false_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w0},[con_tuple_inject[]])
  val true_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w1},[con_tuple_inject[]])
  val string_con = Prim_c(Vector_c,[Prim_c(Int_c Prim.W8,[])])
  val match_tag = Const_e(Prim.tag(IlUtil.match_tag,unit_con))
  val match_exn = Prim_e(NilPrimOp (inj_exn "match"),[unit_con],[match_tag,unit_exp])
  val false_exp = Prim_e(NilPrimOp (inject_nonrecord 0w0),[false_con],[])
  val true_exp = Prim_e(NilPrimOp (inject_nonrecord 0w1),[true_con],[])
  val int_con = Prim_c(Int_c Prim.W32,[])
  val char_con = Prim_c(Int_c Prim.W8,[])
  val exn_con = Prim_c(Exn_c, [])
  fun function_type openness (Function(effect,recur,vklist,dep,vclist,vflist,_,c)) = 
      AllArrow_c(openness,effect,vklist, if dep then SOME (map #1 vclist) else NONE,
		 map #2 vclist,TilWord32.fromInt(length vflist),c)

  fun effect (Var_e _) = false
    | effect (Const_e _) = false
    | effect (Prim_e (NilPrimOp _, _, _)) = false
    | effect _ = true

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
      (case cons
	 of [con] => SOME (tagcount,totalcount,known,hd cons)
	  | _ => error "strip_sum given sum not carrying exactly one type argument")
      | strip_sum' _ = NONE
    fun strip_arrow' (AllArrow_c arg) = SOME arg
      | strip_arrow' _ = NONE
    fun strip_externarrow' (ExternArrow_c arg) = SOME arg
      | strip_externarrow' _ = NONE
    fun strip_record' (Prim_c (Record_c (labs,vars),cons)) = SOME (labs,vars,cons)
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
    val strip_externarrow = strip_annotate strip_externarrow'
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


  fun get_arrow_return con = 
    case strip_arrow con
      of SOME (_,_,_,_,_,_,body_c) => SOME body_c
       | NONE => NONE
  (* Local rebindings from imported structures *)


  fun sub_phase (Compiletime, Runtime) = false
    | sub_phase _ = true


  val fresh_named_var = Name.fresh_named_var


  fun selfify (con,kind) =
    (case kind of
          Type_k => SingleType_k con
	| SingleType_k _ => kind
	| Single_k _ => kind
	| Record_k entries => 
	 Record_k (Sequence.map (fn ((l,v),k) => ((l,v),selfify (Proj_c (con,l),k))) entries)
	| Arrow_k (openness,args,return) => 
	 let
	   val (formal_vars,_) = ListPair.unzip args
	   val actuals = List.map Var_c formal_vars
	 in
	   Arrow_k (openness,args,selfify(App_c (con,actuals),return))
	 end)

  fun singletonize (kind as SingleType_k(_),con) = kind
    | singletonize (kind as Single_k(_),con) = kind
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
    val same_intsize = NilPrimUtil.same_intsize
    val same_floatsize = NilPrimUtil.same_floatsize
    val eq_var2 = Name.eq_var2
  end
  (**)
    

    (* collections *)
 
    type bound = {boundcvars : Name.VarSet.set,
		  boundevars : Name.VarSet.set}
(*
    val emptyCollection = Name.VarMap.empty
    fun collInsert (set, (key,value)) = Name.VarSet.insert(set,key)
    fun collMerge set [] = set
      | collMerge set (a::rest) = collMerge (collInsert(set,a)) rest
    fun collMember (set,k) = (case Name.VarMap.find(set,k) of
				  NONE => false
				| SOME _ => true)
*)
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
		       cbndhandler,bndhandler,conhandler,exphandler,kindhandler}, v) =
	(STATE{bound = {boundcvars = boundcvars,
			boundevars = Name.VarSet.add(boundevars, v)},
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
		       val (con_vars,cons) = ListPair.unzip (Sequence.toList defs)
		       val state' = add_convars (state,con_vars)
		       val cons' = List.map (f_con state') cons
		       val defs' = Sequence.fromList (ListPair.zip (con_vars,cons'))
		   in  Mu_c (flag,defs')
		   end
	       
	     | (AllArrow_c confun) => AllArrow_c (f_arrow state confun)
	     | (ExternArrow_c (cons,c)) => ExternArrow_c(map self cons, self c)
		   
	     | (Var_c var) => con

	     | (Typeof_c exp) => Typeof_c(f_exp state exp)

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

  and f_arrow (state : state) (openness, effect, vklist, vlist, clist, numfloat, result) =
    let
      val (vklist,state) = f_vklist state vklist
      val (vlist,clist,state) = 
	  case vlist of
	      SOME vars => let val (vclist,state) = f_vclist state (Listops.zip vars clist)
			   in  (SOME (map #1 vclist), map #2 vclist, state)
			   end
	    | NONE => (NONE, map (f_con state) clist, state)
      val result = f_con state result
    in
	(openness, effect, vklist, vlist, clist, numfloat, result)
    end

  and f_kind (state : state) (arg_kind : kind) = 
    let 
      val self = f_kind state
      val (STATE{bound,kindhandler,...}) = state
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
    let
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

  and f_vclist state vclist =
    let
	fun fold_one ((var,con),state) = 
	    let
		val con' = f_con state con
		val state' = add_var (state, var)
	    in
		((var,con'),state')
	    end
    in
	foldl_acc fold_one state vclist
    end

  and f_arrow_kind state (args, result) =
    let
      val (args',state') = f_vklist state args
      val result' = f_kind state' result
    in
      (args', result')
    end


  and dofun (state : state) (Function(effect,recur,
				      vklist,dep,vclist,vflist,body,con)) = 
      let 
	  val (vklist', state) = f_vklist state vklist
	  val (vclist', state) = f_vclist state vclist
      in
	  Function(effect,recur,vklist', dep, vclist',
		   vflist, f_exp state body,
		   f_con state con)
      end

  and f_bnd (state : state) (bnd : bnd) : bnd list * state = 
      let val (STATE{bound,bndhandler,exphandler,...}) = state
	  fun do_bnd (bnd,s) : bnd list * state = 
	      case bnd of
		  Con_b(p,cb) => let val (cbnds,state) = f_cbnd state cb
				 in  (map (fn cb => Con_b(p,cb)) cbnds, state)
				 end
		| Exp_b(v,tinfo,e) => ([Exp_b(v, tinfo, f_exp state e)], 
				       add_var(state,v))
		| Fixopen_b vfset => 
		      let val s' = Sequence.foldl (fn ((v,f),s) => add_var(s,v)) s vfset
			  fun doer(v,f) = (v,dofun s' f)
		      in  ([Fixopen_b(Sequence.map doer vfset)], s')
		      end
		| Fixcode_b vfset => 
		      let val s' = Sequence.foldl (fn ((v,f),s) => add_var(s,v)) s vfset
			  fun doer(v,f) = (v,dofun s' f)
		      in  ([Fixcode_b(Sequence.map doer vfset)], s')
		      end
		| Fixclosure_b (is_recur,vcset) => 
		      let val s' = Sequence.foldl (fn ((v,{tipe,...}),s) => add_var(s,v)) s vcset 
			  fun doer(v,{code,cenv,venv,tipe}) = 
			  (v,{code = (case (exphandler (bound,Var_e code)) of
					  NOCHANGE => code
					| (CHANGE_RECURSE (Var_e v')) => v'
					| (CHANGE_NORECURSE (Var_e v')) => v'
					| _ => error "can't have non-var in cllosure code comp"),
			  cenv = f_con s' cenv,
			  venv = f_exp s' venv,
			  tipe = f_con s' tipe})
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
	   Intsw_e {arg, size, arms, default} =>
	       Intsw_e {arg = f_exp state arg,
			size = size, 
			arms = map (fn (t,e) => (t,f_exp state e)) arms,
			default = Util.mapopt (f_exp state) default}
	 | Sumsw_e {arg, sumtype, bound, arms, default} =>
	       let val state' = add_var(state,bound)
	       in  Sumsw_e {arg = f_exp state arg,
			    sumtype = f_con state sumtype,
			    bound = bound,
			    arms = map (fn (t,e) => (t,f_exp state' e)) arms,
			    default = Util.mapopt (f_exp state) default}
	       end
	 | Exncase_e {arg, bound, arms, default} =>
	       let val state' = add_var(state,bound)
	       in  Exncase_e {arg = f_exp state arg,
			      bound = bound,
			      arms = map (fn (t,e) => (f_exp state t,f_exp state' e)) arms,
			      default = Util.mapopt (f_exp state) default}
	       end
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
		| ExternApp_e (func,elist) =>
		      ExternApp_e(self func, map self elist)
		| (App_e (openness,func,clist,elist,eflist)) => 
		      App_e(openness,
			    self func,
			    map (f_con state) clist,
			    map self elist, 
			    map self eflist)
		| Raise_e (e,c) => Raise_e(self e, f_con state c)
		| Handle_e (e,v,h) => let val state' = add_var(state,v)
					in  Handle_e(self e, v, f_exp state' h)
					end
      in case (exphandler (bound,exp)) of
	  CHANGE_NORECURSE e => e
	| CHANGE_RECURSE e => doexp e
	| NOCHANGE => doexp exp
      end

  val default_bound = {boundcvars = Name.VarSet.empty, boundevars = Name.VarSet.empty}
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
  fun cbnd_rewrite h cb = hd (#1(f_cbnd (to_handlers h) cb))

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
      fun export_size (ExportValue (_,v)) = 2
	| export_size (ExportType (_,v)) = 2
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
      if (Name.VarSet.member(boundevars,var)) 
	then NOCHANGE
      else (case (expmap var) of
		NONE => NOCHANGE
	      | SOME e => CHANGE_NORECURSE e)
      | exp_handler _ _ = NOCHANGE

    fun free_handler look_in_kind =
	let val free_evars : (var list ref) = ref []
	    val free_cvars : (var list ref) = ref []
	    fun exp_handler ({boundevars,boundcvars},Var_e v) = 
		(if (not (Name.VarSet.member(boundevars,v)) andalso not (member_eq(eq_var,v,!free_evars)))
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

    fun freeConVarInKind k =
	let val (evars_ref,cvars_ref,handler) = free_handler true
	in  f_kind handler k;
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
      let val vc_list = Sequence.toList vcseq
	  val mu_con = Mu_c(flag,vcseq)
	  fun mapper (which,(v,_)) = (v,if (length vc_list = 1)
					    then mu_con
					else Proj_c(mu_con,generate_tuple_label(which+1)))
	  val vc_list' = Listops.mapcount mapper vc_list
	  val conmap = NilSubst.fromList vc_list'
	  val c = (case (Listops.assoc_eq(eq_var,v,vc_list)) of
		     SOME c => c | NONE => error "bad mu type")
      in  NilSubst.substConInCon conmap c
      end

  fun same_openness (Open,Open) = true
    | same_openness (Closure,Closure) = true
    | same_openness (Code,Code) = true
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
	| (Ref_c,Ref_c) => error "ref_c should be removed"
	| (Exntag_c,Exntag_c) => true
	 | (Sum_c {known=k1,tagcount=t1,totalcount=to1},
	    Sum_c {known=k2,tagcount=t2,totalcount=to2}) => (Util.eq_opt (op =,k1,k2)
							     andalso (to1 = to2)
							     andalso (t1 = t2))
	 | (Record_c (labs1,NONE),Record_c (labs2,NONE)) => 
	      Listops.eq_list (eq_label,labs1,labs2)
	 | (Record_c (labs1,SOME vars1),Record_c (labs2,SOME vars2)) => 
	      Listops.eq_list (eq_label,labs1,labs2) andalso
	      Listops.eq_list (eq_var,vars1,vars2) 	      
	 | (Vararg_c (openness1,effect1),Vararg_c (openness2,effect2)) => 
	  (same_openness (openness1,openness2) andalso
	   same_effect (effect1,effect2))
	 | _  => false
    end
  
  fun same_phase (Compiletime, Compiletime) = true
    | same_phase (Runtime, Runtime) = true
    | same_phase _ = false
(*
  fun alpha_equiv_kind' (context) (kind1,kind2) = 
    let
      val recur = alpha_equiv_kind' context
    in
      (case (kind1,kind2) of
	    (Type_k, Type_k) => true
	  | (Singleton_k con1,Singleton_k con2) => alpha_equiv_con' context (con1,con2)
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
	     val def_list1 = Sequence.toList defs1
	     val def_list2 = Sequence.toList defs2
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


          (* XXXX should be threading e_context here too *)
	  | (Typeof_c exp1, Typeof_c exp2) => alpha_equiv_exp' context (exp1,exp2)
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
    (case (k1,k2) of
	  (Type_k, Type_k) => true
	| (Singleton_k c1,Singleton_k c2) => alpha_equiv_con' context (c1,c2)
	| (Singleton_k c1,k2) => false
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

*)

  fun alpha_mu is_bound (vclist) = 
      let fun folder((v,_),subst) = if (is_bound v)
				       then NilSubst.add subst (v,Var_c(Name.derived_var v))
				    else subst
	  val subst = foldl folder (NilSubst.empty()) vclist
	  fun substcon c = NilSubst.substConInCon subst c
	  fun lookup var = (case (substcon (Var_c var)) of
				(Var_c v) => v
			      | _ => error "substcon returned non Var_c")
      in  if (NilSubst.is_empty subst)
	      then (vclist)
	  else (map (fn (v,c) => (lookup v, substcon c)) vclist)
      end

  (* Returns true if the kind is a shape 
   *)
  fun is_shape kind = 
    (case kind 
       of Type_k => true
        | SingleType_k con => false
        | Single_k con => false
        | Record_k elts => Sequence.all (fn (_,k) => is_shape k) elts
        | Arrow_k (openness, formals, return) => 
(****
            (List.all (fn (_,k) => is_shape k) formals) andalso 
****)
            (is_shape return))

   (* makeLetC.
         Creates a constructor-level sequential let given bindings and
         a body.  Optimizes the let when the bindings or the body are
         especially simple:

         LET_C [] IN con                 ==>  con
         LET_C cvar = con IN cvar        ==>  con
         LET_C cvar' = con IN cvar       ==>  cvar  [where cvar != cvar']
         LET_C cbnds, cvar = con IN cvar ==>  makeLetC cbnds con
         LET_C cbnds IN (LET_C cbnds' IN con) ==> makeLetC (cbnds@cbnds') con
         LET_C cvar = con IN cvar.l      ==>  con.l
    *) 
   fun makeLetC nil body = body
     | makeLetC [conbnd as Con_cb(var,con)] (cv as Var_c var') =
       if (Name.eq_var(var,var')) then con else cv
     | makeLetC [conbnd as Con_cb(var,con)] (cv as Proj_c(Var_c var',l)) =
       if (Name.eq_var(var,var')) then Proj_c(con,l) else cv
     | makeLetC cbnds (cv as Var_c var') =
       (case (List.rev cbnds) of
	    Con_cb(var,con)::rest => 
		if (Name.eq_var(var,var')) then
		    makeLetC (rev rest) con
		else
		    Let_c (Sequential, cbnds, cv)
	  | _ => Let_c (Sequential, cbnds, cv))
     | makeLetC cbnds (Let_c(Sequential, cbnds', body)) =
          makeLetC (cbnds @ cbnds') body
     | makeLetC cbnds body = Let_c (Sequential, cbnds, body)



   fun createBindings (vklist, con_args, vclist, exp_args, fplist, fp_args) =
       let
	   val con_bnds = map (fn((cvar, knd), con) => Con_b (Runtime, Con_cb(cvar, con)))
                              (Listops.zip vklist con_args)
           val exp_bnds = map (fn((evar, con), exp) => Exp_b (evar, TraceUnknown, exp))
	                      (Listops.zip vclist exp_args)
	   val float_type = Prim_c (Float_c Prim.F64, [])
           val fp_bnds = map (fn (evar, exp) => Exp_b (evar, TraceUnknown, exp))
	                     (Listops.zip fplist fp_args)
       in
           con_bnds @ exp_bnds @ fp_bnds
       end

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

   fun makeLetE Parallel ebnds body = Let_e(Parallel, ebnds, body)
     | makeLetE _ nil body = body
     | makeLetE _ ebnds (Let_e(Sequential, ebnds', body)) =
          makeLetE Sequential (ebnds @ ebnds') body
     | makeLetE _ ebnds body =
       (case (List.rev ebnds, body) of
(*         (* XXX Breaks a-normal form *)
           (Exp_b(evar',exp)::rest, Var_e evar) => 
	       if (Name.eq_var(evar',evar)) then 
                  makeLetE Sequential (List.rev rest) exp
               else
		   Let_e(Sequential, ebnds, body)
*)
           (Fixopen_b fset :: rest, 
	    App_e(_,Var_e evar, con_args, exp_args, fp_args))=>
	       (case (Sequence.toList fset) of
		    [(evar', 
		      Function(_,Leaf,vklist,dep,vclist,fplist,fnbody,body_t))] => 
 		       if (Name.eq_var(evar',evar)) then
			   makeLetE Sequential 
			   ((List.rev rest) @ 
			    (createBindings(vklist, con_args,
					    vclist, exp_args,
					    fplist, fp_args)))
			   fnbody
		       else
			   Let_e(Sequential, ebnds, body)
		    | _ => Let_e(Sequential, ebnds, body))
	 | _ => Let_e(Sequential, ebnds, body))

   (* makeApply
         Creates an application.  Optimizes (beta-reduces) 
         certain simple cases:

         APP(LET bnds IN body, arg) ==> LET bnds IN (APP(body, arg))
           [when FV(args) and BV(bnds) are disjoint]
    *)
    fun makeAppE (fn_exp as Let_e (Sequential, bnds, exp)) cargs eargs fargs =
	let 
            fun addone(v1,fv) = VarSet.addList(fv,v1)
	    fun addpair((v1,v2),fv) = VarSet.addList(VarSet.addList(fv,v1),v2)

	    val fv = foldl addpair VarSet.empty 
		           (map (fn e => freeExpConVarInExp(true,e))  eargs)
	    val fv = foldl addpair fv (map (fn e => freeExpConVarInExp(true,e)) fargs)
	    val fv = foldl addone  fv (map (fn c => freeConVarInCon(true,c)) cargs)

	    fun vf_mem(v,_) = VarSet.member(fv,v)

	    fun bnd_check (Con_b(_,Con_cb(v,_))) = VarSet.member(fv,v)
	      | bnd_check (Con_b(_,Open_cb(v,_,_,_))) = VarSet.member(fv,v)
	      | bnd_check (Con_b(_,Code_cb(v,_,_,_))) = VarSet.member(fv,v)
	      | bnd_check (Exp_b(v,_,_)) = VarSet.member(fv,v)
	      | bnd_check (Fixopen_b vfset) = Listops.orfold vf_mem (Sequence.toList vfset)
	      | bnd_check (Fixcode_b vfset) = Listops.orfold vf_mem (Sequence.toList vfset)
	      | bnd_check (Fixclosure_b (_,vfset)) = 
		Listops.orfold vf_mem (Sequence.toList vfset)

	    val intersect = Listops.orfold bnd_check bnds
	in  
	    if (intersect) then 
		App_e(Open, fn_exp, cargs, eargs, fargs)
	    else
		makeLetE Sequential bnds (makeAppE exp cargs eargs fargs)
	end
      | makeAppE fn_exp cargs eargs fargs = 
            App_e(Open, fn_exp, cargs, eargs, fargs)

	    
    fun project_from_kind(lvk_seq,con,label) = 	     
      let
	val lvk_list = Sequence.toList lvk_seq
	fun loop (subst,[]) = error "project_kind: Missing label in record, kind.  "
	  | loop (subst,((l,v),k)::rest) = 
	  if (Name.eq_label(label,l))
	    then NilSubst.substConInKind subst k
	  else loop (NilSubst.add subst (v,Proj_c(con,l)),rest)
      in  loop (NilSubst.empty(),lvk_list)
      end

    fun convert_sum_to_special 
          (Prim_c(Sum_c {tagcount,totalcount,known},carriers), w) =
       Prim_c(Sum_c {tagcount=tagcount, totalcount=totalcount, 
                     known = SOME w}, carriers)

end
