(*
 * This structure is a central repository for NIL syntactic
 * definitions.
 *)

structure NilDefs :> NILDEFS =
  struct

    open Nil Prim


    val fresh_var = Name.fresh_var

    (*
     val small_con : con -> bool
	 small_con con ==> whether con is a "small constructor" suitable to be bound in A-normal form
    *)
    fun small_con con =
	case con of
	    Var_c _ => true
	  | Prim_c (primcon, clist) => length clist = 0
	  | _ => false

    (*
     val small_exp : exp -> bool
	 small_exp exp ==> whether exp is a "small expression" suitable to be bound in A-normal form
    *)
    fun small_exp exp =
	case exp of
	    Var_e _ => true
          | Const_e (Prim.int (Prim.W64,_)) => false
          | Const_e (Prim.uint (Prim.W64,_)) => false
          | Const_e (Prim.int _) => true
          | Const_e (Prim.uint _) => true
	  | _ => false

    fun is_closed_value (Var_e v) = false
      | is_closed_value (Const_e v) = true
      | is_closed_value (Let_e _) = false
      | is_closed_value (Prim_e (NilPrimOp np,trlist,clist,elist)) =
      nilprim_is_closed_value(np,clist,elist)
      | is_closed_value (Prim_e (PrimOp _,_,_,_)) = false
      | is_closed_value (Switch_e _) = false
      | is_closed_value (App_e _) = false
      | is_closed_value (ExternApp_e _ ) = false
      | is_closed_value (Raise_e _) = false
      | is_closed_value (Handle_e _) = false  
      | is_closed_value (Coerce_e(q,_,e)) = (is_closed_value q) andalso (is_closed_value e)
      | is_closed_value (ForgetKnown_e _) = true
      | is_closed_value (Fold_e _)        = true
      | is_closed_value (Unfold_e _)      = true
      
    and nilprim_is_closed_value(np,clist,elist) = 
      (case np of
	 record _ => Listops.andfold is_closed_value elist
       | inject _ => Listops.andfold is_closed_value elist
       | box_float _ => Listops.andfold is_closed_value elist
       | inj_exn _ => Listops.andfold is_closed_value elist
       | _ => false)


    (*See individual comments below
     *)
    fun effect (store,control) e =
      let
	val primcheck =
	  (case (store,control)
	     of (true,true)  => Prim.has_effect
	      | (true,false) => Prim.store_effect
	      | (false,true) => Prim.control_effect)
	fun check e =
	  case e
	    of (Var_e _)     => false
	     | (Const_e _)   => false
	     | (ForgetKnown_e _) => false
	     | (Unfold_e _)  => false
	     | (Fold_e _)    => false
	     | (Coerce_e _)  => false
	     | (Prim_e (NilPrimOp make_exntag, _,_, _)) => store
	     | (Prim_e (NilPrimOp _, _,_, _)) => false
	     | (Prim_e (PrimOp p, _,_, _)) => primcheck p
	     | (Let_e (_, bnds, e)) => (Listops.orfold bnd_check bnds) orelse (check e)
	     | _ => true
	and bnd_check bnd =
	  case bnd
	    of (Exp_b (_,_,e)) => check e
	     | (Con_b _)       => false
	     | (Fixopen_b _)   => false
	     | (Fixcode_b _)   => false
	     | (Fixclosure_b _)=> false
      in check e
      end


    (* The following three functions are predicates on expressions that
     * categorize them according the the kinds of effects they may have.
     * For our purposes, we care about two kinds of effects: control flow
     * effects (non-termination and exceptions) and store effects (reads,
     * writes, and allocates of mutable memory.  See Tarditi's thesis
     * section 5.3.1 for additional discussion.
     *
     * These are only correct if the expression is in a-normal form, since
     * they do not check the arguments to term-constructors.
     *
     * These are a conservative approximation only, since we do not recurse inside
     * of switches, etc.  The intention is that these should only be used on
     * small things, to keep asymptotic completexity down (c.f. Tarditi)
     *)

    (* storeEffect e
     * This function returns true if the expression e may potentially have a
     * store effect.  In particular, if this function returns false, then the
     * effect of e is a subset of {E,N}.  If this function returns true,
     * then the effect of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate may still raise
     * exceptions or not terminate.  This means that while you can safely CSE
     * this term (c.f. Tarditi section 6.1), you cannot eliminate it as dead code.
     *)
    val storeEffect = effect (true,false)

    (* controlEffect e
     * This function returns true if the expression e may potentially have a
     * control effect.  In particular, if this function returns false, then the
     * effect of e is a subset of {A,R,W}.  If this function returns true,
     * then the effect of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate may still depend on
     * or modify the store.  This means that you cannot safely CSE this term
     * nor eliminate it as dead code.
     *)
    val controlEffect = effect (false,true)


    (* anyEffect e
     * This function returns true if the expression e may potentially have some
     * effect.  In particular, if this function returns false, then the effect
     * of e is a subset of {}.  If this function returns true, then the effect
     * of e is a subset of {E,N,A,R,W} (that is, any effect).
     * Note that code that does *not* satisfy this predicate is
     * guaranteed to be tantamount to a value.
     *)
    val anyEffect = effect (true,true)

    fun covariant_prim p =
      (case p of
	 Sum_c _    => true
       | Record_c _ => true
       | GCTag_c    => true
       | Vector_c   => true
       | Vararg_c _ => false  (* Actually, this is covariant in the result type.*)
       | _          => false)

    fun nilprim_uses_carg np =
      (case np of
	 record _ => false
       | select _ => false
       | project_known _ => false
       | project _ => true
       | inject_known _ => false
       | inject _ => true
       | box_float _ => false
       | unbox_float _ => false
       | make_exntag => false
       | inj_exn _ => false
       | make_vararg _ => true
       | make_onearg _ => true
       | mk_record_gctag  => false
       | mk_sum_known_gctag  => false)

  fun aggregate_uses_carg (Prim.OtherArray false) = true
    | aggregate_uses_carg (Prim.OtherVector false) = true
    | aggregate_uses_carg _ = false

  fun prim_uses_carg p =
      let open Prim
      in  (case p of
	     array2vector t => aggregate_uses_carg t
	   | vector2array t => aggregate_uses_carg t
	   | create_table t => aggregate_uses_carg t
	   | create_empty_table t => aggregate_uses_carg t
	   | sub t => aggregate_uses_carg t
	   | update t => aggregate_uses_carg t
	   | length_table t => aggregate_uses_carg t
	   | equal_table t => aggregate_uses_carg t
	   | _ => true)
      end

  fun allprim_uses_carg (NilPrimOp np) = nilprim_uses_carg np
    | allprim_uses_carg (PrimOp p) = prim_uses_carg p


    fun path2con (v,labs) =
	let fun loop c [] = c
	      | loop c (l::rest) = loop (Proj_c(c,l)) rest
	in  loop (Var_c v) labs
	end

    fun con2path c =
	let fun loop (Var_c v,labs) = SOME(v,labs)
  	      | loop (Proj_c(c,l),labs) = loop (c,l::labs)
	      | loop _ = NONE
 	in  loop (c,[])
	end

    val generate_tuple_symbol = IlUtil.generate_tuple_symbol
    val generate_tuple_label = IlUtil.generate_tuple_label

    fun tuple_con clist =
      let fun mapper(i,_) = generate_tuple_label(i+1)
	val labs = Listops.mapcount mapper clist
      in Prim_c(Record_c labs,clist)
      end

    fun con_tuple clist =
      let val lc_list = Listops.mapcount (fn (i,c) => (generate_tuple_label(i+1),c)) clist
      in  Crecord_c lc_list
      end

    fun tuple_kind klist = let fun doer(i,k) = ((generate_tuple_label(i+1),fresh_var()),k)
			       val lvk_list = Listops.mapcount doer klist
			   in  Record_k(Sequence.fromList lvk_list)
			   end

    fun kind_type_tuple 1   = Type_k
    | kind_type_tuple len = tuple_kind(Listops.map0count (fn _ => Type_k) len)

    (*unit*)
    val unit_con = tuple_con []

    (*()*)
    val unit_exp = Prim_e(NilPrimOp(record []),[],[],[])

    (*Type of strings*)
    val string_con = Prim_c(Vector_c,[Prim_c(Int_c Prim.W8,[])])

    (*Type of 32 bit integers
     *)
    val int_con = Prim_c(Int_c Prim.W32,[])
    val zero_int_exp = Const_e(Prim.int(Prim.W32,TilWord64.zero))

    (*Type of 8 bit chars.
     *)
    val char_con = Prim_c(Int_c Prim.W8,[])

    val ftype64 = Prim_c (Float_c Prim.F64,[])  (*Type of 64 bit floats*)

    val boxfloat_con = Prim_c(BoxFloat_c Prim.F64, [])
    val unboxfloat_con = Prim_c(Float_c Prim.F64, [])

    (*Exn type
     *)
    val exn_con = Prim_c(Exn_c, [])

    (* This will not be the same as the user level match tag
     *)
    val match_tag = Const_e(Prim.tag(IlUtil.internal_match_tag,unit_con))

    (* An internal match exception, for use by the compiler.  If the compiler
     * is correct, this should never be raised.
     *)
    val internal_match_exn = Prim_e(NilPrimOp (inj_exn "internal_match"),[],[],[match_tag,unit_exp])

    (* Dummy sum type used by toRtl to stand in for some unknown (and irrelevant) types.
     *)
    val dummy_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[con_tuple[]])

    (* Create a new record, with an appropriate GCTag attached.
     * mk_record_with_gctag lbls traces types exps name
     * lbls   : record labels
     * traces : optional trace args.  If not present, set to TraceUnknown
     * types  : types of fields
     * exps   : field values
     * name   : optional name for the record.
     *
     * If optional name argument is present, then the record will be bound
     * to that variable, and the expression returned will simply be that var
     *)
    fun mk_record_with_gctag (labels,trs_opt,cons,exps,name_opt) : (bnd list * exp) =
      let
	val (bnds,rcrd) =
	  (case labels
	     of [] => ([],Prim_e(NilPrimOp (record labels),[],[],[]))
	      | _  =>
	       let
		 val rt = Prim_c (Record_c labels,cons)
		 val rtvar = Name.fresh_named_var "record_gctag_type"
		 val rtbnd = Con_b(Compiletime,Con_cb(rtvar,rt))
		 val trs = case trs_opt of SOME trs => trs | _ => map (fn _ => TraceUnknown) labels
		 val gctag = Prim_e(NilPrimOp mk_record_gctag,trs,[Var_c rtvar],[])
		 val gtvar = Name.fresh_named_var "record_gctag"
		 val gtbnd = Exp_b(gtvar,TraceKnown TraceInfo.Notrace_Int,gctag)
		 val rcrd = Prim_e(NilPrimOp (record labels),[],[],(Var_e gtvar)::exps)
	       in ([rtbnd,gtbnd],rcrd)
	       end)
      in case name_opt
	   of SOME v => (bnds @ [Exp_b(v,TraceKnown TraceInfo.Trace,rcrd)],Var_e v)
	    | _ => (bnds,rcrd)
      end


  end
