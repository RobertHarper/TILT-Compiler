(*$import NILDEFS Nil IlUtil Listops Prim Name TraceInfo Sequence *)

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
      
    and nilprim_is_closed_value(np,clist,elist) = 
      (case np of
	 record _ => Listops.andfold is_closed_value elist
       | inject _ => Listops.andfold is_closed_value elist
       | box_float _ => Listops.andfold is_closed_value elist
       | roll => Listops.andfold is_closed_value elist
       | inj_exn _ => Listops.andfold is_closed_value elist
       | _ => false)
	 
    (*Correct if in a-normal form
     *)
    fun effect (Var_e _) = false
      | effect (Const_e _) = false
      | effect (Unfold_e _) = false
      | effect (Fold_e _)   = false
      | effect (Coerce_e _) = false
      | effect (Prim_e (NilPrimOp make_exntag, _,_, _)) = true
      | effect (Prim_e (NilPrimOp _, _,_, _)) = false
      | effect (Prim_e (PrimOp p, _,_, _)) = 
      (case p of
	 Prim.plus_uint _ => false
       | Prim.minus_uint _ => false
       | Prim.mul_uint _ => false
       | Prim.less_int _ => false
       | Prim.greater_int _ => false
       | Prim.lesseq_int _ => false
       | Prim.greatereq_int _ => false
       | Prim.less_uint _ => false
       | Prim.lesseq_uint _ => false
       | Prim.greatereq_uint _ => false
       | Prim.eq_int _ => false
       | Prim.neq_int _ => false
       | Prim.neg_int _ => false
       | Prim.abs_int _ => false
       | Prim.not_int _ => false
       | Prim.and_int _ => false
       | Prim.or_int _ => false
       | Prim.xor_int _ => false
       | Prim.lshift_int _ => false
       | Prim.rshift_int _ => false 
       | Prim.rshift_uint _ => false
       | _ => true)
      | effect (Let_e (_, bnds, e)) = (Listops.orfold bnd_effect bnds) orelse (effect e)
      | effect _ = true

    and bnd_effect (Exp_b (_,_,e)) = effect e
      | bnd_effect (Con_b _) = false
      | bnd_effect (Fixopen_b _) = false
      | bnd_effect (Fixcode_b _) = false
      | bnd_effect (Fixclosure_b _) = false

    fun covariant_prim p =
      (case p of
	 Sum_c _ => true
       | Record_c _ => true
       | _ => false)
	 
    fun nilprim_uses_carg np =
      (case np of
	 record _ => false
       | select _ => false
       | roll => false
       | unroll => false
       | project_known_record _ => false
       | project_known _ => false
       | project _ => true
       | inject_known_record _ => false
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
      in Prim_c(Record_c (labs,NONE),clist)
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
      
    val unit_con = tuple_con []
      
    val unit_exp = Prim_e(NilPrimOp(record []),[],[],[])
      
    val bool_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=NONE},[con_tuple[]])
    val false_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w0},[con_tuple[]])
    val true_con = Prim_c(Sum_c{tagcount=0w2,totalcount=0w2,known=SOME 0w1},[con_tuple[]])
    val string_con = Prim_c(Vector_c,[Prim_c(Int_c Prim.W8,[])])
    val match_tag = Const_e(Prim.tag(IlUtil.match_tag,unit_con))
    val match_exn = Prim_e(NilPrimOp (inj_exn "match"),[],[],[match_tag,unit_exp])
    val false_exp = Prim_e(NilPrimOp (inject_known 0w0),[],[false_con],[])
    val true_exp = Prim_e(NilPrimOp (inject_known 0w1),[],[true_con],[])
    val int_con = Prim_c(Int_c Prim.W32,[])
    val char_con = Prim_c(Int_c Prim.W8,[])
    val exn_con = Prim_c(Exn_c, [])
    val boxfloat_con = Prim_c(BoxFloat_c Prim.F64, [])
    val unboxfloat_con = Prim_c(Float_c Prim.F64, [])
      
    fun mk_record_with_gctag (labels,trs_opt,cons,exps,name_opt) : (bnd list * exp) = 
      let 
	val (bnds,rcrd) = 
	  (case labels
	     of [] => ([],Prim_e(NilPrimOp (record labels),[],[],[]))
	      | _  => 
	       let
		 val rt = Prim_c (Record_c (labels,NONE),cons)
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