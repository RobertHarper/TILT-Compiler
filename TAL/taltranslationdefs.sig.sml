signature TALTRANSLATIONDEFS =
  sig

  structure K :
    sig
      val T32 : Tal.kind
      val T64 : Tal.kind
      val Type : Tal.scale -> Tal.kind
      val Arrow : Tal.kind -> Tal.kind -> Tal.kind
      val Nat : Tal.kind
    end

  structure C :
    sig
      val var : Tal.identifier -> Tal.con
      val nat : Tal.int32 -> Tal.con
      val unit : Tal.con
      val lambda : Tal.kind -> (Tal.identifier -> Tal.con) -> Tal.con
      val lam : Tal.identifier -> Tal.kind -> Tal.con -> Tal.con
      val app : Tal.con -> Tal.con -> Tal.con
      val pair : Tal.con -> Tal.con -> Tal.con
      val pi1 : Tal.con -> Tal.con
      val pi2 : Tal.con -> Tal.con
      val pr : Tal.identifier * (Tal.identifier * Tal.kind) * Tal.kind * Tal.identifier * Tal.con -> Tal.con
      val sumcase : Tal.con -> Tal.identifier -> Tal.con list -> Tal.con
      val inj : Tal.int32 -> Tal.kind -> Tal.con -> Tal.con
      val fold : Tal.kind -> Tal.con -> Tal.con
      val ptr: Tal.con -> Tal.con
    end

  structure T :
    sig

      val list2stack : Tal.con -> Tal.con
      val s_tvar : Tal.identifier
      val s_tvar1 : Tal.identifier
      val s_tvar2 : Tal.identifier
	
      val cs_tvar1 : Tal.identifier
      val cs_tvar2 : Tal.identifier
      val cs_tvar3 : Tal.identifier
      val cs_tvar4 : Tal.identifier
      val cs_tvar5 : Tal.identifier
      val cs_tvar6 : Tal.identifier

      val s_con : Tal.con
      val s_con1 : Tal.con
      val s_con2 : Tal.con
	
      val cs_con1 : Tal.con
      val cs_con2 : Tal.con
      val cs_con3 : Tal.con
      val cs_con4 : Tal.con
      val cs_con5 : Tal.con
      val cs_con6 : Tal.con

      val tag : Tal.con -> Tal.con
      val boxed : Tal.scale -> Tal.con -> Tal.con
      val embed : Tal.scale -> Tal.con -> Tal.con 
      val array : Tal.scale -> Tal.con -> Tal.con
      val array_elts : Tal.scale -> Tal.con -> Tal.con -> Tal.con
      val reft  : Tal.con -> Tal.con
      val dyntag : Tal.con -> Tal.con
      val exn_body' : Tal.con -> Tal.con

      val eta_tag : unit -> Tal.con
      val eta_boxed : Tal.scale -> Tal.con
      val eta_embed : Tal.scale -> Tal.con
      val eta_array : Tal.scale -> Tal.con

      val stackargs : Tal.con list -> Tal.con
      val stackargcons : Tal.con -> Tal.con -> Tal.con

      (* handler_code s_con2 handler_fv_con *)
      val handler_code : Tal.con -> Tal.con -> Tal.con 
      (* handler_frame s_con2*)
      val handler_frame : Tal.con -> Tal.con

      val coercion : Tal.con -> Tal.con -> Tal.con
      val code : Tal.con -> Tal.con -> Tal.con -> Tal.con
      val rtype : Tal.con -> Tal.con

      val sum : Tal.int32 -> Tal.con list -> Tal.con
      val ksum : Tal.int32 -> Tal.int32 -> Tal.con list -> Tal.con
      val tuple : Tal.con list -> Tal.con
      val het_tuple_ptr : Tal.con list -> Tal.con list -> Tal.con
      val unit : unit -> Tal.con
      val exists : (Tal.identifier * Tal.kind) -> Tal.con -> Tal.con
      val forall : (Tal.identifier * Tal.kind) -> Tal.con -> Tal.con
      val externarrow : Tal.scale -> Tal.con list -> Tal.con -> Tal.con

      val rek : ((Tal.identifier * Tal.kind) * Tal.con) -> Tal.con -> Tal.con
      val partial_rek : ((Tal.identifier * Tal.kind) * Tal.con) -> Tal.con 
      val eta_rek : Tal.kind -> Tal.con

      val primcon : Lil.primcon -> Tal.con

      val new_dyntag_type : Lil.con
      val new_ptrarray_type : Lil.con
      val new_intarray_type : Lil.size -> Lil.con
      val new_floatarray_type : Lil.con
    end

  structure E : 
    sig
      val mk_tal_lbl : Lil.label -> Tal.label
      val l_overflow : Tal.label
      val l_unit : Tal.label
      val l_array_zero : Tal.label
      val l_wordarray_zero : Tal.label
      val l_floatarray_zero : Tal.label
      val l_raise_subscript : Tal.label
      val l_div_zero : Tal.label


      (* Note: these are Lil labels *)
      val l_newdyntag : Lil.label
      val l_ptrarray : Lil.label
      val l_intarray : Lil.size -> Lil.label
      val l_floatarray : Lil.label
    end
  end