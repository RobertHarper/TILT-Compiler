(* Basically revamped from NIL version *)


structure IlTable  =

struct
    open Il Prim

    exception NotValue
    exception UNIMP

    fun error s = Util.error "exptable.sml" s


(* General functions *)

    fun cmp_maker less greater =
	fn (a,b) =>
	if less (a, b) then
	    LESS
	else
	    if greater (a,b) then
		GREATER
	    else EQUAL

    val cmp_int:(int*int->order) = cmp_maker op< op>
    val cmp_TilWord64 = cmp_maker TilWord64.slt TilWord64.sgt
    val cmp_uTilWord64 = cmp_maker TilWord64.ult TilWord64.ugt

    fun arrow2int TOTAL = 0
      | arrow2int PARTIAL = 1

    fun cmp_arrow(ef1,ef2) = cmp_int(arrow2int ef1, arrow2int ef2)

    fun cmp_bool (false, false) = EQUAL
      | cmp_bool (false, true) = LESS
      | cmp_bool (true, true) = EQUAL
      | cmp_bool (true, false) = GREATER

    fun cmp_option cmp (a,b) =
	case (a,b) of
	    (SOME a, SOME b) => cmp (a,b)
	  | (SOME a, NONE) => GREATER
	  | (NONE, SOME b) => LESS
	  | (NONE, NONE) => EQUAL

    fun cmp_bnd b = LESS
    fun cmp_seq s = LESS

    (* Takes 2 orders lexicographically to produce another *)
    fun cmp_orders [] = EQUAL
      | cmp_orders (EQUAL::rest) = cmp_orders rest
      | cmp_orders (first::rest) = first


(*
    fun cmp_vectors ((c1, arr1), (c2, arr2)) =
	let
	    val length1 = Array.length arr1
	    val length2 = Array.length arr2
	    fun e2n (Nil.Const_e(Prim.uint(Prim.W8,n))) = n
              | e2n _ = error "cmp_vectors: e2n: ill-formed string"
	    fun loop i =
		if (i >= length1) then
		    EQUAL
		else
		    (case cmp_uTilWord64 (e2n (Array.sub(arr1,i)),
					  e2n (Array.sub(arr2,i))) of
			 EQUAL => loop (i+1)
		       | ord => ord)
	in
	    (case (c1,c2) of
		 (Prim_c(Int_c Prim.W8,[]),
		  Prim_c(Int_c Prim.W8,[])) =>
		     (* Comparison of strings *)
		     (case cmp_int(length1, length2) of
			  EQUAL => loop 0
			| ord => ord)
	       | _ => LESS)
	end
*)

    fun cmp_list cmp (a,b) =
	let fun f (nil,nil) = EQUAL
	      | f (nil,_ :: _) = GREATER
	      | f (_ :: _,nil) = LESS
	      | f (h::t,h'::t') =
		(case cmp(h,h')
		     of EQUAL => f(t,t')
		   | r => r)
	in f (a,b)
	end

    val sskip = 4
    val skip = 7*sskip

    fun hash_intsize sz =
	case sz of
	    W8 => 0
	  | W16 => 1
	  | W32 => 2
	  | W64 => 3

    fun hash_floatsize sz =
	case sz of
	    F32 => 0
	  | F64 => 1


    fun hash_tt tt =
	case tt of
	    int_tt => 0
	  | real_tt => 1
	  | both_tt => 2

    fun hash_bool b =
	case b of
	    false => 0
	  | true => 1

    fun hash_table t =
	case t of
	    IntArray sz => 0 *sskip + hash_intsize sz
	  | IntVector sz => 1*sskip + hash_intsize sz
	  | FloatArray sz => 2*sskip + hash_floatsize sz
	  | FloatVector sz => 3*sskip + hash_floatsize sz
	  | OtherArray false => 4*sskip + 0
	  | OtherArray true => 4*sskip + 1
	  | OtherVector false => 4*sskip + 2
	  | OtherVector true => 4*sskip + 3

    exception Incomparable


    fun cmp_con p =
        (case p of

          (* XXX *)
	     (CON_FLEXRECORD (ref (INDIRECT_FLEXINFO r)), c2) =>
                     cmp_con(CON_FLEXRECORD r, c2)
	  |  (c1, CON_FLEXRECORD (ref (INDIRECT_FLEXINFO r))) =>
                     cmp_con(c1, CON_FLEXRECORD r)
          |  (CON_FLEXRECORD (ref (FLEXINFO (_, true, lclist))), c2) =>
                     cmp_con(CON_RECORD lclist, c2)
          |  (c1,CON_FLEXRECORD (ref (FLEXINFO (_, true, lclist)))) =>
                     cmp_con(c1, CON_RECORD lclist)

          | (CON_FLEXRECORD _, _) => raise Incomparable
          | (_, CON_FLEXRECORD _) => raise Incomparable

	  | (CON_COERCION(vs,c1,c2), CON_COERCION(vs',c1',c2')) =>
		     cmp_orders [cmp_list Name.compare_var (vs,vs'),
				 cmp_con(c1,c1'), cmp_con(c2,c2')]
          | (CON_COERCION _,_) => GREATER
          | (_,CON_COERCION _) => LESS


          | (CON_VAR v1, CON_VAR v2) => Name.compare_var(v1,v2)
          | (CON_VAR _,_) => GREATER
          | (_,CON_VAR _) => LESS

          | (CON_TYVAR tv,c2) => cmp_con(Option.valOf(Tyvar.tyvar_deref tv),
                                         c2)
          | (c1,CON_TYVAR tv) => cmp_con(c1,
                                         Option.valOf(Tyvar.tyvar_deref tv))

          | (CON_OVAR tv,c2) => cmp_con(Option.valOf(Tyvar.tyvar_deref(Tyvar.ocon_deref tv)),
                                         c2)
          | (c1,CON_OVAR tv) => cmp_con(c1,
                                        Option.valOf(Tyvar.tyvar_deref(Tyvar.ocon_deref tv)))

          | (CON_INT sz1, CON_INT sz2) => cmp_int (hash_intsize(sz1), hash_intsize(sz2))
          | (CON_INT _, _) => GREATER
          | (_, CON_INT _) => LESS

          | (CON_UINT sz1, CON_UINT sz2) => cmp_int (hash_intsize(sz1), hash_intsize(sz2))
          | (CON_UINT _, _) => GREATER
          | (_, CON_UINT _) => LESS

          | (CON_FLOAT sz1, CON_FLOAT sz2) => cmp_int (hash_floatsize(sz1), hash_floatsize(sz2))
          | (CON_FLOAT _, _) => GREATER
          | (_, CON_FLOAT _) => LESS

          | (CON_ARRAY c1, CON_ARRAY c2) => cmp_con(c1,c2)
          | (CON_ARRAY _, _) => GREATER
          | (_, CON_ARRAY _) => LESS

          | (CON_VECTOR c1, CON_VECTOR c2) => cmp_con(c1,c2)
          | (CON_VECTOR _, _) => GREATER
          | (_, CON_VECTOR _) => LESS

          | (CON_ANY, CON_ANY) => EQUAL
          | (CON_ANY, _) => GREATER
          | (_, CON_ANY) => LESS

          | (CON_REF c1, CON_REF c2) => cmp_con(c1,c2)
          | (CON_REF _, _) => GREATER
          | (_, CON_REF _) => LESS

          | (CON_TAG c1, CON_TAG c2) => cmp_con(c1,c2)
          | (CON_TAG _, _) => GREATER
          | (_, CON_TAG _) => LESS


          | (CON_ARROW(cl1,c1,b1,ao1), CON_ARROW(cl2,c2,b2,ao2)) =>
                cmp_orders[cmp_con_list(cl1,cl2),
                           cmp_con(c1,c2),
                           cmp_bool(b1,b2),
                           cmp_arrow(Option.valOf
                                        (Util.oneshot_deref ao1),
                                     Option.valOf
                                        (Util.oneshot_deref ao2))]
          | (CON_ARROW _, _) => GREATER
          | (_, CON_ARROW _) => LESS


	  | (CON_APP (con1, clist1) , CON_APP (con2, clist2)) =>
		cmp_orders[cmp_con (con1, con2), cmp_con_list (clist1, clist2)]
	  | (CON_APP _, _) => GREATER
	  | (_, CON_APP _) => LESS

          | (CON_MU c1, CON_MU c2) => cmp_con(c1,c2)
          | (CON_MU _, _) => GREATER
          | (_, CON_MU _) => LESS

          | (CON_RECORD lcl1, CON_RECORD lcl2) =>
              cmp_list  (fn ((l1,c1),(l2,c2)) =>
                           cmp_orders [Name.compare_label(l1,l2),
                                       cmp_con(c1,c2)]) (lcl1,lcl2)
          | (CON_RECORD _, _) => GREATER
          | (_, CON_RECORD _) => LESS

          (* XXX No alpha equivalence *)
          | (CON_FUN (v1,c1), CON_FUN (v2,c2)) =>
               cmp_orders [cmp_list Name.compare_var (v1,v2),
                           cmp_con(c1,c2)]
          | (CON_FUN _, _) => GREATER
          | (_, CON_FUN _) => LESS

          | (CON_SUM{names=n1, noncarriers=nc1, carrier=c1, special=s1},
             CON_SUM{names=n2, noncarriers=nc2, carrier=c2, special=s2}) =>
	       cmp_orders [cmp_list Name.compare_label (n1,n2),
                           Int.compare(nc1, nc2),
                           cmp_con (c1,c2),
                           cmp_option Int.compare (s1, s2)]
          | (CON_SUM _, _) => GREATER
          | (_, CON_SUM _) => LESS

          | (CON_TUPLE_INJECT cl1, CON_TUPLE_INJECT cl2) =>
	       cmp_con_list (cl1,cl2)
          | (CON_TUPLE_INJECT _, _) => GREATER
          | (_, CON_TUPLE_INJECT _) => LESS

          | (CON_TUPLE_PROJECT (n1,c1), CON_TUPLE_PROJECT (n2,c2)) =>
	       cmp_orders [Int.compare(n1,n2),
                           cmp_con(c1,c2)]
          | (CON_TUPLE_PROJECT _, _) => GREATER
          | (_, CON_TUPLE_PROJECT _) => LESS

          | (CON_MODULE_PROJECT (m1,l1), CON_MODULE_PROJECT (m2,l2)) =>
	       cmp_orders [cmp_mod(m1,m2),
                           Name.compare_label(l1,l2)])
(* Redundant
          | (CON_MODULE_PROJECT _, _) => GREATER
          | (_, CON_MODULE_PROJECT _) => LESS
*)

    and cmp_con_list e = cmp_list cmp_con e

    and cmp_mod p =
        (case p of
            (MOD_STRUCTURE _, _) => GREATER
          | (_, MOD_STRUCTURE _) => LESS
          | (MOD_APP _, _) => GREATER
          | (_, MOD_APP _) => LESS
          | (MOD_FUNCTOR _, _) => GREATER
          | (_, MOD_FUNCTOR _) => LESS
          | (MOD_SEAL _, _) => GREATER
          | (_, MOD_SEAL _) => LESS
          | (MOD_LET _, _) => GREATER
          | (_, MOD_LET _) => LESS

          | (MOD_VAR v1, MOD_VAR v2) => Name.compare_var(v1,v2)
          | (MOD_VAR v1, _) => GREATER
          | (_,MOD_VAR v1 ) => LESS
          | (MOD_PROJECT(m1,l1), MOD_PROJECT(m2,l2)) =>
               cmp_orders [cmp_mod(m1,m2),
                           Name.compare_label(l1,l2)])
(* Redundant
          | (MOD_PROJECT _, _) => GREATER
          | (_, MOD_PROJECT _) => LESS)
*)

    structure ConKey =
	struct
	    type ord_key = con
	    val compare = cmp_con
	end
    structure Conmap = BinaryMapFn(ConKey)

end



