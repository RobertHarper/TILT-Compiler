structure Prim :> PRIM =
  struct

    datatype intsize = W8 | W16 | W32 | W64
    datatype floatsize = F32 | F64

    (* zero-length arrays and vectors need type *)
    datatype ('con,'sv) value = int     of intsize * TilWord64.word
                               | uint    of intsize * TilWord64.word
			       | float   of floatsize * string
			       | array   of 'con * 'sv Array.array
			       | vector  of 'con * 'sv Array.array
			       | refcell of 'sv ref
			       | tag     of Name.tag * 'con

    datatype traptype = int_tt | real_tt | both_tt

    datatype table =
	IntArray of intsize
      | IntVector of intsize
      | FloatArray of floatsize
      | FloatVector of floatsize
      | OtherArray of bool  (* if bool is true, then type is reducible to HNF *)
      | OtherVector of bool

    datatype prim =

	(* trap instructions *)
	soft_vtrap of traptype
      | soft_ztrap of traptype
      | hard_vtrap of traptype
      | hard_ztrap of traptype


      (* conversions amongst floats, ints, uints with w32 and f64 *)
      | float2int (* floor *)
      | int2float (* real  *)
      | int2int of intsize * intsize
      | uint2uint of intsize * intsize
      | int2uint of intsize * intsize
      | uint2int of intsize * intsize
      | uinta2uinta of intsize * intsize
      | uintv2uintv of intsize * intsize


      (* floatint-point operations *)
      | neg_float of floatsize
      | abs_float of floatsize
      | plus_float of floatsize
      | minus_float of floatsize
      | mul_float of floatsize
      | div_float of floatsize
      | less_float of floatsize
      | greater_float of floatsize
      | lesseq_float of floatsize
      | greatereq_float of floatsize
      | eq_float of floatsize
      | neq_float of floatsize

      (* int operations *)
      | plus_int of intsize
      | minus_int of intsize
      | mul_int of intsize
      | div_int of intsize
      | mod_int of intsize
      | quot_int of intsize
      | rem_int of intsize
      | plus_uint of intsize
      | minus_uint of intsize
      | mul_uint of intsize
      | div_uint of intsize
      | mod_uint of intsize
      | less_int of intsize           (* there is difference between signed *)
      | greater_int of intsize        (*   and unsigned comparisons *)
      | lesseq_int of intsize
      | greatereq_int of intsize
      | less_uint of intsize
      | greater_uint of intsize
      | lesseq_uint of intsize
      | greatereq_uint of intsize
      | eq_int of intsize
      | neq_int of intsize
      | neg_int of intsize
      | abs_int of intsize

      (* bit-pattern manipulation *)
      | not_int of intsize
      | and_int of intsize
      | or_int of intsize
      | xor_int of intsize
      | lshift_int of intsize
      | rshift_int of intsize       (* right shift arithmetic *)
      | rshift_uint of intsize      (* right shift logical *)

      (* array and vectors *)
      | array2vector of table
      | vector2array of table
      | create_table of table
      | create_empty_table of table
      | sub of table
      | update of table
      | length_table of table
      | equal_table of table (* pointer equality for array and element-wise equality for vector *)

    datatype ilprim =
      (* unsigned int operations: separated for type reasons; they are identical to
         the signed version when viewed at the bit-pattern level *)
        eq_uint of intsize
      | neq_uint of intsize
      | not_uint of intsize
      | and_uint of intsize
      | or_uint of intsize
      | xor_uint of intsize
      | lshift_uint of intsize

      | mk_ref
      | deref
      | eq_ref
      | setref

    fun same_intsize (size1,size2) =
	(case (size1,size2)
	   of (W8,W8) => true
	    | (W16,W16) => true
	    | (W32,W32) => true
	    | (W64,W64) => true
	    | _ => false)

    fun same_floatsize (size1,size2) =
	(case (size1,size2)
	   of (F32,F32) => true
	    | (F64,F64) => true
	    | _ => false)

    fun same_traptype (t1 : traptype, t2 : traptype) : bool =
	(case (t1, t2)
	   of (int_tt, int_tt) => true
	    | (real_tt, real_tt) => true
	    | (both_tt, both_tt) => true
	    | _ => false)

    fun same_table (t1 : table, t2 : table) : bool =
	(case (t1, t2)
	   of (IntArray s1, IntArray s2) => same_intsize (s1, s2)
	    | (IntVector s1, IntVector s2) => same_intsize (s1, s2)
	    | (FloatArray s1, FloatArray s2) => same_floatsize (s1, s2)
	    | (FloatVector s1, FloatVector s2) => same_floatsize (s1, s2)
	    | (OtherArray hnf1, OtherArray hnf2) => hnf1 = hnf2
	    | (OtherVector hnf1, OtherVector hnf2) => hnf1 = hnf2
	    | _ => false)

    fun same_prim (p1 : prim, p2 : prim) : bool =
	(case (p1, p2)
	   of (soft_vtrap t1, soft_vtrap t2) => same_traptype (t1,t2)
	    | (soft_ztrap t1, soft_ztrap t2) => same_traptype (t1,t2)
	    | (hard_vtrap t1, hard_vtrap t2) => same_traptype (t1,t2)
	    | (hard_ztrap t1, hard_ztrap t2) => same_traptype (t1,t2)
	    | (float2int, float2int) => true
	    | (int2float, int2float) => true
	    | (int2int (s1,d1), int2int (s2,d2)) =>
		same_intsize (s1,s2) andalso same_intsize (d1,d2)
	    | (uint2uint (s1,d1), uint2uint (s2,d2)) =>
		same_intsize (s1,s2) andalso same_intsize (d1,d2)
	    | (int2uint (s1,d1), int2uint (s2,d2)) =>
		same_intsize (s1,s2) andalso same_intsize (d1,d2)
	    | (uint2int (s1,d1), uint2int (s2,d2)) =>
		same_intsize (s1,s2) andalso same_intsize (d1,d2)
	    | (uinta2uinta (s1,d1), uinta2uinta (s2,d2)) =>
		same_intsize (s1,s2) andalso same_intsize (d1,d2)
	    | (uintv2uintv (s1,d1), uintv2uintv (s2,d2)) =>
		same_intsize (s1,s2) andalso same_intsize (d1,d2)
	    | (neg_float s1, neg_float s2) => same_floatsize (s1,s2)
	    | (abs_float s1, abs_float s2) => same_floatsize (s1,s2)
	    | (plus_float s1, plus_float s2) => same_floatsize (s1,s2)
	    | (minus_float s1, minus_float s2) => same_floatsize (s1,s2)
	    | (mul_float s1, mul_float s2) => same_floatsize (s1,s2)
	    | (div_float s1, div_float s2) => same_floatsize (s1,s2)
	    | (less_float s1, less_float s2) => same_floatsize (s1,s2)
	    | (greater_float s1, greater_float s2) => same_floatsize (s1,s2)
	    | (lesseq_float s1, lesseq_float s2) => same_floatsize (s1,s2)
	    | (greatereq_float s1, greatereq_float s2) => same_floatsize (s1,s2)
	    | (eq_float s1, eq_float s2) => same_floatsize (s1,s2)
	    | (neq_float s1, neq_float s2) => same_floatsize (s1,s2)
	    | (plus_int s1, plus_int s2) => same_intsize (s1,s2)
	    | (minus_int s1, minus_int s2) => same_intsize (s1,s2)
	    | (mul_int s1, mul_int s2) => same_intsize (s1,s2)
	    | (div_int s1, div_int s2) => same_intsize (s1,s2)
	    | (mod_int s1, mod_int s2) => same_intsize (s1,s2)
	    | (quot_int s1, quot_int s2) => same_intsize (s1,s2)
	    | (rem_int s1, rem_int s2) => same_intsize (s1,s2)
	    | (plus_uint s1, plus_uint s2) => same_intsize (s1,s2)
	    | (minus_uint s1, minus_uint s2) => same_intsize (s1,s2)
	    | (mul_uint s1, mul_uint s2) => same_intsize (s1,s2)
	    | (div_uint s1, div_uint s2) => same_intsize (s1,s2)
	    | (mod_uint s1, mod_uint s2) => same_intsize (s1,s2)
	    | (less_int s1, less_int s2) => same_intsize (s1,s2)
	    | (greater_int s1, greater_int s2) => same_intsize (s1,s2)
	    | (lesseq_int s1, lesseq_int s2) => same_intsize (s1,s2)
	    | (greatereq_int s1, greatereq_int s2) => same_intsize (s1,s2)
	    | (less_uint s1, less_uint s2) => same_intsize (s1,s2)
	    | (greater_uint s1, greater_uint s2) => same_intsize (s1,s2)
	    | (lesseq_uint s1, lesseq_uint s2) => same_intsize (s1,s2)
	    | (greatereq_uint s1, greatereq_uint s2) => same_intsize (s1,s2)
	    | (eq_int s1, eq_int s2) => same_intsize (s1,s2)
	    | (neq_int s1, neq_int s2) => same_intsize (s1,s2)
	    | (neg_int s1, neg_int s2) => same_intsize (s1,s2)
	    | (abs_int s1, abs_int s2) => same_intsize (s1,s2)
	    | (not_int s1, not_int s2) => same_intsize (s1,s2)
	    | (and_int s1, and_int s2) => same_intsize (s1,s2)
	    | (or_int s1, or_int s2) => same_intsize (s1,s2)
	    | (xor_int s1, xor_int s2) => same_intsize (s1,s2)
	    | (lshift_int s1, lshift_int s2) => same_intsize (s1,s2)
	    | (rshift_int s1, rshift_int s2) => same_intsize (s1,s2)
	    | (rshift_uint s1, rshift_uint s2) => same_intsize (s1,s2)
	    | (array2vector t1, array2vector t2) => same_table (t1,t2)
	    | (vector2array t1, vector2array t2) => same_table (t1,t2)
	    | (create_table t1, create_table t2) => same_table (t1,t2)
	    | (create_empty_table t1, create_empty_table t2) => same_table (t1,t2)
	    | (sub t1, sub t2) => same_table (t1,t2)
	    | (update t1, update t2) => same_table (t1,t2)
	    | (length_table t1, length_table t2) => same_table (t1,t2)
	    | (equal_table t1, equal_table t2) => same_table (t1,t2)
	    | _ => false)

    fun same_ilprim (p1 : ilprim, p2 : ilprim) : bool =
	(case (p1, p2)
	   of (eq_uint s1, eq_uint s2) => same_intsize (s1,s2)
	    | (neq_uint s1, neq_uint s2) => same_intsize (s1,s2)
	    | (not_uint s1, not_uint s2) => same_intsize (s1,s2)
	    | (and_uint s1, and_uint s2) => same_intsize (s1,s2)
	    | (or_uint s1, or_uint s2) => same_intsize (s1,s2)
	    | (xor_uint s1, xor_uint s2) => same_intsize (s1,s2)
	    | (lshift_uint s1, lshift_uint s2) => same_intsize (s1,s2)
	    | (mk_ref, mk_ref) => true
	    | (deref, deref) => true
	    | (eq_ref, eq_ref) => true
	    | (setref, setref) => true
	    | _ => false)

	(* control_effect p
	 * This function returns true if the primitive p
	 * may potentially have a control effect.  See Tarditi's thesis
	 * section 5.3.1 for additional discussion.  In particular, if
	 * this function returns false, then the effect of p is
	 * a subset of {A,R,W}.  If this function returns true, then the
	 * effect of p is a subset of {E,N,A,R,W}.
	 * Note that code which does *not* satisfy this predicate may still
	 * modify or depend on the store.
	 *)
    fun control_effect p =
    (* For some of these, I don't know what the right thing to say is.
     * There are two issues:
     * 1. From the standpoint of the basis, we use these as if they
     * are unchecked: that is, we guard them with the appropriate checks.
     * 2. Depending on the machine, it seems to me that these may still
     * have some sort of control-flow effect: that is, they may set
     * condition codes or cause signals that may affect the program
     * (such as causing it to core dump, in extreme cases).
     * For the time being, I conservatively mark these as having an effect.
     * The ones that I think fall into this category are marked as XXX
     * -leaf
     *)
	case p of
	  (* At least some traps have a control effect.  This
	   * is probably somewhat machine dependent.
	   * -leaf
	   *)
	    soft_vtrap _ => true
	  | soft_ztrap _ => true
	  | hard_vtrap _ => true
	  | hard_ztrap _ => true
(*
	  (* These don't ever raise an exception
	   *)
	  | array2vector _ => false
	  | vector2array _ => false
*)
	  (* XXX May in principle raise size *)
	  | create_table table => true
	  (* XXX May in principle raise Subscript *)
	  | sub t    => true
	  | update t => true

(*
	  | length_table t => false
	  | equal_table t => false
	  | create_empty_table t => false
*)
	  | float2int => true  (*XXX*)
(*
	  (* At least as currently implemented, these never
	   * raise an exception.
	   *)
	  | int2float => false
	  | int2uint _ => false
	  | uint2int _ => false
	  | int2int _ => false
	  | uint2uint _ => false
	  | uinta2uinta _ => false
	  | uintv2uintv _ => false
*)
	  (* These seem likely to be pure, but to be conservative,
	   * I'm marking them as potentially having an effect.
	   * XXX *)
	  | neg_float _ => true
	  | abs_float _ => true
	  | plus_float _ => true
	  | minus_float  _ => true
	  | mul_float _ => true
	  | div_float _  => true
(*
	  (* These seem safe *)
	  | less_float _ => false
	  | greater_float _ => false
	  | lesseq_float _ => false
	  | greatereq_float _ => false
	  | eq_float _ => false
	  | neq_float _ => false
*)
	  (* XXX *)
	  | plus_int _ => true
	  | minus_int _ => true
	  | mul_int _ => true
	  | div_int _ => true
	  | mod_int _ => true
	  | quot_int _ => true
	  | rem_int _ => true
(*
	  (* Everything unsigned seems safe.
	   *)
	  | plus_uint _ => false
	  | minus_uint _ => false
	  | mul_uint _ => false
	  | div_uint _ => false
	  | mod_uint _ => false

	  (* Comparisons are safe *)
	  | less_int _ => false
	  | greater_int _ => false
	  | lesseq_int _ => false
	  | greatereq_int _ => false
	  | less_uint _ => false
	  | greater_uint _ => false
	  | lesseq_uint _ => false
	  | greatereq_uint _ => false
	  | eq_int _ => false
	  | neq_int _ => false
*)
	  (*Probably not safe *)
	  | neg_int _ => true
(*
	  | abs_int _ => false

	  (* bit-pattern manipulation *)
	  | not_int _ => false
	  | and_int _ => false
	  | or_int _ => false
	  | xor_int _ => false
	  | lshift_int _ => false
	  | rshift_int _ => false
	  | rshift_uint _ => false
*)
	  | _ => false



	(* store_effect p
	 * This function returns true if the primitive p
	 * may potentially have a store effect.  See Tarditi's thesis
	 * section 5.3.1 for additional discussion.  In particular, if
	 * this function returns false, then the effect of p is
	 * a subset of {E,N}.  If this function returns true, then the
	 * effect of p is a subset of {E,N,A,R,W} (that is, any effect).
	 *
	 * Note that code which does *not* satisfy this predicate may still
	 * raise exceptions or not terminate.  This means that while you can
	 * safely CSE this term (c.f. Tarditi section 6.1), you cannot
	 * eliminate it as dead code.
	 *)
    fun store_effect p =
	case p of
	  (* I think you have to view traps as having a kind of
	   * "read" store effect, where the store is the condition codes.
	   * -leaf
	   *)
	    soft_vtrap _ => true
	  | soft_ztrap _ => true
	  | hard_vtrap _ => true
	  | hard_ztrap _ => true

	  | array2vector _ => true   (* This reads updateable memory *)
	  | vector2array _ => true   (* This allocates updateable memory
				      * and equality is by reference *)
	  | create_table table =>
	      (case table
		 of IntArray _   => true
		  | FloatArray _ => true
		  | OtherArray _ => true
		  | _ => false)  (* Vectors are not updateable, and
				  * are compared structurally *)		 
	  | sub t    => true
	  | update t => true

	  (* Some or all of these depend on the rounding mode,
	   * which is a kind of store effect.  
	   * Our use of float2int may not - this is unclear.
	   *)
	  | float2int => true
	  | neg_float _ => false
	  | abs_float _ => true
	  | plus_float _ => true
	  | minus_float  _ => true
	  | mul_float _ => true
	  | div_float _  => true

	  | _ => false (* All others have no store effects *)
(*
	  | length_table t => false (* length does not change *)
	  | equal_table t => false  (* equality does not change *)
	  | create_empty_table t => false (*All empty tables are equal *)

	  (* Some of these may raise exceptions, but they do not have
	   * a store effect.
	   *)

	  | int2float => false
	  | int2uint _ => false
	  | uint2int _ => false
	  | int2int _ => false
	  | uint2uint _ => false
	  | uinta2uinta _ => false
	  | uintv2uintv _ => false

	  | less_float _ => false
	  | greater_float _ => false
	  | lesseq_float _ => false
	  | greatereq_float _ => false
	  | eq_float _ => false
	  | neq_float _ => false

	  | plus_int _ => false
	  | minus_int _ => false
	  | mul_int _ => false
	  | div_int _ => false
	  | mod_int _ => false
	  | quot_int _ => false
	  | rem_int _ => false
	  | plus_uint _ => false
	  | minus_uint _ => false
	  | mul_uint _ => false
	  | div_uint _ => false
	  | mod_uint _ => false
	  | less_int _ => false
	  | greater_int _ => false
	  | lesseq_int _ => false
	  | greatereq_int _ => false
	  | less_uint _ => false
	  | greater_uint _ => false
	  | lesseq_uint _ => false
	  | greatereq_uint _ => false
	  | eq_int _ => false
	  | neq_int _ => false
	  | neg_int _ => false
	  | abs_int _ => false

	  (* bit-pattern manipulation *)
	  | not_int _ => false
	  | and_int _ => false
	  | or_int _ => false
	  | xor_int _ => false
	  | lshift_int _ => false
	  | rshift_int _ => false
	  | rshift_uint _ => false
*)


	(* has_effect p
	 * This function returns true if the primitive p
	 * may potentially have an effect.  See Tarditi's thesis
	 * section 5.3.1 for additional discussion.  In particular, if
	 * this function returns false, then the effect of p is
	 * a subset of {}.  If this function returns true, then the
	 * effect of p is a subset of {E,N,A,R,W} (that is, any effect).
	 *
	 * Note that code that does *not* satisfy this predicate is
	 * guaranteed to be tantamount to a value.
	 *)
    fun has_effect p = (control_effect p) orelse (store_effect p)

  end
