signature PRIM = 
  sig

    datatype intsize = W8 | W16 | W32 | W64
    datatype floatsize = F32 | F64

    (* zero-length arrays and vectors need type *)
    datatype ('con,'exp) value = int     of intsize * TilWord64.word
                               | uint    of intsize * TilWord64.word
			       | float   of floatsize * string
			       | array   of 'con * 'exp Array.array 
			       | vector  of 'con * 'exp Array.array
			       | refcell of 'exp ref
			       | tag     of Name.tag * 'con

    datatype traptype = int_tt | real_tt | both_tt

    datatype prim = 

	(* trap instructions *)
	soft_vtrap of traptype
      | soft_ztrap of traptype
      | hard_vtrap of traptype
      | hard_ztrap of traptype

      (* ref ops *)
      | mk_ref
      | deref

      (* conversions amongst floats, ints, uints with w32 and f64 *)
      | float2int (* floor *)
      | int2float (* real  *) 
      | int2uint of intsize * intsize
      | uint2int of intsize * intsize
      | uinta2uinta of intsize * intsize
      | uintv2uintv of intsize * intsize

      (* ref operation *)
      | eq_ref
      | setref

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
      | lshift_int of intsize
      | rshift_int of intsize       (* right shift arithmetic *)
      | rshift_uint of intsize      (* right shift logical *)

      (* array and vector ops - bool = true indicates writeable *)
      | array2vector
      | sub1 of bool 
      | array1 of bool
      | update1 
      | length1 of bool
      | array_eq of bool (* pointer equality for true and element-wise equality for false *)

      (* array and vector ops - bool = true indicates writeable *)
      | intsub1 of bool 
      | intupdate1 
      | floatsub1 of bool 
      | floatupdate1 
      | ptrsub1 of bool 
      | ptrupdate1 

      (* IO operations *)
      | open_in
      | input
      | input1
      | lookahead
      | open_out
      | close_in
      | output
      | flush_out
      | close_out
      | end_of_stream

    datatype ilprim = 
      (* unsigned int operations: separated for type reasons; they are identical to
         the signed version when viewed at the bit-pattern level *)
        eq_uint of intsize
      | neq_uint of intsize
      | not_uint of intsize
      | and_uint of intsize
      | or_uint of intsize
      | lshift_uint of intsize


  end
