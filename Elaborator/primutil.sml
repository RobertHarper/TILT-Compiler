functor PrimUtil(structure PrimUtilParam : PRIMUTILPARAM)
    :> PRIMUTIL where type con = PrimUtilParam.con
		where type exp = PrimUtilParam.exp
		where type context = PrimUtilParam.context
		=

struct


    open Util
    open PrimUtilParam Prim
    type con = con
    type exp = PrimUtilParam.exp

    type context = PrimUtilParam.context

    type value = (con,exp) Prim.value
    val error = fn s => error "primutil.sml" s
    structure Float = Real

    fun fs2is fs = 
      (case fs
	 of F32 => W32
	  | F64 => W64)

    val con_string = con_vector(con_uint W8)
    fun value_type (exp_typer:exp -> con) (scon:value) : con =
	(case scon of
	     (int (is,_)) => con_int is
	   | (uint (is,_)) => con_uint is
	   | (float (fs,_)) => con_float fs
	   | (array (c,_)) => con_array c
	   | (vector (c,_)) => con_vector c
	   | (intarray (sz,_))  => con_intarray sz
	   | (intvector (sz,_)) => con_intvector sz 
	   | (floatarray (sz,_))  => con_floatarray sz
	   | (floatvector (sz,_)) => con_floatvector sz 
	   | (refcell (ref e)) => con_ref (exp_typer e)
	   | (tag (_,c)) => con_tag c)

    fun get_aggregate_type (context:context,prim:prim,aggregate:table,cons:con list) : bool * con list * con =
	let
	    fun help (arg,res) = (false,[arg],res)
	    fun help' (args,res) = (false,args,res)
	    fun thelp (arg,res) = (true,[arg],res)
	    fun thelp' (args,res) = (true,args,res)

	    fun create_empty_array con_array instance = thelp'([], con_array)
	    fun create_empty_vector con_vector instance = thelp'([], con_vector)
	    fun create_array con_array instance = thelp'([con_uint W32, instance], con_array)
	    fun create_vector con_vector instance = thelp'([con_uint W32, instance], con_vector)
	    fun len_array con_array instance = thelp(con_array, con_uint W32)
	    fun len_vector con_vector instance = thelp(con_vector, con_uint W32)
	    fun sub_array con_array instance = help'([con_array, con_uint W32], instance)
	    fun sub_vector con_vector instance = thelp'([con_vector, con_uint W32], instance)
	    fun update_array con_array instance =  help'([con_array, con_uint W32, instance], con_unit)
	    fun eq_array con_array instance = help'([con_array, con_array],con_bool context)
	    fun eq_vector con_vector instance = help(partial_arrow([instance, instance],con_bool context),
					  partial_arrow([con_vector,
							 con_vector],con_bool context))
	    fun array2vector_array con_array con_vector instance = thelp(con_array, con_vector)
	    fun vector2array_vector con_array con_vector instance = thelp(con_vector, con_array)
	    fun do_array con_array con_vector instance =
		(case prim of
		     create_table _ => create_array con_array instance
		   | create_empty_table _ => create_empty_array con_array instance
		   | length_table _ => len_array con_array instance
		   | sub _ => sub_array con_array instance
		   | update _ => update_array con_array instance
		   | equal_table _ => eq_array con_array instance
		   | array2vector _ => array2vector_array con_array con_vector instance
		   | vector2array _ => error "use array2vector"
		   | _ => error "pattern impossibility")
	    fun do_vector con_array con_vector instance =
		(case prim of
		     create_table _ => create_vector con_vector instance
		   | create_empty_table _ => create_empty_vector con_vector instance
		   | length_table _ => len_vector con_vector instance
		   | sub _ => sub_vector con_vector instance
		   | update _ => error "no vector update"
		   | equal_table _ => eq_vector con_vector instance
		   | array2vector _ => error "use vector2array"
		   | vector2array _ => vector2array_vector con_array con_vector instance
		   | _ => error "pattern impossibility")

	in  (case (aggregate,cons) of
		 (OtherArray  _, [instance]) => do_array (con_array instance) (con_vector instance) instance
	       | (OtherVector _, [instance]) => do_vector (con_array instance) (con_vector instance) instance
	       | (IntArray is, []) => do_array (con_intarray is) (con_intvector is) (con_uint is)
	       | (IntVector is, []) => do_vector (con_intarray is) (con_intvector is) (con_uint is)
	       | (FloatArray fs, []) => do_array (con_floatarray fs) (con_floatvector fs) (con_float fs)
	       | (FloatVector fs, []) => do_vector (con_floatarray fs) (con_floatvector fs) (con_float fs)
	       | _ => error "ill-formed table primitive")
	end


  fun get_type (context:context) (prim:prim) (cons:con list) : bool * con list * con =
     let
	 fun help (arg,res) = (false,[arg],res)
	 fun help' (args,res) = (false,args,res)
	 fun thelp (arg,res) = (true,[arg],res)
	 fun thelp' (args,res) = (true,args,res)
     in
	 (case (prim,cons) of
	 (soft_vtrap _,[]) => (false,[],con_unit)
       | (soft_ztrap _,[]) => (false,[],con_unit)
       | (hard_vtrap _,[]) => (false,[],con_unit)
       | (hard_ztrap _,[]) => (false,[],con_unit)

       | (neg_float fs ,[]) => help(con_float fs, con_float fs)
       | (abs_float fs ,[]) => help(con_float fs, con_float fs)
       | (not_int is,[]) => help(con_int is, con_int is)
       | (neg_int is,[]) => help(con_int is, con_int is)
       | (abs_int is,[]) => help(con_int is, con_int is)
       | (float2int,[]) => help(con_float F64, con_int W32)
       | (int2float,[]) => help(con_int W32, con_float F64)
       | (int2int(is1,is2),[]) => help(con_int is1, con_int is2)
       | (uint2uint(is1,is2),[]) => help(con_uint is1, con_uint is2)
       | (int2uint(is1,is2),[]) => help(con_int is1, con_uint is2)
       | (uint2int(is1,is2),[]) => help(con_uint is1, con_int is2)
       | (uinta2uinta(is1,is2),[]) => help(con_intarray is1, con_intarray is2)
       | (uintv2uintv(is1,is2),[]) => help(con_intvector is1, con_intvector is2)


       | (plus_float fs,[]) => help'([con_float fs, con_float fs], con_float fs)
       | (minus_float fs,[]) =>  help'([con_float fs, con_float fs], con_float fs)
       | (mul_float fs,[]) =>  help'([con_float fs, con_float fs], con_float fs)
       | (div_float fs,[]) => help'([con_float fs, con_float fs], con_float fs)

       | (less_float fs,[]) => help'([con_float fs, con_float fs], con_bool context)
       | (greater_float fs,[]) => help'([con_float fs, con_float fs], con_bool context)
       | (lesseq_float fs,[]) => help'([con_float fs, con_float fs], con_bool context)
       | (greatereq_float fs,[]) => help'([con_float fs, con_float fs], con_bool context)
       | (eq_float fs,[]) => help'([con_float fs, con_float fs], con_bool context)
       | (neq_float fs,[])  => help'([con_float fs, con_float fs], con_bool context)

       | (plus_int is,[]) =>  help'([con_int is, con_int is], con_int is)
       | (minus_int is,[]) =>  help'([con_int is, con_int is], con_int is)
       | (mul_int is,[]) =>  help'([con_int is, con_int is], con_int is)
       | (div_int  is,[]) =>  help'([con_int is, con_int is], con_int is)
       | (mod_int is,[]) =>  help'([con_int is, con_int is], con_int is)
       | (quot_int is,[]) => help'([con_int is, con_int is], con_int is)
       | (rem_int is,[]) => help'([con_int is, con_int is], con_int is)

       | (plus_uint is,[]) => help'([con_uint is, con_uint is], con_uint is)
       | (minus_uint is,[]) => help'([con_uint is, con_uint is], con_uint is)
       | (mul_uint is,[]) => help'([con_uint is, con_uint is], con_uint is)
       | (div_uint is,[]) => help'([con_uint is, con_uint is], con_uint is)
       | (mod_uint is,[]) =>  help'([con_uint is, con_uint is], con_uint is)

       | (less_int is,[])      => help'([con_int is, con_int is], con_bool context)
       | (greater_int is,[])   => help'([con_int is, con_int is], con_bool context)
       | (lesseq_int is,[])    => help'([con_int is, con_int is], con_bool context)
       | (greatereq_int is,[]) => help'([con_int is, con_int is], con_bool context)
       | (eq_int is,[])        => help'([con_int is, con_int is], con_bool context)
       | (neq_int is,[])        => help'([con_int is, con_int is], con_bool context)

       | (lshift_int is,[])  => help'([con_int is, con_int W32], con_int is)
       | (rshift_int is,[])  => help'([con_int is, con_int W32], con_int is)
       | (rshift_uint is,[]) => help'([con_uint is, con_int W32], con_uint is)
       | (and_int is,[])     => help'([con_int is, con_int is], con_int is)
       | (or_int is,[])      => help'([con_int is, con_int is], con_int is)
       | (xor_int is,[])     => help'([con_int is, con_int is], con_int is)

       | (less_uint is,[]) => help'([con_uint is, con_uint is], con_bool context)
       | (greater_uint is,[]) => help'([con_uint is, con_uint is], con_bool context)
       | (lesseq_uint is,[]) => help'([con_uint is, con_uint is], con_bool context)
       | (greatereq_uint is,[]) => help'([con_uint is, con_uint is], con_bool context)


       | (array2vector aggregate,cons) => get_aggregate_type(context,prim,aggregate,cons)
       | (vector2array aggregate,cons) => get_aggregate_type(context,prim,aggregate,cons)
       | (create_table aggregate,cons)  => get_aggregate_type(context,prim,aggregate,cons)
       | (create_empty_table aggregate,cons)  => get_aggregate_type(context,prim,aggregate,cons)
       | (length_table aggregate,cons)  => get_aggregate_type(context,prim,aggregate,cons)
       | (sub aggregate,cons)  => get_aggregate_type(context,prim,aggregate,cons)
       | (update aggregate,cons)  => get_aggregate_type(context,prim,aggregate,cons)
       | (equal_table aggregate,cons) => get_aggregate_type(context,prim,aggregate,cons)

       | (mk_ref, [instance]) => (false,[instance],con_ref instance)
       | (deref, [instance]) => (false,[con_ref instance], instance)
       | (setref, [instance]) => (false,[con_ref instance,instance],con_unit)
       | (eq_ref, [instance]) => (true, [con_ref instance, con_ref instance],con_bool context)

       | _ => (Ppprim.pp_prim prim;
	       error "can't get type"))

     end

  fun get_iltype (context:context) (ilprim:ilprim) (cons:con list) : bool * con list * con =
      (case (ilprim,cons) of
	   (not_uint is, []) => (true,[con_uint is], con_uint is)
	 | (and_uint is, []) => (true,[con_uint is, con_uint is], con_uint is)
	 | (or_uint is, []) => (true,[con_uint is, con_uint is], con_uint is)
	 | (xor_uint is, []) => (true,[con_uint is, con_uint is], con_uint is)
	 | (lshift_uint is, []) => (true,[con_uint is, con_int W32], con_uint is)
	 | (eq_uint is, []) => (true, [con_uint is, con_uint is], con_bool context)
	 | (neq_uint is, []) => (true, [con_uint is, con_uint is], con_bool context)

	 | _ => error "get_iltype is ill-formed")


  fun get_type' (context:context) (prim:prim) (args:con list) : con =
      let val (total,incons,outcon) = get_type context prim args
	  val arrow = if total then total_arrow else partial_arrow
      in  arrow(incons,outcon)
      end

  fun get_iltype' (context:context) (ilprim:ilprim) (arg:con list) : con =
      let val (total,incons,outcon) = get_iltype context ilprim arg
	  val arrow = if total then total_arrow else partial_arrow
      in  arrow(incons,outcon)
      end

    (*
	int2value converts a 64-bit twos-complement integer to a value.
	uint2value converts a 64-bit word to a value.  These ensure that
	the value invariants hold.
    *)
    val s8 : TilWord64.word * TilWord64.word =
	(TilWord64.fromInt ~128,TilWord64.fromInt 127)
    val s16 : TilWord64.word * TilWord64.word =
	(TilWord64.fromInt ~32768,TilWord64.fromInt 32767)
    val s32 : TilWord64.word * TilWord64.word =
	(TilWord64.fromSignedHalf TilWord32.most_neg,
	 TilWord64.fromSignedHalf TilWord32.most_pos)
    val s64 : TilWord64.word * TilWord64.word =
	(TilWord64.most_neg, TilWord64.most_pos)

    fun int2value (is:intsize) (w:TilWord64.word) : value =
	let val (low,high) =
		(case is of
		    W8 => s8
		|   W16 => s16
		|   W32 => s32
		|   W64 => s64)
	    val _ =
		if TilWord64.sgte(w,low) andalso TilWord64.slte(w,high) then
		    ()
		else
		    raise Overflow
	in  int(is,w)
	end

    val u8 : TilWord64.word =
	TilWord64.fromInt 255
    val u16 : TilWord64.word =
	TilWord64.fromInt 65535
    val u32 : TilWord64.word =
	TilWord64.fromUnsignedHalf TilWord32.neg_one
    val u64 : TilWord64.word =
	TilWord64.neg_one

    fun uint2value (is:intsize) (w:TilWord64.word) : value =
	let val mask =
		(case is of
		    W8 => u8
		|   W16 => u16
		|   W32 => u32
		|   W64 => u64)
	    val w = TilWord64.andb(w,mask)
	in  uint(is,w)
	end

    fun apply (context:context) (prim:prim) (cons:con list) (vals:exp list) : exp = (* instance arg *)
	let
	    fun bad (s:string) : 'a =
		(print "Error "; print s; print " while applying ";
		 Ppprim.pp_prim prim;
		 print "\n";
		 error "bad apply")

	    (* Some converters.  If the conversion is impossible, a type error has occurred *)
	    val exp2value : exp -> value =
		(fn e =>
		(case (exp2value e) of
		    NONE => bad "exp2value"
		|   SOME v => v))

	    (* NJ fucks up Real.fromString - it doesn't not handle nan and inf properly.
	     * So if we are compiling under NJ, Real.fromString may fail, and so
	     * we have to try to patch it up here.  This should never happen under the
	     * TILT compiled tilt.
	     *)
	    fun string2float (s:string) : real = 
	      let
		fun error() = bad "string2float"
		fun unsigned_alpha_2float s = 
		  if (s = "inf") orelse (s = "infinity") then (Real.posInf)
		  else if s = "nan" then (0.0/0.0) 
 	          else error()
	      in
		(* I believe that the only valid float strings that contain letters other than "E" or "e"
		 * Are [+,~,-]nan and [+,~,-]inf[inity].
		 *)
		if size s > 0 andalso ((Char.contains s #"n") orelse (Char.contains s #"i")) then
		  let
		    val c0 = String.sub (s,0)
		  in
		    if Char.isAlpha c0 then unsigned_alpha_2float s
		    else
		      let 
			val neg = (c0 = #"~") orelse (c0 = #"-")
			val r = unsigned_alpha_2float (String.extract(s,1,NONE))
		      in if neg then ~r else r
		      end
		  end
		else error()
	      end

	    fun value2float (fs:floatsize) (v:value) : real =
		(case v of
		    float (fs',s) =>
			if (fs = fs')
			then
			    (case (Float.fromString s) of
				NONE => string2float s
			    |	SOME f => f)
			else bad "value2float"
		|   _ => bad "value2float")

	    fun value2int (is:intsize) (v:value) : TilWord64.word =
		(case v of
		    int (is',w) => if (is = is') then w else bad "value2int"
		|   _ => bad "value2int")

	    fun value2uint (is:intsize) (v:value) : TilWord64.word =
		(case v of
		    uint (is',w) => if (is = is') then w else bad "value2uint"
		|   _ => bad "value2uint")

	    val value2shiftint : value -> int =
		TilWord64.toInt o (value2int W32)

	    fun value2ref (v:value) : exp ref =
		(case v of
		    refcell r => r
		|   _ => bad "value2ref")

	    fun objbinary (value2obj1:value -> 'a) (value2obj2:value -> 'b) (op2:'a * 'b -> value) : exp =
		(case vals of
		     [a,b] => let val obj1 = value2obj1(exp2value a)
				  val obj2 = value2obj2(exp2value b)
			      in value2exp(op2 (obj1,obj2))
			      end
		   | _ => bad "objbinary")

	    fun objunary (value2obj:value -> 'a) (op1:'a -> value) : exp =
		(case vals of
		     [a] => let val obj = value2obj(exp2value a)
			    in value2exp(op1 obj)
			    end
		   | _ => bad "objunary")

	    fun objpred (value2obj:value -> 'a) (op2:'a * 'a -> bool) : exp =
		(case vals of
		     [a,b] => let val obj1 = value2obj(exp2value a)
				  val obj2 = value2obj(exp2value b)
			      in bool2exp context (op2 (obj1,obj2))
			      end
		   | x => (print ("x has length " ^ (Int.toString (length x)) ^ "\n");
			   bad "objpred"))

	    fun iunary (is:intsize) (op1:TilWord64.word -> TilWord64.word) : exp =
		objunary (value2int is) ((int2value is) o op1)

	    fun ibinary (is:intsize) (op2:TilWord64.word * TilWord64.word -> TilWord64.word) : exp =
		objbinary (value2int is) (value2int is) ((int2value is) o op2)

	    fun isbinary (is:intsize) (op2:TilWord64.word * int -> TilWord64.word) : exp =
		objbinary (value2int is) value2shiftint ((int2value is) o op2)

	    fun ipred (is:intsize) (pred:TilWord64.word * TilWord64.word -> bool) : exp =
		objpred (value2int is) pred

	    fun uunary (is:intsize) (op1:TilWord64.word -> TilWord64.word) : exp =
		objunary (value2uint is) ((uint2value is) o op1)

	    fun ubinary (is:intsize) (op2:TilWord64.word * TilWord64.word -> TilWord64.word) : exp =
		objbinary (value2uint is) (value2uint is) ((uint2value is) o op2)

	    fun usbinary (is:intsize) (op2:TilWord64.word * int -> TilWord64.word) : exp =
		objbinary (value2uint is) value2shiftint ((uint2value is) o op2)

	    fun upred (is:intsize) (pred:TilWord64.word * TilWord64.word -> bool) : exp =
		objpred (value2uint is) pred

(*
	    fun fbinary (fs:floatsize) (op2:real * real -> real) : exp =
		objbinary (value2float fs) (value2float fs)
		    ((fn f => (float(fs,Float.toString f))) o op2)

	    fun funary (fs:floatsize) (op1:real -> real) : exp =
		objunary (value2float fs)
		    ((fn f => (float(fs,Float.toString f))) o op1)

	    fun fpred (fs:floatsize) (pred:real * real -> bool) : exp =
		objpred (value2float fs) pred
*)

	    (* Unless we're really sure we can get the rounding semantics right,
	     * don't try to evaluate floating point ops.  Note: NJ floats are fucked.
	     *)
	    fun fbinary (fs:floatsize) (op2:real * real -> real) : exp = raise Div
	    fun funary (fs:floatsize) (op1:real -> real) : exp = raise Div
	    fun fpred (fs:floatsize) (pred:real * real -> bool) : exp = raise Div
	in
	    (case (prim,cons,vals) of
		 (soft_vtrap _,[],_) => unit_value
	       | (soft_ztrap _,[],_) => unit_value
	       | (hard_vtrap _,[],_) => unit_value
	       | (hard_ztrap _,[],_) => unit_value


	  | (float2int, [], _) => objunary (value2float F64)
	                                (fn f => (int(W32,TilWord64.fromInt(floor f))))
	  | (int2float, [], _) => objunary (value2int W32)
					(fn w => (float(F64,Float.toString(real(TilWord64.toInt w)))))
	  | (int2uint(is1,is2), [], _) => objunary (value2int is1) (uint2value is2)
	  | (uint2int(is1,is2), [], _) => objunary (value2uint is1) (int2value is2)
	  | (int2int(is1,is2), [], _) => objunary (value2int is1) (int2value is2)
	  | (uint2uint(is1,is2), [], _) => objunary (value2uint is1) (uint2value is2)
	  | (uinta2uinta(is1,is2),_,_) => raise UNIMP
	  | (uintv2uintv(is1,is2),_,_) => raise UNIMP

	  | (neg_float fs, [], _) => funary fs (op ~)
	  | (plus_float fs, [], _) => fbinary fs (op +)
	  | (minus_float fs, [], _) => fbinary fs (op -)
	  | (mul_float fs, [], _) => fbinary fs (op * )
	  | (div_float fs, [], _) => fbinary fs (op /)
	  | (less_float fs, [], _) => fpred fs (op <)
	  | (greater_float fs, [], _) => fpred fs (op >)
	  | (lesseq_float fs, [], _) => fpred fs (op <=)
	  | (greatereq_float fs, [], _) => fpred fs (op >=)
	  | (eq_float fs, [], _) => fpred fs (Real.==)
	  | (neq_float fs, [], _) => fpred fs (not o (Real.==))

	  | (plus_int is, [], _) => ibinary is TilWord64.splus
	  | (minus_int is, [], _) => ibinary is TilWord64.sminus
	  | (mul_int is, [], _) => ibinary is TilWord64.smult
	  | (div_int is, [], _) => ibinary is TilWord64.sdiv
	  | (mod_int is, [], _) => ibinary is TilWord64.smod
	  | (quot_int is, [], _) => ibinary is TilWord64.squot
	  | (rem_int is, [], _) => ibinary is TilWord64.smod
      	  | (plus_uint is, [], _) => ubinary is TilWord64.uplus
      	  | (minus_uint is, [], _) => ubinary is TilWord64.uminus
      	  | (mul_uint is, [], _) => ubinary is TilWord64.umult
      	  | (div_uint is, [], _) => ubinary is TilWord64.udiv
      	  | (mod_uint is, [], _) => ubinary is TilWord64.umod
	  | (lshift_int is, [], _) => isbinary is TilWord64.lshift
	  | (rshift_int is, [], _) => isbinary is TilWord64.rshifta
	  | (rshift_uint is, [], _) => usbinary is TilWord64.rshiftl
	  | (neg_int is, [], _) => iunary is TilWord64.snegate
	  | (abs_int is, [], _) => iunary is TilWord64.absolute
	  | (not_int is, [], _) => uunary is TilWord64.notb
	  | (and_int is, [], _) => ubinary is TilWord64.andb
	  | (or_int is, [], _) => ubinary is TilWord64.orb
	  | (xor_int is, [], _) => ubinary is TilWord64.xorb

	  | (less_int is, [], _) => ipred is TilWord64.slt
	  | (greater_int is, [], _) => ipred is TilWord64.sgt
	  | (lesseq_int is, [], _) => ipred is TilWord64.slte
	  | (greatereq_int is, [], _) => ipred is TilWord64.sgte
	  | (eq_int is, [], _) => ipred is TilWord64.equal
	  | (neq_int is, [], _) => ipred is TilWord64.nequal

	  | (less_uint is, [], _) => upred is (TilWord64.ult)
	  | (greater_uint is, [], _) => upred is (TilWord64.ugt)
	  | (lesseq_uint is, [], _) => upred is (TilWord64.ulte)
	  | (greatereq_uint is, [], _) => upred is (TilWord64.ugte)

	  | (length_table _, _, _) => raise UNIMP
	  | (sub _,_,_)  => raise UNIMP
	  | (create_table _,_,_)  => raise UNIMP
	  | (create_empty_table _,_,_)  => raise UNIMP
	  | (update _, _, _) => raise UNIMP
	  | (equal_table _, _,_)  => raise UNIMP
					
	  | (mk_ref, _,_) => raise UNIMP
	  | (deref, _,_ ) => raise UNIMP
	  | (eq_ref, _,_) => raise UNIMP
	  | (setref, _,_) => raise UNIMP


	  | _ => bad "general"())
	end


end
