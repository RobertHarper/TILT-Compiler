(*$import PRIM Ppprim PRIMUTILPARAM Real64 PRIMUTIL *)

functor PrimUtil(structure PrimUtilParam : PRIMUTILPARAM)
    :> PRIMUTIL where type con = PrimUtilParam.con
		where type exp = PrimUtilParam.exp
		=

struct


    open Util  
    open PrimUtilParam Prim
    type con = con
    type exp = PrimUtilParam.exp
    type value = (con,exp) Prim.value
    val error = fn s => error "primutil.sml" s
    structure Float = Real64

    val con_string = con_vector(con_uint W8)
    fun value_type exp_typer scon : con = 
	(case scon of
	     (int (is,_)) => con_int is
	   | (uint (is,_)) => con_uint is
	   | (float (fs,_)) => con_float fs
	   | (array (c,_)) => con_array c
	   | (vector (c,_)) => con_vector c
	   | (refcell (ref e)) => con_ref (exp_typer e)
	   | (tag (_,c)) => con_tag c)

    fun get_aggregate_type (prim,aggregate,cons) = 
	let      
	    fun help (arg,res) = (false,[arg],res)
	    fun help' (args,res) = (false,args,res)
	    fun thelp (arg,res) = (true,[arg],res)
	    fun thelp' (args,res) = (true,args,res)

	    fun create_empty_array instance = thelp'([], con_array instance)
	    fun create_empty_vector instance = thelp'([], con_vector instance)
	    fun create_array instance = thelp'([con_uint W32, instance], con_array instance)
	    fun create_vector instance = thelp'([con_uint W32, instance], con_vector instance)
	    fun len_array instance = thelp(con_array instance, con_uint W32)
	    fun len_vector instance = thelp(con_vector instance, con_uint W32)
	    fun sub_array instance = help'([con_array instance, con_uint W32], instance)
	    fun sub_vector instance = thelp'([con_vector instance, con_uint W32], instance)
	    fun update_array instance =  help'([con_array instance, con_uint W32, instance], con_unit)
	    fun eq_array instance = help'([con_array instance, con_array instance],con_bool)
	    fun eq_vector instance = help(partial_arrow([instance, instance],con_bool),
					  partial_arrow([con_vector instance, 
							 con_vector instance],con_bool))
	    fun array2vector_array instance = thelp(con_array instance, con_vector instance)
	    fun vector2array_vector instance = thelp(con_vector instance, con_array instance)
	    fun do_array instance = 
		(case prim of
		     create_table _ => create_array instance
		   | create_empty_table _ => create_empty_array instance
		   | length_table _ => len_array instance
		   | sub _ => sub_array instance
		   | update _ => update_array instance
		   | equal_table _ => eq_array instance
		   | array2vector _ => array2vector_array instance
		   | vector2array _ => error "use array2vector"
		   | _ => error "pattern impossibility")
	    fun do_vector instance = 
		(case prim of
		     create_table _ => create_vector instance
		   | create_empty_table _ => create_empty_vector instance
		   | length_table _ => len_vector instance
		   | sub _ => sub_vector instance
		   | update _ => error "no vector update"
		   | equal_table _ => eq_vector instance
		   | array2vector _ => error "use vector2array"
		   | vector2array _ => vector2array_vector instance
		   | _ => error "pattern impossibility")
		     
	in  (case (aggregate,cons) of
		 (OtherArray  _, [instance]) => do_array instance
	       | (OtherVector _, [instance]) => do_vector instance
	       | (IntArray is, []) => do_array (con_uint is)
	       | (IntVector is, []) => do_vector (con_uint is)
	       | (FloatArray fs, []) => do_array (con_float fs)
	       | (FloatVector fs, []) => do_vector (con_float fs)
	       | _ => error "ill-formed table primitive")
	end


  fun get_type prim cons =
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
       | (uinta2uinta(is1,is2),[]) => help(con_array(con_uint is1), con_array(con_uint is2))
       | (uintv2uintv(is1,is2),[]) => help(con_vector(con_uint is1), con_vector(con_uint is2))


       | (plus_float fs,[]) => help'([con_float fs, con_float fs], con_float fs)
       | (minus_float fs,[]) =>  help'([con_float fs, con_float fs], con_float fs)
       | (mul_float fs,[]) =>  help'([con_float fs, con_float fs], con_float fs)
       | (div_float fs,[]) => help'([con_float fs, con_float fs], con_float fs)

       | (less_float fs,[]) => help'([con_float fs, con_float fs], con_bool)
       | (greater_float fs,[]) => help'([con_float fs, con_float fs], con_bool)
       | (lesseq_float fs,[]) => help'([con_float fs, con_float fs], con_bool)
       | (greatereq_float fs,[]) => help'([con_float fs, con_float fs], con_bool)
       | (eq_float fs,[]) => help'([con_float fs, con_float fs], con_bool)
       | (neq_float fs,[])  => help'([con_float fs, con_float fs], con_bool)

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

       | (less_int is,[])      => help'([con_int is, con_int is], con_bool)
       | (greater_int is,[])   => help'([con_int is, con_int is], con_bool)
       | (lesseq_int is,[])    => help'([con_int is, con_int is], con_bool)
       | (greatereq_int is,[]) => help'([con_int is, con_int is], con_bool)
       | (eq_int is,[])        => help'([con_int is, con_int is], con_bool)
       | (neq_int is,[])        => help'([con_int is, con_int is], con_bool)

       | (lshift_int is,[])  => help'([con_int is, con_int W32], con_int is)
       | (rshift_int is,[])  => help'([con_int is, con_int W32], con_int is)
       | (rshift_uint is,[]) => help'([con_uint is, con_int W32], con_uint is)
       | (and_int is,[])     => help'([con_int is, con_int is], con_int is)
       | (or_int is,[])      => help'([con_int is, con_int is], con_int is)
       | (xor_int is,[])     => help'([con_int is, con_int is], con_int is)

       | (less_uint is,[]) => help'([con_uint is, con_uint is], con_bool)
       | (greater_uint is,[]) => help'([con_uint is, con_uint is], con_bool)
       | (lesseq_uint is,[]) => help'([con_uint is, con_uint is], con_bool)
       | (greatereq_uint is,[]) => help'([con_uint is, con_uint is], con_bool)


       | (array2vector aggregate,cons) => get_aggregate_type(prim,aggregate,cons)
       | (vector2array aggregate,cons) => get_aggregate_type(prim,aggregate,cons)
       | (create_table aggregate,cons)  => get_aggregate_type(prim,aggregate,cons)
       | (create_empty_table aggregate,cons)  => get_aggregate_type(prim,aggregate,cons)
       | (length_table aggregate,cons)  => get_aggregate_type(prim,aggregate,cons)
       | (sub aggregate,cons)  => get_aggregate_type(prim,aggregate,cons)
       | (update aggregate,cons)  => get_aggregate_type(prim,aggregate,cons)
       | (equal_table aggregate,cons) => get_aggregate_type(prim,aggregate,cons)

	     
       | _ => (Ppprim.pp_prim prim;
	       error "can't get type"))

     end

  fun get_iltype ilprim cons =
      (case (ilprim,cons) of
	   (not_uint is,_) => (false,[con_uint is], con_uint is)
	 | (and_uint is,_) => (false,[con_uint is, con_uint is], con_uint is)
	 | (or_uint is,_) => (false,[con_uint is, con_uint is], con_uint is)
	 | (xor_uint is,_) => (false,[con_uint is, con_uint is], con_uint is)
	 | (lshift_uint is,_) => (false,[con_uint is, con_int W32], con_uint is)
	 | (eq_uint is,_) => (false, [con_uint is, con_uint is], con_bool)
	 | (neq_uint is,_) => (false, [con_uint is, con_uint is], con_bool)

	 | (mk_ref, [instance]) => (true,[instance],con_ref instance)
	 | (deref, [instance]) => (true,[con_ref instance], instance)
	 | (setref, [instance]) => (true,[con_ref instance,instance],con_unit)
	 | (eq_ref, [instance]) => (false, [con_ref instance, con_ref instance],con_bool))

	   
  fun get_type' prim args = 
      let val (total,incons,outcon) = get_type prim args
	  val arrow = if total then total_arrow else partial_arrow
      in  arrow(incons,outcon)
      end

  fun get_iltype' ilprim arg = 
      let val (total,incons,outcon) = get_iltype ilprim arg
	  val arrow = if total then total_arrow else partial_arrow
      in  arrow(incons,outcon)
      end

    fun apply prim cons vals = (* instance arg *)
	let 
	    fun bad s = (print "Error "; print s; print " while applying ";
			 Ppprim.pp_prim prim;
			 print "\n";
			 error "bad apply")
	    (* Some converters.  If the conversion is impossible, a type error has occurred *)
	    val exp2value = (fn e => (case (exp2value e) of
					  NONE => bad "exp2value"
					| SOME v => v))
	    fun value2float fs (float (fs',s)) = if (fs = fs')
						     then (case (Float.fromString s) of
							       NONE => bad "value2float"
							     | SOME f => f)
						 else bad "value2float"
	      | value2float _ _ = bad "value2float"
	    fun value2int is (int (is',w)) = if (is = is') then w else bad "value2int"
	      | value2int is (uint (is',w)) = if (is = is') then w else bad "value2int"
	      | value2int _ _ = bad "value2int"
	    fun value2int' is (int (is',w)) = if (is = is') then TilWord64.toInt w else bad "value2int'"
	      | value2int' is (uint (is',w)) = if (is = is') then TilWord64.toInt w else bad "value2int'"
	      | value2int' _ _ = bad "value2int'"
	    fun value2ref (refcell r) = r
	      | value2ref _ = bad "value2ref"
			     
	    val int2exp = value2exp o int
	    val float2exp = value2exp o float
	    val uint2exp = value2exp o uint

	    (* Some filters to perform Word canonicalization *)
	    fun filter is w =
		let val one = TilWord64.fromInt 1
		    val shift = (case is of
				     W8 => 8
				   | W16 => 16
				   | W32 => 32
				   | W64 => 64)
		in (is,TilWord64.andb(w,TilWord64.uminus(TilWord64.lshift(one,shift),one)))
		end

	    fun objbinary value2obj op2 = 
		(case vals of
		     [a,b] => let val obj1 = value2obj(exp2value a)
				  val obj2 = value2obj(exp2value b)
			      in value2exp(op2 (obj1,obj2))
			      end
		   | _ => bad "objbinary")
	    fun objbinary value2obj1 value2obj2 op2 = 
		(case vals of
		     [a,b] => let val obj1 = value2obj1(exp2value a)
				  val obj2 = value2obj2(exp2value b)
			      in value2exp(op2 (obj1,obj2))
			      end
		   | _ => bad "objbinary")
	    fun objunary value2obj op1 = 
		(case vals of
		     [a] => let val obj = value2obj(exp2value a)
			    in value2exp(op1 obj)
			    end
		   | _ => bad "objunary")
	    fun objpred value2obj op2 = 
		(case vals of
		     [a,b] => let val obj1 = value2obj(exp2value a)
				  val obj2 = value2obj(exp2value b)
			      in bool2exp(op2 (obj1,obj2))
			      end
		   | x => (print ("x has length " ^ (Int.toString (length x)) ^ "\n");
			   bad "objpred"))

	    fun ibinary is op2 = objbinary (value2int is) (value2int is) (int o (filter is) o op2)
	    fun iunary is op1 = objunary (value2int is) (int o (filter is) o op1)
	    fun fbinary fs op2 = objbinary (value2float fs) (value2float fs)
		                   ((fn f => (float(fs,Float.toString f))) o op2)
	    fun funary fs op1 = objunary (value2float fs)
		                   ((fn f => (float(fs,Float.toString f))) o op1)
	    fun isbinary is op2 = objbinary (value2int is) (value2int' is) (int o (filter is) o op2)

	    fun fpred fs pred = objpred (value2float fs) pred
	    fun ipred is pred = objpred (value2int is) pred

	in 
	    (case (prim,cons,vals) of
		 (soft_vtrap _,[],_) => unit_value
	       | (soft_ztrap _,[],_) => unit_value
	       | (hard_vtrap _,[],_) => unit_value
	       | (hard_ztrap _,[],_) => unit_value


	  | (float2int, [], _) => objunary (value2float F64)
	                                (fn f => (int(W32,TilWord64.fromInt(floor f))))
	  | (int2float, [], [v]) => objunary (value2int W32)
					(fn w => (float(F64,Float.toString(real(TilWord64.toInt w)))))
	  | (int2uint(is1,is2), [], [v]) => objunary (value2int is1) (fn w => uint(is2,w))
	  | (uint2int(is1,is2), [], [v]) => objunary (value2int is1) (fn w => int(is2,w))
	  | (uinta2uinta(is1,is2),_,_) => error "UNIMP"
	  | (uintv2uintv(is1,is2),_,_) => error "UNIMP"

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
      	  | (plus_uint is, [], _) => ibinary is TilWord64.uplus
      	  | (minus_uint is, [], _) => ibinary is TilWord64.uminus
      	  | (mul_uint is, [], _) => ibinary is TilWord64.umult
      	  | (div_uint is, [], _) => ibinary is TilWord64.udiv
      	  | (mod_uint is, [], _) => ibinary is TilWord64.umod
	  | (lshift_int is, [], _) => isbinary is TilWord64.lshift
	  | (rshift_int is, [], _) => isbinary is TilWord64.rshifta
	  | (rshift_uint is, [], _) => isbinary is TilWord64.rshiftl
	  | (neg_int is, [], _) => iunary is TilWord64.snegate
	  | (abs_int is, [], _) => iunary is TilWord64.absolute
	  | (not_int is, [], _) => iunary is TilWord64.notb
	  | (and_int is, [], _) => ibinary is TilWord64.andb
	  | (or_int is, [], _) => ibinary is TilWord64.orb
	  | (xor_int is, [], _) => ibinary is TilWord64.xorb
					
	  | (less_int is, [], _) => ipred is TilWord64.slt
	  | (greater_int is, [], _) => ipred is TilWord64.sgt
	  | (lesseq_int is, [], _) => ipred is TilWord64.slte
	  | (greatereq_int is, [], _) => ipred is TilWord64.sgte
	  | (eq_int is, [], _) => ipred is TilWord64.equal
	  | (neq_int is, [], _) => ipred is TilWord64.nequal
					
	  | (less_uint is, [], _) => ipred is (TilWord64.ult)
	  | (greater_uint is, [], _) => ipred is (TilWord64.ugt)
	  | (lesseq_uint is, [], _) => ipred is (TilWord64.ulte)
	  | (greatereq_uint is, [], _) => ipred is (TilWord64.ugte)
					
	  | (length_table _, [instance], _) => raise UNIMP
	  | (sub _,_,_)  => raise UNIMP
	  | (create_table _,_,_)  => raise UNIMP
	  | (create_empty_table _,_,_)  => raise UNIMP
	  | (update _, _, _) => raise UNIMP
	  | (equal_table _, _,_)  => raise UNIMP

	  | _ => bad "general"())
	end

  fun same_intsize (size1,size2) = 
    (case (size1,size2)
       of (Prim.W8,Prim.W8) => true
	| (Prim.W16,Prim.W16) => true
	| (Prim.W32,Prim.W32) => true
	| (Prim.W64,Prim.W64) => true
	| _ => false)

  fun same_floatsize (size1,size2) =
    (case (size1,size2)
       of (Prim.F32,Prim.F32) => true
	| (Prim.F64,Prim.F64) => true
	| _ => false)
    
(*
	  | (mk_ref, [c], [a]) => value2exp(refcell(ref a))
	  | (deref, [c], [a]) => !(value2ref(exp2value a))
	  | (eq_ref, [c], _) => objpred value2ref (op =)
	  | (setref, [c], [loc1,exp2]) => ((value2ref(exp2value(loc1))) := exp2; unit_value)

    fun applyil  vals = 
	let 
	in
	    (case (,cons,vals) of
	  | (PLUS_uint, [], _) => uibinary (Word32.+)
	  | (MINUS_uint, [], _) => uibinary (Word32.-)
	  | (MUL_uint, [], _) => uibinary (Word32.* )
	  | (DIV_uint, [], _) => uibinary (Word32.div)
	  | (MOD_uint, [], _) => uibinary (Word32.mod)
	  | (LSHIFT_uint, [], _) => uibinary (fn (w1,w2) => Word32.<<(w1,Word31.fromLargeWord w2))
	  | (RSHIFT_uint, [], _) => uibinary (fn (w1,w2) => Word32.>>(w1,Word31.fromLargeWord w2))
	  | (AND_uint, [], _) => uibinary (Word32.andb)
	   | (OR_uint, [], _) => uibinary (Word32.orb)

	end
*)


(*
  val skip = 16

    fun tt2int int_tt = 0
      | tt2int real_tt = 1
      | tt2int both_tt = 2

    fun int2tt 0 =  int_tt
      | int2tt 1 = real_tt
      | int2tt 2 = both_tt

    fun intsize2int W8 = 0
      | intsize2int W16 = 1
      | intsize2int W32 = 2
      | intsize2int W64 = 3

    fun int2intsize 0 = W8
      | int2intsize 1 = W16
      | int2intsize 2 = W32
      | int2intsize 3 = W64
	
    fun floatsize2int F32 = 0
      | floatsize2int F64 = 1

    fun int2floatsize 0 = F32
      | int2floatsize 1 = F64

    fun table2pair t = 
	case t of 
	    IntArray sz => (0, intsize2int sz)
	  | IntVector sz => (1, intsize2int sz)
	  | FloatArray sz => (2, floatsize2int sz)
	  | FloatVector sz => (3, floatsize2int sz)
	  | OtherArray false => (4, 0)
	  | OtherArray true => (4, 1)
	  | OtherVector false => (5, 0)
	  | OtherVector true => (5, 1)

    fun pair2table (0, i) = IntArray (int2intsize i)
      | pair2table (1, i) = IntVector (int2intsize i)
      | pair2table (2, i) = FloatArray (int2floatsize i)
      | pair2table (3, i) = FloatVector (int2floatsize i)
      | pair2table (4, 0) = OtherArray false 
      | pair2table (4, 1) = OtherArray true
      | pair2table (5, 0) = OtherVector false 
      | pair2table (5, 1) = OtherVector true


    fun prim2triple p =
	case p of
	    soft_vtrap tt => (0, tt2int tt, 0)
	  | soft_ztrap tt => (1, tt2int tt, 0)
	  | hard_vtrap tt => (2, tt2int tt, 0)
	  | hard_ztrap tt => (3, tt2int tt, 0)
	    
	  | float2int => (6, 0, 0)
	  | int2float => (7, 0, 0)
	  | int2uint (sz1, sz2) => (8, intsize2int sz1, intsize2int sz2)
	  | uint2int (sz1, sz2) => (9, intsize2int sz1, intsize2int sz2)
	  | int2int (sz1, sz2) => (10, intsize2int sz1, intsize2int sz2)
	  | uint2uint (sz1, sz2) => (11, intsize2int sz1, intsize2int sz2)
	  | uinta2uinta (sz1, sz2) => (12, intsize2int sz1, intsize2int sz2)
	  | uintv2uintv (sz1 , sz2) => (13, intsize2int sz1, intsize2int sz2)

	  | neg_float sz => (14, floatsize2int sz, 0)
	  | abs_float sz => (15, floatsize2int sz, 0)
	  | plus_float sz => (16, floatsize2int sz, 0)
	  | minus_float  sz => (17, floatsize2int sz, 0)
	  | mul_float sz => (18, floatsize2int sz, 0)
	  | div_float sz  => (19, floatsize2int sz, 0) 
	  | less_float sz => (20, floatsize2int sz, 0)
	  | greater_float sz => (21, floatsize2int sz, 0)
	  | lesseq_float sz => (22, floatsize2int sz, 0)
	  | greatereq_float sz => (23, floatsize2int sz, 0)
	  | eq_float sz => (24, floatsize2int sz, 0)
	  | neq_float sz => (25, floatsize2int sz, 0)

	  | plus_int sz => (26, intsize2int sz, 0)
	  | minus_int sz => (27, intsize2int sz, 0) 
	  | mul_int sz => (28, intsize2int sz, 0) 
	  | div_int sz => (29, intsize2int sz, 0) 
	  | mod_int sz => (30, intsize2int sz, 0) 
	  | quot_int sz => (31, intsize2int sz, 0) 
	  | rem_int sz => (32, intsize2int sz, 0) 
	  | plus_uint sz => (33, intsize2int sz, 0) 
	  | minus_uint sz => (34, intsize2int sz, 0) 
	  | mul_uint sz => (35, intsize2int sz, 0) 
	  | div_uint sz => (36, intsize2int sz, 0) 
	  | mod_uint sz => (37, intsize2int sz, 0) 
	  | less_int sz => (38, intsize2int sz, 0) 
	  | greater_int sz => (39, intsize2int sz, 0)
	  | lesseq_int sz => (40, intsize2int sz, 0) 
	  | greatereq_int sz => (41, intsize2int sz, 0) 
	  | less_uint sz => (42, intsize2int sz, 0) 
	  | greater_uint sz => (43, intsize2int sz, 0) 
	  | lesseq_uint sz => (44, intsize2int sz, 0) 
	  | greatereq_uint sz => (45, intsize2int sz, 0)
	  | eq_int sz => (46, intsize2int sz, 0) 
	  | neq_int sz => (47, intsize2int sz, 0)
	  | neg_int sz => (48, intsize2int sz, 0)
	  | abs_int sz => (49, intsize2int sz, 0)
		
	  (* bit-pattern manipulation *)
	  | not_int sz => (50, intsize2int sz, 0)
	  | and_int sz => (51, intsize2int sz, 0)
	  | or_int sz => (52, intsize2int sz, 0)
	  | xor_int sz => (53, intsize2int sz, 0)
	  | lshift_int sz => (54, intsize2int sz, 0)
	  | rshift_int sz => (55, intsize2int sz, 0)
	  | rshift_uint sz => (56, intsize2int sz, 0)
		
	  (* array and vector ops - bool = true indicates writeable *)
	  | array2vector t => let val (a,b) = table2pair t
				  in  (54, a, b)
				  end
	  | vector2array t =>  let val (a,b) = table2pair t
				  in  (55, a, b)
				  end
	  | create_table t =>  let val (a,b) = table2pair t
				  in  (56, a, b)
				  end
	  | create_empty_table t =>  let val (a,b) = table2pair t
				  in  (57, a, b)
				  end
	  | sub t =>  let val (a,b) = table2pair t
				  in  (58, a, b)
				  end
	  | update t =>  let val (a,b) = table2pair t
				  in  (59, a, b)
				  end
	  | length_table t =>  let val (a,b) = table2pair t
				  in  (60, a, b)
				  end
	  | equal_table t =>  let val (a,b) = table2pair t
				  in  (61, a, b)
				  end



*)

end
