(*$import Il ILCONTEXT ILUTIL PPIL Blaster NameBlast Word8 BinIO ILCONTEXTEQ Bool *)
(* Equality of contexts *)

functor IlContextEq (structure IlContext : ILCONTEXT
		     structure IlUtil : ILUTIL
		     structure Ppil : PPIL)
    :> ILCONTEXTEQ =
    struct

	val debug = ref false
	nonfix mod

	open Util
	open IlContext
	open Il

	val blast_debug = ref false
	fun error s = Util.error "IlContextEq" s

	fun foldand f [] = true
	  | foldand f (x::xs) = f x andalso foldand f xs

 
    local 
	open Blaster NameBlast 
	val indent = ref 0 
	fun push() = indent := (!indent) + 2
	fun pop() = indent := (!indent) - 2
	fun tab s = if (!blast_debug)
			then let fun loop 0 = print s
				   | loop n = (print " "; loop (n-1))
			     in  loop (!indent)
			     end
		    else ()
	fun say s = if (!blast_debug) then print s else ()

	fun blastOutChoice os i = if (!useOldBlast)
				      then blastOutInt os i
				  else BinIO.output1(os,Word8.fromInt i)
	fun blastInChoice is = if (!useOldBlast)
				   then blastInInt is
			       else Word8.toInt(case BinIO.input1 is of
						    NONE => error "blastInChoice failed"
						  | SOME w => w)
    in

	fun blastOutPath os (SIMPLE_PATH v) = (blastOutChoice os 0; blastOutVar os v)
	  | blastOutPath os (COMPOUND_PATH (v,ls)) = (blastOutChoice os 1; blastOutVar os v; 
						      blastOutList blastOutLabel os ls)
	fun blastInPath is = let val _ = tab "blastInPath:" 
				 val which = blastInChoice is
				 val v = blastInVar is
				 val res = if (which = 0)
					       then SIMPLE_PATH v
					   else COMPOUND_PATH(v, blastInList blastInLabel is)
				 val _ = (case res of
					      SIMPLE_PATH v => (say (Name.var2string v))
					    | COMPOUND_PATH (v,ls) => (say (Name.var2string v);
								       app (fn l => (say ".";
										     say (Name.label2string l)))
								       ls))
				 val _ = say "\n"
			     in res
			     end

	fun blastOutArrow os TOTAL = blastOutChoice os 0
	  | blastOutArrow os PARTIAL = blastOutChoice os 1
	fun blastInArrow is =
	    (case (blastInChoice is) of
		 0 => TOTAL
	       | 1 => PARTIAL
	       | _ => error "bad blastInArrow")

	fun blastOutIS os Prim.W8 = blastOutChoice os 0
	  | blastOutIS os Prim.W16 = blastOutChoice os 1
	  | blastOutIS os Prim.W32 = blastOutChoice os 2
	  | blastOutIS os Prim.W64 = blastOutChoice os 3
	fun blastInIS is =
	    (case blastInChoice is of
		 0 => Prim.W8
	       | 1 => Prim.W16
	       | 2 => Prim.W32
	       | 3 => Prim.W64
	       | _ => (error "bad blastInIS" handle e => raise e))
	fun blastOutFS os Prim.F32 = blastOutChoice os 0
	  | blastOutFS os Prim.F64 = blastOutChoice os 1
	fun blastInFS is =
	    (case blastInChoice is of
		 0 => Prim.F32
	       | 1 => Prim.F64
	       | _ => error "bad blastInFS")

	fun blastOutDec os dec = 
	    (case dec of
		 DEC_EXP (v,c) => (blastOutChoice os 0; blastOutVar os v; blastOutCon os c)
	       | DEC_CON (v,k,NONE) => (blastOutChoice os 1; blastOutVar os v; blastOutKind os k)
	       | DEC_CON (v,k,SOME c) => (blastOutChoice os 2; blastOutVar os v; blastOutKind os k; blastOutCon os c)
	       | DEC_MOD (v,s) => (blastOutChoice os 3; blastOutVar os v; blastOutSig os s)
	       | DEC_EXCEPTION (t,c) =>  (blastOutChoice os 4; blastOutTag os t; blastOutCon os c))
	and blastInDec is =
	    (case (blastInChoice is) of
		 0 => DEC_EXP (blastInVar is, blastInCon is)
	       | 1 => DEC_CON (blastInVar is, blastInKind is, NONE)
	       | 2 => DEC_CON (blastInVar is, blastInKind is, SOME (blastInCon is))
	       | 3 => DEC_MOD (blastInVar is, blastInSig is)
	       | 4 => DEC_EXCEPTION (blastInTag is, blastInCon is)
	       | _ => error "bad blastInDec")
	and blastOutBnd os bnd = 
	    (case bnd of
		 BND_EXP (v,e) => (blastOutChoice os 0; blastOutVar os v; blastOutExp os e)
	       | BND_CON (v,c) => (blastOutChoice os 1; blastOutVar os v; blastOutCon os c)
	       | BND_MOD (v,m) => (blastOutChoice os 2; blastOutVar os v; blastOutMod os m))
	and blastInBnd is =
	    (case (blastInChoice is) of
		 0 => BND_EXP(blastInVar is, blastInExp is)
	       | 1 => BND_CON(blastInVar is, blastInCon is)
	       | 2 => BND_MOD(blastInVar is, blastInMod is)
	       | _ => error "bad blastInBnd")
	and blastOutSdec os (SDEC(l,dec)) = (blastOutLabel os l; blastOutDec os dec)
	and blastInSdec is = SDEC(blastInLabel is, blastInDec is)
	and blastOutSbnd os (SBND(l,bnd)) = (blastOutLabel os l; blastOutBnd os bnd)
	and blastInSbnd is = SBND(blastInLabel is, blastInBnd is)

	and blastOutSdecs os sdecs = blastOutList blastOutSdec os sdecs
	and blastInSdecs is = blastInList blastInSdec is
	and blastOutSbnds os sbnds = blastOutList blastOutSbnd os sbnds
	and blastInSbnds is = blastInList blastInSbnd is

	and blastOutKind os k = 
	    (case k of
		KIND_TUPLE n => (blastOutChoice os 0; blastOutChoice os n)
	      | KIND_ARROW (m,n) => (blastOutChoice os 1; blastOutChoice os m;  blastOutChoice os n)
	      | KIND_INLINE (k,c) => (blastOutChoice os 2; blastOutKind os k; blastOutCon os c))
		    
	and blastInKind is = 
	    (case blastInChoice is of
		0 => KIND_TUPLE(blastInChoice is)
	      | 1 => KIND_ARROW(blastInChoice is, blastInChoice is)
	      | 2 => KIND_INLINE(blastInKind is, blastInCon is)
	      | _ => error "bad blastInKind")

	and blastOutCon os c = 
	    (case c of
		 CON_VAR v => (blastOutChoice os 0; blastOutVar os v)
	       | CON_TYVAR tv => (case Tyvar.tyvar_deref tv of
				      SOME c => blastOutCon os c
				    | NONE => error "cannot blastOut unresolved CON_TYVAR")
	       | CON_OVAR oc => blastOutCon os (CON_TYVAR (Tyvar.ocon_deref oc))
	       | CON_FLEXRECORD (ref (INDIRECT_FLEXINFO r)) => blastOutCon os (CON_FLEXRECORD r)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, true, lclist))) => blastOutCon os (CON_RECORD lclist)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, false, _))) => error "cannot blastOut flex record type"
	       | CON_INT is => (blastOutChoice os 1; blastOutIS os is)
	       | CON_UINT is => (blastOutChoice os 2; blastOutIS os is)
	       | CON_FLOAT fs => (blastOutChoice os 3; blastOutFS os fs)
	       | CON_ARRAY c => (blastOutChoice os 4; blastOutCon os c)
	       | CON_VECTOR c => (blastOutChoice os 5; blastOutCon os c)
	       | CON_ANY => (blastOutChoice os 6)
	       | CON_REF c => (blastOutChoice os 7; blastOutCon os c)
	       | CON_TAG c => (blastOutChoice os 8; blastOutCon os c)
	       | CON_ARROW (cs,c,f,oa) => (blastOutChoice os 9; blastOutList blastOutCon os cs;
					   blastOutCon os c; blastOutBool os f;
					   blastOutArrow os (case (oneshot_deref oa) of
								 SOME a => a
							       | _ => error "unresolved CON_ARROW"))
	       | CON_APP (c1,c2) => (blastOutChoice os 10; blastOutCon os c1; blastOutCon os c2)
	       | CON_MU c => (blastOutChoice os 11; blastOutCon os c)
	       | CON_RECORD lclist => (blastOutChoice os 12; blastOutList (blastOutPair blastOutLabel blastOutCon) os lclist)
	       | CON_FUN (vlist, c) => (blastOutChoice os 13; blastOutList blastOutVar os vlist; blastOutCon os c)
	       | CON_SUM {noncarriers, carrier, special = NONE} => 
		     (blastOutChoice os 14; blastOutChoice os noncarriers; 
		      blastOutCon os carrier)
	       | CON_SUM {noncarriers, carrier, special = SOME i} => 
		     (blastOutChoice os 15; blastOutChoice os noncarriers; 
		      blastOutCon os carrier; blastOutChoice os i)
	       | CON_TUPLE_INJECT clist => (blastOutChoice os 16; blastOutList blastOutCon os clist)
	       | CON_TUPLE_PROJECT (i,c) => (blastOutChoice os 17; blastOutChoice os i; blastOutCon os c)
	       | CON_MODULE_PROJECT (m,l) => (blastOutChoice os 18; blastOutMod os m; blastOutLabel os l))

        and blastInCon is = 
	    let val _ = push()
		val _ = tab "blastInCon\n"
		val res = blastInCon' is
		val _ = pop()
	    in  res
	    end

	and blastInCon' is = 
	    (case (blastInChoice is) of
		 0 => CON_VAR (blastInVar is)
	       | 1 => CON_INT (blastInIS is)
	       | 2 => CON_UINT (blastInIS is)
	       | 3 => CON_FLOAT (blastInFS is)
	       | 4 => CON_ARRAY (blastInCon is)
	       | 5 => CON_VECTOR (blastInCon is)
	       | 6 => CON_ANY
	       | 7 => CON_REF (blastInCon is)
	       | 8 => CON_TAG (blastInCon is)
	       | 9 => let val _ = tab "  ARROW case\n"
			  val cs = blastInList blastInCon is
			  val _ = tab "  ARROW done cs\n"
			  val c = blastInCon is
			  val _ = tab " ARROW done c\n"
			  val f = blastInBool is
			  val _ = tab "  ARROW done f\n"
			  val a = oneshot_init (blastInArrow is)
			  val _ = tab "  ARROW done a\n"
		      in 
			  CON_ARROW (cs,c,f,a) (* (blastInList blastInCon is,
				     blastInCon is, blastInBool is,
				     oneshot_init (blastInArrow is))) *)
		      end
	       | 10 => CON_APP (blastInCon is, blastInCon is)
	       | 11 => CON_MU (blastInCon is)
	       | 12 => CON_RECORD(blastInList (blastInPair blastInLabel blastInCon) is)
	       | 13 => CON_FUN (blastInList blastInVar is, blastInCon is)
	       | 14 => CON_SUM {noncarriers = blastInChoice is,
				carrier = blastInCon is,
				special = NONE} 
	       | 15 => CON_SUM {noncarriers = blastInChoice is,
				carrier = blastInCon is,
				special = SOME (blastInChoice is)}
	       | 16 => CON_TUPLE_INJECT (blastInList blastInCon is)
	       | 17 => CON_TUPLE_PROJECT (blastInChoice is, blastInCon is)
	       | 18 => CON_MODULE_PROJECT (blastInMod is, blastInLabel is)
	       | _ => error "bad blastInCon")

	and blastOutValue os v = 
	    (case v of
		 (Prim.int (is,w64)) => (blastOutChoice os 0; blastOutIS os is; blastOutWord64 os w64)
	       | (Prim.uint (is,w64)) => (blastOutChoice os 1; blastOutIS os is; blastOutWord64 os w64)
	       | (Prim.float (fs,str)) => (blastOutChoice os 2; blastOutFS os fs; blastOutString os str)
	       | (Prim.tag (t,c)) => (blastOutChoice os 3; blastOutTag os t; blastOutCon os c)
	       | _ => error "blasting of array/vector/refcell not supported")

	and blastInValue is =
	    (case (blastInChoice is) of
		 0 => Prim.int (blastInIS is, blastInWord64 is)
	       | 1 => Prim.uint (blastInIS is, blastInWord64 is)
	       | 2 => Prim.float (blastInFS is, blastInString is)
	       | 3 => Prim.tag (blastInTag is, blastInCon is)
	       | _ => error "bad blastInValue")

	and blastOutIlPrim os ilprim = 
	    let open Prim
	    in  (case ilprim of
		     eq_uint is => (blastOutChoice os 0; blastOutIS os is)
		   | neq_uint is => (blastOutChoice os 1; blastOutIS os is)
		   | not_uint is => (blastOutChoice os 2; blastOutIS os is)
		   | and_uint is => (blastOutChoice os 3; blastOutIS os is)
		   | or_uint is => (blastOutChoice os 4; blastOutIS os is)
		   | lshift_uint is => (blastOutChoice os 5; blastOutIS os is))
	    end

	and blastInIlPrim is = 
	    let open Prim
	    in  (case (blastInChoice is) of
		     0 => eq_uint(blastInIS is)
		   | 1 => neq_uint(blastInIS is)
		   | 2 => not_uint(blastInIS is)
		   | 3 => and_uint(blastInIS is)
		   | 4 => or_uint(blastInIS is)
		   | 5 => lshift_uint(blastInIS is)
		   | _ => error "bad blastInIlPrim")
	    end


	and blastOutTT os tt =
	    let open Prim 
	    in  (case tt of
		     int_tt => blastOutChoice os 0
		   | real_tt => blastOutChoice os 1
		   | both_tt => blastOutChoice os 2)
	    end


	and blastInTT is =
	    let open Prim 
	    in  (case (blastInChoice is) of
		     0 => int_tt
		   | 1 => real_tt
		   | 2 => both_tt
		   | _ => error "bad blastInTT")
	    end
			 
	and blastOutTable os table = 
	    let open Prim 
	    in  (case table of
		     IntArray is => (blastOutChoice os 0; blastOutIS os is)
		   | IntVector is => (blastOutChoice os 1; blastOutIS os is)
		   | FloatArray fs => (blastOutChoice os 2; blastOutFS os fs)
		   | FloatVector fs => (blastOutChoice os 3; blastOutFS os fs)
		   | PtrArray => (blastOutChoice os 4)
		   | PtrVector => (blastOutChoice os 5)
		   | WordArray => (blastOutChoice os 6)
		   | WordVector => (blastOutChoice os 7))
	    end

	and blastInTable is =
	    let open Prim 
	    in  (case (blastInChoice is) of
		     0 => IntArray (blastInIS is)
		   | 1 => IntVector (blastInIS is)
		   | 2 => FloatArray (blastInFS is)
		   | 3 => FloatVector (blastInFS is)
		   | 4 => PtrArray
		   | 5 => PtrVector
		   | 6 => WordArray
		   | 7 => WordVector
		   | _ => error "bad blastInTable")
	    end

	and blastOutPrim os prim = 
	    let open Prim
	    in  (case prim of
		     soft_vtrap tt => (blastOutChoice os 0; blastOutTT os tt)
		   | soft_ztrap tt => (blastOutChoice os 1; blastOutTT os tt)
		   | hard_vtrap tt => (blastOutChoice os 2; blastOutTT os tt)
		   | hard_ztrap tt => (blastOutChoice os 3; blastOutTT os tt)
			 
		   | mk_ref => (blastOutChoice os 4)
		   | deref => (blastOutChoice os 5)

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | float2int (* floor *) => (blastOutChoice os 6)
		   | int2float (* real  *) => (blastOutChoice os 7)
		   | int2uint (is1,is2) => (blastOutChoice os 8; blastOutIS os is1; blastOutIS os is2)
		   | uint2int (is1,is2) => (blastOutChoice os 9; blastOutIS os is1; blastOutIS os is2)
		   | uinta2uinta (is1,is2) => (blastOutChoice os 10; blastOutIS os is1; blastOutIS os is2)
		   | uintv2uintv (is1,is2) => (blastOutChoice os 11; blastOutIS os is1; blastOutIS os is2)

		   (* ref operation *)
		   | eq_ref => (blastOutChoice os 12)
		   | setref => (blastOutChoice os 13)

		   (* floatint-point operations *)	
		   | neg_float fs  => (blastOutChoice os 14; blastOutFS os fs)
		   | abs_float fs  => (blastOutChoice os 15; blastOutFS os fs)
		   | plus_float fs  => (blastOutChoice os 16; blastOutFS os fs)
		   | minus_float fs  => (blastOutChoice os 17; blastOutFS os fs)
		   | mul_float fs  => (blastOutChoice os 18; blastOutFS os fs)
		   | div_float fs  => (blastOutChoice os 19; blastOutFS os fs)
		   | less_float fs  => (blastOutChoice os 20; blastOutFS os fs)
		   | greater_float fs  => (blastOutChoice os 21; blastOutFS os fs)
		   | lesseq_float fs  => (blastOutChoice os 22; blastOutFS os fs)
		   | greatereq_float  fs  => (blastOutChoice os 23; blastOutFS os fs)
		   | eq_float  fs  => (blastOutChoice os 24; blastOutFS os fs)
		   | neq_float fs  => (blastOutChoice os 25; blastOutFS os fs)

		   (* int operations *)
		   | plus_int is  => (blastOutChoice os 26; blastOutIS os is)
		   | minus_int is  => (blastOutChoice os 27; blastOutIS os is)
		   | mul_int is  => (blastOutChoice os 28; blastOutIS os is)
		   | div_int is  => (blastOutChoice os 29; blastOutIS os is)
		   | mod_int is  => (blastOutChoice os 30; blastOutIS os is)
		   | quot_int is  => (blastOutChoice os 31; blastOutIS os is)
		   | rem_int is  => (blastOutChoice os 32; blastOutIS os is)
		   | plus_uint is  => (blastOutChoice os 33; blastOutIS os is)
		   | minus_uint is  => (blastOutChoice os 34; blastOutIS os is)
		   | mul_uint is  => (blastOutChoice os 35; blastOutIS os is)
		   | div_uint is  => (blastOutChoice os 36; blastOutIS os is)
		   | mod_uint is  => (blastOutChoice os 37; blastOutIS os is)
		   | less_int is  => (blastOutChoice os 38; blastOutIS os is)
		   | greater_int is  => (blastOutChoice os 39; blastOutIS os is)
		   | lesseq_int is  => (blastOutChoice os 40; blastOutIS os is)
		   | greatereq_int is  => (blastOutChoice os 41; blastOutIS os is)
		   | less_uint is  => (blastOutChoice os 42; blastOutIS os is)
		   | greater_uint is  => (blastOutChoice os 43; blastOutIS os is)
		   | lesseq_uint is  => (blastOutChoice os 44; blastOutIS os is)
		   | greatereq_uint is  => (blastOutChoice os 45; blastOutIS os is)
		   | eq_int is  => (blastOutChoice os 46; blastOutIS os is)
		   | neq_int is  => (blastOutChoice os 47; blastOutIS os is)
		   | neg_int is  => (blastOutChoice os 48; blastOutIS os is)
		   | abs_int is  => (blastOutChoice os 49; blastOutIS os is)

		   (* bit-pattern manipulation *)
		   | not_int is  => (blastOutChoice os 50; blastOutIS os is)
		   | and_int is  => (blastOutChoice os 51; blastOutIS os is)
		   | or_int is  => (blastOutChoice os 52; blastOutIS os is)
		   | lshift_int is  => (blastOutChoice os 53; blastOutIS os is)
		   | rshift_int is  => (blastOutChoice os 54; blastOutIS os is)
		   | rshift_uint is  => (blastOutChoice os 55; blastOutIS os is)
			 
		   (* array and vectors *)
		   | array2vector t => (blastOutChoice os 56; blastOutTable os t)
		   | create_table t => (blastOutChoice os 57; blastOutTable os t)
		   | sub t => (blastOutChoice os 58; blastOutTable os t)
		   | update t => (blastOutChoice os 59; blastOutTable os t)
		   | length_table t => (blastOutChoice os 60; blastOutTable os t)
		   | equal_table t => (blastOutChoice os 61; blastOutTable os t)

		   (* IO operations *)
		   | open_in => (blastOutChoice os 62)
		   | input => (blastOutChoice os 63)
		   | input1 => (blastOutChoice os 64)
		   | lookahead => (blastOutChoice os 65)
		   | open_out => (blastOutChoice os 66)
		   | close_in => (blastOutChoice os 67)
		   | output => (blastOutChoice os 68)
		   | flush_out => (blastOutChoice os 69)
		   | close_out => (blastOutChoice os 70)
		   | end_of_stream => (blastOutChoice os 71))

	    end

	and blastInPrim is =
	    let open Prim
	    in  (case (blastInChoice is) of
		     0 => soft_vtrap(blastInTT is)
		   | 1 => soft_ztrap(blastInTT is)
		   | 2 => hard_vtrap(blastInTT is)
		   | 3 => hard_ztrap(blastInTT is)
			 
		   | 4 => mk_ref
		   | 5 => deref

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | 6 => float2int (* floor *)
		   | 7 => int2float (* real  *)
		   | 8 => int2uint (blastInIS is, blastInIS is)
		   | 9 => uint2int(blastInIS is, blastInIS is)
		   | 10 => uinta2uinta(blastInIS is, blastInIS is)
		   | 11 => uintv2uintv(blastInIS is, blastInIS is)

		   (* ref operation *)
		   | 12 => eq_ref
		   | 13 => setref

		   (* floatint-point operations *)	
		   | 14 => neg_float(blastInFS is)
		   | 15 => abs_float(blastInFS is)
		   | 16 => plus_float(blastInFS is)
		   | 17 => minus_float(blastInFS is)
		   | 18 => mul_float(blastInFS is)
		   | 19 => div_float(blastInFS is)
		   | 20 => less_float(blastInFS is)
		   | 21 => greater_float(blastInFS is)
		   | 22 => lesseq_float(blastInFS is)
		   | 23 => greatereq_float (blastInFS is)
		   | 24 => eq_float (blastInFS is)
		   | 25 => neq_float(blastInFS is)

		   (* int operations *)
		   | 26 => plus_int(blastInIS is)
		   | 27 => minus_int(blastInIS is)
		   | 28 => mul_int(blastInIS is)
		   | 29 => div_int(blastInIS is)
		   | 30 => mod_int(blastInIS is)
		   | 31 => quot_int(blastInIS is)
		   | 32 => rem_int(blastInIS is)
		   | 33 => plus_uint(blastInIS is)
		   | 34 => minus_uint(blastInIS is)
		   | 35 => mul_uint(blastInIS is)
		   | 36 => div_uint(blastInIS is)
		   | 37 => mod_uint(blastInIS is)
		   | 38 => less_int(blastInIS is)
		   | 39 => greater_int(blastInIS is)
		   | 40 => lesseq_int(blastInIS is)
		   | 41 => greatereq_int(blastInIS is)
		   | 42 => less_uint(blastInIS is)
		   | 43 => greater_uint(blastInIS is)
		   | 44 => lesseq_uint(blastInIS is)
		   | 45 => greatereq_uint(blastInIS is)
		   | 46 => eq_int(blastInIS is)
		   | 47 => neq_int(blastInIS is)
		   | 48 => neg_int(blastInIS is)
		   | 49 => abs_int(blastInIS is)

		   (* bit-pattern manipulation *)
		   | 50 => not_int(blastInIS is)
		   | 51 => and_int(blastInIS is)
		   | 52 => or_int(blastInIS is)
		   | 53 => lshift_int(blastInIS is)
		   | 54 => rshift_int(blastInIS is)
		   | 55 => rshift_uint(blastInIS is)
			 
		   (* array and vectors *)
		   | 56 => array2vector (blastInTable is)
		   | 57 => create_table (blastInTable is)
		   | 58 => sub (blastInTable is)
		   | 59 => update (blastInTable is)
		   | 60 => length_table (blastInTable is)
		   | 61 => equal_table (blastInTable is)

		   (* IO operations *)
		   | 62 => open_in
		   | 63 => input
		   | 64 => input1
		   | 65 => lookahead
		   | 66 => open_out
		   | 67 => close_in
		   | 68 => output
		   | 69 => flush_out
		   | 70 => close_out
		   | 71 => end_of_stream
		   | _ => error "bad blastInPrim")

	    end

	and blastOutExp os exp = 
	    (case exp of
		 OVEREXP (_,_,oe) => (case oneshot_deref oe of
					  SOME e => blastOutExp os e
					| NONE => error "cannot blastOut unresolved OVEREXP")
	       | SCON v => (blastOutChoice os 0; blastOutValue os v)
	       | PRIM (p,clist,elist) => (blastOutChoice os 1; blastOutPrim os p;
					  blastOutList blastOutCon os clist;
					  blastOutList blastOutExp os elist)
	       | ILPRIM (p,clist,elist) => (blastOutChoice os 2; blastOutIlPrim os p;
					    blastOutList blastOutCon os clist;
					    blastOutList blastOutExp os elist)
	       | ETAPRIM (p,clist) => (blastOutChoice os 3; blastOutPrim os p;
				       blastOutList blastOutCon os clist)
	       | ETAILPRIM (p,clist) => (blastOutChoice os 4; blastOutIlPrim os p;
					 blastOutList blastOutCon os clist)
	       | VAR v => (blastOutChoice os 5; blastOutVar os v)
	       | APP (e,elist) => (blastOutChoice os 6; blastOutExp os e; blastOutList blastOutExp os elist)
	       | FIX (b,a,fbnds) => (blastOutChoice os 7; blastOutBool os b; blastOutArrow os a;
				     blastOutList blastOutFbnd os fbnds)
	       | RECORD lelist => (blastOutChoice os 8; blastOutList (blastOutPair blastOutLabel blastOutExp) os lelist)
	       | RECORD_PROJECT (e,l,c) => (blastOutChoice os 9; blastOutExp os e; blastOutLabel os l; blastOutCon os c)
	       | SUM_TAIL (c,e) => (blastOutChoice os 10; blastOutCon os c; blastOutExp os e)
	       | HANDLE (e1,e2) => (blastOutChoice os 11; blastOutExp os e1; blastOutExp os e2)
	       | RAISE (c,e) => (blastOutChoice os 12; blastOutCon os c; blastOutExp os e)
	       | LET(bnds,e) => (blastOutChoice os 13; blastOutList blastOutBnd os bnds; blastOutExp os e)
	       | NEW_STAMP c => (blastOutChoice os 14; blastOutCon os c)
	       | EXN_INJECT (str,e1,e2) => (blastOutChoice os 15; blastOutString os str; blastOutExp os e1; blastOutExp os e2)
	       | ROLL (c,e) => (blastOutChoice os 16; blastOutCon os c; blastOutExp os e)
	       | UNROLL (c1,c2,e) => (blastOutChoice os 17; blastOutCon os c1; blastOutCon os c2;
				      blastOutExp os e)
	       | INJ {sumtype, field, inject} =>
		     (blastOutChoice os 18; blastOutCon os sumtype; 
		      blastOutChoice os field;
		      blastOutOption blastOutExp os inject)
	       | CASE {sumtype, arg, bound, arms, tipe, default} => 
		      (blastOutChoice os 19;
		       blastOutCon os sumtype;
		       blastOutExp os arg;
		       blastOutVar os bound;
		       blastOutList (blastOutOption blastOutExp) os arms;
		       blastOutCon os tipe;
		       blastOutOption blastOutExp os default)
	       | EXN_CASE {arg, arms, default, tipe} => 
		      (blastOutChoice os 20;
		       blastOutExp os arg;
		       blastOutList (blastOutTriple blastOutExp blastOutCon blastOutExp) os arms;
		       blastOutOption blastOutExp os default;
		       blastOutCon os tipe)
	       | MODULE_PROJECT (m,l) => (blastOutChoice os 21; blastOutMod os m; blastOutLabel os l)
	       | SEAL(e,c) => (blastOutChoice os 22; blastOutExp os e; blastOutCon os c))

        and blastInExp is = 
	    let val _ = push()
		val _ = tab "blastInExp\n" 
		val res = blastInExp' is
		val _ = pop()
	    in  res
	    end

	and blastInExp' is =
	     (case (blastInChoice is) of
	         0 => (tab "  SCON\n"; 
		       SCON(blastInValue is))
	       | 1 => (tab "  PRIM\n"; 
		       PRIM (blastInPrim is,
			    blastInList blastInCon is,
			    blastInList blastInExp is))
	       | 2 => ILPRIM (blastInIlPrim is,
			      blastInList blastInCon is,
			      blastInList blastInExp is)
	       | 3 => ETAPRIM (blastInPrim is,
			       blastInList blastInCon is)
	       | 4 => ETAILPRIM (blastInIlPrim is,
				 blastInList blastInCon is)
	       | 5 => let val _ = tab "  VAR"
			  val v = blastInVar is
			  val _ = (say (Name.var2string v); say "\n")
		      in  VAR v
		      end
	       | 6 => APP (blastInExp is, blastInList blastInExp is)
	       | 7 => FIX (blastInBool is, blastInArrow is, blastInList blastInFbnd is)
	       | 8 => RECORD (blastInList (blastInPair blastInLabel blastInExp) is)
	       | 9 => RECORD_PROJECT (blastInExp is, blastInLabel is, blastInCon is)
	       | 10 => SUM_TAIL (blastInCon is, blastInExp is)
	       | 11 => HANDLE (blastInExp is, blastInExp is)
	       | 12 => RAISE (blastInCon is, blastInExp is)
	       | 13 => LET(blastInList blastInBnd is, blastInExp is)
	       | 14 => NEW_STAMP (blastInCon is)
	       | 15 => EXN_INJECT (blastInString is, blastInExp is, blastInExp is)
	       | 16 => ROLL (blastInCon is, blastInExp is)
	       | 17 => UNROLL (blastInCon is, blastInCon is, blastInExp is)
	       | 18 => let val sumtype = blastInCon is
			   val field = blastInChoice is
			   val inject = blastInOption blastInExp is
		       in  INJ {sumtype = sumtype,
				field = field,
				inject = inject}
		       end
	       | 19 => (tab "  CASE\n";
			CASE {sumtype = blastInCon is,
			      arg = blastInExp is, 
			      bound = blastInVar is,
			      arms = blastInList (blastInOption blastInExp) is,
			      tipe = blastInCon is, 
			      default = blastInOption blastInExp is})
	       | 20 => (tab "  EXN_CASE\n";
			EXN_CASE {arg = blastInExp is, 
				 arms = blastInList (blastInTriple blastInExp blastInCon blastInExp) is,
				 default = blastInOption blastInExp is, 
				 tipe = blastInCon is})
	       | 21 => MODULE_PROJECT (blastInMod is, blastInLabel is)
	       | 22 => SEAL(blastInExp is, blastInCon is)
	       | _ => error "bad blastInExp")

	and blastOutFbnd os (FBND(v1,v2,c1,c2,e)) = (blastOutVar os v1;
						     blastOutVar os v2;
						     blastOutCon os c1;
						     blastOutCon os c2;
						     blastOutExp os e)
	and blastInFbnd is = let val _ = tab "blastInFbnd"
				 val v = blastInVar is
				 val _ = (say (Name.var2string v); say "\n")    
				 val _ = push()
				 val res = FBND(v, blastInVar is,
						blastInCon is, blastInCon is, blastInExp is)
				 val _ = pop()
			     in  res
			     end
				  

	and blastOutMod os m = 
	    (case m of
		 MOD_VAR v => (blastOutChoice os 0; blastOutVar os v)
	       | MOD_STRUCTURE sbnds => (blastOutChoice os 1; blastOutSbnds os sbnds)
	       | MOD_FUNCTOR (v,s,m) => (blastOutChoice os 2; blastOutVar os v; blastOutSig os s; blastOutMod os m)
	       | MOD_APP (m1,m2) => (blastOutChoice os 3; blastOutMod os m1; blastOutMod os m2)
	       | MOD_PROJECT (m,l) => (blastOutChoice os 4; blastOutMod os m; blastOutLabel os l)
	       | MOD_SEAL (m,s) => (blastOutChoice os 5; blastOutMod os m; blastOutSig os s)
	       | MOD_LET (v,m1,m2) => (blastOutChoice os 6; blastOutVar os v; blastOutMod os m1; blastOutMod os m2))

        and blastInMod is = 
	    let val _ = push()
		val _ = tab "blastInMod\n"
		val res = blastInMod' is
		val _ = pop()
	    in  res
	    end

	and blastInMod' is =
	    (
		     case (blastInChoice is) of
		 0 => MOD_VAR(blastInVar is)
	       | 1 => MOD_STRUCTURE(blastInSbnds is)
	       | 2 => MOD_FUNCTOR(blastInVar is, blastInSig is, blastInMod is)
	       | 3 => MOD_APP (blastInMod is, blastInMod is)
	       | 4 => MOD_PROJECT (blastInMod is, blastInLabel is)
	       | 5 => MOD_SEAL (blastInMod is, blastInSig is)
	       | 6 => MOD_LET (blastInVar is, blastInMod is, blastInMod is)
	       | _ => error "bad blastInMod")

	and blastOutSig os s = 
		 (tab "    blastInSig\n"; 
		  case s of
		 SIGNAT_STRUCTURE (NONE, sdecs) => (blastOutChoice os 0; blastOutSdecs os sdecs)
	       | SIGNAT_STRUCTURE (SOME p, sdecs) => (blastOutChoice os 1; blastOutPath os p; blastOutSdecs os sdecs)
	       | SIGNAT_FUNCTOR(v, s1, s2, arrow) => (blastOutChoice os 2; blastOutVar os v;
						      blastOutSig os s1; blastOutSig os s2; blastOutArrow os arrow)
	       | SIGNAT_INLINE_STRUCTURE {self=NONE,code,abs_sig} => 
		     (blastOutChoice os 3; blastOutSbnds os code; blastOutSdecs os abs_sig)
	       | SIGNAT_INLINE_STRUCTURE {self=SOME p,code,abs_sig} => 
		     (blastOutChoice os 4; blastOutPath os p;
		      blastOutSbnds os code; blastOutSdecs os abs_sig)
	       | SIGNAT_VAR v => (blastOutChoice os 5; blastOutVar os v)
	       | SIGNAT_OF m => (blastOutChoice os 6; blastOutMod os m))

	and blastInSig is =
	    (case (blastInChoice is) of
		 0 => SIGNAT_STRUCTURE (NONE, blastInSdecs is)
	       | 1 => SIGNAT_STRUCTURE (SOME (blastInPath is), blastInSdecs is)
	       | 2 => SIGNAT_FUNCTOR(blastInVar is, blastInSig is, blastInSig is, blastInArrow is)
	       | 3 => SIGNAT_INLINE_STRUCTURE {self=NONE, code = blastInSbnds is, 
					       abs_sig = blastInSdecs is}
	       | 4 => SIGNAT_INLINE_STRUCTURE {self=SOME(blastInPath is), code = blastInSbnds is, 
					       abs_sig = blastInSdecs is}
	       | 5 => SIGNAT_VAR(blastInVar is)
	       | 6 => SIGNAT_OF(blastInMod is)
	       | _ => error "bad blastInSig")
				     
	fun blastOutPC os pc = 
	    case pc of
		PHRASE_CLASS_EXP (e,c) => (blastOutChoice os 0; blastOutExp os e; blastOutCon os c)
	      | PHRASE_CLASS_CON (c,k) => (blastOutChoice os 1; blastOutCon os c; blastOutKind os k)
	      | PHRASE_CLASS_MOD (m,s) => (blastOutChoice os 2; blastOutMod os m; blastOutSig os s)
	      | PHRASE_CLASS_SIG (v,s) => (blastOutChoice os 3; blastOutVar os v; blastOutSig os s)
	      | PHRASE_CLASS_OVEREXP celist => (blastOutChoice os 4; 
						blastOutList (blastOutPair blastOutCon blastOutExp) os celist)

	fun blastInPC is = 
	    (tab "  blastInPC\n"; 
	    case (blastInChoice is) of
		0 => PHRASE_CLASS_EXP(blastInExp is, blastInCon is)
	      | 1 => PHRASE_CLASS_CON(blastInCon is, blastInKind is)
	      | 2 => PHRASE_CLASS_MOD(blastInMod is, blastInSig is)
	      | 3 => PHRASE_CLASS_SIG(blastInVar is, blastInSig is)
	      | 4 => PHRASE_CLASS_OVEREXP(blastInList (blastInPair blastInCon blastInExp) is)
	      | _ => error "bad blastInPC")

	fun blastOutFixity os Fixity.NONfix = blastOutChoice os 0
	  | blastOutFixity os (Fixity.INfix (m,n)) = (blastOutChoice os 1; 
						      blastOutChoice os m; blastOutChoice os n)
	fun blastInFixity is =
	    if (blastInChoice is = 0) 
		then Fixity.NONfix 
	    else let val m = blastInChoice is
		     val n = blastInChoice is
		 in  Fixity.INfix (m,n)
		 end

	fun blastOutFixityTable os ft = 
	    blastOutList (blastOutPair blastOutLabel blastOutFixity) os ft
	fun blastInFixityTable is = 
	    blastInList (blastInPair blastInLabel blastInFixity) is 


	fun blastOutLabelList os label_list = 
	    blastOutLabelmap os (blastOutPair blastOutPath blastOutPC) label_list
	fun blastInLabelList is = 
	    (tab "blastLabelList\n"; 
	    blastInLabelmap is (blastInPair blastInPath blastInPC) )

	fun blastOutVarList os (vmap,vlist) = 
	    (blastOutVarmap os (blastOutPair blastOutLabel blastOutPC) vmap;
	     blastOutList blastOutVar os vlist)

	fun blastInVarList is = 
	    let val _ = tab "blastInVarList\n";
		val vmap = blastInVarmap is (blastInPair blastInLabel blastInPC) 
		val vlist = blastInList blastInVar is
	    in  (vmap, vlist)
	    end

	fun blastOutTagList os tag_list = blastOutTagmap os blastOutCon tag_list
	fun blastInTagList is = blastInTagmap is blastInCon 

    fun blastOutContext os (CONTEXT {flatlist, fixity_list, label_list, var_list, tag_list, alias_list}) = 
	(if Name.LabelMap.numItems alias_list = 0
	     then ()
	 else error "Blasting out context with non-empty alias_list";
	 blastOutFixityTable os fixity_list;
	 blastOutLabelList os label_list;
	 blastOutVarList os var_list;
	 blastOutTagList os tag_list)

    fun blastInContext is = 
	let val fixity_list = blastInFixityTable is
	    val label_list = blastInLabelList is
	    val var_list = blastInVarList is
	    val tag_list = blastInTagList is
	in CONTEXT {flatlist = [], fixity_list = fixity_list, label_list = label_list, 
		    var_list = var_list, tag_list = tag_list, alias_list = Name.LabelMap.empty}
	end

    end (* local *)


	(* alpha-conversion is necessary when checking contexts for
	 * equality.  This is done by explicitly maintaining a `var
	 * map' mapping variables to variables.  *)

	exception NOT_EQUAL    (* raised when contexts are not equal *)

	fun wrap str f arg = let fun msg() = (print str; print " returning false\n")
				 val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
				 val _ = if res then () 
					 else msg()
			     in  res
			     end
	fun wrap' (str,thunk) f arg =
	    let fun msg() = (print str; print " returning false\n"; thunk arg)
				 val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
				 val _ = if res then () 
					 else msg()
			     in  res
			     end

	type vm = var Name.VarMap.map
	structure VM =
	    struct val empty : vm = Name.VarMap.empty
		   val add : var * var * vm -> vm = fn (v,v',vm) => Name.VarMap.insert(vm,v,v')
		   val lookup : vm -> var -> var = 
		       fn vm => fn v => case Name.VarMap.find(vm,v) of
		                             SOME v => v
					   | NONE => error ("VM.lookup failed on " ^ (Name.var2string v))
		   fun eq_var(vm,v,v') = 
		       case Name.VarMap.find(vm,v) of
			   SOME v => Name.eq_var(v,v')
			 | NONE => Name.eq_var(v,v')
		   val eq_var = wrap' ("eq_var",fn(_,v,v') =>
				       (Ppil.pp_var v; print ", "; Ppil.pp_var v'; print "\n"))
				       eq_var
	    end


	fun extend_vm_context (c : context, c' : context, vm) : vm * var list =
	    let val vlist = Context_Varlist c
		val vlist' = Context_Varlist c'
		fun mapper ctxt v = (case Context_Lookup'(ctxt,v) of
				    SOME (l,_) => (* is this too conservative? *)
					if (IlUtil.is_nonexport_lab l)
					    then NONE else SOME(l, v)
				  | NONE => (print "extend_vm_context: could not find var = ";
					     Ppil.pp_var v; print "\n";
					     error "extend_vm_context"))
		val lvlist = List.mapPartial (mapper c) vlist
		val lvlist' = List.mapPartial (mapper c') vlist'

		val _ = if length lvlist <> length lvlist' 
			    then (print "extend_vm_contxt: lvlist length not equal\n";
				  raise NOT_EQUAL)
			else ()
		fun folder ((l,v), vm) =
		      case Context_Lookup(c',l) of
			   SOME(SIMPLE_PATH v',_) => VM.add(v,v',vm)
		         | SOME(COMPOUND_PATH (v',_),_) => VM.add(v,v',vm)
			 | NONE => (print "label not found in c'\n"; 
				    raise NOT_EQUAL)
	    in (foldr folder vm lvlist, map #2 lvlist)
	    end

	fun sdecs_lookup([],l) = NONE
	  | sdecs_lookup(SDEC(l',dec)::sdecs,l) =
	    if Name.eq_label(l,l') then SOME dec else sdecs_lookup(sdecs,l)

	fun sbnds_lookup([],l) = NONE
	  | sbnds_lookup(SBND(l',bnd)::sbnds,l) =
	    if Name.eq_label(l,l') then SOME bnd else sbnds_lookup(sbnds,l)

	fun add_dec(DEC_EXP(v,_), DEC_EXP(v',_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_MOD(v,_), DEC_MOD(v',_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_CON(v,_,_), DEC_CON(v',_,_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_EXCEPTION(t,_), DEC_EXCEPTION(t',_), vm) = vm
	  | add_dec _ = raise NOT_EQUAL

	fun add_bnd(BND_EXP(v,_), BND_EXP(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_MOD(v,_), BND_MOD(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_CON(v,_), BND_CON(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd _ = raise NOT_EQUAL

	fun extend_vm_sdecs(sdecs,sdecs',vm) : vm =
	    let val _ = if length sdecs <> length sdecs' then raise NOT_EQUAL else ()
	    in foldr (fn (SDEC(l,dec), vm) => case sdecs_lookup(sdecs',l)
		                                of SOME dec' => add_dec(dec,dec',vm)
					         | NONE => raise NOT_EQUAL) vm sdecs
	    end

	fun extend_vm_sbnds(sbnds,sbnds',vm) : vm =
	    let val _ = if length sbnds <> length sbnds' then raise NOT_EQUAL else ()
	    in foldr (fn (SBND(l,bnd), vm) => case sbnds_lookup(sbnds',l)
		                                of SOME bnd' => add_bnd(bnd,bnd',vm)
					         | NONE => raise NOT_EQUAL) vm sbnds
	    end


	fun eq_labels([],[]) = true
	  | eq_labels(lbl::lbls,lbl'::lbls') = 
	    Name.eq_label(lbl,lbl') andalso eq_labels(lbls,lbls')
	  | eq_labels _ = false

	val eq_labels = wrap "eq_labels" eq_labels

	fun eq_vars(vm,v1,v2) = 
	    let fun loop([],[],vm) = SOME vm
		  | loop(v::vs,v'::vs',vm) = loop(vs,vs',VM.add(v,v',vm))
		  | loop _ = NONE
	    in  loop(v1,v2,vm)
	    end

	fun eq_opt equal (opt1,opt2) = 
	    case (opt1,opt2) of
		(NONE,NONE) => true
	      | (SOME c1, SOME c2) => equal(c1,c2)
	      | _ => false

	fun eq_path(vm,path,path') =
	    case (path, path')
	      of (SIMPLE_PATH v, SIMPLE_PATH v') => VM.eq_var(vm,v,v')
               | (COMPOUND_PATH(v,lbls), COMPOUND_PATH(v',lbls')) =>
		     VM.eq_var(vm,v,v') andalso eq_labels(lbls,lbls')
	       | _ => false

	fun eq_pathopt (vm,pathopt,pathopt') = 
	    eq_opt (fn (p1,p2) => eq_path(vm,p1,p2)) (pathopt,pathopt')

	val eq_path = wrap "eq_path" eq_path
	val eq_pathopt = wrap "eq_pathopt" eq_pathopt

	fun eq_mod' (vm,MOD_VAR v,MOD_VAR v') = VM.eq_var(vm,v,v')
	  | eq_mod' (vm,MOD_PROJECT(m1,l1),MOD_PROJECT(m2,l2)) = 
	    eq_mod(vm,m1,m2) andalso Name.eq_label(l1,l2)
	  | eq_mod' (vm,MOD_FUNCTOR(v1,s1,m1),MOD_FUNCTOR(v2,s2,m2)) = 
	    eq_signat (vm,s1,s2) andalso eq_mod(VM.add(v1,v2,vm),m1,m2)
	  | eq_mod' (vm,MOD_STRUCTURE sbnds1,MOD_STRUCTURE sbnds2) = eq_sbnds(vm,sbnds1,sbnds2)
	  | eq_mod' _ = false

	and eq_mod arg = wrap "eq_mod" eq_mod' arg

	and eq_con(vm,con,con') =
	    case (con,con')
	      of (CON_VAR v, CON_VAR v') => VM.eq_var(vm,v,v')           
	       | (CON_TYVAR tv, CON_TYVAR tv') => eq_tyvar(vm,tv,tv')
	       | (CON_OVAR ov, CON_OVAR ov') => eq_ovar(vm,ov,ov')
	       | (CON_FLEXRECORD fr, CON_FLEXRECORD fr') => eq_flexinforef(vm,fr,fr')
	       | (CON_INT intsize, CON_INT intsize') => intsize=intsize'
	       | (CON_UINT intsize, CON_UINT intsize') => intsize=intsize'
	       | (CON_FLOAT floatsize, CON_FLOAT floatsize') => floatsize=floatsize'
	       | (CON_ARRAY con, CON_ARRAY con') => eq_con(vm,con,con')   
	       | (CON_VECTOR con, CON_VECTOR con') => eq_con(vm,con,con')
	       | (CON_ANY, CON_ANY) => true           
	       | (CON_REF con, CON_REF con') => eq_con(vm,con,con')           
	       | (CON_TAG con, CON_TAG con') => eq_con(vm,con,con')           
	       | (CON_ARROW(cons,con,b,arrow_oneshot), CON_ARROW(cons',con',b',arrow_oneshot')) =>
		     eq_cons(vm,cons,cons') andalso eq_con(vm,con,con') andalso b=b'
		     andalso eq_arrow_oneshot(vm,arrow_oneshot,arrow_oneshot')
	       | (CON_APP(con1,con2), CON_APP(con1',con2')) =>
		     eq_con(vm,con1,con1') andalso eq_con(vm,con2,con2')
	       | (CON_MU(con), CON_MU(con')) => eq_con(vm,con,con')    
	       | (CON_RECORD labcons, CON_RECORD labcons') => eq_labcons(vm,labcons,labcons')        
	       | (CON_FUN(vars,con), CON_FUN(vars',con')) => 
		     (case eq_vars(vm,vars,vars') of
			  NONE => false
			| SOME vm => eq_con(vm,con,con'))
	       | (CON_SUM{noncarriers,carrier,special}, 
		  CON_SUM{noncarriers=noncarriers',carrier=carrier',special=special'}) =>
		     noncarriers=noncarriers' andalso special=special' 
		     andalso eq_con(vm,carrier,carrier')
	       | (CON_TUPLE_INJECT cons, CON_TUPLE_INJECT cons') => 
		     eq_cons(vm,cons,cons')
	       | (CON_TUPLE_PROJECT(i,con), CON_TUPLE_PROJECT(i',con')) => 
		     i=i' andalso eq_con(vm,con,con')
	       | (CON_MODULE_PROJECT(mod,lab), CON_MODULE_PROJECT(mod',lab')) =>
		     eq_mod(vm,mod,mod') andalso Name.eq_label(lab,lab')
	       | _ => false
 

	and eq_cons(vm,[],[]) = true
	  | eq_cons(vm,con::cons,con'::cons') = eq_con(vm,con,con') andalso eq_cons(vm,cons,cons')
	  | eq_cons _ = false

	and eq_conopt(vm,SOME con,SOME con') = eq_con(vm,con,con')
	  | eq_conopt(vm,NONE,NONE) = true
	  | eq_conopt _ = false

	and eq_kind(vm,kind,kind') = 
	    case (kind,kind')
	      of (KIND_TUPLE i, KIND_TUPLE i') => i=i'
	       | (KIND_ARROW p, KIND_ARROW p') => p=p'
	       | (KIND_INLINE(kind,con), KIND_INLINE(kind',con')) => 
		     eq_kind(vm,kind,kind') andalso eq_con(vm,con,con')
	       | _ => false
 
	and eq_tyvar(vm,tv,tv') =
	    case (Tyvar.tyvar_deref tv, Tyvar.tyvar_deref tv')
	      of (SOME con, SOME con') => eq_con(vm,con,con')
	       | _ => false

	and eq_ovar(vm,ov,ov') = eq_tyvar(vm, Tyvar.ocon_deref ov, Tyvar.ocon_deref ov')

	and eq_labcons(vm,[],[]) = true
	  | eq_labcons(vm,(l,con)::labcons,(l',con')::labcons') =
	    Name.eq_label(l,l') andalso eq_con(vm,con,con') 
	    andalso eq_labcons(vm,labcons,labcons')
	  | eq_labcons _ = false

	and eq_flexinforef(vm,fr,fr') =
	    let fun find (ref(FLEXINFO(_,_,labcons))) = labcons
		  | find (ref(INDIRECT_FLEXINFO fr)) = find fr
	    in eq_labcons(vm,find fr, find fr')
	    end

	and eq_arrow_oneshot(vm,aos,aos') =
	    case (Util.oneshot_deref aos, Util.oneshot_deref aos')
	      of (SOME a, SOME a') => a=a'
               | _ => false


	and eq_signat'(vm,signat,signat') =
	    case (signat,signat') of
	         (SIGNAT_STRUCTURE(pathopt, sdecs), SIGNAT_STRUCTURE(pathopt', sdecs')) =>
		  eq_pathopt(vm,pathopt,pathopt') andalso eq_sdecs(vm,sdecs,sdecs')
	       | (SIGNAT_FUNCTOR(v,signat1,signat2,a), SIGNAT_FUNCTOR(v',signat1',signat2',a')) =>
		  eq_signat(vm,signat1,signat1') andalso a=a' andalso
		  eq_signat(VM.add(v,v',vm),signat2,signat2')
	       | (SIGNAT_INLINE_STRUCTURE{self=s1,code=c1,abs_sig=a1},
		  SIGNAT_INLINE_STRUCTURE{self=s2,code=c2,abs_sig=a2}) =>
		  eq_pathopt(vm,s1,s2) andalso
		  eq_sbnds(vm,c1,c2) andalso
		  eq_sdecs(vm,a1,a2)
               | (SIGNAT_VAR v1, SIGNAT_VAR v2) => VM.eq_var(vm,v1,v2)
               | _ => false

	and eq_signat arg = wrap "eq_signat" eq_signat' arg

	and eq_sdecs'(vm,sdecs,sdecs') =
	    let	val vm = extend_vm_sdecs(sdecs,sdecs',vm)
	    in foldand (fn (SDEC(l,dec)) =>
			case sdecs_lookup(sdecs',l)
			  of SOME dec' => eq_dec(vm,dec,dec')
			   | NONE => false) sdecs
	    end	    

	and eq_sdecs arg = wrap "eq_sdecs" eq_sdecs' arg

	and eq_sbnds'(vm,sbnds,sbnds') =
	    let	val vm = extend_vm_sbnds(sbnds,sbnds',vm)
	    in foldand (fn (SBND(l,bnd)) =>
			case sbnds_lookup(sbnds',l)
			  of SOME bnd' => eq_bnd(vm,bnd,bnd')
			   | NONE => false) sbnds
	    end	    

	and eq_sbnds arg = wrap "eq_sbnds" eq_sbnds' arg

	and eq_dec(vm,dec,dec') =
	    case (dec, dec')
	      of (DEC_EXP(v,con),DEC_EXP(v',con')) => 
		  VM.eq_var(vm,v,v') andalso eq_con(vm,con,con') 
	       | (DEC_MOD(v,signat), DEC_MOD(v',signat')) => 
	          VM.eq_var(vm,v,v') andalso eq_signat(vm,signat,signat')
               | (DEC_CON(v,kind,conopt), DEC_CON(v',kind',conopt')) =>
		  VM.eq_var(vm,v,v') andalso eq_kind(vm,kind,kind') andalso
		  eq_conopt(vm,conopt,conopt')
	       | (DEC_EXCEPTION(t,con), DEC_EXCEPTION(t',con')) => eq_con(vm,con,con') 
	       | _ => false                        (* MEMO: is this right?? *)
 
	and eq_bnd(vm,bnd,bnd') =
	    case (bnd, bnd')
	      of (BND_EXP(v,exp),BND_EXP(v',exp')) => 
		  VM.eq_var(vm,v,v') andalso eq_exp(vm,exp,exp')
	       | (BND_MOD(v,m), BND_MOD(v',m')) => 
	          VM.eq_var(vm,v,v') andalso eq_mod(vm,m,m')
               | (BND_CON(v,c), BND_CON(v',c')) =>
		  VM.eq_var(vm,v,v') andalso eq_con(vm,c,c')
	       | _ => false                        (* MEMO: is this right?? *)

	and eq_exp (vm,VAR v,VAR v') = VM.eq_var(vm,v,v')
	  | eq_exp (vm,MODULE_PROJECT(m1,l1),MODULE_PROJECT(m2,l2)) = 
	    eq_mod(vm,m1,m2) andalso Name.eq_label(l1,l2)
	  | eq_exp(vm,INJ{sumtype=c1,field=f1,inject=eopt1},
		   INJ{sumtype=c2,field=f2, inject=eopt2}) = 
	       eq_con(vm,c1,c2) andalso f1=f2 andalso
	       eq_opt (fn (e1,e2) => eq_exp(vm,e1,e2)) (eopt1,eopt2)
	  | eq_exp(vm,ROLL(c1,e1),ROLL(c2,e2)) = eq_con(vm,c1,c2) andalso eq_exp(vm,e1,e2)
	  | eq_exp(vm,UNROLL(c11,c12,e1),UNROLL(c21,c22,e2)) = 
	       eq_con(vm,c11,c21) 
	       andalso eq_con (vm,c12,c22)
	       andalso eq_exp(vm,e1,e2)
	  | eq_exp(vm,FIX(b1,a1,fbnds1),FIX(b2,a2,fbnds2)) = 
	       b1=b2 andalso a1=a2 andalso eq_fbnds(vm,fbnds1,fbnds2)
	  | eq_exp _ = false

	and eq_fbnds(vm,fbnds1,fbnds2) = 
	    let val fbnds1 = map (fn (FBND x) => x) fbnds1
		val fbnds2 = map (fn (FBND x) => x) fbnds2
		fun extend (((v1,_,_,_,_),(v2,_,_,_,_)),vm) = VM.add(v1,v2,vm)
		fun equaler vm ((_,arg1,c1,rc1,e1),(_,arg2,c2,rc2,e2)) = 
		    let val vm = VM.add(arg1,arg2,vm)
		    in  eq_con(vm,c1,c2) andalso eq_con(vm,rc1,rc2) andalso eq_exp(vm,e1,e2)
		    end
	    in  (length fbnds1 = length fbnds2) andalso
		Listops.eq_list (equaler (foldl extend vm (Listops.zip fbnds1 fbnds2)), fbnds1, fbnds2)
	    end

	fun eq_pc(vm, pc, pc') =
	    case (pc, pc')
	      of (PHRASE_CLASS_EXP(exp,con), PHRASE_CLASS_EXP(exp',con')) =>
		  eq_exp(vm,exp,exp') andalso eq_con(vm,con,con')
	       | (PHRASE_CLASS_CON(con,kind), PHRASE_CLASS_CON(con',kind')) =>
		  eq_con(vm,con,con') andalso eq_kind(vm,kind,kind')		  
	       | (PHRASE_CLASS_MOD(mod,signat), PHRASE_CLASS_MOD(mod',signat')) =>
		  eq_mod(vm,mod,mod') andalso eq_signat(vm,signat,signat')
	       | (PHRASE_CLASS_SIG (v,signat), PHRASE_CLASS_SIG (v',signat')) =>
		  VM.eq_var(vm,v,v') andalso eq_signat(vm,signat,signat')
	       | _ => false 

	fun eq_cntxt(vm,c,c',vars) =
	    let fun folder v =
		    let fun diag s = 
			if (!debug)
			    then (print s;
				  print "eq_cntxt was processing v = ";
				  Ppil.pp_var v; print "\n";
				  print "context 1 = \n";
				  Ppil.pp_context c;
				  print "\n\ncontext 2 = \n";
				  Ppil.pp_context c';
				  print "\n\n")
			else ()
			val res = 
			    (case (Context_Lookup'(c,v), Context_Lookup'(c',VM.lookup vm v)) of
				 (SOME (_, pc), SOME (_, pc')) => eq_pc(vm,pc,pc')
			       | _ => false)
				handle e => (diag "eq_cntxt caught exception\n";
					     raise e)
		    in  if res then res else (diag "eq_cntxt got false\n"; false)
		    end
		val res = foldand folder vars
		val _ = (print "eq_cntxt returning "; 
			 print (Bool.toString res);
			 print "\n")
	    in  res
	    end	    

	fun eq_context (c: context, c': context) : bool =
	    let 
(*
		val _ = (print "context c = \n";
			 Ppil.pp_context c; 
			 print "\n";
			 print "context c' = \n";
			 Ppil.pp_context c'; 
			 print "\n")
*)
		val (vm,vlist) = extend_vm_context(c,c',VM.empty)
(*
		val _ = print "done extend_vm_context\n"
*)
	    in eq_cntxt(vm,c,c',vlist)
	    end handle NOT_EQUAL => false

    end


