(*$import Il IlContext IlUtil Ppil Blaster NameBlast Word8 BinIO ILCONTEXTEQ Bool Stats Util *)
(* Equality of contexts *)

structure IlContextEq 
    :> ILCONTEXTEQ =
struct

    val debug = Stats.ff("IlcontexteqDebug")
    val blast_debug = Stats.ff("BlastDebug")
    val useOldBlast = ref false

    val cur_out = ref (NONE : BinIO.outstream option)
    val cur_in = ref (NONE : BinIO.instream option)
    fun curOut() = valOf(!cur_out)
    fun curIn() = valOf(!cur_in)

    open Util
    open IlContext
    open Il

    fun error s = Util.error "IlContextEq" s
    nonfix mod

    (* debugging stuff *)
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

	
    (* specialized blasters from Blaster and NameBlast *)
    fun blastOutInt i = Blaster.blastOutInt (curOut()) i
    fun blastInInt () = Blaster.blastInInt (curIn())
    fun blastOutBool i = Blaster.blastOutBool (curOut()) i
    fun blastInBool () = Blaster.blastInBool (curIn())
    fun blastOutString i = Blaster.blastOutString (curOut()) i
    fun blastInString () = Blaster.blastInString (curIn())
    fun blastOutWord64 i = Blaster.blastOutWord64 (curOut()) i
    fun blastInWord64 () = Blaster.blastInWord64 (curIn())
    fun blastOutList blaster objs = Blaster.blastOutList (fn _ => blaster) (curOut()) objs
    fun blastInList blaster = Blaster.blastInList (fn _ => blaster()) (curIn()) 
    fun blastOutOption blaster obj = Blaster.blastOutOption (fn _ => blaster) (curOut()) obj
    fun blastInOption blaster = Blaster.blastInOption (fn _ => blaster()) (curIn()) 
    fun blastOutPair b1 b2 obj = Blaster.blastOutPair (fn _ => b1) (fn _ => b2) (curOut()) obj
    fun blastInPair b1 b2 = Blaster.blastInPair (fn _ => b1()) (fn _ => b2()) (curIn()) 
    fun blastOutTriple b1 b2 b3 obj = Blaster.blastOutTriple (fn _ => b1) (fn _ => b2) (fn _ => b3) (curOut()) obj
    fun blastInTriple b1 b2 b3 = Blaster.blastInTriple (fn _ => b1()) (fn _ => b2()) (fn _ => b3()) (curIn()) 

    fun blastOutVar v = NameBlast.blastOutVar (curOut()) v
    fun blastInVar () = NameBlast.blastInVar (curIn())
    fun blastOutLabel l = NameBlast.blastOutLabel (curOut()) l
    fun blastInLabel () = NameBlast.blastInLabel (curIn())
    fun blastOutTag t = NameBlast.blastOutTag (curOut()) t
    fun blastInTag () = NameBlast.blastInTag (curIn())


    fun blastOutChoice i = if (!useOldBlast)
				  then blastOutInt i
			      else BinIO.output1(curOut(),Word8.fromInt i)
    fun blastInChoice is = if (!useOldBlast)
			          then blastInInt ()
			   else Word8.toInt(case BinIO.input1 (curIn()) of
						NONE => error "blastInChoice failed"
					      | SOME w => w)

    fun blastOutPath (SIMPLE_PATH v) = (blastOutChoice 0; blastOutVar v)
      | blastOutPath (COMPOUND_PATH (v,ls)) = (blastOutChoice 1; blastOutVar v; 
					       blastOutList blastOutLabel ls)

    fun blastInPath () = let val _ = tab "blastInPath:" 
			     val which = blastInChoice()
			     val v = blastInVar()
			     val res = if (which = 0)
					   then SIMPLE_PATH v
				       else COMPOUND_PATH(v, blastInList blastInLabel)
			     val _ = (case res of
					  SIMPLE_PATH v => (say (Name.var2string v))
					| COMPOUND_PATH (v,ls) => (say (Name.var2string v);
								   app (fn l => (say ".";
										 say (Name.label2string l)))
								   ls))
			     val _ = say "\n"
			 in res
			 end

	fun blastOutArrow TOTAL = blastOutChoice 0
	  | blastOutArrow PARTIAL = blastOutChoice 1
	fun blastInArrow () =
	    (case (blastInChoice()) of
		 0 => TOTAL
	       | 1 => PARTIAL
	       | _ => error "bad blastInArrow")

	fun blastOutIS Prim.W8 = blastOutChoice 0
	  | blastOutIS Prim.W16 = blastOutChoice 1
	  | blastOutIS Prim.W32 = blastOutChoice 2
	  | blastOutIS Prim.W64 = blastOutChoice 3
	fun blastInIS () =
	    (case blastInChoice() of
		 0 => Prim.W8
	       | 1 => Prim.W16
	       | 2 => Prim.W32
	       | 3 => Prim.W64
	       | _ => (error "bad blastInIS" handle e => raise e))
	fun blastOutFS Prim.F32 = blastOutChoice 0
	  | blastOutFS Prim.F64 = blastOutChoice 1
	fun blastInFS () =
	    (case blastInChoice() of
		 0 => Prim.F32
	       | 1 => Prim.F64
	       | _ => error "bad blastInFS")

	fun blastOutDec dec = 
	    (case dec of
		 DEC_EXP (v,c) => (blastOutChoice 0; blastOutVar v; blastOutCon c)
	       | DEC_CON (v,k,NONE) => (blastOutChoice 1; blastOutVar v; blastOutKind k)
	       | DEC_CON (v,k,SOME c) => (blastOutChoice 2; blastOutVar v; blastOutKind k; blastOutCon c)
	       | DEC_MOD (v,s) => (blastOutChoice 3; blastOutVar v; blastOutSig s)
	       | DEC_EXCEPTION (t,c) =>  (blastOutChoice 4; blastOutTag t; blastOutCon c))
	and blastInDec () =
	    (case (blastInChoice()) of
		 0 => DEC_EXP (blastInVar (), blastInCon ())
	       | 1 => DEC_CON (blastInVar (), blastInKind (), NONE)
	       | 2 => DEC_CON (blastInVar (), blastInKind (), SOME (blastInCon ()))
	       | 3 => DEC_MOD (blastInVar (), blastInSig ())
	       | 4 => DEC_EXCEPTION (blastInTag (), blastInCon ())
	       | _ => error "bad blastInDec")
	and blastOutBnd bnd = 
	    (case bnd of
		 BND_EXP (v,e) => (blastOutChoice 0; blastOutVar v; blastOutExp e)
	       | BND_CON (v,c) => (blastOutChoice 1; blastOutVar v; blastOutCon c)
	       | BND_MOD (v,m) => (blastOutChoice 2; blastOutVar v; blastOutMod m))
	and blastInBnd () =
	    (case (blastInChoice()) of
		 0 => BND_EXP(blastInVar (), blastInExp ())
	       | 1 => BND_CON(blastInVar (), blastInCon ())
	       | 2 => BND_MOD(blastInVar (), blastInMod ())
	       | _ => error "bad blastInBnd")
	and blastOutSdec (SDEC(l,dec)) = (blastOutLabel l; blastOutDec dec)
	and blastInSdec () = SDEC(blastInLabel (), blastInDec ())
	and blastOutSbnd (SBND(l,bnd)) = (blastOutLabel l; blastOutBnd bnd)
	and blastInSbnd () = SBND(blastInLabel (), blastInBnd ())

	and blastOutSdecs sdecs = blastOutList blastOutSdec sdecs
	and blastInSdecs () = blastInList blastInSdec
	and blastOutSbnds sbnds = blastOutList blastOutSbnd sbnds
	and blastInSbnds () = blastInList blastInSbnd

	and blastOutKind k = 
	    (case k of
		KIND_TUPLE n => (blastOutChoice 0; blastOutChoice n)
	      | KIND_ARROW (m,n) => (blastOutChoice 1; blastOutChoice m;  blastOutChoice n)
	      | KIND_INLINE (k,c) => (blastOutChoice 2; blastOutKind k; blastOutCon c))
		    
	and blastInKind () = 
	    (case blastInChoice() of
		0 => KIND_TUPLE(blastInChoice())
	      | 1 => KIND_ARROW(blastInChoice(), blastInChoice())
	      | 2 => KIND_INLINE(blastInKind (), blastInCon ())
	      | _ => error "bad blastInKind")

	and blastOutCon c = 
	    (case c of
		 CON_VAR v => (blastOutChoice 0; blastOutVar v)
	       | CON_TYVAR tv => (case Tyvar.tyvar_deref tv of
				      SOME c => blastOutCon c
				    | NONE => error "cannot blastOut unresolved CON_TYVAR")
	       | CON_OVAR oc => blastOutCon (CON_TYVAR (Tyvar.ocon_deref oc))
	       | CON_FLEXRECORD (ref (INDIRECT_FLEXINFO r)) => blastOutCon (CON_FLEXRECORD r)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, true, lclist))) => blastOutCon (CON_RECORD lclist)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, false, _))) => error "cannot blastOut flex record type"
	       | CON_INT is => (blastOutChoice 1; blastOutIS is)
	       | CON_UINT is => (blastOutChoice 2; blastOutIS is)
	       | CON_FLOAT fs => (blastOutChoice 3; blastOutFS fs)
	       | CON_ARRAY c => (blastOutChoice 4; blastOutCon c)
	       | CON_VECTOR c => (blastOutChoice 5; blastOutCon c)
	       | CON_ANY => (blastOutChoice 6)
	       | CON_REF c => (blastOutChoice 7; blastOutCon c)
	       | CON_TAG c => (blastOutChoice 8; blastOutCon c)
	       | CON_ARROW (cs,c,f,oa) => (blastOutChoice 9; 
					   blastOutList blastOutCon cs;
					   blastOutCon c; blastOutBool f;
					   blastOutArrow (case (oneshot_deref oa) of
								 SOME a => a
							       | _ => error "unresolved CON_ARROW"))
	       | CON_APP (c1,c2) => (blastOutChoice 10; blastOutCon c1; blastOutCon c2)
	       | CON_MU c => (blastOutChoice 11; blastOutCon c)
	       | CON_RECORD lclist => (blastOutChoice 12; blastOutList (blastOutPair blastOutLabel blastOutCon) lclist)
	       | CON_FUN (vlist, c) => (blastOutChoice 13; blastOutList blastOutVar vlist; blastOutCon c)
	       | CON_SUM {noncarriers, carrier, special = NONE} => 
		     (blastOutChoice 14; blastOutChoice noncarriers; 
		      blastOutCon carrier)
	       | CON_SUM {noncarriers, carrier, special = SOME i} => 
		     (blastOutChoice 15; blastOutChoice noncarriers; 
		      blastOutCon carrier; blastOutChoice i)
	       | CON_TUPLE_INJECT clist => (blastOutChoice 16; blastOutList blastOutCon clist)
	       | CON_TUPLE_PROJECT (i,c) => (blastOutChoice 17; blastOutChoice i; blastOutCon c)
	       | CON_MODULE_PROJECT (m,l) => (blastOutChoice 18; blastOutMod m; blastOutLabel l))

        and blastInCon () = 
	    let val _ = push()
		val _ = tab "blastInCon\n"
		val res = blastInCon' ()
		val _ = pop()
	    in  res
	    end

	and blastInCon' () = 
	    (case (blastInChoice()) of
		 0 => CON_VAR (blastInVar ())
	       | 1 => CON_INT (blastInIS ())
	       | 2 => CON_UINT (blastInIS ())
	       | 3 => CON_FLOAT (blastInFS ())
	       | 4 => CON_ARRAY (blastInCon ())
	       | 5 => CON_VECTOR (blastInCon ())
	       | 6 => CON_ANY
	       | 7 => CON_REF (blastInCon ())
	       | 8 => CON_TAG (blastInCon ())
	       | 9 => let val cs = blastInList blastInCon
			  val c = blastInCon ()
			  val f = blastInBool ()
			  val a = oneshot_init (blastInArrow ())
		      in 
			  CON_ARROW (cs,c,f,a)
		      end
	       | 10 => CON_APP (blastInCon (), blastInCon ())
	       | 11 => CON_MU (blastInCon ())
	       | 12 => CON_RECORD(blastInList (fn () => blastInPair blastInLabel blastInCon))
	       | 13 => CON_FUN (blastInList blastInVar, blastInCon ())
	       | 14 => CON_SUM {noncarriers = blastInChoice(),
				carrier = blastInCon (),
				special = NONE} 
	       | 15 => CON_SUM {noncarriers = blastInChoice(),
				carrier = blastInCon (),
				special = SOME (blastInChoice())}
	       | 16 => CON_TUPLE_INJECT (blastInList blastInCon)
	       | 17 => CON_TUPLE_PROJECT (blastInChoice(), blastInCon ())
	       | 18 => CON_MODULE_PROJECT (blastInMod (), blastInLabel ())
	       | _ => error "bad blastInCon")

	and blastOutValue v = 
	    (case v of
		 (Prim.int (is,w64)) => (blastOutChoice 0; blastOutIS is; blastOutWord64 w64)
	       | (Prim.uint (is,w64)) => (blastOutChoice 1; blastOutIS is; blastOutWord64 w64)
	       | (Prim.float (fs,str)) => (blastOutChoice 2; blastOutFS fs; blastOutString str)
	       | (Prim.tag (t,c)) => (blastOutChoice 3; blastOutTag t; blastOutCon c)
	       | _ => error "blasting of array/vector/refcell not supported")

	and blastInValue () =
	    (case (blastInChoice()) of
		 0 => Prim.int (blastInIS (), blastInWord64 ())
	       | 1 => Prim.uint (blastInIS (), blastInWord64 ())
	       | 2 => Prim.float (blastInFS (), blastInString ())
	       | 3 => Prim.tag (blastInTag (), blastInCon ())
	       | _ => error "bad blastInValue")

	and blastOutIlPrim ilprim = 
	    let open Prim
	    in  (case ilprim of
		     eq_uint is => (blastOutChoice 0; blastOutIS is)
		   | neq_uint is => (blastOutChoice 1; blastOutIS is)
		   | not_uint is => (blastOutChoice 2; blastOutIS is)
		   | and_uint is => (blastOutChoice 3; blastOutIS is)
		   | or_uint is => (blastOutChoice 4; blastOutIS is)
		   | lshift_uint is => (blastOutChoice 5; blastOutIS is)
		   | mk_ref => (blastOutChoice 6)
		   | deref => (blastOutChoice 7)
		   | eq_ref => (blastOutChoice 8)
		   | setref => (blastOutChoice 9))
	    end

	and blastInIlPrim () = 
	    let open Prim
	    in  (case (blastInChoice()) of
		     0 => eq_uint(blastInIS ())
		   | 1 => neq_uint(blastInIS ())
		   | 2 => not_uint(blastInIS ())
		   | 3 => and_uint(blastInIS ())
		   | 4 => or_uint(blastInIS ())
		   | 5 => lshift_uint(blastInIS ())
		   | 6 => mk_ref
		   | 7 => deref
		   | 8 => eq_ref
		   | 9 => setref
		   | _ => error "bad blastInIlPrim")
	    end


	and blastOutTT tt =
	    let open Prim 
	    in  (case tt of
		     int_tt => blastOutChoice 0
		   | real_tt => blastOutChoice 1
		   | both_tt => blastOutChoice 2)
	    end


	and blastInTT () =
	    let open Prim 
	    in  (case (blastInChoice()) of
		     0 => int_tt
		   | 1 => real_tt
		   | 2 => both_tt
		   | _ => error "bad blastInTT")
	    end
			 
	and blastOutTable table = 
	    let open Prim 
	    in  (case table of
		     IntArray is => (blastOutChoice 0; blastOutIS is)
		   | IntVector is => (blastOutChoice 1; blastOutIS is)
		   | FloatArray fs => (blastOutChoice 2; blastOutFS fs)
		   | FloatVector fs => (blastOutChoice 3; blastOutFS fs)
		   | OtherArray hnf => (blastOutChoice 4; blastOutBool hnf)
		   | OtherVector hnf => (blastOutChoice 5; blastOutBool hnf))
	    end

	and blastInTable () =
	    let open Prim 
	    in  (case (blastInChoice()) of
		     0 => IntArray (blastInIS ())
		   | 1 => IntVector (blastInIS ())
		   | 2 => FloatArray (blastInFS ())
		   | 3 => FloatVector (blastInFS ())
		   | 4 => OtherArray (blastInBool ())
		   | 5 => OtherVector (blastInBool ())
		   | _ => error "bad blastInTable")
	    end

	and blastOutPrim prim = 
	    let open Prim
	    in  (case prim of
		     soft_vtrap tt => (blastOutChoice 0; blastOutTT tt)
		   | soft_ztrap tt => (blastOutChoice 1; blastOutTT tt)
		   | hard_vtrap tt => (blastOutChoice 2; blastOutTT tt)
		   | hard_ztrap tt => (blastOutChoice 3; blastOutTT tt)
			 

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | float2int (* floor *) => (blastOutChoice 6)
		   | int2float (* real  *) => (blastOutChoice 7)
		   | int2uint (is1,is2) => (blastOutChoice 8; blastOutIS is1; blastOutIS is2)
		   | uint2int (is1,is2) => (blastOutChoice 9; blastOutIS is1; blastOutIS is2)
		   | uinta2uinta (is1,is2) => (blastOutChoice 10; blastOutIS is1; blastOutIS is2)
		   | uintv2uintv (is1,is2) => (blastOutChoice 11; blastOutIS is1; blastOutIS is2)


		   (* floatint-point operations *)	
		   | neg_float fs  => (blastOutChoice 14; blastOutFS fs)
		   | abs_float fs  => (blastOutChoice 15; blastOutFS fs)
		   | plus_float fs  => (blastOutChoice 16; blastOutFS fs)
		   | minus_float fs  => (blastOutChoice 17; blastOutFS fs)
		   | mul_float fs  => (blastOutChoice 18; blastOutFS fs)
		   | div_float fs  => (blastOutChoice 19; blastOutFS fs)
		   | less_float fs  => (blastOutChoice 20; blastOutFS fs)
		   | greater_float fs  => (blastOutChoice 21; blastOutFS fs)
		   | lesseq_float fs  => (blastOutChoice 22; blastOutFS fs)
		   | greatereq_float  fs  => (blastOutChoice 23; blastOutFS fs)
		   | eq_float  fs  => (blastOutChoice 24; blastOutFS fs)
		   | neq_float fs  => (blastOutChoice 25; blastOutFS fs)

		   (* int operations *)
		   | plus_int is  => (blastOutChoice 26; blastOutIS is)
		   | minus_int is  => (blastOutChoice 27; blastOutIS is)
		   | mul_int is  => (blastOutChoice 28; blastOutIS is)
		   | div_int is  => (blastOutChoice 29; blastOutIS is)
		   | mod_int is  => (blastOutChoice 30; blastOutIS is)
		   | quot_int is  => (blastOutChoice 31; blastOutIS is)
		   | rem_int is  => (blastOutChoice 32; blastOutIS is)
		   | plus_uint is  => (blastOutChoice 33; blastOutIS is)
		   | minus_uint is  => (blastOutChoice 34; blastOutIS is)
		   | mul_uint is  => (blastOutChoice 35; blastOutIS is)
		   | div_uint is  => (blastOutChoice 36; blastOutIS is)
		   | mod_uint is  => (blastOutChoice 37; blastOutIS is)
		   | less_int is  => (blastOutChoice 38; blastOutIS is)
		   | greater_int is  => (blastOutChoice 39; blastOutIS is)
		   | lesseq_int is  => (blastOutChoice 40; blastOutIS is)
		   | greatereq_int is  => (blastOutChoice 41; blastOutIS is)
		   | less_uint is  => (blastOutChoice 42; blastOutIS is)
		   | greater_uint is  => (blastOutChoice 43; blastOutIS is)
		   | lesseq_uint is  => (blastOutChoice 44; blastOutIS is)
		   | greatereq_uint is  => (blastOutChoice 45; blastOutIS is)
		   | eq_int is  => (blastOutChoice 46; blastOutIS is)
		   | neq_int is  => (blastOutChoice 47; blastOutIS is)
		   | neg_int is  => (blastOutChoice 48; blastOutIS is)
		   | abs_int is  => (blastOutChoice 49; blastOutIS is)

		   (* bit-pattern manipulation *)
		   | not_int is  => (blastOutChoice 50; blastOutIS is)
		   | and_int is  => (blastOutChoice 51; blastOutIS is)
		   | or_int is  => (blastOutChoice 52; blastOutIS is)
		   | lshift_int is  => (blastOutChoice 53; blastOutIS is)
		   | rshift_int is  => (blastOutChoice 54; blastOutIS is)
		   | rshift_uint is  => (blastOutChoice 55; blastOutIS is)
			 
		   (* array and vectors *)
		   | array2vector t => (blastOutChoice 56; blastOutTable t)
		   | create_table t => (blastOutChoice 57; blastOutTable t)
		   | sub t => (blastOutChoice 58; blastOutTable t)
		   | update t => (blastOutChoice 59; blastOutTable t)
		   | length_table t => (blastOutChoice 60; blastOutTable t)
		   | equal_table t => (blastOutChoice 61; blastOutTable t)

		   (* IO operations *)
		   | open_in => (blastOutChoice 62)
		   | input => (blastOutChoice 63)
		   | input1 => (blastOutChoice 64)
		   | lookahead => (blastOutChoice 65)
		   | open_out => (blastOutChoice 66)
		   | close_in => (blastOutChoice 67)
		   | output => (blastOutChoice 68)
		   | flush_out => (blastOutChoice 69)
		   | close_out => (blastOutChoice 70)
		   | end_of_stream => (blastOutChoice 71))

	    end

	and blastInPrim () =
	    let open Prim
	    in  (case (blastInChoice()) of
		     0 => soft_vtrap(blastInTT ())
		   | 1 => soft_ztrap(blastInTT ())
		   | 2 => hard_vtrap(blastInTT ())
		   | 3 => hard_ztrap(blastInTT ())
			 

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | 6 => float2int (* floor *)
		   | 7 => int2float (* real  *)
		   | 8 => int2uint (blastInIS (), blastInIS ())
		   | 9 => uint2int(blastInIS (), blastInIS ())
		   | 10 => uinta2uinta(blastInIS (), blastInIS ())
		   | 11 => uintv2uintv(blastInIS (), blastInIS ())


		   (* floatint-point operations *)	
		   | 14 => neg_float(blastInFS ())
		   | 15 => abs_float(blastInFS ())
		   | 16 => plus_float(blastInFS ())
		   | 17 => minus_float(blastInFS ())
		   | 18 => mul_float(blastInFS ())
		   | 19 => div_float(blastInFS ())
		   | 20 => less_float(blastInFS ())
		   | 21 => greater_float(blastInFS ())
		   | 22 => lesseq_float(blastInFS ())
		   | 23 => greatereq_float (blastInFS ())
		   | 24 => eq_float (blastInFS ())
		   | 25 => neq_float(blastInFS ())

		   (* int operations *)
		   | 26 => plus_int(blastInIS ())
		   | 27 => minus_int(blastInIS ())
		   | 28 => mul_int(blastInIS ())
		   | 29 => div_int(blastInIS ())
		   | 30 => mod_int(blastInIS ())
		   | 31 => quot_int(blastInIS ())
		   | 32 => rem_int(blastInIS ())
		   | 33 => plus_uint(blastInIS ())
		   | 34 => minus_uint(blastInIS ())
		   | 35 => mul_uint(blastInIS ())
		   | 36 => div_uint(blastInIS ())
		   | 37 => mod_uint(blastInIS ())
		   | 38 => less_int(blastInIS ())
		   | 39 => greater_int(blastInIS ())
		   | 40 => lesseq_int(blastInIS ())
		   | 41 => greatereq_int(blastInIS ())
		   | 42 => less_uint(blastInIS ())
		   | 43 => greater_uint(blastInIS ())
		   | 44 => lesseq_uint(blastInIS ())
		   | 45 => greatereq_uint(blastInIS ())
		   | 46 => eq_int(blastInIS ())
		   | 47 => neq_int(blastInIS ())
		   | 48 => neg_int(blastInIS ())
		   | 49 => abs_int(blastInIS ())

		   (* bit-pattern manipulation *)
		   | 50 => not_int(blastInIS ())
		   | 51 => and_int(blastInIS ())
		   | 52 => or_int(blastInIS ())
		   | 53 => lshift_int(blastInIS ())
		   | 54 => rshift_int(blastInIS ())
		   | 55 => rshift_uint(blastInIS ())
			 
		   (* array and vectors *)
		   | 56 => array2vector (blastInTable ())
		   | 57 => create_table (blastInTable ())
		   | 58 => sub (blastInTable ())
		   | 59 => update (blastInTable ())
		   | 60 => length_table (blastInTable ())
		   | 61 => equal_table (blastInTable ())

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

	and blastOutExp exp = 
	    (case exp of
		 OVEREXP (_,_,oe) => (case oneshot_deref oe of
					  SOME e => blastOutExp e
					| NONE => error "cannot blastOut unresolved OVEREXP")
	       | SCON v => (blastOutChoice 0; blastOutValue v)
	       | PRIM (p,clist,elist) => (blastOutChoice 1; blastOutPrim p;
					  blastOutList blastOutCon clist;
					  blastOutList blastOutExp elist)
	       | ILPRIM (p,clist,elist) => (blastOutChoice 2; blastOutIlPrim p;
					    blastOutList blastOutCon clist;
					    blastOutList blastOutExp elist)
	       | ETAPRIM (p,clist) => (blastOutChoice 3; blastOutPrim p;
				       blastOutList blastOutCon clist)
	       | ETAILPRIM (p,clist) => (blastOutChoice 4; blastOutIlPrim p;
					 blastOutList blastOutCon clist)
	       | VAR v => (blastOutChoice 5; blastOutVar v)
	       | APP (e1,e2) => (blastOutChoice 6; blastOutExp e1; 
				 blastOutExp e2)
	       | EXTERN_APP (c,e,elist) => (blastOutChoice 23; 
					    blastOutCon c;
					    blastOutExp e; 
					    blastOutList blastOutExp elist)
	       | FIX (b,a,fbnds) => (blastOutChoice 7; blastOutBool b; blastOutArrow a;
				     blastOutList blastOutFbnd fbnds)
	       | RECORD lelist => (blastOutChoice 8; blastOutList (blastOutPair blastOutLabel blastOutExp) lelist)
	       | RECORD_PROJECT (e,l,c) => (blastOutChoice 9; blastOutExp e; blastOutLabel l; blastOutCon c)
	       | SUM_TAIL (i,c,e) => (blastOutChoice 10; blastOutChoice i;
				      blastOutCon c; blastOutExp e)
	       | HANDLE (e1,e2) => (blastOutChoice 11; blastOutExp e1; blastOutExp e2)
	       | RAISE (c,e) => (blastOutChoice 12; blastOutCon c; blastOutExp e)
	       | LET(bnds,e) => (blastOutChoice 13; blastOutList blastOutBnd bnds; blastOutExp e)
	       | NEW_STAMP c => (blastOutChoice 14; blastOutCon c)
	       | EXN_INJECT (str,e1,e2) => (blastOutChoice 15; blastOutString str; blastOutExp e1; blastOutExp e2)
	       | ROLL (c,e) => (blastOutChoice 16; blastOutCon c; blastOutExp e)
	       | UNROLL (c1,c2,e) => (blastOutChoice 17; blastOutCon c1; blastOutCon c2;
				      blastOutExp e)
	       | INJ {sumtype, field, inject} =>
		     (blastOutChoice 18; blastOutCon sumtype; 
		      blastOutChoice field;
		      blastOutOption blastOutExp inject)
	       | CASE {sumtype, arg, bound, arms, tipe, default} => 
		      (blastOutChoice 19;
		       blastOutCon sumtype;
		       blastOutExp arg;
		       blastOutVar bound;
		       blastOutList (blastOutOption blastOutExp) arms;
		       blastOutCon tipe;
		       blastOutOption blastOutExp default)
	       | EXN_CASE {arg, arms, default, tipe} => 
		      (blastOutChoice 20;
		       blastOutExp arg;
		       blastOutList (blastOutTriple blastOutExp blastOutCon blastOutExp) arms;
		       blastOutOption blastOutExp default;
		       blastOutCon tipe)
	       | MODULE_PROJECT (m,l) => (blastOutChoice 21; blastOutMod m; blastOutLabel l)
	       | SEAL(e,c) => (blastOutChoice 22; blastOutExp e; blastOutCon c))

        and blastInExp () = 
	    let val _ = push()
		val _ = tab "blastInExp\n" 
		val res = blastInExp' ()
		val _ = pop()
	    in  res
	    end

	and blastInExp' () =
	     (case (blastInChoice()) of
	         0 => (tab "  SCON\n"; 
		       SCON(blastInValue ()))
	       | 1 => (tab "  PRIM\n"; 
		       PRIM (blastInPrim (),
			    blastInList blastInCon,
			    blastInList blastInExp))
	       | 2 => ILPRIM (blastInIlPrim (),
			      blastInList blastInCon,
			      blastInList blastInExp)
	       | 3 => ETAPRIM (blastInPrim (),
			       blastInList blastInCon)
	       | 4 => ETAILPRIM (blastInIlPrim (),
				 blastInList blastInCon)
	       | 5 => let val _ = tab "  VAR"
			  val v = blastInVar ()
			  val _ = (say (Name.var2string v); say "\n")
		      in  VAR v
		      end
	       | 6 => APP (blastInExp (), blastInExp ())
	       | 23 => EXTERN_APP (blastInCon (),
				   blastInExp (), 
				   blastInList blastInExp)
	       | 7 => FIX (blastInBool (), blastInArrow (), blastInList blastInFbnd)
	       | 8 => RECORD (blastInList (fn () => blastInPair blastInLabel blastInExp))
	       | 9 => RECORD_PROJECT (blastInExp (), blastInLabel (), blastInCon ())
	       | 10 => SUM_TAIL (blastInChoice(), blastInCon (), blastInExp ())
	       | 11 => HANDLE (blastInExp (), blastInExp ())
	       | 12 => RAISE (blastInCon (), blastInExp ())
	       | 13 => LET(blastInList blastInBnd, blastInExp ())
	       | 14 => NEW_STAMP (blastInCon ())
	       | 15 => EXN_INJECT (blastInString (), blastInExp (), blastInExp ())
	       | 16 => ROLL (blastInCon (), blastInExp ())
	       | 17 => UNROLL (blastInCon (), blastInCon (), blastInExp ())
	       | 18 => let val sumtype = blastInCon ()
			   val field = blastInChoice()
			   val inject = blastInOption blastInExp
		       in  INJ {sumtype = sumtype,
				field = field,
				inject = inject}
		       end
	       | 19 => (tab "  CASE\n";
			CASE {sumtype = blastInCon (),
			      arg = blastInExp (), 
			      bound = blastInVar (),
			      arms = blastInList (fn() => blastInOption blastInExp),
			      tipe = blastInCon (), 
			      default = blastInOption blastInExp})
	       | 20 => (tab "  EXN_CASE\n";
			EXN_CASE {arg = blastInExp (), 
				 arms = blastInList (fn() => blastInTriple blastInExp blastInCon blastInExp),
				 default = blastInOption blastInExp, 
				 tipe = blastInCon ()})
	       | 21 => MODULE_PROJECT (blastInMod (), blastInLabel ())
	       | 22 => SEAL(blastInExp (), blastInCon ())
	       | _ => error "bad blastInExp")

	and blastOutFbnd (FBND(v1,v2,c1,c2,e)) = (blastOutVar v1;
						     blastOutVar v2;
						     blastOutCon c1;
						     blastOutCon c2;
						     blastOutExp e)
	and blastInFbnd () = let val _ = tab "blastInFbnd"
				 val v = blastInVar ()
				 val _ = (say (Name.var2string v); say "\n")    
				 val _ = push()
				 val res = FBND(v, blastInVar (),
						blastInCon (), blastInCon (), blastInExp ())
				 val _ = pop()
			     in  res
			     end
				  

	and blastOutMod m = 
	    (case m of
		 MOD_VAR v => (blastOutChoice 0; blastOutVar v)
	       | MOD_STRUCTURE sbnds => (blastOutChoice 1; blastOutSbnds sbnds)
	       | MOD_FUNCTOR (v,s1,m,s2) => (blastOutChoice 2; blastOutVar v; 
					     blastOutSig s1; blastOutMod m;
					     blastOutSig s2)
	       | MOD_APP (m1,m2) => (blastOutChoice 3; blastOutMod m1; blastOutMod m2)
	       | MOD_PROJECT (m,l) => (blastOutChoice 4; blastOutMod m; blastOutLabel l)
	       | MOD_SEAL (m,s) => (blastOutChoice 5; blastOutMod m; blastOutSig s)
	       | MOD_LET (v,m1,m2) => (blastOutChoice 6; blastOutVar v; blastOutMod m1; blastOutMod m2))

        and blastInMod () = 
	    let val _ = push()
		val _ = tab "blastInMod\n"
		val res = blastInMod' ()
		val _ = pop()
	    in  res
	    end

	and blastInMod' () =
	    (
		     case (blastInChoice()) of
		 0 => MOD_VAR(blastInVar ())
	       | 1 => MOD_STRUCTURE(blastInSbnds ())
	       | 2 => MOD_FUNCTOR(blastInVar (), blastInSig (), blastInMod (), blastInSig ())
	       | 3 => MOD_APP (blastInMod (), blastInMod ())
	       | 4 => MOD_PROJECT (blastInMod (), blastInLabel ())
	       | 5 => MOD_SEAL (blastInMod (), blastInSig ())
	       | 6 => MOD_LET (blastInVar (), blastInMod (), blastInMod ())
	       | _ => error "bad blastInMod")

	and blastOutSig s = 
		 (tab "    blastInSig\n"; 
		  case s of
		 SIGNAT_STRUCTURE (NONE, sdecs) => (blastOutChoice 0; blastOutSdecs sdecs)
	       | SIGNAT_STRUCTURE (SOME p, sdecs) => (blastOutChoice 1; blastOutPath p; blastOutSdecs sdecs)
	       | SIGNAT_FUNCTOR(v, s1, s2, arrow) => (blastOutChoice 2; blastOutVar v;
						      blastOutSig s1; blastOutSig s2; blastOutArrow arrow)
	       | SIGNAT_INLINE_STRUCTURE {self=NONE,code,abs_sig} => 
		     (blastOutChoice 3; blastOutSbnds code; blastOutSdecs abs_sig)
	       | SIGNAT_INLINE_STRUCTURE {self=SOME p,code,abs_sig} => 
		     (blastOutChoice 4; blastOutPath p;
		      blastOutSbnds code; blastOutSdecs abs_sig)
	       | SIGNAT_VAR v => (blastOutChoice 5; blastOutVar v)
	       | SIGNAT_OF m => (blastOutChoice 6; blastOutMod m))

	and blastInSig () =
	    (case (blastInChoice()) of
		 0 => SIGNAT_STRUCTURE (NONE, blastInSdecs ())
	       | 1 => SIGNAT_STRUCTURE (SOME (blastInPath ()), blastInSdecs ())
	       | 2 => SIGNAT_FUNCTOR(blastInVar (), blastInSig (), blastInSig (), blastInArrow ())
	       | 3 => SIGNAT_INLINE_STRUCTURE {self=NONE, code = blastInSbnds (), 
					       abs_sig = blastInSdecs ()}
	       | 4 => SIGNAT_INLINE_STRUCTURE {self=SOME(blastInPath ()), code = blastInSbnds (), 
					       abs_sig = blastInSdecs ()}
	       | 5 => SIGNAT_VAR(blastInVar ())
	       | 6 => SIGNAT_OF(blastInMod ())
	       | _ => error "bad blastInSig")
				     
	fun blastOutPC pc = 
	    case pc of
		PHRASE_CLASS_EXP (e,c) => (blastOutChoice 0; blastOutExp e; blastOutCon c)
	      | PHRASE_CLASS_CON (c,k) => (blastOutChoice 1; blastOutCon c; blastOutKind k)
	      | PHRASE_CLASS_MOD (m,s) => (blastOutChoice 2; blastOutMod m; blastOutSig s)
	      | PHRASE_CLASS_SIG (v,s) => (blastOutChoice 3; blastOutVar v; blastOutSig s)
	      | PHRASE_CLASS_OVEREXP celist => (blastOutChoice 4; 
						blastOutList (blastOutPair blastOutCon blastOutExp) celist)

	fun blastInPC () = 
	    (tab "  blastInPC\n"; 
	    case (blastInChoice()) of
		0 => PHRASE_CLASS_EXP(blastInExp (), blastInCon ())
	      | 1 => PHRASE_CLASS_CON(blastInCon (), blastInKind ())
	      | 2 => PHRASE_CLASS_MOD(blastInMod (), blastInSig ())
	      | 3 => PHRASE_CLASS_SIG(blastInVar (), blastInSig ())
	      | 4 => PHRASE_CLASS_OVEREXP(blastInList (fn() => blastInPair blastInCon blastInExp))
	      | _ => error "bad blastInPC")

	fun blastOutFixity Fixity.NONfix = blastOutChoice 0
	  | blastOutFixity (Fixity.INfix (m,n)) = (blastOutChoice 1; 
						      blastOutChoice m; blastOutChoice n)
	fun blastInFixity () =
	    if (blastInChoice() = 0) 
		then Fixity.NONfix 
	    else let val m = blastInChoice()
		     val n = blastInChoice()
		 in  Fixity.INfix (m,n)
		 end

	fun blastOutFixityTable ft = 
	    blastOutList (blastOutPair blastOutLabel blastOutFixity) ft
	fun blastInFixityTable () = 
	    blastInList (fn() => blastInPair blastInLabel blastInFixity) 


	fun blastOutLabelList label_list = 
	    NameBlast.blastOutLabelmap (curOut()) (fn _ => blastOutPair blastOutPath blastOutPC) label_list
	fun blastInLabelList () = 
	    (tab "blastLabelList\n"; 
	    NameBlast.blastInLabelmap (curIn()) (fn _ => blastInPair blastInPath blastInPC))

	fun blastOutVarList (vmap,vlist) = 
	    (NameBlast.blastOutVarmap (curOut()) (fn _ => blastOutPair blastOutLabel blastOutPC) vmap;
	     blastOutList blastOutVar vlist)

	fun blastInVarList () = 
	    let val _ = tab "blastInVarList\n";
		val vmap = NameBlast.blastInVarmap (curIn()) (fn _ => blastInPair blastInLabel blastInPC) 
		val vlist = blastInList blastInVar 
	    in  (vmap, vlist)
	    end

	fun blastOutTagList tag_list = NameBlast.blastOutTagmap (curOut()) (fn _ => blastOutCon) tag_list
	fun blastInTagList () = NameBlast.blastInTagmap (curIn()) (fn _ => blastInCon())

    fun blastOutContext os (CONTEXT {flatlist, fixity_list, label_list, var_list, tag_list, alias_list}) = 
	(cur_out := (SOME os);
	 if Name.LabelMap.numItems alias_list = 0
	     then ()
	 else error "Blasting out context with non-empty alias_list";
	 blastOutFixityTable fixity_list;
	 blastOutLabelList label_list;
	 blastOutVarList var_list;
	 blastOutTagList tag_list)

    fun blastInContext is = 
	let val _ = cur_in := (SOME is)
	    val fixity_list = blastInFixityTable ()
	    val label_list = blastInLabelList ()
	    val var_list = blastInVarList ()
	    val tag_list = blastInTagList ()
	in CONTEXT {flatlist = [], fixity_list = fixity_list, label_list = label_list, 
		    var_list = var_list, tag_list = tag_list, alias_list = Name.LabelMap.empty}
	end




	(* alpha-conversion () necessary when checking contexts for
	 * equality.  This () done by explicitly maintaining a `var
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
				    SOME (l,_) => (* () this too conservative? *)
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
	  | eq_mod' (vm,MOD_FUNCTOR(v1,s1,m1,s1'),MOD_FUNCTOR(v2,s2,m2,s2')) = 
	    eq_signat (vm,s1,s2) andalso eq_mod(VM.add(v1,v2,vm),m1,m2) andalso
	    eq_signat (vm,s1',s2')
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
		     eq_cons(vm,cons,cons') andalso eq_con(vm,con,con')
		     andalso b=b' 
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
	    in Listops.andfold 
		(fn (SDEC(l,dec)) =>
			case sdecs_lookup(sdecs',l)
			  of SOME dec' => eq_dec(vm,dec,dec')
			   | NONE => false) sdecs
	    end	    

	and eq_sdecs arg = wrap "eq_sdecs" eq_sdecs' arg

	and eq_sbnds'(vm,sbnds,sbnds') =
	    let	val vm = extend_vm_sbnds(sbnds,sbnds',vm)
	    in Listops.andfold
		(fn (SBND(l,bnd)) =>
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
	       | _ => false                        (* MEMO: () this right?? *)
 
	and eq_bnd(vm,bnd,bnd') =
	    case (bnd, bnd')
	      of (BND_EXP(v,exp),BND_EXP(v',exp')) => 
		  VM.eq_var(vm,v,v') andalso eq_exp(vm,exp,exp')
	       | (BND_MOD(v,m), BND_MOD(v',m')) => 
	          VM.eq_var(vm,v,v') andalso eq_mod(vm,m,m')
               | (BND_CON(v,c), BND_CON(v',c')) =>
		  VM.eq_var(vm,v,v') andalso eq_con(vm,c,c')
	       | _ => false                        (* MEMO: () this right?? *)

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
		val res = Listops.andfold folder vars
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


