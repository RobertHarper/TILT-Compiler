(*$import Il IlContext IlUtil Ppil Blaster NameBlast Word8 BinIO ILCONTEXTEQ Bool Stats Util *)

structure IlContextEq 
    :> ILCONTEXTEQ =
struct

    open Util
    open IlContext
    open Il

    fun error s = Util.error "IlContextEq" s
    val error = fn str => Util.error "ilcontexteq.sml" str
    val debug = Stats.ff("IlcontexteqDebug")
    val blast_debug = Stats.ff("BlastDebug")

    (* The mapping of integers to variables is used to alpha-vary variables on input. *)
    structure IntKey =
	struct
	    type ord_key = int
	    val compare = Int.compare
	end
    structure IntMap = SplayMapFn(IntKey)

    type inState = BinIO.instream * Name.var IntMap.map ref
    type outState = BinIO.outstream

    fun makeInState ins = (ins, ref IntMap.empty)
    fun makeOutState outs = outs

    fun int2var (_, mapRef) i = 
	(case IntMap.find(!mapRef,i) of
	     SOME v => v
	   | NONE => let val v = Name.fresh_var()
			 val _ = mapRef := IntMap.insert(!mapRef,i,v)
		     in  v
		     end)

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
    fun blastOutInt outState i = Blaster.blastOutInt outState i
    fun blastInInt (inState : inState) = Blaster.blastInInt (#1 inState)
    fun blastOutBool outState i = Blaster.blastOutBool outState i
    fun blastInBool (inState : inState) = Blaster.blastInBool (#1 inState)
    fun blastOutString outState i = Blaster.blastOutString outState i
    fun blastInString (inState : inState) = Blaster.blastInString (#1 inState)
    fun blastOutWord64 outState i = Blaster.blastOutWord64 outState i
    fun blastInWord64 (inState : inState) = Blaster.blastInWord64 (#1 inState)
    val blastOutList = Blaster.blastOutList
    fun blastInList blaster (inState : inState) = Blaster.blastInList (fn _ => blaster inState) (#1 inState) 
    val blastOutOption = Blaster.blastOutOption
    fun blastInOption blaster (inState : inState) = Blaster.blastInOption (fn _ => blaster inState) (#1 inState) 
    val blastOutPair = Blaster.blastOutPair
    fun blastInPair b1 b2 (inState : inState) = Blaster.blastInPair (fn _ => b1 inState) (fn _ => b2 inState)
	                                             (#1 inState) 
    val blastOutTriple = Blaster.blastOutTriple
    fun blastInTriple b1 b2 b3 (inState : inState) = Blaster.blastInTriple (fn _ => b1 inState) (fn _ => b2 inState)
	                                               (fn _ => b3 inState)  (#1 inState) 

    fun blastOutVar outState v = NameBlast.blastOutVar outState v
    fun blastInVar (inState : inState) = NameBlast.blastInVar (int2var inState) (#1 inState)
    fun blastOutLabel outState l = NameBlast.blastOutLabel outState l
    fun blastInLabel (inState : inState) = NameBlast.blastInLabel (#1 inState)
    fun blastOutTag outState t = NameBlast.blastOutTag outState t
    fun blastInTag (inState : inState) = NameBlast.blastInTag (#1 inState)


    fun blastOutChoice outState i = BinIO.output1(outState,Word8.fromInt i)
    fun blastInChoice (inState : inState) = Word8.toInt(case BinIO.input1 (#1 inState) of
						NONE => error "blastInChoice failed"
					      | SOME w => w)

    fun blastOutPath outState (PATH (v,ls)) = (blastOutVar outState v;  blastOutList blastOutLabel outState ls)
    fun blastInPath (inState : inState) = PATH(blastInVar inState, blastInList blastInLabel inState)

    fun blastOutArrow outState arrow = blastOutChoice outState (case arrow of
								    TOTAL => 0
								  | PARTIAL => 1)
    fun blastInArrow inState =
	(case (blastInChoice inState) of
	     0 => TOTAL
	   | 1 => PARTIAL
	   | _ => error "bad blastInArrow")

    fun blastOutIS outState ws = 
	blastOutChoice outState
	(case ws of
	     Prim.W8 => 0
	   | Prim.W16 => 1
	   | Prim.W32 => 2
	   | Prim.W64 => 3)
    fun blastInIS inState = 
	(case blastInChoice inState of
	     0 => Prim.W8
	   | 1 => Prim.W16
	   | 2 => Prim.W32
	   | 3 => Prim.W64
	   | _ => (error "bad blastInIS" handle e => raise e))
    fun blastOutFS outState Prim.F32 = blastOutChoice outState 0
      | blastOutFS outState Prim.F64 = blastOutChoice outState 1
    fun blastInFS inState = 
	(case blastInChoice inState of
	     0 => Prim.F32
	   | 1 => Prim.F64
	   | _ => error "bad blastInFS")

    fun blastOutDec outState dec = 
	let val choice = blastOutChoice outState
	    val var = blastOutVar outState
	    val bool = blastOutBool outState
	    val con = blastOutCon outState
	in
	    case dec of
	     DEC_EXP (v,c,NONE, inline)   => (choice 0; var v; con c; bool inline)
	   | DEC_EXP (v,c,SOME e, inline) => (choice 1; var v; con c; blastOutExp outState e; 
					      bool inline)
	   | DEC_CON (v,k,NONE, inline)   => (choice 2; var v; blastOutKind outState k; bool inline)
	   | DEC_CON (v,k,SOME c, inline) => (choice 3; var v; blastOutKind outState k; con c; bool inline)
	   | DEC_MOD (v,b,s)              => (choice 4; var v; bool b; blastOutSig outState s)
	end

    and blastInDec inState = 
	(case (blastInChoice inState) of
	     0 => DEC_EXP (blastInVar  inState, blastInCon  inState, NONE, blastInBool inState)
	   | 1 => DEC_EXP (blastInVar  inState, blastInCon  inState, SOME (blastInExp inState), blastInBool inState)
	   | 2 => DEC_CON (blastInVar  inState, blastInKind  inState, NONE, blastInBool inState)
	   | 3 => DEC_CON (blastInVar  inState, blastInKind  inState, SOME (blastInCon  inState), blastInBool inState)
	   | 4 => DEC_MOD (blastInVar  inState, blastInBool  inState, blastInSig  inState)
	   | _ => error "bad blastInDec")

    and blastOutBnd outState bnd = 
	let val choice = blastOutChoice outState
	    val var = blastOutVar outState
	in  (case bnd of
		 BND_EXP (v,e) => (choice 0; var v; blastOutExp outState e)
	       | BND_CON (v,c) => (choice 1; var v; blastOutCon outState c)
	       | BND_MOD (v,b,m) => (choice 2; var v; blastOutBool outState b; blastOutMod outState m))
	end

    and blastInBnd  inState =
	(case (blastInChoice inState) of
	     0 => BND_EXP(blastInVar  inState, blastInExp  inState)
	   | 1 => BND_CON(blastInVar  inState, blastInCon  inState)
	   | 2 => BND_MOD(blastInVar  inState, blastInBool  inState, blastInMod  inState)
	   | _ => error "bad blastInBnd")

    and blastOutSdec outState (SDEC(l,dec)) = (blastOutLabel outState l; blastOutDec outState  dec)
    and blastInSdec  inState = SDEC(blastInLabel  inState, blastInDec  inState)
    and blastOutSbnd outState (SBND(l,bnd)) = (blastOutLabel outState l; blastOutBnd outState bnd)
    and blastInSbnd  inState = SBND(blastInLabel  inState, blastInBnd  inState)
	
    and blastOutSdecs outState sdecs = blastOutList blastOutSdec outState sdecs
    and blastInSdecs  inState = blastInList blastInSdec inState
    and blastOutSbnds outState sbnds = blastOutList blastOutSbnd outState sbnds
    and blastInSbnds  inState = blastInList blastInSbnd inState

    and blastOutKind outState k = 
	let val choice = blastOutChoice outState
	in  (case k of
		 KIND => choice 0
	       | KIND_TUPLE n => (choice 1; choice n)
	       | KIND_ARROW (m,k) => (choice 2; choice m; blastOutKind outState k))
	end
		    
    and blastInKind inState = 
	    (case blastInChoice inState of
		 0 => KIND
	       | 1 => KIND_TUPLE(blastInChoice inState)
	       | 2 => KIND_ARROW(blastInChoice inState, blastInKind inState)
	       | _ => error "bad blastInKind")
		 
    and blastOutCon outState c = 
	let val choice = blastOutChoice outState
	    val var = blastOutVar outState
	    val con = blastOutCon outState
	in (case c of
		 CON_VAR v => (choice 0; var v)
	       | CON_TYVAR tv => (case Tyvar.tyvar_deref tv of
				      SOME c => con c
				    | NONE => error "cannot blastOut unresolved CON_TYVAR")
	       | CON_OVAR oc => con (CON_TYVAR (Tyvar.ocon_deref oc))
	       | CON_FLEXRECORD (ref (INDIRECT_FLEXINFO r)) => con (CON_FLEXRECORD r)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, true, lclist))) => con (CON_RECORD lclist)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, false, _))) => error "cannot blastOut flex record type"
	       | CON_INT is => (choice 1; blastOutIS outState is)
	       | CON_UINT is => (choice 2; blastOutIS outState  is)
	       | CON_FLOAT fs => (choice 3; blastOutFS outState fs)
	       | CON_ARRAY c => (choice 4; con c)
	       | CON_VECTOR c => (choice 5; con c)
	       | CON_ANY => (choice 6)
	       | CON_REF c => (choice 7; con c)
	       | CON_TAG c => (choice 8; con c)
	       | CON_ARROW (cs,c,f,oa) => (choice 9; 
					   blastOutList blastOutCon outState cs;
					   con c; blastOutBool outState f;
					   blastOutArrow outState
					   (case (oneshot_deref oa) of
						SOME a => a
					      | _ => error "unresolved CON_ARROW"))
	       | CON_APP (c1,cargs) => (choice 10; con c1; blastOutList blastOutCon outState cargs)
	       | CON_MU c => (choice 11; con c)
	       | CON_RECORD lclist => (choice 12; blastOutList (blastOutPair blastOutLabel blastOutCon) outState lclist)
	       | CON_FUN (vlist, c) => (choice 13; blastOutList blastOutVar outState vlist; con c)
	       | CON_SUM {names, noncarriers, carrier, special = NONE} => 
		     (choice 14; 
		      blastOutList blastOutLabel outState names;
		      choice noncarriers; 
		      con carrier)
	       | CON_SUM {names, noncarriers, carrier, special = SOME i} => 
		     (choice 15; 
		      blastOutList blastOutLabel outState names;
		      choice noncarriers; 
		      con carrier; 
		      choice i)
	       | CON_TUPLE_INJECT clist => (choice 16; blastOutList blastOutCon outState clist)
	       | CON_TUPLE_PROJECT (i,c) => (choice 17; choice i; con c)
	       | CON_MODULE_PROJECT (m,l) => (choice 18; blastOutMod outState m; blastOutLabel outState l))
	end

	and blastInCon  inState = 
	    (case (blastInChoice inState) of
		 0 => CON_VAR (blastInVar  inState)
	       | 1 => CON_INT (blastInIS  inState)
	       | 2 => CON_UINT (blastInIS  inState)
	       | 3 => CON_FLOAT (blastInFS  inState)
	       | 4 => CON_ARRAY (blastInCon  inState)
	       | 5 => CON_VECTOR (blastInCon  inState)
	       | 6 => CON_ANY
	       | 7 => CON_REF (blastInCon  inState)
	       | 8 => CON_TAG (blastInCon  inState)
	       | 9 => let val cs = blastInList blastInCon inState
			  val c = blastInCon  inState
			  val f = blastInBool  inState
			  val a = oneshot_init (blastInArrow  inState)
		      in 
			  CON_ARROW (cs,c,f,a)
		      end
	       | 10 => CON_APP (blastInCon  inState, blastInList blastInCon inState)
	       | 11 => CON_MU (blastInCon  inState)
	       | 12 => CON_RECORD(blastInList (blastInPair blastInLabel blastInCon) inState)
	       | 13 => CON_FUN (blastInList blastInVar inState, blastInCon  inState)
	       | 14 => CON_SUM {names = blastInList blastInLabel inState,
				noncarriers = blastInChoice inState,
				carrier = blastInCon  inState,
				special = NONE} 
	       | 15 => CON_SUM {names = blastInList blastInLabel inState,
				noncarriers = blastInChoice inState,
				carrier = blastInCon  inState,
				special = SOME (blastInChoice inState)}
	       | 16 => CON_TUPLE_INJECT (blastInList blastInCon inState)
	       | 17 => CON_TUPLE_PROJECT (blastInChoice inState, blastInCon  inState)
	       | 18 => CON_MODULE_PROJECT (blastInMod  inState, blastInLabel  inState)
	       | _ => error "bad blastInCon")

    and blastOutValue outState v = 
	let val choice = blastOutChoice outState
	in  (case v of
		 (Prim.int (is,w64)) => (choice 0; blastOutIS outState is; blastOutWord64 outState w64)
	       | (Prim.uint (is,w64)) => (choice 1; blastOutIS outState is; blastOutWord64 outState w64)
	       | (Prim.float (fs,str)) => (choice 2; blastOutFS outState fs; blastOutString outState str)
	       | (Prim.tag (t,c)) => (choice 3; blastOutTag outState t; blastOutCon outState c)
	       | _ => error "blasting of array/vector/refcell not supported")
	end

    and blastInValue  inState =
	(case (blastInChoice inState) of
	     0 => Prim.int (blastInIS  inState, blastInWord64  inState)
	   | 1 => Prim.uint (blastInIS  inState, blastInWord64  inState)
	   | 2 => Prim.float (blastInFS  inState, blastInString  inState)
	   | 3 => Prim.tag (blastInTag  inState, blastInCon  inState)
	   | _ => error "bad blastInValue")

	and blastOutIlPrim outState ilprim = 
	    let open Prim
		val choice = blastOutChoice outState
		val IS = blastOutIS outState
	    in  (case ilprim of
		     eq_uint is => (choice 0; IS is)
		   | neq_uint is => (choice 1; IS is)
		   | not_uint is => (choice 2; IS is)
		   | and_uint is => (choice 3; IS is)
		   | or_uint is => (choice 4; IS is)
		   | xor_uint is => (choice 5; IS is)
		   | lshift_uint is => (choice 6; IS is)

		   | mk_ref => (choice 7)
		   | deref => (choice 8)
		   | eq_ref => (choice 9)
		   | setref => (choice 10))
	    end

	and blastInIlPrim  inState = 
	    let open Prim
	    in  (case (blastInChoice inState) of
		     0 => eq_uint(blastInIS  inState)
		   | 1 => neq_uint(blastInIS  inState)
		   | 2 => not_uint(blastInIS  inState)
		   | 3 => and_uint(blastInIS  inState)
		   | 4 => or_uint(blastInIS  inState)
		   | 5 => xor_uint(blastInIS  inState)
		   | 6 => lshift_uint(blastInIS  inState)

		   | 7 => mk_ref
		   | 8 => deref
		   | 9 => eq_ref
		   | 10 => setref
		   | _ => error "bad blastInIlPrim")
	    end


	and blastOutTT outState tt =
	    let open Prim 
		val choice = blastOutChoice outState
		val IS = blastOutIS outState
	    in  (case tt of
		     int_tt => choice 0
		   | real_tt => choice 1
		   | both_tt => choice 2)
	    end


	and blastInTT  inState =
	    let open Prim 
	    in  (case (blastInChoice inState) of
		     0 => int_tt
		   | 1 => real_tt
		   | 2 => both_tt
		   | _ => error "bad blastInTT")
	    end
			 
	and blastOutTable outState table = 
	    let open Prim 
		val choice = blastOutChoice outState
		val IS = blastOutIS outState
		val FS = blastOutFS outState
		val bool = blastOutBool outState
	    in  (case table of
		     IntArray is => (choice 0; IS is)
		   | IntVector is => (choice 1; IS is)
		   | FloatArray fs => (choice 2; FS fs)
		   | FloatVector fs => (choice 3; FS fs)
		   | OtherArray hnf => (choice 4; bool hnf)
		   | OtherVector hnf => (choice 5; bool hnf))
	    end

	and blastInTable  inState =
	    let open Prim 
	    in  (case (blastInChoice inState) of
		     0 => IntArray (blastInIS  inState)
		   | 1 => IntVector (blastInIS  inState)
		   | 2 => FloatArray (blastInFS  inState)
		   | 3 => FloatVector (blastInFS  inState)
		   | 4 => OtherArray (blastInBool  inState)
		   | 5 => OtherVector (blastInBool  inState)
		   | _ => error "bad blastInTable")
	    end

	and blastOutPrim outState prim = 
	    let open Prim
		val choice = blastOutChoice outState
		val IS = blastOutIS outState
		val FS = blastOutFS outState
		val TT = blastOutTT outState
		val table = blastOutTable outState
	    in  (case prim of
		     soft_vtrap tt => (choice 0; TT tt)
		   | soft_ztrap tt => (choice 1; TT tt)
		   | hard_vtrap tt => (choice 2; TT tt)
		   | hard_ztrap tt => (choice 3; TT tt)
			 
		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | float2int (* floor *) => (choice 6)
		   | int2float (* real  *) => (choice 7)
		   | int2uint (is1,is2) => (choice 8; IS is1; IS is2)
		   | uint2int (is1,is2) => (choice 9; IS is1; IS is2)
		   | uinta2uinta (is1,is2) => (choice 10; IS is1; IS is2)
		   | uintv2uintv (is1,is2) => (choice 11; IS is1; IS is2)
		   | int2int (is1,is2) => (choice 12; IS is1; IS is2)
		   | uint2uint (is1,is2) => (choice 13; IS is1; IS is2)

		   (* floatint-point operations *)	
		   | neg_float fs  => (choice 14; FS fs)
		   | abs_float fs  => (choice 15; FS fs)
		   | plus_float fs  => (choice 16; FS fs)
		   | minus_float fs  => (choice 17; FS fs)
		   | mul_float fs  => (choice 18; FS fs)
		   | div_float fs  => (choice 19; FS fs)
		   | less_float fs  => (choice 20; FS fs)
		   | greater_float fs  => (choice 21; FS fs)
		   | lesseq_float fs  => (choice 22; FS fs)
		   | greatereq_float  fs  => (choice 23; FS fs)
		   | eq_float  fs  => (choice 24; FS fs)
		   | neq_float fs  => (choice 25; FS fs)

		   (* int operations *)
		   | plus_int is  => (choice 26; IS is)
		   | minus_int is  => (choice 27; IS is)
		   | mul_int is  => (choice 28; IS is)
		   | div_int is  => (choice 29; IS is)
		   | mod_int is  => (choice 30; IS is)
		   | quot_int is  => (choice 31; IS is)
		   | rem_int is  => (choice 32; IS is)
		   | plus_uint is  => (choice 33; IS is)
		   | minus_uint is  => (choice 34; IS is)
		   | mul_uint is  => (choice 35; IS is)
		   | div_uint is  => (choice 36; IS is)
		   | mod_uint is  => (choice 37; IS is)
		   | less_int is  => (choice 38; IS is)
		   | greater_int is  => (choice 39; IS is)
		   | lesseq_int is  => (choice 40; IS is)
		   | greatereq_int is  => (choice 41; IS is)
		   | less_uint is  => (choice 42; IS is)
		   | greater_uint is  => (choice 43; IS is)
		   | lesseq_uint is  => (choice 44; IS is)
		   | greatereq_uint is  => (choice 45; IS is)
		   | eq_int is  => (choice 46; IS is)
		   | neq_int is  => (choice 47; IS is)
		   | neg_int is  => (choice 48; IS is)
		   | abs_int is  => (choice 49; IS is)

		   (* bit-pattern manipulation *)
		   | not_int is  => (choice 50; IS is)
		   | and_int is  => (choice 51; IS is)
		   | or_int is  => (choice 52; IS is)
		   | xor_int is  => (choice 53; IS is)
		   | lshift_int is  => (choice 54; IS is)
		   | rshift_int is  => (choice 55; IS is)
		   | rshift_uint is  => (choice 56; IS is)
			 
		   (* array and vectors *)
		   | array2vector t => (choice 57; table t)
		   | vector2array t => (choice 58; table t)
		   | create_table t => (choice 59; table t)
		   | create_empty_table t => (choice 60; table t)
		   | sub t => (choice 61; table t)
		   | update t => (choice 62; table t)
		   | length_table t => (choice 63; table t)
		   | equal_table t => (choice 64; table t))

	    end

	and blastInPrim  inState =
	    let open Prim
	    in  (case (blastInChoice inState) of
		     0 => soft_vtrap(blastInTT  inState)
		   | 1 => soft_ztrap(blastInTT  inState)
		   | 2 => hard_vtrap(blastInTT  inState)
		   | 3 => hard_ztrap(blastInTT  inState)
			 

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | 6 => float2int (* floor *)
		   | 7 => int2float (* real  *)
		   | 8 => int2uint (blastInIS  inState, blastInIS  inState)
		   | 9 => uint2int(blastInIS  inState, blastInIS  inState)
		   | 10 => uinta2uinta(blastInIS  inState, blastInIS  inState)
		   | 11 => uintv2uintv(blastInIS  inState, blastInIS  inState)
		   | 12 => int2int (blastInIS  inState, blastInIS  inState)
		   | 13 => uint2uint (blastInIS  inState, blastInIS  inState)

		   (* floatint-point operations *)	
		   | 14 => neg_float(blastInFS  inState)
		   | 15 => abs_float(blastInFS  inState)
		   | 16 => plus_float(blastInFS  inState)
		   | 17 => minus_float(blastInFS  inState)
		   | 18 => mul_float(blastInFS  inState)
		   | 19 => div_float(blastInFS  inState)
		   | 20 => less_float(blastInFS  inState)
		   | 21 => greater_float(blastInFS  inState)
		   | 22 => lesseq_float(blastInFS  inState)
		   | 23 => greatereq_float (blastInFS  inState)
		   | 24 => eq_float (blastInFS  inState)
		   | 25 => neq_float(blastInFS  inState)

		   (* int operations *)
		   | 26 => plus_int(blastInIS  inState)
		   | 27 => minus_int(blastInIS  inState)
		   | 28 => mul_int(blastInIS  inState)
		   | 29 => div_int(blastInIS  inState)
		   | 30 => mod_int(blastInIS  inState)
		   | 31 => quot_int(blastInIS  inState)
		   | 32 => rem_int(blastInIS  inState)
		   | 33 => plus_uint(blastInIS  inState)
		   | 34 => minus_uint(blastInIS  inState)
		   | 35 => mul_uint(blastInIS  inState)
		   | 36 => div_uint(blastInIS  inState)
		   | 37 => mod_uint(blastInIS  inState)
		   | 38 => less_int(blastInIS  inState)
		   | 39 => greater_int(blastInIS  inState)
		   | 40 => lesseq_int(blastInIS  inState)
		   | 41 => greatereq_int(blastInIS  inState)
		   | 42 => less_uint(blastInIS  inState)
		   | 43 => greater_uint(blastInIS  inState)
		   | 44 => lesseq_uint(blastInIS  inState)
		   | 45 => greatereq_uint(blastInIS  inState)
		   | 46 => eq_int(blastInIS  inState)
		   | 47 => neq_int(blastInIS  inState)
		   | 48 => neg_int(blastInIS  inState)
		   | 49 => abs_int(blastInIS  inState)

		   (* bit-pattern manipulation *)
		   | 50 => not_int(blastInIS  inState)
		   | 51 => and_int(blastInIS  inState)
		   | 52 => or_int(blastInIS  inState)
		   | 53 => xor_int(blastInIS  inState)
		   | 54 => lshift_int(blastInIS  inState)
		   | 55 => rshift_int(blastInIS  inState)
		   | 56 => rshift_uint(blastInIS  inState)
			 
		   (* array and vectors *)
		   | 57 => array2vector (blastInTable  inState)
		   | 58 => vector2array (blastInTable  inState)
		   | 59 => create_table (blastInTable  inState)
		   | 60 => create_empty_table (blastInTable  inState)
		   | 61 => sub (blastInTable  inState)
		   | 62 => update (blastInTable  inState)
		   | 63 => length_table (blastInTable  inState)
		   | 64 => equal_table (blastInTable  inState)

		   | _ => error "bad blastInPrim")

	    end

	and blastOutExp outState e = 
	    let val choice = blastOutChoice outState
		val exp = blastOutExp outState
		val con = blastOutCon outState
		val expList = blastOutList blastOutExp outState
		val conList = blastOutList blastOutCon outState
	    in
	    (case e of
		 OVEREXP (_,_,oe) => (case oneshot_deref oe of
					  SOME e => exp e
					| NONE => error "cannot blastOut unresolved OVEREXP")
	       | SCON v => (choice 0; blastOutValue outState v)
	       | PRIM (p,clist,elist) => (choice 1; blastOutPrim outState p;
					  conList clist; expList elist)
	       | ILPRIM (p,clist,elist) => (choice 2; blastOutIlPrim outState p;
					    conList clist; expList elist)
	       | ETAPRIM (p,clist) => (choice 3; blastOutPrim outState p; conList clist)
	       | ETAILPRIM (p,clist) => (choice 4; blastOutIlPrim outState p; conList clist)
	       | VAR v => (choice 5; blastOutVar outState v)
	       | APP (e1,e2) => (choice 6; exp e1; exp e2)
	       | EXTERN_APP (c,e,elist) => (choice 23; con c; exp e; expList elist)
	       | FIX (b,a,fbnds) => (choice 7; blastOutBool outState b; blastOutArrow outState a;
				     blastOutList blastOutFbnd outState fbnds)
	       | RECORD lelist => (choice 8; blastOutList (blastOutPair blastOutLabel blastOutExp) outState lelist)
	       | RECORD_PROJECT (e,l,c) => (choice 9; exp e; blastOutLabel outState l; con c)
	       | SUM_TAIL (i,c,e) => (choice 10; choice i; con c; exp e)
	       | HANDLE (c,e1,e2) => (choice 11; con c; exp e1; exp e2)
	       | RAISE (c,e) => (choice 12; con c; exp e)
	       | LET(bnds,e) => (choice 13; blastOutList blastOutBnd outState bnds; exp e)
	       | NEW_STAMP c => (choice 14; con c)
	       | EXN_INJECT (str,e1,e2) => (choice 15; blastOutString outState str; exp e1; exp e2)
	       | ROLL (c,e) => (choice 16; con c; exp e)
	       | UNROLL (c1,c2,e) => (choice 17; con c1; con c2; exp e)
	       | INJ {sumtype, field, inject} =>
		     (choice 18; con sumtype; 
		      choice field;
		      blastOutOption blastOutExp outState inject)
	       | CASE {sumtype, arg, bound, arms, tipe, default} => 
		      (choice 19;
		       con sumtype;
		       exp arg;
		       blastOutVar outState bound;
		       blastOutList (blastOutOption blastOutExp) outState arms;
		       con tipe;
		       blastOutOption blastOutExp outState default)
	       | EXN_CASE {arg, arms, default, tipe} => 
		      (choice 20;
		       exp arg;
		       blastOutList (blastOutTriple blastOutExp blastOutCon blastOutExp) outState arms;
		       blastOutOption blastOutExp outState default;
		       con tipe)
	       | MODULE_PROJECT (m,l) => (choice 21; blastOutMod outState m; blastOutLabel outState l)
	       | SEAL(e,c) => (choice 22; exp e; con c))
	    end

	and blastInExp  inState =
	     (case (blastInChoice inState) of
	         0 => SCON(blastInValue  inState)
	       | 1 => PRIM (blastInPrim  inState,
			    blastInList blastInCon inState,
			    blastInList blastInExp inState)
	       | 2 => ILPRIM (blastInIlPrim  inState,
			      blastInList blastInCon inState,
			      blastInList blastInExp inState)
	       | 3 => ETAPRIM (blastInPrim  inState,
			       blastInList blastInCon inState)
	       | 4 => ETAILPRIM (blastInIlPrim  inState,
				 blastInList blastInCon inState)
	       | 5 => VAR (blastInVar  inState)
	       | 6 => APP (blastInExp  inState, blastInExp  inState)
	       | 23 => EXTERN_APP (blastInCon  inState,
				   blastInExp  inState, 
				   blastInList blastInExp inState)
	       | 7 => FIX (blastInBool  inState, blastInArrow  inState, blastInList blastInFbnd inState)
	       | 8 => RECORD (blastInList (blastInPair blastInLabel blastInExp) inState)
	       | 9 => RECORD_PROJECT (blastInExp  inState, blastInLabel  inState, blastInCon  inState)
	       | 10 => SUM_TAIL (blastInChoice inState, blastInCon  inState, blastInExp  inState)
	       | 11 => HANDLE (blastInCon inState, blastInExp  inState, blastInExp  inState)
	       | 12 => RAISE (blastInCon  inState, blastInExp  inState)
	       | 13 => LET(blastInList blastInBnd inState, blastInExp  inState)
	       | 14 => NEW_STAMP (blastInCon  inState)
	       | 15 => EXN_INJECT (blastInString  inState, blastInExp  inState, blastInExp  inState)
	       | 16 => ROLL (blastInCon  inState, blastInExp  inState)
	       | 17 => UNROLL (blastInCon  inState, blastInCon  inState, blastInExp  inState)
	       | 18 => let val sumtype = blastInCon  inState
			   val field = blastInChoice inState
			   val inject = blastInOption blastInExp inState
		       in  INJ {sumtype = sumtype,
				field = field,
				inject = inject}
		       end
	       | 19 => (CASE {sumtype = blastInCon  inState,
			      arg = blastInExp  inState, 
			      bound = blastInVar  inState,
			      arms = blastInList (blastInOption blastInExp) inState,
			      tipe = blastInCon  inState, 
			      default = blastInOption blastInExp inState})
	       | 20 => (EXN_CASE {arg = blastInExp  inState, 
				 arms = blastInList (blastInTriple blastInExp blastInCon blastInExp) inState,
				 default = blastInOption blastInExp inState, 
				 tipe = blastInCon  inState})
	       | 21 => MODULE_PROJECT (blastInMod  inState, blastInLabel  inState)
	       | 22 => SEAL(blastInExp  inState, blastInCon  inState)
	       | _ => error "bad blastInExp")

	and blastOutFbnd outState (FBND(v1,v2,c1,c2,e)) = 
	    let val var = blastOutVar outState
		val con = blastOutCon outState
	    in  var v1; var v2; con c1; con c2; blastOutExp outState e
	    end

	and blastInFbnd  inState = 
	    FBND(blastInVar  inState, blastInVar  inState,
		 blastInCon  inState, blastInCon  inState, blastInExp  inState)

	and blastOutMod outState m = 
	    let val choice = blastOutChoice outState
		val mod = blastOutMod outState
		val var = blastOutVar outState
		val signat = blastOutSig outState
	    in
		(case m of
		     MOD_VAR v => (choice 0; var v)
		   | MOD_STRUCTURE sbnds => (choice 1; blastOutSbnds outState sbnds)
		   | MOD_FUNCTOR (a,v,s1,m,s2) => (choice 2; 
						   blastOutArrow outState a; var v; 
						   signat s1; mod m;
						   signat s2)
		   | MOD_APP (m1,m2) => (choice 3; mod m1; mod m2)
		   | MOD_PROJECT (m,l) => (choice 4; mod m; blastOutLabel outState l)
		   | MOD_SEAL (m,s) => (choice 5; mod m; signat s)
		   | MOD_LET (v,m1,m2) => (choice 6; var v; mod m1; mod m2))
	    end

	and blastInMod  inState =
	    (case (blastInChoice inState) of
		 0 => MOD_VAR(blastInVar  inState)
	       | 1 => MOD_STRUCTURE(blastInSbnds  inState)
	       | 2 => MOD_FUNCTOR(blastInArrow inState, blastInVar  inState, 
				  blastInSig  inState, blastInMod  inState, blastInSig  inState)
	       | 3 => MOD_APP (blastInMod  inState, blastInMod  inState)
	       | 4 => MOD_PROJECT (blastInMod  inState, blastInLabel  inState)
	       | 5 => MOD_SEAL (blastInMod  inState, blastInSig  inState)
	       | 6 => MOD_LET (blastInVar  inState, blastInMod  inState, blastInMod  inState)
	       | _ => error "bad blastInMod")

	and blastOutSig (outState : outState) s = 
	    let val signat = blastOutSig outState
		val var = blastOutVar outState
		val choice = blastOutChoice outState
	    in  (case s of
		 SIGNAT_STRUCTURE sdecs => (choice 0; blastOutSdecs outState sdecs)
	       | SIGNAT_SELF(self, unselfSigOpt, selfSig) => (choice 1; blastOutPath outState self; 
							      blastOutOption blastOutSig outState unselfSigOpt; 
							      signat selfSig)
	       | SIGNAT_FUNCTOR(v, s1, s2, arrow) => (choice 2; var v;
						      signat s1; signat s2; blastOutArrow outState arrow)
	       | SIGNAT_VAR v => (choice 5; var v)
	       | SIGNAT_OF m => (choice 6; blastOutPath outState m))
	    end

	and blastInSig  inState =
	    (case (blastInChoice inState) of
		 0 => SIGNAT_STRUCTURE (blastInSdecs  inState)
	       | 1 => SIGNAT_SELF (blastInPath  inState, blastInOption blastInSig inState, blastInSig  inState)
	       | 2 => SIGNAT_FUNCTOR(blastInVar  inState, blastInSig  inState, blastInSig  inState, blastInArrow  inState)
	       | 5 => SIGNAT_VAR(blastInVar  inState)
	       | 6 => SIGNAT_OF(blastInPath  inState)
	       | _ => error "bad blastInSig")
				
	fun blastOutCelist celist = blastOutList (blastOutPair blastOutCon blastOutExp) celist
        fun blastInCelist inState = blastInList (blastInPair blastInCon blastInExp) inState

	fun blastOutPC outState pc = 
	    let val choice = blastOutChoice outState
		val con = blastOutCon outState
		val bool = blastOutBool outState
	    in
	    case pc of
		PHRASE_CLASS_EXP (e,c,eopt,inline) => (choice 0; blastOutExp outState e; 
						       con c; blastOutOption blastOutExp outState eopt;
						       bool inline)
	      | PHRASE_CLASS_CON (c,k,copt,inline) => (choice 1; con c; blastOutKind outState k; 
						       blastOutOption blastOutCon outState copt; 
						       bool inline)
	      | PHRASE_CLASS_MOD (m,b,s) => (choice 2; blastOutMod outState m; bool b;
					     blastOutSig outState s)
	      | PHRASE_CLASS_SIG (v,s) => (choice 3; blastOutVar outState v; blastOutSig outState s)
	    end

	fun blastInPC  inState = 
	    (case (blastInChoice inState) of
		0 => PHRASE_CLASS_EXP(blastInExp  inState, blastInCon  inState, 
				      blastInOption blastInExp inState, blastInBool inState)
	      | 1 => PHRASE_CLASS_CON(blastInCon  inState, blastInKind  inState, 
				      blastInOption blastInCon inState, blastInBool inState)
	      | 2 => PHRASE_CLASS_MOD(blastInMod  inState, blastInBool  inState, blastInSig  inState)
	      | 3 => PHRASE_CLASS_SIG(blastInVar  inState, blastInSig  inState)
	      | _ => error "bad blastInPC")

	fun blastOutFixity outState fixity = 
	    let val choice = blastOutChoice outState
	    in  case fixity of
		Fixity.NONfix => choice 0
	      | Fixity.INfix (m,n) => (choice 1; choice m; choice n)
	    end

	fun blastInFixity  inState =
	    if (blastInChoice inState = 0) 
		then Fixity.NONfix 
	    else let val m = blastInChoice inState
		     val n = blastInChoice inState
		 in  Fixity.INfix (m,n)
		 end

	fun blastOutFixityMap outState fm = 
	    NameBlast.blastOutLabelmap outState blastOutFixity fm
	fun blastInFixityMap  inState = 
	    NameBlast.blastInLabelmap (#1 inState) (fn _ => blastInFixity inState)

	fun blastOutOverloadMap outState fm = 
	    NameBlast.blastOutLabelmap  outState blastOutCelist  fm
	fun blastInOverloadMap  inState = 
	    NameBlast.blastInLabelmap (#1 inState) (fn _ => blastInCelist inState)

	fun blastOutPathMap outState label_list = 
	    NameBlast.blastOutPathmap outState (blastOutPair blastOutLabel blastOutPC) label_list
	fun blastInPathMap  inState = 
	    NameBlast.blastInPathmap (int2var inState) (#1 inState) (fn _ => blastInPair blastInLabel blastInPC inState)

	fun reconstructLabelMap pathMap = 
	    let fun folder(p,(l,pc),pmap) = Name.LabelMap.insert(pmap,l,(PATH p,pc))
	    in  Name.PathMap.foldli folder Name.LabelMap.empty pathMap 
	    end

	fun blastOutUnresolved outState unresolved = 
	    NameBlast.blastOutVarmap outState blastOutLabel unresolved

	fun blastInUnresolved  inState : Name.label Name.VarMap.map =
	    NameBlast.blastInVarmap (int2var inState) (#1 inState) (fn _ => blastInLabel inState)

    fun blastOutContext outState (CONTEXT {fixityMap, overloadMap, labelMap, pathMap, ordering}) = 
	(blastOutFixityMap outState fixityMap;
         blastOutOverloadMap outState overloadMap;
	 blastOutPathMap outState pathMap;
	 blastOutList blastOutPath outState ordering)

    fun blastInContext  ins = 
	let val inState = makeInState ins
	    val fixityMap = blastInFixityMap  inState
	    val overloadMap = blastInOverloadMap inState	 
	    val pathMap = blastInPathMap  inState
	    val ordering = blastInList blastInPath inState
	    val labelMap = reconstructLabelMap pathMap
	in CONTEXT {fixityMap = fixityMap, overloadMap = overloadMap,
		    labelMap = labelMap, pathMap = pathMap, ordering = ordering}
	end

    fun blastOutPartialContext out (ctxt, unresolved) = 
	(blastOutContext out ctxt;
	 blastOutUnresolved out unresolved)

    fun blastInPartialContext  ins = 
	let val ctxt = blastInContext ins
	    val inState = makeInState ins
	    val unresolved = blastInUnresolved inState
	in  (ctxt, unresolved)
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
		   fun show_vm vm = 
		       Name.VarMap.appi (fn (v,v') => (Ppil.pp_var v; print " -> ";
						       Ppil.pp_var v'; print "\n")) vm
	    end

	fun extend_vm_unresolved (ur, ur' : label Name.VarMap.map, vm, vlist) : vm * var list =
	    let val reverse = Name.VarMap.foldli 
		                   (fn (v,l,lm) => Name.LabelMap.insert(lm,l,v)) 
		                   Name.LabelMap.empty ur'
		fun folder (v, l, (vm, vlist)) =
		      case Name.LabelMap.find(reverse,l) of
			   SOME v' => (VM.add(v,v',vm), v :: vlist)
			 | NONE => (print "label "; Ppil.pp_label l;
				    print " not found in ur'\n"; 
				    raise NOT_EQUAL)
	    in  Name.VarMap.foldli folder (vm, vlist) ur
	    end

	fun extend_vm_context (c : context, c' : context, vm, vlist) : vm * var list =
	    let val ord = Context_Ordering c
		val ord' = Context_Ordering c'
		fun getVar(PATH(v,[])) = SOME v
		  | getVar _ = NONE
		val vlist = List.mapPartial getVar ord
		val vlist' = List.mapPartial getVar ord'
		fun mapper ctxt v = (case Context_Lookup_Var(ctxt,v) of
					 SOME (l,_) => SOME(l, v)
				       | NONE => (print "extend_vm_context: could not find var = ";
						  Ppil.pp_var v; print "\n";
						  error "extend_vm_context"))
		val lvlist = List.mapPartial (mapper c) vlist
		val lvlist' = List.mapPartial (mapper c') vlist'

		fun printLvlist name list =
		    (print (name ^ " = ");
		     List.app (fn (l, v) => (print "("; Ppil.pp_label l;
					     print ","; Ppil.pp_var v;
					     print ") :: ")) list;
		     print "nil\n")
		    
		val _ = if length lvlist <> length lvlist' 
			    then (print "extend_vm_contxt: lvlist length not equal\n";
				  if (!debug)
				      then (printLvlist "lvlist" lvlist;
					    printLvlist "lvlist'" lvlist')
				  else ();
				  raise NOT_EQUAL)
			else ()
		fun folder ((l,v), (vm,vlist)) =
		      case Context_Lookup_Label(c',l) of
			   SOME(PATH (v',_),_) => (VM.add(v,v',vm), v::vlist)
			 | NONE => (print "label "; Ppil.pp_label l;
				    print " not found in c'\n"; 
				    raise NOT_EQUAL)
	       (* Note use of foldr *)
	    in foldr folder (vm,vlist) lvlist
	    end

	fun sdecs_lookup([],l) = NONE
	  | sdecs_lookup(SDEC(l',dec)::sdecs,l) =
	    if Name.eq_label(l,l') then SOME dec else sdecs_lookup(sdecs,l)

	fun sbnds_lookup([],l) = NONE
	  | sbnds_lookup(SBND(l',bnd)::sbnds,l) =
	    if Name.eq_label(l,l') then SOME bnd else sbnds_lookup(sbnds,l)

	fun add_dec(DEC_EXP(v,_,_,_), DEC_EXP(v',_,_,_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_CON(v,_,_,_), DEC_CON(v',_,_,_), vm) = VM.add(v,v',vm)
	  | add_dec(DEC_MOD(v,_,_), DEC_MOD(v',_,_), vm) = VM.add(v,v',vm)
	  | add_dec _ = raise NOT_EQUAL

	fun add_bnd(BND_EXP(v,_), BND_EXP(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_MOD(v,_,_), BND_MOD(v',_,_), vm) = VM.add(v,v',vm)
	  | add_bnd(BND_CON(v,_), BND_CON(v',_), vm) = VM.add(v,v',vm)
	  | add_bnd _ = raise NOT_EQUAL

	fun extend_vm_sdecs(sdecs,sdecs',vm) : vm =
	    let val _ = if length sdecs <> length sdecs' then raise NOT_EQUAL else ()
	    in foldr (fn (SDEC(l,dec), vm) => case sdecs_lookup(sdecs',l)
		                                of SOME dec' => add_dec(dec,dec',vm)
					         | NONE => (print "XXX label mismatch ";
							    Ppil.pp_label l; print "\n";
							    raise NOT_EQUAL)) vm sdecs
	    end

	fun extend_vm_sbnds(sbnds,sbnds',vm) : vm =
	    let val _ = if length sbnds <> length sbnds' then raise NOT_EQUAL else ()
	    in foldr (fn (SBND(l,bnd), vm) => case sbnds_lookup(sbnds',l)
		                                of SOME bnd' => add_bnd(bnd,bnd',vm)
					         | NONE => (print "XXX label mismatch ";
							    Ppil.pp_label l; print "\n";
							    raise NOT_EQUAL)) vm sbnds
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

	fun eq_path(vm,PATH(v,lbls), PATH(v',lbls')) = VM.eq_var(vm,v,v') andalso eq_labels(lbls,lbls')

	fun eq_pathopt (vm,pathopt,pathopt') = 
	    eq_opt (fn (p1,p2) => eq_path(vm,p1,p2)) (pathopt,pathopt')

	val eq_path = wrap "eq_path" eq_path
	val eq_pathopt = wrap "eq_pathopt" eq_pathopt

	fun eq_mod' (vm,MOD_VAR v,MOD_VAR v') = VM.eq_var(vm,v,v')
	  | eq_mod' (vm,MOD_PROJECT(m1,l1),MOD_PROJECT(m2,l2)) = 
	    eq_mod(vm,m1,m2) andalso Name.eq_label(l1,l2)
	  | eq_mod' (vm,MOD_FUNCTOR(a1,v1,s1,m1,s1'),MOD_FUNCTOR(a2,v2,s2,m2,s2')) = 
	    a1 = a2 andalso
	    eq_signat (vm,s1,s2) andalso eq_mod(VM.add(v1,v2,vm),m1,m2) andalso
	    eq_signat (vm,s1',s2')
	  | eq_mod' (vm,MOD_STRUCTURE sbnds1,MOD_STRUCTURE sbnds2) = eq_sbnds(vm,sbnds1,sbnds2)
	  | eq_mod' _ = false

	and eq_mod arg = wrap "eq_mod" eq_mod' arg

	and eq_con(vm,con,con') =
	    let val res = case (con,con') of
	         (CON_VAR v, CON_VAR v') => VM.eq_var(vm,v,v')           
	       | (CON_TYVAR tv, _) => (case Tyvar.tyvar_deref tv of
					   NONE => error "eq_con on unresolved CON_TYVAR"
					 | SOME c => eq_con(vm, c, con'))
	       | (_, CON_TYVAR tv') => (case Tyvar.tyvar_deref tv' of
					   NONE => error "eq_con on unresolved CON_TYVAR"
					 | SOME c' => eq_con(vm, con, c'))
	       | (CON_OVAR ov, _) => eq_con(vm, CON_TYVAR (Tyvar.ocon_deref ov), con')
	       | (_, CON_OVAR ov') => eq_con(vm, con, CON_TYVAR (Tyvar.ocon_deref ov'))
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
	       | (CON_APP(con1,cons2), CON_APP(con1',cons2')) =>
		     eq_con(vm,con1,con1') andalso eq_cons(vm,cons2,cons2')
	       | (CON_MU(con), CON_MU(con')) => eq_con(vm,con,con')    
	       | (CON_RECORD labcons, CON_RECORD labcons') => eq_labcons(vm,labcons,labcons')        
	       | (CON_FUN(vars,con), CON_FUN(vars',con')) => 
		     (case eq_vars(vm,vars,vars') of
			  NONE => false
			| SOME vm => eq_con(vm,con,con'))
	       | (CON_SUM{names,noncarriers,carrier,special}, 
		  CON_SUM{names=names',noncarriers=noncarriers',carrier=carrier',special=special'}) =>
		     Listops.eq_list(Name.eq_label,names,names') andalso
		     noncarriers=noncarriers' andalso special=special' 
		     andalso eq_con(vm,carrier,carrier')
	       | (CON_TUPLE_INJECT cons, CON_TUPLE_INJECT cons') => 
		     eq_cons(vm,cons,cons')
	       | (CON_TUPLE_PROJECT(i,con), CON_TUPLE_PROJECT(i',con')) => 
		     i=i' andalso eq_con(vm,con,con')
	       | (CON_MODULE_PROJECT(mod,lab), CON_MODULE_PROJECT(mod',lab')) =>
		     eq_mod(vm,mod,mod') andalso Name.eq_label(lab,lab')
	       | _ => false
		val _ = if res then () 
		    else (print "XXX eq_con false - \ncon = ";
			  Ppil.pp_con con;
			  print "\ncon' = ";
			  Ppil.pp_con con'; print "\n")
	    in res
	    end
 

	and eq_cons(vm,[],[]) = true
	  | eq_cons(vm,con::cons,con'::cons') = eq_con(vm,con,con') andalso eq_cons(vm,cons,cons')
	  | eq_cons _ = false

	and eq_conopt(vm,SOME con,SOME con') = eq_con(vm,con,con')
	  | eq_conopt(vm,NONE,NONE) = true
	  | eq_conopt _ = false

	and eq_expopt(vm,SOME exp,SOME exp') = eq_exp(vm,exp,exp')
	  | eq_expopt(vm,NONE,NONE) = true
	  | eq_expopt _ = false

	and eq_kind(vm,kind,kind') = 
	    case (kind,kind') of
		 (KIND_TUPLE i, KIND_TUPLE i') => i=i'
	       | (KIND_ARROW (i,k), KIND_ARROW (i',k')) => i=i' andalso eq_kind(vm,k, k')
	       | (KIND, KIND) => true
	       | _ => false
 
	and eq_tyvar(vm,tv,tv') =
	    case (Tyvar.tyvar_deref tv, Tyvar.tyvar_deref tv')
	      of (SOME con, SOME con') => eq_con(vm,con,con')
	       | _ => false

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


	and eq_signat(vm,signat,signat') =
	  let val res = 
	    case (signat,signat') of
	         (SIGNAT_STRUCTURE sdecs, SIGNAT_STRUCTURE sdecs') =>
		     eq_sdecs(vm,sdecs,sdecs')
	       | (SIGNAT_SELF (p1, uso1, s1), SIGNAT_SELF(p2, uso2, s2)) =>
		  eq_path(vm,p1,p2) andalso eq_opt (fn(a,b) => eq_signat(vm,a,b)) (uso1,uso2) andalso eq_signat(vm,s1,s2)
	       | (SIGNAT_FUNCTOR(v,signat1,signat2,a), SIGNAT_FUNCTOR(v',signat1',signat2',a')) =>
		  eq_signat(vm,signat1,signat1') andalso a=a' andalso
		  eq_signat(VM.add(v,v',vm),signat2,signat2')
               | (SIGNAT_VAR v1, SIGNAT_VAR v2) => VM.eq_var(vm,v1,v2)
               | (SIGNAT_OF p1, SIGNAT_OF p2) => eq_path(vm,p1,p2)
               | _ => false
		val _ = if res then () 
		    else (print "XXX eq_signat false - \nsignat = ";
			  Ppil.pp_signat signat;
			  print "\nsignat' = ";
			  Ppil.pp_signat signat'; print "\n")
	  in  res
	  end

	and eq_sdecs'(vm,sdecs,sdecs') =
	    let	val vm = extend_vm_sdecs(sdecs,sdecs',vm)
		val res = Listops.andfold 
		    (fn (SDEC(l,dec)) =>
		     case sdecs_lookup(sdecs',l) of
			 SOME dec' => eq_dec(vm,dec,dec')
		       | NONE => (print "XXX eq_sdecs' returning false due to ";
				  Ppil.pp_label l; print "\n";
				  false)) sdecs
	    in  res
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
	    let val res = case (dec, dec')
	      of (DEC_EXP(v,con,exp,inline),DEC_EXP(v',con',exp',inline')) => 
		  VM.eq_var(vm,v,v') andalso eq_con(vm,con,con')  andalso 
		  eq_expopt(vm,exp,exp') andalso inline=inline'
	       | (DEC_MOD(v,b,signat), DEC_MOD(v',b',signat')) => 
	          VM.eq_var(vm,v,v') andalso (b = b') andalso eq_signat(vm,signat,signat')
               | (DEC_CON(v,kind,conopt,inline), DEC_CON(v',kind',conopt',inline')) =>
		  VM.eq_var(vm,v,v') andalso eq_kind(vm,kind,kind') andalso
		  eq_conopt(vm,conopt,conopt') andalso inline=inline'
	       | _ => false
		val _ = if res then () 
		    else (print "XXX eq_dec false - \ndec = ";
			  Ppil.pp_dec dec;
			  print "\ndec' = ";
			  Ppil.pp_dec dec'; print "\n")
	    in res
	    end
	and eq_bnd(vm,bnd,bnd') =
	    case (bnd, bnd')
	      of (BND_EXP(v,exp),BND_EXP(v',exp')) => 
		  VM.eq_var(vm,v,v') andalso eq_exp(vm,exp,exp')
	       | (BND_MOD(v,b,m), BND_MOD(v',b',m')) => 
	          VM.eq_var(vm,v,v') andalso (b = b') andalso eq_mod(vm,m,m')
               | (BND_CON(v,c), BND_CON(v',c')) =>
		  VM.eq_var(vm,v,v') andalso eq_con(vm,c,c')
	       | _ => false

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
	      of (PHRASE_CLASS_EXP(exp,con,eopt,inline), PHRASE_CLASS_EXP(exp',con',eopt',inline')) =>
		  eq_exp(vm,exp,exp') andalso eq_con(vm,con,con') andalso eq_expopt(vm,eopt,eopt')
		  andalso inline=inline'
	       | (PHRASE_CLASS_CON(con,kind,copt,inline), PHRASE_CLASS_CON(con',kind',copt',inline')) =>
		  eq_con(vm,con,con') andalso eq_kind(vm,kind,kind') andalso eq_conopt(vm,copt,copt')
		  andalso inline=inline'
	       | (PHRASE_CLASS_MOD(mod,b,signat), PHRASE_CLASS_MOD(mod',b',signat')) =>
		  eq_mod(vm,mod,mod') andalso (b = b') andalso eq_signat(vm,signat,signat')
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
			    (case (Context_Lookup_Var(c,v), Context_Lookup_Var(c',VM.lookup vm v)) of
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

	fun eq_partial_context ((c,u): partial_context, (c',u'): partial_context) : bool =
	    let 
		val (vm,vlist) = extend_vm_unresolved(u,u',VM.empty,[])
		val (vm,vlist) = extend_vm_context(c,c',vm,vlist)
(*		val _ = (print "vm = "; VM.show_vm vm; print "\n") 
		val _ =( print "c = "; Ppil.pp_context c; print "\n\n";
			print "c' = "; Ppil.pp_context c'; print "\n\n") *)
	    in eq_cntxt(vm,c,c',vlist)
	    end handle NOT_EQUAL => false


	fun eq_context (c: context, c': context) : bool = 
	    let val pc = (c, Name.VarMap.empty)
		val pc' = (c', Name.VarMap.empty)
	    in  eq_partial_context (pc, pc')
	    end

	val eq_con = 
	    fn (c: con, c': con) =>
	    let val vm = Name.VarMap.empty
	    in  eq_con (vm, c, c')
	    end

    end


