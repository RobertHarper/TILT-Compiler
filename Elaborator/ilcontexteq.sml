(*$import Prelude TopLevel Int Name Il SplayMapFn Prim Tyvar Fixity List Listops IlContext IlUtil Ppil Blaster NameBlast Word8 BinIO ILCONTEXTEQ Bool Stats Util IlTable IntListMap Option *)
(* Equality of contexts *)

structure IlContextEq 
    :> ILCONTEXTEQ =
struct

    open Util
    open IlContext
    open Il

    fun error s = Util.error "IlContextEq" s
    nonfix mod


    val error = fn str => Util.error "ilcontexteq.sml" str
    val showUnequal = Stats.ff("IlcontexteqShowUnequal")
    val debug = Stats.ff("IlcontexteqDebug")
    val blast_debug = Stats.ff("BlastDebug")

    (* --- State is kept in this module so avoid excessive parameter passing --- *)
    val cur_out = ref (NONE : BinIO.outstream option)
    val cur_in = ref (NONE : BinIO.instream option)
    fun curOut() = valOf(!cur_out)
    fun curIn() = valOf(!cur_in)

    fun exportOut f os arg =
	let val _ = cur_out := SOME os
	    val res = f arg
	    val _ = cur_out := NONE
	in  res
	end

    (* The mapping of integers to variables is used to alpha-vary variables on input. *)
    structure IntKey =
	struct
	    type ord_key = int
	    val compare = Int.compare
	end
    structure IntMap = SplayMapFn(IntKey)
    val inMap = ref (IntMap.empty : Name.var IntMap.map)
    fun int2var i = 
	(case IntMap.find(!inMap,i) of
	     SOME v => v
	   | NONE => let val v = Name.fresh_var()
			 val _ = if (!blast_debug)
				     then (print "int2var adding ";
					   print (Int.toString i);
					   print "  to  ";
					   Ppil.pp_var v; print "\n")
				 else ()
			 val _ = inMap := IntMap.insert(!inMap,i,v)
		     in  v
		     end)

    fun exportIn f is arg =
	let val _ = cur_in := SOME is
	    val _ = inMap := IntMap.empty
	    val res = f arg
	    val _ = cur_in := NONE
	    val _ = inMap := IntMap.empty
	in  res
	end



        structure CTab = IlTable.Conmap
        val ctab : int CTab.map ref = ref CTab.empty 
        val initial_tab_count = 25  (* must be > any encoding
                                        tag for a constructor! *)
        val ctab_count = ref initial_tab_count
        val itab : con IntListMap.map ref = ref IntListMap.empty
        val itab_count = ref initial_tab_count

        fun local_reset() = (ctab := CTab.empty;
                             itab := IntListMap.empty;
                             ctab_count := initial_tab_count; 
                             itab_count := initial_tab_count)


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
    fun blastInVar () = NameBlast.blastInVar int2var (curIn())
    fun blastOutLabel l = NameBlast.blastOutLabel (curOut()) l
    fun blastInLabel () = NameBlast.blastInLabel (curIn())
    fun blastOutTag t = NameBlast.blastOutTag (curOut()) t
    fun blastInTag () = NameBlast.blastInTag (curIn())

    fun blastOutPath (PATH (v,ls)) = (blastOutVar v;  blastOutList blastOutLabel ls)
    fun blastInPath () = PATH(blastInVar(), blastInList blastInLabel)

	fun blastOutArrow TOTAL = blastOutInt 0
	  | blastOutArrow PARTIAL = blastOutInt 1
	fun blastInArrow () =
	    (case (blastInInt()) of
		 0 => TOTAL
	       | 1 => PARTIAL
	       | _ => error "bad blastInArrow")

	fun blastOutIS Prim.W8 = blastOutInt 0
	  | blastOutIS Prim.W16 = blastOutInt 1
	  | blastOutIS Prim.W32 = blastOutInt 2
	  | blastOutIS Prim.W64 = blastOutInt 3
	fun blastInIS () =
	    (case blastInInt() of
		 0 => Prim.W8
	       | 1 => Prim.W16
	       | 2 => Prim.W32
	       | 3 => Prim.W64
	       | _ => (error "bad blastInIS" handle e => raise e))
	fun blastOutFS Prim.F32 = blastOutInt 0
	  | blastOutFS Prim.F64 = blastOutInt 1
	fun blastInFS () =
	    (case blastInInt() of
		 0 => Prim.F32
	       | 1 => Prim.F64
	       | _ => error "bad blastInFS")

	fun blastOutDec dec = 
	    (case dec of
		 DEC_EXP (v,c,NONE, inline)   => (blastOutInt 0; blastOutVar v; blastOutCon c;
						  blastOutBool inline)
	       | DEC_EXP (v,c,SOME e, inline) => (blastOutInt 1; blastOutVar v; blastOutCon c;
						  blastOutExp e; blastOutBool inline)
	       | DEC_CON (v,k,NONE, inline)   => (blastOutInt 2; blastOutVar v; blastOutKind k;
						  blastOutBool inline)
	       | DEC_CON (v,k,SOME c, inline) => (blastOutInt 3; blastOutVar v; blastOutKind k; 
						 blastOutCon c; blastOutBool inline)
	       | DEC_MOD (v,b,s)      => (blastOutInt 4; blastOutVar v; blastOutBool b; 
					  blastOutSig s))
	and blastInDec () =
	    (case (blastInInt()) of
		 0 => DEC_EXP (blastInVar (), blastInCon (), NONE, blastInBool())
	       | 1 => DEC_EXP (blastInVar (), blastInCon (), SOME (blastInExp()), blastInBool())
	       | 2 => DEC_CON (blastInVar (), blastInKind (), NONE, blastInBool())
	       | 3 => DEC_CON (blastInVar (), blastInKind (), SOME (blastInCon ()), blastInBool())
	       | 4 => DEC_MOD (blastInVar (), blastInBool (), blastInSig ())
	       | _ => error "bad blastInDec")
	and blastOutBnd bnd = 
	    (case bnd of
		 BND_EXP (v,e) => (blastOutInt 0; blastOutVar v; blastOutExp e)
	       | BND_CON (v,c) => (blastOutInt 1; blastOutVar v; blastOutCon c)
	       | BND_MOD (v,b,m) => (blastOutInt 2; blastOutVar v; blastOutBool b; blastOutMod m))
	and blastInBnd () =
	    (case (blastInInt()) of
		 0 => BND_EXP(blastInVar (), blastInExp ())
	       | 1 => BND_CON(blastInVar (), blastInCon ())
	       | 2 => BND_MOD(blastInVar (), blastInBool (), blastInMod ())
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
		 KIND => blastOutInt 0
	       | KIND_TUPLE n => (blastOutInt 1; blastOutInt n)
	       | KIND_ARROW (m,kres) => (blastOutInt 2; blastOutInt m;  blastOutKind kres))
		    
	and blastInKind () = 
	    (case blastInInt() of
		 0 => KIND
	       | 1 => KIND_TUPLE(blastInInt())
	       | 2 => KIND_ARROW(blastInInt(), blastInKind())
	       | _ => error "bad blastInKind")

        and simplifyCon c = 
            (case c of
	         CON_FLEXRECORD (ref (INDIRECT_FLEXINFO r)) => simplifyCon (CON_FLEXRECORD r)
	       | CON_FLEXRECORD (ref (FLEXINFO (_, true, lclist))) => CON_RECORD lclist
	       | CON_FLEXRECORD (ref (FLEXINFO (_, false, _))) => error "cannot simplify flex record type"
	       | CON_OVAR oc => simplifyCon (CON_TYVAR (Tyvar.ocon_deref oc))
	       | CON_TYVAR tv => (case Tyvar.tyvar_deref tv of
				      SOME c => simplifyCon c
				    | NONE => error "cannot simplify unresolved CON_TYVAR")
               | _ => c)
     
		 
        and blastOutCon c = 
            let
               val c = simplifyCon c
            in
	     case CTab.find(!ctab, c) of
               NONE => (blastOutCon' c;
                        ctab := CTab.insert(!ctab, c, !ctab_count);
(*
                        print "Wrote ";
                        Ppil.pp_con c;
                        print " as # ";
                        print (Int.toString (!ctab_count));
                        print "\n";
*)
			ctab_count := !ctab_count + 1)
             | SOME n => (
(*
                          print "Remembering ";
                          Ppil.pp_con c;
                          print " was # ";
                          print (Int.toString n);
                          print "\n";
*)
                          blastOutInt n)
            end

        and blastOutCon' c' =
            (case c' of
		 CON_VAR v => (blastOutInt 0; blastOutVar v)

	       | CON_INT is => (blastOutInt 1; blastOutIS is)
	       | CON_UINT is => (blastOutInt 2; blastOutIS is)
	       | CON_FLOAT fs => (blastOutInt 3; blastOutFS fs)

	       | CON_ARRAY c => (blastOutInt 4; blastOutCon c)
	       | CON_VECTOR c => (blastOutInt 5; blastOutCon c)
	       | CON_ANY => (blastOutInt 6)
	       | CON_REF c => (blastOutInt 7; blastOutCon c)
	       | CON_TAG c => (blastOutInt 8; blastOutCon c)
	       | CON_ARROW (cs,c,f,oa) => (blastOutInt 9; 
					   blastOutList blastOutCon cs;
					   blastOutCon c; blastOutBool f;
					   blastOutArrow (case (oneshot_deref oa) of
								 SOME a => a
							       | _ => error "unresolved CON_ARROW"))
	       | CON_APP (c1,cargs) => (blastOutInt 10; blastOutCon c1; 
					blastOutList blastOutCon cargs)
	       | CON_MU c => (blastOutInt 11; blastOutCon c)
	       | CON_RECORD lclist => (blastOutInt 12; blastOutList (blastOutPair blastOutLabel blastOutCon) lclist)
	       | CON_FUN (vlist, c) => (blastOutInt 13; blastOutList blastOutVar vlist; blastOutCon c)
	       | CON_SUM {names, noncarriers, carrier, special = NONE} => 
		     (blastOutInt 14; 
		      blastOutList blastOutLabel names;
		      blastOutInt noncarriers; 
		      blastOutCon carrier)
	       | CON_SUM {names, noncarriers, carrier, special = SOME i} => 
		     (blastOutInt 15; 
		      blastOutList blastOutLabel names;
		      blastOutInt noncarriers; 
		      blastOutCon carrier; 
		      blastOutInt i)
	       | CON_TUPLE_INJECT clist => (blastOutInt 16; blastOutList blastOutCon clist)
	       | CON_TUPLE_PROJECT (i,c) => (blastOutInt 17; blastOutInt i; blastOutCon c)
	       | CON_MODULE_PROJECT (m,l) => (blastOutInt 18; blastOutMod m; blastOutLabel l))

        and blastInCon () = 
	    let val _ = push()
		val _ = tab "blastInCon\n"
		val res = blastInCon' ()
		val _ = pop()
	    in  res
	    end

	and blastInCon' () =
            let val n = blastInInt() 
            in  
               if (n >= initial_tab_count) then
                  (
(*
                   print "Re-using # ";
                   print (Int.toString n);
                   print "\n";
*)
                   Option.valOf (IntListMap.find(!itab, n)))
               else
                  let val c = blastInCon'' n
                  in
                      itab := IntListMap.insert(!itab, !itab_count, c);
(*
                        print "Read ";
                        Ppil.pp_con c;
                        print " as # ";
                        print (Int.toString (!itab_count));
                        print "\n";
*)
		      itab_count := !itab_count + 1;
	              c
                  end
            end

        and blastInCon'' n =
	    (case n of
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
	       | 10 => CON_APP (blastInCon (), blastInList blastInCon)
	       | 11 => CON_MU (blastInCon ())
	       | 12 => CON_RECORD(blastInList (fn () => blastInPair blastInLabel blastInCon))
	       | 13 => CON_FUN (blastInList blastInVar, blastInCon ())
	       | 14 => CON_SUM {names = blastInList blastInLabel,
				noncarriers = blastInInt(),
				carrier = blastInCon (),
				special = NONE} 
	       | 15 => CON_SUM {names = blastInList blastInLabel,
				noncarriers = blastInInt(),
				carrier = blastInCon (),
				special = SOME (blastInInt())}
	       | 16 => CON_TUPLE_INJECT (blastInList blastInCon)
	       | 17 => CON_TUPLE_PROJECT (blastInInt(), blastInCon ())
	       | 18 => CON_MODULE_PROJECT (blastInMod (), blastInLabel ())
	       | _ => error "bad blastInCon")

	and blastOutValue v = 
	    (case v of
		 (Prim.int (is,w64)) => (blastOutInt 0; blastOutIS is; blastOutWord64 w64)
	       | (Prim.uint (is,w64)) => (blastOutInt 1; blastOutIS is; blastOutWord64 w64)
	       | (Prim.float (fs,str)) => (blastOutInt 2; blastOutFS fs; blastOutString str)
	       | (Prim.tag (t,c)) => (blastOutInt 3; blastOutTag t; blastOutCon c)
	       | _ => error "blasting of array/vector/refcell not supported")

	and blastInValue () =
	    (case (blastInInt()) of
		 0 => Prim.int (blastInIS (), blastInWord64 ())
	       | 1 => Prim.uint (blastInIS (), blastInWord64 ())
	       | 2 => Prim.float (blastInFS (), blastInString ())
	       | 3 => Prim.tag (blastInTag (), blastInCon ())
	       | _ => error "bad blastInValue")

	and blastOutIlPrim ilprim = 
	    let open Prim
	    in  (case ilprim of
		     eq_uint is => (blastOutInt 0; blastOutIS is)
		   | neq_uint is => (blastOutInt 1; blastOutIS is)
		   | not_uint is => (blastOutInt 2; blastOutIS is)
		   | and_uint is => (blastOutInt 3; blastOutIS is)
		   | or_uint is => (blastOutInt 4; blastOutIS is)
		   | xor_uint is => (blastOutInt 5; blastOutIS is)
		   | lshift_uint is => (blastOutInt 6; blastOutIS is)

		   | mk_ref => (blastOutInt 7)
		   | deref => (blastOutInt 8)
		   | eq_ref => (blastOutInt 9)
		   | setref => (blastOutInt 10))
	    end

	and blastInIlPrim () = 
	    let open Prim
	    in  (case (blastInInt()) of
		     0 => eq_uint(blastInIS ())
		   | 1 => neq_uint(blastInIS ())
		   | 2 => not_uint(blastInIS ())
		   | 3 => and_uint(blastInIS ())
		   | 4 => or_uint(blastInIS ())
		   | 5 => xor_uint(blastInIS ())
		   | 6 => lshift_uint(blastInIS ())

		   | 7 => mk_ref
		   | 8 => deref
		   | 9 => eq_ref
		   | 10 => setref
		   | _ => error "bad blastInIlPrim")
	    end


	and blastOutTT tt =
	    let open Prim 
	    in  (case tt of
		     int_tt => blastOutInt 0
		   | real_tt => blastOutInt 1
		   | both_tt => blastOutInt 2)
	    end


	and blastInTT () =
	    let open Prim 
	    in  (case (blastInInt()) of
		     0 => int_tt
		   | 1 => real_tt
		   | 2 => both_tt
		   | _ => error "bad blastInTT")
	    end
			 
	and blastOutTable table = 
	    let open Prim 
	    in  (case table of
		     IntArray is => (blastOutInt 0; blastOutIS is)
		   | IntVector is => (blastOutInt 1; blastOutIS is)
		   | FloatArray fs => (blastOutInt 2; blastOutFS fs)
		   | FloatVector fs => (blastOutInt 3; blastOutFS fs)
		   | OtherArray hnf => (blastOutInt 4; blastOutBool hnf)
		   | OtherVector hnf => (blastOutInt 5; blastOutBool hnf))
	    end

	and blastInTable () =
	    let open Prim 
	    in  (case (blastInInt()) of
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
		     soft_vtrap tt => (blastOutInt 0; blastOutTT tt)
		   | soft_ztrap tt => (blastOutInt 1; blastOutTT tt)
		   | hard_vtrap tt => (blastOutInt 2; blastOutTT tt)
		   | hard_ztrap tt => (blastOutInt 3; blastOutTT tt)
			 

		       (* conversions amongst floats, ints, uints with w32 and f64 *)
		   | float2int (* floor *) => (blastOutInt 6)
		   | int2float (* real  *) => (blastOutInt 7)
		   | int2uint (is1,is2) => (blastOutInt 8; blastOutIS is1; blastOutIS is2)
		   | uint2int (is1,is2) => (blastOutInt 9; blastOutIS is1; blastOutIS is2)
		   | uinta2uinta (is1,is2) => (blastOutInt 10; blastOutIS is1; blastOutIS is2)
		   | uintv2uintv (is1,is2) => (blastOutInt 11; blastOutIS is1; blastOutIS is2)
		   | int2int (is1,is2) => (blastOutInt 12; blastOutIS is1; blastOutIS is2)
		   | uint2uint (is1,is2) => (blastOutInt 13; blastOutIS is1; blastOutIS is2)

		   (* floatint-point operations *)	
		   | neg_float fs  => (blastOutInt 14; blastOutFS fs)
		   | abs_float fs  => (blastOutInt 15; blastOutFS fs)
		   | plus_float fs  => (blastOutInt 16; blastOutFS fs)
		   | minus_float fs  => (blastOutInt 17; blastOutFS fs)
		   | mul_float fs  => (blastOutInt 18; blastOutFS fs)
		   | div_float fs  => (blastOutInt 19; blastOutFS fs)
		   | less_float fs  => (blastOutInt 20; blastOutFS fs)
		   | greater_float fs  => (blastOutInt 21; blastOutFS fs)
		   | lesseq_float fs  => (blastOutInt 22; blastOutFS fs)
		   | greatereq_float  fs  => (blastOutInt 23; blastOutFS fs)
		   | eq_float  fs  => (blastOutInt 24; blastOutFS fs)
		   | neq_float fs  => (blastOutInt 25; blastOutFS fs)

		   (* int operations *)
		   | plus_int is  => (blastOutInt 26; blastOutIS is)
		   | minus_int is  => (blastOutInt 27; blastOutIS is)
		   | mul_int is  => (blastOutInt 28; blastOutIS is)
		   | div_int is  => (blastOutInt 29; blastOutIS is)
		   | mod_int is  => (blastOutInt 30; blastOutIS is)
		   | quot_int is  => (blastOutInt 31; blastOutIS is)
		   | rem_int is  => (blastOutInt 32; blastOutIS is)
		   | plus_uint is  => (blastOutInt 33; blastOutIS is)
		   | minus_uint is  => (blastOutInt 34; blastOutIS is)
		   | mul_uint is  => (blastOutInt 35; blastOutIS is)
		   | div_uint is  => (blastOutInt 36; blastOutIS is)
		   | mod_uint is  => (blastOutInt 37; blastOutIS is)
		   | less_int is  => (blastOutInt 38; blastOutIS is)
		   | greater_int is  => (blastOutInt 39; blastOutIS is)
		   | lesseq_int is  => (blastOutInt 40; blastOutIS is)
		   | greatereq_int is  => (blastOutInt 41; blastOutIS is)
		   | less_uint is  => (blastOutInt 42; blastOutIS is)
		   | greater_uint is  => (blastOutInt 43; blastOutIS is)
		   | lesseq_uint is  => (blastOutInt 44; blastOutIS is)
		   | greatereq_uint is  => (blastOutInt 45; blastOutIS is)
		   | eq_int is  => (blastOutInt 46; blastOutIS is)
		   | neq_int is  => (blastOutInt 47; blastOutIS is)
		   | neg_int is  => (blastOutInt 48; blastOutIS is)
		   | abs_int is  => (blastOutInt 49; blastOutIS is)

		   (* bit-pattern manipulation *)
		   | not_int is  => (blastOutInt 50; blastOutIS is)
		   | and_int is  => (blastOutInt 51; blastOutIS is)
		   | or_int is  => (blastOutInt 52; blastOutIS is)
		   | xor_int is  => (blastOutInt 53; blastOutIS is)
		   | lshift_int is  => (blastOutInt 54; blastOutIS is)
		   | rshift_int is  => (blastOutInt 55; blastOutIS is)
		   | rshift_uint is  => (blastOutInt 56; blastOutIS is)
			 
		   (* array and vectors *)
		   | array2vector t => (blastOutInt 57; blastOutTable t)
		   | vector2array t => (blastOutInt 58; blastOutTable t)
		   | create_table t => (blastOutInt 59; blastOutTable t)
		   | create_empty_table t => (blastOutInt 60; blastOutTable t)
		   | sub t => (blastOutInt 61; blastOutTable t)
		   | update t => (blastOutInt 62; blastOutTable t)
		   | length_table t => (blastOutInt 63; blastOutTable t)
		   | equal_table t => (blastOutInt 64; blastOutTable t))



	    end

	and blastInPrim () =
	    let open Prim
	    in  (case (blastInInt()) of
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
		   | 12 => int2int (blastInIS (), blastInIS ())
		   | 13 => uint2uint (blastInIS (), blastInIS ())

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
		   | 53 => xor_int(blastInIS ())
		   | 54 => lshift_int(blastInIS ())
		   | 55 => rshift_int(blastInIS ())
		   | 56 => rshift_uint(blastInIS ())
			 
		   (* array and vectors *)
		   | 57 => array2vector (blastInTable ())
		   | 58 => vector2array (blastInTable ())
		   | 59 => create_table (blastInTable ())
		   | 60 => create_empty_table (blastInTable ())
		   | 61 => sub (blastInTable ())
		   | 62 => update (blastInTable ())
		   | 63 => length_table (blastInTable ())
		   | 64 => equal_table (blastInTable ())

		   | _ => error "bad blastInPrim")

	    end

	and blastOutExp exp = 
	    (case exp of
		 OVEREXP (_,_,oe) => (case oneshot_deref oe of
					  SOME e => blastOutExp e
					| NONE => error "cannot blastOut unresolved OVEREXP")
	       | SCON v => (blastOutInt 0; blastOutValue v)
	       | PRIM (p,clist,elist) => (blastOutInt 1; blastOutPrim p;
					  blastOutList blastOutCon clist;
					  blastOutList blastOutExp elist)
	       | ILPRIM (p,clist,elist) => (blastOutInt 2; blastOutIlPrim p;
					    blastOutList blastOutCon clist;
					    blastOutList blastOutExp elist)
	       | ETAPRIM (p,clist) => (blastOutInt 3; blastOutPrim p;
				       blastOutList blastOutCon clist)
	       | ETAILPRIM (p,clist) => (blastOutInt 4; blastOutIlPrim p;
					 blastOutList blastOutCon clist)
	       | VAR v => (blastOutInt 5; blastOutVar v)
	       | APP (e1,e2) => (blastOutInt 6; blastOutExp e1; 
				 blastOutExp e2)
	       | EXTERN_APP (c,e,elist) => (blastOutInt 23; 
					    blastOutCon c;
					    blastOutExp e; 
					    blastOutList blastOutExp elist)
	       | FIX (b,a,fbnds) => (blastOutInt 7; blastOutBool b; blastOutArrow a;
				     blastOutList blastOutFbnd fbnds)
	       | RECORD lelist => (blastOutInt 8; blastOutList (blastOutPair blastOutLabel blastOutExp) lelist)
	       | RECORD_PROJECT (e,l,c) => (blastOutInt 9; blastOutExp e; blastOutLabel l; blastOutCon c)
	       | SUM_TAIL (i,c,e) => (blastOutInt 10; blastOutInt i;
				      blastOutCon c; blastOutExp e)
	       | HANDLE (c,e1,e2) => (blastOutInt 11; blastOutCon c; blastOutExp e1; blastOutExp e2)
	       | RAISE (c,e) => (blastOutInt 12; blastOutCon c; blastOutExp e)
	       | LET(bnds,e) => (blastOutInt 13; blastOutList blastOutBnd bnds; blastOutExp e)
	       | NEW_STAMP c => (blastOutInt 14; blastOutCon c)
	       | EXN_INJECT (str,e1,e2) => (blastOutInt 15; blastOutString str; blastOutExp e1; blastOutExp e2)
	       | ROLL (c,e) => (blastOutInt 16; blastOutCon c; blastOutExp e)
	       | UNROLL (c1,c2,e) => (blastOutInt 17; blastOutCon c1; blastOutCon c2;
				      blastOutExp e)
	       | INJ {sumtype, field, inject} =>
		     (blastOutInt 18; blastOutCon sumtype; 
		      blastOutInt field;
		      blastOutOption blastOutExp inject)
	       | CASE {sumtype, arg, bound, arms, tipe, default} => 
		      (blastOutInt 19;
		       blastOutCon sumtype;
		       blastOutExp arg;
		       blastOutVar bound;
		       blastOutList (blastOutOption blastOutExp) arms;
		       blastOutCon tipe;
		       blastOutOption blastOutExp default)
	       | EXN_CASE {arg, arms, default, tipe} => 
		      (blastOutInt 20;
		       blastOutExp arg;
		       blastOutList (blastOutTriple blastOutExp blastOutCon blastOutExp) arms;
		       blastOutOption blastOutExp default;
		       blastOutCon tipe)
	       | MODULE_PROJECT (m,l) => (blastOutInt 21; blastOutMod m; blastOutLabel l)
	       | SEAL(e,c) => (blastOutInt 22; blastOutExp e; blastOutCon c))

        and blastInExp () = 
	    let val _ = push()
		val _ = tab "blastInExp\n" 
		val res = blastInExp' ()
		val _ = pop()
	    in  res
	    end

	and blastInExp' () =
	     (case (blastInInt()) of
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
	       | 10 => SUM_TAIL (blastInInt(), blastInCon (), blastInExp ())
	       | 11 => HANDLE (blastInCon(), blastInExp (), blastInExp ())
	       | 12 => RAISE (blastInCon (), blastInExp ())
	       | 13 => LET(blastInList blastInBnd, blastInExp ())
	       | 14 => NEW_STAMP (blastInCon ())
	       | 15 => EXN_INJECT (blastInString (), blastInExp (), blastInExp ())
	       | 16 => ROLL (blastInCon (), blastInExp ())
	       | 17 => UNROLL (blastInCon (), blastInCon (), blastInExp ())
	       | 18 => let val sumtype = blastInCon ()
			   val field = blastInInt()
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
		 MOD_VAR v => (blastOutInt 0; blastOutVar v)
	       | MOD_STRUCTURE sbnds => (blastOutInt 1; blastOutSbnds sbnds)
	       | MOD_FUNCTOR (a,v,s1,m,s2) => (blastOutInt 2; 
					     blastOutArrow a; blastOutVar v; 
					     blastOutSig s1; blastOutMod m;
					     blastOutSig s2)
	       | MOD_APP (m1,m2) => (blastOutInt 3; blastOutMod m1; blastOutMod m2)
	       | MOD_PROJECT (m,l) => (blastOutInt 4; blastOutMod m; blastOutLabel l)
	       | MOD_SEAL (m,s) => (blastOutInt 5; blastOutMod m; blastOutSig s)
	       | MOD_LET (v,m1,m2) => (blastOutInt 6; blastOutVar v; blastOutMod m1; blastOutMod m2))

        and blastInMod () = 
	    let val _ = push()
		val _ = tab "blastInMod\n"
		val res = blastInMod' ()
		val _ = pop()
	    in  res
	    end

	and blastInMod' () =
	    (
		     case (blastInInt()) of
		 0 => MOD_VAR(blastInVar ())
	       | 1 => MOD_STRUCTURE(blastInSbnds ())
	       | 2 => MOD_FUNCTOR(blastInArrow(), blastInVar (), 
				  blastInSig (), blastInMod (), blastInSig ())
	       | 3 => MOD_APP (blastInMod (), blastInMod ())
	       | 4 => MOD_PROJECT (blastInMod (), blastInLabel ())
	       | 5 => MOD_SEAL (blastInMod (), blastInSig ())
	       | 6 => MOD_LET (blastInVar (), blastInMod (), blastInMod ())
	       | _ => error "bad blastInMod")

	and blastOutSig s = 
		 (tab "    blastInSig\n"; 
		  case s of
		 SIGNAT_STRUCTURE sdecs => (blastOutInt 0; blastOutSdecs sdecs)
	       | SIGNAT_SELF(self, unselfSigOpt, selfSig) => (blastOutInt 1; blastOutPath self; 
							      blastOutOption blastOutSig unselfSigOpt; blastOutSig selfSig)
	       | SIGNAT_FUNCTOR(v, s1, s2, arrow) => (blastOutInt 2; blastOutVar v;
						      blastOutSig s1; blastOutSig s2; blastOutArrow arrow)
	       | SIGNAT_VAR v => (blastOutInt 5; blastOutVar v)
	       | SIGNAT_OF m => (blastOutInt 6; blastOutPath m))

	and blastInSig () =
	    (case (blastInInt()) of
		 0 => SIGNAT_STRUCTURE (blastInSdecs ())
	       | 1 => SIGNAT_SELF (blastInPath (), blastInOption blastInSig, blastInSig ())
	       | 2 => SIGNAT_FUNCTOR(blastInVar (), blastInSig (), blastInSig (), blastInArrow ())
	       | 5 => SIGNAT_VAR(blastInVar ())
	       | 6 => SIGNAT_OF(blastInPath ())
	       | _ => error "bad blastInSig")

	fun blastOutOvld (OVLD (celist,default)) =
	    let val _ = blastOutInt (case default
					  of NONE => 0
					   | SOME n => n+1)
	    in  blastOutList (blastOutPair blastOutCon blastOutExp) celist
	    end
	fun blastInOvld () =
	    let val default = case blastInInt()
				of 0 => NONE
				 | n => SOME (n-1)
		val celist = blastInList (fn () => blastInPair blastInCon blastInExp)
	    in  OVLD (celist, default)
	    end

	fun blastOutPC pc = 
	    case pc of
		PHRASE_CLASS_EXP (e,c,eopt,inline) => (blastOutInt 0; blastOutExp e; 
						       blastOutCon c; blastOutOption blastOutExp eopt;
						       blastOutBool inline)
	      | PHRASE_CLASS_CON (c,k,copt,inline) => (blastOutInt 1; blastOutCon c; blastOutKind k; 
						       blastOutOption blastOutCon copt; 
						       blastOutBool inline)
	      | PHRASE_CLASS_MOD (m,b,s) => (blastOutInt 2; blastOutMod m; blastOutBool b; 
					     blastOutSig s)
	      | PHRASE_CLASS_SIG (v,s) => (blastOutInt 3; blastOutVar v; blastOutSig s)

	fun blastInPC () = 
	    (tab "  blastInPC\n"; 
	    case (blastInInt()) of
		0 => PHRASE_CLASS_EXP(blastInExp (), blastInCon (), blastInOption blastInExp, blastInBool())
	      | 1 => PHRASE_CLASS_CON(blastInCon (), blastInKind (), blastInOption blastInCon, blastInBool())
	      | 2 => PHRASE_CLASS_MOD(blastInMod (), blastInBool (), blastInSig ())
	      | 3 => PHRASE_CLASS_SIG(blastInVar (), blastInSig ())
	      | _ => error "bad blastInPC")

	fun blastOutFixity Fixity.NONfix = blastOutInt 0
	  | blastOutFixity (Fixity.INfix (m,n)) = (blastOutInt 1; 
						      blastOutInt m; blastOutInt n)
	fun blastInFixity () =
	    if (blastInInt() = 0) 
		then Fixity.NONfix 
	    else let val m = blastInInt()
		     val n = blastInInt()
		 in  Fixity.INfix (m,n)
		 end

	fun blastOutFixityMap fm = 
	    NameBlast.blastOutLabelmap (curOut()) (fn _ => blastOutFixity) fm
	fun blastInFixityMap () = 
	    NameBlast.blastInLabelmap (curIn()) (fn _ => blastInFixity())

	fun blastOutOverloadMap fm = 
	    NameBlast.blastOutLabelmap (curOut()) (fn _ => blastOutOvld) fm
	fun blastInOverloadMap () = 
	    NameBlast.blastInLabelmap (curIn()) (fn _ => blastInOvld())


	fun blastOutPathMap label_list = 
	    NameBlast.blastOutPathmap (curOut()) (fn _ => blastOutPair blastOutLabel blastOutPC) label_list
	fun blastInPathMap () = 
	    NameBlast.blastInPathmap int2var (curIn()) (fn _ => blastInPair blastInLabel blastInPC)

	fun reconstructLabelMap pathMap = 
	    let fun folder(p,(l,pc),pmap) = Name.LabelMap.insert(pmap,l,(PATH p,pc))
	    in  Name.PathMap.foldli folder Name.LabelMap.empty pathMap 
	    end

	fun blastOutUnresolved unresolved = 
	    NameBlast.blastOutVarmap (curOut()) (fn _ => blastOutLabel) unresolved

	fun blastInUnresolved () : Name.label Name.VarMap.map =
	    NameBlast.blastInVarmap int2var (curIn()) (fn _ => blastInLabel())

    fun blastOutContext (CONTEXT {fixityMap, overloadMap, labelMap, pathMap, ordering}) = 
	(Blaster.reset();
         local_reset();
         blastOutFixityMap fixityMap;
         blastOutOverloadMap overloadMap;
	 blastOutPathMap pathMap;
	 blastOutList blastOutPath ordering)

    fun blastInContext () = 
	let val _ = Blaster.reset();
            val _ = local_reset();
	    val fixityMap = blastInFixityMap ()
	    val overloadMap = blastInOverloadMap()	 
	    val pathMap = blastInPathMap ()
	    val ordering = blastInList blastInPath
	    val labelMap = reconstructLabelMap pathMap
	in CONTEXT {fixityMap = fixityMap, overloadMap = overloadMap,
		    labelMap = labelMap, pathMap = pathMap, ordering = ordering}
	end

    fun blastOutPartialContext (ctxt, unresolved) = 
	(blastOutContext ctxt;
	 blastOutUnresolved unresolved)

    fun blastInPartialContext () = 
	let val ctxt = blastInContext()
	    val unresolved = blastInUnresolved()
	in  (ctxt, unresolved)
	end

    val blastOutContext = exportOut blastOutContext
    val blastOutPartialContext = exportOut blastOutPartialContext
    val blastInContext = fn (is : BinIO.instream) => exportIn blastInContext is ()
    val blastInPartialContext = fn (is : BinIO.instream) => exportIn blastInPartialContext is ()

	(* alpha-conversion () necessary when checking contexts for
	 * equality.  This () done by explicitly maintaining a `var
	 * map' mapping variables to variables.  *)

	exception NOT_EQUAL    (* raised when contexts are not equal *)

	fun wrap str f arg =
	    if (!debug) then
		let fun msg() = (print str; print " returning false\n")
		    val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
		    val _ = if res then () 
			    else msg()
		in  res
		end
	    else f arg
		
	fun wrap' (str,thunk) f arg =
	    if (!debug) then
		let fun msg() = (print str; print " returning false\n"; thunk arg)
		    val res = (f arg handle NOT_EQUAL => (msg(); raise NOT_EQUAL))
		    val _ = if res then () 
			    else msg()
		in  res
		end
	    else f arg

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
			 | NONE => let val _ = if (!debug)
						   then (print "label "; Ppil.pp_label l;
							 print " not found in ur'\n")
					       else ()
				   in  raise NOT_EQUAL
				   end
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
			    then (if (!debug)
				      then (print "extend_vm_contxt: lvlist length not equal\n";
					    printLvlist "lvlist" lvlist;
					    printLvlist "lvlist'" lvlist')
				  else ();
				  raise NOT_EQUAL)
			else ()
		fun folder ((l,v), (vm,vlist)) =
		      case Context_Lookup_Label(c',l) of
			   SOME(PATH (v',_),_) => (VM.add(v,v',vm), v::vlist)
			 | NONE => let val _ = if (!debug)
						   then (print "label "; Ppil.pp_label l;
							 print " not found in c'\n")
					       else ()
				   in  raise NOT_EQUAL
				   end
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
					         | NONE => let val _ = if (!debug)
									   then (print "XXX label mismatch ";
										 Ppil.pp_label l; print "\n")
								       else ()
							   in  raise NOT_EQUAL
							   end) vm sdecs
	    end

	fun extend_vm_sbnds(sbnds,sbnds',vm) : vm =
	    let val _ = if length sbnds <> length sbnds' then raise NOT_EQUAL else ()
	    in foldr (fn (SBND(l,bnd), vm) => case sbnds_lookup(sbnds',l)
		                                of SOME bnd' => add_bnd(bnd,bnd',vm)
					         | NONE => let val _ = if (!debug)
									   then (print "XXX label mismatch ";
										 Ppil.pp_label l; print "\n")
								       else ()
							   in  raise NOT_EQUAL
							   end) vm sbnds
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
		val _ = if res orelse not (!debug)
			    then () 
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
		val _ = if res orelse not (!debug)
			    then () 
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
		       | NONE => let val _ = if (!debug)
						 then (print "XXX eq_sdecs' returning false due to ";
						       Ppil.pp_label l; print "\n")
					     else ()
				 in  false
				 end) sdecs
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
		val _ = if res orelse not (!debug)
			    then () 
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
		    let
			fun diag s = 
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
	    in  res
	    end	    

	fun eq_label_map (equaller : 'a * 'a -> bool) (m : 'a Name.LabelMap.map, m' : 'a Name.LabelMap.map) : bool =
	    let val intersection = Name.LabelMap.intersectWith (fn (a,b) =>
								if equaller (a,b) then a
								else raise NOT_EQUAL) (m, m')
		val numItems = Name.LabelMap.numItems
		val n = numItems intersection
	    in  n = numItems m andalso n = numItems m'
	    end handle NOT_EQUAL => false

	val eq_fixity_map = wrap "eq_fixity_map" (eq_label_map (op= : Fixity.fixity * Fixity.fixity -> bool))

	fun eq_conexp vm ((c,e),(c',e')) = eq_con(vm,c,c')
	fun eq_ovld vm (OVLD (ce,d), OVLD (ce',d')) = (Listops.eq_list (eq_conexp vm, ce,ce') andalso
						       eq_opt (op= : int * int -> bool) (d, d'))
	fun eq_overload_map (vm,om1,om2) = eq_label_map (eq_ovld vm) (om1,om2)
	val eq_overload_map = wrap "eq_overload_map" eq_overload_map

	fun eq_partial_context (pc as (c,u): partial_context, pc' as (c',u'): partial_context) : bool =
	    let
		val (vm,vlist) = extend_vm_unresolved(u,u',VM.empty,[])
		val (vm,vlist) = extend_vm_context(c,c',vm,vlist)
		val CONTEXT {fixityMap=fm, overloadMap=om, ...} = c
		val CONTEXT {fixityMap=fm', overloadMap=om', ...} = c'
		val res = (eq_cntxt(vm,c,c',vlist) andalso
			   eq_overload_map(vm,om,om') andalso
			   eq_fixity_map(fm,fm'))
		val _ = if (!showUnequal andalso not res)
			    then (print "vm = "; VM.show_vm vm; print "\n";
				  print "pc = "; Ppil.pp_pcontext pc; print "\n\n";
				  print "pc' = "; Ppil.pp_pcontext pc'; print "\n\n")
			else ()
	    in  res
	    end
	    handle NOT_EQUAL => false

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

	fun wrapTopLevel f arg = (f arg handle NOT_EQUAL => false)
	    
	val eq_partial_context = wrapTopLevel eq_partial_context
	val eq_context         = wrapTopLevel eq_context
	val eq_con             = wrapTopLevel eq_con
    end


