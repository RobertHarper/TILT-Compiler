structure IlBlast :>
sig
   val blastOutPinterface : Blaster.outstream -> Il.pinterface -> unit
   val blastInPinterface : Blaster.instream -> Il.pinterface
   val blastInPinterfaceParms : Blaster.instream -> Il.parms
end =
struct
    open Il

    val error = fn str => Util.error "ilblast.sml" str

    val BlastDebug = Blaster.BlastDebug

    (* --- State is kept in this module so avoid excessive parameter passing --- *)
    val cur_out = ref (NONE : Blaster.outstream option)
    val cur_in = ref (NONE : Blaster.instream option)
    fun curOut() = valOf(!cur_out)
    fun curIn() = valOf(!cur_in)

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
			 val _ = if (!BlastDebug)
				     then (print "int2var adding ";
					   print (Int.toString i);
					   print "  to  ";
					   Ppil.pp_var v; print "\n")
				 else ()
			 val _ = inMap := IntMap.insert(!inMap,i,v)
		     in  v
		     end)

        structure CTab = IlTable.Conmap
        val ctab : int CTab.map ref = ref CTab.empty
        val initial_tab_count = 30  (* must be > any encoding
				     tag for a constructor! *)
        val ctab_count = ref initial_tab_count
        val itab : con IntListMap.map ref = ref IntListMap.empty
        val itab_count = ref initial_tab_count

        fun local_reset() = (ctab := CTab.empty;
                             itab := IntListMap.empty;
                             ctab_count := initial_tab_count;
                             itab_count := initial_tab_count)

    fun exportOut f os arg =
	let val _ = local_reset()
	    val _ = cur_out := SOME os
	    val res = f arg
	    val _ = cur_out := NONE
	in  res
	end

    fun exportIn f is arg =
	let val _ = local_reset()
	    val _ = cur_in := SOME is
	    val _ = inMap := IntMap.empty
	    val res = f arg
	    val _ = cur_in := NONE
	    val _ = inMap := IntMap.empty
	in  res
	end

    (* debugging stuff *)
    val indent = ref 0
    fun push() = indent := (!indent) + 2
    fun pop() = indent := (!indent) - 2
    fun tab s = if (!BlastDebug)
		    then let fun loop 0 = print s
			       | loop n = (print " "; loop (n-1))
			 in  loop (!indent)
			 end
		else ()
    fun say s = if (!BlastDebug) then print s else ()


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

    fun blastOutVpath ((v,ls) : vpath) = (blastOutVar v;  blastOutList blastOutLabel ls)
    fun blastInVpath () : vpath = (blastInVar(), blastInList blastInLabel)
    fun blastOutPath (PATH vpath) = blastOutVpath vpath
    fun blastInPath () = PATH (blastInVpath())

	fun blastOutArrow TOTAL = blastOutInt 0
	  | blastOutArrow PARTIAL = blastOutInt 1
	  | blastOutArrow APPLICATIVE = blastOutInt 2
	  | blastOutArrow GENERATIVE = blastOutInt 3
	fun blastInArrow () =
	    (case (blastInInt()) of
		 0 => TOTAL
	       | 1 => PARTIAL
	       | 2 => APPLICATIVE
	       | 3 => GENERATIVE
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
	       | _ => error "bad blastInIS")
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

	       | CON_TYVAR _ => error "blastOutCon CON_TYVAR"
	       | CON_OVAR _ => error "blastOutCon CON_OVAR"
	       | CON_FLEXRECORD _ => error "blastOutCon CON_FLEXRECORD"

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
					   blastOutArrow (case (Util.oneshot_deref oa) of
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
	       | CON_MODULE_PROJECT (m,l) => (blastOutInt 18; blastOutMod m; blastOutLabel l)
	       | CON_COERCION (vs,c1,c2) =>
		     (blastOutInt 19; blastOutList blastOutVar vs; blastOutCon c1; blastOutCon c2)
	       | CON_INTARRAY is => (blastOutInt 20; blastOutIS is)
	       | CON_INTVECTOR is => (blastOutInt 21; blastOutIS is)
	       | CON_FLOATARRAY fs => (blastOutInt 22; blastOutFS fs)
	       | CON_FLOATVECTOR fs => (blastOutInt 23; blastOutFS fs)
		     )


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
                   valOf (IntListMap.find(!itab, n)))
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
			  val a = Util.oneshot_init (blastInArrow ())
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
	       | 19 => CON_COERCION (blastInList blastInVar, blastInCon (), blastInCon ())
	       | 20 => CON_INTARRAY (blastInIS ())
	       | 21 => CON_INTVECTOR(blastInIS ())
	       | 22 => CON_FLOATARRAY (blastInFS ())
	       | 23 => CON_FLOATVECTOR (blastInFS ())
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
		       )
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
		   | equal_table t => (blastOutInt 64; blastOutTable t)		   
		   | mk_ref => (blastOutInt 65)
		   | deref => (blastOutInt 66)
		   | eq_ref => (blastOutInt 67)
		   | setref => (blastOutInt 68))



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

		   | 65 => mk_ref
		   | 66 => deref
		   | 67 => eq_ref
		   | 68 => setref

		   | _ => error "bad blastInPrim")

	    end

	and blastOutExp exp =
	    (case exp of
		 OVEREXP (_,_,oe) => (case Util.oneshot_deref oe of
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
	       | COERCE (coercion,cs,e) =>
		     (blastOutInt 24; blastOutExp coercion; blastOutList blastOutCon cs; blastOutExp e)
	       | FOLD (vs,c1,c2) => (blastOutInt 25; blastOutList blastOutVar vs;
				     blastOutCon c1; blastOutCon c2)
	       | UNFOLD (vs,c1,c2) => (blastOutInt 26; blastOutList blastOutVar vs;
				       blastOutCon c1; blastOutCon c2)
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
	       | 23 => EXTERN_APP (blastInCon (),
				   blastInExp (),
				   blastInList blastInExp)
	       | 24 => COERCE (blastInExp (), blastInList blastInCon, blastInExp ())
	       | 25 => FOLD (blastInList blastInVar, blastInCon (), blastInCon ())
	       | 26 => UNFOLD (blastInList blastInVar, blastInCon (), blastInCon ())
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
	       | MOD_LET (v,m1,m2) => (blastOutInt 6; blastOutVar v; blastOutMod m1; blastOutMod m2)
	       | MOD_CANONICAL s => (blastOutInt 7; blastOutSig s)
               | MOD_REC (v,s,m) => (blastOutInt 8; blastOutVar v; blastOutSig s; blastOutMod m))


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
	       | 7 => MOD_CANONICAL (blastInSig ())
	       | 8 => MOD_REC (blastInVar (), blastInSig (), blastInMod ())
	       | _ => error "bad blastInMod")

	and blastOutSig s =
		 (tab "    blastOutSig\n";
		  case s of
		 SIGNAT_STRUCTURE sdecs => (blastOutInt 0; blastOutSdecs sdecs)
	       | SIGNAT_RDS (v,sdecs) => (blastOutInt 1; blastOutVar v; blastOutSdecs sdecs)
	       | SIGNAT_FUNCTOR(v, s1, s2, arrow) => (blastOutInt 2; blastOutVar v;
						      blastOutSig s1; blastOutSig s2; blastOutArrow arrow)
	       | SIGNAT_VAR v => (blastOutInt 5; blastOutVar v)
               | _ => error "bad blastOutSig"
                 )
	and blastInSig () =
	    (case (blastInInt()) of
		 0 => SIGNAT_STRUCTURE (blastInSdecs ())
	       | 1 => SIGNAT_RDS (blastInVar (), blastInSdecs ())
	       | 2 => SIGNAT_FUNCTOR(blastInVar (), blastInSig (), blastInSig (), blastInArrow ())
	       | 5 => SIGNAT_VAR(blastInVar ())
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

	fun blastOutEntry ce =
	    (case ce
	       of CONTEXT_SDEC sdec => (blastOutInt 0; blastOutSdec sdec)
		| CONTEXT_SIGNAT (l,v,s) => (blastOutInt 1; blastOutLabel l; blastOutVar v; blastOutSig s)
		| CONTEXT_EXTERN (l,v,l',c) =>
		    (blastOutInt 2; blastOutLabel l; blastOutVar v;
		     blastOutLabel l'; blastOutCon c)
		| CONTEXT_FIXITY (l,f) => (blastOutInt 3; blastOutLabel l; blastOutFixity f)
		| CONTEXT_OVEREXP (l,ovld) => (blastOutInt 4; blastOutLabel l; blastOutOvld ovld))

	fun blastInEntry () =
	    (case blastInInt()
	       of 0 => CONTEXT_SDEC (blastInSdec ())
		| 1 => CONTEXT_SIGNAT (blastInLabel (), blastInVar (), blastInSig ())
		| 2 => CONTEXT_EXTERN (blastInLabel (), blastInVar (),
				       blastInLabel (), blastInCon ())
		| 3 => CONTEXT_FIXITY (blastInLabel (), blastInFixity ())
		| 4 => CONTEXT_OVEREXP (blastInLabel (), blastInOvld ())
		| _ => error "bad blastInEntry")

	fun blastOutParm parm =
	    (case parm
	       of PARM mainlab => (blastOutInt 0; blastOutLabel mainlab)
		| PARM_SIG (mainlab,siglab) =>
		   (blastOutInt 1; blastOutLabel mainlab; blastOutLabel siglab)
		| PARM_EXT (mainlab,extlab) =>
		   (blastOutInt 2; blastOutLabel mainlab; blastOutLabel extlab))

	fun blastInParm () =
	    (case blastInInt ()
	       of 0 => PARM (blastInLabel ())
		| 1 => PARM_SIG (blastInLabel (), blastInLabel ())
		| 2 => PARM_EXT (blastInLabel (), blastInLabel ())
		| _ => error "bad blastInParm")

	fun blastOutParms parms =
	    NameBlast.blastOutVarmap (curOut()) (fn _ => blastOutParm) parms

	fun blastInParms () =
	    NameBlast.blastInVarmap int2var (curIn()) (fn _ => blastInParm())

	fun blastOutPinterface ({parms,entries} : Il.pinterface) =
	    (blastOutParms parms; blastOutList blastOutEntry entries)

	fun blastInPinterface () =
	    {parms = blastInParms (),
	     entries = blastInList blastInEntry}

    val blastOutPinterface = exportOut blastOutPinterface
    val blastInPinterface =
	fn (is : Blaster.instream) => exportIn blastInPinterface is ()

    val magic = "pinterface $Revision$"
    val (blastOutPinterface, blastInPinterface) =
	Blaster.magic (blastOutPinterface, blastInPinterface, magic)

    val (_,blastInPinterfaceParms) =
	Blaster.magic (fn _ => error "blastOutPinterfaceParms",
		       fn is => exportIn blastInParms is (),
		       magic)
end
