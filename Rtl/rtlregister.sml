functor MakeRegisterSet(structure Pprtl : PPRTL)
	:> REGISTERSET = 
  struct

      val error = fn s => Util.error "rtlregister.sml" s

    (* pick a prime *)
    val ireg_size = 997;
    val freg_size = 997;
    val suppress_warning = ref false;
    fun warn (s : string) = if (!suppress_warning) then () else print s

    type instr = Rtl.instr
    type regi = Rtl.regi
    type regf = Rtl.regf
    open Rtl
    structure W = TilWord32

    datatype iword    = WORD of W.word | INSTR of instr 
    datatype quad_val = LONGS of (iword * iword) | QFLOAT of real

(* --------------------- helper functions ---------------- *)
    fun loop ~1 f = ()
      | loop n  f = (f n; loop (n-1) f)
   val wzero = W.zero
   val w2i = W.toInt
   val i2w = W.fromInt
   val uninit_val = ref (LONGS(WORD (i2w 42), WORD (i2w 42)));
   fun word2asc(lon) = 
       let val disps = [0,8,16,24]
	   fun doer disp = chr (w2i (W.andb(W.rshiftl(lon,disp),i2w 255)))
       in  implode (map doer disps)
       end
   fun iword2str (WORD a) = (W.toDecimalString a) ^ "(" ^ (word2asc a) ^ ")"
     | iword2str (INSTR i) = Formatter.makestring_fmt(Pprtl.pp_Instr' i)
   fun quad2str (QFLOAT r) = Real.toString r
     | quad2str (LONGS (a,b)) = (iword2str a) ^ " " ^ (iword2str b)


(* --------------------- basic access functions ---------------- *)

   structure Ikey : HASH_KEY =
     struct
       type hash_key = int
       fun hashVal (i : hash_key) = Word.fromInt i;
       fun sameKey (i : hash_key,j) = i = j;
     end

   structure IHash : MONO_HASH_TABLE = HashTableFn(Ikey);

   exception NotFound
   val iregs : quad_val IHash.hash_table = IHash.mkTable(ireg_size,NotFound);
   val fregs : quad_val IHash.hash_table = IHash.mkTable(freg_size,NotFound);
     
   fun reset_register() = 
     (IHash.filter (fn _ => false) iregs;
      IHash.filter (fn _ => false) fregs)

   fun ireg2int (REGI  (v,_)) = Name.var2int v
     | ireg2int (SREGI HEAPPTR) = 0
     | ireg2int (SREGI HEAPLIMIT) = 1
     | ireg2int (SREGI EXNPTR) = 2
     | ireg2int (SREGI EXNARG) = 3
     | ireg2int (SREGI STACKPTR) = 4
     | ireg2int (SREGI THREADPTR) = 5

   fun update_ireg(ri,v) = 
     let val rib = ireg2int ri
     in (((IHash.remove iregs rib; ())
	  handle NotFound => ()); IHash.insert iregs (rib,v))
     end
   fun update_freg(REGF(rfb,_),v) = 
     (((IHash.remove fregs (Name.var2int rfb); ())
       handle NotFound => ()); IHash.insert fregs (Name.var2int rfb,v))
   fun lookup_ireg ri = 
     let val rib = ireg2int ri
     in  (case (IHash.find iregs rib) of
	      SOME v => v
	    | NONE => 
		  let val defval = !uninit_val; 
		      val _ = warn ("Warning: Reading uninitialized iregister " ^ (Int.toString rib) ^ "\n");
		      val _ = update_ireg(ri,defval)
		  in defval
		  end)
     end
   fun lookup_freg(rf as REGF(rfb,_)) = 
       (case (IHash.find fregs (Name.var2int rfb)) of
	   SOME v => v
	 | NONE => 
	       let val defval = QFLOAT 0.0
		   val _ = warn ("Warning: Reading uninitialized fregister " ^ 
				 (Int.toString (Name.var2int rfb)) ^ "\n");
		   val _ = update_freg(rf,defval)
	       in defval
	       end)

   fun show prefix regs =
     IHash.appi (fn (key,v : quad_val) => print (prefix ^ (Int.toString key) ^ "   " ^
				       (quad2str v) ^ "\n"))
     regs

   fun shownonzero prefix regs =
     IHash.appi (fn (key,v : quad_val) => if (case v of
				(LONGS(WORD x,WORD y)) => not((x = wzero) andalso (y = wzero))
			      | _ => true)
			    then print (prefix ^ (Int.toString key) ^ "   " ^
					(quad2str v) ^ "\n")
			      else ())
     regs


   fun showreg() = 
       (show "i" iregs;
	show "f" fregs)

   fun showreg_nonzero() = 
       (shownonzero "i" iregs;
	shownonzero "f" fregs)



(* --------------------- layered access functions ---------------- *)

    fun lookupval_ireg(r) = 
    	case (lookup_ireg r) of
              (LONGS (ls as (WORD a, WORD b))) => (a,b)
            | _ => (error "lookupval_ireg");
    fun lookupval_freg(r) = 
    	case (lookup_freg r) of
              (QFLOAT f) => f
            | _ => (error "lookupval_freg")
    fun ireg_save iregs = map lookup_ireg iregs
    fun freg_save fregs = map lookup_freg fregs
    fun ireg_restore ([],[]) = ()
      | ireg_restore (ri::iregs,ival::ivals) = 
    	(update_ireg(ri,ival); ireg_restore(iregs,ivals))
      | ireg_restore _ = (error "ireg_restore given lists of unequal length")
    fun freg_restore ([],[]) = ()
      | freg_restore (rf::fregs,fval::fvals) = 
    	(update_freg(rf,fval); freg_restore(fregs,fvals))
      | freg_restore _ = (error "freg_restore")
    fun register_save (iregs,fregs) = (ireg_save iregs,freg_save fregs)
    fun register_restore (iregs,fregs) (ivals,fvals) = 
    	(ireg_restore (iregs,ivals); freg_restore(fregs,fvals))
    fun register_parmove src dest = register_restore dest (register_save(src))
  end; 

