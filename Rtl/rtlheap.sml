functor MakeHeap(structure Rtl : RTL
		 structure Registerset : REGISTERSET
		 sharing type Rtl.instr = Registerset.instr
		 and type Rtl.regi = Registerset.regi
		 and type Rtl.regf = Registerset.regf
		 val hs : int) : RTLHEAP = 
struct

  open Rtl Registerset

  val heapsize = hs;
  val error = fn s => Util.error "rtlheap.sml" s

  structure W = TilWord32
  type w32 = W.word

  (* positive shift disp is left shift; 
   shifting left is same for unsigned/signed *)
  fun bitshift(v,disp) = if (disp >= 0) 
			     then W.lshift(v,disp)
			 else W.rshiftl(v,~disp)

  val i2w = W.fromInt
  val w2i = W.toInt
  val wzero = i2w 0
  val heap  = Array.array(heapsize, (LONGS(WORD wzero, WORD wzero)));
  val uninit_val = ref (i2w 42);
  fun reset_heap() = 
    let	fun loop ~1 = ()
	  | loop n = (Array.update(heap,n,(LONGS(WORD (!uninit_val),WORD (!uninit_val))));
		      loop (n-1))
    in	loop(heapsize-1)
    end
  
  fun getquad(add,align) = 
    let val _ = if (W.umod(add,i2w align) = wzero)
		  then () else error ("Heap: Unaligned access on "
						 ^(Int.toString align)^" boundary.")
    in w2i(W.udiv(add,i2w 8))
    end

  fun weq (a : W.word, b) = a = b;
  fun lookupquad(add) = 
    let val whichquad = getquad(add,8)
    in  Array.sub(heap,whichquad)
    end

  fun storequad(add,quad) = 
    let val whichquad = getquad(add,8)
    in  Array.update(heap,whichquad,quad)
    end;

  fun lookuplong(add) = 
    let val whichquad = getquad(add,4)
        val low = weq(W.umod(add,i2w 8),wzero)
    in  case (Array.sub(heap,whichquad)) of
  	LONGS (WORD a,WORD b) => (if low then b else a)
        | _  => (error "lookuplong")
    end;
  fun storelong(add,l) = 
    let val whichquad = getquad(add,4)
        val low = weq(W.umod(add,i2w 8),wzero)
    in  case (Array.sub(heap,whichquad)) of
  	LONGS (WORD a,WORD b) => Array.update(heap,whichquad,
  			LONGS(if low then (WORD a,WORD l) else (WORD l,WORD b)))
        | q  => (error ("storelong: " ^ (quad2str q)))
    end;
  fun lookupinstr(add) = 
    let val whichquad = getquad(add,4)
        val low = weq(W.umod(add,i2w 8),wzero)
    in  case (Array.sub(heap,whichquad)) of
  	LONGS (a,b) => (case (if low then b else a) of 
			  INSTR i => i
		        | _ => (error "lookupinstr, Not INSTR"))
        | _  => (error "lookupintr, Not LONGS")
    end;
  fun storeinstr(add,i) = 
    let val whichquad = getquad(add,4)
        val low = weq(W.umod(add,i2w 8),wzero)
    in  case (Array.sub(heap,whichquad)) of
  	LONGS (a, b) => Array.update(heap,whichquad,
  			LONGS(if low then (a,INSTR i) else (INSTR i, b)))
        | _  => (error "storeinstr")
    end;
  fun lookupfloat(add) = 
    let val whichquad = getquad(add,8)
    in  case (Array.sub(heap,whichquad)) of
          QFLOAT f => f
        | LONGS ls => (error "lookupfloat")
    end;
  fun storefloat(add,f) = 
    let val whichquad = getquad(add,8)
    in  Array.update(heap,whichquad,QFLOAT f)
    end;

  fun storebyte(add,b) = 
    let val whichquad = getquad(add,1)
        val low = W.ult(W.umod(add,i2w 8),i2w 4)
        val pos = w2i(W.umod(add,i2w 4))
		(* XXX this assumes big/little endian *)
        val shiftamount = 8 * pos;
	val mask = bitshift(i2w 255 , shiftamount);
	val nmask = W.notb mask;
        val shiftedb = bitshift(i2w b,shiftamount);
        fun modbyte(w) = W.orb(W.andb(nmask,w),shiftedb)
    in  case (Array.sub(heap,whichquad)) of
  	LONGS (WORD a,WORD b) => Array.update(heap,whichquad,
  			LONGS(if low then (WORD a,WORD (modbyte b))
				else (WORD (modbyte a),WORD b)))
        | _  => (error "storebyte")
    end;

     
    local
      (* i is position in quad array *)
      fun loop (i,0) = ()
        | loop (i,sz) = 
	  let val quad = Array.sub(heap,i)
	      val msg = (Int.toString (i * 8)) ^ ":  " ^ 
		        (quad2str quad) ^ "\n"
	  in (print msg; loop(i+1,sz-1))
	  end
    in
	fun showheap(start,sz) = 
	    let val quadpos = w2i(W.udiv(start,i2w 8))
	    in loop(quadpos,sz)
	    end
    end
end;
