(* alpha32Jumps.sml --- information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
functor Alpha32Jumps
  (structure Instr : ALPHA32INSTR
   structure Shuffle : ALPHA32SHUFFLE
      sharing Shuffle.I = Instr) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = I.C
  structure Const = I.Constant
  structure LE = LabelExp

  fun error msg = MLRiscErrorMsg.impossible ("Alpha32Jumps." ^ msg)

  val branchDelayedArch = false

  fun isSdi(I.DEFFREG _) 	          = true
    | isSdi(I.LDA{d=I.CONSTop _, ...})    = true
    | isSdi(I.LDA{d=I.LABop _, ...})      = true
    | isSdi(I.LOAD{d=I.CONSTop _, ...})   = true
    | isSdi(I.STORE{d=I.CONSTop _, ...})  = true
    | isSdi(I.FSTORE{d=I.CONSTop _, ...}) = true
    | isSdi(I.FLOAD{d=I.CONSTop _, ...})  = true
    | isSdi(I.OPERATE{rb=I.CONSTop _, ...})= true
    | isSdi(I.OPERATEV{rb=I.CONSTop _, ...})= true
    | isSdi(I.COPY _)			  = true
    | isSdi(I.FCOPY _)			  = true
    | isSdi _            		  = false

  fun minSize(I.DEFFREG _) = 0
    | minSize(I.COPY _)    = 0
    | minSize(I.FCOPY _)   = 0
    | minSize _            = 4

  (* max Size is not used for the alpha span dependency analysis. *)
  fun maxSize _ = error "maxSize"

  fun immed16 n =  ~32768 <= n andalso n < 32768 
  fun im16load n = if immed16 n then 4 else 8
  fun im16Oper c = if immed16 (Const.valueOf c) then 4 else 12

  fun sdiSize(I.DEFFREG _, _, _, _) = 0
    | sdiSize(I.LDA{d=I.LABop le, ...}, _, _, _) = im16load(LE.valueOf le)
    | sdiSize(I.LDA{d=I.CONSTop c, ...}, _, _, _) = im16load(Const.valueOf c)
    | sdiSize(I.LOAD{d=I.CONSTop c, ...}, _, _, _) = im16Oper c
    | sdiSize(I.STORE{d=I.CONSTop c, ...}, _, _, _) = im16Oper c
    | sdiSize(I.FLOAD{d=I.CONSTop c, ...}, _, _, _) = im16Oper c
    | sdiSize(I.FSTORE{d=I.CONSTop c, ...}, _, _, _) = im16Oper c
    | sdiSize(I.OPERATE{rb=I.CONSTop c, ...}, _, _, _) = im16Oper c
    | sdiSize(I.OPERATEV{rb=I.CONSTop c, ...}, _, _, _) = im16Oper c
    | sdiSize(I.COPY{impl=ref(SOME l),...}, _, _, _) = 4 * length l
    | sdiSize(I.FCOPY{impl=ref(SOME l),...}, _, _, _) = 4 * length l
    | sdiSize(I.COPY{dst, src, impl as ref NONE, tmp}, regmap, _, _) = let
	val lookup = Intmap.map regmap
	val instrs = Shuffle.shuffle{regMap=lookup, temp=tmp, dst=dst, src=src}
      in  impl := SOME instrs;  4 * length instrs
      end
    | sdiSize(I.FCOPY{dst, src, impl as ref NONE, tmp}, regmap, _, _) = let
	val lookup = Intmap.map regmap
        val instrs = 
	  Shuffle.shufflefp{regMap=lookup, temp=tmp, dst=dst, src=src}
      in impl := SOME(instrs); 4 * length instrs
      end
    | sdiSize _ = error "sdiSize"

 (* NOTE: All sdis must use a dedicated physical register as a 
  * temporaries, since sdi expansion is performed after register 
  * allocation.
  *)
  val zeroR = 31

  fun expand(instr, size) = let
    fun load(ldClass, ldOp, r, b, d as I.CONSTop c, mem) = 
      (case size 
       of 4 => [ldClass{ldOp=ldOp, r=r, b=b, d=I.IMMop(Const.valueOf c), mem=mem}]
        | 12 => let
            val instrs = expand(I.LDA{r=r, b=b, d=d}, 8)
          in instrs @ [ldClass{ldOp=ldOp, r=r, b=r, d=I.IMMop 0, mem=mem}]
          end)

    fun store(stClass, stOp, r, b, d as I.CONSTop c, mem) = 
      (case size 
       of 4 => [stClass{stOp=stOp, r=r, b=b, d=I.IMMop(Const.valueOf c), mem=mem}]
        | 12 => let
	    val instrs = expand(I.LDA{r=C.asmTmpR, b=b, d=d}, 8)
	  in instrs @ [stClass{stOp=stOp, r=r, b=C.asmTmpR, d=I.IMMop 0, mem=mem}]
	  end)

    fun operate(opClass, oper, ra, rb as I.CONSTop c, rc) =
      (case size
       of 4 => [opClass{oper=oper, ra=ra, rb=I.IMMop(Const.valueOf c), rc=rc}]
	| 12 => let
	    val instrs = expand(I.LDA{r=C.asmTmpR, b=zeroR, d=rb}, 8)
	  in instrs @ [opClass{oper=oper, ra=ra, rb=I.REGop C.asmTmpR, rc=rc}]
	  end)
  in
    case instr
    of I.DEFFREG _ => []
     | I.LDA{r=rd, b=rs, d=I.LABop le} => 
       (case size of
	  4  => [I.LDA{r=rd, b=rs, d=I.LOLABop le}]
	| 8  => [I.LDA{r=rd, b=rs, d=I.LOLABop le},
		 I.LDAH{r=rd, b=rd, d=I.HILABop le}]
	| _  => error "expand:LDA")

    | I.LDA{r=rd, b=rs, d=I.CONSTop c} =>
       (case size of
	  4 => [I.LDA{r=rd, b=rs, d=I.IMMop(Const.valueOf c)}]
	| 8 => let
	      val itow = Word.fromInt

	      val  n = Const.valueOf c
	      val low = Word.toIntX(Word.andb(itow n, 0w65535))
	      val high = n div 65536
	      val (lowsgn, highsgn) =			
		 if low <= 32767 then (low, high) else (low -65536, high+1)
	  in
	    [I.LDA{r=rd, b=rs, d=I.IMMop lowsgn},
	     I.LDAH{r=rd, b=rd, d=I.IMMop highsgn}]
	  end)
    | I.COPY{impl=ref(SOME instrs),...} => instrs
    | I.FCOPY{impl=ref(SOME instrs),...} => instrs
    | I.LOAD{ldOp, r, b, d, mem} => load(I.LOAD, ldOp, r, b, d, mem)
    | I.FLOAD{ldOp, r, b, d, mem} => load(I.FLOAD, ldOp, r, b, d, mem)
    | I.STORE{stOp, r, b, d, mem} => store(I.STORE, stOp, r, b, d, mem)
    | I.FSTORE{stOp, r, b, d, mem} => store(I.FSTORE, stOp, r, b, d, mem)
    | I.OPERATE{oper, ra, rb, rc} => operate(I.OPERATE, oper, ra, rb, rc)
    | I.OPERATEV{oper, ra, rb, rc} => operate(I.OPERATEV, oper, ra, rb, rc)
    | _ => error "expand"
  end

end


(*
 * $Log$
# Revision 1.1  99/02/17  21:15:08  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:38  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
