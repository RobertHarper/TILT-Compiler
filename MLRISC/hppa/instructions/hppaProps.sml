(* hppaProps.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor HppaProps 
  (structure HppaInstr : HPPAINSTR
   structure Shuffle : HPPASHUFFLE
     sharing Shuffle.I = HppaInstr) : INSN_PROPERTIES = 
struct
  structure I = HppaInstr
  structure C = HppaInstr.C
  structure LE = LabelExp

  exception NegateConditional

  fun error msg = MLRiscErrorMsg.impossible ("HppaProps." ^ msg)

  datatype kind = IK_JUMP | IK_NOP | IK_INSTR
  datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

   (*========================================================================
    *  Instruction Kinds
    *========================================================================*)
  (* Note: BLE and BL used to implement calls are not view as branches *)
  fun instrKind(I.BCOND _)  = IK_JUMP
    | instrKind(I.BCONDI _) = IK_JUMP
    | instrKind(I.B _)      = IK_JUMP
    | instrKind(I.FBRANCH _)= IK_JUMP
    | instrKind(I.BV _)     = IK_JUMP
    | instrKind(I.BLR _)    = IK_JUMP
    | instrKind(I.NOP)      = IK_NOP
    | instrKind _	    = IK_INSTR

  fun moveInstr(I.COPY _)   = true
    | moveInstr(I.FCOPY _)  = true
    | moveInstr _ = false

  fun nop() = I.NOP

   (*========================================================================
    *  Parallel Move
    *========================================================================*)
  fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
    | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
    | moveTmpR _ = NONE

  fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
    | moveDstSrc(I.FCOPY{dst, src, ...}) = (dst, src)
    | moveDstSrc _ = error "moveDstSrc"

  fun copy{src, dst} = 
    I.COPY{src=src, dst=dst, impl=ref NONE,
	   tmp=case src of [_] => NONE | _ => SOME(I.Direct(C.newReg()))}

  fun fcopy{dst,src} = let
    fun trans r = if r >= 32 andalso r < 64 then r-32 else r
    val src = map trans src
    val dst = map trans dst
  in
    I.FCOPY{dst=dst,src=src,impl=ref NONE,
	    tmp=case src of [_] => NONE | _   => SOME(I.FDirect(C.newFreg()))}
  end

  fun splitCopies{regmap, insns} = let
    val shuffle = Shuffle.shuffle
    val shufflefp = Shuffle.shufflefp
    fun scan([],is') = rev is'
      | scan(I.COPY{dst, src, tmp,...}::is,is') =
	  scan(is, shuffle{regMap=regmap,temp=tmp,dst=dst,src=src}@is')
      | scan(I.FCOPY{dst, src, tmp,...}::is,is') =
	  scan(is, shufflefp{regMap=regmap,temp=tmp,dst=dst,src=src}@is')
      | scan(i::is, is') = scan(is, i::is')
  in scan(insns,[]) 
  end

   (*========================================================================
    *  Branches and Calls/Returns
    *========================================================================*)
  fun branchTargets(I.BCOND{t, ...})    = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.BCONDI{t, ...})   = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.B{lab, ...})      = [LABELLED lab]
    | branchTargets(I.FBRANCH{t,...})   = [LABELLED t, FALLTHROUGH]
    | branchTargets(I.BV{labs=[],...})  = [ESCAPES]
    | branchTargets(I.BV{labs,...})     = map LABELLED labs
    | branchTargets(I.BLR{labs,...})    = map LABELLED labs
    | branchTargets _ = error "branchTargets"

  fun jump label = I.B{lab=label,n=true}

  fun setTargets(I.BCOND{cmp,bc,r1,r2,t,f,n},[F,T]) =
          I.BCOND{cmp=cmp,bc=bc,r1=r1,r2=r2,t=T,f=F,n=n}
    | setTargets(I.BCONDI{cmpi,bc,i,r2,t,f,n},[F,T]) =
          I.BCONDI{cmpi=cmpi,bc=bc,i=i,r2=r2,t=T,f=F,n=n}
    | setTargets(I.B{n,...},[L]) = I.B{lab=L,n=n}
    | setTargets(I.FBRANCH{cc,n,long,f1,f2,...},[F,T]) =
          I.FBRANCH{cc=cc,t=T,f=F,n=n,long=long,f1=f1,f2=f2} 
    | setTargets(I.BV{x,b,n,...},labels) = I.BV{x=x,b=b,labs=labels,n=n}
    | setTargets(I.BLR{x,t,n,...},labels) = I.BLR{x=x,t=t,labs=labels,n=n}
    | setTargets(i,_) = i

  fun negateConditional br = let
    fun revFcond I.?    = I.!?
      | revFcond I.!<=> = I.<=>
      | revFcond I.==   = I.!=
      | revFcond I.?=   = I.!?=
      | revFcond I.!<>  = I.<>
      | revFcond I.!?>= = I.?>=
      | revFcond I.<    = I.!<
      | revFcond I.?<   = I.!?<
      | revFcond I.!>=  = I.>=
      | revFcond I.!?>  = I.?>
      | revFcond I.<=   = I.!<=
      | revFcond I.?<=  = I.!?<=
      | revFcond I.!>   = I.>
      | revFcond I.!?<= = I.?<=
      | revFcond I.>    = I.!>
      | revFcond I.?>   = I.!?>
      | revFcond I.!<=  = I.<=
      | revFcond I.!?<  = I.?<
      | revFcond I.>=   = I.!>=
      | revFcond I.?>=  = I.!?>=
      | revFcond I.!<   = I.<
      | revFcond I.!?=  = I.?=
      | revFcond I.<>   = I.!<>
      | revFcond I.!=   = I.==
      | revFcond I.!?   = I.?
      | revFcond I.<=>  = I.!<=>
  in
    case br of 
      I.BCOND{cmp,bc,r1,r2,t,f,n} => 
         I.BCOND{bc=bc, r1=r1, r2=r2, t=t, f=f, n=n,
		 cmp=case cmp of I.COMBT => I.COMBF | I.COMBF => I.COMBT}
    | I.BCONDI{cmpi,bc,i,r2,t,f,n} =>
        I.BCONDI{bc=bc, i=i, r2=r2, t=t, f=f, n=n,
		 cmpi=case cmpi of I.COMIBT => I.COMIBF | I.COMIBF => I.COMIBT}
    | I.FBRANCH{cc,f1,f2,t,f,n,long} =>
        I.FBRANCH{cc=revFcond cc,f1=f1,f2=f2,t=t,f=f,n=n,long=long} 
    | _ => raise NegateConditional
  end

  (*========================================================================
   *  Definition and use (for register allocation mainly)
   *========================================================================*)
  fun defUseR instr = let
    fun trap((I.ADDO | I.SUBO | I.SH1ADDO), d, u) = (d, u)
      | trap(_, d, u) = (d, u)
    fun trapi((I.ADDIO | I.SUBIO), d, u) = (d, u)
      | trapi(_, d, u) = (d, u)
  in
    case instr
     of I.STORE {b, r,...}          => ([],  [b,r])
      | I.LOAD {l, r1, r2, t, ...}  => ([t], [r1,r2])
      | I.LOADI {li, r, t, ...}     => ([t], [r])
      | I.ARITH {a, r1, r2, t, ...} => trap(a, [t], [r1,r2])
      | I.ARITHI {ai, r, t, ...}    => trapi(ai, [t], [r])
      | I.COMCLR{r1, r2, t, ...}    => ([t], [r1, r2])
      | I.SHIFTV {r, t, ...}        => ([t], [r])
      | I.SHIFT {r, t, ...}         => ([t], [r])
      | I.BCOND {r1, r2, ...}       => ([],  [r1,r2])
      | I.BCONDI {r2, ...} 	    => ([],  [r2])
      | I.BV {x, b, ...}	    => ([],  [x,b])
      | I.BLR{x, t, ...}            => ([t], [x])
      | I.BL{defs, uses, ...}       => (#1 defs, #1 uses)
      | I.BLE{t, b, defs, uses, ...}=> (31 :: t :: #1 defs, b :: #1 uses)
      | I.LDIL{i, t}		    => ([t], [])
      | I.LDO{b, t, ...}	    => ([t], [b])
      | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
      | I.COPY{dst, src, ...}       => (dst, src)
      | I.MTCTL{r, t}		    => ([],  [r])
      | I.FSTORE {b, ...}	    => ([],  [b])
      | I.FSTOREX {b, x, ...}  	    => ([],  [b,x])
      | I.FLOAD {b, ...}	    => ([],  [b])
      | I.FLOADX{b, x, ...} 	    => ([],  [b,x])
      | _   => ([],[])
  end

  fun defUseF instr = 
    case instr
      of I.FSTORE {r, ...}  	   => ([],  [r])
       | I.FSTOREX{r, ...}	   => ([],  [r])
       | I.FLOAD{t, ...}	   => ([t], [])
       | I.FLOADX{t, ...}	   => ([t], [])
       | I.FARITH {r1, r2, t, ...} => ([t], [r1,r2])
       | I.FUNARY {f, t, ...}      => ([t], [f])
       | I.FBRANCH{f1, f2,...}	   => ([],  [f1, f2])
       | I.BL{defs, uses, ...}     => (#2 defs, #2 uses)
       | I.BLE{defs, uses, ...}    => (#2 defs, #2 uses)
       | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...} => (f::dst, src)
       | I.FCOPY{dst, src, ...}    => (dst, src)
       | _ => ([],[])

  fun defUse C.GP = defUseR
    | defUse C.FP = defUseF
    | defUse _ = error "defUse"
end



(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:10  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:16:25  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:08:04  pscheng
# *** empty log message ***
#
 * Revision 1.4  1998/10/06 14:04:35  george
 *   The instruction sequence FCMP, FTEST, FBCC is being replaced
 *   by the composite instruction FBRANCH.  This makes scheduling and
 *   other tasks easier.  Also, added BLR and BL in the instruction set.
 * 							[leunga]
 *
 * Revision 1.3  1998/05/25 15:10:58  george
 *   Fixed RCS keywords
 *
 *)
