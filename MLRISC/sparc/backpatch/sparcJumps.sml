(* sparcJumps.sml --- information to resolve jumps. 
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
functor SparcJumps
  (structure Instr:SPARCINSTR
   structure Shuffle:SPARCSHUFFLE
      sharing Shuffle.I = Instr) : SDI_JUMPS = 
struct
  structure I = Instr
  structure C = Instr.C
  structure LE = LabelExp
  structure Const = I.Constant

  fun error msg = MLRiscErrorMsg.impossible ("SparcJumps." ^ msg)

  val branchDelayedArch = true

  fun minSize(I.COPY _)  = 0
    | minSize(I.FCOPY _) = 0
    | minSize(I.Bicc{nop=true,...}) = 8
    | minSize(I.FBfcc{nop=true,...}) = 8
    | minSize(I.JMP{nop=true,...}) = 8
    | minSize(I.JMPL{nop=true,...}) = 8
    | minSize(I.CALL{nop=true,...}) = 8
    | minSize(I.RET{nop=true,...}) = 8
    | minSize(I.FCMP{nop=true,...}) = 8
    | minSize(I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd),...}) = 8
(*
    | minSize(I.ANNOTATION(i,_)) = minSize i
*)
    | minSize _          = 4

  fun maxSize (I.COPY _)   = error "maxSize:COPY"
    | maxSize (I.FCOPY _)  = error "maxSize:FCOPY"
    | maxSize(I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd),...}) = 8
(*
    | maxSize (I.ANNOTATION(i,_)) = maxSize i
*)
    | maxSize _		   = 4

  fun immed13 n = ~4096 <= n andalso n < 4096
  fun immed22 n = ~0x200000 <= n andalso n < 0x1fffff
  fun immed30 n = ~0x4000000 <= n andalso n < 0x3ffffff

  fun isSdi instr = 
  let fun oper(I.IMMED n) = false
        | oper(I.REG _) = false
        | oper(I.HI _) = false
        | oper(I.LO _) = false
        | oper(I.LAB _) = true
        | oper(I.CONST _) = true
  in  case instr of 
        I.ARITH{i,...} => oper i
      | I.SHIFT{i,...} => oper i
      | I.LOAD{i,...} => oper i
      | I.STORE{i,...} => oper i
      | I.FLOAD{i,...} => oper i
      | I.FSTORE{i,...} => oper i
      | I.JMPL{i,...} => oper i
      | I.JMP{i,...} => oper i
      | I.CALL _ => true
      | I.Bicc _ => true
      | I.FBfcc _ => true
      | I.Ticc{i,...} => oper i
      | I.WRY{i,...} => oper i
      | I.COPY _ => true
      | I.FCOPY _ => true
      | I.SAVE{i,...} => oper i
      | I.RESTORE{i,...} => oper i
(*
      | I.ANNOTATION(i,_) => isSdi i
*)
      | _ => false
  end

  fun instrLength([],n) = n
    | instrLength(I.FPop1{a=(I.FMOVd | I.FNEGd | I.FABSd),...}::is,n) =
        instrLength(is,n+8)
    | instrLength(_::is,n) = instrLength(is,n+4)

  fun sdiSize(instr, regmap, labMap, loc) = 
  let fun oper(I.IMMED n,_) = 4
        | oper(I.REG _,_) = 4
        | oper(I.HI _,_) = 4
        | oper(I.LO _,_) = 4
        | oper(I.LAB lexp,hi) = if immed13(LE.valueOf lexp) then 4 else hi
        | oper(I.CONST c,hi) = if immed13(Const.valueOf c) then 4 else hi
      fun displacement lab = ((labMap lab) - loc) div 4
      fun branch lab = if immed22(displacement lab) then 4 else 16
      fun call lab = if immed30(displacement lab) then 4 else 20
      fun delaySlot false = 0
        | delaySlot true = 4
      fun size instr =
      case instr of
        I.ARITH{a=I.OR,r=0,i,...} => oper(i,8)
      | I.ARITH{i,...} => oper(i,12)
      | I.SHIFT{i,...} => oper(i,12)
      | I.LOAD{i,...} => oper(i,12)
      | I.STORE{i,...} => oper(i,12)
      | I.FLOAD{i,...} => oper(i,12)
      | I.FSTORE{i,...} => oper(i,12)
      | I.Ticc{i,...} => oper(i,12)
      | I.SAVE{i,...} => oper(i,12)
      | I.RESTORE{i,...} => oper(i,12)
      | I.JMPL{i,nop,...} => oper(i,12) + delaySlot nop
      | I.JMP{i,nop,...} => oper(i,12) + delaySlot nop
      | I.Bicc{label,nop,...} => branch label + delaySlot nop
      | I.FBfcc{label,nop,...} => branch label + delaySlot nop
      | I.CALL{label,...} => call label
      | I.WRY{i,...} => oper(i,12)
      | I.COPY{impl=ref(SOME l), ...} => 4 * length l
      | I.FCOPY{impl=ref(SOME l), ...} => instrLength(l,0)
      | I.COPY{dst, src, impl, tmp} =>
        let val instrs = 
          Shuffle.shuffle{regMap=Intmap.map regmap,temp=tmp,dst=dst,src=src}
        in impl := SOME(instrs); 4 * length instrs
        end
      | I.FCOPY{dst, src, impl, tmp} => 
        let val instrs = 
          Shuffle.shufflefp{regMap=Intmap.map regmap,temp=tmp,dst=dst,src=src}
        in impl := SOME instrs; instrLength(instrs,0)
        end
(*
      | I.ANNOTATION(i,_) => size i
*)
      | _ => error "sdiSize"
  in  size instr
  end

  fun split22_10 n =
  let val w = Word32.fromInt n
  in  {hi=Word32.toInt(Word32.>>(w,0w10)),
       lo=Word32.toInt(Word32.andb(w,0wx3ff))
      }
  end

  fun split(I.CONST c) = split22_10(Const.valueOf c)
    | split(I.LAB lexp) = split22_10(LE.valueOf lexp)
    | split _ = error "split"

  (* Expand the immediate constant into two instructions *)
  fun expandImm(immed,instr) = 
      let val {lo,hi} = split immed
      in  [I.SETHI{i=hi,d=C.asmTmpR},
           I.ARITH{a=I.OR,cc=false,r=C.asmTmpR,i=I.IMMED lo,d=C.asmTmpR},
           instr
          ]
      end

  (* Expand a span dependent instruction *)
  fun expand(I.COPY{impl=ref(SOME instrs),...},_) = instrs
    | expand(I.FCOPY{impl=ref(SOME instrs),...},_) = instrs
    | expand(instr,4) = [instr]
    | expand(I.ARITH{a=I.OR,r=0,i,d,cc},8) =
        let val {lo,hi} = split i
        in  [I.SETHI{i=hi,d=C.asmTmpR},
             I.ARITH{a=I.OR,cc=cc,r=C.asmTmpR,i=I.IMMED lo,d=d}
            ]
        end
    | expand(I.ARITH{a,r,i,d,cc},12) =
        expandImm(i,I.ARITH{a=a,r=r,i=I.REG C.asmTmpR,d=d,cc=cc})
    | expand(I.SHIFT{s,r,i,d},12) =
        expandImm(i,I.SHIFT{s=s,r=r,i=I.REG C.asmTmpR,d=d})
    | expand(I.SAVE{r,i,d},12) =
        expandImm(i,I.SAVE{r=r,i=I.REG C.asmTmpR,d=d})
    | expand(I.RESTORE{r,i,d},12) =
        expandImm(i,I.RESTORE{r=r,i=I.REG C.asmTmpR,d=d})
    | expand(I.LOAD{l,r,i,d,mem},12) =  
        expandImm(i,I.LOAD{l=l,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
    | expand(I.STORE{s,r,i,d,mem},12) =
        expandImm(i,I.STORE{s=s,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
    | expand(I.FLOAD{l,r,i,d,mem},12) = 
        expandImm(i,I.FLOAD{l=l,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
    | expand(I.FSTORE{s,r,i,d,mem},12) = 
        expandImm(i,I.FSTORE{s=s,r=r,i=I.REG C.asmTmpR,d=d,mem=mem})
    | expand(i as I.JMPL _,8) = [i]
    | expand(i as I.JMP _,8) = [i]
    | expand(i as I.Bicc _,8) = [i]
    | expand(i as I.FBfcc _,8) = [i]
    | expand(I.JMPL{r,i,d,defs,uses,nop},(12 | 16)) = 
        expandImm(i,I.JMPL{r=r,i=I.REG C.asmTmpR,d=d,defs=defs,uses=uses,
                           nop=nop})
    | expand(I.JMP{r,i,labs,nop},(12 | 16)) = 
        expandImm(i,I.JMP{r=r,i=I.REG C.asmTmpR,labs=labs,nop=nop})
    | expand(I.Ticc{t,r,i},12) =
        expandImm(i,I.Ticc{t=t,r=r,i=I.REG C.asmTmpR})
        (* 
         * The sparc uses 22bits signed extended displacement offsets
         * Let's hope it's enough
         *)
    | expand(I.Bicc{b,a,label,nop},_) = error "Bicc"  
    | expand(I.FBfcc{b,a,label,nop},_) = error "FBfcc" 
    | expand(I.WRY{r,i},12) = expandImm(i,I.WRY{r=r,i=I.REG C.asmTmpR})
(*
    | expand(I.ANNOTATION(i,a),size) = expand(i,size)
*)
    | expand _ = error "expand"

end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:27  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:38  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:36  pscheng
# *** empty log message ***
#
 * Revision 1.2  1998/08/12 13:36:23  leunga
 *   Fixed the 2.0 + 2.0 == nan bug by treating FCMP as instrs with delay slots
 *
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
