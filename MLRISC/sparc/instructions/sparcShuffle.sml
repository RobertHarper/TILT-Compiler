functor SparcShuffle(I:SPARCINSTR) = struct
  structure I = I
  structure W = Word32
  structure Shuffle = Shuffle(I)
  type t = {regMap:int->int, temp:I.ea option, dst:int list, src:int list}

  fun error msg = MLRiscErrorMsg.impossible ("SparcShuffle." ^ msg)
  val mem = I.Region.memory

  fun immed13 n = ~4096 <= n andalso n < 4096
  (* split into 22 high bits/10 low bits *)
  fun split n =
      let val w = W.fromInt n
      in  {hi=W.toInt(W.~>>(w,0w10)),lo=W.toInt(W.andb(w,0wx3ff))} end
  fun offset disp = 
      if immed13 disp then ([],I.IMMED disp)
      else let val {lo,hi} = split disp
               val r = I.C.newReg()
           in  ([I.SETHI{i=hi,d=r},
                 I.ARITH{a=I.OR,r=r,i=I.IMMED lo,d=r,cc=false}],
                I.REG r) 
           end

  fun move{src=I.Direct rs, dst=I.Direct rt} = 
       [I.ARITH{a=I.OR, cc=false, r=0, i=I.REG rs, d=rt}]
    | move{src=I.Displace{base, disp}, dst=I.Direct rt} =
       let val (insns,i) = offset disp
       in  insns@[I.LOAD{l=I.LD, r=base, i=i, d=rt, mem=mem}] end
    | move{src=I.Direct rs, dst=I.Displace{base, disp}} = 
       let val (insns,i) = offset disp
       in  insns@[I.STORE{s=I.ST, r=base, i=i, d=rs, mem=mem}] end
    | move _ = error "move"

  fun fmove{src=I.FDirect fs, dst=I.FDirect fd} = 
        [I.FPop1{a=I.FMOVd, r=fs, d=fd}] 
    | fmove{src=I.Displace{base, disp}, dst=I.FDirect ft} = 
       let val (insns,i) = offset disp
       in  insns@[I.FLOAD{l=I.LDDF, r=base, i=i, d=ft, mem=mem}] end
    | fmove{src=I.FDirect fs, dst=I.Displace{base, disp}} = 
       let val (insns,i) = offset disp
       in  insns@[I.FSTORE{s=I.STDF, r=base, i=i, d=fs, mem=mem}] end
    | fmove _ = error "fmove"

  val shuffle = Shuffle.shuffle{mvInstr = move, ea=I.Direct}

  val shufflefp = Shuffle.shuffle {mvInstr=fmove, ea=I.FDirect}
end


(*
 * $Log$
# Revision 1.1  99/02/17  21:17:45  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:45  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
