functor SparcRewrite(Instr:SPARCINSTR) = 
struct
   structure I = Instr

   fun rwset(S,rw) = SortedList.uniq(map rw S)

   fun rewriteUse(mapr,instr,rs,rt) =  
   let fun R r = if mapr r = rs then rt else r 
       fun O(i as I.REG r) = if mapr r = rs then I.REG rt else i
         | O i = i
   in  case instr of
         I.LOAD{l,r,i,d,mem} => I.LOAD{l=l,r=R r,i=O i,d=d,mem=mem}
       | I.STORE{s,d,r,i,mem} => I.STORE{s=s,d=R d,r=R r,i=O i,mem=mem}
       | I.FLOAD{l,r,i,d,mem} => I.FLOAD{l=l,r=R r,i=O i,d=d,mem=mem}
       | I.FSTORE{s,d,r,i,mem} => I.FSTORE{s=s,d=d,r=R r,i=O i,mem=mem}
       | I.ARITH{a,r,i,d,cc} => I.ARITH{a=a,r=R r,i=O i,d=d,cc=cc}
       | I.SHIFT{s,r,i,d} => I.SHIFT{s=s,r=R r,i=O i,d=d}
       | I.JMP{r,i,labs,nop} => I.JMP{r=R r,i=O i,labs=labs,nop=nop}
       | I.JMPL{r,i,d,defs,uses=(A,B),nop} => 
            I.JMPL{r=R r,i=O i,d=d,defs=defs,uses=(rwset(A,R),B),nop=nop}
       | I.CALL{defs,uses=(A,B),label,nop} => 
            I.CALL{defs=defs,uses=(rwset(A,R),B),label=label,nop=nop}
       | I.SAVE{r,i,d} => I.SAVE{r=R r,i=O i,d=d}
       | I.RESTORE{r,i,d} => I.RESTORE{r=R r,i=O i,d=d}
       | I.WRY{r,i} => I.WRY{r=R r,i=O i}
       | I.Ticc{t,r,i} => I.Ticc{t=t,r=R r,i=O i}
       | I.COPY{src,dst,tmp,impl} => 
           I.COPY{src=map R src,dst=dst,tmp=tmp,impl=impl}
(*
       | I.ANNOTATION(i,a) => I.ANNOTATION(rewriteUse(mapr,i,rs,rt),a)
*)
       | _ => instr
   end

   fun rewriteDef(mapr,instr,rs,rt) =
   let fun R r = if mapr r = rs then rt else r 
       fun ea(SOME(I.Direct r)) = SOME(I.Direct(R r))
         | ea x = x 
   in  case instr of
         I.LOAD{l,r,i,d,mem} => I.LOAD{l=l,r=r,i=i,d=R d,mem=mem}
       | I.ARITH{a,r,i,d,cc} => I.ARITH{a=a,r=r,i=i,d=R d,cc=cc}
       | I.SHIFT{s,r,i,d} => I.SHIFT{s=s,r=r,i=i,d=R d}
       | I.SETHI{i,d} => I.SETHI{i=i,d=R d}
       | I.JMPL{r,i,d,defs=(A,B),uses,nop=nop} => 
            I.JMPL{r=r,i=i,d=R d,defs=(rwset(A,R),B),uses=uses,nop=nop}
       | I.CALL{defs=(A,B),uses,label,nop} => 
            I.CALL{defs=(rwset(A,R),B),uses=uses,label=label,nop=nop}
       | I.SAVE{r,i,d} => I.SAVE{r=r,i=i,d=R d}
       | I.RESTORE{r,i,d} => I.RESTORE{r=r,i=i,d=R d}
       | I.RDY{d} => I.RDY{d=R d}
       | I.COPY{src,dst,tmp,impl} => 
           I.COPY{src=src,dst=map R dst,tmp=ea tmp,impl=impl}
(*
       | I.ANNOTATION(i,a) => I.ANNOTATION(rewriteDef(mapr,i,rs,rt),a)
*)
       | _ => instr
   end

   fun frewriteUse(mapr,instr,rs,rt) = 
   let fun R r = if mapr r = rs then rt else r 
   in  case instr of
         I.FPop1{a,r,d} => I.FPop1{a=a,r=R r,d=d}
       | I.FPop2{a,r1,r2,d} => I.FPop2{a=a,r1=R r1,r2=R r2,d=d}
       | I.FCMP{cmp,r1,r2,nop} => I.FCMP{cmp=cmp,r1=R r1,r2=R r2,nop=nop}
       | I.FSTORE{s,r,i,d,mem} => I.FSTORE{s=s,r=r,i=i,d=R d,mem=mem}
       | I.JMPL{r,i,d,defs,uses=(A,B),nop} =>
           I.JMPL{r=r,i=i,d=d,defs=defs,uses=(A,rwset(B,R)),nop=nop}
       | I.CALL{defs,uses=(A,B),label,nop} =>
           I.CALL{defs=defs,uses=(A,rwset(B,R)),label=label,nop=nop}
       | I.FCOPY{src,dst,tmp,impl} => 
           I.FCOPY{src=map R src,dst=dst,tmp=tmp,impl=impl}
(*
       | I.ANNOTATION(i,a)=> I.ANNOTATION(frewriteUse(mapr,i,rs,rt),a)
*)
       | _ => instr
   end

   fun frewriteDef(mapr,instr,rs,rt) = 
   let fun R r = if mapr r = rs then rt else r 
       fun ea(SOME(I.FDirect r)) = SOME(I.FDirect(R r))
         | ea x = x 
   in  case instr of
         I.FPop1{a,r,d} => I.FPop1{a=a,r=r,d=R d}
       | I.FPop2{a,r1,r2,d} => I.FPop2{a=a,r1=r1,r2=r2,d=R d}
       | I.FLOAD{l,r,i,d,mem} => I.FLOAD{l=l,r=r,i=i,d=R d,mem=mem}
       | I.JMPL{r,i,d,defs=(A,B),uses,nop} =>
           I.JMPL{r=r,i=i,d=d,defs=(A,rwset(B,R)),uses=uses,nop=nop}
       | I.CALL{defs=(A,B),uses,label,nop} =>
           I.CALL{defs=(A,rwset(B,R)),uses=uses,label=label,nop=nop}
       | I.FCOPY{src,dst,tmp,impl} => 
           I.FCOPY{src=src,dst=map R dst,tmp=ea tmp,impl=impl}
(*
       | I.ANNOTATION(i,a)=> I.ANNOTATION(frewriteDef(mapr,i,rs,rt),a)
*)
       | _ => instr
   end
  
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:32:29  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:17:48  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:50  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1999/01/04 21:56:27  george
 *   Version 110.12
 *
 * Revision 1.2  1998/08/12 13:36:27  leunga
 *   Fixed the 2.0 + 2.0 == nan bug by treating FCMP as instrs with delay slots
 *
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
