functor SparcMCEmitter
  (structure Instr : SPARCINSTR
   structure Assembler : EMITTER_NEW where I = Instr
   structure CodeString : CODE_STRING) : EMITTER_NEW =
struct
   structure I = Instr
   structure P = Assembler.P

   val <<  = Word.<<
   val >>  = Word.>>
   val ~>> = Word.~>>
   val ++  = Word.orb
   val &   = Word.andb
   val itow = Word.fromInt
   
   infix << >> ~>> ++ &

   fun error msg = MLRiscErrorMsg.impossible ("SparcMCEmitter."^msg)

   val loc = ref 0

   fun emitByte n =  
   let val i = !loc 
       fun wtob w = Word8.fromLargeWord(Word.toLargeWord w)
   in  loc := i+1; CodeString.update(i,wtob n) end

   fun emitbyte w8 = 
   let val i = !loc
   in  loc := i+1; CodeString.update(i,w8) end

   fun emitWord w = (emitByte((w >> 0w8) & 0wxff); emitByte(w & 0wxff))
  
   fun defineLabel lab = ()
   fun pseudoOp pOp = P.emitValue{pOp=pOp, loc= !loc, emit=emitbyte}
   fun comment msg = ()
   fun init n = (CodeString.init n; loc:=0)

   fun emitInstr(instr,regmap) = 
   let 
       val rMap = Intmap.map regmap
       fun rNum r = itow(rMap r)
       val fNum = rNum

       fun RRR(op1,op3,rd,rs1,rs2) =
           (emitWord((op1 << 0w14) ++
                     (rd << 0w9) ++ 
                     (op3 << 0w3) ++
                     (rs1 >> 0w2));
            emitWord(((rs1 & 0w3) << 0w14) ++ rs2)
           )

       fun RIR(op1,op3,rd,rs1,immed13) =
           (emitWord((op1 << 0w14) ++
                     (rd << 0w9) ++ 
                     (op3 << 0w3) ++
                     (rs1 >> 0w2));
            emitWord(((rs1 & 0w3) << 0w14) ++ 0wx2000 
                     ++ (immed13 & 0wx1fff))
           )

       fun emitOp(op1,op3,r,i,d) =
       let fun hi22 n = (itow n) ~>> 0w10
           fun lo10 n = (itow n) & 0wx3ff
       in
           case i of
              I.REG r2   => RRR(op1,op3,d,r,rNum r2)
           |  I.IMMED i  => RIR(op1,op3,d,r,itow i)
           |  I.LAB lexp => RIR(op1,op3,d,r,itow(LabelExp.valueOf lexp))
           |  I.HI lexp  => RIR(op1,op3,d,r,hi22(LabelExp.valueOf lexp))
           |  I.LO lexp  => RIR(op1,op3,d,r,lo10(LabelExp.valueOf lexp))
           |  I.CONST c  => RIR(op1,op3,d,r,itow(I.Constant.valueOf c))
       end

       fun emitIop(op1,op3,r,i,d) = emitOp(op1,op3,rNum r,i,rNum d)
       fun emitFop(op1,op3,r,i,d) = emitOp(op1,op3,rNum r,i,fNum d)

       fun arith(op3,r,i,d,cc) =
           emitIop(0w2,if cc then op3 ++ 0wx10 else op3,r,i,d)

       fun branch(cond,a,op3,label) = 
       let val disp = itow(((Label.addrOf label) - !loc) div 4)
           val a = if a then 0wx2000 else 0w0
       in  emitWord(a ++ (cond << 0w9) ++ (op3 << 0w6) ++ 
                    ((disp ~>> 0w16) & 0wx3f));
           emitWord(disp & 0wxffff)
       end

       fun trap(cond,r,i) = emitOp(0w2,0w58,rNum r,i,cond) 

       fun sethi(rd,i) =
       let val i = itow i
       in  emitWord((rNum rd << 0w9) ++ 0wx100 ++ ((i ~>> 0w16) & 0wx3f));
           emitWord(i & 0wxffff)
       end

       fun call label = 
       let val disp = itow(((Label.addrOf label) - !loc) div 4)
       in  emitWord(0wx4000 ++ (disp ~>> 0w16));
           emitWord(disp & 0wxffff)
       end

       fun farith(opf,rs1,rs2,rd) =
       let val r1 = rNum rs1  
       in  emitWord(0wx81a0 ++ (fNum rd << 0w9) ++ (r1 >> 0w2));
           emitWord(((r1 & 0w3) << 0w14) ++ (opf << 0w5) ++ (fNum rs2))
       end

       fun funary(opf,rs,rd) = 
          (emitWord(0wx81a0 ++ (fNum rd << 0w9));
           emitWord((opf << 0w5) ++ (fNum rs))
          ) 

       fun fcmp(opf,rs1,rs2) =
       let val r1 = rNum rs1  
       in  emitWord(0wx81a8 ++ (r1 >> 0w2));
           emitWord(((r1 & 0w3) << 0w14) ++ (opf << 0w5) ++ (fNum rs2))
       end

       fun cond I.BA   = 0w8
         | cond I.BN   = 0w0 
         | cond I.BNE  = 0w9 
         | cond I.BE   = 0w1 
         | cond I.BG   = 0w10 
         | cond I.BLE  = 0w2 
         | cond I.BGE  = 0w11 
         | cond I.BL   = 0w3 
         | cond I.BGU  = 0w12 
         | cond I.BLEU = 0w4 
         | cond I.BCC  = 0w13 
         | cond I.BCS  = 0w5 
         | cond I.BPOS = 0w14 
         | cond I.BNEG = 0w6 
         | cond I.BVC  = 0w15 
         | cond I.BVS  = 0w7 

       fun fcond I.FBA   = 0w8
         | fcond I.FBN   = 0w0
         | fcond I.FBU   = 0w7
         | fcond I.FBG   = 0w6
         | fcond I.FBUG  = 0w5
         | fcond I.FBL   = 0w4
         | fcond I.FBUL  = 0w3
         | fcond I.FBLG  = 0w2
         | fcond I.FBNE  = 0w1
         | fcond I.FBE   = 0w9
         | fcond I.FBUE  = 0w10
         | fcond I.FBGE  = 0w11
         | fcond I.FBUGE = 0w12
         | fcond I.FBLE  = 0w13
         | fcond I.FBULE = 0w14
         | fcond I.FBO   = 0w15

       fun delaySlot false = ()
         | delaySlot true = (emitWord(0wx100); emitWord(0w0))
          
   in  case instr of
         I.LOAD{l,r,i,d,...} =>
           emitIop(0w3,case l of I.LDSB => 0w9
                               | I.LDSH => 0w10
                               | I.LDUB => 0w1
                               | I.LDUH => 0w2
                               | I.LD   => 0w0
                               | I.LDD  => 0w3,r,i,d)
       | I.FLOAD{l,r,i,d,...} =>
           emitFop(0w3,case l of I.LDF => 0w32
                               | I.LDDF => 0w35
                               | I.LDFSR => 0w33,r,i,d)
       | I.STORE{s,r,i,d,...} =>
           emitIop(0w3,case s of I.STB => 0w5
                               | I.STH => 0w6 
                               | I.ST  => 0w4 
                               | I.STD => 0w7,r,i,d)
       | I.FSTORE{s,r,i,d,...} =>
           emitFop(0w3,case s of I.STF => 0w36
                               | I.STDF => 0w39
                               | I.STFSR => 0w37,r,i,d)
       | I.SETHI{d,i} => sethi(d,i)
       | I.ARITH{a,r,i,d,cc} =>
           arith(case a of I.AND  => 0w1
                         | I.ANDN => 0w5
                         | I.OR   => 0w2
                         | I.ORN  => 0w6
                         | I.XOR  => 0w3
                         | I.XNOR => 0w7
                         | I.ADD  => 0w0
                         | I.SUB  => 0w4
                         | I.UMUL => 0w10
                         | I.SMUL => 0w11
                         | I.UDIV => 0w14
                         | I.SDIV => 0w15
                         | I.TADD => 0w32
                         | I.TADDTV => 0w34,
                r,i,d,cc)
       | I.SHIFT{s,r,i,d} =>
           arith(case s of I.SLL => 0w37
                         | I.SRL => 0w38
                         | I.SRA => 0w39,r,i,d,false)
       | I.Bicc{b,a,label,nop} => (branch(cond b,a,0w2,label); delaySlot nop)
       | I.FBfcc{b,a,label,nop} => (branch(fcond b,a,0w6,label); delaySlot nop)
       | I.JMP{r,i,nop,...} => (emitIop(0w2,0w56,r,i,0); delaySlot nop)
       | I.JMPL{r,i,d,nop,...} => (emitIop(0w2,0w56,r,i,d); delaySlot nop)
       | I.CALL{label,nop,...} => (call(label); delaySlot nop)
       | I.RET{leaf,nop} => 
           (emitIop(0w2,0w56,if leaf then 31 else 15,I.IMMED 8,0);
            delaySlot nop)
       | I.Ticc{t,r,i} => trap(cond t,r,i)
       | I.FPop1{a=I.FMOVd,r,d} => 
         let val r' = rMap r and d' = rMap d
         in  funary(0w1,r',d'); funary(0w1,r'+1,d'+1) end
       | I.FPop1{a=I.FNEGd,r,d} => 
         let val r' = rMap r and d' = rMap d
         in  funary(0w5,r',d'); funary(0w1,r'+1,d'+1) end
       | I.FPop1{a=I.FABSd,r,d} => 
         let val r' = rMap r and d' = rMap d
         in  funary(0w9,r',d'); funary(0w1,r'+1,d'+1) end
       | I.FPop1{a,r,d} => 
            funary(case a of
                   I.FiTOs => 0w196
                 | I.FiTOd => 0w200
                 | I.FsTOi => 0w209
                 | I.FdTOi => 0w210
                 | I.FsTOd => 0w201
                 | I.FdTOs => 0w198 
                 | I.FMOVs => 0w1
                 | I.FNEGs => 0w5
                 | I.FABSs => 0w9
                 | I.FSQRTs => 0w41
                 | I.FSQRTd => 0w42
                 | _ => error "FPop1",r,d)
       | I.FPop2{a,r1,r2,d} =>
            farith(
             case a of
               I.FADDd => 0w66
             | I.FSUBd => 0w70
             | I.FMULd => 0w74
             | I.FDIVd => 0w78
             | I.FADDs => 0w65
             | I.FSUBs => 0w69
             | I.FMULs => 0w73
             | I.FDIVs => 0w77
             | I.FsMULd => 0w57,
             r1,r2,d)
       | I.FCMP{cmp,r1,r2,nop} =>
           (fcmp(case cmp of 
                   I.FCMPd => 0w82
                 | I.FCMPEd => 0w86
                 | I.FCMPs => 0w81
                 | I.FCMPEs => 0w85,
                 r1,r2);
            delaySlot nop
           )
       | I.SAVE{r,i,d} => arith(0w60,r,i,d,false)
       | I.RESTORE{r,i,d} => arith(0w61,r,i,d,false)
       | I.RDY{d} => arith(0w40,0,I.REG 0,d,false)
       | I.WRY{r,i} => arith(0w48,r,i,0,false)
       | I.COPY _ => error "emitInstr:COPY"
       | I.FCOPY _ => error "emitInstr:FCOPY"
(*
       | I.ANNOTATION(i,_) => emitInstr(i,regmap)
*)
   end    

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:17:40  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:09:40  pscheng
# *** empty log message ***
#
 * Revision 1.5  1998/12/30 20:19:11  jhr
 *   Modifications to support direct generation of code into code objects.
 *
 * Revision 1.4  1998/10/06 14:06:25  george
 *  fixed up some machine description problems. [leunga]
 *
 * Revision 1.3  1998/09/30 19:38:10  dbm
 * fixing sharing/defspec conflict
 *
 * Revision 1.2  1998/08/12 13:36:25  leunga
 *
 *
 *   Fixed the 2.0 + 2.0 == nan bug by treating FCMP as instrs with delay slots
 *
 * Revision 1.1.1.1  1998/08/05 19:38:49  george
 *   Release 110.7.4
 *
 *)
