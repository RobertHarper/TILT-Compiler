functor SparcAsmEmitter
   (structure Instr : SPARCINSTR
    structure PseudoOps : PSEUDO_OPS
    structure Shuffle : SPARCSHUFFLE where I = Instr
   ) : EMITTER_NEW =
struct
   structure I = Instr
   structure C = I.C
   structure P = PseudoOps
   structure R = I.Region
   structure Constant = I.Constant

   fun ms n = if n < 0 then ("-" ^ Int.toString (~n)) else Int.toString n

   fun error msg = MLRiscErrorMsg.impossible ("SparcAsmEmitter." ^ msg)

   fun emit s = TextIO.output(!AsmStream.asmOutStream,s)

   fun pseudoOp pOp = emit(P.toString pOp)

   fun defineLabel lab = emit(Label.nameOf lab ^ ":\n")

   fun comment msg = emit ("\t/* " ^ msg ^ " */")

   fun init size = (comment ("Code size = " ^ ms size))

   fun emitInstr(instr,regmap) = 
   let
       fun rmap r     = Intmap.map regmap r handle _ => r
       fun eReg i     = emit(I.C.cellToString(rmap i,I.C.GP))
       fun eReg' i    = emit(I.C.cellToString(rmap i,I.C.GP))
       fun fReg f     = emit(I.C.cellToString(rmap f,I.C.FP))
       fun fReg' f    = emit(I.C.cellToString(rmap f,I.C.FP))
       fun fReg2 f    = emit(I.C.cellToString(rmap f+1,I.C.FP))
       fun fReg2' f   = emit(I.C.cellToString(rmap f+1,I.C.FP))
       fun eLabel lab = emit (Label.nameOf lab)
       fun nl()       = emit "\n"
       fun tab()      = emit "\t"
       fun comma()    = emit ", "
       fun eInt i     = emit (ms i)
       fun emita a    = emit (if a then ",a\t" else "\t")

       fun emitLExp lexp = emit(LabelExp.toString lexp)
       fun eOp' f (I.REG r)   = f r
         | eOp' f (I.IMMED n) = eInt n
         | eOp' f (I.LO l)    = (emit "%lo("; emitLExp l; emit ")")
         | eOp' f (I.HI l)    = (emit "%hi("; emitLExp l; emit ")")
         | eOp' f (I.LAB l)   = emitLExp l
         | eOp' f (I.CONST c) = emit(Constant.toString c)

       val eOp  = eOp' eReg
       val eFop = eOp' fReg

       fun eAddr'(r,i) =
            (eReg r;
             case i of
               (I.REG 0 | I.IMMED 0) => ()
             | I.IMMED n => (if n > 0 then emit "+" else (); eInt n)
             | i => (emit "+"; eOp i)
            )

       fun eAddr(r,i) = (emit "["; eAddr'(r,i); emit "]")

       fun arith3(x,cc,r,i,d) = 
          (tab(); emit x; if cc then emit "cc" else ();
           tab(); eReg r; comma(); eOp i; comma(); eReg' d)
       fun eLoad(x,r,i,d) = 
          (tab(); emit x; tab(); eAddr(r,i); comma(); eReg' d)
       fun eStore(x,d,r,i) = 
          (tab(); emit x; tab(); eReg d; comma(); eAddr(r,i))
       fun eFload(x,r,i,d) = 
          (tab(); emit x; tab(); eAddr(r,i); comma(); fReg' d)
       fun eFstore(x,d,r,i) = 
          (tab(); emit x; tab(); fReg d; comma(); eAddr(r,i))
       fun eJump(r,i,0) =
           (emit "\tjmp\t"; eAddr'(r,i))
         | eJump(r,i,d) =
           (emit "\tjmpl\t"; eAddr'(r,i); comma(); eReg' d)
       fun eMem mem = comment(R.toString mem)
       fun eDelay false = ()
         | eDelay true  = emit "\n\tnop"

       fun arith I.ADD  = "add"
         | arith I.SUB  = "sub"
         | arith I.UMUL = "umul"
         | arith I.SMUL = "smul"
         | arith I.UDIV = "udiv"
         | arith I.SDIV = "sdiv"
         | arith I.AND  = "and"
         | arith I.ANDN = "andn"
         | arith I.OR   = "or"
         | arith I.ORN  = "orn"
         | arith I.XOR  = "xor"
         | arith I.XNOR = "xnor"
         | arith I.TADD = "tadd"
         | arith I.TADDTV = "taddtv"
       fun shift I.SLL  = "sll"
         | shift I.SRL  = "srl"
         | shift I.SRA  = "srl"
       fun load I.LD   = "ld"
         | load I.LDD  = "ldd"
         | load I.LDUB = "ldub"
         | load I.LDSB = "ldsb"
         | load I.LDSH = "ldsh"
         | load I.LDUH = "lduh"
       fun store I.ST  = "st"
         | store I.STB = "stb"
         | store I.STH = "sth"
         | store I.STD = "std"
       fun fload I.LDF = "ld" 
         | fload I.LDDF = "ldd" 
         | fload I.LDFSR = "ld" 
       fun fstore I.STF = "st"
         | fstore I.STDF = "std"
         | fstore I.STFSR = "st"
       fun cond I.BA   = "a"
         | cond I.BN   = "n"
         | cond I.BNE  = "ne"
         | cond I.BE   = "e"
         | cond I.BG   = "g"
         | cond I.BLE  = "le"
         | cond I.BGE  = "ge"
         | cond I.BL   = "l"
         | cond I.BGU  = "gu"
         | cond I.BLEU = "leu"
         | cond I.BCC  = "cc"
         | cond I.BCS  = "cs"
         | cond I.BPOS = "pos"
         | cond I.BNEG = "neg"
         | cond I.BVC  = "vc"
         | cond I.BVS  = "vs"
       fun branch cc = "b"^cond cc
       fun trap t = "t"^cond t
       fun fbranch I.FBA   = "fba"
         | fbranch I.FBN   = "fbn"
         | fbranch I.FBU   = "fbu"
         | fbranch I.FBG   = "fbg"
         | fbranch I.FBUG  = "fbug"
         | fbranch I.FBL   = "fbl"
         | fbranch I.FBUL  = "fbul"
         | fbranch I.FBLG  = "fblg"
         | fbranch I.FBNE  = "fbne"
         | fbranch I.FBE   = "fbe"
         | fbranch I.FBUE  = "fbue"
         | fbranch I.FBGE  = "fbge"
         | fbranch I.FBUGE = "fbuge"
         | fbranch I.FBLE  = "fble"
         | fbranch I.FBULE = "fbule"
         | fbranch I.FBO   = "fbo"
       fun farith1 I.FiTOd = "fitod"
         | farith1 I.FdTOi = "fdtoi"
         | farith1 I.FsTOi = "fstoi"
         | farith1 I.FiTOs = "fitos"
         | farith1 I.FdTOs = "fdtos"
         | farith1 I.FsTOd = "fstod"
         | farith1 I.FMOVs = "fmovs"
         | farith1 I.FABSs = "fabss"
         | farith1 I.FNEGs = "fnegs"
         | farith1 I.FSQRTs = "fsqrts"
         | farith1 I.FSQRTd = "fsqrtd"
         | farith1 _       = error "farith1"
       fun farith2 I.FADDd = "faddd"
         | farith2 I.FSUBd = "fsubd"
         | farith2 I.FMULd = "fmuld"
         | farith2 I.FDIVd = "fdivd"
         | farith2 I.FADDs = "fadds"
         | farith2 I.FSUBs = "fsubs"
         | farith2 I.FMULs = "fmuls"
         | farith2 I.FDIVs = "fdivs"
         | farith2 I.FsMULd = "fsmuld"
       fun fcmp I.FCMPd = "fcmpd"
         | fcmp I.FCMPEd = "fcmped"
         | fcmp I.FCMPs = "fcmps"
         | fcmp I.FCMPEs = "fcmpes"
   in 
       case instr of
         I.LOAD{l,r,i,d,mem} => (eLoad(load l,r,i,d); eMem mem)
       | I.STORE{s,d,r,i,mem} => (eStore(store s,d,r,i); eMem mem)
       | I.FLOAD{l,r,i,d,mem} => (eFload(fload l,r,i,d); eMem mem)
       | I.FSTORE{s,d,r,i,mem} => (eFstore(fstore s,d,r,i); eMem mem)
       | I.SETHI {d=0,i=0} => emit "\tnop"
       | I.SETHI {d,i} => (emit "\tsethi\t"; eInt i; comma(); eReg' d)
       | I.ARITH {a=I.OR,cc=false,r=0,i=I.REG r,d} => 
          (emit "\tmov\t"; eReg r; comma(); eReg' d)
       | I.ARITH {a=I.OR,cc=false,r=0,i,d} => 
          (emit "\tset\t"; eOp i; comma(); eReg' d)
       | I.ARITH {a=I.SUB,cc=true,r,i,d=0} => 
             (emit "\tcmp\t"; eReg r; comma(); eOp i)
       | I.ARITH {a=I.TADDTV,cc=true,r,i,d} => arith3("taddcctv",false,r,i,d)
       | I.ARITH {a,cc,r,i,d} => arith3(arith a,cc,r,i,d)
       | I.SHIFT {s,r,i,d} => arith3(shift s,false,r,i,d)
       | I.Bicc { b, a, label, nop} => (tab(); emit(branch b); 
                                    emita a; eLabel label; eDelay nop)
       | I.FBfcc { b, a, label, nop} => (tab(); emit(fbranch b); 
                                    emita a; eLabel label; eDelay nop)
       | I.JMP  {r,i,nop,...} => (eJump(r,i,0); eDelay nop)
       | I.JMPL {d,r,i,nop,...} => (eJump(r,i,d); eDelay nop)
       | I.CALL {label,nop,...} => (emit "\tcall\t"; eLabel label; eDelay nop)
       | I.RET{leaf=false,nop} => (emit "\tret"; eDelay nop) 
       | I.RET{leaf=true,nop} => (emit "\tretl"; eDelay nop)
       | I.Ticc {t,r,i} => (tab();emit(trap t);emit "\t"; eAddr'(r,i))
       | I.FPop1 {a=I.FMOVd,r,d} =>
           (emit "\tfmovs\t"; fReg r; comma(); fReg' d; nl();
            emit "\tfmovs\t"; fReg2 r; comma(); fReg2' d) 
       | I.FPop1 {a=I.FNEGd,r,d} =>
           (emit "\tfnegs\t"; fReg r; comma(); fReg' d; nl();
            emit "\tfmovs\t"; fReg2 r; comma(); fReg2' d) 
       | I.FPop1 {a=I.FABSd,r,d} =>
           (emit "\tfabds\t"; fReg r; comma(); fReg' d; nl();
            emit "\tfmovs\t"; fReg2 r; comma(); fReg2' d) 
       | I.FPop1 {a,r,d} => 
           (tab(); emit(farith1 a); tab(); fReg r; comma(); fReg' d)
       | I.FPop2 {a,r1,r2,d} => 
           (tab(); emit(farith2 a); tab();
            fReg r1; comma(); fReg r2; comma(); fReg' d)
       | I.FCMP {cmp,r1,r2,nop} =>
           (tab(); emit(fcmp cmp); tab();
            fReg r1; comma(); fReg r2; eDelay nop)
       | I.COPY{src,dst,tmp,...} => 
           (app (fn i => emitInstr(i,regmap))
                (Shuffle.shuffle{src=src,dst=dst,temp=tmp,regMap=rmap}))
       | I.FCOPY{src,dst,tmp,...} =>
           (app (fn i => emitInstr(i,regmap))
                (Shuffle.shufflefp{src=src,dst=dst,temp=tmp,regMap=rmap}))
       | I.SAVE{r,i,d} => arith3("save",false,r,i,d)
       | I.RESTORE{r,i,d} => arith3("restore",false,r,i,d)
       | I.RDY{d} => (emit "\trd\t%y, "; eReg' d)
       | I.WRY{r,i} => (emit "\twr\t"; eReg r; comma(); eOp i; emit ", %y")
       (*
       | I.ANNOTATION(i,a) => (emitInstr(i,regmap); comment(I.A.toString a))
        *)
       ; 
       nl()
   end
end  
