functor SparcDelaySlotProps
   (structure I : SPARCINSTR
    structure P : INSN_PROPERTIES where I = I
    (* sharing/defn conflict:   sharing P.I = I*)
   ) : DELAY_SLOT_PROPERTIES =
struct
   structure I  = I
   structure SL = SortedList

   fun error msg = MLRiscErrorMsg.impossible("SparcDelaySlotProps."^msg)

   datatype delay_slot = D_NONE | D_ERROR | D_ALWAYS | D_TAKEN | D_FALLTHRU

   val delaySlotSize = 4

   fun delaySlot{instr, backward} =
     case instr of
       I.CALL{nop,...} => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.JMP{nop,...}  => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.JMPL{nop,...} => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.RET{nop,...}  => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | I.Bicc{b=I.BA,a,nop,...} => {n=false,nOn=D_NONE,nOff=D_ALWAYS,nop=nop}
     | I.Bicc{a,nop,...} => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.FBfcc{a,nop,...} => {n=a,nOn=D_TAKEN,nOff=D_ALWAYS,nop=nop}
     | I.FCMP{nop,...} => {n=false,nOn=D_ERROR,nOff=D_ALWAYS,nop=nop}
     | _ => {n=false,nOn=D_ERROR,nOff=D_NONE,nop=false}

   fun enableDelaySlot{instr, n, nop} =
       case (instr,n) of
         (I.CALL{defs,uses,label,...},false) => 
	    I.CALL{defs=defs,uses=uses,label=label,nop=nop}
       | (I.JMPL{r,i,d,defs,uses,...},false) => 
	    I.JMPL{r=r,i=i,d=d,defs=defs,uses=uses,nop=nop}
       | (I.JMP{r,i,labs,...},false) => 
	    I.JMP{r=r,i=i,labs=labs,nop=nop}
       | (I.RET{leaf,...},false) => I.RET{leaf=leaf,nop=nop}
       | (I.Bicc{b,a,label,...},_) => I.Bicc{b=b,a=n,nop=nop,label=label}
       | (I.FBfcc{b,a,label,...},_) => I.FBfcc{b=b,a=n,nop=nop,label=label}
       | (I.FCMP{cmp,r1,r2,...},false) => I.FCMP{cmp=cmp,r1=r1,r2=r2,nop=nop}
       | _ => error "enableDelaySlot"

    (* %y   = 64
     * %psr = 65
     * %fsr = 66
     *)

    val defUseI = P.defUse I.C.GP
    val defUseF = P.defUse I.C.FP
    fun conflict{regmap,src=i,dst=j} = 
        let fun defUseOther(I.Ticc _) = ([],[65])
              | defUseOther(I.ARITH{cc=true,...}) = ([65],[])
              | defUseOther(I.WRY _) = ([64],[])
              | defUseOther(I.RDY _) = ([],[64])
              | defUseOther(I.FCMP _) = ([66],[])
              | defUseOther(I.Bicc{b=I.BA,...}) = ([],[])
              | defUseOther(I.Bicc _) = ([],[65])
              | defUseOther(I.FBfcc _) = ([],[66])
              | defUseOther _ = ([],[])
            fun clash(defUse) =
                let val (di,ui) = defUse i
                    val (dj,uj) = defUse j
                in  case SL.intersect(di,uj) of
                       [] => (case SL.intersect(di,dj) of
                                [] => (case SL.intersect(ui,dj) of
                                         [] => false
                                       | _ => true)
                              | _ => true)
                    |  _ => true
                end
            fun defUseInt i = 
                let val (d,u) = defUseI i
                    val d     = SL.uniq(map regmap d)
                    val u     = SL.uniq(map regmap u)
                    (* no dependence on register 0! *) 
                    fun elim0(0::l) = l
                      | elim0 l     = l
                in  (elim0 d, elim0 u) end
            fun defUseReal i = 
                let val (d,u) = defUseF i
                    val d     = SL.uniq(map regmap d)
                    val u     = SL.uniq(map regmap u)
                in  (d,u) end
        in  clash(defUseInt) orelse 
            clash(defUseReal) orelse
            clash(defUseOther)
        end

    fun delaySlotCandidate{jmp,delaySlot=
                         (I.CALL _ | I.Bicc _ | I.FBfcc _ | I.Ticc _
                         | I.JMP _ | I.JMPL _ | I.RET _)} = false
      | delaySlotCandidate{jmp=I.FCMP _,delaySlot=I.FCMP _} = false
      | delaySlotCandidate _ = true

   fun setTarget(I.Bicc{b,a,nop,...},lab) = I.Bicc{b=b,a=a,nop=nop,label=lab}
     | setTarget(I.FBfcc{b,a,nop,...},lab) = I.FBfcc{b=b,a=a,nop=nop,label=lab}
     | setTarget _ = error "setTarget"

end
