signature ALPHA32INSTR = sig
  structure Constant : CONSTANT
  structure Region : REGION

  structure C : ALPHA32CELLS where type cellset = int list * int list

  datatype ea = 
      Direct of int 
    | FDirect of int 
    | Displace of {base:int, disp:int}

  datatype operand =      (* Instruction formats *)
      REGop of int
    | IMMop of int
    | HILABop of LabelExp.labexp
    | LOLABop of LabelExp.labexp
    | LABop of LabelExp.labexp
    | CONSTop of Constant.const

  datatype cond_code =
      CC_EQ  | CC_NEQ
    | CC_LT  | CC_LE  | CC_GT  | CC_GE
    | CC_LTU | CC_LEU | CC_GTU | CC_GEU

  datatype branch = BR | BEQ | BGE | BGT | BLE | BLT | BNE | BLBC | BLBS

  datatype load = LDL | LDQ | LDQ_U
  datatype store = STL | STQ | STQ_U
  datatype fload = LDT | LDS
  datatype fstore = STT

  datatype operate =
      ZAP | ADDL | ADDQ | SUBL | SUBQ | MULL
    | S4ADDL | S8ADDL
    | CMPULE | CMPULT | CMPEQ | CMPLE | CMPLT | SGNXL 
    | AND | BIS | XOR | SRA | SRL | SLL
    | INSBL | EXTBL | EXTQH | MSKBL | MSKLH

  datatype pseudo_op = DIVL | DIVLU

  datatype operateV = ADDLV | SUBLV | MULLV

  datatype foperate =  
      CPYS | CPYSN 
    | CVTQT | CVTLQ
    | CMPTEQ | CMPTLT | CMPTLE | CMPTUN
  datatype foperateV = CVTTQ | ADDT | SUBT | MULT | DIVT

  datatype osf_user_palcode = 
    BPT | BUGCHK | CALLSYS | GENTRAP | IMB | RDUNIQUE | WRUNIQUE


  datatype instruction =
    DEFFREG of int			(* define a floating point register *)

  (* Load/Store *)
  | LDA of {r:int, b:int, d:operand}	(* use of REGop is illegal *)
  | LDAH of {r:int, b:int, d:operand} (* use of REGop is illegal *)

  | LOAD of {ldOp:load, r:int, b:int, d:operand, mem:Region.region}
  | STORE of {stOp:store, r:int, b:int, d:operand, mem:Region.region}
  | FLOAD of {ldOp:fload, r:int, b:int, d:operand, mem:Region.region}
  | FSTORE of {stOp:fstore, r:int, b:int, d:operand, mem:Region.region}

  (* Control Instructions *)
  | JMPL of {r:int, b:int, d:int} * Label.label list
  | JSR of {r:int, b:int, d:int} * C.cellset * C.cellset
  | BRANCH of branch * int * Label.label   
  | FBRANCH of branch * int * Label.label  

  (* Integer Operate *)
  | OPERATE of {oper:operate, ra:int, rb:operand, rc:int}
  | OPERATEV of {oper:operateV, ra:int, rb:operand, rc:int}
  | PSEUDOARITH of {oper: pseudo_op, ra:int, rb:operand, rc:int, tmps: C.cellset}

  (* Copy instructions *)
  | COPY of {dst: int list, src:int list, impl: instruction list option ref,
	     tmp: ea option}
  | FCOPY of {dst: int list, src:int list, impl: instruction list option ref,
	     tmp: ea option}

  (* Floating Point Operate *)
  | FOPERATE of {oper:foperate, fa:int, fb:int, fc:int}
  | FOPERATEV of {oper:foperateV, fa:int, fb:int, fc:int}

  (* Misc *)
  | TRAPB				(* Trap barrier *)

  | CALL_PAL of {code:osf_user_palcode, def:int list, use:int list}
end
