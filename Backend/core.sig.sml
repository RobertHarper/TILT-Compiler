(*$import Rtl TilWord32 *)

signature CORE =
  sig

    type align = Rtl.align
    type label = Rtl.label
    type data = Rtl.data
	
    datatype register = R of int
                      | F of int
	
    datatype stacklocation = CALLER_FRAME_ARG of int
                           | THIS_FRAME_ARG of int
                           | SPILLED_INT of int
                           | SPILLED_FP of int
                           | ACTUAL4 of int
                           | ACTUAL8 of int
                           | RETADD_POS
      
    (* Where a given pseudoregister is physically located *)
    datatype assign = IN_REG of register
                    | ON_STACK of stacklocation
                    | HINT of register
                    | UNKNOWN
									      
    structure Labelmap : ORD_MAP where type Key.ord_key = label
    structure Regmap   : ORD_MAP where type Key.ord_key = register
    structure Regset   : ORD_SET where type Key.ord_key = register

    val eqRegs    : register -> register -> bool
    val eqRegs'   : register * register -> bool
    val eqLabs    : label -> label -> bool
    val eqAssigns : assign -> assign -> bool

    val sloc2int  : stacklocation -> int (* return offset if stacklocation concrete *)
    val isInt     : register -> bool
    val isFloat   : register -> bool
    val regNum    : register -> int


    datatype call_type = DIRECT of label * register option
                       | INDIRECT of register
    datatype rtl_instruction =
      CALL of 
      {calltype : Rtl.calltype,          (* is this a C call? *)
       func: call_type,                  (* label or temp containing addr. *)
       args : register list,             (* integer, floating temps *)
       results : register list,          (* integer, floating temps *)
       argregs : register list option,   (* actual registers *)
       resregs : register list option,   (*   "         "    *)
       destroys: register list option}   (*   "         "    *)
    | JMP of register * label list
    | RETURN of {results: register list}        (* formals *)
    | SAVE_CS of label
    | HANDLER_ENTRY


    datatype base_instruction = 
      MOVI    of register * register
    | MOVF    of register * register
    | PUSH    of register * stacklocation
    | POP     of register * stacklocation
    | PUSH_RET of stacklocation option  (* may not be fully realized during register allocation *)
    | POP_RET  of stacklocation option
    | RTL     of rtl_instruction
    | TAILCALL of label
    | BR      of label
    | BSR     of label * register option * {regs_modified : register list, regs_destroyed : register list,
					    args : register list}
                       (* if register is SOME ?, then ? is the reg where we want to put retadd *)
    | JSR     of bool * register * int * label list (* link, dest, hint, labels *)
    | RET     of bool * int (* link, hint *)
    | GC_CALLSITE of label
    | ILABEL  of label
    | ICOMMENT of string
    | LADDR of register * label         (* dest, label *)

   datatype procsig = 
     PROCSIG of {arg_ra_pos : (assign list) option,
		 res_ra_pos : assign list option,
		 allocated  : bool,
		 regs_destroyed  : register list ref,
		 regs_modified  : register list ref,
		 blocklabels: label list,
                 framesize  : int option,
		 ra_offset : int option,
		 callee_saved: register list,
		 args   : register list,  (* formals *)
		 res    : register list}  (* formals *)


   val msReg : register -> string
   val makeAsmLabel : string -> string
   val msStackLocation : stacklocation -> string
   val msAssign : assign -> string

  end


