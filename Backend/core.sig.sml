(*$import Prelude ORD_MAP ORD_SET Rtl TilWord32 *)

signature CORE =
  sig

    val branchingTraps : bool ref
	
    type label = Rtl.label
    type data = Rtl.data
	
    datatype register = R of int
                      | F of int
	
    datatype stacklocation = CALLER_FRAME_ARG4 of int  (* int indicates word count *)
	                   | CALLER_FRAME_ARG8 of int
                           | THIS_FRAME_ARG4 of int
                           | THIS_FRAME_ARG8 of int
                           | FRAME_TEMP                (* should use SPILL *)
                           | SPILLED_INT of int        
                           | SPILLED_FP of int
                           | RETADD_POS
                           | ACTUAL4 of int            (* int indicates actual offset - mult of 4 *)
                           | ACTUAL8 of int
      
    (* Where a given pseudoregister is physically located *)
    datatype assign = IN_REG of register
                    | ON_STACK of stacklocation
                    | HINT of register
                    | UNKNOWN_ASSIGN
									      
    structure Labelmap : ORD_MAP where type Key.ord_key = label
    structure Labelset : ORD_SET where type Key.ord_key = label
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
      MOVE    of register * register
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
		       (* regs_modified includes any register that is written to, whether or not it
			  is restored by the end of the instruction.  So regs_destroyed is a subset
			  of regs_modified.  This is only used by some of the exception handling
			  code, since exceptions can skip over the register-restoring epilogue of
			  functions.  *)
    | JSR     of bool * register * int * label list (* link, dest, hint, labels *)
    | RET     of bool * int (* link, hint *)
    | GC_CALLSITE of label
    | ILABEL  of label
    | ICOMMENT of string
    | LADDR of register * label         (* dest, label *)

    datatype linkage = 
	LINKAGE of {argCaller : assign list,    (* Where are the arguments from the caller's view? *)
		    resCaller : assign list,    (* Where are the results from the caller's view? *)
		    argCallee : assign list,    (* Where are the arguments from the callee's view? *)
		    resCallee : assign list     (* Where are the results from the callee's view? *)
		    }

   datatype procsig = 
       UNKNOWN_PROCSIG of 
           {linkage : linkage,
	    regs_destroyed  : Regset.set,     (* Physical registers modified by procedure *)
	    regs_modified  : Regset.set,      (* Physical registers modified (but restored) by procedure *)
	    callee_saved: Regset.set}         (* Physical registers saved by procedure *)
     | KNOWN_PROCSIG of 
           {linkage : linkage,                (* Physical locations of arguments and registers *)
	    argFormal : register list,        (* What are the formal (the pseudo-registers) for args and res *)
	    resFormal : register list,  
	    framesize : int,                  (* How big is the stack frame for the procedure? *)
	    ra_offset : int,                  (* Where in the stack is the return addr stored? *)
	    blocklabels: label list,          (* A list of basic block labels of the proc *)
	    regs_destroyed  : Regset.set,     (* Physical registers modified by procedure *)
	    regs_modified  : Regset.set,      (* Physical registers modified (but restored) by procedure *)
	    callee_saved: Regset.set}         (* Physical registers saved by procedure *)

   val msReg : register -> string
   val makeAsmLabel : string -> string
   val msStackLocation : stacklocation -> string
   val msAssign : assign -> string

  end


