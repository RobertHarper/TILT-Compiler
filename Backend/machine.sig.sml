(*$import RTL *)
signature MACHINE =
  sig
    structure Rtl : RTL
    datatype register = R of int
                      | F of int
    type loclabel = Rtl.local_label
    datatype label    = I of loclabel  
                      | MLE of string                  (* a label emitted from direct assembly *)
                      | CE of string * register option (* a label emitted from C compiler *)
    datatype align    = LONG (* 4 bytes *)
                      | QUAD (* 8 bytes *)
                      | OCTA (* 16 bytes *)

   structure Labelmap : ORD_MAP where type Key.ord_key = loclabel
   structure Regmap   : ORD_MAP where type Key.ord_key = register
   structure Regset   : ORD_SET where type Key.ord_key = register

    type specific_instruction
    datatype stacklocation = CALLER_FRAME_ARG of int
                           | THIS_FRAME_ARG of int
                           | SPILLED_INT of int
                           | SPILLED_FP of int
                           | ACTUAL4 of int
                           | ACTUAL8 of int
                           | RETADD_POS
      
    val sloc2int  : stacklocation -> int (* return offset if stacklocation concrete *)
    val isInt     : register -> bool
    val isFloat   : register -> bool
    val isPhysical : register -> bool

    val freshCodeLabel : unit -> loclabel
    val freshDataLabel : unit -> loclabel
    val freshIreg  : unit -> register
    val freshFreg  : unit -> register

    val Rat     : register   (* Integer assembler temporary *)
    val Rat2    : register   (* Second temporary *)
    val Rsp     : register   (* Stack pointer *)
    val Rheap   : register   (* Heap pointer *)
    val Rhlimit : register   (* Heap limit pointer *)
    val Rexnarg : register   (* Exception argument *)
    val Rexnptr : register   (* Pointer to most-recently saved context *)
    val Rpv     : register option   
                  (* A register that holds the address that we are 
		   branching or jumping to.  Some calling conventions
		   like Alpha's, require it in which case we have SOME x.
		   Otherwise, for other conventions, like PPC, it is NONE *)
    val Fat     : register   (* Floating-point assembler temporary *)
    val Fat2    : register   (* Second floating-point temporary *)


    datatype call_type = DIRECT of label | INDIRECT of register
    datatype rtl_instruction =
      CALL of 
      {extern_call : bool,               (* is this a C call? *)
       func: call_type,                  (* label or temp containing addr. *)
       args : register list,             (* integer, floating temps *)
       results : register list,          (* integer, floating temps *)
       argregs : register list option,   (* actual registers *)
       resregs : register list option,   (*   "         "    *)
       destroys: register list option,   (*   "         "    *)
       tailcall : bool}
    | JMP of register * loclabel list
    | RETURN of {results: register list}        (* formals *)
    | SAVE_CS of loclabel
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
    | JSR     of bool * register * int * loclabel list (* link, dest, hint, labels *)
    | RET     of bool * int (* link, hint *)
    | GC_CALLSITE of loclabel
    | ILABEL  of loclabel
    | COMMENT of string
    | LADDR of register * label         (* dest, label *)


    datatype instruction = 
      BASE     of base_instruction
    | SPECIFIC of specific_instruction

    val msReg           : register -> string
    val msLabel         : label -> string
    val msLoclabel      : loclabel -> string
    val msData          : Rtl.data -> (int * string) list
    val msInstruction   : string -> instruction -> string
    val msStackLocation : stacklocation -> string

    val num_iregs : int
    val num_fregs : int
    val physical_regs : register list (* Physical register numbers *)
    val int_regs      : register list
    val fp_regs       : register list

   val eqRegs    : register -> register -> bool
   val eqLLabs   : loclabel -> loclabel -> bool
   val eqLabs    : label -> label -> bool

   (* these functions should be used with care since careless use
      of them will lead to incorrect register usage and calling conventions;
      they are provided for GC table generation and efficient
      naming of physical registers *)
   val ireg      : int -> register
   val freg      : int -> register
   val regNum    : register -> int
   val regLE     : register -> register -> bool

   (* map src registers using fs and destination using fd and return mapped instruction 
        ---   (i,fs,fd)  --- *)
   val translate_to_real_reg : 
       instruction * (register -> register)
                           * (register -> register) -> instruction
    
   val increase_stackptr : int -> instruction
   val decrease_stackptr : int -> instruction
   val std_return_code : register option -> instruction list (* register contains pv for gp reload *)
   val std_entry_code : unit -> instruction list

   val pop  : register * stacklocation -> instruction
   val push : register * stacklocation -> instruction

   val defUse : instruction -> register list * register list
   val cFlow : instruction -> (bool * loclabel list) option
   val extern_decl : string -> string

   (* Where a given pseudoregister is physically located *)
   datatype assign = IN_REG of register
                   | ON_STACK of stacklocation
                   | HINT of register
		   | UNKNOWN

   val assign2s  : assign -> string
   val eqAssigns : assign -> assign -> bool


   (* information on a procedure:
         - arg_ra_pos: the assigned positions for arguments and the
	               return register.
	 - res_ra_pos: the assigned positions for results.   I don't
	               understand what the second component is for.
	 - allocated: whether or not the procedure has been allocated
   *)

   datatype procsig = 
     PROCSIG of {arg_ra_pos : (assign list) option,
		 res_ra_pos : assign list option,
		 allocated  : bool,
		 regs_destroyed  : register list ref,
		 regs_modified  : register list ref,
		 blocklabels: loclabel list,
                 framesize  : int option,
		 ra_offset : int option,
		 callee_saved: register list,
		 args   : register list,  (* formals *)
		 res    : register list}  (* formals *)

   val special_iregs : register list 
   val special_fregs : register list 
   val general_iregs : register list
   val general_fregs : register list
   val special_regs  : register list (* special_iregs + special_fregs *)
   val general_regs  : register list (* physical_regs - special_regs  *)

  end
