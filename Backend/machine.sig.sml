(*$import Prelude Core Rtl Name TilWord32 *)

signature MACHINE =
  sig

    type register = Core.register
    type stacklocation = Core.stacklocation
    type label = Core.label
    type data = Core.data
    type assign = Core.assign
    type linkage = Core.linkage

    type specific_instruction
    datatype instruction = 
      BASE     of Core.base_instruction
    | SPECIFIC of specific_instruction


    val sloc2int  : stacklocation -> int (* return offset if stacklocation concrete *)
    val isPhysical : register -> bool

    val freshCodeLabel : unit -> label
    val freshDataLabel : unit -> label
    val freshIreg  : unit -> register
    val freshFreg  : unit -> register

    val Rat     : register   (* Integer assembler temporary *)
    val Rat2    : register   (* Second temporary *)
    val Rsp     : register   (* Stack pointer *)
    val Rth     : register   (* Thread pointer *)
    val Rheap   : register   (* Heap pointer *)
    val Rhlimit : register   (* Heap limit pointer *)
    val Rexnarg : register   (* Exception argument *)
    val Rexnptr : register   (* Pointer to most-recently saved context *)
    val Rhandler : register   (* Contains the handler address as we jump to it *)
    val Rpv     : register option   
                  (* A register that holds the address that we are 
		   branching or jumping to.  Some calling conventions
		   like Alpha's, require it in which case we have SOME x.
		   Otherwise, for other conventions, like PPC, it is NONE *)
    val Fat     : register   (* Floating-point assembler temporary *)
    val Fat2    : register   (* Second floating-point temporary *)




    val msReg           : register -> string
    val msLabel         : label -> string
    val msData          : Rtl.data -> (int * string) list
    val msInstruction   : string * instruction -> string
    val msInstructions   : (string * instruction) list -> string  list (* May perform peephole optimization *)
    val msStackLocation : stacklocation -> string
    val assign2s        : assign -> string

    val num_iregs : int
    val num_fregs : int
    val physical_regs : register list (* Physical register numbers *)
    val int_regs      : register list
    val fp_regs       : register list

   val eqRegs    : register -> register -> bool
   val eqRegs'   : register * register -> bool
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
    
   val allocate_stack_frame : int * int -> instruction list  (* Frame size in bytes, Max offset of previous frame used *)
   val deallocate_stack_frame : int -> instruction list  (* Frame size in bytes *)
   val std_return_code : register option -> instruction list (* register contains pv for gp reload *)
   val std_entry_code : unit -> instruction list

   val pop  : register * stacklocation -> instruction list
   val push : register * stacklocation -> instruction list

   val defUse : instruction -> register list * register list

   (* bool indicates possible fallthrough *)
   datatype instr_flow = NOBRANCH | BRANCH of bool * label list | DELAY_BRANCH of bool * label list
   val cFlow : instruction -> instr_flow

   val extern_decl : string -> string



   (* information on a procedure:
         - arg_ra_pos: the assigned positions for arguments and the
	               return register.
	 - res_ra_pos: the assigned positions for results.   I don't
	               understand what the second component is for.
	 - allocated: whether or not the procedure has been allocated
   *)

   val special_iregs : register list 
   val special_fregs : register list 
   val general_iregs : register list
   val general_fregs : register list
   val special_regs  : register list (* special_iregs + special_fregs *)
   val general_regs  : register list (* physical_regs - special_regs  *)

  end
