(*$import Name TilWord32 *)
signature RTL =
sig

  type var = Name.var
  datatype label = ML_EXTERN_LABEL of string  
                 | LOCAL_DATA of string
                 | LOCAL_CODE of string

  val eq_label : label * label -> bool
  val named_data_label : string -> label
  val named_code_label : string -> label
  val fresh_data_label : string -> label
  val fresh_code_label : string -> label

  (* ML externs are addresses of values
     C externs are addresses which _are_ values;

     In C parlance, C externs are r-values, while ML externs
     are l-values.*)

  (* Special registers at the RTL level translate to actual registers at the lower Alpha/Sparc level.
     The HANDLER, EXNSTACK, EXNARG, and STACK are exposed at this level to allow exceptions to be compiled.
     It would be nice if these last 4 registers are unnecssary.  However, the implementation of
     an exception stack requires allocation and the lower levels cannot correctly insert
     allocation code given that the RTL code has performed GC coaslescing.
   *)
  datatype sregi = THREADPTR | HEAPALLOC | HEAPLIMIT | 
                   HANDLER | EXNSTACK | EXNARG | STACK

  datatype regi = REGI of var * rep  (* int in var is register # *)
                | SREGI of sregi

  and rep_path = Projvar_p of (regi * int list)     (* if list is empty, then it is not a projection *)
               | Projlabel_p of (label * int list)  (* if list is empty, then it is just a label *)
               | Notneeded_p

  and rep = TRACE
          | NOTRACE_INT
          | NOTRACE_CODE
          | NOTRACE_REAL
          | NOTRACE_LABEL (* global labels outside the heap -- should not be traced *)
          | LOCATIVE      (* pointer into middle of array/record: must not be live across GC *)
          | UNSET         (* an uninitialized locative address: will be set once, may persist across GC *)
          | COMPUTE of rep_path (* trace is not statically known *)

  datatype regf = REGF of var * rep
  datatype reg = I of regi | F of regf

  val eqsregi : sregi * sregi -> bool
  val eqregi  : regi * regi -> bool
  val eqregf  : regf * regf -> bool
  val eqreg : reg * reg -> bool

  val regi2int : regi -> int
  val sregi2int : sregi -> int


 (* effective address: register + sign-extended displacement *) 
  datatype ea = REA of regi * int  
              | LEA of label * int
              | RREA of regi * regi

  (* in_ea_disp_range: is an effective address in range for the displacement value ? *)

   val in_ea_disp_range : int -> bool

  datatype reg_or_label = REG' of regi
                        | LABEL' of label

  (* small value: small integer value or register *)

  datatype sv = REG of regi
	      | IMM of int

  (* in_imm_range: is an integer in a range of immediate values ? *) 

  val in_imm_range : TilWord32.word -> bool

  datatype cmp = EQ | LE | LT | GE | GT | NE 


  datatype align = LONG    (* 4 bytes *)
                 | QUAD    (* 8 bytes *)
                 | ODDLONG (* align at 8 byte boundary +4 *)
                 | OCTA    (* 16 bytes *)
                 | ODDOCTA (* 16 bytes bound + 12 *)

  datatype traptype = INT_TT | REAL_TT | BOTH_TT
  datatype calltype = ML_NORMAL | ML_TAIL of regi | C_NORMAL

  datatype instr = 
      LI     of TilWord32.word * regi
    | LADDR  of ea * regi               
    | MV     of regi * regi               (* src,dest *)
    | CMV    of cmp * regi * sv * regi    (* if cmp ra then c <- b *)
    | FMV    of regf * regf 


    | ADD    of regi * sv * regi        (* add(a,b,c): c <- a+b *)
    | SUB    of regi * sv * regi        (* c <- a-b *)
    | MUL   of regi * sv * regi        (* c <- a*b *)
    | DIV   of regi * sv * regi        (* c <- a/b *)
    | MOD   of regi * sv * regi        (* c <- a mod b *)
    | S4ADD of regi * sv * regi        (* scaled add by 4: c <- 4*a+*b *)
    | S8ADD of regi * sv * regi        (* scaled add by 8: c <- 8*a+*b *)
    | S4SUB of regi * sv * regi        (* scaled sub by 4: c <- 4*a-b *)
    | S8SUB of regi * sv * regi        (* scaled sub by 8: c <- 8*a-b *)
    | ADDT   of regi * sv * regi        (* c <- a+b, trap on overflow *)
    | SUBT   of regi * sv * regi        (* c <- a-b, trap on overflow *)
    | MULT   of regi * sv * regi        (* c <- a*b, trap on overflow *)
    | DIVT   of regi * sv * regi        (* c <- a/b, trap on zero *)
    | MODT   of regi * sv * regi        (* c <- a mod b, trap on zero *)
    | CMPSI  of cmp * regi * sv * regi  (* c <- a op b *)
    | CMPUI  of cmp * regi * sv * regi  (* c <- a op b *)

    | NOTB    of regi * regi
    | ANDB    of regi * sv * regi
    | ORB     of regi * sv * regi
    | ANDNOTB of regi * sv * regi
    | ORNOTB  of regi * sv * regi
    | XORB    of regi * sv * regi 
    | SRA     of regi * sv * regi (* shift right arithmetic:(value, shift)*)
    | SRL     of regi * sv * regi (* shift right logical *)
    | SLL     of regi * sv * regi (* shift left logical *)

    | CVT_REAL2INT  of regf * regi  (* does a floor with rounding towards neg inf *)
    | CVT_INT2REAL  of regi * regf  (* converts int to real format *)

    | FADDD  of regf * regf * regf        (* c <- a+b *)
    | FSUBD  of regf * regf * regf        (* c <- a-b *)
    | FMULD  of regf * regf * regf        (* c <- a*b *)
    | FDIVD  of regf * regf * regf        (* c <- a/b *)
    | FABSD  of regf * regf 
    | FNEGD  of regf * regf

    | CMPF   of cmp * regf * regf * regi

    (* flow of control instructions *)

    (* jumps and branches *)

    | BR     of label

    (* BCND(I/F): compare operands with cmp and branch is cmp succeeds 
                  bool predicts whether branch taken or not *)
    | BCNDI  of cmp * regi * sv * label * bool  
    | BCNDF  of cmp * regf * regf * label * bool
    | JMP    of regi * label list 
                         (* label list includes set of possible destinations.
			    These destinations must all be within the same
			    procedure as this jump.*)

    (* procedure call and return: these are "heavyweight" operations in
       this machine.   This avoids over-constraining register allocation.*)


    (* CALL function will invoke the function identified in _func
       at the point of invocation, the arguments lie in _arg
       Upon a return, the code expects its results to be in _results
       The call may be a normal ML call or a normal C call.  If the call
          is an ML tailcall, then there are no results and the extra
	  register contains the return address to return to.
       The save field contains registers the caller would like save/restore
          around the call.
    *)

    | CALL of {call_type : calltype,
	       func: reg_or_label,
	       args : reg list,
	       results : reg list,
	       save : reg list}

    | RETURN of regi                 (* address to return to *)

    (* Exceptions are implemented by maintaining a stack
       of exception record.  Each exception record contains the
       handler, its free variables, and the stack pointer where
       the handler was installed.  The stack itself is implemented
       by chaining together the exception records.  Thus,
       the exception pointer is always a record with
       the following format:

	   . the pc to jump to if an exception occurs
	   . the stack pointer to restore
           . the handler's free variables
           . the previous value of the exnptr

       Four RTL instructions (PUSH_EXN, POP_EXN, THROW_EXN, and CATCH_EXN)
       are used in the translation of the Nil level Handle_e and Raise_e.
       Though the creation, installation, de-installation, and use of the
       exeption record is made explicit in the translation to RTL,
       we retain these abstract instructions since some of them, 
       notably CATCH_EXN, still translate into platform-dependent
       code.  Currently, PUSH_EXN, POP_EXN, THROW_EXN are no-ops
       while CATCH_EXN translate into code that records the fact that
       a handler has entered and some special calling convention code
       on the Alpha.

       - PUSH_EXN adds a new entry to the exception stack
       - POP_EXN  discards the top entry of the exception stack
       - THROW_EXN jumps to the handler on the exception stack
       - CATCH_EXN __assumes__ the stack pointer has been restored 
                   by THROW_EXN or the code itself.  It also
		   pops the control stack.
         
       Nil.Handle_e(e,h) becomes

       The expression e with the handler is translated as
         (1) Allocating a record free containing all variables that need to be saved.
	     Include the handler, the stack pointer, and the previous exnptr as the first 3 fields.
	 (2) PUSH_EXN - nop
         (3) Evaluates the expression e into register d (an exception may be raised here)
	 (4) pop the exn record
         (5) POP_EXN - nop
	 (6) branch to after the handler

       The handler h itself follows next as
         (1) CATCH_EXN which performs action that occurs after stack pointer restored
	 (2) restore free variable record and pops the exn stack
	 (3) restore free variables from free variable record
	 (4) move EXNARG into exception variable register
	 (5) the handler code h evaluating to destination register d
	 (6) fall through to the code after the handler

       A Nil.Raise_e is compiled as 
         (1) THROW_EXN - nop
	 (2) move exception into EXNARG
	 (3) restore the stack pointer of top record of the exception stack
         (4) branch to the handler of the top record of the exception stack

  *)

    | PUSH_EXN
    | POP_EXN
    | THROW_EXN
    | CATCH_EXN

    | LOAD32I  of ea * regi           (* displacements not scaled *)
    | STORE32I of ea * regi           (* unchecked stores *)
    | LOAD8I  of ea * regi            (* displacements not scaled *)
    | STORE8I of ea * regi            (* unchecked stores *)
    | LOADQF   of ea * regf
    | STOREQF  of ea * regf

    | MUTATE of ea * regi * regi option   (* if option is present, a nonzero value indicates pointer;
						   *ea = regi *)
    | INIT of ea * regi * regi option     (* if option is present, a nonzero value indicates pointer *)

    | NEEDGC of sv          (* needgc(sv) calls garbage collector if that
			       many words are not allocatable *)

    (* for signalling hardware(Alpha): SOFT -> BARRIER; HARD -> nop
      for non-signalling hardware(PPC): SOFT -> NOP; HARD -> test-and-branch *)
    | SOFT_VBARRIER of traptype
    | SOFT_ZBARRIER of traptype
    | HARD_VBARRIER of traptype
    | HARD_ZBARRIER of traptype

    | ILABEL of label
    | IALIGN of align       (* alignment of blocks *)
    | HALT                  (* needed for termination of main *)    
    | ICOMMENT of string

  datatype labelortag = PTR of label | TAG of TilWord32.word

  datatype data = 
      COMMENT  of string
    | DLABEL   of label                   (* data segment label *)
    | STRING   of string                  (* separate string case to avoid endian-ness issue *)
    | INT32    of TilWord32.word      
    | FLOAT    of string                  (* double-precision float point literal *)
    | DATA     of label                   (* address value - label as value *)
    | ALIGN    of align

  (* _return is where the return address is expected to be passed in
     _args is where args should be passed in
     _results is where this procedure will put the results 
     _save is what registers this procedure will use
	so the first action upon entry is to save these regs
  *)
  datatype proc = PROC of {name : label,
			   return : regi,
			   args : reg list,
			   results : reg list,
			   code : instr array,
			   save : reg list,
                           vars : (int * int) option}


  (* an RTL module

     procs is a list a procedures.

     data is a list of static data for the module.

     main is the entry point for the module.

     global is a list of addresses of objects whose fields may point into the heap
       (1) The list contains addresses of statically allocated objects which have
             pointers into the heap.
       (2) The list also contains the addresses of globals by treating them as
             records of one field.  Globals pointing to statically allocated
	     objects are not recorded only globals that may contain heap pointers.

     Objects in the global list must contain a header word.

  *)
 
  datatype module = MODULE of
                          {procs : proc list,
			   data : data list,
			   main : label,
			   global : label list}

end (* RTL *)



