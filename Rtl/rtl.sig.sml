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

  datatype sregi = HEAPPTR | HEAPLIMIT | EXNPTR | EXNARG | STACKPTR | THREADPTR

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
  datatype ea = EA of regi * int  

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
    | LADDR  of label * int * regi
    | LEA    of ea * regi               
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

    (* exceptions --- we implement exceptions by saving the
       control state of the machine and the point to jump to
       if an exception occurs.    The exnptr points to a
       record containing this information.

       The record has the following format:

	   . the pc to jump to if an exception occurs.
	   . the stack pointer to restore.
           . the previous value of the exnptr.
           . a subset of the integer registers.
           . either the tag 0 or an array of the subset of
             the fp registers.

       SAVE_CS, END_SAVE, RESTORE are used by the Rtl
       interpreter and in the translation to Alpha machine code.
       In the Rtl interpreter, they manipulate the implicit
       call stack.  Note that the stack pointer register, although
       bogus at this stage, is explicitly changed by the RTL code.

       - SAVE_CS saves the call stack on an exception stack
       - END_SAVE pops the top entry of the exception stack and
         throws it away.
       - RESTORE pops the top entry of the exception stack and 
         installs it as the control stack.
         
       A handler is installed by doing by setting the exnptr
       and doing a SAVE_CS.  It is uninstalled by resetting
       the exnptr and doing an END_SAVE.

       A raise is compiled as 
         (1) extract the new pc value from the exnrecord
         (1) move the value associated with the
	     exception to the exnarg.
	 (2) a RESTORE
	 (3) jump to the new pc value.

       The code for a handler restores the integer registers
       and fp registers, including the exnptr and stackptr.*)

    | SAVE_CS of label
    | END_SAVE
    | RESTORE_CS 

    | LOAD32I  of ea * regi           (* displacements not scaled *)
    | STORE32I of ea * regi           (* unchecked stores *)
    | LOAD8I  of ea * regi            (* displacements not scaled *)
    | STORE8I of ea * regi            (* unchecked stores *)
    | LOADQF   of ea * regf
    | STOREQF  of ea * regf

    | MUTATE of regi * sv * regi * regi option   (* if option is present, a nonzero value indicates pointer;
						   *(regi1 + sv) = regi2 *)
    | INIT of ea * regi * regi option     (* if option is present, a nonzero value indicates pointer *)

    | NEEDGC of sv          (* needgc(sv) calls garbage collector if that
			       many words are not allocatable *)

    (* for signalling hardware(Alpha): SOFT -> BARRIER; HARD -> nop
      for non-signalling hardware(PPC): SOFT -> NOP; HARD -> test-and-branch *)
    | SOFT_VBARRIER of traptype
    | SOFT_ZBARRIER of traptype
    | HARD_VBARRIER of traptype
    | HARD_ZBARRIER of traptype
    | HANDLER_ENTRY          (* mark beginning of handler, which is target
			        of an interprocedural jump *)
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
			   known : bool,
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



