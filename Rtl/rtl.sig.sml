(*$import Name TilWord32 *)
signature RTL =
sig

  type var = Name.var
  datatype label = ML_EXTERN_LABEL of string  
                 | C_EXTERN_LABEL of string
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
  and regf = REGF of var * rep

  and rep_path = Projvar_p of (regi * int list)     (* if the list is empty, then it is not a projection *)
               | Projlabel_p of (label * int list)  (* if the list is empty, then it is just a label *)
               | Notneeded_p

  and rep = TRACE
          | UNSET         (* a locative address that is not yet set; needs to be set once *)
          | NOTRACE_INT
          | NOTRACE_CODE
          | NOTRACE_REAL
            (* global label --- outside the heap, so it shouldn't
	       be traced *)
         | LABEL 
	    (* LOCATIVE: pointer into middle of array/record. 
	       This must NEVER be live across a GC point *)
         | LOCATIVE
         | COMPUTE of rep_path

  val eqsregi : sregi * sregi -> bool
  val eqregi  : regi * regi -> bool
  val eqregf  : regf * regf -> bool

  val regi2int : regi -> int
  val sregi2int : sregi -> int


  (* save: set of registers to save before a procedure call or at the
     start of executing a procedure body.  These registers are restored
     after the procedure call or executing the procedure body.*)

  datatype save = SAVE of regi list * regf list

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

  datatype cmp = EQ | LE | LT | GE | GT | NE | LBC | LBS


  datatype align = LONG    (* 4 bytes *)
                 | QUAD    (* 8 bytes *)
                 | ODDLONG (* align at 8 byte boundary +4 *)
                 | OCTA    (* 16 bytes *)
                 | ODDOCTA (* 16 bytes bound + 12 *)

  datatype traptype = INT_TT | REAL_TT | BOTH_TT
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

    | NOTB of regi * regi
    | ANDB   of regi * sv * regi
    | ORB    of regi * sv * regi
    | XORB   of regi * sv * regi 
    | SRA   of regi * sv * regi (* shift right arithmetic:(value, shift)*)
    | SRL   of regi * sv * regi (* shift right logical *)
    | SLL   of regi * sv * regi (* shift left logical *)

    | CVT_REAL2INT  of regf * regi  (* does a floor with rounding towards neg inf *)
    | CVT_INT2REAL  of regi * regf  (* converts int to real format *)

    | FADDD  of regf * regf * regf        (* c <- a+b *)
    | FSUBD  of regf * regf * regf        (* c <- a-b *)
    | FMULD  of regf * regf * regf        (* c <- a*b *)
    | FDIVD  of regf * regf * regf        (* c <- a/b *)
    | FABSD  of regf * regf 
    | FNEGD  of regf * regf

    | SQRT   of regf * regf
    | SIN    of regf * regf
    | COS    of regf * regf
    | ARCTAN of regf * regf
    | EXP    of regf * regf
    | LN     of regf * regf
    | CMPF   of cmp * regf * regf * regi

    (* flow of control instructions *)

    (* jumps and branches *)

    | BR     of label

    (* BCNDI, BCNDF: compare against 0 and branch.  bool = whether
       predicted taken or not *)

    | BCNDI2  of cmp * regi * sv * label * bool  
    | BCNDF2  of cmp * regf * regf * label * bool
    | BCNDI   of cmp * regi * label * bool  
    | BCNDF   of cmp * regf * label * bool
    | JMP    of regi * label list 
                         (* label list includes set of possible destinations.
			    These destinations must all be within the same
			    procedure as this jump.*)

    (* procedure call and return: these are "heavyweight" operations in
       this machine.   This avoids over-constraining register allocation.*)


    (* CALL function will invoke the function identified in _func
       at the point of invocation, the arguments lie in _arg
       upon a return, the code expects its results to be in _results
       the return should be made to the value in _return
		if _return is NONE, then return to the instr after CALL
       if _tailcall is true, then it happens to be that the
		return values from this call can be directly
		returned to the caller of THIS function
       _save contains registers the caller would like saved
		and restored
    *)

    | CALL of {extern_call : bool,
	       func: reg_or_label,
	       return : regi option,
	       args : regi list * regf list, 
	       results : regi list * regf list,
	       tailcall : bool,
	       save : save}
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
    | LOADQF   of ea * regf
    | STOREQF  of ea * regf

    | MUTATE of ea * regi * regi option   (* if option is present, a nonzero value indicates pointer *)
    | INIT of ea * regi * regi option     (* if option is present, a nonzero value indicates pointer *)

    | NEEDGC of sv          (* needgc(sv) calls garbage collector if that
			       many words are not allocatable *)
    | FLOAT_ALLOC of regi * regf * regi * TilWord32.word  (* number of floats *)
    | INT_ALLOC   of regi * regi * regi * TilWord32.word  (* number of bytes *)
    | PTR_ALLOC   of regi * regi * regi * TilWord32.word  (* number of words *)
                            (* len, value, dest:
			     allocate a f/i/p array of logical length len
			     filled with the f/i/p value v and put the
			     result in dest *)
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
      COMMENT of string
    | STRING of (string)
    | INT32 of  (TilWord32.word)
    | INT_FLOATSIZE of (TilWord32.word)
    | FLOAT of  (string)
    | DATA of   (label)
(* array of i words inited to word32 *)
    | ARRAYI of (int * TilWord32.word)
(* array of i words initialized to fp value in string *)
    | ARRAYF of (int * string)
(* array of i words initialized to label or small int *)
    | ARRAYP of (int * labelortag)
    | ALIGN of  (align)
    | DLABEL of (label)

  (* _return is where the return address is expected to be passed in
     _args is where args should be passed in
     _results is where this procedure will put the results 
     _save is what registers this procedure will use
	so the first action upon entry is to save these regs
  *)
  datatype proc = PROC of {name : label,
			   return : regi,
			   args : regi list * regf list ,
			   results : regi list * regf list,
			   code : instr array,
			   known : bool,
			   save : save,
                           vars : (int * int) option}


  (* an RTL module

     procs is a list a procedures.

     data is a list of static data for the module.

     main is the entry point for the module.

     mutable_variables and mutable_objects are used to track
     pointers from static data to the heap for the garbage
     collector.   When the garbage collector moves objects in
     the heap, it needs to update the pointers to these
     objects in static data.

     mutable is a list of addresses whose contents may be initialized 
        once during program execution by a pointer into the heap.
        This list is used to represent globals and cells of statically
	allocated byt uninitialized objects.  The rep for a mutable
        is either TRACE or COMPUTE (COMPUTEs may arise from imports)
  *)
 
  datatype module = MODULE of
                          {procs : proc list,
			   data : data list,
			   main : label,
			   mutable : (label * rep) list}

end (* RTL *)



