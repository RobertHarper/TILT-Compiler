(*$import RTL TilWord32 Name Util String HashString *)

structure Rtl :> RTL = 
struct

  fun error str = Util.error "rtl.sml" str
    
  type var = Name.var
  val eq_var = Name.eq_var
  val fresh_var = Name.fresh_var
  val fresh_named_var = Name.fresh_named_var
  datatype label = ML_EXTERN_LABEL of string
	         | C_EXTERN_LABEL of string
		 | LINK_EXTERN_LABEL of string
                 | LOCAL_DATA of string
                 | LOCAL_CODE of string

  datatype sregi = THREADPTR | HEAPALLOC | HEAPLIMIT | 
                   HANDLER | EXNSTACK | EXNARG | STACK
  datatype regi = REGI of var * rep  (* int in var is register # *)
                | SREGI of sregi

  and rep_path = Projvar_p of (regi * int list)     (* if list is empty, then it is not a projection *)
               | Projlabel_p of (label * int list)  (* if list is empty, then it is just a label *)
               | Projglobal_p of (label * int list)  (* if list is empty, then it is just the global *)
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

  (* LOCAL_CODE < LOCAL_DATA < LINK_EXTERN_LABEL < ML_EXTERN_LABEL < C_EXTERN_LABEL *)
  fun compare_label (LOCAL_CODE s1, LOCAL_CODE s2) = String.compare(s1,s2)
    | compare_label (LOCAL_DATA s1, LOCAL_DATA s2) = String.compare(s1,s2)
    | compare_label (LINK_EXTERN_LABEL s1, LINK_EXTERN_LABEL s2) = String.compare(s1,s2)
    | compare_label (ML_EXTERN_LABEL s1, ML_EXTERN_LABEL s2) = String.compare(s1,s2)
    | compare_label (C_EXTERN_LABEL s1, C_EXTERN_LABEL s2) = String.compare(s1,s2)
    | compare_label (LOCAL_CODE _, _) = LESS
    | compare_label (_, LOCAL_CODE _) = GREATER
    | compare_label (LOCAL_DATA _, _) = LESS
    | compare_label (_, LOCAL_DATA _) = GREATER
    | compare_label (LINK_EXTERN_LABEL _, _) = LESS
    | compare_label (_, LINK_EXTERN_LABEL _) = GREATER
    | compare_label (ML_EXTERN_LABEL _, _) = LESS
    | compare_label (_, ML_EXTERN_LABEL _) = GREATER
  fun eq_label (ML_EXTERN_LABEL s1, ML_EXTERN_LABEL s2) = s1 = s2
    | eq_label (LINK_EXTERN_LABEL s1, LINK_EXTERN_LABEL s2) = s1 = s2
    | eq_label (C_EXTERN_LABEL s1, C_EXTERN_LABEL s2) = s1 = s2
    | eq_label (LOCAL_DATA s1, LOCAL_DATA s2) = s1 = s2
    | eq_label (LOCAL_CODE s1, LOCAL_CODE s2) = s1 = s2
    | eq_label (_,_) = false
  fun hash_label (ML_EXTERN_LABEL s) = HashString.hashString s
    | hash_label (LINK_EXTERN_LABEL s) = HashString.hashString s
    | hash_label (C_EXTERN_LABEL s) = HashString.hashString s
    | hash_label (LOCAL_CODE s) = HashString.hashString s
    | hash_label (LOCAL_DATA s) = HashString.hashString s

  fun named_code_label s = LOCAL_CODE s
  fun named_data_label s = LOCAL_DATA s
  fun fresh_data_label s = LOCAL_DATA(Name.var2string(fresh_named_var s))
  fun fresh_code_label s = LOCAL_CODE(Name.var2string(fresh_named_var s))

  fun eqsregi(a : sregi, b) = a = b
  fun eqregi (REGI(v,_),REGI(v',_)) = eq_var(v,v')
    | eqregi (SREGI a, SREGI b) = eqsregi(a,b)
    | eqregi _ = false
  fun eqregf(REGF(v,_),REGF(v',_)) = eq_var(v,v')
  fun eqreg (I ir1, I ir2) = eqregi(ir1,ir2)
    | eqreg (F fr1, F fr2) = eqregf(fr1,fr2)
    | eqreg _ = false

  fun sregi2int HEAPALLOC = 0
    | sregi2int HEAPLIMIT = 1
    | sregi2int STACK = 2
    | sregi2int THREADPTR = 3
    | sregi2int EXNSTACK = 4
    | sregi2int EXNARG = 5
    | sregi2int HANDLER = 6

  (* This is okay since variable number start at 256 *)
  fun regi2int (REGI (v,_)) = let val n = Name.var2int v
				  val _ = if (n < 256) then error "variable number < 256" else ()
			      in  n
			      end
    | regi2int (SREGI sregi) = sregi2int sregi

 (* effective address: register + sign-extended displacement *) 
  datatype ea = REA of regi * int  
              | LEA of label * int 
              | RREA of regi * regi    (* needed for the MUTATE *)

  fun in_ea_disp_range x = x >= ~32768 andalso x<32768

  (* small value: small integer value or register *)

  datatype reg_or_label = REG' of regi
                        | LABEL' of label

  datatype sv = REG of regi
	      | IMM of int 

  (* in_imm_range: is an integer in a range of immediate values *)
  fun in_imm_range x =  TilWord32.ult(x,0w255)

  datatype cmp = EQ | LE | LT | GE | GT | NE 

  datatype traptype = INT_TT | REAL_TT | BOTH_TT

  datatype calltype = ML_NORMAL | ML_TAIL of regi | C_NORMAL

  datatype mutateType = INT_MUTATE | FLOAT_MUTATE | PTR_MUTATE | GLOBAL_INIT

  datatype instr = 
      LI     of TilWord32.word * regi
    | LADDR  of ea * regi               
    | MV     of regi * regi               (* src,dest *)
    | CMV    of cmp * regi * sv * regi    (* if ra cmp 0, then c <- b (signed compare) *)
    | FMV    of regf * regf 

    | ADD    of regi * sv * regi        (* add(a,b,c): c <- a+b *)
    | SUB    of regi * sv * regi        (* c <- a-b *)
    | MUL   of regi * sv * regi        (* c <- a*b *)
    | UDIV   of regi * sv * regi       (* c <- a/b *)
    | UMOD   of regi * sv * regi       (* c <- a mod b *)
    | S4ADD of regi * sv * regi        (* scaled add by 4: c <- a+4*b *)
    | S8ADD of regi * sv * regi        (* scaled add by 8: c <- a+8*b *)
    | S4SUB of regi * sv * regi        (* scaled sub by 4: c <- a-4*b *)
    | S8SUB of regi * sv * regi        (* scaled sub by 8: c <- a-8*b *)
    | ADDT   of regi * sv * regi        (* c <- a+b *)
    | SUBT   of regi * sv * regi        (* c <- a-b *)
    | MULT   of regi * sv * regi        (* c <- a*b *)
    | DIVT   of regi * sv * regi        (* c <- a/b *)
    | MODT   of regi * sv * regi        (* c <- a mod b *)
    | CMPSI  of cmp * regi * sv * regi  (* c <- a signed-op b *)
    | CMPUI  of cmp * regi * sv * regi  (* c <- a unsigned-op b *)

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

    (* BCND(SI/UI/F): compare operands with cmp and branch is cmp succeeds 
                      bool predicts whether branch taken or not *)
    | BCNDSI of cmp * regi * sv * label * bool
    | BCNDUI of cmp * regi * sv * label * bool
    | BCNDF  of cmp * regf * regf * label * bool
    | JMP    of regi * label list

    (* procedure call and return: these are "heavyweight" operations in
       this machine.   This avoids over-constraining register allocation.*)


    (* see sig for comments *)

    | CALL of {call_type : calltype,
	       func: reg_or_label,
	       args : reg list,
	       results : reg list,
	       save : reg list}
    | RETURN of regi                 (* address to return to *)


    | PUSH_EXN
    | POP_EXN
    | THROW_EXN
    | CATCH_EXN

    | LOAD8I    of ea * regi             
    | LOAD32I   of ea * regi
    | LOAD64F   of ea * regf

    (* unchecked stores - STOREMUTATE adds to write list *)
    | STORE8I   of ea * regi
    | STORE32I  of ea * regi
    | STORE64F  of ea * regf

    | MIRROR_GLOBAL_OFFSET of regi    (* 0 or 4 *)
    | MIRROR_PTR_ARRAY_OFFSET of regi (* 0 or 4 *)

    (* Use LOADSP to calculate a representation of the current stack
       pointer.  To restore a saved stack pointer, first load SREGI
       STACK with the value from LOADSP then RESTORESP. *)
      
    | LOADSP    of regi			(* a <- SP - stackletOffset *)
    | RESTORESP				(* SP <- SP + stackletOffset; also changes stacklet chain *)

    | STOREMUTATE of ea * mutateType
    | NEEDALLOC  of sv                        (* Calls GC if sv words are not allocatable *)
    | NEEDMUTATE of int                      (* Calls GC if int writes won't fit in write list *)
          
    | SOFT_VBARRIER of traptype
    | SOFT_ZBARRIER of traptype
    | HARD_VBARRIER of traptype
    | HARD_ZBARRIER of traptype

    | ILABEL of label
    | HALT
    | ICOMMENT of string

  datatype labelortag = PTR of label | TAG of TilWord32.word

  datatype data = 
      COMMENT  of string
    | DLABEL   of label                   (* data segment label *)
    | STRING   of string                  (* separate string case to avoid endian-ness issue *)
    | INT32    of TilWord32.word      
    | FLOAT    of string                  (* double-precision float point literal *)
    | DATA     of label                   (* address value - label as value *)

  (* see sig for comments *)
  datatype proc = PROC of {name : label,
			   return : regi,
			   args : reg list,
			   results : reg list,
			   code : instr array,
			   save : reg list,
                           vars : (int * int) option}

  type entry = {main : label,
		global_start : label,
		global_end : label,
		gc_table : label,
		trace_global_start : label,
		trace_global_end : label}
       
  datatype module = MODULE of
                          {procs : proc list,
			   data : data list,  (* assumed that data segment starts even-word aligned *)
			   entry : entry,
			   global : label list}

end
