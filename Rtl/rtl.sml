(*$import RTL TilWord32 Name *)

fun in_imm_range x =  TilWord32.ult(x,0w255)
fun in_ea_disp_range x = x >= ~32768 andalso x<32768

structure Rtl :> RTL = 
struct


    type var = Name.var
    val eq_var = Name.eq_var
    val fresh_var = Name.fresh_var
    val fresh_named_var = Name.fresh_named_var

    datatype label = ML_EXTERN_LABEL of string
                   | C_EXTERN_LABEL of string
                   | LOCAL_DATA of string
                   | LOCAL_CODE of string

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

  fun eq_label (ML_EXTERN_LABEL s1, ML_EXTERN_LABEL s2) = s2 = s1
    | eq_label (C_EXTERN_LABEL s1, C_EXTERN_LABEL s2) = s2 = s1
    | eq_label (LOCAL_DATA s1, LOCAL_DATA s2) = s1 = s2
    | eq_label (LOCAL_CODE s1, LOCAL_CODE s2) = s1 = s2
    | eq_label (_,_) = false
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

  fun sregi2int HEAPPTR = 0
    | sregi2int HEAPLIMIT = 1
    | sregi2int STACKPTR = 2
    | sregi2int THREADPTR = 3
    | sregi2int EXNPTR = 4
    | sregi2int EXNARG = 5

  (* This is okay since variable number start at 256 *)
  fun regi2int (REGI (v,_)) = Name.var2int v
    | regi2int (SREGI sregi) = sregi2int sregi

 (* effective address: register + sign-extended displacement *) 
  datatype ea = EA of regi * int  

  val in_ea_disp_range = in_ea_disp_range

  (* small value: small integer value or register *)

  datatype reg_or_label = REG' of regi
                        | LABEL' of label

  datatype sv = REG of regi
	      | IMM of int 

  (* in_imm_range: is an integer in a range of immediate values *)
  val in_imm_range = in_imm_range

  datatype align = LONG | QUAD | ODDLONG | OCTA | ODDOCTA

  datatype cmp =  EQ | LE  | LT  | GE  | GT | NE 

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
    | MOD   of regi * sv * regi        (* c <- a/b *)
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

    | NOTB   of regi * regi
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

    | CMPF   of cmp * regf * regf * regi

    (* flow of control instructions *)

    (* jumps and branches *)

    | BR     of label

    (* BCNDI, BCNDF: compare against 0 and branch.  bool = whether
       predicted taken or not; the comparison is signed  *)

    | BCNDI2  of cmp * regi * sv  * label * bool  (* signed *)
    | BCNDF2  of cmp * regf * regf * label * bool
    | BCNDI   of cmp * regi * label * bool  
    | BCNDF   of cmp * regf * label * bool
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

    (* see signature for comments *)

    | SAVE_CS of label
    | END_SAVE
    | RESTORE_CS

    | LOAD32I    of ea * regi           (* address, data *)
    | STORE32I   of ea * regi
    | LOADQF     of ea * regf
    | STOREQF    of ea * regf


    | MUTATE of ea * regi * regi option   (* if option is present, a nonzero value indicates pointer *)
    | INIT of ea * regi * regi option     (* if option is present, a nonzero value indicates pointer *)

    | NEEDGC     of sv
          
    | SOFT_VBARRIER of traptype
    | SOFT_ZBARRIER of traptype
    | HARD_VBARRIER of traptype
    | HARD_ZBARRIER of traptype

    | HANDLER_ENTRY
    | ILABEL of label
    | IALIGN of align
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
    | ALIGN    of align

  (* see sig for comments *)
  datatype proc = PROC of {name : label,
			   return : regi,
			   args : reg list,
			   results : reg list,
			   code : instr array,
			   known: bool,
			   save : reg list,
                           vars : (int * int) option}

  datatype module = MODULE of
                          {procs : proc list,
			   data : data list,
			   main : label,
			   mutable : (label * rep) list}

end (* fuctor RTL *)

