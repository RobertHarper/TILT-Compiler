functor Rtl (val in_imm_range : int -> bool
	     val in_ea_disp_range : int -> bool) : RTL =
struct


    type var = Name.var
    val eq_var = Name.eq_var
    val fresh_var = Name.fresh_var
    val fresh_named_var = Name.fresh_named_var

    datatype local_label = LOCAL_DATA of var 
                         | LOCAL_CODE of var
    datatype label = ML_EXTERN_LABEL of string
                   | C_EXTERN_LABEL of string
                   | LOCAL_LABEL of local_label

  datatype sregi = HEAPPTR | HEAPLIMIT | EXNPTR | EXNARG | STACKPTR
  datatype regi = REGI of var * rep  (* int in var is register # *)
                | SREGI of sregi
  and regf = REGF of var * rep

  and rep_path = 
       Var_p of regi | Projvar_p of (regi * int list)
     | Label_p of label | Projlabel_p of (label * int list) | Notneeded_p

  and rep = TRACE
          | UNSET         (* a locative address that is not yet set; needs to be set once *)
          | NOTRACE_INT
          | NOTRACE_CODE
          | NOTRACE_REAL
          | LABEL 
          | LOCATIVE
          | COMPUTE of rep_path

  fun named_code_label s = LOCAL_CODE(fresh_named_var s)
  fun named_data_label s = LOCAL_DATA(fresh_named_var s)
  fun eq_locallabel (LOCAL_DATA v1, LOCAL_DATA v2) = eq_var(v1,v2)
    | eq_locallabel (LOCAL_CODE v1, LOCAL_CODE v2) = eq_var(v1,v2)
    | eq_locallabel (_,_) = false
  fun eq_label (ML_EXTERN_LABEL s1, ML_EXTERN_LABEL s2) = s2 = s1
    | eq_label (C_EXTERN_LABEL s1, C_EXTERN_LABEL s2) = s2 = s1
    | eq_label (LOCAL_LABEL ll1, LOCAL_LABEL ll2) = eq_locallabel(ll1,ll2)
    | eq_label (_,_) = false
  fun fresh_data_label () = LOCAL_DATA(fresh_var())
  fun fresh_code_label () = LOCAL_CODE(fresh_var())

  fun eqsregi(a : sregi, b) = a = b
  fun eqregi (REGI(v,_),REGI(v',_)) = eq_var(v,v')
    | eqregi (SREGI a, SREGI b) = eqsregi(a,b)
    | eqregi _ = false
  fun eqregf(REGF(v,_),REGF(v',_)) = eq_var(v,v')
(*
  val heapptr = REGI(V(25,"heapptr"),NOTRACE_INT)
  val exnptr = REGI(V(24,"exnptr"),NOTRACE_INT)
  val stackptr = REGI(V(30,"stackptr"),NOTRACE_INT)
  val exnarg = REGI(V(26,"exnarg"),TRACE)
*)

  (* save: set of registers to save before a procedure call or at the
     start of executing a procedure body.  These registers are restored
     after the procedure call or executing the procedure body.*)

  datatype save = SAVE of regi list * regf list

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

  datatype cmp =  EQ | LE  | LT  | GE  | GT | NE | LBC | LBS

  datatype traptype = INT_TT | REAL_TT | BOTH_TT
  datatype instr = 
      LI     of Word32.word * regi
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

    | SQRT   of regf * regf
    | SIN    of regf * regf
    | COS    of regf * regf
    | ARCTAN of regf * regf
    | EXP    of regf * regf
    | LN     of regf * regf
    | CMPF   of cmp * regf * regf * regi

    (* flow of control instructions *)

    (* jumps and branches *)

    | BR     of local_label

    (* BCNDI, BCNDF: compare against 0 and branch.  bool = whether
       predicted taken or not; the comparison is signed  *)

    | BCNDI2  of cmp * regi * sv  * local_label * bool  (* signed *)
    | BCNDF2  of cmp * regf * regf * local_label * bool
    | BCNDI   of cmp * regi * local_label * bool  
    | BCNDF   of cmp * regf * local_label * bool
    | JMP    of regi * local_label list

    (* procedure call and return: these are "heavyweight" operations in
       this machine.   This avoids over-constraining register allocation.*)


    (* see sig for comments *)

    | CALL of {func: reg_or_label,
	       return : regi option,
	       args : regi list * regf list, 
	       results : regi list * regf list,
	       tailcall : bool,
	       save : save}
    | RETURN of regi                 (* address to return to *)

    (* see signature for comments *)

    | SAVE_CS of local_label
    | END_SAVE
    | RESTORE_CS

    | LOAD32I    of ea * regi           (* address, data *)
    | STORE32I   of ea * regi
    | LOADQF     of ea * regf
    | STOREQF    of ea * regf


    | NEEDMUTATE of regi
    | NEEDGC     of sv
    | FLOAT_ALLOC of regi * regf * regi * Word32.word
    | INT_ALLOC   of regi * regi * regi * Word32.word
    | PTR_ALLOC   of regi * regi * regi * Word32.word
          
    | SOFT_VBARRIER of traptype
    | SOFT_ZBARRIER of traptype
    | HARD_VBARRIER of traptype
    | HARD_ZBARRIER of traptype

    | HANDLER_ENTRY
    | ILABEL     of local_label
    | IALIGN of align
    | HALT

  datatype labelortag = PTR of label | TAG of Word32.word


  datatype data = 
      COMMENT of string
    | STRING of (string)
    | INT32 of  (Word32.word)
    | INT_FLOATSIZE of (Word32.word)
    | FLOAT of  (string)
    | DATA of   (label)
(* array of i words inited to word32 *)
    | ARRAYI of (int * Word32.word)
(* array of i words initialized to fp value in string *)
    | ARRAYF of (int * string)
(* array of i words initialized to label or small int *)
    | ARRAYP of (int * labelortag)
    | ALIGN of  (align)
    | DLABEL of (label)


  (* see sig for comments *)
  datatype proc = PROC of {name : local_label,
			   return : regi,
			   args : regi list * regf list ,
			   results : regi list * regf list,
			   code : instr array,
			   known: bool,
			   save : save,
                           vars : (int * int) option}

  datatype module = MODULE of
                          {procs : proc list,
			   data : data array,
			   main : local_label,
			   mutable_objects : label list,
			   mutable_variables : (label * rep) list}

end (* fuctor RTL *)

