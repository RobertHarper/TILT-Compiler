(*
 * exp describes the semantics of an instruction 
 *)
signature SSA_EXP =
sig

  datatype cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU 
                | VC | VS | SETCC
  datatype fcond = ? | !<=> | == | ?= | !<> | !?>= | < | ?< | !>= | !?> |
                   <= | ?<= | !> | !?<= | > | ?> | !<= | !?< | >= | ?>= |
                   !< | !?= | <> | != | !? | <=> | SETFCC

            (* supported types *)
  datatype ty   = I8 | U8 | I16 | U16 | I32 | U32 | I64 | U64 | F | D

            (* arithmetic operations *)
  datatype binop = ADD | SUB | MUL | DIV | MOD
                 | ADDT | SUBT | MULT | DIVT | MODT 
                 | ANDB | ORB | XORB | SRA | SRL | SLL 
                 | CMP of cond | FCMP of fcond | SET of cond 
                 | TRAP of cond
  datatype unary = ABS | NEG | NOT | NOTB | SQRT | CVT of ty 
                 | CC of cond | FCC of fcond

  datatype exp =
      LI 
    | ID of int
    | COPY
    | PC
    | XFER
    | BINOP of binop * ty * exp * exp
    | UNARY of unary * ty * exp
    | LOAD  of ty * exp         (* addr *)
    | STORE of ty * exp * exp   (* data addr *) 
    | INIT  of ty * exp * exp   (* data addr *)
    | DEALLOC of exp
    | JMP of exp
    | BRANCH of exp
    | CALL of exp
    | RET 
    | NOP
    | PAR of exp list  (* parallel actions *)
    | PHI of int
    | PINNED of exp
    | MISC of {hash:int,arity:int,name:string,attribs:int} ref

  val A_TRAPPING : int
  val A_PINNED   : int
  val A_MUTATOR  : int
  val A_LOOKER   : int
  val A_PURE     : int

  val hash     : exp -> int         (* hash function *)
  val toString : string list -> exp -> string      (* pretty print *)
  val revCond  : cond -> cond       (* reverse *)
  val revFcond : fcond -> fcond     (* reverse *)

       (* generate a new instruction *)
  val misc     : {name:string,arity:int,attribs:int} -> exp 

  val isCommutative : exp -> bool  (* commutative operation? *)
  val isTrapping    : exp -> bool  (* can it generate an arithmetic trap? *)
  val isPinned      : exp -> bool  (* is it safe to removed it? *)    

  val isMutator     : exp -> bool  
  val isLooker      : exp -> bool
  val isPure        : exp -> bool

  val can'tMoveDown : exp -> bool  (* can't be moved downward *)
  val can'tMoveUp   : exp -> bool  (* can't be moved upward *)
end

(*
 * $Log$
# Revision 1.2  2001/12/13  16:31:48  swasey
# *** empty log message ***
# 
# Revision 1.1  99/02/17  21:14:53  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:21  pscheng
# *** empty log message ***
#
 *)
