signature MLRISC_OPCODES =
sig

  datatype cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU
  datatype fcond = ? | !<=> | == | ?= | !<> | !?>= | < | ?< | !>= | !?> |
                   <= | ?<= | !> | !?<= | > | ?> | !<= | !?< | >= | ?>= |
                   !< | !?= | <> | != | !? | <=>

  datatype opcode =
      (* load immediate *) 
      LI 
      (* copy *)
    | COPY
      (* arithmetic *) 
    | ADD | SUB | MULU | DIVU | ADDT | MULT | SUBT | DIVT | SH1ADD | SH1ADDT
      (* logical *) 
    | ANDB | ORB | XORB | SRA | SRL | SLL | EXTRS | EXTRU | ZDEP
      (* floating point *)
    | FADDD | FMULD | FSUBD | FDIVD | FABSD | FNEGD | CVTI2D 
      (* load *)
    | LOAD8 | LOAD16 | LOAD32 | LOADD 
      (* indexed load *)
    | LOAD8X | LOAD16X | LOAD32X | LOADDX
      (* store *)
    | STORE8 | STORE16 | STORE32 | STORED 
      (* indexed store *)
    | STORE8X | STORE16X | STORE32X | STOREDX 
      (* branches *)
    | CMP of cond | FCMP of fcond | SET of cond
      (* jump/calls *)
    | JMP | INDJMP | IDXJMP | RET | CALL | INDCALL
      (* Miscellenous op *)
    | MISC of int
      (* Nop *)
    | NOP
      (* phi *)
    | PHI of int

  val toString : opcode -> string  (* pretty print *)
  val hash     : opcode -> int     (* hash function *)
  val revCond  : cond -> cond      (* reverse *)
  val revFcond : fcond -> fcond    (* reverse *)
  val isCommutative : opcode -> bool (* communtative operation? *)

end
