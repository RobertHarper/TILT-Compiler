structure Opcode : MLRISC_OPCODES =
struct

  datatype cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU
  datatype fcond = ? | !<=> | == | ?= | !<> | !?>= | < | ?< | !>= | !?> |
                   <= | ?<= | !> | !?<= | > | ?> | !<= | !?< | >= | ?>= |
                   !< | !?= | <> | != | !? | <=>

  datatype opcode =
      (* load immediate *) 
      LI 
    | COPY
    | ADD | SUB | MULU | DIVU | ADDT | MULT | SUBT | DIVT | SH1ADD | SH1ADDT
    | ANDB | ORB | XORB | SRA | SRL | SLL | EXTRU | EXTRS | ZDEP
    | FADDD | FMULD | FSUBD | FDIVD | FABSD | FNEGD | CVTI2D 
    | LOAD8 | LOAD16 | LOAD32 | LOADD 
    | LOAD8X | LOAD16X | LOAD32X | LOADDX
    | STORE8 | STORE16 | STORE32 | STORED 
    | STORE8X | STORE16X | STORE32X | STOREDX 
    | CMP of cond | FCMP of fcond | SET of cond
    | JMP | INDJMP | IDXJMP | RET | CALL | INDCALL
    | MISC of int
    | NOP
    | PHI of int

  fun toString opcode =
      case opcode of
      LI   => "li"
    | COPY => "copy"
    | ADD  => "add"
    | SUB  => "sub"
    | MULU => "mulu"
    | DIVU => "divu"
    | ADDT => "addt"
    | MULT => "mult"
    | SUBT => "subt"
    | DIVT => "divt"
    | SH1ADD => "sh1add"
    | SH1ADDT => "sh1addt"
    | ANDB => "andb"
    | ORB  => "orb"
    | XORB => "xorb"
    | SRA  => "sra"
    | SRL  => "srl"
    | SLL  => "sll"
    | EXTRU  => "extru"
    | EXTRS  => "extrs"
    | ZDEP  => "zdep"
    | FADDD => "faddd"
    | FMULD => "fmuld"
    | FSUBD => "fsubd"
    | FDIVD => "fdivd"
    | FABSD => "fabsd"
    | FNEGD => "fnegd"
    | CVTI2D => "cvti2d"
    | LOAD8 => "load8"
    | LOAD16 => "load16"
    | LOAD32 => "load32"
    | LOADD => "loadd"
    | LOAD8X => "load8x"
    | LOAD16X => "load16x"
    | LOAD32X => "load32x"
    | LOADDX => "loaddx"
    | STORE8 => "store8"
    | STORE16 => "store16"
    | STORE32 => "store32"
    | STORED => "stored"
    | STORE8X => "store8x"
    | STORE16X => "store16x"
    | STORE32X => "store32x"
    | STOREDX => "storedx"
    | CMP cond => "cmp"^condToString cond
    | FCMP fcond => "fcmp"^fcondToString fcond
    | SET cond => "set"^condToString cond
    | JMP    => "jmp"
    | INDJMP => "indjmp"
    | IDXJMP => "idxjmp"
    | RET    => "ret"
    | CALL   => "call"
    | INDCALL => "indcall"
    | MISC i => "misc"^Int.toString i
    | NOP => "nop"
    | PHI b=> "phi["^Int.toString b^"]"

  and condToString cond =
      case cond of
        LT  => "<"
      | LTU => "<u"
      | LE  => "<="
      | LEU => "<=u"
      | EQ  => "="
      | NE  => "!="
      | GE  => ">="
      | GEU => ">=u"
      | GT  => ">"
      | GTU => ">=u"

  and fcondToString c =
     case c of
       ?    => "?"
     | !<=> => "!<=>"
     | ==   => "=="
     | ?=   => "?="
     | !<>  => "!<>"
     | !?>= => "!?>="
     | op <    => "<"
     | ?<   => "?<"
     | !>=  => "!>="
     | !?>  => "!?>"
     | op <=   => "<="
     | ?<=  => "?<="
     | !>   => "!>"
     | !?<= => "!?<="
     | op >    => ">"
     | ?>   => "?>"
     | !<=  => "!<="
     | !?<  => "!?<"
     | op >=   => ">="
     | ?>=  => "?>="
     | !<   => "!<"
     | !?=  => "!?="
     | op <>   => "<>"
     | !=   => "!="
     | !?   => "!?"
     | <=>  => "<=>"

  fun hash opcode =
     case opcode of 
      LI   => 0
    | COPY => 100
    | ADD  => 200
    | SUB  => 300
    | MULU => 400
    | DIVU => 500
    | ADDT => 600
    | MULT => 700
    | SUBT => 800
    | DIVT => 900
    | SH1ADD => 930
    | SH1ADDT => 960
    | ANDB => 1000
    | ORB  => 1100
    | XORB => 1200
    | SRA  => 1300
    | SRL  => 1400
    | SLL  => 1500
    | EXTRU  => 6000
    | EXTRS  => 6100
    | ZDEP  => 6200
    | FADDD => 1600
    | FMULD => 1700
    | FSUBD => 1800
    | FDIVD => 1900
    | FABSD => 2000
    | FNEGD => 2100
    | CVTI2D => 2200
    | LOAD8  => 2300
    | LOAD16 => 2400
    | LOAD32 => 2500
    | LOADD  => 2600
    | LOAD8X => 2700
    | LOAD16X => 2800
    | LOAD32X => 2900
    | LOADDX  => 3000
    | STORE8  => 3100
    | STORE16 => 3200
    | STORE32 => 3300
    | STORED  => 3400
    | STORE8X => 3500
    | STORE16X => 3600
    | STORE32X => 3700
    | STOREDX => 3800
    | CMP cond => 3900 + hashCond cond
    | FCMP fcond => 4000 + hashFcond fcond
    | SET cond => 4100 + hashCond cond
    | JMP    => 4200
    | INDJMP => 4300
    | IDXJMP => 4400
    | RET    => 4500
    | CALL    => 4600
    | INDCALL => 4700
    | MISC i => 5000 + i
    | NOP => 5100
    | PHI b => 5200 + b

  and hashCond cond = 
      case cond of
        LT  => 0
      | LTU => 64
      | LE  => 128
      | LEU => 192
      | EQ  => 256
      | NE  => 512
      | GE  => 1024
      | GEU => 2048
      | GT  => 3172
      | GTU => 4096

  and hashFcond fcond =
      case fcond of
       ?    => 1
     | !<=> => 2
     | ==   => 3
     | ?=   => 123
     | !<>  => 12412
     | !?>= => 5837
     | op < => 35
     | ?<   => 1234
     | !>=  => 125534
     | !?>  => 12385
     | op <= => 857349
     | ?<=  => 683
     | !>   => 1245
     | !?<= => 124984
     | op > => 1234
     | ?>   => 132584
     | !<=  => 12837
     | !?<  => 75738
     | op >= => 234
     | ?>=  => 881
     | !<   => 91
     | !?=  => 483
     | op <> => 54738
     | !=   => 1234
     | !?   => 4638
     | <=>  => 56483

  fun revCond cond = 
      case cond of
        LT  => GE
      | LTU => GEU
      | LE  => GT
      | LEU => GTU
      | EQ  => NE
      | NE  => EQ
      | GE  => LT
      | GEU => LTU
      | GT  => LE
      | GTU => LEU


   fun revFcond ?    = !?
     | revFcond !<=> = <=>
     | revFcond ==   = !=
     | revFcond ?=   = !?=
     | revFcond !<>  = op <>
     | revFcond !?>= = ?>=
     | revFcond op < = !<
     | revFcond ?<   = !?<
     | revFcond !>=  = op >=
     | revFcond !?>  = ?>
     | revFcond op <= = !<=
     | revFcond ?<=  = !?<=
     | revFcond !>   = op >
     | revFcond !?<= = ?<=
     | revFcond op >  = !>
     | revFcond ?>   = !?>
     | revFcond !<=  = op <=
     | revFcond !?<  = ?<
     | revFcond op >= = !>=
     | revFcond ?>=  = !?>=
     | revFcond !<   = op <
     | revFcond !?=  = ?=
     | revFcond op <>  = !<>
     | revFcond !=   = ==
     | revFcond !?   = ?
     | revFcond <=>  = !<=>

   fun isCommutative opcode =
       case opcode of 
         ( ADD | MULU | ADDT | MULT | ANDB | ORB | XORB 
         | FADDD | FMULD | CMP NE | CMP EQ | FCMP == | FCMP != 
         ) => true
       | _ => false
end

