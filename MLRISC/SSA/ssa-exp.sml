structure SSAExp : SSA_EXP =
struct

  infix &&

  fun x && y = Word.andb(Word.fromInt x,Word.fromInt y) <> 0w0

  fun error msg = MLRiscErrorMsg.impossible("SSAExp."^msg)

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
    | XFER
    | COPY
    | PC
    | BINOP of binop * ty * exp * exp
    | UNARY of unary * ty * exp
    | LOAD  of ty * exp         (* addr *)
    | STORE of ty * exp * exp   (* data addr  *) 
    | INIT  of ty * exp * exp   (* data addr *)
    | DEALLOC of exp
    | JMP of exp
    | BRANCH of exp
    | RET 
    | CALL of exp
    | NOP
    | PAR of exp list
    | PHI of int
    | PINNED of exp
    | MISC of {hash:int, arity:int, name:string, attribs:int} ref

  val A_TRAPPING = 0x1
  val A_PINNED   = 0x2
  val A_MUTATOR  = 0x4
  val A_LOOKER   = 0x8
  val A_PURE     = 0x10

  fun tyToString ty =
      case ty of
        I8  => "8"
      | U8  => "u8"
      | I16 => "16"
      | U16 => "u16"
      | I32 => ""
      | U32 => "u"
      | I64 => "64"
      | U64 => "u64"
      | F   => "f"
      | D   => "d"

  fun condToString cond =
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
      | VC  => "vc"
      | VS  => "vs"
      | SETCC => "cc"

  fun fcondToString c =
     case c of
       ?     => "?"
     | !<=>  => "!<=>"
     | ==    => "=="
     | ?=    => "?="
     | !<>   => "!<>"
     | !?>=  => "!?>="
     | op <  => "<"
     | ?<    => "?<"
     | !>=   => "!>="
     | !?>   => "!?>"
     | op <= => "<="
     | ?<=   => "?<="
     | !>    => "!>"
     | !?<=  => "!?<="
     | op >  => ">"
     | ?>    => "?>"
     | !<=   => "!<="
     | !?<   => "!?<"
     | op >= => ">="
     | ?>=   => "?>="
     | !<    => "!<"
     | !?=   => "!?="
     | op <> => "<>"
     | !=    => "!="
     | !?    => "!?"
     | <=>   => "<=>"
     | SETFCC => "fcc"

  fun binopToString a =
      case a of
      ADD  => "add"
    | SUB  => "sub"
    | MUL  => "mul"
    | DIV  => "div"
    | MOD  => "mod"
    | ADDT => "addt"
    | SUBT => "subt"
    | MULT => "mult"
    | DIVT => "divt"
    | MODT => "modt"
    | ANDB => "andb"
    | ORB  => "orb"
    | XORB => "xorb"
    | SRA  => "sra"
    | SRL  => "srl"
    | SLL  => "sll"
    | CMP c  => "cmp"^condToString c
    | FCMP c => "fcmp"^fcondToString c
    | SET c  => "set"^condToString c
    | TRAP c => "trap"^condToString c

  fun unaryToString a =
      case a of
      ABS    => "abs"
    | NEG    => "neg"
    | NOT    => "not"
    | NOTB   => "notb"
    | SQRT   => "sqrt"
    | CVT ty => "cvt"^tyToString ty
    | CC cc  => condToString cc
    | FCC cc => fcondToString cc

  fun list (l,sep,r) f es =
       l^List.foldr (fn (x,"") => f x | (x,s)  => f x^sep^s) "" es^r 

  fun toString args =
  let fun paren e = "("^exp e^")"
      and exps es = list ("(",",",")") exp es 
      and seqs es = exps es
      and arg i = List.nth(args,i) handle Subscript => "?"
      and allArgs() = list ("(",",",")") (fn x => x) args
      and argn n    = list ("(",",",")") (fn x => x) (List.take(args,n))
      and exp e =
          case e of
            LI   => "li("^arg 0^")"
          | XFER => "xfer("^arg 0^")"
          | COPY => "copy"^allArgs()
          | PC   => "pc"
          | ID i => (List.nth(args,i) handle Subscript => "?")
          | BINOP(a,t,x,y) => binopToString a^tyToString t^exps[x,y]
          | UNARY(a,t,x) => unaryToString a^tyToString t^paren x
          | LOAD(t,a) => "load"^tyToString t^paren a
          | STORE(t,x,a) => "store"^tyToString t^exps[x,a]
          | INIT(t,x,a) => "init"^tyToString t^exps[x,a]
          | DEALLOC e => "dealloc"^paren e
          | JMP e  => "jmp"^paren e
          | BRANCH e => "branch"^paren e
          | RET => "ret"
          | CALL e => "call"^allArgs()
          | NOP    => "nop"
          | PAR es => seqs es
          | PHI b  => "phi["^Int.toString b^"]"
          | PINNED e => exp e
          | MISC(ref{name,arity,...}) => name^argn arity
  in  exp
  end

  fun hashTy ty =
      case ty of
        I8  => 12
      | U8  => 15
      | I16 => 58
      | U16 => 919
      | I32 => 123
      | U32 => 1651
      | I64 => 889
      | U64 => 321
      | F   => 789 
      | D   => 2368
  
  fun hashCond cond = 
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
      | VC  => 3455
      | VS  => 24515
      | SETCC => 1312

  fun hashFcond fcond =
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
     | SETFCC => 123145

  fun hashBinop a = 
      case a of
      ADD  => 1341
    | SUB  => 142515
    | MUL  => 131
    | DIV  => 514
    | MOD  => 15151
    | ADDT => 41341
    | SUBT => 4142515
    | MULT => 4131
    | DIVT => 4514
    | MODT => 415151
    | ANDB => 1351
    | ORB  => 591
    | XORB => 4980
    | SRA  => 8591
    | SRL  => 17541
    | SLL  => 3128
    | CMP(c) => 5000 + hashCond c
    | FCMP(c) => 6000 + hashFcond c
    | SET(c) => 7000 + hashCond c 
    | TRAP(c)=> 8000 + hashCond c

  fun hashUnary a =
      case a of
      ABS  => 12434
    | NEG  => 49
    | NOT  => 4414
    | NOTB => 245
    | SQRT => 3445
    | CVT _ => 123
    | CC cc => 145134 + hashCond cc
    | FCC cc => 5155 + hashFcond cc

  fun hash e =
      case e of 
      LI   => 0
    | XFER => 500
    | COPY => 1000
    | PC   => 1500
    | ID i => 2000 + i
    | BINOP(a,t,x,y) => 3000 + hashBinop a + hashTy t + hash x + hash y
    | UNARY(a,t,x) => 4000 + hashUnary a + hashTy t + hash x
    | LOAD(t,a) => 8000 + hashTy t + hash a 
    | STORE(t,x,a) => 9000 + hashTy t + hash x + hash a
    | INIT(t,x,a) => 9500 + hashTy t + hash x + hash a
    | DEALLOC e => 32000 + hash e
    | JMP e  => 10000 + hash e
    | BRANCH e => 22000 + hash e
    | RET => 13000
    | CALL e => 14000 + hash e
    | NOP    => 16000
    | PAR es => 17000 + hashExps es
    | PHI b  => 18000 + b
    | PINNED e => hash e
    | MISC(ref{hash,...}) => hash

  and hashExps [] = 0
    | hashExps (e::es) = hash e + hashExps es

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
      | _   => error "revCond"

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
     | revFcond _    = error "revFcond"

  fun isCommutative(BINOP(a,_,_,_)) = isCommutativeBinop a
    | isCommutative(BRANCH e) = isCommutative e
    | isCommutative _ = false

  and isCommutativeBinop(ADD | MUL | ANDB | ORB | XORB | ADDT | MULT) = true
    | isCommutativeBinop(CMP(EQ | NE)) = true
    | isCommutativeBinop(SET(EQ | NE)) = true
    | isCommutativeBinop(TRAP(EQ | NE)) = true
    | isCommutativeBinop(FCMP(== | !=)) = true
    | isCommutativeBinop _ = false

  fun isTrapping(BINOP(a,_,_,_)) = isTrappingBinop a
    | isTrapping(MISC(ref{attribs,...})) = attribs && A_TRAPPING
    | isTrapping(CALL _) = true
    | isTrapping _ = false
  and isTrappingBinop(ADDT | SUBT | MULT | DIVT | MODT | TRAP _) = true
    | isTrappingBinop _ = false

  fun isPinned(JMP _) = true
    | isPinned(XFER) = true
    | isPinned(BRANCH _) = true
    | isPinned(CALL _) = true
    | isPinned(RET) = true
    | isPinned(STORE _) = true
    | isPinned(INIT _) = true
    | isPinned(DEALLOC _) = true
    | isPinned(BINOP(a,_,x,y)) = isPinned x orelse isPinned y
    | isPinned(UNARY(_,_,x)) = isPinned x
    | isPinned(PAR es) = List.exists isPinned es
    | isPinned(MISC(ref{attribs,...})) = attribs && A_PINNED
    | isPinned(PINNED _) = true
    | isPinned _ = false

  fun isMutator(LI)     = false
    | isMutator(ID _)   = false
    | isMutator(XFER)   = false
    | isMutator(COPY)   = false
    | isMutator(PC)     = false
    | isMutator(BINOP(_,_,x,y)) = isMutator x orelse isMutator y
    | isMutator(UNARY(_,_,x)) = isMutator x
    | isMutator(LOAD(_,e)) = isMutator e
    | isMutator(STORE(_,x,y)) = true
    | isMutator(INIT(_,x,y)) = true
    | isMutator(DEALLOC _) = true
    | isMutator(JMP x) = isMutator x
    | isMutator(BRANCH x) = isMutator x
    | isMutator(RET) = false 
    | isMutator(CALL _) = true
    | isMutator(NOP) = false
    | isMutator(PAR es) = List.exists isMutator es
    | isMutator(PHI _) = false
    | isMutator(PINNED e) = isMutator e
    | isMutator(MISC(ref{attribs,...})) = attribs && A_MUTATOR

  fun isLooker(LI)     = false
    | isLooker(ID _)   = false
    | isLooker(XFER)   = false
    | isLooker(COPY)   = false
    | isLooker(PC)     = false
    | isLooker(BINOP(_,_,x,y)) = isLooker x orelse isLooker y
    | isLooker(UNARY(_,_,x)) = isLooker x
    | isLooker(LOAD(_,e)) = true
    | isLooker(STORE(_,x,y)) = isLooker x orelse isLooker y
    | isLooker(INIT(_,x,y)) = isLooker x orelse isLooker y
    | isLooker(DEALLOC x) = isLooker x
    | isLooker(JMP x) = isLooker x
    | isLooker(BRANCH x) = isLooker x
    | isLooker(RET) = false 
    | isLooker(CALL _) = true
    | isLooker(NOP) = false
    | isLooker(PAR es) = List.exists isLooker es
    | isLooker(PHI _) = false
    | isLooker(PINNED e) = isLooker e
    | isLooker(MISC(ref{attribs,...})) = attribs && A_LOOKER

  fun isPure(LI)     = true
    | isPure(ID _)   = true
    | isPure(XFER)   = true
    | isPure(COPY)   = true
    | isPure(PC)     = true
    | isPure(BINOP(_,_,x,y)) = isPure x andalso isPure y
    | isPure(UNARY(_,_,x)) = isPure x
    | isPure(LOAD(_,e)) = false
    | isPure(STORE(_,x,y)) = false
    | isPure(INIT(_,x,y)) = false
    | isPure(DEALLOC _) = false
    | isPure(JMP x) = isPure x
    | isPure(BRANCH x) = isPure x
    | isPure(RET) = true 
    | isPure(CALL _) = false
    | isPure(NOP) = true
    | isPure(PAR es) = List.all isPure es
    | isPure(PHI _) = true
    | isPure(PINNED e) = isPure e
    | isPure(MISC(ref{attribs,...})) = attribs && A_PURE

  fun can'tMoveDown(BRANCH _) = true
    | can'tMoveDown(JMP _)   = true
    | can'tMoveDown(RET)     = true
    | can'tMoveDown(CALL _)  = true
    | can'tMoveDown(STORE _) = true
    | can'tMoveDown(INIT _)    = true
    | can'tMoveDown(DEALLOC _) = true
    | can'tMoveDown(LOAD _)  = true
    | can'tMoveDown(BINOP(a,_,_,_)) = isTrappingBinop a
    | can'tMoveDown(MISC(ref{attribs,...})) = attribs && A_TRAPPING
    | can'tMoveDown(PINNED _) = true
    | can'tMoveDown _ = false 

  fun can'tMoveUp(BRANCH _)  = true
    | can'tMoveUp(JMP _)     = true
    | can'tMoveUp(RET)       = true
    | can'tMoveUp(CALL _)    = true
    | can'tMoveUp(STORE _)   = true
    | can'tMoveUp(INIT _)    = true
    | can'tMoveUp(DEALLOC _) = true
   (* | can'tMoveUp(LOAD _)  = true *)
    | can'tMoveUp(BINOP(a,_,_,_)) = isTrappingBinop a
    | can'tMoveUp(MISC(ref{attribs,...})) = attribs && A_TRAPPING
    | can'tMoveUp(PINNED _) = true
    | can'tMoveUp _ = false 

  val newHash = ref 123

  fun misc{name,arity,attribs} = 
         MISC(ref{name=name,arity=arity,hash= !newHash,attribs=attribs})
             before newHash := !newHash + 89

end

(*
 * $Log$
# Revision 1.1  99/02/17  21:14:57  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:06:22  pscheng
# *** empty log message ***
#
 *)
