functor ProjectLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Project_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Parser for project description files. *)
structure E = ExtSyn

type label = Name.label

fun var_label (x:string) : label = Name.symbol_label(Symbol.varSymbol x)
val env_label= Name.env_label
val unit_label = Name.unit_label
val interface_label = Name.interface_label

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\173\000\008\000\157\000\025\000\173\000\026\000\173\000\
\\029\000\173\000\030\000\173\000\031\000\173\000\032\000\173\000\
\\033\000\173\000\034\000\173\000\035\000\173\000\036\000\173\000\000\000\
\\001\000\002\000\023\000\004\000\022\000\006\000\097\000\017\000\021\000\
\\018\000\020\000\021\000\019\000\024\000\018\000\038\000\017\000\
\\039\000\016\000\040\000\015\000\000\000\
\\001\000\002\000\023\000\004\000\022\000\017\000\021\000\018\000\020\000\
\\021\000\019\000\024\000\018\000\027\000\082\000\028\000\081\000\
\\038\000\017\000\039\000\016\000\040\000\015\000\000\000\
\\001\000\002\000\023\000\004\000\022\000\017\000\021\000\018\000\020\000\
\\021\000\019\000\024\000\018\000\027\000\089\000\028\000\088\000\
\\038\000\017\000\039\000\016\000\040\000\015\000\000\000\
\\001\000\002\000\023\000\004\000\022\000\017\000\021\000\018\000\020\000\
\\021\000\019\000\024\000\018\000\027\000\101\000\028\000\100\000\
\\038\000\017\000\039\000\016\000\040\000\015\000\000\000\
\\001\000\002\000\023\000\004\000\022\000\017\000\021\000\018\000\020\000\
\\021\000\019\000\024\000\018\000\038\000\017\000\039\000\016\000\
\\040\000\015\000\000\000\
\\001\000\002\000\023\000\004\000\022\000\017\000\021\000\038\000\017\000\
\\039\000\016\000\040\000\015\000\000\000\
\\001\000\002\000\045\000\004\000\044\000\017\000\043\000\025\000\042\000\
\\026\000\041\000\000\000\
\\001\000\003\000\039\000\005\000\074\000\008\000\038\000\009\000\037\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\013\000\033\000\
\\014\000\032\000\022\000\031\000\023\000\030\000\000\000\
\\001\000\003\000\039\000\006\000\097\000\008\000\038\000\009\000\037\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\013\000\033\000\
\\014\000\032\000\022\000\031\000\023\000\030\000\000\000\
\\001\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\019\000\073\000\022\000\031\000\023\000\030\000\000\000\
\\001\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\020\000\107\000\022\000\031\000\023\000\030\000\000\000\
\\001\000\005\000\090\000\000\000\
\\001\000\007\000\122\000\000\000\
\\001\000\008\000\052\000\000\000\
\\001\000\008\000\053\000\000\000\
\\001\000\008\000\057\000\015\000\056\000\016\000\055\000\000\000\
\\001\000\008\000\083\000\000\000\
\\001\000\015\000\104\000\000\000\
\\001\000\035\000\092\000\000\000\
\\001\000\037\000\027\000\000\000\
\\001\000\037\000\028\000\000\000\
\\001\000\037\000\029\000\000\000\
\\001\000\037\000\048\000\000\000\
\\001\000\037\000\050\000\000\000\
\\001\000\037\000\068\000\000\000\
\\001\000\037\000\069\000\000\000\
\\001\000\037\000\070\000\000\000\
\\001\000\037\000\072\000\000\000\
\\001\000\037\000\084\000\000\000\
\\001\000\037\000\085\000\000\000\
\\001\000\037\000\117\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\022\000\031\000\023\000\030\000\000\000\
\\141\000\000\000\
\\142\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\000\000\
\\143\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\022\000\031\000\000\000\
\\144\000\003\000\039\000\000\000\
\\145\000\003\000\039\000\000\000\
\\146\000\003\000\039\000\000\000\
\\147\000\003\000\039\000\000\000\
\\148\000\003\000\039\000\000\000\
\\149\000\003\000\039\000\000\000\
\\150\000\003\000\039\000\000\000\
\\151\000\000\000\
\\152\000\037\000\110\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\003\000\039\000\006\000\097\000\008\000\038\000\009\000\037\000\
\\010\000\036\000\011\000\035\000\012\000\034\000\013\000\033\000\
\\014\000\032\000\022\000\031\000\023\000\030\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\015\000\104\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\002\000\023\000\004\000\022\000\017\000\021\000\018\000\020\000\
\\021\000\019\000\024\000\018\000\038\000\017\000\039\000\016\000\
\\040\000\015\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\033\000\077\000\034\000\076\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\022\000\031\000\023\000\030\000\000\000\
\\180\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\022\000\031\000\023\000\030\000\000\000\
\\181\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\022\000\031\000\023\000\030\000\000\000\
\\182\000\000\000\
\\183\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\022\000\031\000\023\000\030\000\000\000\
\\184\000\003\000\039\000\008\000\038\000\009\000\037\000\010\000\036\000\
\\011\000\035\000\012\000\034\000\013\000\033\000\014\000\032\000\
\\022\000\031\000\023\000\030\000\025\000\011\000\026\000\010\000\
\\029\000\009\000\030\000\008\000\031\000\007\000\032\000\006\000\
\\036\000\005\000\000\000\
\\184\000\025\000\011\000\026\000\010\000\029\000\009\000\030\000\008\000\
\\031\000\007\000\032\000\006\000\036\000\005\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\"
val actionRowNumbers =
"\090\000\092\000\090\000\006\000\
\\006\000\006\000\006\000\021\000\
\\022\000\023\000\091\000\088\000\
\\044\000\037\000\036\000\035\000\
\\008\000\007\000\006\000\024\000\
\\006\000\025\000\089\000\086\000\
\\085\000\015\000\016\000\017\000\
\\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\
\\006\000\006\000\057\000\026\000\
\\027\000\028\000\008\000\029\000\
\\047\000\011\000\034\000\009\000\
\\033\000\074\000\006\000\003\000\
\\018\000\030\000\031\000\004\000\
\\049\000\048\000\056\000\055\000\
\\054\000\053\000\051\000\050\000\
\\052\000\045\000\040\000\039\000\
\\042\000\013\000\041\000\006\000\
\\038\000\020\000\090\000\006\000\
\\084\000\077\000\061\000\067\000\
\\069\000\005\000\078\000\001\000\
\\079\000\061\000\019\000\002\000\
\\043\000\012\000\087\000\075\000\
\\089\000\066\000\062\000\058\000\
\\010\000\061\000\081\000\002\000\
\\064\000\071\000\032\000\019\000\
\\010\000\006\000\074\000\014\000\
\\058\000\068\000\080\000\083\000\
\\010\000\070\000\065\000\063\000\
\\073\000\019\000\046\000\076\000\
\\060\000\059\000\082\000\072\000\
\\000\000"
val gotoT =
"\
\\012\000\002\000\013\000\001\000\014\000\124\000\000\000\
\\000\000\
\\012\000\002\000\013\000\010\000\000\000\
\\001\000\012\000\003\000\011\000\000\000\
\\001\000\012\000\003\000\022\000\000\000\
\\001\000\012\000\003\000\023\000\000\000\
\\001\000\012\000\003\000\024\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\038\000\000\000\
\\001\000\044\000\000\000\
\\001\000\012\000\003\000\045\000\000\000\
\\000\000\
\\001\000\012\000\003\000\047\000\000\000\
\\000\000\
\\012\000\002\000\013\000\049\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\052\000\000\000\
\\001\000\012\000\003\000\056\000\000\000\
\\001\000\012\000\003\000\057\000\000\000\
\\001\000\012\000\003\000\058\000\000\000\
\\001\000\012\000\003\000\059\000\000\000\
\\001\000\012\000\003\000\060\000\000\000\
\\001\000\012\000\003\000\061\000\000\000\
\\001\000\012\000\003\000\062\000\000\000\
\\001\000\012\000\003\000\063\000\000\000\
\\001\000\012\000\003\000\064\000\000\000\
\\001\000\012\000\003\000\065\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\073\000\000\000\
\\001\000\012\000\003\000\076\000\000\000\
\\001\000\012\000\003\000\078\000\009\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\003\000\085\000\010\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\012\000\003\000\089\000\000\000\
\\000\000\
\\000\000\
\\012\000\002\000\013\000\091\000\000\000\
\\001\000\012\000\003\000\092\000\000\000\
\\000\000\
\\000\000\
\\005\000\094\000\006\000\093\000\000\000\
\\000\000\
\\001\000\012\000\003\000\096\000\000\000\
\\001\000\012\000\003\000\097\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\094\000\006\000\100\000\000\000\
\\007\000\101\000\000\000\
\\001\000\012\000\003\000\104\000\005\000\103\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\002\000\013\000\106\000\000\000\
\\000\000\
\\000\000\
\\004\000\107\000\000\000\
\\005\000\109\000\000\000\
\\005\000\094\000\006\000\110\000\000\000\
\\000\000\
\\001\000\012\000\003\000\112\000\005\000\111\000\000\000\
\\007\000\114\000\008\000\113\000\000\000\
\\000\000\
\\000\000\
\\007\000\116\000\000\000\
\\005\000\117\000\000\000\
\\001\000\012\000\003\000\118\000\000\000\
\\011\000\119\000\000\000\
\\000\000\
\\004\000\121\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\122\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\123\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 125
val numrules = 60
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = Pos.pos
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | BOOL of  (bool)
 | INT of  (int) | STRING of  (string) | ID of  (string)
 | start of  (E.ents) | ents of  (E.ents) | ent of  (E.ent)
 | cc of  (E.ents) | ue of  (E.ue) | ie of  (E.ie) | asc' of  (E.asc')
 | asc of  (E.asc) | units' of  (E.units') | units of  (E.units)
 | unit_list of  (E.units) | exp of  (E.exp) | dexp of  (E.exp)
 | aexp of  (E.exp)
end
type svalue = MlyValue.svalue
type result = E.ents
end
structure EC=
struct
open LrTable
val is_keyword =
fn (T 16) => true | (T 17) => true | (T 18) => true | (T 19) => true
 | (T 20) => true | (T 21) => true | (T 22) => true | (T 23) => true
 | (T 24) => true | (T 25) => true | (T 26) => true | (T 27) => true
 | (T 28) => true | (T 29) => true | (T 30) => true | (T 31) => true
 | (T 32) => true | (T 33) => true | (T 34) => true | (T 35) => true
 | _ => false
val preferred_change = 
(nil
,(T 18) :: nil
)::
(nil
,(T 19) :: nil
)::
(nil
,(T 19) :: (T 36) :: nil
)::
(nil
,(T 3) :: nil
)::
(nil
,(T 5) :: (T 6) :: nil
)::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "DOLLAR"
  | (T 2) => "CARAT"
  | (T 3) => "LPAREN"
  | (T 4) => "RPAREN"
  | (T 5) => "LBRACE"
  | (T 6) => "RBRACE"
  | (T 7) => "EQ"
  | (T 8) => "SEQ"
  | (T 9) => "BEQ"
  | (T 10) => "LT"
  | (T 11) => "LE"
  | (T 12) => "GT"
  | (T 13) => "GE"
  | (T 14) => "COLON"
  | (T 15) => "COLONCOLON"
  | (T 16) => "ENV"
  | (T 17) => "IF"
  | (T 18) => "THEN"
  | (T 19) => "ELSE"
  | (T 20) => "NOT"
  | (T 21) => "ANDALSO"
  | (T 22) => "ORELSE"
  | (T 23) => "DEFINED"
  | (T 24) => "UNIT"
  | (T 25) => "INTERFACE"
  | (T 26) => "COMPILED"
  | (T 27) => "PRIMITIVE"
  | (T 28) => "VAL"
  | (T 29) => "INCLUDE"
  | (T 30) => "LOCAL"
  | (T 31) => "IF'"
  | (T 32) => "ELIF"
  | (T 33) => "ELSE'"
  | (T 34) => "ENDIF"
  | (T 35) => "ERROR"
  | (T 36) => "ID"
  | (T 37) => "STRING"
  | (T 38) => "INT"
  | (T 39) => "BOOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 36) => MlyValue.ID(("bogus_id")) | 
(T 37) => MlyValue.STRING(("bogus_string")) | 
(T 38) => MlyValue.INT((~1)) | 
(T 39) => MlyValue.BOOL((false)) | 
_ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13)
 :: (T 14) :: (T 15) :: (T 16) :: (T 17) :: (T 18) :: (T 19) :: (T 20)
 :: (T 21) :: (T 22) :: (T 23) :: (T 24) :: (T 25) :: (T 26) :: (T 27)
 :: (T 28) :: (T 29) :: (T 30) :: (T 31) :: (T 32) :: (T 33) :: (T 34)
 :: (T 35) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.ID ID,IDleft,ID1right))::(_,(_,DOLLAR1left,_))::
rest671) => let val result=MlyValue.aexp((
E.EXP_MARK(IDleft,E.EXP_VAR (var_label ID))))
 in (LrTable.NT 0,(result,DOLLAR1left,ID1right),rest671) end
| (1,(_,(MlyValue.ID ID,IDleft,ID1right))::(_,(_,ENV1left,_))::rest671
) => let val result=MlyValue.aexp((
E.EXP_MARK(IDleft,E.EXP_ENV (env_label ID))))
 in (LrTable.NT 0,(result,ENV1left,ID1right),rest671) end
| (2,(_,(MlyValue.STRING STRING,STRING1left,STRING1right))::rest671)
 => let val result=MlyValue.aexp((E.EXP_STR STRING))
 in (LrTable.NT 0,(result,STRING1left,STRING1right),rest671) end
| (3,(_,(MlyValue.INT INT,INT1left,INT1right))::rest671) => let val 
result=MlyValue.aexp((E.EXP_INT INT))
 in (LrTable.NT 0,(result,INT1left,INT1right),rest671) end
| (4,(_,(MlyValue.BOOL BOOL,BOOL1left,BOOL1right))::rest671) => let 
val result=MlyValue.aexp((E.EXP_BOOL BOOL))
 in (LrTable.NT 0,(result,BOOL1left,BOOL1right),rest671) end
| (5,(_,(_,_,RPAREN1right))::(_,(MlyValue.exp exp,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.aexp((exp))
 in (LrTable.NT 0,(result,LPAREN1left,RPAREN1right),rest671) end
| (6,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,UNIT1left,_))::rest671)
 => let val result=MlyValue.dexp((E.EXP_DEF (unit_label ID)))
 in (LrTable.NT 1,(result,UNIT1left,ID1right),rest671) end
| (7,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,INTERFACE1left,_))::
rest671) => let val result=MlyValue.dexp((
E.EXP_DEF (interface_label ID)))
 in (LrTable.NT 1,(result,INTERFACE1left,ID1right),rest671) end
| (8,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,DOLLAR1left,_))::rest671)
 => let val result=MlyValue.dexp((E.EXP_DEF (var_label ID)))
 in (LrTable.NT 1,(result,DOLLAR1left,ID1right),rest671) end
| (9,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,ENV1left,_))::rest671) => 
let val result=MlyValue.dexp((E.EXP_DEF (env_label ID)))
 in (LrTable.NT 1,(result,ENV1left,ID1right),rest671) end
| (10,(_,(_,_,RPAREN1right))::(_,(MlyValue.dexp dexp,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.dexp((dexp))
 in (LrTable.NT 1,(result,LPAREN1left,RPAREN1right),rest671) end
| (11,(_,(MlyValue.aexp aexp,aexp1left,aexp1right))::rest671) => let 
val result=MlyValue.exp((aexp))
 in (LrTable.NT 2,(result,aexp1left,aexp1right),rest671) end
| (12,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,CARATleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(CARATleft,E.EXP_CAT(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (13,(_,(MlyValue.exp exp3,_,exp3right))::_::(_,(MlyValue.exp exp2,_,
_))::_::(_,(MlyValue.exp exp1,_,_))::(_,(_,IFleft as IF1left,_))::
rest671) => let val result=MlyValue.exp((
E.EXP_MARK(IFleft,E.EXP_IF(exp1,exp2,exp3))))
 in (LrTable.NT 2,(result,IF1left,exp3right),rest671) end
| (14,(_,(MlyValue.aexp aexp,_,aexp1right))::(_,(_,NOTleft as NOT1left
,_))::rest671) => let val result=MlyValue.exp((
E.EXP_MARK(NOTleft,E.EXP_NOT aexp)))
 in (LrTable.NT 2,(result,NOT1left,aexp1right),rest671) end
| (15,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,ANDALSOleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(ANDALSOleft,E.EXP_AND(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (16,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,ORELSEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(ORELSEleft,E.EXP_OR(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (17,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,SEQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(SEQleft,E.EXP_SEQ(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (18,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,BEQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(BEQleft,E.EXP_BEQ(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (19,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,EQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(EQleft,E.EXP_IEQ(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (20,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,LTleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(LTleft,E.EXP_ILT(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (21,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,LEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(LEleft,E.EXP_ILE(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (22,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,GTleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(GTleft,E.EXP_IGT(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (23,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,GEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((E.EXP_MARK(GEleft,E.EXP_IGE(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (24,(_,(MlyValue.dexp dexp,_,dexp1right))::(_,(_,DEFINED1left,_))::
rest671) => let val result=MlyValue.exp((dexp))
 in (LrTable.NT 2,(result,DEFINED1left,dexp1right),rest671) end
| (25,rest671) => let val result=MlyValue.unit_list((nil))
 in (LrTable.NT 3,(result,defaultPos,defaultPos),rest671) end
| (26,(_,(MlyValue.unit_list unit_list,_,unit_list1right))::(_,(
MlyValue.ID ID,ID1left,_))::rest671) => let val result=
MlyValue.unit_list(((unit_label ID) :: unit_list))
 in (LrTable.NT 3,(result,ID1left,unit_list1right),rest671) end
| (27,(_,(_,_,RBRACE1right))::(_,(MlyValue.unit_list unit_list,_,_))::
(_,(_,LBRACE1left,_))::rest671) => let val result=MlyValue.units((
unit_list))
 in (LrTable.NT 4,(result,LBRACE1left,RBRACE1right),rest671) end
| (28,rest671) => let val result=MlyValue.units'((NONE))
 in (LrTable.NT 5,(result,defaultPos,defaultPos),rest671) end
| (29,(_,(MlyValue.units units,units1left,units1right))::rest671) => 
let val result=MlyValue.units'((SOME units))
 in (LrTable.NT 5,(result,units1left,units1right),rest671) end
| (30,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,COLON1left,_))::rest671)
 => let val result=MlyValue.asc((interface_label ID))
 in (LrTable.NT 6,(result,COLON1left,ID1right),rest671) end
| (31,rest671) => let val result=MlyValue.asc'((NONE))
 in (LrTable.NT 7,(result,defaultPos,defaultPos),rest671) end
| (32,(_,(MlyValue.asc asc,asc1left,asc1right))::rest671) => let val 
result=MlyValue.asc'((SOME asc))
 in (LrTable.NT 7,(result,asc1left,asc1right),rest671) end
| (33,(_,(MlyValue.units' units',_,units'1right))::(_,(MlyValue.exp 
exp,exp1left,_))::rest671) => let val result=MlyValue.ie((
E.SRCI(exp,units')))
 in (LrTable.NT 8,(result,exp1left,units'1right),rest671) end
| (34,(_,(_,PRIMITIVE1left,PRIMITIVE1right))::rest671) => let val 
result=MlyValue.ie((E.PRIMI))
 in (LrTable.NT 8,(result,PRIMITIVE1left,PRIMITIVE1right),rest671) end
| (35,(_,(MlyValue.units units,_,units1right))::(_,(MlyValue.exp exp,_
,_))::(_,(_,COMPILED1left,_))::rest671) => let val result=MlyValue.ie(
(E.PRECOMPI(exp,units)))
 in (LrTable.NT 8,(result,COMPILED1left,units1right),rest671) end
| (36,(_,(_,COMPILED1left,COMPILED1right))::rest671) => let val result
=MlyValue.ie((E.COMPI))
 in (LrTable.NT 8,(result,COMPILED1left,COMPILED1right),rest671) end
| (37,(_,(MlyValue.asc' asc',_,asc'1right))::(_,(MlyValue.units' 
units',_,_))::(_,(MlyValue.exp exp,exp1left,_))::rest671) => let val 
result=MlyValue.ue((E.SRCU(exp,units',asc')))
 in (LrTable.NT 9,(result,exp1left,asc'1right),rest671) end
| (38,(_,(MlyValue.asc asc,_,asc1right))::(_,(_,PRIMITIVE1left,_))::
rest671) => let val result=MlyValue.ue((E.PRIMU asc))
 in (LrTable.NT 9,(result,PRIMITIVE1left,asc1right),rest671) end
| (39,(_,(MlyValue.asc asc,_,asc1right))::(_,(MlyValue.units units,_,_
))::(_,(MlyValue.exp exp,_,_))::(_,(_,COMPILED1left,_))::rest671) => 
let val result=MlyValue.ue((E.PRECOMPU(exp,units,asc)))
 in (LrTable.NT 9,(result,COMPILED1left,asc1right),rest671) end
| (40,(_,(MlyValue.asc asc,_,asc1right))::(_,(MlyValue.units units,_,_
))::(_,(_,COMPILED1left,_))::rest671) => let val result=MlyValue.ue((
E.COMPU (units,asc)))
 in (LrTable.NT 9,(result,COMPILED1left,asc1right),rest671) end
| (41,rest671) => let val result=MlyValue.cc((nil))
 in (LrTable.NT 10,(result,defaultPos,defaultPos),rest671) end
| (42,(_,(MlyValue.ents ents,_,ents1right))::(_,(_,ELSE'1left,_))::
rest671) => let val result=MlyValue.cc((ents))
 in (LrTable.NT 10,(result,ELSE'1left,ents1right),rest671) end
| (43,(_,(MlyValue.cc cc,_,cc1right))::(_,(MlyValue.ents ents,_,_))::(
_,(MlyValue.exp exp,_,_))::(_,(_,ELIFleft as ELIF1left,_))::rest671)
 => let val result=MlyValue.cc(([E.MARK(ELIFleft,E.IF(exp,ents,cc))]))
 in (LrTable.NT 10,(result,ELIF1left,cc1right),rest671) end
| (44,(_,(MlyValue.ie ie,_,ie1right))::_::(_,(MlyValue.ID ID,_,_))::(_
,(_,INTERFACE1left,_))::rest671) => let val result=MlyValue.ent((
E.INTERFACE(interface_label ID,ie)))
 in (LrTable.NT 11,(result,INTERFACE1left,ie1right),rest671) end
| (45,(_,(MlyValue.ID ID2,_,ID2right))::_::(_,(MlyValue.ID ID1,_,_))::
(_,(_,UNIT1left,_))::rest671) => let val result=MlyValue.ent((
E.SC(unit_label ID1, interface_label ID2, true)))
 in (LrTable.NT 11,(result,UNIT1left,ID2right),rest671) end
| (46,(_,(MlyValue.ID ID2,_,ID2right))::_::(_,(MlyValue.ID ID1,_,_))::
(_,(_,UNIT1left,_))::rest671) => let val result=MlyValue.ent((
E.SC(unit_label ID1, interface_label ID2, false)))
 in (LrTable.NT 11,(result,UNIT1left,ID2right),rest671) end
| (47,(_,(MlyValue.ue ue,_,ue1right))::_::(_,(MlyValue.ID ID,_,_))::(_
,(_,UNIT1left,_))::rest671) => let val result=MlyValue.ent((
E.UNIT(unit_label ID, ue)))
 in (LrTable.NT 11,(result,UNIT1left,ue1right),rest671) end
| (48,(_,(MlyValue.units' units',_,units'1right))::(_,(MlyValue.exp 
exp,_,_))::_::(_,(MlyValue.asc asc,_,_))::(_,(MlyValue.ID ID,_,_))::(_
,(_,UNIT1left,_))::rest671) => let val result=MlyValue.ent((
E.UNIT(unit_label ID, E.SRCU(exp,units',SOME asc))))
 in (LrTable.NT 11,(result,UNIT1left,units'1right),rest671) end
| (49,(_,(_,_,PRIMITIVE1right))::_::(_,(MlyValue.asc asc,_,_))::(_,(
MlyValue.ID ID,_,_))::(_,(_,UNIT1left,_))::rest671) => let val result=
MlyValue.ent((E.UNIT(unit_label ID, E.PRIMU asc)))
 in (LrTable.NT 11,(result,UNIT1left,PRIMITIVE1right),rest671) end
| (50,(_,(MlyValue.units units,_,units1right))::(_,(MlyValue.exp exp,_
,_))::_::_::(_,(MlyValue.asc asc,_,_))::(_,(MlyValue.ID ID,_,_))::(_,(
_,UNIT1left,_))::rest671) => let val result=MlyValue.ent((
E.UNIT(unit_label ID, E.PRECOMPU(exp,units,asc))))
 in (LrTable.NT 11,(result,UNIT1left,units1right),rest671) end
| (51,(_,(MlyValue.units units,_,units1right))::_::_::(_,(MlyValue.asc
 asc,_,_))::(_,(MlyValue.ID ID,_,_))::(_,(_,UNIT1left,_))::rest671)
 => let val result=MlyValue.ent((
E.UNIT(unit_label ID, E.COMPU (units,asc))))
 in (LrTable.NT 11,(result,UNIT1left,units1right),rest671) end
| (52,(_,(MlyValue.exp exp,_,exp1right))::_::(_,(MlyValue.ID ID,_,_))
::(_,(_,VAL1left,_))::rest671) => let val result=MlyValue.ent((
E.VAL(var_label ID,exp)))
 in (LrTable.NT 11,(result,VAL1left,exp1right),rest671) end
| (53,(_,(MlyValue.exp exp,_,exp1right))::(_,(_,INCLUDE1left,_))::
rest671) => let val result=MlyValue.ent((E.INCLUDE exp))
 in (LrTable.NT 11,(result,INCLUDE1left,exp1right),rest671) end
| (54,(_,(MlyValue.exp exp,_,exp1right))::(_,(_,LOCAL1left,_))::
rest671) => let val result=MlyValue.ent((E.LOCAL exp))
 in (LrTable.NT 11,(result,LOCAL1left,exp1right),rest671) end
| (55,(_,(_,_,ENDIF1right))::(_,(MlyValue.cc cc,_,_))::(_,(
MlyValue.ents ents,_,_))::(_,(MlyValue.exp exp,_,_))::(_,(_,IF'left
 as IF'1left,_))::rest671) => let val result=MlyValue.ent((
E.MARK(IF'left,E.IF(exp,ents,cc))))
 in (LrTable.NT 11,(result,IF'1left,ENDIF1right),rest671) end
| (56,(_,(MlyValue.exp exp,_,exp1right))::(_,(_,ERRORleft as 
ERROR1left,_))::rest671) => let val result=MlyValue.ent((
E.MARK(ERRORleft,E.ERROR exp)))
 in (LrTable.NT 11,(result,ERROR1left,exp1right),rest671) end
| (57,rest671) => let val result=MlyValue.ents((nil))
 in (LrTable.NT 12,(result,defaultPos,defaultPos),rest671) end
| (58,(_,(MlyValue.ents ents,_,ents1right))::(_,(MlyValue.ent ent,
entleft as ent1left,_))::rest671) => let val result=MlyValue.ents((
E.MARK(entleft,ent) :: ents))
 in (LrTable.NT 12,(result,ent1left,ents1right),rest671) end
| (59,(_,(MlyValue.ents ents,ents1left,ents1right))::rest671) => let 
val result=MlyValue.start((ents))
 in (LrTable.NT 13,(result,ents1left,ents1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Project_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DOLLAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun CARAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun SEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun BEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun COLONCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ENV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun ANDALSO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun ORELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun DEFINED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun UNIT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun INTERFACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun COMPILED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun PRIMITIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun INCLUDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun LOCAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun IF' (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun ELIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE' (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun ERROR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.ID i,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.STRING i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.INT i,p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.BOOL i,p1,p2))
end
end
