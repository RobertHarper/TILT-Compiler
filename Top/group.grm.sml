functor GroupLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Group_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Parser for group files. *)

structure S = ExtSyn

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\025\000\005\000\024\000\017\000\023\000\018\000\022\000\
\\021\000\021\000\024\000\020\000\044\000\019\000\045\000\018\000\
\\046\000\017\000\000\000\
\\001\000\002\000\025\000\005\000\024\000\017\000\023\000\044\000\019\000\
\\045\000\018\000\046\000\017\000\000\000\
\\001\000\002\000\053\000\005\000\052\000\017\000\051\000\025\000\050\000\
\\030\000\049\000\000\000\
\\001\000\004\000\047\000\006\000\087\000\009\000\046\000\010\000\045\000\
\\011\000\044\000\012\000\043\000\013\000\042\000\014\000\041\000\
\\015\000\040\000\022\000\039\000\023\000\038\000\000\000\
\\001\000\004\000\047\000\007\000\094\000\009\000\046\000\010\000\045\000\
\\011\000\044\000\012\000\043\000\013\000\042\000\014\000\041\000\
\\015\000\040\000\022\000\039\000\023\000\038\000\000\000\
\\001\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\019\000\086\000\022\000\039\000\023\000\038\000\000\000\
\\001\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\020\000\120\000\022\000\039\000\023\000\038\000\000\000\
\\001\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\029\000\129\000\000\000\
\\001\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\029\000\138\000\000\000\
\\001\000\006\000\103\000\000\000\
\\001\000\007\000\094\000\000\000\
\\001\000\008\000\122\000\000\000\
\\001\000\008\000\126\000\000\000\
\\001\000\009\000\070\000\000\000\
\\001\000\009\000\098\000\000\000\
\\001\000\009\000\100\000\016\000\099\000\000\000\
\\001\000\009\000\101\000\000\000\
\\001\000\009\000\128\000\000\000\
\\001\000\009\000\130\000\000\000\
\\001\000\016\000\095\000\000\000\
\\001\000\016\000\097\000\000\000\
\\001\000\025\000\034\000\030\000\033\000\000\000\
\\001\000\025\000\036\000\030\000\035\000\000\000\
\\001\000\030\000\031\000\033\000\030\000\000\000\
\\001\000\030\000\032\000\000\000\
\\001\000\033\000\029\000\000\000\
\\001\000\036\000\028\000\037\000\027\000\000\000\
\\001\000\041\000\105\000\000\000\
\\001\000\043\000\037\000\000\000\
\\001\000\043\000\056\000\000\000\
\\001\000\043\000\058\000\000\000\
\\001\000\043\000\064\000\000\000\
\\001\000\043\000\065\000\000\000\
\\001\000\043\000\066\000\000\000\
\\001\000\043\000\067\000\000\000\
\\001\000\043\000\068\000\000\000\
\\001\000\043\000\069\000\000\000\
\\001\000\043\000\081\000\000\000\
\\001\000\043\000\082\000\000\000\
\\001\000\043\000\083\000\000\000\
\\001\000\043\000\085\000\000\000\
\\001\000\043\000\114\000\000\000\
\\001\000\043\000\115\000\000\000\
\\001\000\043\000\117\000\000\000\
\\001\000\043\000\124\000\000\000\
\\001\000\043\000\125\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\000\000\
\\156\000\000\000\
\\157\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\000\000\
\\158\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\000\000\
\\159\000\004\000\047\000\000\000\
\\160\000\004\000\047\000\000\000\
\\161\000\004\000\047\000\000\000\
\\162\000\004\000\047\000\000\000\
\\163\000\004\000\047\000\000\000\
\\164\000\004\000\047\000\000\000\
\\165\000\004\000\047\000\000\000\
\\166\000\000\000\
\\167\000\043\000\113\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\025\000\111\000\026\000\110\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\\175\000\039\000\090\000\040\000\089\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\000\000\
\\186\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\000\000\
\\187\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\000\000\
\\188\000\000\000\
\\189\000\004\000\047\000\007\000\092\000\009\000\046\000\010\000\045\000\
\\011\000\044\000\012\000\043\000\013\000\042\000\014\000\041\000\
\\015\000\040\000\022\000\039\000\023\000\038\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\000\000\
\\193\000\004\000\047\000\009\000\046\000\010\000\045\000\011\000\044\000\
\\012\000\043\000\013\000\042\000\014\000\041\000\015\000\040\000\
\\022\000\039\000\023\000\038\000\026\000\013\000\027\000\012\000\
\\028\000\011\000\031\000\010\000\032\000\009\000\034\000\008\000\
\\035\000\007\000\038\000\006\000\042\000\005\000\000\000\
\\193\000\026\000\013\000\027\000\012\000\028\000\011\000\031\000\010\000\
\\032\000\009\000\034\000\008\000\035\000\007\000\038\000\006\000\
\\042\000\005\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\"
val actionRowNumbers =
"\099\000\101\000\099\000\001\000\
\\001\000\027\000\026\000\024\000\
\\025\000\022\000\023\000\029\000\
\\100\000\097\000\058\000\051\000\
\\050\000\048\000\003\000\002\000\
\\001\000\030\000\001\000\031\000\
\\098\000\001\000\001\000\001\000\
\\001\000\032\000\033\000\034\000\
\\035\000\036\000\037\000\014\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\071\000\038\000\
\\039\000\040\000\003\000\041\000\
\\061\000\006\000\049\000\004\000\
\\047\000\080\000\094\000\005\000\
\\090\000\091\000\020\000\011\000\
\\021\000\015\000\016\000\017\000\
\\001\000\063\000\062\000\070\000\
\\069\000\068\000\067\000\065\000\
\\064\000\066\000\059\000\055\000\
\\056\000\054\000\010\000\053\000\
\\001\000\052\000\028\000\099\000\
\\001\000\095\000\077\000\093\000\
\\072\000\042\000\088\000\043\000\
\\001\000\044\000\001\000\001\000\
\\092\000\057\000\007\000\096\000\
\\081\000\098\000\012\000\077\000\
\\045\000\046\000\013\000\072\000\
\\089\000\018\000\008\000\019\000\
\\005\000\005\000\001\000\080\000\
\\079\000\078\000\076\000\075\000\
\\074\000\073\000\001\000\001\000\
\\001\000\085\000\083\000\060\000\
\\082\000\009\000\084\000\005\000\
\\001\000\086\000\087\000\000\000"
val gotoT =
"\
\\010\000\002\000\011\000\001\000\012\000\139\000\000\000\
\\000\000\
\\010\000\002\000\011\000\012\000\000\000\
\\001\000\014\000\003\000\013\000\000\000\
\\001\000\014\000\003\000\024\000\000\000\
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
\\002\000\046\000\000\000\
\\001\000\052\000\000\000\
\\001\000\014\000\003\000\053\000\000\000\
\\000\000\
\\001\000\014\000\003\000\055\000\000\000\
\\000\000\
\\010\000\002\000\011\000\057\000\000\000\
\\001\000\014\000\003\000\058\000\000\000\
\\001\000\014\000\003\000\059\000\000\000\
\\001\000\014\000\003\000\060\000\000\000\
\\001\000\014\000\003\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\003\000\069\000\000\000\
\\001\000\014\000\003\000\070\000\000\000\
\\001\000\014\000\003\000\071\000\000\000\
\\001\000\014\000\003\000\072\000\000\000\
\\001\000\014\000\003\000\073\000\000\000\
\\001\000\014\000\003\000\074\000\000\000\
\\001\000\014\000\003\000\075\000\000\000\
\\001\000\014\000\003\000\076\000\000\000\
\\001\000\014\000\003\000\077\000\000\000\
\\001\000\014\000\003\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\082\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\086\000\000\000\
\\008\000\089\000\000\000\
\\005\000\091\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\003\000\100\000\000\000\
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
\\001\000\014\000\003\000\102\000\000\000\
\\000\000\
\\000\000\
\\010\000\002\000\011\000\104\000\000\000\
\\001\000\014\000\003\000\105\000\000\000\
\\000\000\
\\006\000\107\000\007\000\106\000\000\000\
\\000\000\
\\004\000\110\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\003\000\114\000\000\000\
\\000\000\
\\001\000\014\000\003\000\116\000\000\000\
\\001\000\014\000\003\000\117\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\002\000\011\000\119\000\000\000\
\\000\000\
\\006\000\107\000\007\000\121\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\125\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\129\000\000\000\
\\005\000\130\000\000\000\
\\001\000\014\000\003\000\131\000\000\000\
\\009\000\132\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\014\000\003\000\133\000\000\000\
\\001\000\014\000\003\000\134\000\000\000\
\\001\000\014\000\003\000\135\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\137\000\000\000\
\\001\000\014\000\003\000\138\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 140
val numrules = 54
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
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | BOOL of  (bool)
 | INT of  (int) | STRING of  (string) | ID of  (string)
 | start of  (S.groupfile) | ents of  (S.entries) | ent of  (S.entry)
 | cc of  (S.entries) | exports of  (S.exports) | exps of  (S.exports)
 | export of  (S.export) | imports of  (string list)
 | imps of  (string list) | exp of  (S.exp) | dexp of  (S.exp)
 | aexp of  (S.exp)
end
type svalue = MlyValue.svalue
type result = S.groupfile
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
 | (T 36) => true | (T 38) => true | (T 40) => true | (T 41) => true
 | _ => false
val preferred_change = 
(nil
,(T 18) :: nil
)::
(nil
,(T 19) :: nil
)::
(nil
,(T 4) :: nil
)::
(nil
,(T 19) :: (T 42) :: nil
)::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "DOLLAR"
  | (T 2) => "DOT"
  | (T 3) => "CARAT"
  | (T 4) => "LPAREN"
  | (T 5) => "RPAREN"
  | (T 6) => "LBRACE"
  | (T 7) => "RBRACE"
  | (T 8) => "EQ"
  | (T 9) => "SEQ"
  | (T 10) => "BEQ"
  | (T 11) => "LT"
  | (T 12) => "LE"
  | (T 13) => "GT"
  | (T 14) => "GE"
  | (T 15) => "COLON"
  | (T 16) => "ENV"
  | (T 17) => "IF"
  | (T 18) => "THEN"
  | (T 19) => "ELSE"
  | (T 20) => "NOT"
  | (T 21) => "ANDALSO"
  | (T 22) => "ORELSE"
  | (T 23) => "DEFINED"
  | (T 24) => "INTERFACE"
  | (T 25) => "VAL"
  | (T 26) => "SOURCE"
  | (T 27) => "COMPILED"
  | (T 28) => "AND"
  | (T 29) => "UNIT"
  | (T 30) => "PRIMITIVE"
  | (T 31) => "IMPORT"
  | (T 32) => "GROUP"
  | (T 33) => "INCLUDE"
  | (T 34) => "MAKE"
  | (T 35) => "EXECUTABLE"
  | (T 36) => "LIBRARY"
  | (T 37) => "IF'"
  | (T 38) => "ELIF"
  | (T 39) => "ELSE'"
  | (T 40) => "ENDIF"
  | (T 41) => "ERROR"
  | (T 42) => "ID"
  | (T 43) => "STRING"
  | (T 44) => "INT"
  | (T 45) => "BOOL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 42) => MlyValue.ID(("bogusId")) | 
(T 43) => MlyValue.STRING(("bogusString")) | 
(T 44) => MlyValue.INT((~1)) | 
(T 45) => MlyValue.BOOL((false)) | 
_ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13)
 :: (T 14) :: (T 15) :: (T 16) :: (T 17) :: (T 18) :: (T 19) :: (T 20)
 :: (T 21) :: (T 22) :: (T 23) :: (T 24) :: (T 25) :: (T 26) :: (T 27)
 :: (T 28) :: (T 29) :: (T 30) :: (T 31) :: (T 32) :: (T 33) :: (T 34)
 :: (T 35) :: (T 36) :: (T 37) :: (T 38) :: (T 39) :: (T 40) :: (T 41)
 :: nil
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
S.EXP_MARK(IDleft,S.EXP_VAR ID)))
 in (LrTable.NT 0,(result,DOLLAR1left,ID1right),rest671) end
| (1,(_,(MlyValue.STRING STRING,STRING1left,STRING1right))::rest671)
 => let val result=MlyValue.aexp((S.EXP_STR STRING))
 in (LrTable.NT 0,(result,STRING1left,STRING1right),rest671) end
| (2,(_,(MlyValue.ID ID,IDleft,ID1right))::(_,(_,ENV1left,_))::rest671
) => let val result=MlyValue.aexp((S.EXP_MARK(IDleft,S.EXP_ENV ID)))
 in (LrTable.NT 0,(result,ENV1left,ID1right),rest671) end
| (3,(_,(MlyValue.INT INT,INT1left,INT1right))::rest671) => let val 
result=MlyValue.aexp((S.EXP_INT INT))
 in (LrTable.NT 0,(result,INT1left,INT1right),rest671) end
| (4,(_,(MlyValue.BOOL BOOL,BOOL1left,BOOL1right))::rest671) => let 
val result=MlyValue.aexp((S.EXP_BOOL BOOL))
 in (LrTable.NT 0,(result,BOOL1left,BOOL1right),rest671) end
| (5,(_,(_,_,RPAREN1right))::(_,(MlyValue.exp exp,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.aexp((exp))
 in (LrTable.NT 0,(result,LPAREN1left,RPAREN1right),rest671) end
| (6,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,DOLLAR1left,_))::rest671)
 => let val result=MlyValue.dexp((S.EXP_DEFV ID))
 in (LrTable.NT 1,(result,DOLLAR1left,ID1right),rest671) end
| (7,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,ENV1left,_))::rest671) => 
let val result=MlyValue.dexp((S.EXP_DEFE ID))
 in (LrTable.NT 1,(result,ENV1left,ID1right),rest671) end
| (8,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,UNIT1left,_))::rest671)
 => let val result=MlyValue.dexp((S.EXP_DEFU ID))
 in (LrTable.NT 1,(result,UNIT1left,ID1right),rest671) end
| (9,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,INTERFACE1left,_))::
rest671) => let val result=MlyValue.dexp((S.EXP_DEFI ID))
 in (LrTable.NT 1,(result,INTERFACE1left,ID1right),rest671) end
| (10,(_,(_,_,RPAREN1right))::(_,(MlyValue.dexp dexp,_,_))::(_,(_,
LPAREN1left,_))::rest671) => let val result=MlyValue.dexp((dexp))
 in (LrTable.NT 1,(result,LPAREN1left,RPAREN1right),rest671) end
| (11,(_,(MlyValue.aexp aexp,aexp1left,aexp1right))::rest671) => let 
val result=MlyValue.exp((aexp))
 in (LrTable.NT 2,(result,aexp1left,aexp1right),rest671) end
| (12,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,CARATleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(CARATleft,S.EXP_CAT(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (13,(_,(MlyValue.exp exp3,_,exp3right))::_::(_,(MlyValue.exp exp2,_,
_))::_::(_,(MlyValue.exp exp1,_,_))::(_,(_,IFleft as IF1left,_))::
rest671) => let val result=MlyValue.exp((
S.EXP_MARK(IFleft,S.EXP_IF(exp1,exp2,exp3))))
 in (LrTable.NT 2,(result,IF1left,exp3right),rest671) end
| (14,(_,(MlyValue.aexp aexp,_,aexp1right))::(_,(_,NOTleft as NOT1left
,_))::rest671) => let val result=MlyValue.exp((
S.EXP_MARK(NOTleft,S.EXP_NOT aexp)))
 in (LrTable.NT 2,(result,NOT1left,aexp1right),rest671) end
| (15,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,ANDALSOleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(ANDALSOleft,S.EXP_AND(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (16,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,ORELSEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(ORELSEleft,S.EXP_OR(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (17,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,SEQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(SEQleft,S.EXP_SEQ(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (18,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,BEQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(BEQleft,S.EXP_BEQ(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (19,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,EQleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(EQleft,S.EXP_IEQ(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (20,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,LTleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(LTleft,S.EXP_ILT(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (21,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,LEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(LEleft,S.EXP_ILE(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (22,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,GTleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(GTleft,S.EXP_IGT(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (23,(_,(MlyValue.exp exp2,_,exp2right))::(_,(_,GEleft,_))::(_,(
MlyValue.exp exp1,exp1left,_))::rest671) => let val result=
MlyValue.exp((S.EXP_MARK(GEleft,S.EXP_IGE(exp1,exp2))))
 in (LrTable.NT 2,(result,exp1left,exp2right),rest671) end
| (24,(_,(MlyValue.dexp dexp,_,dexp1right))::(_,(_,DEFINED1left,_))::
rest671) => let val result=MlyValue.exp((dexp))
 in (LrTable.NT 2,(result,DEFINED1left,dexp1right),rest671) end
| (25,rest671) => let val result=MlyValue.imps((nil))
 in (LrTable.NT 3,(result,defaultPos,defaultPos),rest671) end
| (26,(_,(MlyValue.imps imps,_,imps1right))::(_,(MlyValue.ID ID,
ID1left,_))::rest671) => let val result=MlyValue.imps((ID :: imps))
 in (LrTable.NT 3,(result,ID1left,imps1right),rest671) end
| (27,(_,(_,_,RBRACE1right))::(_,(MlyValue.imps imps,_,_))::(_,(_,
LBRACE1left,_))::rest671) => let val result=MlyValue.imports((imps))
 in (LrTable.NT 4,(result,LBRACE1left,RBRACE1right),rest671) end
| (28,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,INTERFACE1left,_))::
rest671) => let val result=MlyValue.export((S.EXPORTI ID))
 in (LrTable.NT 5,(result,INTERFACE1left,ID1right),rest671) end
| (29,(_,(MlyValue.ID ID,_,ID1right))::(_,(_,VAL1left,_))::rest671)
 => let val result=MlyValue.export((S.EXPORTV ID))
 in (LrTable.NT 5,(result,VAL1left,ID1right),rest671) end
| (30,rest671) => let val result=MlyValue.exps((nil))
 in (LrTable.NT 6,(result,defaultPos,defaultPos),rest671) end
| (31,(_,(MlyValue.exps exps,_,exps1right))::(_,(MlyValue.export 
export,export1left,_))::rest671) => let val result=MlyValue.exps((
export :: exps))
 in (LrTable.NT 6,(result,export1left,exps1right),rest671) end
| (32,(_,(_,_,RBRACE1right))::(_,(MlyValue.exps exps,_,_))::(_,(_,
LBRACE1left,_))::rest671) => let val result=MlyValue.exports((exps))
 in (LrTable.NT 7,(result,LBRACE1left,RBRACE1right),rest671) end
| (33,rest671) => let val result=MlyValue.cc((nil))
 in (LrTable.NT 8,(result,defaultPos,defaultPos),rest671) end
| (34,(_,(MlyValue.ents ents,_,ents1right))::(_,(_,ELSE'1left,_))::
rest671) => let val result=MlyValue.cc((ents))
 in (LrTable.NT 8,(result,ELSE'1left,ents1right),rest671) end
| (35,(_,(MlyValue.cc cc,_,cc1right))::(_,(MlyValue.ents ents,_,_))::(
_,(MlyValue.exp exp,_,_))::(_,(_,ELIFleft as ELIF1left,_))::rest671)
 => let val result=MlyValue.cc(([S.MARK(ELIFleft,S.IF(exp,ents,cc))]))
 in (LrTable.NT 8,(result,ELIF1left,cc1right),rest671) end
| (36,(_,(MlyValue.imports imports,_,imports1right))::(_,(MlyValue.exp
 exp,_,_))::_::(_,(MlyValue.ID ID,_,_))::_::(_,(_,SOURCE1left,_))::
rest671) => let val result=MlyValue.ent((S.SRCI(ID,exp,imports)))
 in (LrTable.NT 9,(result,SOURCE1left,imports1right),rest671) end
| (37,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,_,
_))::_::(_,(MlyValue.ID ID,_,_))::_::(_,(_,COMPILED1left,_))::rest671)
 => let val result=MlyValue.ent((S.COMPI(ID,exp1,exp2)))
 in (LrTable.NT 9,(result,COMPILED1left,exp2right),rest671) end
| (38,(_,(MlyValue.imports imports,_,imports1right))::(_,(MlyValue.exp
 exp,_,_))::_::(_,(MlyValue.ID ID,_,_))::_::(_,(_,SOURCE1left,_))::
rest671) => let val result=MlyValue.ent((S.SRCU(ID,NONE,exp,imports)))
 in (LrTable.NT 9,(result,SOURCE1left,imports1right),rest671) end
| (39,(_,(MlyValue.imports imports,_,imports1right))::(_,(MlyValue.exp
 exp,_,_))::_::(_,(MlyValue.ID ID2,_,_))::_::(_,(MlyValue.ID ID1,_,_))
::_::(_,(_,SOURCE1left,_))::rest671) => let val result=MlyValue.ent((
S.SRCU(ID1,SOME ID2,exp,imports)))
 in (LrTable.NT 9,(result,SOURCE1left,imports1right),rest671) end
| (40,(_,(MlyValue.exp exp2,_,exp2right))::_::(_,(MlyValue.exp exp1,_,
_))::_::(_,(MlyValue.ID ID2,_,_))::_::(_,(MlyValue.ID ID1,_,_))::_::(_
,(_,COMPILED1left,_))::rest671) => let val result=MlyValue.ent((
S.COMPU(ID1,ID2,exp1,exp2)))
 in (LrTable.NT 9,(result,COMPILED1left,exp2right),rest671) end
| (41,(_,(MlyValue.imports imports,_,imports1right))::(_,(MlyValue.ID 
ID,_,_))::_::(_,(_,PRIMITIVE1left,_))::rest671) => let val result=
MlyValue.ent((S.PRIMU(ID,imports)))
 in (LrTable.NT 9,(result,PRIMITIVE1left,imports1right),rest671) end
| (42,(_,(MlyValue.ID ID2,_,ID2right))::_::(_,(MlyValue.ID ID1,_,_))::
_::(_,(_,IMPORT1left,_))::rest671) => let val result=MlyValue.ent((
S.IMPORTU(ID1,ID2)))
 in (LrTable.NT 9,(result,IMPORT1left,ID2right),rest671) end
| (43,(_,(MlyValue.exp exp,_,exp1right))::_::(_,(_,INCLUDE1left,_))::
rest671) => let val result=MlyValue.ent((S.INCLUDE exp))
 in (LrTable.NT 9,(result,INCLUDE1left,exp1right),rest671) end
| (44,(_,(MlyValue.exp exp,_,exp1right))::_::(_,(_,IMPORT1left,_))::
rest671) => let val result=MlyValue.ent((S.IMPORT exp))
 in (LrTable.NT 9,(result,IMPORT1left,exp1right),rest671) end
| (45,(_,(MlyValue.exp exp,_,exp1right))::_::(_,(MlyValue.ID ID,_,_))
::(_,(_,VAL1left,_))::rest671) => let val result=MlyValue.ent((
S.VAL(ID,exp)))
 in (LrTable.NT 9,(result,VAL1left,exp1right),rest671) end
| (46,(_,(MlyValue.imports imports,_,imports1right))::(_,(MlyValue.exp
 exp,_,_))::_::(_,(_,MAKE1left,_))::rest671) => let val result=
MlyValue.ent((S.MAKE_EXE(exp,imports)))
 in (LrTable.NT 9,(result,MAKE1left,imports1right),rest671) end
| (47,(_,(MlyValue.exp exp,_,exp1right))::_::(_,(_,MAKE1left,_))::
rest671) => let val result=MlyValue.ent((S.MAKE_LIB(exp,nil)))
 in (LrTable.NT 9,(result,MAKE1left,exp1right),rest671) end
| (48,(_,(MlyValue.exports exports,_,exports1right))::(_,(MlyValue.exp
 exp,_,_))::_::(_,(_,MAKE1left,_))::rest671) => let val result=
MlyValue.ent((S.MAKE_LIB(exp,exports)))
 in (LrTable.NT 9,(result,MAKE1left,exports1right),rest671) end
| (49,(_,(_,_,ENDIF1right))::(_,(MlyValue.cc cc,_,_))::(_,(
MlyValue.ents ents,_,_))::(_,(MlyValue.exp exp,_,_))::(_,(_,IF'left
 as IF'1left,_))::rest671) => let val result=MlyValue.ent((
S.MARK(IF'left,S.IF(exp,ents,cc))))
 in (LrTable.NT 9,(result,IF'1left,ENDIF1right),rest671) end
| (50,(_,(MlyValue.exp exp,_,exp1right))::(_,(_,ERRORleft as 
ERROR1left,_))::rest671) => let val result=MlyValue.ent((
S.MARK(ERRORleft,S.ERROR exp)))
 in (LrTable.NT 9,(result,ERROR1left,exp1right),rest671) end
| (51,rest671) => let val result=MlyValue.ents((nil))
 in (LrTable.NT 10,(result,defaultPos,defaultPos),rest671) end
| (52,(_,(MlyValue.ents ents,_,ents1right))::(_,(MlyValue.ent ent,
entleft as ent1left,_))::rest671) => let val result=MlyValue.ents((
S.MARK(entleft,ent) :: ents))
 in (LrTable.NT 10,(result,ent1left,ents1right),rest671) end
| (53,(_,(MlyValue.ents ents,ents1left,ents1right))::rest671) => let 
val result=MlyValue.start((ents))
 in (LrTable.NT 11,(result,ents1left,ents1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Group_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DOLLAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun CARAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun BEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
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
fun INTERFACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun SOURCE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun COMPILED (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun UNIT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun PRIMITIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun GROUP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun INCLUDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun EXECUTABLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun LIBRARY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun IF' (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun ELIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE' (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun ERROR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.ID i,p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.STRING i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.INT i,p1,p2))
fun BOOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.BOOL i,p1,p2))
end
end
