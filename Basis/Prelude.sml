(* basic overloads - extended by TopLevel *)
overload + : 'a as TiltPrim.iplus  and TiltPrim.bplus  and TiltPrim.uplus  and TiltPrim.fplus
overload - : 'a as TiltPrim.iminus and TiltPrim.bminus and TiltPrim.uminus and TiltPrim.fminus
overload * : 'a as TiltPrim.imult  and TiltPrim.bmult  and TiltPrim.umult  and TiltPrim.fmult
overload / : 'a as TiltPrim.fdiv
overload ~ : 'a as TiltPrim.ineg and TiltPrim.fneg
overload <  : 'a as TiltPrim.ilt  and TiltPrim.blt  and TiltPrim.ult  and TiltPrim.flt
overload >  : 'a as TiltPrim.igt  and TiltPrim.bgt  and TiltPrim.ugt  and TiltPrim.fgt
overload <= : 'a as TiltPrim.ilte and TiltPrim.blte and TiltPrim.ulte and TiltPrim.flte
overload >= : 'a as TiltPrim.igte and TiltPrim.bgte and TiltPrim.ugte and TiltPrim.fgte

(* fixity *)
infix  7 * / div mod
infix  6 + - ^
infixr 5 :: @
infix  4 = <> > >= < <=
infix  3 := o
infix  0 before

(* types *)
(* unit, int, word, real, char -- primitive *)
(* eqtype string -- primitive *)
(* type substring -- provided by TopLevel *)
(* exn, 'a array, 'a vector, 'a ref -- primitive *)
(* datatype bool = false | true -- primitive *)
datatype 'a option = NONE | SOME of 'a
datatype order = LESS | EQUAL | GREATER
datatype 'a list = nil | :: of 'a * 'a list

type string = string (* TiltPrim *)

(* exceptions *)
exception Bind
exception Chr
exception Domain
exception Empty
exception Fail of string
exception Match 
exception Option
exception Size
exception Span
exception Subscript

(* values *)
(* o, before, ignore -- primitive *)
fun exnName (exn:exn) : string = Ccall(exnNameRuntime,exn)
fun exnMessage (exn:exn) : string= Ccall(exnMessageRuntime,exn)

fun getOpt (SOME x, _) = x | getOpt (NONE, y) = y
fun isSome (SOME _) = true | isSome NONE = false
fun valOf (SOME x) = x | valOf NONE = raise Option

fun not true = false | not false = true

fun rev l =
    let fun revappend([],x) = x
	  | revappend(hd::tl,x) = revappend(tl,hd::x)
    in  revappend(l,[])
    end
