(*$import Firstlude TiltPrim *)

(* The purpose of Prelude is to set up types, fixity information, and
   values that are used throughout the basis library implementation.
   It is also a convienient place to hang values required by other
   parts of the compiler.  The interface provided by Prelude should
   not be assumed by users of the basis library.

   Users who want the standard top-level environment should import
   Prelude and TopLevel.

   A few compiler primitives are pre-defined at the top-level.  Most
   are in structure TiltPrim.  See Elaborator/basis.sml.
*)

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
(* eqtype string -- provided later *)
(* type substring -- provided by TopLevel *)
(* exn, 'a array, 'a vector, 'a ref -- primitive *)
(* datatype bool = false | true -- Firstlude *)
datatype 'a option = NONE | SOME of 'a
datatype order = LESS | EQUAL | GREATER
datatype 'a list = nil | :: of 'a * 'a list
    
(* vector_eq needed so we can use vector types: may change if elaborator changes *)
structure TiltVectorEq =
struct
    fun vector_eq (equaler : 'a * 'a -> bool) (x : 'a vector, y : 'a vector) : bool = 
	let val lx = TiltPrim.vector_length x
	    val ly = TiltPrim.vector_length y
	    fun vector_eq_loop n =
		n >= lx orelse (equaler(TiltPrim.unsafe_vsub(x,n),TiltPrim.unsafe_vsub(y,n))
				andalso (vector_eq_loop (n + 0w1)))
	in  (lx = ly) andalso vector_eq_loop 0w0
	end
end

type string = char vector

(* exceptions *)
exception Bind
exception Chr
exception Div				(* must be toplevel for runtime *)
exception Domain
exception Empty
exception Fail of string
exception Match
exception Option
exception Overflow			(* must be toplevel for runtime *)
exception Size
exception Span
exception Subscript

(* values *)
fun f o g = fn x => f(g x)
fun a before b = a
fun ignore _ = ()

fun not true = false
  | not false = true
    
fun rev l = 
    let fun revappend([],x) = x
	  | revappend(hd::tl,x) = revappend(tl,hd::x)
    in  revappend(l,[])
    end

structure TiltExn =			(* must agree with Runtime/exn.c *)
struct
    exception SysErr of string * int option
    exception LibFail of string
end