(* parse.sml by Tom 7 *)

structure Parse :> PARSE =
struct

  fun curry2 f a b = f (a, b)

  val VERSION = "$Id$"

  exception Impossible

  open Parsing Gml Tokens

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infixr 2 wth suchthat return guard
  infixr 1 ||
      
  fun resvd x = 
      case Envmap.find (Eval.T.opers, x) of 
	  NONE => false
	  | _  => true

  fun stl x = String.extract (x, 1, NONE)

  fun lit x = (anyWord suchthat (curry2 op= x))
      
  val oper = (anyWord suchthat resvd) wth Oper

  val binder = ((anyWord suchthat (fn x => CharVector.sub(x, 0) = #"/")) 
		suchthat (fn x => not (resvd (stl x))))
                wth (fn x => Binder (stl x))

    
  val bool = lit "true" return (Bool true)
          || lit "false" return (Bool false)
		
  val literal = anyWord wth Var
      
  fun array () = ((atok LSquare) >> repeat ($exp) << (atok RSquare)) 
      wth Array
      
  and function () = ((atok LCurly) >> repeat ($exp) << (atok RCurly)) wth Fun
      
  and exp () =
      alt [ anyNumber wth Int,
	    anyFloat wth Real,
	    anyString wth String,
	    $array,
	    $function,
	    lit "if" return If,
	    lit "apply" return Apply,
	    bool,
	    oper,
	    binder,
	    literal ]
      
  val prog = repeat ($exp)
						
end

