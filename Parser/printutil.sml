(*$import PRINTUTIL Symbol SYMBOL Control Char TextIO Int String *)
(* Copyright 1989 by AT&T Bell Laboratories *)
(* printutil.sml *)

structure PrintUtil :> PRINTUTIL where Symbol = Symbol = 
struct

  val say = Control.Print.say

  structure Symbol : SYMBOL = Symbol

  fun newline () = say "\n"
  fun tab 0 = () | tab n = (say " "; tab(n-1))

  fun printSequence (separator: string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; say separator; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun printClosedSequence (front: string, sep, back:string) pr elems =
      (say front; printSequence sep pr elems; say back)

  fun printSym(s: Symbol.symbol) = TextIO.print(Symbol.name s)
      (* fix -- maybe this belongs in Symbol *)

  fun formatQid p =
    let fun f [s] = [Symbol.name s]
          | f (a::r) = Symbol.name a :: "." :: f r
	  | f nil = ["<bogus qid>"]
     in concat(f p)
    end

  val stringDepth = Control.Print.stringDepth

  fun escape i = let
	val m = Int.toString: int->string 
	in
	  concat ["\\", m(i div 100), m((i div 10)mod 10), m(i mod 10)]
	end
  val offset = Char.ord #"A" - Char.ord #"\^A"
  fun ml_char #"\n" = "\\n"
    | ml_char #"\t" = "\\t"
    | ml_char #"\\" = "\\\\"
    | ml_char #"\"" = "\\\""
    | ml_char c = if ((c >= #"\^A") andalso (c <= #"\^Z"))
	  then "\\^" ^ String.str(Char.chr(Char.ord c + offset))
	else if ((#" " <= c) andalso (c <= #"~"))
	  then String.str c
	  else escape(Char.ord c)

  fun mlstr s = concat["\"", concat(map ml_char (explode s)), "\""]
  fun pr_mlstr s =
      let val depth = !stringDepth
	  fun pr(i, res) =
	      if i=depth then ("#" :: res)
	      else (let val ch = String.sub(s,i)
		    in  pr(i+1, (ml_char ch)::res)
		    end handle Substring => res)

          val res = ("\"") :: (pr (0, ["\""]))
       in concat (rev res)
      end

  fun nlindent n = (newline(); tab n)

  fun printvseq ind (sep:string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; nlindent ind; say sep; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  (* debug print functions *)
  val prIntPath = printClosedSequence ("[",",","]") (say o Int.toString)
  val prSymPath = printSequence "." printSym

end (* structure PrintUtil *)

