structure Tokens :> TOKENS =
struct

  open Parsing

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard
  infixr 1 ||

  exception Impossible

  val quotc = #"\"" (* "  emacs syntax coloring =( *)


  (* Report an error with its position. *)

  fun error s pos = print (Pos.toString pos ^ ": " ^ s ^ "\n")

  (* Succeeds if p succeeds at the current point, but doesn't
     consume anything. *)

  fun ahead p = lookahead p (fn _ => succeed ())

  (* Ignores the result of p *)

  fun ignore p = p return ()



  (**** Tokenizer ****)

  datatype tok =
      LSquare 
    | RSquare
    | RCurly
    | LCurly
		| EOF

  datatype token =
      Wordtok of string
    | Numbertok of int
    | Stringtok of string
    | Floattok of real
    | Tok of tok

  (* Predicates for different kinds of characters. *)
      
  (* Tom 7 made these a little more efficient by
     avoiding the computation of the table in .contains
     each call. Also, - is now allowed in identifiers
     because-it-is-easy-to-type. *)

  (* FIXME: ICFP: no escapes in string literals!
     FP correct?
     vertical tab
     Also fix chars allowed in identifiers, etc.
   *)

  fun isSpace #"\v" = true
    | isSpace #" "  = true
    | isSpace #"\t" = true
    | isSpace #"\n" = true
    | isSpace #"\r" = true
    | isSpace _ = false

  fun isLetter #"_" = true
    | isLetter #"-" = true
    | isLetter c = Char.isAlpha c orelse Char.isDigit c

  fun isIdent c = isLetter c orelse Char.isDigit c

  (* A sequence of alphanumeric characters starting with an
     alphabetic character. *)

  val letters = satisfy isLetter && repeat (satisfy isIdent) wth op::

  (* A "word" of letters or symbolic characters. *)

  val word = ((opt (literal #"/")) && letters) wth 
			(fn (SOME _, l) => implode ( #"/" :: l )
		    | (NONE, l) => implode l)

  (* A sequence of digits. *)

  val digstream = (repeat1 (satisfy Char.isDigit))

  val integer = 
      ((digstream -- done) ||
       (digstream << (ahead (satisfy (fn x => not (Char.isDigit x))))))
      wth (Option.valOf o Int.fromString o implode)

  val escapechar = 
      ((literal #"\\" >> literal quotc) ||
       (literal #"\\" >> literal #"\\") ||
       (literal #"\\" && literal #"n" return #"\n"))
		 
  val floatmag = ((repeat1  (satisfy Char.isDigit)) <<
		  (literal #".")) &&
      (repeat1 (satisfy Char.isDigit)) &&
      (opt (alt [literal #"e", literal #"E"] >> 
	    ((opt (literal #"-")) && 
	     (repeat (satisfy Char.isDigit)))))
      wth (fn (a, (b, exponent)) =>
	   let val mantissa = 
	       (Option.valOf (Real.fromString ("0." ^ (implode b)))) +
	       (case a of 
		    nil => 0.0
		  | _ => Real.fromInt 
			(Option.valOf (Int.fromString (implode a))))
	   in
	       case exponent of
		   NONE => mantissa
		 | SOME (nexp, e) => 
		       let val ee = Real.fromInt (Option.valOf (Int.fromString 
								(implode e)))
		       in
			   mantissa * (case nexp of
					   NONE => 10.0 * ee
					 | SOME _ => ee / 10.0)
		       end
	   end)
      
  val float = opt (literal #"-") && floatmag
      wth (fn (SOME a, b) => ~ b
    | (NONE, b) => b)
      
  (*
val insidechars = (repeat (satisfy (fn x => x <> quotc))) wth implode
    *)
      
  val getchar = (satisfy (fn x => x <> quotc andalso x <> #"\\") ||
		 escapechar)
      
  val insidechars = (repeat getchar) wth implode

  val stringlit = middle 
      (literal quotc)
	  insidechars
      (literal quotc)

  (* A positive or negative integer constant. *)

  val number = integer || (literal #"-" >> integer) wth op~

(* old skool
  (* A comment, potentially with nested comments inside. *)

  fun comment () = string [#"(",#"*"]
                && (repeat ($insideComment) && string [#"*",#")"]
                    guard error "Unterminated comment.")

  (* Either a nested comment or a single character (which is not
     start of a nested comment or the comment terminator). *)

  and insideComment () =
         ignore ($comment)
      || any -- (fn #"*" => ahead (satisfy (fn x => x <> #")"))
                  | #"(" => ahead (satisfy (fn x => x <> #"*"))
                  | _    => succeed ())
*)

  fun comment () = literal #"%" >> 
      repeat (any suchthat (fn x => x <> #"\n"))

  (* White space. *)

  val space = repeat (ignore ($comment) || ignore (satisfy isSpace))

  (* A token (skipping over white space).  The token is marked with
     its position. *)

  val tok = alt [ literal #"[" return LSquare,
		  literal #"]" return RSquare,
		  literal #"{" return LCurly,
		  literal #"}" return RCurly,
			literal #"\001" return EOF ]

  val token = space >> (!! (
			    float wth Floattok ||
			    number wth Numbertok || 
			    stringlit wth Stringtok || 
			    tok wth Tok ||
			    word wth Wordtok
			    ))

(* token is no longer an eqtype (reals...) *)

  fun litWord s  = satisfy (fn Wordtok ss => s = ss | _ => false)
      wth (fn Wordtok ss => ss | _ => raise Impossible)
  fun litNumber i = satisfy (fn Numbertok ii => i = ii | _ => false) 
      wth (fn Numbertok ii => ii | _ => raise Impossible)

  val anyWord = any -- (fn Wordtok s => succeed s | _ => fail)
  val anyNumber = any -- (fn Numbertok i => succeed i | _ => fail)
  val anyString = any -- (fn Stringtok s => succeed s | _ => fail)
  val anyFloat = any -- (fn Floattok f => succeed f | _ => fail)

  fun atok t = any -- (fn Tok tt => if (t = tt) then succeed t else fail
                        | _ => fail)

end
