(* Chris Okasaki / Robert Harper
   School of Computer Science
   Carnegie Mellon University
   Pittsburgh, PA 15213
*)

signature PARSING =
  (* See                                    *)
  (*   Hutton                               *)
  (*   "Higher-order functions for parsing" *)
  (*   JFP 2(3) (Jul 92) 323-43             *)
  (* or                                     *)
  (*   Paulson                              *)
  (*   ML for the Working Programmer        *)
  (*   p 321-328                            *)
sig

  include BASIC_PARSING

(*
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard
  infixr 1 ||
*)

  (* sequential composition *)
  val &&       : ('a,'t) T * ('b,'t) T -> ('a * 'b,'t) T 
  (* alternation *)
  val ||       : ('a,'t) T * ('a,'t) T -> ('a,'t) T 

  (* apply function to success value *)
  val wth      : ('a,'t) T * ('a -> 'b) -> ('b,'t) T 
  (* succeed only if check on successful is true *)
  val suchthat : ('a,'t) T * ('a -> bool) -> ('a,'t) T
  (* specify success value *)
  val return   : ('b,'t) T * 'a -> ('a,'t) T

  (* apply function to failure position *)
  val guard    : ('a,'t) T * (Pos.T -> 'b) -> ('a,'t) T

  (* n-ary sequential composition *)
  val seq      : ('a,'t) T list -> ('a list,'t) T
  (* n-ary alternation *)
  val alt      : ('a,'t) T list -> ('a,'t) T

  (* ensure that next token satisfies condition, yielding that token *)
  val satisfy  : ('t -> bool) -> ('t,'t) T 

  (* these require equality on tokens! *)

  (* check for a given token *)
  val literal  : ''t -> (''t,''t) T
  (* check for a given list of tokens *)
  val string   : ''t list -> (''t list,''t) T
  (* check for one of a list of tokens *)
  val oneof    : ''t list -> (''t,''t) T

  (* optional parse, yielding an optional result *)
  val opt      : ('a,'t) T -> ('a option,'t) T 
  (* optional parse, with given action on success *)
  val optional : ('a -> 'b) -> 'b -> ('a,'t) T -> ('b,'t) T

  (* zero or more copies *)
  val repeat   : ('a,'t) T -> ('a list,'t) T 
  (* one or more *)
  val repeat1  : ('a,'t) T -> ('a list,'t) T 

  (* parse two things, yielding value of first *)
  val first    : ('a,'t) T -> ('b,'t) T -> ('a,'t) T 
  val <<       : ('a,'t) T * ('b,'t) T -> ('a,'t) T 
  (* ... second *)
  val second   : ('a,'t) T -> ('b,'t) T -> ('b,'t) T 
  val >>       : ('a,'t) T * ('b,'t) T -> ('b,'t) T 
  (* .... middle of three *)
  val middle   : ('a,'t) T -> ('b,'t) T -> ('c,'t) T -> ('b,'t) T 

  (* parse one or more, with given separator between items *)
  val separate : ('a,'t) T -> ('b,'t) T -> ('a list,'t) T 
  (* ... zero or more *)
  val separate0: ('a,'t) T -> ('b,'t) T -> ('a list,'t) T 
  (* one or more, allowing trailing separator *)
  val separate': ('a,'t) T -> ('b,'t) T -> ('a list,'t) T 

  (* nested parsers *)
  val use      : (('a,'t) T,'t) T -> ('a,'t) T

  (***** Pre/In/Post-fix utilities *****)

  datatype Associativity = LeftAssoc | RightAssoc | NonAssoc

  datatype 'a Opr =
      Prefix of int * ('a -> 'a)
    | Infix of Associativity * int * ('a * 'a -> 'a)
    | Postfix of int * ('a -> 'a)

  datatype 'a FixityItem =
      Atm of 'a
    | Opr of 'a Opr

  val parsefixity : ('a FixityItem, 't) T -> ('a,'t) T
  val parsefixityadj : ('a FixityItem, 't) T -> ('a * 'a -> 'a) -> ('a,'t) T


  (***** Helpful utilities for manipulating intermediate results *****)

  val flat3 : 'a * ('b * 'c) -> 'a * 'b * 'c
  val flat4 : 'a * ('b * ('c * 'd)) -> 'a * 'b * 'c * 'd
  val flat5 : 'a * ('b * ('c * ('d * 'e))) -> 'a * 'b * 'c * 'd * 'e

end
