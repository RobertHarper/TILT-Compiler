(* Overloaded special constants and operators. *)

(* See the Top-Level Environment chapter of the basis library spec and
 * appendix E of the Definition.  *)

(* For the operators, only one representative from each set of shared
 * types is checked.  (So Int.int is checked but not Int32.int,
 * Position.int, or LargeInt.int.)  *)

signature NUM =
sig
    type num
    val plus   : num * num -> num
    val minus  : num * num -> num
    val times  : num * num -> num
end

signature WORDINT =
sig
    type wordint
    val div : wordint * wordint -> wordint
    val mod : wordint * wordint -> wordint
end

signature REALINT =
sig
    type realint
    val negate : realint -> realint
    val abs    : realint -> realint
end

signature NUMTEXT =
sig
    type numtext
    val lt  : numtext * numtext -> bool
    val gt  : numtext * numtext -> bool
    val lte : numtext * numtext -> bool
    val gte : numtext * numtext -> bool
end

signature INTEGER_OVERLOAD =
sig
    type int
    include NUM where type num = int
    include WORDINT where type wordint = int
    include REALINT where type realint = int
    include NUMTEXT where type numtext = int
end

signature WORD_OVERLOAD =
sig
    type word
    include NUM where type num = word
    include WORDINT where type wordint = word
    include NUMTEXT where type numtext = word
end

signature REAL_OVERLOAD =
sig
    type real
    include NUM where type num = real
    include REALINT where type realint = real
    include NUMTEXT where type numtext = real
end	

signature CHAR_OVERLOAD =
sig
    type char
    include NUMTEXT where type numtext = char
end

signature STRING_OVERLOAD =
sig
    type string
    include NUMTEXT where type numtext = string
end

signature OVERLOAD =
sig

    (* special constant overloading *)
    val int : int
    val word : word
    val word8 : Word8.word
    val real : real
    val char : char
    val string : string
	
    (* operator overloading *)
    structure I : INTEGER_OVERLOAD where type int = Int.int
	
    structure W : WORD_OVERLOAD where type word = Word.word
    structure W8 : WORD_OVERLOAD where type word = Word8.word

    structure R : REAL_OVERLOAD where type real = Real.real

    structure C : CHAR_OVERLOAD where type char = Char.char
    structure S : STRING_OVERLOAD where type string = String.string

    (* default types at overloaded special constants and operators *)
    structure Defaults :
    sig
	val int : int
	val word : word
	val real : real
	val string : string
	val char : char
	    
	val plus : int * int -> int
	val minus : int * int -> int
	val times : int * int -> int
	val div : int * int -> int
	val mod : int * int -> int
	val / : real * real -> real
	val ~ : int -> int
	val abs : int -> int
	val lt : int * int -> bool
	val gt : int * int -> bool
	val lte : int * int -> bool
	val gte : int * int -> bool
    end
end
