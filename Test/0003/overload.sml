(*$import Firstlude TiltPrim Prelude TopLevel Int Word Word8 Word31 Real Char String OVERLOAD OvldDefaults *)

(* Overloaded special constants and operators. *)

(* See the Top-Level Environment chapter of the basis library spec and
 * appendix E of the Definition.  *)

structure Overload :> OVERLOAD =
struct

    (* special constant overloading *)
    val int : int = 0
    val word : word = 0w0
    val word8 : Word8.word = 0w0
    (* val word31 : Word31.word = 0w0 -- unsupported *)
    val real : real = 0e0
    val char : char = #"\000"
    val string : string = ""
	
    (* operator overloading *)
    structure I =
    struct
	type int = Int.int
	type num = int
	val plus   : num * num -> num = op +
	val minus  : num * num -> num = op -
	val times  : num * num -> num = op *
	type wordint = int
	val div : wordint * wordint -> wordint = op div
	val mod : wordint * wordint -> wordint = op mod
	type realint = int
	val negate : realint -> realint = ~
	val abs    : realint -> realint = abs
	type numtext = int
	val lt  : numtext * numtext -> bool = op <
	val gt  : numtext * numtext -> bool = op >
	val lte : numtext * numtext -> bool = op <=
	val gte : numtext * numtext -> bool = op >=
    end

    structure W =
    struct
	type word = Word.word
	type num = word
	val plus   : num * num -> num = op +
	val minus  : num * num -> num = op -
	val times  : num * num -> num = op *
	type wordint = word
	val div : wordint * wordint -> wordint = op div
	val mod : wordint * wordint -> wordint = op mod
	type numtext = word
	val lt  : numtext * numtext -> bool = op <
	val gt  : numtext * numtext -> bool = op >
	val lte : numtext * numtext -> bool = op <=
	val gte : numtext * numtext -> bool = op >=
    end

    structure W8 =
    struct
	type word = Word8.word
	type num = word
	val plus   : num * num -> num = op +
	val minus  : num * num -> num = op -
	val times  : num * num -> num = op *
	type wordint = word
	val div : wordint * wordint -> wordint = op div
	val mod : wordint * wordint -> wordint = op mod
	type numtext = word
	val lt  : numtext * numtext -> bool = op <
	val gt  : numtext * numtext -> bool = op >
	val lte : numtext * numtext -> bool = op <=
	val gte : numtext * numtext -> bool = op >=
    end

    structure W31 =
    struct
	type word = Word31.word
	type num = word
	val plus   : num * num -> num = op +
	val minus  : num * num -> num = op -
	val times  : num * num -> num = op *
	type wordint = word
	val div : wordint * wordint -> wordint = op div
	val mod : wordint * wordint -> wordint = op mod
	type numtext = word
	val lt  : numtext * numtext -> bool = op <
	val gt  : numtext * numtext -> bool = op >
	val lte : numtext * numtext -> bool = op <=
	val gte : numtext * numtext -> bool = op >=
    end

    structure R =
    struct
	type real = Real.real
	type num = real
	val plus   : num * num -> num = op +
	val minus  : num * num -> num = op -
	val times  : num * num -> num = op *
	type realint = real
	val negate : realint -> realint = ~
	val abs    : realint -> realint = abs
	type numtext = real
	val lt  : numtext * numtext -> bool = op <
	val gt  : numtext * numtext -> bool = op >
	val lte : numtext * numtext -> bool = op <=
	val gte : numtext * numtext -> bool = op >=
    end

    structure C =
    struct
	type char = Char.char
	type numtext = char
	val lt  : numtext * numtext -> bool = op <
	val gt  : numtext * numtext -> bool = op >
	val lte : numtext * numtext -> bool = op <=
	val gte : numtext * numtext -> bool = op >=
    end

    structure S =
    struct
	type string = String.string
	type numtext = string
	val lt  : numtext * numtext -> bool = op <
	val gt  : numtext * numtext -> bool = op >
	val lte : numtext * numtext -> bool = op <=
	val gte : numtext * numtext -> bool = op >=
    end
	    
    (* operator default types *)
    structure Defaults = OvldDefaults
end
