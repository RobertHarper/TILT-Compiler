(*$import TOP Word8 Int Int32 Position SysWord Real64 Word32 Word31 *)

(* Top-level types and values. *)

(* See the Top-Level Environment chapter of the basis library spec. *)

structure Top : TOP =
struct

    type unit = unit
    type int = int
    type word = word
    type real = real
    type char = char
    type string = string
    type substring = substring
    type exn = exn
    type 'a array = 'a array
    type 'a vector = 'a vector
    type 'a ref = 'a ref
    datatype bool = datatype bool
    datatype 'a option = datatype option
    datatype order = datatype order
    datatype 'a list = datatype list

    exception Bind = Bind
    exception Chr = Chr
    exception Div = Div
    exception Domain = Domain
    exception Empty = Empty
    exception Fail = Fail
    exception Match = Match
    exception Option = Option
    exception Overflow = Overflow
    exception Size = Size
    exception Span = Span
    exception Subscript = Subscript

    nonfix  * / div mod + - ^ :: @ = <> > >= < <= := o before
    
    val ! = !
    val := = :=
    val before = before
    val ignore = ignore
    val o = o
    val exnName = exnName
    val exnMessage = exnMessage
    val getOpt = getOpt
    val isSome = isSome
    val valOf = valOf
    val not = not
    val real = real
    val trunc = trunc
    val floor = floor
    val ceil = ceil
    val round = round
    val ord = ord
    val chr = chr
    val size = size
    val str = str
    val concat = concat
    val implode = implode
    val explode = explode
    val substring = substring
    val ^ = ^
    val null = null
    val hd = hd
    val tl = tl
    val length = length
    val rev = rev
    val @ = @
    val app = app
    val map = map
    val foldr = foldr
    val foldl = foldl
    val print = print
    val vector = vector
    val use = use
end
