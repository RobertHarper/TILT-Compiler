(* Proof of concept: Using prefix-free mappings to represent lists of
   indices. *)

structure CodeError =
struct
    exception Error of string
end

(* Encode list of integers as a single word. *)
signature PROJECTION_CODE =
sig
    type word

    (* Number of significant bits in a code word. *)
    val nBits : int
	
    (* Returns NONE if the indices do not fit in nBits.  Otherwise,
       SOME w, where the least significant nBits bits of w represent
       the indices.  *)
    val encode : int list -> word option
	
    (* May raise CodeError.Error but never when word comes from
       encode.  *)
    val decode : word -> int list
end

structure Bit =
struct
    datatype bit = Z | O
end

(* Encode words as a bit strings. *)
signature CODE =
sig
    type word
	
    val encode : word -> Bit.bit list
	
    (* May raise CodeError.Error but never when the bit string comes
       from encode. *)
    val decode : Bit.bit list -> word
end

(* Encode words as prefix-free bit strings. *)
signature PREFIX_FREE_CODE =
sig
    (* Invariant: If (encode n) is a prefix of (encode m), then n = m.  *)
    type word
	
    val encode : word -> Bit.bit list
	
    (* Will raise CodeError.Error if the bit string does not begin
       with a coded number.  Note that the empty list can not be the
       code of any number since it is a prefix of every code.  *)
    val decode : Bit.bit list -> word * Bit.bit list
end

(* Normal binary representation *)
functor Binary (structure Word : WORD) 
    :> CODE where type word = Word.word =
struct
    type word = Word.word
    val zero = Word.fromInt 0
    val one = Word.fromInt 1
	
    (* encode' : word * bit list -> bit list *)
    fun encode' (w, acc) =
	if w = zero then acc
	else let val lsb = Word.andb (w, one)
		 val w' = Word.>> (w, 0w1)
		 val bit = if lsb = zero then Bit.Z else Bit.O
	     in  encode' (w', bit :: acc)
	     end
	 
    (* encode : word -> bit list *)
    fun encode w =
	if w = zero then [Bit.Z]
	else encode' (w, nil)
	    
    (* decode' : bit * word -> word *)
    fun decode' (Bit.Z, w) = Word.<< (w, 0w1)
      | decode' (Bit.O, w) = Word.orb (Word.<< (w, 0w1), one)
	
    (* decode : bit list -> word *)
    fun decode bits =
	if length bits > Word.wordSize
	    then raise CodeError.Error "too many bits"
	else foldl decode' zero bits
end

(* Prefix-free code from Knuth.
 
	I write |s| for the length of a bit string s, L(n) for the
	length of the normal binary encoding of the number n, lg(n)
	for the floor of the base-two logarithm of n, and lg*(n) for
	the least integer m > 0 such that (lg^m)(n) = 0.  The length
	of an encoded word under this prefix-free mapping rho is

		|rho(n)| = 1 + lg(n) + lg(lg(n)) + ... + lg*(n)
                         =      L(n) + lg(lg(n)) + ... + lg*(n)
			 =      L(n) + O(lg(lg(n)))
 *)
functor PrefixFree (structure Word : WORD
		    structure Binary : CODE where type word = Word.word)
    :> PREFIX_FREE_CODE where type word = Word.word =
struct
    type word = Word.word
    val zero = Word.fromInt 0
    val one = Word.fromInt 1

    (* encode : word -> bit list *)
    fun encode w =
	if w = zero then [Bit.Z]
	else let val alpha = tl (Binary.encode w)
		 val length = Word.fromInt (length alpha)
	     in  Bit.O :: (encode length @ alpha)
	     end

    (* split : int * 'a list * 'a list -> 'a list * 'a list *)
    fun split (0, acc, bits) = (rev acc, bits)
      | split (n, acc, nil) = raise CodeError.Error "too few bits"
      | split (n, acc, bit :: bits) = split (n-1, bit :: acc, bits)

    (* decode : bit list -> word * bit list *)
    fun decode nil = raise CodeError.Error "empty code"
      | decode (Bit.Z :: bits) = (zero, bits)
      | decode (Bit.O :: bits) =
	let val (length, bits) = decode bits
	    val length = Word.toInt length
	    val (alpha, bits) = split (length, nil, bits)
	    val w = Binary.decode (Bit.O :: alpha)
	in  (w, bits)
	end
end

functor Projection (structure Word : WORD
		    structure Binary : CODE where type word = Word.word
		    structure PrefixFree : PREFIX_FREE_CODE
			where type word = Word.word
		    val nBits : int)
    :> PROJECTION_CODE where type word = Word.word =
struct
    
    (* We encode the list [a_1, ..., a_n] as rho(n) @ rho(a_1) @ ... @
       rho(a_n) @ junk where rho is PrefixFree.encode and junk extends
       the code to nBits bits. *)

    type word = Word.word

    val nBits = nBits

    (* encodeInt : int -> bit list *)
    fun encodeInt i = PrefixFree.encode (Word.fromInt i)

    (* decodeInt : bit list -> int * bit list *)
    fun decodeInt bits =
	let val (w, bits) = PrefixFree.decode bits
	in  (Word.toIntX w, bits)
	end

    (* junk : int -> bit list *)
    fun junk n = List.tabulate (n, fn _ => Bit.Z)

    (* encode : int list -> word option *)
    fun encode indices =
	let val code = (length indices) :: indices
	    val code = List.concat (map encodeInt code)
	    val njunk = nBits - (length code)
	in  if njunk < 0 then NONE
	    else SOME (Binary.decode (code @ (junk njunk)))
	end

    (* decode' : int * int list * bit list -> int list *)
    fun decode' (0, acc, _) = rev acc
      | decode' (n, acc, bits) =
	let val (index, bits) = decodeInt bits
	in  decode' (n-1, index :: acc, bits)
	end

    (* decode : word -> int list *)
    fun decode w =
	let val bits = Binary.encode w
	    val (n, bits) = decodeInt bits
	in  decode' (n, nil, bits)
	end
end

structure Binary32 = Binary (structure Word = Word32)
structure PrefixFree32 = PrefixFree (structure Word = Word32
				     structure Binary = Binary32)
structure Projection30 = Projection(structure Word = Word32
				    structure Binary = Binary32
				    structure PrefixFree = PrefixFree32
				    val nBits = 30)
