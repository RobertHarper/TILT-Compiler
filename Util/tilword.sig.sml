signature TILWORD =
    sig

	type halfword
	type word
	val wordsize : int    (* guaranteed to be an positive even integer *)
	val halfsize : int
	val zero : word       (* all bits zero *)
	val one : word        (* low bit one *)
	val neg_one : word    (* all bits one *)
	val low_mask : word   (* bits in lower half of word set *)
	val high_mask : word  (* bits in upper half of word set *)
	val most_neg : word   (* only high bit set *)
	val most_pos : word   (* only high bit clear *)

	(* ----- checks for bit-pattern equality: sign-insensitive *)
	val equal : word * word -> bool
	val nequal : word * word -> bool

	(* ----- signed arithmetic operations: can raise Overflow or raise Div *)
	val sign : word -> int           (* -1 for neg nums, 0 for zero, 1 for pos nums *)
	val absolute : word -> word
	val snegate : word -> word
	val smult : word * word -> word
	val splus : word * word -> word
	val sminus : word * word -> word
	val sdiv : word * word -> word   (* rounds toward negative infinity *)
	val smod : word * word -> word   (* (a sdiv b) * b + (a smod b) = a   *)
	val squot : word * word -> word   (* rounds toward zero *)
	val srem : word * word -> word   (* (a squot b) * b + (a srem b) = a   *)
	val slt : word * word -> bool
	val slte : word * word -> bool
	val sgt : word * word -> bool
	val sgte : word * word -> bool

	(* ----- unsigned arithmetic operations: can raise Div but wraps to avoid Overflow *)
	val uplus : word * word -> word
	val uminus: word * word -> word
	val umult : word * word -> word
	val udiv  : word * word -> word
	val umod : word * word -> word
	val uplus' : word * word -> word * word    (* high * low *)
	val umult' : word * word -> word * word    (* high * low *)
	val ult : word * word -> bool
	val ulte : word * word -> bool
	val ugt : word * word -> bool
	val ugte : word * word -> bool

	(* ----- logical operations *)
	val notb : word -> word
	val orb : word * word -> word
	val andb : word * word -> word
	val xorb : word * word -> word
	val lshift : word * int -> word
	val rshiftl : word * int -> word   (* logical right shift *)
	val rshifta : word * int -> word   (* arithmatic right shift *)


	(* ----- conversion operations *)
	val fromSignedHalf : halfword -> word	(* never fails *)
	val fromUnsignedHalf : halfword -> word	(* never fails *)
	val toSignedHalf : word -> halfword
	val toUnsignedHalf : word -> halfword	(* never fails *)
	(*
		Warning: On SML/NJ int is 31 bits so use these with care.
	*)
	val fromInt : int -> word	(* with sign extension *)
	val toInt : word -> int		(* signed *)
	val toIntU : word -> int		(* unsigned *)
	val fromHexString : string -> word	(* must match [0-9a-fA-F]+ *)
	val fromDecimalString : string -> word	(* must match ~?[0-9]+ *)
	val toHexString : word -> string	(* matches [0-9a-fA-F]+ *)
	val toDecimalString : word -> string	(* matches -?[0-9]+ *)

end
