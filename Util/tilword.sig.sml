signature TILWORD =
    sig
	
	type word
	val wordsize : int    (* guaranteed to be an positive even integer *)
	val zero : word       (* all bits zero *)
	val one : word        (* low bit one *)
	val neg_one : word    (* all bits one *)
	val low_mask : word   (* bits in lower half of word set *)
	val high_mask : word  (* bits in upper half of word set *)
	val most_neg : word   (* only high bit set *)
			        
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
	val smod : word * word -> word   (* (a div b) * b + (a mod b) = a   *)
	val squot : word * word -> word  (* rounds toward zero *)
	val srem : word * word -> word   (* (a quot b) * b + (a rem b) = a   *)
	val slt : word * word -> bool
	val slte : word * word -> bool
	val sgt : word * word -> bool
	val sgte : word * word -> bool
	    
	(* ----- unsigned arithmetic operations: can raise Div but wraps to avoid Overflow *)
	val unegate : word -> word
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
  val fromInt : int -> word       (* converts from int to word with sign extension *)
  val toInt : word -> int       (* converts to int treating word as signed *)
  val fromHexString : string -> word         (* for exmaple, "ff0a435" *)
  val fromDecimalString : string -> word     (* for example, "~342" *)
  val fromWordStringLiteral : string -> word  (* for exmaple, "0wx024334" or "0w123" *)
  val toHexString : word -> string
  val toDecimalString : word -> string

(*
  val wordToString : word -> string    (* prints a signed base10 representation 
                                          with an optional leading "-" *)
  val uwordToString : word -> string   (* prints a signed base10 representation *)

  (* ----- misc operations *)
  val log_2 : word -> (int * int)      (* the first int gives the position of the highest set bit
					  the second int gives the position of the lowest set bit
					  if no bits are set, both numbers are -1
					  if both numbers are non-zero and equal, the number is
					      an exact power of two *)

  val sub_word : string * int -> word 
      (* pulls out the ith word from a string -- endian dependent *)
*)
end
