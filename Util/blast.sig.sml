(*
	The per-stream codec can be reset with resetIn and resetOut.
*)

signature BLASTER =
sig

    exception BadMagicNumber
    val BlastDebug : bool ref

    type instream
    type outstream
    type 'a blastin = instream -> 'a
    type 'a blastout = outstream -> 'a -> unit

    val openIn : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val resetIn : instream -> unit
    val resetOut : outstream -> unit
    val closeIn : instream -> unit
    val closeOut : outstream -> unit

    val endOfStream : instream -> bool

    (*
	If (bout',bin') = magic (bout,bin,m),
	then bout' writes magic number m before invoking bout
	and bin' checks for magic number m before invoking
	bin.  Bin' may raise BadMagicNumber.
    *)
    val magic : 'a blastout * 'a blastin * string -> 'a blastout * 'a blastin
    val checkMagic : instream -> string option	(* For debugging *)

    val blastOutWord8 : outstream -> Word8.word -> unit
    val blastInWord8 : instream -> Word8.word
    val blastOutWord64 : outstream -> TilWord64.word -> unit
    val blastInWord64 : instream -> TilWord64.word
    val blastOutWord32 : outstream -> Word32.word -> unit
    val blastInWord32 : instream -> Word32.word
    val blastOutInt : outstream -> int -> unit
    val blastInInt : instream -> int
    val blastOutBool : outstream -> bool -> unit
    val blastInBool : instream -> bool
    val blastOutString : outstream -> string -> unit
    val blastInString : instream -> string
    val blastOutPair : 'a blastout -> 'b blastout -> outstream -> ('a * 'b) -> unit
    val blastInPair : 'a blastin -> 'b blastin -> instream -> ('a * 'b)
    val blastOutTriple : 'a blastout -> 'b blastout -> 'c blastout -> outstream -> ('a * 'b * 'c) -> unit
    val blastInTriple : 'a blastin -> 'b blastin -> 'c blastin -> instream -> ('a * 'b * 'c)
    val blastOutOption : 'a blastout -> outstream -> 'a option -> unit
    val blastInOption : 'a blastin -> instream -> 'a option
    val blastOutList : 'a blastout -> outstream -> 'a list -> unit
    val blastInList : 'a blastin -> instream -> 'a list

end
