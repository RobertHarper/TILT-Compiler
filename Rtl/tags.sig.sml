signature RTLTAGS =
sig
   structure Rtl : RTL

   (* the 3-bit type field *)

   val record : Word32.word
   val intarray : Word32.word
   val realarray : Word32.word
   val ptrarray : Word32.word

   (* bit offset of length field *)

   val real_len_offset : int  (* measured in double-precision floats = 8 bytes *)
   val int_len_offset : int   (* measured in bytes *)

   (* compute tags statically given size of array *)

   val skiptag : Word32.word
   val realarraytag : Word32.word -> Word32.word
   val intarraytag : Word32.word -> Word32.word
   val ptrarraytag : Word32.word -> Word32.word

   (* # of bytes in string *)

   val rawstringtag : int -> Word32.word

   (* given traceability of fields, compute list of tag words.
       For each tag word, give
          (1) static portion of tag, making COMPUTEs 0
	  (2) how to compute dynamic portion of tag.
	      This is a list of (1) the bitpos
				(2) the location of the traceability
				    information.*)
   type tags = {static : Word32.word,
		dynamic : {bitpos : int,
			   path : Rtl.rep_path} list} list
   val recordtag : Rtl.rep list -> tags


end

     
