(*$import Rtl TilWord32 *)
signature RTLTAGS =
sig

   (* the 3-bit type field *)

   val record : TilWord32.word
   val intarray : TilWord32.word
   val realarray : TilWord32.word
   val ptrarray : TilWord32.word

   (* bit offset of length field *)

   val real_len_offset : int  (* measured in double-precision floats = 8 bytes *)
   val int_len_offset : int   (* measured in bytes *)
   val ptr_len_offset : int   (* measured in words *)

   (* compute tags statically given size of array *)

   val skiptag : TilWord32.word
   val realarraytag : TilWord32.word -> TilWord32.word
   val intarraytag : TilWord32.word -> TilWord32.word
   val ptrarraytag : TilWord32.word -> TilWord32.word

   (* # of bytes in string *)

   val rawstringtag : int -> TilWord32.word

   (* given traceability of fields, compute list of tag words.
       For each tag word, give
          (1) static portion of tag, making COMPUTEs 0
	  (2) how to compute dynamic portion of tag.
	      This is a list of (1) the bitpos
				(2) the location of the traceability
				    information.*)
   type tags = {static : TilWord32.word,
		dynamic : {bitpos : int,
			   path : Rtl.rep_path} list} list
   val recordtag : Rtl.rep list -> tags


end

     
