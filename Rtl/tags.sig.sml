(*$import Rtl TilWord32 *)
signature RTLTAGS =
sig

   (* the low 3 bits indicate the object type *)
   val skip : TilWord32.word
   val record : TilWord32.word
   val intarray : TilWord32.word
   val realarray : TilWord32.word
   val ptrarray : TilWord32.word

   (* bit offset of length and mask field *)
   val int_len_offset : int   (* measured in bytes *)
   val ptr_len_offset : int   (* measured in words *)
   val real_len_offset : int  (* measured in double-precision floats = 8 bytes *)
   val maxRecordLength : int
   val rec_len_offset : int   (* measured in words *)
   val rec_len_mask : int   
   val rec_mask_offset : int  (* low bit corresponds to first entry in record *)

   (* Compute array tags given logical size *)
   val intarraytag : TilWord32.word -> TilWord32.word
   val ptrarraytag : TilWord32.word -> TilWord32.word
   val realarraytag : TilWord32.word -> TilWord32.word

   (* Compute a record tag(s) given a list of RTL representation.
      The tag words is expressed by the bitwise OR of 2 parts:
         (1) static portion of tag
	 (2) dynamic portion of tag
	      (a) Present only if there are COMPUTEs
	      (b) A list of (1) the bitpos
			    (2) the path location of the dynamic traceability
   *)
   type tag = {static : TilWord32.word,
	       dynamic : {bitpos : int,
			   path : Rtl.rep_path} list}
   val recordtag : Rtl.rep list -> tag


end

     
