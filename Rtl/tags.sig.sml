(*$import Rtl TilWord32 *)
signature RTLTAGS =
sig

   val ptrWriteBarrier : bool ref     (* record pointer array mutation *)
   val fullWriteBarrier : bool ref    (* all mutations recorded *)
   val mirrorGlobal : bool ref     (* replicate pointer globals *)
   val mirrorPtrArray : bool ref   (* replicate pointer arrays *)

   val uninitVal : TilWord32.word

   (* special tags - with low 3 bits set to 7 *)
   val skip : int -> TilWord32.word  (* generates tag that skips n (> 0) words including tag *)
   val mirrorGlobalTag : TilWord32.word
   val stall : TilWord32.word

   (* the low 3 bits indicate the object type *)
   val record : TilWord32.word
   val wordarray : TilWord32.word
   val quadarray : TilWord32.word
   val ptrarray : TilWord32.word
   val mirrorptrarray : TilWord32.word

   (* bit offset of length and mask field *)
   val word_array_len_offset : int        (* measured in bytes *)
   val quad_array_len_offset : int        (* measured in bytes - mult of 8 *)
   val ptr_array_len_offset  : int        (* measured in bytes - mult of 4 *)
   val mirror_ptr_array_len_offset : int  (* measured in bytes - mult of 8 *)
   val maxRecordLength : int
   val rec_len_offset : int   (* measured in words *)
   val rec_len_mask : int     (* mask to get length since length not in upper bits *)
   val rec_mask_offset : int  (* low bit corresponds to first entry in record - no masking since mask is in upper bits *)

   (* Compute array tags given in bytes *)
   val mk_word_array_tag : TilWord32.word -> TilWord32.word
   val mk_quad_array_tag : TilWord32.word -> TilWord32.word
   val mk_ptr_array_tag : TilWord32.word -> TilWord32.word
   val mk_mirror_ptr_array_tag : TilWord32.word -> TilWord32.word

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

     
