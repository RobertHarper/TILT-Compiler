(*$import Prelude TopLevel Word32 TilWord32 Stats Rtl RTLTAGS Util *)

(* It is crucial that the layout of tags here matches that of the runtime.
   Compare with file Runtime/tag.h
*)
structure Rtltags :> RTLTAGS =
struct
  
   val ptrWriteBarrier = Stats.tt "PtrWriteBarrier"     (* record pointer array mutation *)
   val fullWriteBarrier = Stats.tt "FullWriteBarrier"   (* all mutations recorded *)
   val mirrorGlobal = Stats.tt "MirrorGlobal"           (* replicate pointer globals *)
   val mirrorPtrArray = Stats.ff "MirrorPtrArray"       (* replicate pointer arrays *)

   val uninitVal = 0w258 : TilWord32.word

    (* The low 3 bits of the 32-bit word describe the object type - 0w0 and 0w4 for forwarding ptrs; 0w7 for special tags *)
    val record         = 0w1 : TilWord32.word
    val wordarray      = 0w2 : TilWord32.word
    val quadarray      = 0w3 : TilWord32.word
    val ptrarray       = 0w5 : TilWord32.word
    val mirrorptrarray = 0w6 : TilWord32.word

    (* Special tags have low 3 bits set to 0w7; fourth bit is zero for skip tags *)
    fun skip numSkip    = TilWord32.orb(0w7, TilWord32.lshift(TilWord32.fromInt numSkip, 4))
    val stall           = TilWord32.orb(0w15, TilWord32.lshift(0w0, 5))
    val segstall        = TilWord32.orb(0w15, TilWord32.lshift(0w1, 5))
    val segproceed      = TilWord32.orb(0w15, TilWord32.lshift(0w2, 5))
    val mirrorGlobalTag = TilWord32.orb(0w15, TilWord32.lshift(0w3, 5))

    (* For raw(bytes), pointer(words), and real(double) arrays, 
       the upper 29 bits measure the length of the array in bytes.
       The following offsets can be used for masking in the logical lengths. *)
   val word_array_len_offset = 3        (* measured in bytes *)
   val quad_array_len_offset = 3        (* measured in bytes - mult of 8 *)
   val ptr_array_len_offset  = 3        (* measured in bytes - mult of 4 *)
   val mirror_ptr_array_len_offset = 3  (* measured in bytes - mult of 8 *)

    (* For records, bits 3 to 7 inclusive give the record length which 
       can vary from 1 to 24.  Note that the empty record is represented 
       with a small value.  Bits 8 to 31 indicate whether the fields of
       the record is a pointer or not.  If bit 8 of the header is set,
       then the first field of the record is a pointer and so on.... *)
    val rec_len_offset = 3
    val rec_len_mask = 31
    val maxRecordLength = 24
    val rec_mask_offset = 8

    open Rtl
    val error = fn s => Util.error "Rtl/tags.sml" s
    structure W = TilWord32
    val i2w = W.fromInt
    val w2i = W.toInt
    (* positive shift disp is left shift *)
    fun bitshift(v,disp) = if (disp >= 0) 
			       then W.lshift(v,disp)
			   else W.rshiftl(v,~disp)
    val bitor = W.orb

    fun mk_word_array_tag len =
	bitor(bitshift(len,word_array_len_offset),wordarray)

    fun mk_quad_array_tag len =
	bitor(bitshift(len,quad_array_len_offset),quadarray)

    fun mk_ptr_array_tag len =
	bitor(bitshift(len,ptr_array_len_offset),ptrarray)

    fun mk_mirror_ptr_array_tag len =
	bitor(bitshift(len,mirror_ptr_array_len_offset),mirrorptrarray)


    fun pp_flag TRACE = print "TRACE, "
      | pp_flag UNSET = print "UNSET, "
      | pp_flag NOTRACE_INT = print "NOTRACE_INT, "
      | pp_flag NOTRACE_CODE = print "NOTRACE_CODE, "
      | pp_flag NOTRACE_REAL = print "NOTRACE_REAL, "
      | pp_flag NOTRACE_LABEL = print "NOTRACE_LABEL, "
      | pp_flag LOCATIVE = print "LOCATIVE, "
      | pp_flag (COMPUTE _) = print "COMPUTE, "

    type tag = {static : Word32.word,
		dynamic : {bitpos : int,
			   path : Rtl.rep_path} list}

    fun recordtag flags : tag =
      let 
	val len = length flags
	fun make_tag() =
	  let 
	    fun stat_loop []          = 0
	      | stat_loop (NOTRACE_INT :: rest) = 0 + 2 * (stat_loop rest)
	      | stat_loop (NOTRACE_CODE :: rest) = 0 + 2 * (stat_loop rest)
	      | stat_loop (NOTRACE_REAL :: rest) = 
		error "can't have record with double-floats"
	      | stat_loop (LOCATIVE :: rest) = 
		error "can't have record with locatives"
	      | stat_loop (UNSET :: rest) = 
		error "can't have record with unsets"
	      | stat_loop (COMPUTE _ :: rest) = 0 + 2 * (stat_loop rest)
	      | stat_loop (NOTRACE_LABEL :: rest)  = 0 + 2 * (stat_loop rest)
	      | stat_loop (TRACE :: rest)  = 1 + 2 * (stat_loop rest)

	    fun dyn_loop ([],pos)          = []
	      | dyn_loop (COMPUTE p :: rest,pos) =
		          {bitpos=pos,path=p} :: dyn_loop(rest,pos+1)
	      | dyn_loop (_ :: rest,pos)  = dyn_loop(rest,pos+1)

	    val static_mask = stat_loop flags
	    val dynamic_mask = dyn_loop (flags,rec_mask_offset)
	    val rec_aspect = record
	    val rec_len = bitshift(i2w len,rec_len_offset)
	    val rec_mask = bitshift(i2w static_mask,rec_mask_offset)
	  in
	      {static = bitor(bitor(rec_aspect,rec_len),rec_mask),
	       dynamic = dynamic_mask}
	  end
      in  if (len = 0)
	      then error "Empty record"
	  else if (len > maxRecordLength)
	      then error "Record too long"
	  else make_tag()
      end
end

