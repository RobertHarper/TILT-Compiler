functor Rtltags(structure Rtl : RTL) : RTLTAGS =
struct

    structure Rtl = Rtl
  
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

   type tags = {static : Word32.word,
		dynamic : {bitpos : int,
			   path : Rtl.rep_path} list} list

(* Note that this must agree with the Runtime's notion of tags *)
    val skiptag = i2w 2
    val max_rec_len = 24	
    val record =    i2w 0
    val subrecord = i2w 1
    val intarray =  i2w 4
    val realarray = i2w 5
    val ptrarray =  i2w 6

    val real_len_offset = 6
    val int_len_offset = 3
    val ptr_len_offset = 5

    fun realarraytag len =
	bitor(bitshift(len,real_len_offset),realarray)

    fun intarraytag len =
	bitor(bitshift(len,int_len_offset),intarray)

    fun ptrarraytag len =
	bitor(bitshift(len,int_len_offset),ptrarray)

    fun rawstringtag (len : int) =
	let val wordlen = (len+3) div 4
        in intarraytag (i2w wordlen)
        end

    fun recordtag(flaglist) = 
      let 
	  fun pp_flag TRACE = print "TRACE, "
	    | pp_flag UNSET = print "UNSET, "
	    | pp_flag NOTRACE_INT = print "NOTRACE_INT, "
	    | pp_flag NOTRACE_CODE = print "NOTRACE_CODE, "
	    | pp_flag NOTRACE_REAL = print "NOTRACE_REAL, "
	    | pp_flag LABEL = print "LABEL, "
	    | pp_flag LOCATIVE = print "LOCATIVE, "
	    | pp_flag (COMPUTE _) = print "COMPUTE, "

	fun split _ [] = (nil,nil)
	  | split 0 r  = (nil,r)
	  | split n (a::b) = (case (split (n-1) b) of (x,y) => (a::x,y))
	fun part arg = 
	  case (split max_rec_len arg) of
	    (a,[]) => [(false,a)]
	  | (a,b) => (true,a)::(part b)
	fun tagger (isfirst,(notlast,flags)) = 
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
	      | stat_loop (LABEL :: rest)  = 0 + 2 * (stat_loop rest)
	      | stat_loop (TRACE :: rest)  = 1 + 2 * (stat_loop rest)

	    fun dyn_loop ([],pos)          = []
	      | dyn_loop (COMPUTE p :: rest,pos) =
		          {bitpos=pos,path=p} :: dyn_loop(rest,pos+1)
	      | dyn_loop (_ :: rest,pos)  = dyn_loop(rest,pos+1)

	    val static_mask = stat_loop flags
	    val compute_mask = dyn_loop (flags,4)
	    val lenval = 
	      if (length flags < max_rec_len) 
		then (length flags) 
	      else 
		if (notlast)
		  then 31
		else max_rec_len
	    val tagval = if (isfirst) then record else subrecord
	  in
	     {static=bitor(bitor(tagval,bitshift(i2w static_mask,3)),
			   bitshift(i2w (lenval),27)),
	      dynamic = compute_mask}
	  end
      in
	map tagger (case (part flaglist) of
		      (a::b) => (true,a) :: (map (fn arg => (false,arg)) b)
			| _ => error "tags.sml: bad top case")
      end
end

