(*$import Rtl Pprtl Rtltags TortlBase Nil TORTLRECORD Stats *)

structure TortlRecord :> TORTL_RECORD =
struct

   (* Module-level declarations *)

    open Util Listops
    open Name 
    open Rtl
    open Pprtl 
    open Rtltags 
    open TortlBase
	
    fun error s = Util.error "tortl-record.sml" s
    structure TW32 = TilWord32
    structure TW64 = TilWord64

    val do_reject_nonValue = Stats.tt("Reject_NonValue")
    val debug = Stats.ff("TortlRecordDebug")

    val empty_record_int = 256
    val empty_record = VALUE (TAG 0w256)
    val maxRtlRecord = maxRecordLength - 1      (* We reserve one slot from Rtl-generated record so that sums can be handled *)



  fun make_record_core (const, state, reps, terms, labopt) = 
    let 
	val numTerms = length terms
	val _ = if (numTerms > maxRtlRecord) 
		    then error "max_record_core given too maxn terms" else ()
	val _ = add_instr(ICOMMENT ("allocating " ^ (Int.toString (length terms)) ^ "-record"))

	val heapCount = ref 0
	val nonHeapCount = ref 0
	val dest = alloc_regi TRACE

	val tagwords = [recordtag reps]

        (* total number of words needed *)
	val words_alloced = numTerms + length tagwords

	(* shadow heapptr with thunk to prevent accidental use *)
	val (heapptr,state) = 
	    if const
		then let fun f _ = error "should not use heapptr here"
		     in (add_data(COMMENT "static record tag");
			 (f, state))
		     end
	    else let val state = needalloc(state,IMM(words_alloced))
		 in  (fn _ => heapptr, state)
		 end

	(* use if statically allocated *)
	val recordLabel = (case labopt of
			       NONE => fresh_data_label "record" 
			     | SOME l => l)
	val staticComponents = (Listops.andfold (fn VALUE _ => true
						 | _ => false) terms) andalso
	                       (Listops.andfold (fn {dynamic=[],...} => true
						  | _ => false) tagwords)
	fun storenew(ea,r,rep) = 
	    (case rep of
		 TRACE => add_instr(STORE32I(ea,r))
	       | NOTRACE_INT => add_instr(STORE32I(ea,r))
	       | NOTRACE_CODE => add_instr(STORE32I(ea,r))
	       | NOTRACE_LABEL => add_instr(STORE32I(ea,r))
	       | COMPUTE path => let val isPointer = repPathIsPointer path
				 in  add_instr(STORE32I(ea,r))
				 end
	       | _ => error "storenew got funny rep")

	(* Offset starts from the point of allocation not from start of the object *)
	fun scan_vals (offset,_,[]) = offset
	  | scan_vals (offset,[],vl::vls) = error "not enough reps"
	  | scan_vals (offset,rep::reps,vl::vls) =
	    (if const
		 then (case vl of
			   VALUE v => (nonHeapCount := 1 + !nonHeapCount;
				       (case v of
					    (INT w32) => add_data(INT32 w32)
					  | (TAG w32) => add_data(INT32 w32)
					  | (RECORD (l,_)) => add_data(DATA l)
					  | (LABEL l) => add_data(DATA l)
					  | (CODE l) => add_data(DATA l)
					  | (REAL l) => error "make_record_core given REAL"
					  | (VOID _) => error "make_record_core given VOID"))
			 | _ => let val nonheap = repIsNonheap rep
				    val _ = if nonheap
						then nonHeapCount := 1 + !nonHeapCount
					    else heapCount := 1 + !heapCount;
				    val r = load_ireg_term(vl,NONE)
				in  add_data(INT32 uninitVal);
				    add_instr(STORE32I(LEA(recordLabel, 
							   offset - 4 * (length tagwords)), r))
				end)
	     else let val r = load_ireg_term(vl,NONE)
		  in  storenew(REA(heapptr(),offset),r,rep)
		  end;
	     scan_vals(offset+4,reps,vls))

	(* usually, the tags are known at compile time *)	
	fun scantags (offset,nil : Rtltags.tag list) = offset
	  | scantags (offset,({static,dynamic}::vl) : Rtltags.tag list) =
	    (if const
		 then (if staticComponents
			   then (if (null dynamic)
				     then add_data(INT32 static)
				 else error "making constant record with dynamic tag")
		       else let val _ = add_data(INT32 (Rtltags.skip (1 + numTerms)))
				val tag = alloc_regi(NOTRACE_INT)
			    in  add_instr (LI(static,tag));
				add_dynamic(tag,dynamic);
				add_instr(STORE32I(LEA(recordLabel,
						       offset - 4 * (length tagwords)),tag))
			  end)
	   else 
	       let val r = alloc_regi(NOTRACE_INT)
	       in  add_instr (LI(static,r));
		   add_dynamic(r,dynamic);
		   add_instr(STORE32I(REA(heapptr(),offset),r))
	       end;
	   scantags(offset+4,vl))

      val offset = 0
      val offset = scantags(offset,tagwords)
      val (result,templabelopt) = 
	  if const
	      then (add_data(DLABEL recordLabel);
		    (VALUE(LABEL recordLabel), SOME recordLabel))
	  else (LOCATION (REGISTER (false,I dest)), NONE)
      val offset = scan_vals (offset, reps, terms)

      (* The values of nonHeapCount and heapCount must be accessed only after scan_vals *)
      val _ = (case (const, templabelopt) of
		   (true, SOME label) => add_static_record (label, !nonHeapCount, !heapCount)
		 | (true, NONE) => error "impossible control flow"
		 | _ => ())
      val _ = if const
		  then ()
	      else (add(heapptr(),4 * length tagwords,dest);
		    add(heapptr(),4 * words_alloced,heapptr()))
      val _ = add_instr(ICOMMENT ("done allocating " ^ (Int.toString (length terms)) ^ " record"))
    in  (result, state)
    end

  fun make_record_help (const, state, reps, [], _) = (empty_record, state)
    | make_record_help (const, state, reps, terms, labopt) =
      let fun loop (state,reps,terms,labopt) =
	  if (length terms < maxRtlRecord)
	      then make_record_core(const, state, reps, terms, labopt)
	  else let fun split(ls,n) = (List.take(ls,n), List.drop(ls,n))
		   val (terms1,terms2) = split(terms,maxRtlRecord-1)
		   val (reps1,reps2) = split(reps,maxRtlRecord-1)
		   val (record2,state) = loop (state,reps2,terms2,NONE)
		   val terms = terms1 @ [record2]
		   val reps = reps1 @ [TRACE]
	       in  make_record_core(const,state, map term2rep terms, terms, labopt)
	       end
	  fun check [] = loop (state,reps,terms,labopt)
	    | check ((VALUE (VOID _))::_) = (VALUE(VOID Rtl.TRACE), state)
	    | check (_::rest) = check rest
      in   check terms
      end

  fun is_value_nonCompute terms = 
      let val reps = map term2rep terms
	  fun loop (vFlag,ncFlag) [] = (vFlag,ncFlag,reps)
	    | loop (vFlag,ncFlag) ((term,rep)::rest) = 
	      let val vFlag = vFlag andalso (case term of
						 VALUE _ => true
					       | _ => false)
		  val ncFlag = ncFlag andalso (case rep of
						   COMPUTE _ => false
						 | _ => true)
	      in  loop (vFlag,ncFlag) rest
	      end
      in  loop (true,true) (zip terms reps)
      end

  (* These are the interface functions: determines static allocation *)
  fun make_record (state, terms) = 
      let val (allValues, allNonComputes, reps) = is_value_nonCompute terms
	  val const = allNonComputes andalso (if (!do_reject_nonValue)
						  then allValues
					      else (istoplevel() orelse allValues))
      in  make_record_help(const,state,reps,terms,NONE)
      end
  
  fun make_record_const (state, terms, labopt) = 
      let val (allValues, allNonComputes, reps) = is_value_nonCompute terms
	  val const = allNonComputes andalso (allValues orelse not (!do_reject_nonValue))
	  val res as (lv,_) = make_record_help(const,state, reps, terms, labopt)
	  val labopt2 = (case lv of
			     VALUE(RECORD(lab,_)) => SOME lab
			   | VALUE(LABEL lab) => SOME lab
			   | _ => NONE)
	  val _ = (case (labopt,labopt2) of
		       (NONE,_) => ()
		     | (SOME lab, SOME lab') =>
			   if (Rtl.eq_label(lab,lab'))
			       then () else error "make_record_const failed"
		     | _ => error "make_record_const failed")
      in  res
      end
  
  fun make_record_mutable (state, terms) = 
      let val reps = map term2rep terms
      in  make_record_help(false,state,reps,terms,NONE)
      end

  fun record_project (src, index, dest) = 
      if (index >= 0 andalso index < (maxRtlRecord-1))
	  then add_instr(LOAD32I(REA(src,4*index), dest))
      else let val tmp = alloc_regi TRACE
	       val _ = add_instr(LOAD32I(REA(src,4*(maxRtlRecord-1)), tmp))
	   in  record_project(tmp,index-(maxRtlRecord-1),dest)
	   end

  (* Record_insert takes 4 arguments: (1) a totrl state, (2) a record r : TRACE, (3) a type rt where r : rt, 
                                      (4) a value v : NOTRACE_INT
        Reutrn a record r' whose first field is v and whose remaining fields are those of r.
	Note that (length of r < maxRtlRecord) and (length of r' < maxRecord).
	Note that r may be the empty record, requiring special treatment.
  *)

  fun record_insert (state,record : regi, recType : regi, field : regi) : state * regi = 
      let
	  val state = needalloc(state,IMM (1+maxRecordLength)) (* one more for tag word *)

	  val len = alloc_regi NOTRACE_INT
	  val mask = alloc_regi NOTRACE_INT

	  (* Empty record are not pointer and have no tag *)
          val afterLenMask = fresh_code_label "afterLenMask"
	  val _ = add_instr(LI(0w0,len))
	  val _ = add_instr(LI(0w0,mask))
	  val _ = add_instr(BCNDI(EQ,record,IMM empty_record_int,afterLenMask,false))
	  (* Non-empty record required tag extraction to compute len and mask *)
	  val tag = alloc_regi NOTRACE_INT
	  val tmp = alloc_regi NOTRACE_INT
	  val _ = add_instr(LOAD32I(REA(record,~4),tag))
	  val _ = add_instr(SRL(tag, IMM rec_len_offset, tmp))
	  val _ = add_instr(ANDB(tmp, IMM rec_len_mask, len))
	  val _ = add_instr(SRL(tag, IMM rec_mask_offset, mask))
	  val _ = add_instr(ILABEL afterLenMask)

	  (* Creating the new tag - Relies on all non-pointer types represented by 3 or less *)
	  val newmask = alloc_regi NOTRACE_INT
	  val _ = add_instr(SLL(mask, IMM 1, newmask))                  (* make room at the least significant end *)
	                                                                (* new slot is for an int so leave it unset *)
	  val newlen = alloc_regi NOTRACE_INT
	  val _ = add_instr(ADD(len, IMM 1, newlen))
	  val _ = if (Rtltags.record = 0w0) then () else error "record_insert relies on record aspect being zero"
	  val newtag = alloc_regi NOTRACE_INT
	  val _ = add_instr(ORB(newmask, REG newlen, newtag))
	  val _ = add_instr(STORE32I(REA(heapptr, 0), newtag))           (* write new tag *)
	  val _ = add_instr(STORE32I(REA(heapptr, 4), field))            (* write first field v *)

	  (* Now initialize the remaining fields *)
	  val copyLoop = fresh_code_label "copyLoop"
	  val afterLoop = fresh_code_label "afterLoop"
	  val current = alloc_regi NOTRACE_INT                       (* When entering the loop, current contains 
								        one more than the index of the field in r
									we are copying *)
	  val _ = add_instr(MV(len, current))
	  val _ = add_instr(ILABEL copyLoop)
	  val _ = add_instr(BCNDI(EQ,current, IMM 0, afterLoop, false))
	  val _ = add_instr(SUB(current, IMM 1, current))
	  val rLoc = alloc_regi LOCATIVE
	  val rTypeLoc = alloc_regi LOCATIVE
	  val destLoc = alloc_regi TRACE
	  val curFieldType = alloc_regi TRACE  
	  val curField = alloc_regi (COMPUTE (Projvar_p (curFieldType, [])))
	  val _ = add_instr(S4ADD(current, REG record, rLoc))
	  val _ = add_instr(S4ADD(current, REG recType, rTypeLoc))
	  val _ = LOAD32I(REA(rTypeLoc, 4), curFieldType) (* First word is tag *)
	  val _ = LOAD32I(REA(rLoc, 0), curField)
	  val _ = add_instr(ADD(heapptr, IMM 8, destLoc))
	  val _ = add_instr(S4ADD(current, REG destLoc, destLoc))
	  val _ = STORE32I(REA(destLoc, 0), curField)
	  val _ = add_instr(BR copyLoop)
	  val _ = add_instr(ILABEL afterLoop)

	  (* Return result and update heap pointer *)
	  val dest = alloc_regi TRACE
	  val _ = add_instr(ADD(heapptr, IMM 4, dest))
	  val _ = add_instr(S4ADD(newlen, REG dest, heapptr))
      in  (state,dest)
      end

  fun record_delete (record : regi) : regi = error "record_delete not done"

end
