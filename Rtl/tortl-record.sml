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

    val do_constant_records = ref true
    val do_forced_constant_records = ref true
    val debug = Stats.ff("TortlRecordDebug")

    val empty_record_int = 256
    val empty_record = VALUE (TAG 0w256)
    val maxRtlRecord = maxRecordLength - 1      (* We reserve one slot from Rtl-generated record so that sums can be handled *)


  fun make_record_core (const, state, reps, terms, labopt) = 
    let 
	val _ = if (length terms > maxRtlRecord) then error "max_record_core given too maxn terms" else ()

	val is_mutable = ref false
	val _ = add_instr(ICOMMENT ("allocating " ^ (Int.toString (length terms)) ^ "-record"))
	val tagword = recordtag reps
	val dest = alloc_regi TRACE

	val tagwords = 
	    if (not (!HeapProfile))
		then [tagword]
	    else [{dynamic=nil,static=MakeProfileTag()}, tagword]
        (* total number of words needed *)
	val words_alloced = length terms+length tagwords


	(* shadow heapptr with thunk to prevent accidental use *)
	val (heapptr,state) = 
	    if const
		then let fun f _ = error "should not use heapptr here"
		     in (add_data(COMMENT "static record tag");
			 (f, state))
		     end
	    else let val state = needgc(state,IMM(words_alloced))
		 in  (fn _ => heapptr, state)
		 end

	fun storenew(base,offset,r,rep) = 
	    (case rep of
		 TRACE => add_instr(INIT(EA(base,offset),r,NONE))
	       | NOTRACE_INT => add_instr(STORE32I(EA(base,offset),r))
	       | NOTRACE_CODE => add_instr(STORE32I(EA(base,offset),r))
	       | NOTRACE_LABEL => add_instr(STORE32I(EA(base,offset),r))
	       | COMPUTE path => let val isPointer = repPathIsPointer path
				 in  add_instr(INIT(EA(base,offset),r,SOME isPointer))
				 end
	       | _ => error "storenew got funny rep")

	fun scan_vals (offset,_,[]) = offset
	  | scan_vals (offset,[],vl::vls) = error "not enough reps"
	  | scan_vals (offset,rep::reps,vl::vls) =
	    ((case (const,vl) of
		  (true, VALUE (INT w32)) => add_data(INT32 w32)
		| (true, VALUE (TAG w32)) => add_data(INT32 w32)
		| (true, VALUE (RECORD (l,_))) => add_data(DATA l)
		| (true, VALUE (LABEL l)) => add_data(DATA l)
		| (true, VALUE (CODE l)) => add_data(DATA l)
		| (true, VALUE (REAL l)) => error "make_record_core given REAL"
		| (true, VALUE (VOID _)) => error "make_record_core given VOID"
		| _ => let val r = load_ireg_term(vl,NONE)
		       in  if const 
			       then 
				   let val fieldl = fresh_data_label "location"
				       val addr = alloc_regi NOTRACE_LABEL
				   in  (add_data(DLABEL fieldl);
					add_data(INT32 uninit_val);
					add_instr(LADDR(fieldl,0,addr));
					storeWithBarrier(addr, r, rep);
					())
				   end
			   else 
			       storenew(heapptr(),offset,r,rep)
		       end);
	    scan_vals(offset+4,reps,vls))

        (* sometime the tags must be computed at run-time *)
	fun do_dynamic (r,{bitpos,path}) =
	    let val isPointer = repPathIsPointer path
		val tmp = alloc_regi NOTRACE_INT
	    in  add_instr(SLL(isPointer,IMM bitpos,tmp));
		add_instr(ORB(tmp,REG r,r))
	    end

      (* usually, the tags are known at compile time *)	
      fun scantags (offset,nil : Rtltags.tag list) = offset
	| scantags (offset,({static,dynamic}::vl) : Rtltags.tag list) =
	  (if const
	       then (if (null dynamic)
			 then add_data(INT32 static)
		     else error "making constant record with dynamic tag")
	   else 
	       let val r = alloc_regi(NOTRACE_INT)
	       in  add_instr (LI(static,r));
		   app (fn a => do_dynamic(r,a)) dynamic;
		   add_instr(STORE32I(EA(heapptr(),offset),r)) (* tags *)
	       end;
	   scantags(offset+4,vl))

      val offset = 0
      val offset = scantags(offset,tagwords)
      val (result,templabelopt) = 
	  if const
	      then let val label = (case labopt of
					SOME lab => lab
				      | NONE => fresh_data_label "record")
		   in  (add_data(DLABEL label);
			(VALUE(LABEL label), SOME label))
		   end
	  else (LOCATION (REGISTER (false,I dest)), NONE)
      val offset = scan_vals (offset, reps, terms)

      (* The test for is_mutable and call to add_mutable must FOLLOW scan_vals *)
      val _ = (case (const andalso !is_mutable, templabelopt) of
		   (true, SOME label) => add_mutable label
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
	  else let val _ = print ("make_record_help encountered large record " ^ (Int.toString (length terms)) ^ "\n")
		   fun split(ls,n) = (List.take(ls,n), List.drop(ls,n))
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

  (* These are the interface functions: determines static allocation *)
  fun make_record (state, terms) = 
      let fun is_value (VALUE _) = true 
	    | is_value _ = false
	  val reps = map term2rep terms
	  fun is_static (COMPUTE _) = false
	    | is_static _ = true
	  val const = (istoplevel() orelse (andfold is_value terms)) andalso (andfold is_static reps)
	  val const = const andalso (!do_constant_records)
      in  make_record_help(const,state,reps,terms,NONE)
      end

  fun make_record_const (state, terms, labopt) = 
      let val reps = map term2rep terms
	  val res as (lv,_) = make_record_help(!do_forced_constant_records,state, reps, terms, labopt)
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
	  then add_instr(LOAD32I(EA(src,4*index), dest))
      else let val _ = print ("record_project given large index " ^ (Int.toString index) ^ "\n")
	       val tmp = alloc_regi TRACE
	       val _ = add_instr(LOAD32I(EA(src,4*(maxRtlRecord-1)), tmp))
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
	  val state = needgc(state,IMM (1+maxRecordLength)) (* one more for tag word *)

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
	  val _ = add_instr(LOAD32I(EA(record,~4),tag))
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
	  val _ = add_instr(INIT(EA(heapptr, 0), newtag, NONE))           (* write new tag *)
	  val _ = add_instr(INIT(EA(heapptr, 4), field, NONE))            (* write first field v *)

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
	  val _ = LOAD32I(EA(rTypeLoc, 4), curFieldType) (* First word is tag *)
	  val _ = LOAD32I(EA(rLoc, 0), curField)
	  val _ = add_instr(ADD(heapptr, IMM 8, destLoc))
	  val _ = add_instr(S4ADD(current, REG destLoc, destLoc))
	  val _ = STORE32I(EA(destLoc, 0), curField)
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