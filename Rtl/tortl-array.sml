(*$import TORTLARRAY Rtl Pprtl TortlBase TortlRecord Rtltags Nil NilUtil Ppnil Stats *)

(* Note that integer and floating-point array updates do not require a barrier
   except when using a concurent collector.  Also, the complexity of get_tag
   is unneeded when not using a concurrent collector. *)

structure TortlArray :> TORTL_ARRAY =

struct

    structure TortlBase = TortlBase
    open Name
    open Rtl
    open Nil
    open TortlBase TortlRecord
    open Rtltags

    val maxByteRequest = 8192
    val w2i = TilWord32.toInt
    val i2w = TilWord32.fromInt
    fun error s = Util.error "tortl-array" s

  (* ----------  Subscript operations ----------------- *)
  fun xsub_float (state, fs) (vl1 : term, vl2 : term, tr) =
      let
	  val a' = load_ireg_term(vl1,NONE)
	  val destf = alloc_regf()
	  val _ =  (case (in_ea_range 8 vl2) of
			SOME i => add_instr(LOAD64F(REA(a',i),destf))
		      | NONE =>
			    let val b' = load_ireg_term(vl2,NONE)
				val addr = alloc_regi (LOCATIVE)
			    in  add_instr(S8ADD(b',REG a',addr));
				add_instr(LOAD64F(REA(addr,0),destf))
			    end)
      in  (LOCATION(REGISTER(false, F destf)), state)
      end

  fun xsub_int (state : state, is)  (vl1 : term, vl2 : term, tr) = 
      let val a' = load_ireg_term(vl1,NONE)
	  val desti = alloc_regi (niltrace2rep state tr)
      in  add_instr(ICOMMENT "int sub start");
	  (case is of
	       Prim.W32 => (case (in_ea_range 4 vl2) of
				SOME i => add_instr(LOAD32I(REA(a',i),desti))
			      | NONE => let val b' = load_ireg_term(vl2,NONE)
					    val addr = alloc_regi (LOCATIVE)
					in  add_instr(S4ADD(b',REG a',addr));
					    add_instr(LOAD32I(REA(addr,0),desti))
					end)
	     | Prim.W8 => let val b' = load_ireg_term(vl2,NONE)
			      val addr = alloc_regi (LOCATIVE)
			  in  add_instr(ADD(b',REG a',addr));
			      add_instr(LOAD8I(REA(addr,0),desti))
			  end
	     | _ => error "xintptrsub not done on all int sizes");
	  add_instr(ICOMMENT "int sub end");
	  (LOCATION(REGISTER(false,I desti)), state)
      end

  fun xsub_known(state : state, c) (vl1 : term, vl2 : term, tr) =
      let val desti = alloc_regi (niltrace2rep state tr)
	  val _ = add_instr(ICOMMENT "ptr sub start")
	  val ar = load_ireg_term(vl1,NONE)
      in  (case (in_ea_range 4 vl2) of
	       SOME i => add_instr(LOAD32I(REA(ar,i),desti))
	     | NONE => let val b' = load_ireg_term(vl2,NONE)
			   val addr = alloc_regi (LOCATIVE)
		       in  add_instr(S4ADD(b',REG ar,addr));
			   add_instr(LOAD32I(REA(addr,0),desti))
		       end);
	  add_instr(ICOMMENT "ptr sub end");
	  (LOCATION(REGISTER(false,I desti)), state)
      end

  fun xsub_dynamic(state,c, con_ir) (vl1 : term, vl2 : term, tr) : term * state =
      let
	  val _ = Stats.counter("Rtlxsub_dyn")()
	  fun floatcase s = let val (LOCATION(REGISTER(_,F fr)),s) = xsub_float(state,Prim.F64) 
								(vl1,vl2,Nil.TraceKnown TraceInfo.Notrace_Real)
				val (ir,s) = boxFloat(s,fr)
			    in  (I ir, s)
			    end
	  fun nonfloatcase (s,is) = let val (LOCATION(REGISTER(_, reg)),s) = xsub_int(s,is) (vl1,vl2,tr)
				    in  (reg,s)
				    end
				
	  val r = con_ir
	  val tmp = alloc_regi NOTRACE_INT
	  val desti = alloc_regi(niltrace2rep state tr)
	  val afterl = fresh_code_label "sub_after"
	  val floatl = fresh_code_label "sub_float"
	  val charl = fresh_code_label "sub_char"
	  val nonfloatl = fresh_code_label "sub_nonfloat"
	  val _ = (add_instr(BCNDI(EQ, r, IMM 11, floatl, false));
		   add_instr(BCNDI(EQ, r, IMM 0, charl, false)))
	  val _ = add_instr(ILABEL nonfloatl)
	  val (I desti,w32_state) = nonfloatcase(state, Prim.W32)
	  val _ = add_instr(BR afterl)

	  val _ = add_instr(ILABEL charl)
	  val (I desti8,w8_state) = nonfloatcase(state, Prim.W8)
	  val _ = add_instr(MV(desti8,desti))
	  val _ = add_instr(BR afterl)

	  val _ = add_instr(ILABEL floatl)
	  val (I boxi,float_state) = floatcase state

	  val _ = (add_instr(MV(boxi,desti));
		   add_instr(ILABEL afterl))
	  val state = join_states[w8_state,w32_state,float_state]
      in (LOCATION(REGISTER(false, I desti)), state)
      end
 
  (* ----------  Update operations ----------------- *)

  fun xupdate_float(state : state, fs) (vl1 : term, vl2 : term, vl3 : term) : term * state =
      let val _ = incMutate()
	  val state = if (!do_fullWriteBarrier)
			  then needmutate(state,1)
		      else state
	  val obj = load_ireg_term(vl1,NONE)
	  val newVal = load_freg_term(vl3,NONE)
	  val ea = (case (in_ea_range 8 vl2) of
			SOME i => REA(obj, i)
		      | NONE => let val index = load_ireg_term(vl2,NONE)
				    val byteDisp = alloc_regi NOTRACE_INT
				    val _ = add_instr(SLL(index, IMM 3, byteDisp))
				in  RREA(obj, byteDisp)
				end)
	  val _ = if (!do_fullWriteBarrier)
		      then add_instr(STOREMUTATE ea)
		  else ()
	  val _ = add_instr(STORE64F(ea, newVal))
      in  (empty_record, state)
      end

  fun xupdate_int(state : state, is) (vl1 : term, vl2 : term, vl3 : term) : term * state =
      let val _ = incMutate()
	  val state = if (!do_fullWriteBarrier)
			  then needmutate(state,1)
		      else state
	  val obj = load_ireg_term(vl1,NONE)
	  val newVal = load_ireg_term(vl3,NONE)
	  val _ = 
	      (case is of
		   Prim.W32 => 
		       (case (in_ea_range 4 vl2) of
			    SOME i => (if (!do_fullWriteBarrier)
					   then add_instr(STOREMUTATE (REA(obj,i)))
				       else ();
				       add_instr(STORE32I(REA(obj,i),newVal)))
			  | NONE => let val index = load_ireg_term(vl2,NONE)
					val byteDisp = alloc_regi NOTRACE_INT
					val _ = add_instr(SLL(index, IMM 2, byteDisp))
				    in  if (!do_fullWriteBarrier)
					    then add_instr(STOREMUTATE (RREA(obj,byteDisp)))
					else ();
					add_instr(STORE32I(RREA(obj,byteDisp),newVal))
				    end)
		 | Prim.W8 => 
			    let val index = load_ireg_term(vl2,NONE)
				val byteDisp = index
			    in  if (!do_fullWriteBarrier)
				    then add_instr(STOREMUTATE(RREA(obj,byteDisp)))
				else ();
				add_instr(STORE8I(RREA(obj,byteDisp),newVal))
			    end
		 | _ => error "xintupdate not implemented on this size")
      in  (empty_record, state)
      end

  fun xptrupdate(state, c) (vl1 : term, vl2 : term, vl3 : term) : term * state =
      let val _ = incMutate()
	  val state = if (!do_writeBarrier)
			  then needmutate(state,1)
		      else state
	  val obj = load_ireg_term(vl1,NONE)
	  val newVal = load_ireg_term(vl3,NONE)
	  val ea = (case (in_ea_range 4 vl2) of
	       SOME offset => REA(obj, offset)
	     | NONE => let val index = load_ireg_term(vl2,NONE)
			   val byteOffset = alloc_regi NOTRACE_INT
			   val _ = add_instr(SLL(index, IMM 2, byteOffset))
		       in  RREA(obj, byteOffset)
		       end)
	  val _ = if (!do_writeBarrier)
		      then add_instr(STOREMUTATE ea)
		  else ()
	  val _ = add_instr(STORE32I(ea, newVal))
      in  (empty_record, state)
      end

  fun xupdate_known(state, c) vlist : term * state =
      let
	  val is_ptr = (case #2(simplify_type state c) of
			    Prim_c(Sum_c{totalcount,tagcount,...},_) => totalcount <> tagcount
			  | _ => true)
      in  if is_ptr
	      then xptrupdate(state, c) vlist
	  else xupdate_int(state,Prim.W32) vlist
      end

  fun xupdate_dynamic(state : state,c, con_ir) 
                     (vl1 : term, vl2 : term, vl3 : term) 
      : term * state =
      let
	  val _ = Stats.counter("Rtlxupdate_dyn")()
	   val r = con_ir
	   val tmp = alloc_regi NOTRACE_INT
	   val afterl = fresh_code_label "update_after"
	   val floatl = fresh_code_label "update_float"
	   val intl = fresh_code_label "update_int"
	   val charl = fresh_code_label "update_char"
	   val _ = (add_instr(BCNDI(EQ, r, IMM 11, floatl, false));
		    add_instr(BCNDI(EQ, r, IMM 2, intl, false));
		    add_instr(BCNDI(EQ, r, IMM 0, charl, false)))
	   val _ = xptrupdate(state,c) (vl1,vl2,vl3)
	   val _ = add_instr(BR afterl)
	   val _ = add_instr(ILABEL intl)
	   val _ = xupdate_int(state,Prim.W32) (vl1,vl2,vl3)
	   val _ = add_instr(BR afterl)
	   val _ = add_instr(ILABEL charl)
	   val _ = xupdate_int(state,Prim.W8) (vl1,vl2,vl3)
	   val _ = add_instr(BR afterl)
	   val _ = add_instr(ILABEL floatl)
	   val fr = unboxFloat(load_ireg_term(vl3,NONE))
	   val _ = xupdate_float(state,Prim.F64) (vl1,vl2,LOCATION(REGISTER (false,F fr)))
	   val _ = add_instr(ILABEL afterl)
      in  (empty_record, state)
      end


  fun get_tag (vl : term) : regi = 
      let val obj = load_ireg_term(vl,NONE)
	  val tag = alloc_regi NOTRACE_INT
	  val low2 = alloc_regi NOTRACE_INT
	  val load_nonstall = fresh_code_label "load_nonstall_tag"
	  val load_tag = fresh_code_label "load_true_tag"
	  val done = fresh_code_label "loaded_tag"
	  val _ = add_instr(ILABEL load_nonstall)
	  val _ = add_instr(LOAD32I(REA(obj,~4),tag))
	  val _ = add_instr(BCNDI(EQ,tag,IMM (TilWord32.toInt stall),load_nonstall,true))
	  val _ = add_instr(ILABEL load_tag)
	  val _ = add_instr(ANDB(tag, IMM 3, low2))
	  val _ = add_instr(BCNDI(NE,low2,IMM 0,done,true))        (* if low 2 bits not zero, then not a forwarding pointer *)
	  val _ = add_instr(LOAD32I(REA(tag,~4),tag))              (* tag is actually a forwarding pointer so load true tag *)
	  val _ = add_instr(BR load_tag)                           (* must branch back in case of multiple forwarding as in gen. collector *)
	  val _ = add_instr(ILABEL done)
      in  tag
      end

  (* -------------------- Length operations ------------------ *)
  fun xlen_float (state,fs) (vl : term) : term * state =
      let val tag = get_tag vl
	  val len = alloc_regi NOTRACE_INT
	  val _ = add_instr(SRL(tag,IMM real_len_offset,len))
      in  (LOCATION (REGISTER (false,I len)), state)
      end

  fun xlen_int (state,is) (vl : term) : term * state =
      let val tag = get_tag vl
	  val len = alloc_regi NOTRACE_INT
	  val offset = int_len_offset + (case is of
					     Prim.W8 => 0
					   | Prim.W16 => 1
					   | Prim.W32 => 2)
	  val _ = add_instr(SRL(tag,IMM offset,len))
      in  (LOCATION (REGISTER (false,I len)), state)
      end

  fun xlen_known (state,c) (vl : term) : term * state =
      let val tag = get_tag vl
	  val len = alloc_regi NOTRACE_INT
	  val _ = add_instr(SRL(tag,IMM (2 + int_len_offset),len))
      in  (LOCATION (REGISTER (false,I len)), state)
      end

  fun xlen_dynamic (state,c,con_ir) (vl : term) : term * state =
      let 
	  val _ = Stats.counter("Rtlxlen_dyn")()
	  val tag = get_tag vl
	  val len = alloc_regi NOTRACE_INT
	  val tmp = alloc_regi NOTRACE_INT
	  val afterl = fresh_code_label "length_after"
	  val floatl = fresh_code_label "length_float"
	  val charl = fresh_code_label "length_char"
	  val _ = (add_instr(BCNDI(EQ, con_ir, IMM 11, floatl, false));
		   add_instr(BCNDI(EQ, con_ir, IMM 0, charl, false));
		   add_instr(SRL(tag,IMM (2+int_len_offset),len));
		   add_instr(BR afterl);
		   add_instr(ILABEL floatl);
		   add_instr(SRL(tag,IMM real_len_offset,len));
		   add_instr(BR afterl);
		   add_instr(ILABEL charl);
		   add_instr(SRL(tag,IMM (int_len_offset),len));
		   add_instr(ILABEL afterl))
      in  (LOCATION (REGISTER (false,I len)), state)
      end

    (* -----------------  allocation code ---------------------- *)

    fun xarray_float (state, Prim.F32) (_, _) : term * state = error "no 32-bit floats"
      | xarray_float (state, Prim.F64) (vl1, vl2opt) = (* logical length, float value *)
	let 
	    val len = load_ireg_term(vl1,NONE)
	    val dest = alloc_regi TRACE
	    val skiptag = alloc_regi NOTRACE_INT
	    val tag = alloc_regi(NOTRACE_INT)
	    val i = alloc_regi(NOTRACE_INT)
	    val tmp = alloc_regi(NOTRACE_INT)
	    val gctemp  = alloc_regi(NOTRACE_INT)
	    val fsmall_alloc = fresh_code_label "array_float_smallalloc"
	    val fafter       = fresh_code_label "array_float_after" 
	    val fbottom      = fresh_code_label "array_float_bottom"
	    val ftop         = fresh_code_label "array_float_top"
	    (* store object tag with alignment so that
	     raw data is octaligned;  then loop through and initialize *)

	    val fr = (case vl2opt of
			  SOME vl2 => load_freg_term(vl2,NONE)
			| NONE => alloc_regf()) 
	    
	in  add_instr(ADD(len,REG len, gctemp));
	    add_instr(ADD(gctemp, IMM 2, gctemp));     (* gctemp is # words needed - one for tag, one for alignment *)

	    (* if array is too large, call runtime to allocate *)
	    let	val tmp' = alloc_regi(NOTRACE_INT)
	    in	add_instr(LI(TilWord32.fromInt (maxByteRequest div 4),tmp'));  (* gctemp in words *)
		add_instr(BCNDI(LE, gctemp, REG tmp', fsmall_alloc, true))
	    end;
	    (* call the runtime to allocate large array *)
	    add_instr(CALL{call_type = C_NORMAL,
			   func = LABEL' (ML_EXTERN_LABEL "alloc_bigfloatarray"),
			   args = [I len,F fr],
			   results = [I dest],
			   save = getLocals()});
	    add_instr(BR fafter);
	     
	    (* inline allocation code - start by doing the tag stuff*)
	    add_instr(ILABEL fsmall_alloc);

	    needalloc(state,REG gctemp);
	    align_odd_word();
	    mk_realarraytag(len,tag);
	    add_instr(STORE32I(REA(heapptr,0),tag)); (* store tag *)
	    add_instr(ADD(heapptr,IMM 4,dest));
		
	    (* now use a loop to initialize the data portion *)
	    add_instr(S8ADD(len,REG dest,heapptr));
	    add_instr(LI(Rtltags.skip 0, skiptag));
	    add_instr(STORE32I(REA(heapptr,0),skiptag));
	    add_instr(ADD(heapptr,IMM 4,heapptr));
	    add_instr(SUB(len,IMM 1,i));       (* init val *)
	    add_instr(BR fbottom);             (* enter loop from bottom *)
	    add_instr(ILABEL ftop);            (* loop start *)
	    add_instr(S8ADD(i,REG dest,tmp));
	    add_instr(STORE64F(REA(tmp,0),fr));  (* initialize value *)
	    add_instr(SUB(i,IMM 1,i));
	    add_instr(ILABEL fbottom);
	    add_instr(BCNDI(GE,i,IMM 0,ftop,true));
	    add_instr(ILABEL fafter);
	    (LOCATION(REGISTER(false, I dest)),
	     new_gcstate state)   (* after all this allocation, we cannot merge *)
	end (* end of floatcase *)


  fun general_init_case(state : state,
			tag  : regi,         (* tag *)
			dest : regi,         (* destination register *)
			len : regi,          (* number of words to write *)
			v : regi,            (* word-sized value to write *)
			gafter,              (* label to jump to when done *)
			isptr
			) : state = 
      let val wordsNeeded = alloc_regi NOTRACE_INT
	  val bytesNeeded = alloc_regi NOTRACE_INT
	  val skiptag     = alloc_regi NOTRACE_INT
	  val byteOffset      = alloc_regi NOTRACE_INT
	  val loopBottom  = fresh_code_label "array_init_loopcheck"
	  val loopTop     = fresh_code_label "array_init_loopto"
	  val _ = (add_instr(LI(0w1, wordsNeeded));                    (* 1 word for tag *)
		   add_instr(ADD(wordsNeeded,REG len, wordsNeeded));   (* len words to store data of array *)
		   needalloc(state,REG wordsNeeded))
      in 
	  add_instr(ICOMMENT "storing tag");
	  add_instr(STORE32I(REA(heapptr,0),tag));           (* store tag *)
	  add_instr(LI(Rtltags.skip 0, skiptag));               (* init field 0 to handle empty arrays *)
	  add_instr(STORE32I(REA(heapptr,4),skiptag));
	  add_instr(ADD(heapptr,IMM 4,dest));            (* compute final array *)
	  add_instr(SLL(wordsNeeded,IMM 2,bytesNeeded));
	  add_instr(ADD(heapptr,REG bytesNeeded,heapptr));      (* update heap pointer including possible padding *)
	  add_instr(SUB(len,IMM 1,byteOffset));                 (* initialize byteOffset *)
	  add_instr(SLL(byteOffset,IMM 2,byteOffset));
	  add_instr(BR loopBottom);                             (* enter loop from the bottom *)
	  add_instr(ILABEL loopTop);                            (* beginning of loop *)
	  add_instr(STORE32I(RREA(dest,byteOffset),v));	        (* initialize field *)
	  add_instr(SUB(byteOffset,IMM 4,byteOffset));          (* decrement byte offset *)
	  add_instr(ILABEL loopBottom);
	  add_instr(BCNDI(GE,byteOffset,IMM 0,loopTop,true));   (* check if byte offset still positive *)
	  add_instr(ILABEL gafter);
	  new_gcstate state                                     (* must occur after heapptr incremented *)
      end

    fun xarray_int (state,is) (vl1,vl2opt) : term * state = 
	    let val dest  = alloc_regi TRACE
		val i       = alloc_regi(NOTRACE_INT)
		val initv = 
		    let val vl2 = (case vl2opt of
				       NONE => VALUE(INT 0w0)
				     | SOME vl2 => vl2)
		    in  load_ireg_term(vl2,NONE)
		    end
		val loglen = load_ireg_term(vl1,NONE)

		val _ = add_instr(ICOMMENT "initializing int/ptr array start")
		(* bytelen - the logical length of the array in bytes which is used by alloca_bigintarray
		   wordlen - the physical length of the array in words - note padding
		   value - word value with wihch the array needs to be filled *)
		val (bytelen,wordlen,word) = 
		    (case is of
		         (* Byte arrays and vectors have the same format.  Since they are word aligned,
			    there can 1 to 3 extra bytes left over.  These should not be used by
			    the mutator and cannot be relied on by the mutator to contain anything.
			    Specifically, they are not necessarily zero.  Consequentaly, ML strings
			    which are byte vectors are not the same as C strings. To make ML strings
			    compatiable with C strings, we would not only have to null-terminate but also
			    to introduce an extra word when the logical string length is a multiple of 4. *)
			 Prim.W8 => 
			     let val word = alloc_regi NOTRACE_INT
				 val wordlen = alloc_regi NOTRACE_INT
				 val tmp     = alloc_regi NOTRACE_INT
				 val tmp2    = alloc_regi NOTRACE_INT
				 val _ = (add_instr(ADD(loglen,IMM 3,tmp));      (* tmp = loglen + 3 *)  
					  add_instr(SRL(tmp,IMM 2,wordlen));     (* wordlen = (loglen + 3)/4 *)
					  add_instr(SLL(initv,IMM 8,tmp2));      (* tmp2 = 00b0 where b is the byte *)
					  add_instr(ORB(tmp2,REG initv,tmp2));   (* tmp2 = 00bb *)
					  add_instr(SLL(tmp2,IMM 16, word));     (* word = bb00 *)
					  add_instr(ORB(word,REG tmp2,word)))    (* word = bbbb *)
			     in  (loglen,wordlen,word)
			     end
		       | Prim.W16 => error "Prim.W16 is not implemented"
		       | Prim.W32 => (let val bytelen = alloc_regi NOTRACE_INT
				      in  add_instr(SLL(loglen,IMM 2, bytelen)); (* bytelen = loglen * 4  *)
					  (bytelen,loglen,initv)
				      end)
		       | Prim.W64 => error "Prim.W64 not implemented")
		val gafter = fresh_code_label "array_int_after"
		val ismall_alloc = fresh_code_label "array_int_small"
		val _ = let val dataMax = alloc_regi(NOTRACE_INT)
			in  add_instr(LI(TilWord32.fromInt ((maxByteRequest div 4)-1),
					 dataMax));
			    add_instr(BCNDI(LE, wordlen, REG dataMax, ismall_alloc, true))
			end
		val _ = add_instr(CALL{call_type = C_NORMAL,
				    func = LABEL' (ML_EXTERN_LABEL "alloc_bigintarray"),
				    args = [I bytelen,I word],
				    results = [I dest],
				    save = getLocals()})
		val _ = add_instr(BR gafter)
		val _ = add_instr(ILABEL ismall_alloc)
		val tag = alloc_regi(NOTRACE_INT)
		val _ = mk_intarraytag(bytelen,tag)
		val state = general_init_case(state,tag,dest,
					      wordlen,word,gafter,false)

	    in  (LOCATION(REGISTER(false, I dest)), state)
	    end

     fun xarray_ptr (state,c) (vl1,vl2opt) : term * state = 
	    let val tag = alloc_regi(NOTRACE_INT)
		val dest = alloc_regi TRACE
		val i       = alloc_regi(NOTRACE_INT)
		val tmp     = alloc_regi(LOCATIVE)
		val len = load_ireg_term(vl1,NONE)
		val initv = 
		    let val vl2 = (case vl2opt of
				       NONE => VALUE(INT 0w0)
				     | SOME vl2 => vl2)
		    in  load_ireg_term(vl2,NONE)
		    end
		val gafter = fresh_code_label "array_ptr_aftert"
		val psmall_alloc = fresh_code_label "array_ptr_alloc"
		val state = new_gcstate state
		val _ = let val dataMax = alloc_regi(NOTRACE_INT)
			in  add_instr(LI(TilWord32.fromInt ((maxByteRequest div 4) - 2),
					 dataMax));
			    add_instr(BCNDI(LE, len, REG dataMax, psmall_alloc, true))
			end
		val _ = add_instr(CALL{call_type = C_NORMAL,
				       func = LABEL' (ML_EXTERN_LABEL "alloc_bigptrarray"),
				       args = [I len, I initv],
				       results = [I dest],
				       save = getLocals()})
		val _ = add_instr(BR gafter)
		val _ = add_instr(ILABEL psmall_alloc)
		val _ = mk_ptrarraytag(len,tag)
		val state = general_init_case(state,tag,dest,
					      len,initv,gafter,true)
	    in  (LOCATION(REGISTER(false, I dest)), state)
	    end

  fun xarray_known(state, c) vl_list : term * state =
      let
	  val is_ptr =
	      (case #2(simplify_type state c) of
		   Prim_c(Sum_c{totalcount,tagcount,...},_) => totalcount <> tagcount
		 | _ => true)
      in  if  is_ptr
	      then xarray_ptr(state, c) vl_list
	  else xarray_int(state,Prim.W32) vl_list
      end

  (* if we allocate arrays statically, we must add labels of pointer arrays to mutable_objects *)
 and xarray_dynamic (state,c, con_ir) (vl1 : term, vl2opt : term option) : term * state =
    let 
	val _ = Stats.counter("Rtlxarray_dyn")()
	val dest = alloc_regi TRACE
	val r = con_ir
	val tmp = alloc_regi NOTRACE_INT
	val afterl = fresh_code_label "array_after"
	val floatl = fresh_code_label "array_float"
	val intl = fresh_code_label "array_int"
	val charl = fresh_code_label "array_char"
	val _ = (add_instr(BCNDI(EQ, r, IMM 11, floatl, false));
		 add_instr(BCNDI(EQ, r, IMM 2, intl, false));
		 add_instr(BCNDI(EQ, r, IMM 0, charl, false)))
	val ptr_state = 
	    let val (term,state) = xarray_ptr(state,c) (vl1,vl2opt)
		val LOCATION(REGISTER(_, I tmp)) = term
		val _ = add_instr(MV(tmp,dest))
	    in  state 
	    end
	val _ = add_instr(BR afterl)
	    
	val _ = add_instr(ILABEL intl)
	val int_state = 
	    let val (term,state) = xarray_int(state,Prim.W32) (vl1,vl2opt)
		val LOCATION(REGISTER(_, I tmp)) = term
		val _ = add_instr(MV(tmp,dest))
	    in  state 
	    end
	val _ = add_instr(BR afterl)
	    
	val _ = add_instr(ILABEL charl)
	val char_state = 
	    let val (term,state) = xarray_int(state,Prim.W8) (vl1,vl2opt)
		val LOCATION(REGISTER(_, I tmp)) = term
		val _ = add_instr(MV(tmp,dest))
	    in  state 
	    end
	val _ = add_instr(BR afterl)
	    
	val _ = add_instr(ILABEL floatl)
	val vl2unbox_opt = 
	    (case vl2opt of
		 NONE => NONE
	       | SOME vl2 => let val fr = alloc_regf()
				 val temp = load_ireg_term(vl2,NONE)
				 val _ = add_instr(LOAD64F(REA(temp,0),fr))
			     in  SOME (LOCATION(REGISTER(false, F fr)))
			     end)
	val float_state = 
	    let 
		val (term,state) = xarray_float(state,Prim.F64) (vl1,vl2unbox_opt)
		val LOCATION(REGISTER(_, I tmp)) = term
		val _ = add_instr(MV(tmp,dest))
	    in  state
	    end
	val _ = add_instr(ILABEL afterl)
	val state = join_states [float_state, int_state, char_state, ptr_state]
    in  (LOCATION (REGISTER (false,I dest)), state)
    end




end
