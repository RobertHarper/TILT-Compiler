(*$import TORTLARRAY Rtl Pprtl TortlBase Rtltags Nil NilUtil Ppnil Stats *)


structure TortlArray :> TORTL_ARRAY =

struct

    structure TortlBase = TortlBase
    open Name
    open Rtl
    open Nil
    open TortlBase
    open Rtltags
    
    val w2i = TilWord32.toInt
    val i2w = TilWord32.fromInt
    val do_writelist = ref true
    fun error s = Util.error "tortl-array" s

  (* ----------  Subscript operations ----------------- *)
  fun xsub_float (state, fs) (vl1 : loc_or_val, vl2 : loc_or_val) =
      let
	  val a' = load_ireg_locval(vl1,NONE)
	  val destf = alloc_regf()
	  val _ =  (case (in_ea_range 8 vl2) of
			SOME i => add_instr(LOADQF(EA(a',i),destf))
		      | NONE =>
			    let val b' = load_ireg_locval(vl2,NONE)
				val addr = alloc_regi (LOCATIVE)
			    in  add_instr(S8ADD(b',REG a',addr));
				add_instr(LOADQF(EA(addr,0),destf))
			    end)
      in  (VAR_LOC(VREGISTER(false, F destf)), 
	   Prim_c(Float_c fs, []), 
	   state)
      end

  fun xsub_help (state : state, is, c)  (vl1 : loc_or_val, vl2 : loc_or_val) =
      let val a' = load_ireg_locval(vl1,NONE)
	  val desti = alloc_regi(con2rep state c)
      in  (case is of
	       Prim.W32 => (add_instr(ICOMMENT "int sub start");
			    (case (in_ea_range 4 vl2) of
				SOME i => add_instr(LOAD32I(EA(a',i),desti))
			      | NONE => let val b' = load_ireg_locval(vl2,NONE)
					    val addr = alloc_regi (LOCATIVE)
					in  add_instr(S4ADD(b',REG a',addr));
					    add_instr(LOAD32I(EA(addr,0),desti))
					end);
				 add_instr(ICOMMENT "int sub end"))
	     | Prim.W8 => let val addr = alloc_regi LOCATIVE
			      val offset = load_ireg_locval(vl2,NONE) (* cannot reuse *)
			      val subaddr = alloc_regi NOTRACE_INT
			      val data = alloc_regi NOTRACE_INT
			      val temp = alloc_regi NOTRACE_INT
			      val _ = (add_instr(ICOMMENT "character sub start\n");
				       add_instr(ANDB(offset,IMM 3, subaddr));
				       add_instr(NOTB(subaddr,temp));
				       add_instr(SLL(subaddr,IMM 3, subaddr));
				       add_instr(ANDB(offset,REG temp, temp));
				       add_instr(ADD(a',REG temp,addr));
				       add_instr(LOAD32I(EA(addr,0),data));
				       add_instr(SRL(data,REG subaddr,data));
				       add_instr(ANDB(data,IMM 255, desti));
				       add_instr(ICOMMENT "character sub end\n"))
			  in ()
			  end
	     | _ => error "xintptrsub not done on all int sizes");
	  (VAR_LOC(VREGISTER(false,I desti)), c, state)
      end

  fun xsub_int (state : state, is)  (vl1 : loc_or_val, vl2 : loc_or_val) =
      let val c = Prim_c(Int_c is, [])
      in  xsub_help (state,is,c) (vl1,vl2)
      end

  fun xsub_known(state : state, c) (vl1 : loc_or_val, vl2 : loc_or_val) =
      xsub_help(state, Prim.W32,c) (vl1,vl2)


  fun xsub_dynamic(state,c, con_ir) (vl1 : loc_or_val, vl2 : loc_or_val) : loc_or_val * con * state =
      let
	  val _ = Stats.counter("Rtlxsub_dyn")()
	  fun floatcase s = let val (VAR_LOC(VREGISTER(_,F fr)),_,s) = xsub_float(state,Prim.F64) (vl1,vl2)
				val (ir,s) = boxFloat(s,fr)
			    in  (I ir, s)
			    end
	  fun nonfloatcase (s,is) = let val (VAR_LOC(VREGISTER(_, reg)),_,s) = xsub_int(s,is) (vl1,vl2)
				    in  (reg,s)
				    end
				
	  val r = con_ir
	  val tmp = alloc_regi NOTRACE_INT
	  val desti = alloc_regi(con2rep state c)
	  val afterl = fresh_code_label "sub_after"
	  val floatl = fresh_code_label "sub_float"
	  val charl = fresh_code_label "sub_char"
	  val nonfloatl = fresh_code_label "sub_nonfloat"
	  val _ = (add_instr(CMPUI(EQ, r, IMM 11, tmp));
		   add_instr(BCNDI(NE,tmp,floatl,false));
		   add_instr(CMPUI(EQ, r, IMM 0, tmp));
		   add_instr(BCNDI(NE,tmp,charl,false)))
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
      in (VAR_LOC(VREGISTER(false, I desti)), c, state)
      end
 
  (* ----------  Update operations ----------------- *)

  fun xupdate_float(state : state, fs) (vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) : loc_or_val * con * state =
      let val a' = load_ireg_locval(vl1,NONE)
	  val argf = load_freg_locval(vl3,NONE)
      in (case (in_ea_range 8 vl2) of
	      SOME i => add_instr(STOREQF(EA(a',i),argf))
	    | NONE => let val b' = load_ireg_locval(vl2,NONE)
			  val addr = alloc_regi (LOCATIVE)
		      in  add_instr(S8ADD(b',REG a',addr));
			  add_instr(STOREQF(EA(addr,0),argf))
		      end);
	  (#1 unit_vvc, #2 unit_vvc, state)
      end

  fun xupdate_int(state : state, is) (vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) : loc_or_val * con * state =
      let
	  val a' = load_ireg_locval(vl1,NONE)
	  val argi = load_ireg_locval(vl3,NONE)
      in  (case is of
	       Prim.W32 => (case (in_ea_range 4 vl2) of
				SOME i => add_instr(STORE32I(EA(a',i),argi))
			      | NONE => let val b' = load_ireg_locval(vl2,NONE)
					    val addr = alloc_regi (LOCATIVE)
					in  add_instr(S4ADD(b',REG a',addr));
					    add_instr(STORE32I(EA(addr,0),argi))
					end)
	     | Prim.W8 => let val addr = alloc_regi LOCATIVE
			      val offset = load_ireg_locval(vl2,NONE)
			      val subaddr = alloc_regi NOTRACE_INT
			      val temp = alloc_regi NOTRACE_INT
			      val temp2 = alloc_regi NOTRACE_INT
			      val data = alloc_regi NOTRACE_INT
			      val ordata = alloc_regi NOTRACE_INT
			      val mask = alloc_regi NOTRACE_INT
			      val _ = (add_instr(ICOMMENT "character update start");
				       add_instr(ANDB(offset,IMM 3, subaddr));
				       add_instr(NOTB(subaddr,temp));
				       add_instr(SLL(subaddr,IMM 3, subaddr));
				       add_instr(ANDB(offset,REG temp, temp2));
				       add_instr(ADD(a',REG temp2,addr));
				       add_instr(LOAD32I(EA(addr,0),data));
				       add_instr(LI(0w255, mask));
				       add_instr(SLL(mask,REG subaddr, mask));
				       add_instr(NOTB(mask,mask));
				       add_instr(ANDB(data,REG mask, data));
				       add_instr(SLL(argi,REG subaddr, ordata));
				       add_instr(ORB(data,REG ordata, data));
				       add_instr(STORE32I(EA(addr,0),data));
				       add_instr(ICOMMENT "character update start"))
			  in ()
			  end
	     | _ => error "xintupdate not implemented on this size");
	  (#1 unit_vvc, #2 unit_vvc, state)
      end

  fun xptrupdate(state, c) (vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) : loc_or_val * con * state =
      let
	  val base = load_ireg_locval(vl1,NONE)
	  val newval = load_ireg_locval(vl3,NONE)
      in  (case (in_imm_range_vl vl2) of
	       SOME offset => add_instr(MUTATE(EA(base,offset),newval,NONE))
	     | NONE => let val addr = alloc_regi (LOCATIVE)
			   val offset = load_ireg_locval(vl2,NONE)
		       in  add_instr(S4ADD(offset,REG base,addr));
			   add_instr(MUTATE(EA(addr,0),newval,NONE))
		       end);
	  (#1 unit_vvc, #2 unit_vvc, state)
      end

  fun xupdate_known(state, c) vlist : loc_or_val * con * state =
      let
	  val is_ptr = (case #2(simplify_type state c) of
			    Prim_c(Sum_c{totalcount,tagcount,...},_) => totalcount <> tagcount
			  | _ => true)
      in  if (!do_writelist andalso is_ptr)
	      then xptrupdate(state, c) vlist
	  else xupdate_int(state,Prim.W32) vlist
      end

  fun xupdate_dynamic(state : state,c, con_ir) 
                     (vl1 : loc_or_val, vl2 : loc_or_val, vl3 : loc_or_val) 
      : loc_or_val * con * state =
      let
	  val _ = Stats.counter("Rtlxupdate_dyn")()
	   val r = con_ir
	   val tmp = alloc_regi NOTRACE_INT
	   val afterl = fresh_code_label "update_after"
	   val floatl = fresh_code_label "update_float"
	   val intl = fresh_code_label "update_int"
	   val charl = fresh_code_label "update_char"
	   val _ = (add_instr(CMPUI(EQ, r, IMM 11, tmp));
		    add_instr(BCNDI(NE,tmp,floatl,false));
		    add_instr(CMPUI(EQ, r, IMM 2, tmp));
		    add_instr(BCNDI(NE,tmp,intl,false));
		    add_instr(CMPUI(EQ, r, IMM 0, tmp));
		    add_instr(BCNDI(NE,tmp,charl,false)))
	   val _ = xptrupdate(state,c) (vl1,vl2,vl3)
	   val _ = add_instr(BR afterl)
	   val _ = add_instr(ILABEL intl)
	   val _ = xupdate_int(state,Prim.W32) (vl1,vl2,vl3)
	   val _ = add_instr(BR afterl)
	   val _ = add_instr(ILABEL charl)
	   val _ = xupdate_int(state,Prim.W8) (vl1,vl2,vl3)
	   val _ = add_instr(BR afterl)
	   val _ = add_instr(ILABEL floatl)
	   val fr = unboxFloat(load_ireg_locval(vl3,NONE))
	   val _ = xupdate_float(state,Prim.F64) (vl1,vl2,VAR_LOC(VREGISTER (false,F fr)))
	   val _ = add_instr(ILABEL afterl)
      in (#1 unit_vvc, #2 unit_vvc, state)
      end



  (* -------------------- Length operations ------------------ *)
  fun xlen_float (state,fs) (vl : loc_or_val) : loc_or_val * con * state =
      let val dest = alloc_regi NOTRACE_INT
	  val src = load_ireg_locval(vl,NONE)
	  val _ = add_instr(LOAD32I(EA(src,~4),dest))
	  val _ = add_instr(SRL(dest,IMM real_len_offset,dest))
      in  (VAR_LOC (VREGISTER (false,I dest)), Prim_c(Int_c Prim.W32, []),state)
      end

  fun xlen_int (state,is) (vl : loc_or_val) : loc_or_val * con * state =
      let val dest = alloc_regi NOTRACE_INT
	  val src = load_ireg_locval(vl,NONE)
	  val _ = add_instr(LOAD32I(EA(src,~4),dest))
	  val offset = int_len_offset + (case is of
					     Prim.W8 => 0
					   | Prim.W16 => 1
					   | Prim.W32 => 2)
	  val _ = add_instr(SRL(dest,IMM offset,dest))
      in  (VAR_LOC (VREGISTER (false,I dest)), Prim_c(Int_c Prim.W32, []),state)
      end

  fun xlen_known (state,c) (vl : loc_or_val) : loc_or_val * con * state =
      let val dest = alloc_regi NOTRACE_INT
	  val src = load_ireg_locval(vl,NONE)
	  val _ = add_instr(LOAD32I(EA(src,~4),dest))
	  val _ = add_instr(SRL(dest,IMM (2 + int_len_offset),dest))
      in  (VAR_LOC (VREGISTER (false,I dest)), Prim_c(Int_c Prim.W32, []),state)
      end

  fun xlen_dynamic (state,c, con_ir) (vl : loc_or_val) : loc_or_val * con * state =
      let 
	  val _ = Stats.counter("Rtlxlen_dyn")()
	  val dest = alloc_regi NOTRACE_INT
	  val src = load_ireg_locval(vl,NONE)
	  val _ = add_instr(LOAD32I(EA(src,~4),dest))
	  val r = con_ir
	  val tmp = alloc_regi NOTRACE_INT
	  val afterl = fresh_code_label "length_after"
	  val floatl = fresh_code_label "length_float"
	  val charl = fresh_code_label "length_char"
	  val _ = (add_instr(CMPUI(EQ, r, IMM 11, tmp));
		   add_instr(BCNDI(NE,tmp,floatl,false));
		   add_instr(CMPUI(EQ, r, IMM 0, tmp));
		   add_instr(BCNDI(NE,tmp,charl,false));
		   add_instr(SRL(dest,IMM (2+int_len_offset),dest));
		   add_instr(BR afterl);
		   add_instr(ILABEL floatl);
		   add_instr(SRL(dest,IMM real_len_offset,dest));
		   add_instr(BR afterl);
		   add_instr(ILABEL charl);
		   add_instr(SRL(dest,IMM (int_len_offset),dest));
		   add_instr(ILABEL afterl))
      in  (VAR_LOC (VREGISTER (false,I dest)), Prim_c(Int_c Prim.W32, []),state)
      end

    (* -----------------  allocation code ---------------------- *)
    fun xarray_float (state, Prim.F32) (_, _) : loc_or_val * con * state = error "no 32-bit floats"
      | xarray_float (state, Prim.F64) (vl1, vl2) = 
	let 
	    val len = load_ireg_locval(vl1,NONE)
	    val fr = load_freg_locval(vl2,NONE)
	    val dest = alloc_regi TRACE
	    val ptag = if (!HeapProfile) then MakeProfileTag() else (i2w 0)
	    val skiptag      = alloc_regi NOTRACE_INT
	    val tag = alloc_regi(NOTRACE_INT)
	    val i = alloc_regi(NOTRACE_INT)
	    val tmp = alloc_regi(NOTRACE_INT)
	    val gctemp  = alloc_regi(NOTRACE_INT)
	    val cmptemp = alloc_regi(NOTRACE_INT)
	    val fsmall_alloc = fresh_code_label "array_float_smallalloc"
	    val fafter       = fresh_code_label "array_float_after" 
	    val fbottom      = fresh_code_label "array_float_bottom"
	    val ftop         = fresh_code_label "array_float_top"
	    (* store object tag and profile tag with alignment so that
	     raw data is octaligned;  then loop through and initialize *)

	in 
	    (* if array is too large, call runtime to allocate *)
	    add_instr(LI(0w4096,cmptemp));
	    add_instr(CMPUI(LE, len, REG cmptemp, cmptemp));
	    add_instr(BCNDI(NE,cmptemp,fsmall_alloc,true));
	    add_instr(FLOAT_ALLOC(len,fr,dest,ptag));
	    add_instr(BR fafter);
	     
	    (* inline allocation code - start by doing the tag stuff*)
	    add_instr(ILABEL fsmall_alloc);
	    add_instr(ADD(len,REG len, gctemp));
	    if (not (!HeapProfile))
		then
		    (add_instr(ADD(gctemp,IMM 3, gctemp));
		     needgc(state,REG gctemp);
		     align_odd_word();
		     mk_realarraytag(len,tag);
		     add_instr(STORE32I(EA(heapptr,0),tag)); (* store tag *)
		     add_instr(ADD(heapptr,IMM 4,dest)))
	    else
		(add_instr(ADD(gctemp,IMM 4, gctemp)));
		needgc(state,REG gctemp);
		align_even_word();
		store_tag_disp(0,ptag);                 (* store profile tag *)
		mk_realarraytag(len,tag);               
		add_instr(STORE32I(EA(heapptr,4),tag)); (* store tag *)
		add_instr(ADD(heapptr,IMM 8,dest));
		
		(* now use a loop to initialize the data portion *)
		add_instr(S8ADD(len,REG dest,heapptr));
		add_instr(LI(Rtltags.skiptag, skiptag));
		add_instr(STORE32I(EA(heapptr,0),skiptag));
		add_instr(ADD(heapptr,IMM 4,heapptr));
		add_instr(SUB(len,IMM 1,i));      (* init val *)
		add_instr(BR fbottom);             (* enter loop from bottom *)
		do_code_align();
		add_instr(ILABEL ftop);            (* loop start *)
		add_instr(S8ADD(i,REG dest,tmp));
		add_instr(STOREQF(EA(tmp,0),fr));  (* initialize value *)
		add_instr(SUB(i,IMM 1,i));
		add_instr(ILABEL fbottom);
		add_instr(BCNDI(GE,i,ftop,true));
		add_instr(ILABEL fafter);
		(VAR_LOC(VREGISTER(false, I dest)),
		 Prim_c(Array_c, [Prim_c(Float_c Prim.F64,[])]),
		 new_gcstate state)   (* after all this allocation, we cannot merge *)
	end (* end of floatcase *)


  and general_init_case(ptag : Word32.word, (* profile tag *)
			tag  : regi,        (* tag *)
			dest : regi,        (* destination register *)
			gctemp : loc_or_val,   (* number of words to increment heapptr by *)
			len : regi,         (* number of words to write *)
			v : regi,           (* write v (len) times *)
			gafter,             (* label to jump to when done *)
			isptr
			) = 
      let 
	  val skiptag      = alloc_regi NOTRACE_INT
	  val tmp          = alloc_regi NOTRACE_INT
	  val i            = alloc_regi NOTRACE_INT
	  val gbottom      = fresh_code_label "array_init_bottom"
	  val gtop         = fresh_code_label "array_init_top"
      in 
	  (if (not (!HeapProfile))
	       then
		   (add_instr(ICOMMENT "storing tag");
		    add_instr(STORE32I(EA(heapptr,0),tag)); (* allocation *)
		    add_instr(ADD(heapptr,IMM 4,dest)))
	   else
	       (store_tag_disp(0,ptag);
		add_instr(STORE32I(EA(heapptr,4),tag)); (* allocation *)
		add_instr(ADD(heapptr,IMM 8,dest))));
	       
	   (* gctemp's contents reflects the profile tag already *)
	   (case gctemp of
		   (VAR_VAL (VINT n)) => add_instr(ADD(heapptr, IMM (4*(w2i n)), heapptr))
		  | _ => let val gctemp = load_ireg_locval(gctemp,NONE)
			 in  add_instr(S4ADD(gctemp,REG heapptr,heapptr))
			 end);

	    add_instr(LI(Rtltags.skiptag, skiptag));
	    add_instr(STORE32I(EA(heapptr,0),skiptag));
	    add_instr(SUB(len,IMM 1,i));  (* init val and enter loop from bot *)
	    add_instr(BR gbottom);
	    do_code_align();
	    add_instr(ILABEL gtop);        (* top of loop *)
	    add_instr(S4ADD(i,REG dest,tmp));
	    if isptr
		then add_instr(MUTATE(EA(tmp,0),v,NONE))
	    else add_instr(STORE32I(EA(tmp,0),v)); (* allocation *)
	    add_instr(SUB(i,IMM 1,i));
	    add_instr(ILABEL gbottom);
	    add_instr(BCNDI(GE,i,gtop,true));
	    add_instr(ILABEL gafter)
      end

    and xarray_int (state,is) (vl1,vl2) : loc_or_val * con * state = 
	    let val tag = alloc_regi(NOTRACE_INT)
		val dest  = alloc_regi TRACE
		val gctemp  = alloc_regi(NOTRACE_INT)
		val cmptemp = alloc_regi(NOTRACE_INT)
		val i       = alloc_regi(NOTRACE_INT)
		val tmp     = alloc_regi(LOCATIVE)
		val vtemp = load_ireg_locval(vl2,NONE)
		val loglen = load_ireg_locval(vl1,NONE)
		val ptag = if (!HeapProfile) then MakeProfileTag() else (i2w 0)
		val _ = add_instr(ICOMMENT "initializing int/ptr array start")
		val (wordlen,v,afteropt) = 
		    (case is of
			 Prim.W8 => 
			     let val fullres = alloc_regi NOTRACE_INT
				 val endres = alloc_regi NOTRACE_INT
				 val wordlen = alloc_regi(NOTRACE_INT)
				 val shift = alloc_regi(NOTRACE_INT)
				 val _ = (add_instr(ICOMMENT "about to make tag");
					  mk_intarraytag(loglen,tag);
					  add_instr(ICOMMENT "done making tag");
					  add_instr(ADD(loglen,IMM 3,tmp));      
					  add_instr(SRL(tmp,IMM 2,wordlen));     (* wordlen = (loglen + 3)/4 *)
					  add_instr(ANDB(loglen,IMM 3,tmp));     (* tmp = loglen % 4 *)
					  add_instr(LI(0w4,shift));              (* use shift as a temp *)
					  add_instr(SUB(shift,REG tmp,tmp));
					  add_instr(ANDB(tmp,IMM 3,tmp));        (* tmp = (4 - tmp) % 4 *)
					  add_instr(SLL(tmp, IMM 3,shift));  (* shift = # of zero bits in end res *)
					  
					  add_instr(SLL(vtemp,IMM 8,fullres));
					  add_instr(ORB(fullres,REG vtemp,vtemp));
					  add_instr(SLL(vtemp,IMM 16,fullres));    
					  add_instr(ORB(fullres,REG vtemp,fullres)); (* fullres = array word *)
					  add_instr(SRL(fullres,REG shift, endres))) (* endres = last array word *)
				     
			     in  (wordlen,fullres, SOME endres)
			     end
		       | Prim.W16 => error "someday"
		       | Prim.W32 => (let val bytelen = alloc_regi NOTRACE_INT
				      in  add_instr(SLL(loglen,IMM 2, bytelen));
					  mk_intarraytag(bytelen,tag);
					  (loglen,vtemp,NONE)
				      end)
		       | Prim.W64 => error "someday")
		val gafter = fresh_code_label "array_int_after"
		val ismall_alloc = fresh_code_label "array_int_small"
		fun check() = 
		    (add_instr(LI(i2w 4096,cmptemp));
		     add_instr(CMPUI(LE, wordlen, REG cmptemp, cmptemp));
		     add_instr(BCNDI(NE,cmptemp,ismall_alloc,true));
		     add_instr(INT_ALLOC(wordlen,v,dest,ptag));
		     add_instr(BR gafter);
		     do_code_align();
		     add_instr(ILABEL ismall_alloc);
		     add_instr(CMPUI(EQ,loglen,IMM 0, gctemp));
		     add_instr(ADD(gctemp,IMM 1, gctemp));
		     add_instr(ADD(gctemp,REG wordlen, gctemp));
		     if (!HeapProfile)
			 then add_instr(ADD(gctemp, IMM 1, gctemp))
		     else ();
		     needgc(state,REG gctemp))

(*
		val state = 
		    (case vl1 of
			 VAR_VAL(VINT sz) => 
			     if (TilWord32.ult(sz,TilWord32.fromInt 6))
				 then (add_instr(LI(0w8,gctemp));
				       needgc(state,IMM 8))
			     else check()
		       | _ => check())
*)
		val _ = check()
		val state = (general_init_case(ptag,tag,dest,
					       VAR_LOC(VREGISTER(false,I gctemp)),
					       wordlen,v,gafter,false);
			     (case afteropt of
				  NONE => ()
				| SOME ir => (add_instr(SUB(wordlen,IMM 1,i));
					      add_instr(S4ADD(i,REG dest,tmp));
					      add_instr(STORE32I(EA(tmp,0),ir))));
				  add_instr(ICOMMENT "initializing int/ptr array end");
				  new_gcstate state)   (* after all this allocation, we cannot merge *)
	    in  (VAR_LOC(VREGISTER(false, I dest)),
		 Prim_c(Array_c, [Prim_c(Int_c is,[])]),
		 state)
	    end

     fun xarray_ptr (state,c) (vl1,vl2) : loc_or_val * con * state = 
	    let val tag = alloc_regi(NOTRACE_INT)
		val dest = alloc_regi TRACE
		val gctemp  = alloc_regi(NOTRACE_INT)
		val cmptemp = alloc_regi(NOTRACE_INT)
		val i       = alloc_regi(NOTRACE_INT)
		val tmp     = alloc_regi(LOCATIVE)
		val ptag = if (!HeapProfile) then MakeProfileTag() else (i2w 0)
		val len = load_ireg_locval(vl1,NONE)
		val v = load_ireg_locval(vl2,NONE)
		val gafter = fresh_code_label "array_ptr_aftert"
		val psmall_alloc = fresh_code_label "array_ptr_alloc"
		val state = new_gcstate state
	    in  (add_instr(LI((i2w 4096),cmptemp));
		 add_instr(CMPUI(LE, len, REG cmptemp, cmptemp));
		 add_instr(BCNDI(NE,cmptemp,psmall_alloc,true));
		 add_instr(PTR_ALLOC(len,v,dest,ptag));
		 add_instr(BR gafter);
		 do_code_align();
		 add_instr(ILABEL psmall_alloc);
		 add_instr(CMPUI(EQ,len,IMM 0, gctemp));
		 add_instr(ADD(gctemp,IMM 1, gctemp));
		 add_instr(ADD(gctemp,REG len, gctemp));
		 if (!HeapProfile)
		     then add_instr(ADD(gctemp, IMM 1, gctemp))
		 else ();
		     needgc(state,REG gctemp);
		     mk_ptrarraytag(len,tag);
		     general_init_case(ptag,tag,dest,
				       VAR_LOC(VREGISTER(false,I gctemp)),
				       len,v,gafter,true);
		     (* after all this allocation, we cannot merge *)
		     (VAR_LOC(VREGISTER(false, I dest)),
		      Prim_c(Array_c, [c]),
		      new_gcstate state))
	    end

  fun xarray_known(state, c) vl_list : loc_or_val * con * state =
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
 and xarray_dynamic (state,c, con_ir) (vl1 : loc_or_val, vl2 : loc_or_val) : loc_or_val * con * state =
    let 
	val _ = Stats.counter("Rtlxarray_dyn")()
	val dest = alloc_regi TRACE
	val r = con_ir
	val tmp = alloc_regi NOTRACE_INT
	val afterl = fresh_code_label "array_after"
	val floatl = fresh_code_label "array_float"
	val intl = fresh_code_label "array_int"
	val charl = fresh_code_label "array_char"
	val _ = (add_instr(CMPUI(EQ, r, IMM 11, tmp));
		 add_instr(BCNDI(NE,tmp,floatl,false));
		 add_instr(CMPUI(EQ, r, IMM 2, tmp));
		 add_instr(BCNDI(NE,tmp,intl,false));
		 add_instr(CMPUI(EQ, r, IMM 0, tmp));
		 add_instr(BCNDI(NE,tmp,charl,false)))
	val ptr_state = 
	    let val (VAR_LOC(VREGISTER(_, I tmp)),_,state) = xarray_ptr(state,c) (vl1,vl2)
		val _ = add_instr(MV(tmp,dest))
	    in  state 
	    end
	val _ = add_instr(BR afterl)
	    
	val _ = add_instr(ILABEL intl)
	val int_state = 
	    let val (VAR_LOC(VREGISTER(_, I tmp)),_,state) = xarray_int(state,Prim.W32) (vl1,vl2)
		val _ = add_instr(MV(tmp,dest))
	    in  state 
	    end
	val _ = add_instr(BR afterl)
	    
	val _ = add_instr(ILABEL charl)
	val char_state = 
	    let val (VAR_LOC(VREGISTER(_, I tmp)),_,state) = xarray_int(state,Prim.W8) (vl1,vl2)
		val _ = add_instr(MV(tmp,dest))
	    in  state 
	    end
	val _ = add_instr(BR afterl)
	    
	val _ = add_instr(ILABEL floatl)
	val temp = load_ireg_locval(vl2,NONE)
	val fr = alloc_regf()
	val _ = add_instr(LOADQF(EA(temp,0),fr))
	    
	val float_state = 
	    let val vl2 = VAR_LOC(VREGISTER(false, F fr))
		val (VAR_LOC(VREGISTER(_, I tmp)),_,state) = xarray_float(state,Prim.F64) (vl1,vl2)
		val _ = add_instr(MV(tmp,dest))
	    in  state
	    end
	val _ = add_instr(ILABEL afterl)
	val state = join_states [float_state, int_state, char_state, ptr_state]
    in  (VAR_LOC (VREGISTER (false,I dest)), Prim_c(Array_c, [c]),state)
    end




end