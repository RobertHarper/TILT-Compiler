(*$import TORTLSUM Rtl Pprtl TortlBase Rtltags Nil NilUtil Ppnil Stats *)

(* ASSUMPTIONS and GUARANTEES:
	(1) Integer types are represented by the tags 0 to 3.
	(2) (Special) Sum types are represetnted by a pointer to a record whose fields are
		(a) tag 7 to indicate sum or special sum type
		(b) known - indicates the type of special sum - ~1 if not known
		(c) tagcount - number of non-value-varrying components
		(d) total - total number of components
		(e) type argument(s)
		      if there are no type arguments, then this will be an empty crecord
		      if there is one type argument, then this will be the type argument
		      if there are more than one type arguments, then this will be a pointer 
			  to the tuple of the summand types
	(3) Mu types are represented by a record whose first field is 8.	
		There are currently no other fields since mu's are not fully represented.  
	(4) The representation of values of sum and mu types will always be a pointer
	        though that might include small tag values.
*)
	
(* PROBLEMS:
        (1) dynamic code does not work for multi-records
 *)


structure TortlSum :> TORTL_SUM =

struct

    open Util Listops Name
    open Nil NilUtil
    open Rtl TortlBase Rtltags Pprtl 

    val diag = ref true
    val debug = ref false
    val error = fn s => (Util.error "tortl.sml" s)

    structure TW32 = TilWord32
    structure TW64 = TilWord64
    val w2i = TW32.toInt
    val i2w = TW32.fromInt

    type typearg = state * TilWord32.word * con
    fun help ((state,known,sumcon) : typearg) = 
	let
	  val (tagcount,_,sumtypes) = reduce_to_sum "xsum_???" state sumcon
	  val field_sub = TW32.uminus(known,tagcount)
	  val nontagcount = length sumtypes
	  val single_carrier = nontagcount = 1
	  val is_tag = TW32.ult(known,tagcount)
	in  (state,known,sumcon,
	     tagcount,nontagcount,single_carrier,is_tag,
	     sumtypes,field_sub)
	end

    fun needs_boxing state field_type = 
	(case simplify_type state field_type of
	     (true,Prim_c(Int_c _, _)) => true
	   | (true,Mu_c _) => true
	   | (true,Proj_c(Mu_c _,_)) => true
	   | (true,Prim_c(Sum_c _,_)) => true
	   | (true,Prim_c(Exntag_c, _)) => true
	   | (true,_) => false
	   | _ => error "needs_boxing: field type cannot be determined")

  (* First, we assume that datatypes have already been translated by 
     rearranging the non value-carrying components to the beginning.
     To support efficient representations, we let m and n denote
     the length of the maximal initial segment of non value-carrying components 
     and n the rest of the components.  Note that this m is
     NOT the number(k) of non-value-carrying constructors in the datatype.
     It is possible for m > k. There are then 6 cases to consider:

         1. m=0, n=0    we use a tag to represent the datatype
         2. m<>0, n=0   we use tags to represent the datatype
         3. m=0, n=1    we use the argument type to represent the datatype
         4. m<>0, n=1   we use tag or pointer to represent the datatype
         5. m<>0, n>1   we use tag or tagged sum to represent the datatype
         6. m=0, n>1    we use tagged sum to represent the datatype

     Tagged sums are flattened if the carried type is a record type.

  *)

    (* (1) Falls through to nobox case 
       (2) con_ir must be a sum-type with one carrier 
     *)
    fun needs_boxing_dynamic str con_ir = 
	let
	  val tmp = alloc_regi NOTRACE_INT
	  val tag = alloc_regi NOTRACE_INT		  
	  val field_con_ir = alloc_regi TRACE
		
	  val boxl = fresh_code_label (str ^ "_box")
	  val noboxl = fresh_code_label (str ^ "_nobox")
	  val _ = 
	      (add_instr(LOAD32I(EA(con_ir,4*4),field_con_ir));
	       add_instr(CMPUI(LE, field_con_ir, IMM 4, tmp)); (* check ints *)
	       add_instr(BCNDI(NE, tmp, boxl, false));
	       add_instr(CMPUI(LE, field_con_ir, IMM 255, tmp)); (* check for other small types *)
	       add_instr(BCNDI(NE, tmp, noboxl, false));
	       add_instr(LOAD32I(EA(field_con_ir,0),tag));
	       add_instr(CMPUI(EQ, tag, IMM 4, tmp)); (* check for sums *)
	       add_instr(BCNDI(NE, tmp, boxl, false));
	       add_instr(CMPUI(EQ, tag, IMM 8, tmp)); (* check mus *)
	       add_instr(BCNDI(NE, tmp, boxl, false)))
	in  (boxl, noboxl)
	end

    (* Falls through to non-record case *)
    fun isrecord_dynamic str field_con_ir = 
	let
	  val tmp = alloc_regi NOTRACE_INT
	  val tag = alloc_regi NOTRACE_INT
	  val recordl = fresh_code_label (str ^ "_box")
	  val nonrecordl = fresh_code_label (str ^ "_nobox")
	  val _ = (add_instr(CMPUI(GE, field_con_ir, IMM 255, tmp));
		   add_instr(BCNDI(EQ, tmp, nonrecordl, false));
		   add_instr(LOAD32I(EA(field_con_ir,0),tag));
		   add_instr(CMPUI(EQ, tag, IMM 5, tmp));
		   add_instr(BCNDI(NE, tmp, recordl, false)))
	in  (recordl,nonrecordl)
	end

    fun xsum_dynamic_single (info,
			     con_varloc,
			     exp_varloc) : term * con * state = 
      let val _ = Stats.counter("RTLxsum_dyn_single") ()
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  val desti = alloc_regi TRACE
	  val afterl = fresh_code_label "xsum_dyn_after"
	  val con_ir = load_ireg_term(con_varloc,NONE)
	  val exp_ir = load_ireg_term(exp_varloc,NONE)
	  
	  (* Perform the branching with fall-through to nobox case *)
	  val (boxl,noboxl) = needs_boxing_dynamic "xsum_dyn_single" con_ir

	  (* nobox case *)
	  val _ = (add_instr(ILABEL noboxl);
		   add_instr(MV(exp_ir,desti));
		   add_instr(BR afterl))
	  (* box case *)
	  val _ = add_instr(ILABEL boxl)
	  val (lv,state) = make_record(state,NONE,[valloc2rep exp_varloc],[exp_varloc])
	  val rec_ir = load_ireg_term(lv,NONE)
	  val _ = add_instr(MV(rec_ir,desti))

	  (* afterwards *)
	  val _ = add_instr(ILABEL afterl)
	      
      in  (LOCATION(REGISTER(false, I desti)),sumcon,state)
      end

    fun xsum_dynamic_multi (info,
			    con_varloc,
			    exp_varloc) : term * con * state = 
	let val _ = Stats.counter("RTLxsum_dyn_multi") ()
	    
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  val numfields = alloc_regi NOTRACE_INT
	  val gctemp = alloc_regi NOTRACE_INT
	  val oldmask = alloc_regi NOTRACE_INT
	  val tmp = alloc_regi NOTRACE_INT
	  val tmp2 = alloc_regi NOTRACE_INT
	  val data = alloc_regi NOTRACE_INT
	  val tagi = alloc_regi NOTRACE_INT
	  val rectagi = alloc_regi NOTRACE_INT
	  val newtag = alloc_regi NOTRACE_INT
	  val srccursor = alloc_regi LOCATIVE
	  val destcursor = alloc_regi LOCATIVE
	  val field_type = List.nth(sumtypes, TW32.toInt field_sub)
	  val desti = alloc_regi(con2rep state sumcon)
	      
	  val nonrecordl = fresh_code_label "dyntagsum_norecord"
	  val afterl = fresh_code_label "dyntagsum_after"
	  val loopl = fresh_code_label "dyntagsum_loop"
	  val recordl = fresh_code_label "dyntagsum_record"
	      
	  val con_ir = load_ireg_term(con_varloc,NONE)
	  (* the 5 fields of the sum are: tag, known, tagcount, total, type args *)
	  val summand_con_ir = alloc_regi TRACE
	  val _ = add_instr(LOAD32I(EA(con_ir,4*4),summand_con_ir))
	  (* since there is more than one type arg, it is stored in a record *)
	  val field_con_ir = alloc_regi TRACE
	  val _ = add_instr(LOAD32I(EA(summand_con_ir,4*(w2i field_sub)),field_con_ir))

	  (* is it a record type?  fall-through to non-record case *)
	  val (recordl,nonrecordl) = isrecord_dynamic "xsum_multi" field_con_ir
	      
	  (* easier non-record case *)
	  val _ = add_instr(ILABEL nonrecordl) 
	  val vls = [VALUE(INT field_sub),exp_varloc]
	  val (lv,state) = make_record(state,NONE,map valloc2rep vls,vls)
	  val ir = load_ireg_term(lv,NONE)
	  val _ = add_instr(MV(ir,desti))
	  val _ = add_instr(BR afterl)
	      
	  (* difficult record case - XXX breaks for multi-record *)
	  val _ = add_instr(ILABEL recordl) 
	  val vl_ir = load_ireg_term(exp_varloc,NONE)
	  val _ = add_instr(LOAD32I(EA(vl_ir,~4),rectagi));  (* load record tag *)
	  val _ = add_instr(SRL(rectagi,IMM rec_len_offset,numfields))
	  val _ = add_instr(ANDB(numfields,IMM 31,numfields))       (* numfields = orig # of fields *)
	  val _ = add_instr(SRL(rectagi,IMM rec_mask_offset,oldmask))  (* tmp = old mask right-justified *)
	  val _ = add_instr(SLL(oldmask,IMM (1+rec_mask_offset), tmp)) (* tmp = new mask with extra field *)
	  (* XXX record aspect is zero *)
	  val _ = add_instr(ADD(numfields,IMM 1, tmp2))
	  val _ = add_instr(SLL(tmp2,IMM rec_len_offset, tmp2)) (* compute record aspect and new length *)
	  val _ = add_instr(ORB(tmp,REG tmp2,newtag))           (* throw in the mask *)
	      
	  val _ = add_instr(ADD(numfields,IMM 2, gctemp))
	  val state = needgc(state,REG gctemp)             (* allocate space for sum record *)
	      
	  val _ = add_instr(STORE32I(EA(heapptr,0),newtag))  (* store record tag *)
	  val _ = add_instr(LI(field_sub, tmp))
	  val _ = add_instr(STORE32I(EA(heapptr,4),tmp))     (* store sum tag *)
	  val _ = add_instr(S4ADD(numfields, REG vl_ir, srccursor)) (* initialize src cursor *)
	  val _ = add_instr(ADD(heapptr,IMM 8, destcursor))
	  val _ = add_instr(S4ADD(numfields, REG destcursor, destcursor)) (* init dest cursor *)
	      
	  (* record has at least one field *)
	  val _ = (add_instr(ILABEL loopl); 
		   add_instr(SUB(srccursor,IMM 4, srccursor));
		   add_instr(SUB(destcursor,IMM 4, destcursor));
		   add_instr(LOAD32I(EA(srccursor,0),data));
		   add_instr(ANDB(oldmask,IMM 1, tmp));
		   add_instr(SRL(oldmask, IMM 1, oldmask));
		   add_instr(INIT(EA(destcursor,0),data,SOME tmp));
		   add_instr(CMPUI(LE,srccursor,REG vl_ir, tmp));
		   add_instr(BCNDI(EQ, tmp, loopl,false)))  (* loop will copy record fields *)
	      
	  val _ = add_instr(ADD(heapptr,IMM 4, desti))
	  val _ = add_instr(S4ADD(gctemp, REG heapptr, heapptr))

	  (* result is in desti at this point *)
	  val _ = add_instr(ILABEL afterl) 
	      
      in  (LOCATION(REGISTER(false, I desti)),sumcon,state)
      end

    fun xsum_dynamic (info,
		      con_varloc,
		      exp_varloc) : term * con * state = 
	let
	    val  (state,known,sumcon,
		  tagcount,nontagcount,single_carrier,is_tag,
		  sumtypes,field_sub) = help info
	in  if single_carrier
		then xsum_dynamic_single(info,con_varloc,exp_varloc)
	    else xsum_dynamic_multi(info,con_varloc,exp_varloc)
	end
    
  fun xsum_nonrecord (info,
		      varloc_opt) : term * con * state = 
      let
	  open Prim
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  fun single () =
	      let 
		  val SOME varloc = varloc_opt
		  val field_type = List.nth(sumtypes, TW32.toInt field_sub)
		  val desti = alloc_regi(con2rep state sumcon)
	      in  if (needs_boxing state field_type)
		  then 
		      let val rep = valloc2rep varloc
			  val (vl,state) = make_record(state,NONE,[rep],[varloc])
		      in  (vl,sumcon,state)
		      end
		  else 
		      (varloc,sumcon,state)
	      end

	  fun multi () =
	      let val SOME varloc = varloc_opt
		  val varlocs = [VALUE(INT field_sub), varloc]
		  val reps = map valloc2rep varlocs
		  val (vl,state) = make_record(state,NONE,reps,varlocs)
	      in  (vl,sumcon,state)
	      end
	  
      in  if is_tag
	      then (VALUE(TAG known), sumcon,state)
	  else (if single_carrier
		    then single() else multi())
      end

  fun xsum_record (info,
		   varlocs) : term * con * state = 
      let
	  open Prim
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info
      in  if is_tag
	      then (VALUE(TAG known), sumcon,state)
	  else
	      let val varlocs = if single_carrier
				    then varlocs
				else (VALUE(INT field_sub)) :: varlocs
		  val reps = map valloc2rep varlocs
		  val (vl,state) = make_record(state,NONE,reps,varlocs)
	      in  (vl,sumcon,state)
	      end
      end


  fun xproject_sum_dynamic 
			(info, con_ir, exp_ir, traceinfo)
			: term * con * state = 
     let val _ = Stats.counter("RTLprojsum") ()
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info
	  val summand_type = (List.nth(sumtypes,w2i field_sub)
			      handle _ => error "bad project_sum: record_con")

	  val (_,dest as I desti) = alloc_reg_trace state traceinfo

	  fun single_case() = 
	      let val afterl = fresh_code_label "projsum_single_after"

		  (* Perform the branching with fall-through to nobox case *)
		  val (boxl,noboxl) = needs_boxing_dynamic "xprojsum_dyn_single" con_ir
		  (* no box case *)
		  val _ = (add_instr(ILABEL noboxl);
			   add_instr(MV(exp_ir,desti));
			   add_instr(BR afterl))
		  (* box case *)
		  val _ = (add_instr(ILABEL boxl);
			   add_instr(LOAD32I(EA(exp_ir,0),desti)))
		  (* afterwards *)
		  val _ = add_instr(ILABEL afterl)
	      in  state
	      end

	  fun multi_case() = 
	      let val afterl = fresh_code_label "projsum_multi_after"
		  val loopl = fresh_code_label "projsum_multi_loop"
		  val tag = alloc_regi NOTRACE_INT
		  val rectag = alloc_regi NOTRACE_INT
		  val oldmask = alloc_regi NOTRACE_INT
		  val tmp = alloc_regi NOTRACE_INT
		  val gctemp = alloc_regi NOTRACE_INT
		  val sumrectag = alloc_regi NOTRACE_INT
		  val numfields = alloc_regi NOTRACE_INT
		  val data = alloc_regi NOTRACE_INT
		  val srccursor = alloc_regi LOCATIVE
		  val destcursor = alloc_regi LOCATIVE

		  (* the 5 fields of the sum are: tag, known, tagcount, total, type args *)
		  val summand_con_ir = alloc_regi TRACE
		  val field_con_ir = alloc_regi TRACE
		  val _ = add_instr(LOAD32I(EA(con_ir,4*4),summand_con_ir))
		  (* since there is more than one type arg, it is stored in a record *)
		  val _ = add_instr(LOAD32I(EA(summand_con_ir,4*w2i field_sub),field_con_ir))

		  (* is it a record type?  fall-through to non-record case *)
		  val (recordl,nonrecordl) = isrecord_dynamic "xprojsum_multi" field_con_ir

		  (* non-record case *)
		  val _ = (add_instr(ILABEL nonrecordl);
			   add_instr(LOAD32I(EA(exp_ir,4),desti));
			   add_instr(BR afterl))

		  (* XXX this will break if record is a multi-record  *)		  
                  (* record case *)
		  val _ = (add_instr(ILABEL recordl);
			   add_instr(LOAD32I(EA(field_con_ir,4),numfields)))

		  val _ = add_instr(LOAD32I(EA(exp_ir,~4),sumrectag))
		  val _ = add_instr(SRL(sumrectag, IMM rec_len_offset,gctemp))
		  val _ = add_instr(ANDB(gctemp, IMM 31, gctemp))         (* gctemp is original length *)
		  val _ = add_instr(SUB(gctemp,IMM 1,numfields))          (* numfield = # of fields in final rec *)
		  val _ = add_instr(SRL(sumrectag, 
					IMM (1+rec_mask_offset), oldmask))    (* tmp is mask with first field removed *)
		  val _ = add_instr(SLL(oldmask,IMM rec_mask_offset, rectag)) (* rectag is new mask with XXX record aspect zero *)
		  val _ = add_instr(SLL(numfields,IMM rec_len_offset, tmp))
		  val _ = add_instr(ORB(rectag,REG tmp,rectag))           (* final record tag *)

		  val state = needgc(state,REG gctemp)       (* allocate space for record *)

		  val _ = add_instr(STORE32I(EA(heapptr,0),rectag))
		  val _ = add_instr(S4ADD(numfields, REG heapptr, destcursor)) (* record tag *)
		  val _ = add_instr(S4ADD(numfields, REG exp_ir, srccursor)) (* sum tag *)

		  (* XXX this will break if record is a multi-record  *)		  
		  val _ = (add_instr(ILABEL loopl); 
			   add_instr(LOAD32I(EA(srccursor,0),data));
			   add_instr(ANDB(oldmask,IMM 1, tmp));
			   add_instr(SRL(oldmask, IMM 1, oldmask));
			   add_instr(INIT(EA(destcursor,0),data,SOME tmp));
			   add_instr(SUB(srccursor,IMM 4, srccursor));
			   add_instr(SUB(destcursor,IMM 4, destcursor));
			   add_instr(CMPUI(EQ,destcursor,REG heapptr, tmp));
			   add_instr(BCNDI(EQ, tmp, loopl,false)); (* loop will copy record fields *)
			   add_instr(ADD(heapptr,IMM 4, desti));
			   add_instr(S4ADD(gctemp, REG heapptr,heapptr)))

		  (* afterwards *)
		  val _ = add_instr(ILABEL afterl)
	      in  state
	      end
	  val state = if single_carrier then single_case() 
			   else multi_case()
	  val lv = LOCATION(REGISTER (false, dest))
      in  (lv,summand_type,state)
      end



  fun xproject_sum_record(info, field, clist, base, niltrace) : term * con * state = 
      let val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  val fieldcon = (List.nth(sumtypes,w2i field_sub) 
			  handle _ => error "list.nth 2")
	  val (_, Prim_c(Record_c (labs,_), cons)) = simplify_type state fieldcon
	  val labs_cons = Listops.zip labs cons
	  val (recfield_index, recfield_con) = 
	      let fun loop n [] = 
		  (print "bad project_sum_record: missing field ";
		   Ppnil.pp_label field; print "\n in type = \n";
		   Ppnil.pp_con fieldcon; print "\n";
		   error "bad project_sum_record: bad econ field not found")
		    | loop n ((a:Name.label*con)::rest) = if (Name.eq_label(#1 a,field))
							 then (n,#2 a) else loop (n+1) rest
	      in  loop 0 labs_cons
	      end

	  val (_,I desti) = alloc_reg_trace state niltrace
	  val subscript = if single_carrier then recfield_index else recfield_index + 1
	  val _ = add_instr(LOAD32I(EA(base,4*subscript),desti))
      in (LOCATION(REGISTER(false, I desti)), recfield_con, state)
      end

  fun xproject_sum_nonrecord (info, base, ssumcon, niltrace) : term * con * state = 
      let val lv = LOCATION(REGISTER(false, I base))
	  val v = fresh_named_var "named_project_sumee"
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info
	  val state = add_reg (state,v,ssumcon,I base)
	  val summand_type = (List.nth(sumtypes,w2i field_sub)
				     handle _ => error "bad project_sum: record_con")
	      
	  val need_unbox = single_carrier andalso (needs_boxing state summand_type)

	  fun unbox offset =
	      let val ir = load_ireg_term(lv,NONE)
		  val (_,I desti) = alloc_reg_trace state niltrace
		  val _ = add_instr(LOAD32I(EA(ir,offset), desti))
	      in  (LOCATION(REGISTER(false, I desti)), summand_type,state)
	      end
      in  (case (single_carrier,need_unbox) of
	       (true,false) => (lv,ssumcon,state)
	     | (true,true) => unbox 0
	     | (false,_) => unbox 4)
      end
	       


end

