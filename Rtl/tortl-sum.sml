(*$import TORTL TORTLSUM RTL PPRTL TORTLBASE RTLTAGS NIL NILUTIL PPNIL Stats *)

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
		      if there are more than one type argument, then this will be a pointer 
			  to the tuple of the summand types
	(3) Mu types are represented by a record whose first field is 8.	
		There are currently no other fields since mu's are not fully represented.  
	(4) The representation of values of sum and mu types will always be a pointer
	        though that might include small tag values.
*)
	
(* xtagsum_dynamic record case is fragile *)


functor TortlSum(structure Pprtl : PPRTL
		 structure TortlBase : TORTL_BASE 
		 structure Rtltags : RTLTAGS 
		 structure NilUtil : NILUTIL
		 structure Ppnil : PPNIL
		 sharing Pprtl.Rtltags = Rtltags)
    :> TORTL_SUM where TortlBase = TortlBase
=
struct

structure TortlBase = TortlBase
open Nil
open TortlBase

val do_constant_records = ref true
val do_gcmerge = ref true
val do_single_crecord = ref true


val diag = ref true
val debug = ref false
val debug_full = ref false
val debug_full_env = ref false
val debug_simp = ref false
val debug_bound = ref false

   (* Module-level declarations *)

    structure Rtl = Rtl
    structure Nil = Nil
    open Util Listops
    open Nil
    open NilUtil
    open Rtl
    open Name
    open Rtltags 
    open Pprtl 
    open TortlBase

    val exncounter_label = ML_EXTERN_LABEL "exncounter"
    val error = fn s => (Util.error "tortl.sml" s)
    structure TW32 = TilWord32
    structure TW64 = TilWord64
    val w2i = TW32.toInt
    val i2w = TW32.fromInt



  (* First, we assume that datatypes have already been translated by 
     rearranging the non value-carrying components to the beginning.
     To support efficient representations, we let m and n denote
     the length of the maximal initial segment of non value-carrying components 
     and n the rest of the components.  Note that this m is
     NOT the number(k) of non-value-carrying constructors in the datatype.
     It is possible for m > k. There are then 6 cases to consider:

         1. m=0, n=0    we use the unit representation
         2. m<>0, n=0   we use tags to represent the datatype
         3. m=0, n=1    we use the argument type to represent the datatype
         4. m<>0, n=1   we use tag or pointer to represent the datatype
         5. m<>0, n>1   we use tag or tagged sum to represent the datatype
         6. m=0, n>1    we use tagged sum to represent the datatype

     Tag sums are flat if the carrier is a record type.
     
     is_record indicates that the injectee is of a record type and the 
       that the term arguments are passed in separately as record components
  *)

  fun xsum xcon is_record (orig_state : state,
		      known,
		      sumcon : con, 
		      varlocs) : loc_or_val * con * state = 
      let
	  open Prim
	  val (tagcount,sumtypes) = reduce_to_sum "xsum" orig_state sumcon

	  val field_64 = TW64.fromInt(TW32.toInt known)
	  val field_sub = TW32.uminus(known,tagcount)
	  val nontagcount = length sumtypes
	  val single_carrier = nontagcount = 1

          fun xtagsum_tag state = (VAR_VAL(VTAG known), state)
	  fun xtagsum_single state =
	      let 
		  val field_type = List.nth(sumtypes, TW32.toInt field_sub)
		  val desti = alloc_regi(con2rep state sumcon)
		  fun box_case_vl state = 
		      let val reps = map valloc2rep varlocs
		      in  make_record(state,NONE,reps,varlocs)
		      end
		  fun nobox_case_vl state = 
		      if is_record
			  then box_case_vl state
		      else (hd varlocs, state)
	      in  (case simplify_type orig_state field_type of
			  (true,Prim_c(Int_c _, _)) => box_case_vl state
			| (true,Mu_c _) => box_case_vl state
			| (true,Proj_c(Mu_c _,_)) => box_case_vl state
			| (true,Prim_c(Sum_c _,_)) => box_case_vl state
			| (true,Prim_c(Exntag_c, _)) => box_case_vl state
			| (true,_) => nobox_case_vl state
			| _ =>
			      let val beginl = alloc_code_label "tagsum_begin"
				  val boxl = alloc_code_label "tagsum_boxcase"
				  val noboxl = alloc_code_label "tagsum_noboxcase"
				  val afterl = alloc_code_label "tagsum_after"
				  val (con_ir,_,state) = xcon(state,fresh_var(),sumcon, NONE)
				  (* the 5 fields of the sum are: tag, known, tagcount, total, type args *)
				  (* since there is exactly one type arg, it is directly stored *)
				  val summand_con_ir = alloc_regi TRACE
				  val _ = add_instr(LOAD32I(EA(con_ir,4*4),summand_con_ir))
				  val tmp = alloc_regi NOTRACE_INT
				  val tagi = alloc_regi NOTRACE_INT
				  (* XXX should check dynamically for exntag too *)
				  val _ = (add_instr(ILABEL beginl);
					   add_instr(CMPUI(LE, summand_con_ir, IMM 4, tmp)); (* check ints *)
					   add_instr(BCNDI(NE, tmp, boxl, false));
					   add_instr(CMPUI(LE, summand_con_ir, IMM 255, tmp)); (* check for other small types *)
					   add_instr(BCNDI(NE, tmp, noboxl, false));
					   add_instr(LOAD32I(EA(summand_con_ir,0),tagi));
					   add_instr(CMPUI(EQ, tagi, IMM 4, tmp)); (* check for sums *)
					   add_instr(BCNDI(NE, tmp, boxl, false));
					   add_instr(CMPUI(EQ, tagi, IMM 8, tmp)); (* check mus *)
					   add_instr(BCNDI(NE, tmp, boxl, false)))

				  (* no box case *)
				  val _ = add_instr(ILABEL noboxl)
				  val (vl,nobox_state) = nobox_case_vl state
				  val _ = add_instr(MV(load_ireg_locval(vl,NONE),desti))
				  val _ = add_instr(BR afterl)

				  (* box case *)
				  val _ = add_instr(ILABEL boxl)
				  val (vl,box_state) = box_case_vl state
				  val _ = add_instr(MV(load_ireg_locval(vl,NONE), desti))
				  val _ = add_instr(ILABEL afterl)

			      in  (VAR_LOC(VREGISTER(false, I desti)),
				   join_states[nobox_state,box_state])
			      end)
	      end

	  fun xtagsum_dynamic (state,vl) =
	      let val numfields = alloc_regi NOTRACE_INT
		  val gctemp = alloc_regi NOTRACE_INT
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

		  val nonrecordl = alloc_code_label "dyntagsum_norecord"
		  val afterl = alloc_code_label "dyntagsum_after"
		  val loopl = alloc_code_label "dyntagsum_loop"
		  val recordl = alloc_code_label "dyntagsum_record"

		  val (con_ir,_,state) = xcon(state,fresh_var(),sumcon,NONE)
		  (* the 5 fields of the sum are: tag, known, tagcount, total, type args *)
		  val summand_con_ir = alloc_regi TRACE
		  val _ = add_instr(LOAD32I(EA(con_ir,4*4),summand_con_ir))
		  (* since there is more than one type arg, it is stored in a record *)
		  val field_con_ir = alloc_regi TRACE
		  val _ = add_instr(LOAD32I(EA(summand_con_ir,4*(w2i field_sub)),field_con_ir)) (* field type *)

		  val _ = (add_instr(CMPUI(LE, field_con_ir, IMM 255, tmp)); (* check for small types *)
			   add_instr(BCNDI(NE, tmp, nonrecordl, false));
			   add_instr(LOAD32I(EA(field_con_ir,0),tagi));  (* load tag of the type *)
			   add_instr(CMPUI(NE, tagi, IMM 5, tmp)); (* check for record *)
			   add_instr(BCNDI(NE, tmp, nonrecordl, false)))

                  (* difficult record case *)
		  val _ = add_instr(ILABEL recordl) 
		  val vl_ir = load_ireg_locval(vl,NONE)
		  val _ = add_instr(LOAD32I(EA(vl_ir,~4),rectagi));  (* load record tag *)
  (* XXX this will break if record is a multi-record  *)
		  val _ = add_instr(SRL(rectagi,IMM 27,numfields))
		  val _ = add_instr(SLL(rectagi,IMM 5, tmp)) 
		  val _ = add_instr(SRL(tmp,IMM (3+5), tmp))         (* tmp = old mask right-justified *)
		  val _ = add_instr(SLL(tmp,IMM 4, tmp))             (* tmp = new mask with extra field *)
		                                                     (* XXX record aspect is zero *)
		  val _ = add_instr(ADD(numfields,IMM 1, tmp2))
		  val _ = add_instr(SLL(tmp2,IMM 27, tmp2))        (* compute new length *)
		  val _ = add_instr(ORB(tmp,REG tmp2,newtag))      (* final record tag *)
				    
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
			   add_instr(STORE32I(EA(destcursor,0),data));
			   add_instr(CMPUI(LE,srccursor,REG vl_ir, tmp));
			   add_instr(BCNDI(EQ, tmp, loopl,false)))  (* loop will copy record fields *)

		  val _ = add_instr(ADD(heapptr,IMM 4, desti))
		  val _ = add_instr(S4ADD(gctemp, REG heapptr, heapptr))
		  val _ = add_instr(BR afterl)

                  (* easier non-record case *)
		  val _ = add_instr(ILABEL nonrecordl) 
		  val vls = (case varlocs of
				 [vl] => [VAR_VAL(VINT field_sub),vl]
			       | _ => error "xtagsum_dynamic not with one arg")
		  val (lv,state) = make_record(state,NONE,map valloc2rep vls,vls)
		  val ir = load_ireg_locval(lv,NONE)
		  val _ = add_instr(MV(ir,desti))

                  (* result is in desti at this point *)
		  val _ = add_instr(ILABEL afterl) 

	      in  (VAR_LOC(VREGISTER(false, I desti)),state)
	      end

	  fun xtagsum state = 
	      let fun decompose vl labs cons = 
		    let val addr = load_ireg_locval(vl,NONE)
		        fun mapper (which,l,c) =
			let val I desti = alloc_reg state c
			    val _ = add_instr(LOAD32I(EA(addr,which*4),desti))
			in  VAR_LOC(VREGISTER(false, I desti))
			end
		    in  Listops.map2count mapper (labs,cons)
		    end
		  val field_type = List.nth(sumtypes, TW32.toInt field_sub)
		  val (hnf,field_type') = simplify_type orig_state field_type
(*
val _ = (print "xtagsum - field_type = \n"; Ppnil.pp_con field_type;
	print "  field_type' = \n"; Ppnil.pp_con field_type'; print "\nhnf = ";
	print (Bool.toString hnf); print "\nis_record = ";
	print (Bool.toString is_record); print "\n")
*)
		  val (vls,state) = 
		      (case (is_record,hnf,field_type',varlocs) of
			   (true,_,_,_) => (SOME varlocs, state)
			 | (_,_,Prim_c(Record_c labs, cons),[vl]) => 
			       let val vls = decompose vl labs cons
			       in  (SOME vls, state)
			       end
			 | (_,true,_,[vl]) => (SOME [vl], state)
			 | _ => (NONE, state))
	      in  case (vls,varlocs) of
		  (NONE,[vl]) => xtagsum_dynamic(state, vl)
		| (NONE,_) => error "xtagsum_dynamic case not with one vl"
		| (SOME varlocs,_) =>
		      let 
			  val varlocs = (VAR_VAL(VINT field_sub)) :: varlocs
			  val reps = map valloc2rep varlocs
		      in  make_record(state,NONE,reps,varlocs)
		      end
	      end
	  val (varloc,state) =
	      case (TW32.toInt tagcount,nontagcount) of
		  (0,0) => xtagsum_tag orig_state
		| (_,0) => xtagsum_tag orig_state
		| (_,1) => if (TW32.equal(known,tagcount))
			       then xtagsum_single orig_state
			   else xtagsum_tag orig_state
		| (_,_) => if (TW32.ult(known,tagcount))
			       then xtagsum_tag orig_state
			   else xtagsum orig_state
      in (varloc, sumcon, state) 
      end

  fun xdynamic_project_sum xcon
			(state : state, sumcon, (tagcount,sumtype), 
			   summand_types, summand_type,
			   exp_ir) : loc_or_val * con * state = 
      let val single_carrier = (length summand_types) = 1
	  val field_sub = TW32.uminus(sumtype,tagcount)
	  fun single_case() = 
	      let val desti = alloc_regi(con2rep state summand_type)
		  val afterl = alloc_code_label "projsum_single_after"
		  val boxl = alloc_code_label "projsum_single_box"
		  val noboxl = alloc_code_label "projsum_single_nobox"
		  val (con_ir,_,state) = xcon(state,fresh_var(),sumcon, NONE)
		  (* the 5 fields of the sum are: tag, known, tagcount, total, type args *)
		  (* since there is exactly one type arg, it is directly stored *)
		  val summand_con_ir = alloc_regi TRACE
		  val _ = add_instr(LOAD32I(EA(con_ir,4*4),summand_con_ir))
		  val tmp = alloc_regi NOTRACE_INT
		  val tagi = alloc_regi NOTRACE_INT
		  val _ = (add_instr(CMPUI(LE, summand_con_ir, IMM 4, tmp)); (* check ints *)
			   add_instr(BCNDI(NE, tmp, boxl, false));
			   add_instr(CMPUI(LE, summand_con_ir, IMM 255, tmp)); (* check for other small types *)
			   add_instr(BCNDI(NE, tmp, noboxl, false));
			   add_instr(LOAD32I(EA(summand_con_ir,0),tagi));
			   add_instr(CMPUI(EQ, tagi, IMM 4, tmp)); (* check for sums *)
			   add_instr(BCNDI(NE, tmp, boxl, false));
			   add_instr(CMPUI(EQ, tagi, IMM 8, tmp)); (* check mus *)
			   add_instr(BCNDI(NE, tmp, boxl, false)))
		  (* no box case *)
		  val _ = (add_instr(ILABEL noboxl);
			   add_instr(MV(exp_ir,desti));
			   add_instr(BR afterl))
		  (* box case *)
		  val _ = (add_instr(ILABEL boxl);
			   add_instr(LOAD32I(EA(exp_ir,0),desti));
			   add_instr(ILABEL afterl))
	      in  (desti, state)
	      end

	  fun multi_case() = 
	      let val afterl = alloc_code_label "projsum_multi_after"
		  val nonrecordl = alloc_code_label "projsum_multi_nonrecord"
		  val recordl = alloc_code_label "projsum_multi_record"
		  val loopl = alloc_code_label "projsum_multi_loop"
		  val tag = alloc_regi NOTRACE_INT
		  val rectag = alloc_regi NOTRACE_INT
		  val tmp = alloc_regi NOTRACE_INT
		  val tmp2 = alloc_regi NOTRACE_INT
		  val gctemp = alloc_regi NOTRACE_INT
		  val sumrectag = alloc_regi NOTRACE_INT
		  val numfields = alloc_regi NOTRACE_INT
		  val data = alloc_regi NOTRACE_INT
		  val srccursor = alloc_regi LOCATIVE
		  val destcursor = alloc_regi LOCATIVE
		  val desti = alloc_regi(con2rep state summand_type)

		  val (con_ir,_,state) = xcon(state,fresh_var(),sumcon, NONE)

		  (* the 5 fields of the sum are: tag, known, tagcount, total, type args *)
		  val summand_con_ir = alloc_regi TRACE
		  val field_con_ir = alloc_regi TRACE
		  val _ = add_instr(LOAD32I(EA(con_ir,4*4),summand_con_ir))
		  (* since there is more than one type arg, it is stored in a record *)
		  val _ = add_instr(LOAD32I(EA(summand_con_ir,4*(w2i field_sub)),field_con_ir))

		  val _ = (add_instr(CMPUI(GE, field_con_ir, IMM 255, tmp));
			   add_instr(BCNDI(EQ, tmp, nonrecordl, false));
			   add_instr(LOAD32I(EA(field_con_ir,0),tag));
			   add_instr(CMPUI(EQ, tag, IMM 5, tmp));
			   add_instr(BCNDI(NE, tmp, recordl, false)))
		  (* non-record case *)
		  val _ = (add_instr(ILABEL nonrecordl);
			   add_instr(LOAD32I(EA(exp_ir,4),desti));
			   add_instr(BR afterl))

		  (* XXX this will break if record is a multi-record  *)		  
                  (* record case *)
		  val _ = (add_instr(ILABEL recordl);
			   add_instr(LOAD32I(EA(field_con_ir,4),numfields)))

		  val _ = add_instr(LOAD32I(EA(exp_ir,~4),sumrectag))
		  val _ = add_instr(LI(0wxf8000000,tmp2))
		  val _ = add_instr(ANDB(tmp2,REG sumrectag,tmp2))      (* tmp2 is extracted length *)
		  val _ = add_instr(XORB(tmp2,REG sumrectag,tmp))       (* tmp is mask only *)
		                                                    (* XXX since record aspect is zero *)
		  val _ = add_instr(SRL(tmp,IMM 1, tmp))          (* tmp is new mask with record aspect *)
		  val _ = add_instr(SRL(tmp2,IMM 27, gctemp))
		  val _ = add_instr(SUB(gctemp,IMM 1, numfields))   (* # of fields in final rec *)
		  val _ = add_instr(SLL(numfields,IMM 27, tmp2))    (* length bits *)
		  val _ = add_instr(ORB(tmp,REG tmp2,rectag))      (* final record tag *)

		  val state = needgc(state,REG gctemp)       (* allocate space for record *)

		  val _ = add_instr(STORE32I(EA(heapptr,0),rectag))
		  val _ = add_instr(S4ADD(numfields, REG heapptr, destcursor)) (* record tag *)
		  val _ = add_instr(S4ADD(numfields, REG exp_ir, srccursor)) (* sum tag *)

		  val _ = (add_instr(ILABEL loopl); 
			   add_instr(LOAD32I(EA(srccursor,0),data));
			   add_instr(STORE32I(EA(destcursor,0),data));
			   add_instr(SUB(srccursor,IMM 4, srccursor));
			   add_instr(SUB(destcursor,IMM 4, destcursor));
			   add_instr(CMPUI(EQ,destcursor,REG heapptr, tmp));
			   add_instr(BCNDI(EQ, tmp, loopl,false)); (* loop will copy record fields *)
			   add_instr(ADD(heapptr,IMM 4, desti));
			   add_instr(S4ADD(gctemp, REG heapptr,heapptr));
			   add_instr(ILABEL afterl))
	      in  (desti,state)
	      end
	  val (ir,state) = if single_carrier then single_case() 
			   else multi_case()
	  val lv = VAR_LOC(VREGISTER (false, I ir))
      in  (lv,summand_type,state)
      end



  fun xproject_sum_record(state : state, k, field, clist, 
			  base, econ, copt) : loc_or_val * con * state = 
	       let val (tagcount, sumtype, summands) = 
		         reduce_to_known_sum "project_sum_record" state econ
		   val index = TW32.toInt(TW32.uminus(sumtype, tagcount))

		   val fieldcon = List.nth(summands,index) handle _ => error "list.nth 2"
		   val field' = 
		        let fun loop n [] = 
				     (print "bad project_sum_record: missing field ";
				      Ppnil.pp_label field; print "\n in type = \n";
				      Ppnil.pp_con fieldcon; print "\n";
				      error "bad project_sum_record: bad econ field not found")
			      | loop n (a::rest) = if (Name.eq_label(a,field))
						       then n else loop (n+1) rest
			in  (case fieldcon of
				 (Prim_c(Record_c labels, _)) => loop 0 labels
			       | _ => (case #2(simplify_type state fieldcon) of
					   (Prim_c(Record_c labels, _)) => loop 0 labels
					 | c => (print "bad project_sum_record: not record\n";
						 Ppnil.pp_con c;
						 error "bad project_sum_record: not record\n")))
			end
		   val single_carrier = (length summands) = 1
		   local
		       val record_con = (List.nth(summands,index)
					 handle _ => error "bad project_sum_record: record_con")
		   in
		       val field_con = 
			   (case (copt,record_con) of
				(SOME c, _) => c
			      | (_,Prim_c(Record_c _,cons)) => (List.nth(cons,field')
							    handle _ => error "list.nth 4")
			      | _ => (case #2(simplify_type state record_con) of
					  Prim_c(Record_c _,cons) => (List.nth(cons,field')
								      handle _ => error "list.nth 5")
					| c => (print "bad project_sum_record: field_con";
						Ppnil.pp_con c;
					    error "bad project_sum_record: field_con")))
		   end
		   val desti = alloc_regi (con2rep state field_con)
		   val subscript = if single_carrier then field' else field' + 1
		   val _ = add_instr(LOAD32I(EA(base,4*subscript),desti))
	       in (VAR_LOC(VREGISTER(false, I desti)), field_con, state)
	       end

  fun xproject_sum xcon(state : state, k, clist, 
			  base, ssumcon, copt) : loc_or_val * con * state = 
	       let val sumcon = hd clist
		   val v_lv = VAR_LOC(VREGISTER(false, I base))
		   val v = fresh_named_var "named_project_sumee"
		   val state = add_reg (state,v,ssumcon,I base)
		   val (tagcount, sumtype, summand_types) = 
		         reduce_to_known_sum "project_sum_record" state ssumcon
	           val index = TW32.toInt(TW32.uminus(sumtype, tagcount))
		   val summand_type = (List.nth(summand_types,index)
				     handle _ => error "bad project_sum: record_con")
		   fun record_case labels field_cons = 
		       let fun folder (l,state) = let val (lv,c,s) = xproject_sum_record(state,k,l,[sumcon],
								       base, ssumcon,NONE)
						  in  ((lv,c),s)
						  end
			   val (lv_cons,state) = foldl_acc folder state labels
			   val vallocs = map #1 lv_cons
		           val cons = map#2 lv_cons
			   val reps = map valloc2rep vallocs
			   val (lv,state) = make_record(state,NONE,reps,vallocs)
		       in  (lv, Prim_c(Record_c labels, cons), state)
		       end
		   fun nonrecord_case() = 
		       let 
			   val (single_carrier,need_unbox) = 
			       if (length summand_types) = 1
				   then
				       (case simplify_type state (hd summand_types) of
					    (_,Prim_c(Int_c _,_)) => (true,true)
					  | (_,Prim_c(Sum_c _,_)) => (true,true)
					  | (_,Mu_c _) => (true,true)
					  | (_,Proj_c(Mu_c _,_)) => (true,true)
					  | (true,c) => (true,false)
					  | _ => error "project_sum not fully done")
			       else  (false,false)

			   val lv = v_lv
			   val c = ssumcon

			   fun unbox offset =
			       let val ir = load_ireg_locval(lv,NONE)
				   val desti = alloc_regi(con2rep state summand_type)
				   val _ = add_instr(LOAD32I(EA(ir,offset), desti))
			       in  (VAR_LOC(VREGISTER(false, I desti)), summand_type,state)
			       end
		       in  (case (single_carrier,need_unbox) of
				(true,false) => (lv,c,state)
			      | (true,true) => unbox 0
			      | (false,_) => unbox 4)
		       end
	       in  (case summand_type of
			Prim_c(Record_c labs,cons) => record_case labs cons
		      | c => 
			    (case (simplify_type state summand_type) of
				 (_,Prim_c(Record_c labs,cons)) => record_case labs cons
			       | (true,_) => nonrecord_case()
			       | _ => xdynamic_project_sum xcon(state,sumcon,
								 (tagcount,sumtype),
								 summand_types,
								 summand_type,
								 base)))
	       end


end

