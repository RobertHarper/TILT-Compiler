(*$import Prelude TopLevel Util Listops Name List Int Prim TilWord32 TilWord64 TORTLSUM Rtl Pprtl TortlBase TortlRecord Rtltags Nil NilUtil Ppnil Stats *)

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
	


structure TortlSum :> TORTL_SUM =

struct

    open Util Listops Name
    open Nil NilUtil
    open Rtl Pprtl
    open Rtltags TortlBase TortlRecord

    val debug = ref false
    val error = fn s => (Util.error "tortl-sum.sml" s)

    structure TW32 = TilWord32
    structure TW64 = TilWord64
    val w2i = TW32.toInt
    val i2w = TW32.fromInt

    type typearg = state * TilWord32.word * con
    fun help ((state,known,sumcon) : typearg) = 
	let
	  val (tagcount,_,sumtypes) = reduce_to_sum "xsum_???" state sumcon
	  val known = w2i known
	  val tagcount = w2i tagcount
	  val field_sub = known - tagcount
	  val nontagcount = length sumtypes
	  val single_carrier = nontagcount = 1
	  val is_tag = known < tagcount
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
	   | (_,c) => (print "needs_boxing got irreducible type: "; Ppnil.pp_con c; print "\n";
		error "needs_boxing: field type cannot be determined"))

  (* First, we assume that datatypes have already been translated so
     that carriers and non-carries are statically distinguished.  Let
     m and n denote the number of non value-carrying components and n
     the number of value-carrying components.  There are various cases.

     INVARIANT: Values of sum type always look like pointers or tags, but not an int.

         1. n=0         we use a tag to represent the datatype
	 3. m>0, n=1    we use a tag for the non-carriers;
	                the carrier is boxed only if the carried type might look like a tag 
			(e.g., ints, sums, mu's)
         4. n>1         we use tags for the non-carriers and box all carriers

     NOTE: It might seem that we can further specialize the case where m=0 and n=1
           where the sum is represented by the type carried by the one constructor.
	   This is possible at some additional complexity:
	   (1) Sums might look like ints now.
	   (2) The GC must now decode sum types to check for this case.
	       This potentially requires repeated decoding of nested sum types
	       and mu types.  The current representation of mu types is that
	       it is a base case and has a pointer type which is possible 
	       because we only have mu's of sums.
  *)

    (* Check if values of the carriedType might look like an int and thus require boxing
     *)
    fun needs_boxing_dynamic carriedType =
	let
	  val tag = alloc_regi NOTRACE_INT		  
	  val boxl = fresh_code_label "dynamic_box"
	  val noboxl = fresh_code_label "dynamic_nobox"
	  val _ = 
	      (add_instr(BCNDI(LE, carriedType, IMM 4, boxl, false));     (* check for int types *)
	       add_instr(BCNDI(LE, carriedType, IMM 255, noboxl, false)); (* check for other small types *)
	       record_project(carriedType, 0, tag);
	       add_instr(BCNDI(EQ, tag, IMM 12, boxl, false));               (* check for exntags *)
	       add_instr(BCNDI(EQ, tag, IMM 4, boxl, false));                (* check for sums *)
	       add_instr(BCNDI(EQ, tag, IMM 8, boxl, false)))                (* check mus *)
	in  (boxl, noboxl)
	end

    fun projectFieldFromSum (sumtype, tagcount, nontagcount, known) = 
	let val sumtypeArg = alloc_regi TRACE
	    val _ = record_project(sumtype, 4, sumtypeArg)  (* See comment at top on sum type layout *)
	in  if (nontagcount = 1)
		then sumtypeArg
	    else let val result = alloc_regi TRACE
		     val whichCarrier = known - nontagcount
		     val _ = record_project(sumtype, whichCarrier, result)
		 in  result
		 end
	end
    
  fun xinject_sum_static (info, term_opt, trace) : term * state = 
      let
	  open Prim
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  fun single () =
	      let 
		  val SOME term = term_opt
		  val field_type = List.nth(sumtypes, field_sub)
		  val desti = alloc_regi(niltrace2rep state trace)
	      in  if (needs_boxing state field_type)
		  then 
		      make_record(state,[term])
		  else 
		      (term,state)
	      end

	  fun multi () =
	      let val SOME term = term_opt
		  val terms = [VALUE(INT (i2w field_sub)), term]
	      in make_record(state,terms)
	      end
	  
      in  (case (is_tag, nontagcount) of
	       (true, _) => if (known < 256) 
				then (VALUE(TAG (i2w known)), state)
			    else error ("Tag too large " ^ (Int.toString known))
	     | (_, 0) => error "Can't get here"
	     | (_, 1) => single()
	     | _ => multi())
      end


    fun xinject_sum_dynamic_single (info,
				    sumtypeTerm,
				    exp_varloc,
				    trace) : term * state = 
      let val _ = Stats.counter("RTLxinject_sum_dyn_single") ()
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  val desti = alloc_regi TRACE
	  val afterl = fresh_code_label "xinject_sum_dyn_after"
	  val sumtypeReg = load_ireg_term(sumtypeTerm,NONE)
	  val exp_ir = load_ireg_term(exp_varloc,NONE)
	  
	  (* Perform the branching with fall-through to nobox case *)
	  val fieldtypeReg = projectFieldFromSum (sumtypeReg, tagcount, nontagcount, known)
	  val (boxl,noboxl) = needs_boxing_dynamic fieldtypeReg

	  (* nobox case *)
	  val _ = (add_instr(ILABEL noboxl);
		   add_instr(MV(exp_ir,desti));
		   add_instr(BR afterl))
	  (* box case *)
	  val _ = add_instr(ILABEL boxl)
	  val (lv,state) = make_record(state,[exp_varloc])
	  val rec_ir = load_ireg_term(lv,NONE)
	  val _ = add_instr(MV(rec_ir,desti))

	  (* afterwards *)
	  val _ = add_instr(ILABEL afterl)
	      
      in  (LOCATION(REGISTER(false, I desti)),state)
      end

    fun xinject_sum_dynamic_multi (info,
			    con_varloc,
			    exp_varloc,
			    trace) : term * state = 
	let val _ = Stats.counter("RTLxinject_sum_dyn_multi") ()
	    
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  val dest = alloc_regi(niltrace2rep state trace)
	  val vls = [VALUE(INT (i2w field_sub)),exp_varloc]
	  val (lv,state1) = make_record(state,vls)
	  val ir = load_ireg_term(lv,NONE)
	  val _ = add_instr(MV(ir,dest))
	      
      in  (LOCATION(REGISTER(false, I dest)),state)
      end

    fun xinject_sum_dynamic (info,
		      con_varloc,
		      exp_varloc,
		      trace) : term * state = 
	let
	    val  (state,known,sumcon,
		  tagcount,nontagcount,single_carrier,is_tag,
		  sumtypes,field_sub) = help info
	in  if (known < tagcount)
		then xinject_sum_static(info, NONE, trace)   (* Not really dynamic - just a tag *)
	    else (case nontagcount of
		      0 => error "Cannot get here"
		    | 1 => xinject_sum_dynamic_single(info,con_varloc,exp_varloc,trace)
		    | _ => xinject_sum_dynamic_multi(info,con_varloc,exp_varloc,trace))
	end

  fun xproject_sum_dynamic (info, sumtypeReg, exp_ir, traceinfo) : term * state = 
     let val _ = Stats.counter("RTLprojsum") ()
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  val (_,dest as I desti) = alloc_reg_trace state traceinfo

	  fun single_case() = 
	      let val afterl = fresh_code_label "projsum_single_after"

		  (* Perform the branching with fall-through to nobox case *)
		  val fieldtypeReg = projectFieldFromSum (sumtypeReg, tagcount, nontagcount, known)
		  val (boxl,noboxl) = needs_boxing_dynamic fieldtypeReg
		  (* no box case *)
		  val _ = (add_instr(ILABEL noboxl);
			   add_instr(MV(exp_ir,desti));
			   add_instr(BR afterl))
		  (* box case *)
		  val _ = (add_instr(ILABEL boxl);
			   record_project(exp_ir,0,desti))
		  (* afterwards *)
		  val _ = add_instr(ILABEL afterl)
	      in  state
	      end

	  fun multi_case() = 
	      let val _ = record_project(exp_ir,1,desti)
	      in  state
	      end
	  val state = (case (tagcount,nontagcount) of
			   (_, 0) => error "xproject_sum_dybnamic projecting from non-carrier"
			 | (_, 1) => single_case()
			 | _ => multi_case())

	  val lv = LOCATION(REGISTER (false, dest))
      in  (lv,state)
      end


  fun xproject_sum_static (info, base, niltrace) : term * state = 
      let val lv = LOCATION(REGISTER(false, I base))
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  fun unbox offset =
	      let val (_,I desti) = alloc_reg_trace state niltrace
		  val _ = record_project(base,offset,desti)
	      in  (LOCATION(REGISTER(false, I desti)), state)
	      end

      in  (case (tagcount,nontagcount) of
	       (_, 0) => error "xproject_sum_static projectin from non-carrier"
	     | (_, 1) => let val summand_type = (List.nth(sumtypes,field_sub)
						 handle _ => error "bad project_sum: record_con")
			 in  if (needs_boxing state summand_type)
				 then unbox 0
			     else (lv, state)
			 end
	     | _ => unbox 1)
      end
	       


end

