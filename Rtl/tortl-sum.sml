(*$import Prelude TopLevel Util Name List Int Prim TilWord32 TORTLSUM Rtl TortlBase TortlRecord Rtltags Nil NilUtil Ppnil Stats *)

(* ASSUMPTIONS and GUARANTEES:
	(1) Integer types are represented by the tags 0 to 3.
	(2) (Special) Sum types are represetnted by a pointer to a record whose fields are
		(a) tag 7 to indicate sum or special sum type
		(b) known - indicates the type of special sum - ~1 if not known
		(c) tagcount - number of non-value-varrying components
		(d) total - total number of components
		(e) type argument(s)
		      if there are no type arguments, then this will be an empty crecord
		      if there is one type argument, then this will be the type argument      (what? -joev)
		      if there are more than one type arguments, then this will be a pointer 
			  to the tuple of the summand types
	(3) Mu types are represented by a record whose first field is 8.	
		There are currently no other fields since mu's are not fully represented.  
	(4) The representation of values of sum and mu types will always be a pointer
	        though that might include small tag values.
*)

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
	


structure TortlSum :> TORTL_SUM =

struct

    open Nil 
    open Rtl 
    open TortlBase 
    structure TR = TortlRecord

    val debug = ref false
    val error = fn s => (Util.error "tortl-sum.sml" s)

    val w2i = TilWord32.toInt
    val i2w = TilWord32.fromInt

    type typearg = state * TilWord32.word * con

    (* Extracts a bunch of useful facts about a sumcon. *)
    (* State remains unchanged across call. Leaf - 3/22/02
     *)
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

    (* Check if values of the field_type might look like an int and thus require boxing
     *)
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


    (* Emits code to check whether values of the carriedType might look like an int and 
     thus require boxing.  Returns a pair of labels (boxl,noboxl); if boxing is required,
     this code will jump to boxl; if not, it will jump to noboxl.  It's the responsibility
     of whoever called this function to actually emit those labels.
       joev, 8/2002
     *)
    fun needs_boxing_dynamic carriedType =
	let
	  val tag = alloc_regi NOTRACE_INT		  
	  val boxl = fresh_code_label "dynamic_box"
	  val noboxl = fresh_code_label "dynamic_nobox"
	  val _ = 
	      (add_instr(BCNDUI(LE, carriedType, IMM 4, boxl, false));     (* check for int types *)
	       add_instr(BCNDUI(LE, carriedType, IMM 255, noboxl, false)); (* check for other small types *)
	       record_project(carriedType, 0, tag);
	       add_instr(BCNDSI(EQ, tag, IMM 12, boxl, false));               (* check for exntags *)
	       add_instr(BCNDSI(EQ, tag, IMM 4, boxl, false));                (* check for sums *)
	       add_instr(BCNDSI(EQ, tag, IMM 8, boxl, false)))                (* check mus *)
	in  (boxl, noboxl)
	end

    (* projectFieldFromSum : regi * int * int * int -> regi   *)
    (* Returns a register r, and emits code with the following spec:   *)
    (*      PRE: sumtype contains a sum constructor (see description at top) *)
    (*           nontagcount and known correctly describe sumtype.           *)
    (*      POST: r contains the type of the injected value in a value of    *)
    (*            the type in sumtype.                                       *)
    (* XXX tagcount is not used.                                             *)
    (*        joev 8/2002                                                    *) 
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
    

  fun xinject_sum_static (info, terms, trace) : term * state =
      let
	val  (state,known,sumcon,
	      tagcount,nontagcount,single_carrier,is_tag,
	      sumtypes,field_sub) = help info

	(* For when we are injecting into the sole carrier.                          *)
	(* If the injected value doesn't look like a tag, then injection is a no-op. *)
	(* If it might, then we have to box it.                                      *)
	(*    joev                                                                   *)
	fun single () = 
	  if (needs_boxing state (List.nth (sumtypes,field_sub))) then 
	    (* I suppose [hd (tl terms)] is the same as (tl terms), since *)
	    (* terms will have exactly 2 elements.                        *)
	    TR.make_record_with_tag(state,hd terms,[hd (tl terms)])
	  else (hd terms,state)
	    
	  
	(* For when we are injecting into one of multiple carriers. *)
	(* Make a record containing the index of the injection and the value. *)
        (*    joev                                                            *)
	fun multi () =
	    let val [tagword,term] = terms
	    in TR.make_record_with_tag(state,tagword,[VALUE (INT (i2w field_sub)),term])
	    end

      in  (case (is_tag, nontagcount) of
	     (true, _) => if (known < 256) 
			  then (VALUE(TAG (i2w known)), state)
			  else error ("Tag too large " ^ (Int.toString known))
	   | (_, 0) => error "Can't get here"
	   | (_, 1) => single()
	   | _ => multi())
      end

  (* Create the gctag for an injection, based on the niltrace of the injected value. *)
  (*    joev 8/2002                                                                  *)
    fun make_sum_tag_static (info,tr) : term = 
	let
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	in
	  case (is_tag,nontagcount) of
	    (true,_) => VALUE(INT 0w0) (* Tag will not be used *)
	  | (_,0) => VALUE(INT 0w0) (* This one will never happen *)
	  | (_,1) =>
	    let
	      val field_type = List.nth(sumtypes, field_sub)
	    in  if (needs_boxing state field_type)
		then 
		  TR.record_tag_from_reps([niltrace2rep state tr])
		else 
		  VALUE(INT 0w0) (* Tag will not be used *)
		                 (* This case should never happen!*)
	    end
	  | _ =>
	    let 
		val field_type = List.nth(sumtypes, field_sub)
		val rep = niltrace2rep state tr
	    in TR.record_tag_from_reps [NOTRACE_INT,rep]
	    end
      end	     


    (* Translate an injection into the sole carrying arm of a sum type, where *)
    (* the type of the injected value is not statically known.                *)
    (* Generated code must check whether argument needs to be boxed or not.   *)
    (*   joev 8/2002  *)
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
	  
	  (* Perform the branching with fall-through to nobox case *)
	  val fieldtypeReg = projectFieldFromSum (sumtypeReg, tagcount, nontagcount, known)
	  val (boxl,noboxl) = needs_boxing_dynamic fieldtypeReg

	  (* nobox case *)
	  val _ = add_instr(ILABEL noboxl)
	  val _ = load_ireg_term(exp_varloc,SOME desti)
	  val _ = add_instr(BR afterl)
	  (* box case *)
	  val _ = add_instr(ILABEL boxl)
	  val (lv,state) = TR.make_record(state,[exp_varloc])
	  val rec_ir = load_ireg_term(lv,NONE)
	  val _ = add_instr(MV(rec_ir,desti))

	  (* afterwards *)
	  val _ = add_instr(ILABEL afterl)
	      
      in  (LOCATION(REGISTER(false, I desti)),state)
      end

    (* Translate an injection into the sole carrying arm of a sum type, where *)
    (* the type of the injected value is not statically known.                *)
    (*   joev 8/2002  *)
    fun xinject_sum_dynamic_multi (info,
			    con_varloc,
			    exp_varloc,
			    trace) : term * state = 
	let val _ = Stats.counter("RTLxinject_sum_dyn_multi") ()
	    
	  val  (state,known,sumcon,
		tagcount,nontagcount,single_carrier,is_tag,
		sumtypes,field_sub) = help info

	  val dest = alloc_regi(niltrace2rep state trace)
	  val (result,state1) = TR.make_record(state,[VALUE(INT (i2w field_sub)),exp_varloc])
      in  (result,state)  (* XXX: Why not state1?  Is it the same? *)
      end

    (* Translate an injection where the type of the injected value is not known. *)
    (*   joev 8/2002                                                             *)
    fun xinject_sum_dynamic (info,
		      con_varloc,
		      exp_varloc,
		      trace) : term * state = 
	let
	    val  (state,known,sumcon,
		  tagcount,nontagcount,single_carrier,is_tag,
		  sumtypes,field_sub) = help info
	in  if (known < tagcount)
		then xinject_sum_static(info, [], trace)   (* Not really dynamic - just a tag *)
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
	  val state = (case nontagcount of
			   0 => error "xproject_sum_dynamic projecting from non-carrier"
			 | 1 => single_case()
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

