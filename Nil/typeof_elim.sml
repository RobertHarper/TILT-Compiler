(*$import Nil TypedNilRewrite Stats TYPEOF_ELIM NilContext Normalize Ppnil NilStatic NilRename NilUtil List Int Real Measure *)


structure Typeof_Elim :> TYPEOF_ELIM = 
  struct

    val debug = Stats.ff "typeof_elim_debug"
    val leave_toplevel_typeofs = Stats.ff "leave_toplevel_typeofs"
    val report = Stats.ff "typeofShowSize"
    val max = Stats.int "TypeofMaxInc"
    local
      
      open TypedNilRewrite

      type state = (NilContext.context * (int ref) * bool)

      fun conhandler ((D,i,eliminate) : state, c : Nil.con) = 
	(case (eliminate,c)
	   of (false,Nil.Typeof_c e) => 
	     let 
	       val c = NilRename.renameCon(Normalize.type_of(D,e))
	       val _ = 
		 if !debug then 
		   (
		    print "\nRewrote ";
		    Ppnil.pp_con (Nil.Typeof_c e);
		    print "\n to ";
		    Ppnil.pp_con c;
		    print "\n"
		    ) else ()
	     in
	       i := !i +1;
	       CHANGE_RECURSE ((D,i,eliminate),c)
	     end
	    | _ => NOCHANGE)

      fun cbndhandler ((D,i,eliminate) : state, cb : Nil.conbnd) = 
	if eliminate then
	  (case cb
	     of Nil.Open_cb _ => CHANGE_RECURSE((D,i,false),[cb])
	      | Nil.Code_cb _ => CHANGE_RECURSE((D,i,false),[cb])
	      | _ => NOCHANGE)
	else NOCHANGE

      fun bndhandler ((D,i,eliminate) : state, b : Nil.bnd) = 
	if eliminate then
	  (case b
	     of Nil.Fixopen_b _ => CHANGE_RECURSE((D,i,false),[b])
	      | Nil.Fixcode_b _ => CHANGE_RECURSE((D,i,false),[b])
	      | _ => NOCHANGE)
	else NOCHANGE

      val exp_definer = NilContext.insert_exp_pre(fn (D,e) => (Normalize.type_of(D,NilRename.renameExp e))) 
      fun sum_binder (D,v,(sumtype,known)) = 
	let
	  val sumtype = #2 (Normalize.reduce_hnf (D,sumtype))
	  val sum = NilUtil.convert_sum_to_special (sumtype,known)
	in NilContext.insert_con (D,v,NilRename.renameCon sum)
	end

      fun exn_binder (D,v,exntag) = 
	let val c = valOf (NilUtil.strip_exntag(#2 (Normalize.reduce_hnf(D,Normalize.type_of(D,exntag)))))
	in NilContext.insert_con (D,v,c)
	end

      fun label_binder ((D,i,t) : state,l : Nil.label,v : Nil.var) = (NilContext.insert_label (D,l,v),i,t)

      fun none f = fn ((D,i,t) : state,v : Nil.var ,item : 'item) => ((f (D,v,item),i,t), NONE)

      val all_handlers = 
	HANDLER {bndhandler     = bndhandler,
		 cbndhandler    = cbndhandler,
		 conhandler     = conhandler,
		 exphandler     = null_handler,
		 kindhandler    = null_handler,
		 tracehandler   = null_handler,
		 con_var_bind   = none NilContext.insert_kind,
		 con_var_define = none NilContext.insert_equation,
		 exp_var_bind   = none NilContext.insert_con,
		 exp_var_define = none exp_definer,
		 sum_var_bind   = none sum_binder,
		 exn_var_bind   = none exn_binder,
		 labelled_var   = label_binder
		 }

      val {rewrite_con,rewrite_exp,rewrite_kind,rewrite_mod,...} = rewriters all_handlers
    in
      fun mod_elim D nilmod = 
	let 
	  val nilmod = if !debug then 
	    if NilRename.isRenamedMod nilmod then nilmod
	    else (print "\nWarning: got an unrenamed module\n";
		  NilRename.renameMod nilmod)
		       else nilmod
	  val i = ref 0
	  val nilmod' = rewrite_mod (D,i,!leave_toplevel_typeofs) nilmod
	  val _ = 
	    if !report then
	      if !i > 0 then 
		let 
		  val old = Measure.mod_size {cstring = Measure.cstring,count = [],count_in = []} nilmod
		  val new = Measure.mod_size {cstring = Measure.cstring,count = [],count_in = []} nilmod'
		  val pct = Real.fromInt(#total new) / Real.fromInt(#total old) * 100.0
		  val pcti = Real.round pct
		  val _ = if pcti > !max then max := pcti else ()
		in
		  List.app print [Int.toString (!i)," Typeof_c nodes rewritten\n",
				  "New module is ", Int.toString pcti, "% of old module\n"
				  ]
		end
	      else print "Typeof elimination made no changes\n"
	    else ()


	in 

	  nilmod'
	end
    end
  end

  
