(*$import Prelude TopLevel Nil TypedNilRewrite Stats TYPEOF_ELIM NilContext Normalize Ppnil NilStatic NilRename NilUtil List Int *)


structure Typeof_Elim :> TYPEOF_ELIM = 
  struct

    val debug = Stats.ff "typeof_elim_debug"
    local 
      
      open TypedNilRewrite

      type state = (NilContext.context * (int ref))

      fun conhandler ((D,i) : state, c : Nil.con) = 
	(case c
	   of Nil.Typeof_c e => 
	     let 
	       val c = NilRename.renameCon(Normalize.type_of(D,e))
	       val _ = 
		 if !debug then (
		   print "\nRewrote ";
		   Ppnil.pp_con (Nil.Typeof_c e);
		   print "\n to ";
		   Ppnil.pp_con c;
		   print "\n"
		 ) else ()
	     in
	       i := !i +1;
	       CHANGE_RECURSE ((D,i),c)
	     end
	    | _ => NOCHANGE)

      val exp_definer = NilContext.insert_exp_pre(fn (D,e) => (Normalize.type_of(D,NilRename.renameExp e))) 
      fun sum_binder (D,v,(sumtype,known)) = 
	let
	  val sumtype = #2 (Normalize.reduce_hnf (D,sumtype))
	  val sum = NilUtil.convert_sum_to_special (sumtype,known)
	in NilContext.insert_con (D,v,NilRename.renameCon sum)
	end

(* NilContext.insert_exp)*)
      val all_handlers =  
	let
	  fun none f = fn ((D,i) : state,v : Nil.var ,item : 'item) => ((f (D,v,item),i), NONE)
	  fun label_binder ((D,i) : state,l : Nil.label,v : Nil.var) = (NilContext.insert_label (D,l,v),i)
	  val h = set_conhandler default_handler conhandler
	  val h = set_con_binder h (none NilContext.insert_kind)
	  val h = set_con_definer h (none NilContext.insert_equation)
	  val h = set_exp_binder h (none NilContext.insert_con)
	  val h = set_exp_definer h (none exp_definer)
          val h = set_sum_binder h (none sum_binder)
	  val h = set_label_binder h label_binder
	in
	  h
	end

      val {rewrite_con,rewrite_exp,rewrite_kind,rewrite_mod,...} = rewriters all_handlers
    in
      fun mod_elim D nilmod = 
	let 
	  val i = ref 0
	  val nilmod = rewrite_mod (D,i) nilmod
	in 
	  if !i > 0 then List.app print [Int.toString (!i)," Typeof_c nodes rewritten\n"] else ();
	  nilmod
	end
    end
  end

  