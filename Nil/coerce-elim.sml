(*$import Nil NilRewrite Stats COERCEELIM Ppnil NilStatic NilRename List Int Name Sequence NilRename *)

structure CoerceElim :> COERCEELIM =
  struct

    val debug = Stats.ff "coerce_elim_debug"

    local
      open NilRewrite
      structure N = Nil
    
      type state = int ref

      fun c_inner_arrow (from_con,to_con) =
	let
	  val inner_arrow = N.AllArrow_c {openness    = N.Open,
					  effect      = N.Total,
					  tFormals    = [],
					  eFormals    = [from_con],
					  fFormals    = 0w0,
					  body_type   = to_con}
	in inner_arrow
	end

      fun coercion2arrow (vars,from_con,to_con) = 
	let 
	  val tformals = map (fn v => (v,N.Type_k)) vars
	  val inner_arrow = c_inner_arrow (from_con,to_con)
	    
	  val arrow = N.AllArrow_c {openness    = N.Open,
				    effect      = N.Total,
				    tFormals    = tformals,
				    eFormals    = [],
				    fFormals    = 0w0,
				    body_type   = inner_arrow}
	in arrow
	end

      fun conhandler (i : state, c : N.con) = 
	(case c
	   of N.Coercion_c {vars=vars,from=from_con,to=to_con} => 
	     let 
	       val arrow = coercion2arrow(vars,from_con,to_con)
	       val _ = 
		 if !debug then (
		   print "\nRewrote ";
		   Ppnil.pp_con c;
		   print "\n to ";
		   Ppnil.pp_con arrow;
		   print "\n"
		 ) else ()
	     in
	       i := !i +1;
	       CHANGE_RECURSE (i,arrow)
	     end
	    | _ => NOCHANGE)

      fun exphandler (i : state, e : N.exp) =
	let
	  fun dofoldunfold name (cvars,from_con,to_con) = 
	    let
	      val arrow= coercion2arrow(cvars,from_con,to_con)
	      val arrow = NilRename.renameCon arrow
	      val coercion = NilRename.renameExp e

	      val inner_arrow = c_inner_arrow (from_con,to_con)
		
	      val fun_name = Name.fresh_named_var name
	      val inner_fun_name = Name.fresh_named_var (name^"_inner")
	      val arg_name = Name.fresh_named_var (name ^"_arg")

	      val body = N.Coerce_e(coercion,map N.Var_c cvars,N.Var_e arg_name)

	      val inner_lambda = N.Function {effect = N.Total,
					     recursive = N.Leaf,
					     tFormals = [],
					     eFormals = [(arg_name,N.TraceUnknown)],
					     fFormals = [],
					     body = body}

	      val body = N.Let_e (N.Sequential,[N.Fixopen_b ([((inner_fun_name,inner_arrow),inner_lambda)])],N.Var_e inner_fun_name)

	      val lambda = N.Function {effect = N.Total,
				       recursive = N.Leaf,
				       tFormals = cvars,
				       eFormals = [],
				       fFormals = [],
				       body = body}
		
	      val e' = N.Let_e (N.Sequential,[N.Fixopen_b ([((fun_name,arrow),lambda)])],N.Var_e fun_name)
	    in
	      i := !i + 1;
	      CHANGE_NORECURSE (i,e')
	    end
	in
	  case e
	    of N.Coerce_e (coercion,cargs,earg) =>
	      let
		val e' = N.App_e(N.Open,N.App_e(N.Open,coercion,cargs,[],[]),[],[earg],[])
	      in
		i := !i + 1;
		CHANGE_RECURSE (i,e')
	      end
	     | N.Fold_e args => dofoldunfold "fold" args
	     | N.Unfold_e args => dofoldunfold "unfold" args
	     | _ => NOCHANGE
	end

	     

      val all_handlers = 
	let
	  val h = set_conhandler default_handler conhandler
	  val h = set_exphandler h exphandler
	in h
	end

      val {rewrite_con,rewrite_exp,rewrite_kind,rewrite_mod,...} = 
	rewriters all_handlers

    in
      fun transform nilmod =
	let 
	  val i = ref 0
	  val nilmod = rewrite_mod i nilmod
	in 
	  if !i > 0 then 
	    List.app print [Int.toString (!i)," Nodes rewritten\n"] 
	  else 
	    ();
	  nilmod
	end

    end

  end
