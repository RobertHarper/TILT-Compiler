(*$import Prelude TopLevel Nil TypedNilRewrite Stats COERCEELIM Ppnil NilStatic NilRename List Int Name Sequence *)

structure CoerceElim :> COERCEELIM =
  struct

    val debug = Stats.ff "coerce_elim_debug"

    local
      open TypedNilRewrite
      structure N = Nil
    
      type state = int ref

      fun conhandler (i : state, c : N.con) = 
	(case c
	   of N.Coercion_c {vars=vars,from=from_con,to=to_con} => 
	     let 
	       val tformals = map (fn v => (v,N.Type_k)) vars
	       val arrow = N.AllArrow_c {openness    = N.Open,
					   effect      = N.Total,
					   isDependent = false,
					   tFormals    = tformals,
					   eFormals    = [(NONE,from_con)],
					   fFormals    = 0w0,
					   body_type   = to_con}
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
	(case e
	   of N.Coerce_e (coercion,cargs,earg) =>
	     let
	       val e' = N.App_e(N.Open,coercion,cargs,[earg],[])
	     in
	       i := !i + 1;
	       CHANGE_RECURSE (i,e')
	     end
	 | N.Fold_e (cvars,from_con,to_con) =>
	     let
	       val tformals = map (fn v => (v,N.Type_k)) cvars

	       val fun_name = Name.fresh_named_var "fold"
	       val arg_name = Name.fresh_named_var "fold_arg"
	       val body = N.Prim_e(N.NilPrimOp N.roll, [to_con],[N.Var_e arg_name])
	       val lambda = N.Function {effect = N.Total,
					  recursive = N.Leaf,
					  isDependent = false,
					  tFormals = tformals,
					  eFormals = [(arg_name,N.TraceUnknown,from_con)],
					  fFormals = [],
					  body = body,
					  body_type = to_con}
	       val e' = N.Let_e (N.Sequential,[N.Fixopen_b (Sequence.fromList [(fun_name,lambda)])],N.Var_e fun_name)
	     in
	       i := !i + 1;
	       CHANGE_RECURSE (i,e')
	     end
	 | N.Unfold_e (cvars,from_con,to_con) =>
	     let
	       val tformals = map (fn v => (v,N.Type_k)) cvars

	       val fun_name = Name.fresh_named_var "unfold"
	       val arg_name = Name.fresh_named_var "unfold_arg"
	       val body = N.Prim_e(N.NilPrimOp N.unroll, [from_con],[N.Var_e arg_name])
	       val lambda = N.Function {effect = N.Total,
					  recursive = N.Leaf,
					  isDependent = false,
					  tFormals = tformals,
					  eFormals = [(arg_name,N.TraceUnknown,from_con)],
					  fFormals = [],
					  body = body,
					  body_type = to_con}
	       val e' = N.Let_e (N.Sequential,[N.Fixopen_b (Sequence.fromList [(fun_name,lambda)])],N.Var_e fun_name)
	     in
	       i := !i + 1;
	       CHANGE_RECURSE (i,e')
	     end
	 | _ => NOCHANGE)
	     

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