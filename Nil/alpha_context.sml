(*$import Name Nil Listops ALPHA Option ListPair *)

(*
 Contexts and routines for alpha-renaming of variables
*)

structure Alpha
    :> ALPHA =
  struct

    structure VarMap = Name.VarMap

    open Nil

    type var = Name.var
    val eq_var  = Name.eq_var
    fun fresh_var() = Name.fresh_named_var "alpha"
    val foldl_acc = Listops.foldl_acc


    type alpha_context = var VarMap.map

    (*
     val renamed : alpha_context * var -> bool
	 renamed (ctxt, v) ==> true iff v is renamed to a different variable in ctxt
    *)
    fun renamed (context,var) = 
      (case VarMap.find (context,var)
	 of NONE => false
	  | SOME var' => not (eq_var (var,var')))

    (*
     val is_empty : alpha_context -> bool
	 is_empty ctxt ==> true iff ctxt is empty
    *)
    fun is_empty a = (VarMap.numItems a) = 0

    (*
     val bound : alpha_context * var -> bool 
	 bound (ctxt, v) ==> true iff v is bound in ctxt
    *)
    val bound : alpha_context * var -> bool = Option.isSome o VarMap.find

    (*
     val substitute : alpha_context * var -> var
	 substitute (ctxt, v) ==> the replacement variable assigned to v by ctxt
    *)
    fun substitute (bindings,var) = (Option.getOpt (VarMap.find (bindings,var),var))

    (*
     val rename : alpha_context * var * var -> alpha_context 
	 rename (ctxt, v1, v2) ==> ctxt modified to replace v1 with v2
    *)
    val rename : alpha_context * var * var -> alpha_context = VarMap.insert

    (*
     val bind : alpha_context * var -> alpha_context
	 bind (ctxt, v) ==> ctxt modified to include v as a bound variable with no replacement
    *)
    fun bind (context,var) = VarMap.insert (context,var,var)

    (*
     val unbind : alpha_context * var -> alpha_context
	 unbind (ctxt, v) ==> ctxt with any binding/renaming for v removed
    *)
    val unbind : alpha_context * var -> alpha_context = #1 o VarMap.remove

    (*
     val empty_context : unit -> alpha_context
	 empty_context () => an alpha context with no bound or renamed variables
    *)
    fun empty_context () : alpha_context = VarMap.empty

    (*
     val alpha_bind : (alpha_context * var) -> (alpha_context * var)
	 alpha_bind (ctxt, v) ==> (ctxt modified to rename v to a fresh variable,
	                           the fresh variable to which v is renamed)
    *)
    fun alpha_bind (context,var) = 
      let 
	val newvar = fresh_var ()
      in (VarMap.insert (context,var,newvar),newvar)
      end

    (*
     val alpha_bind_list : (alpha_context * (var list)) -> (alpha_context * (var list))
	 alpha_bind_list (ctxt, vlist) ==> (ctxt modified to rename each variable in vlist to a fresh variable,
					    the variables to which the members of vlist have been renamed, in order)
    *)
    fun alpha_bind_list (context,vars) = 
      let
	fun fold_one (var,context) = 
	  let 
	    val (context',var') = alpha_bind (context,var) 
	  in (var',context') 
	  end
	val (vars',context') = foldl_acc fold_one context vars
      in
	(context',vars')
      end

    (*
     val alpha_equate_pair : (alpha_context * alpha_context) * (var * var)
	 -> alpha_context * alpha_context
	 alpha_equate_pair ((ctxt1, ctxt2), (v1, v2)) ==> (ctxt1', ctxt2'),
	     where ctxt1' and ctxt2' are created from ctxt1 and ctxt2 by making any changes needed to ensure that
	     ctxt1 treats v1 the same way that ctxt2 treats v2
    *)
    fun alpha_equate_pair ((subst1: alpha_context, subst2 :alpha_context), 
			   (var1, var2)) = 
      if not (eq_var (var1,var2)) then 
	let
	  val var = fresh_var()
	in
	  (rename(subst1,var1,var),rename(subst2,var2,var))
	end
      else 
	(bind(subst1,var1),bind(subst2,var2))

    (*
     val alpha_equate_pairs :
      (alpha_context * alpha_context) * (var list * var list)
           -> alpha_context * alpha_context
      alpha_equate_pairs ((ctxt1, ctxt2), (vlist1, vlist2)) ==> the result of folding alpha_equate_pair over vlist1 and vlist2
	  taken as successive pairs, using ctxt1 and ctxt2 as the starting contexts
    *)
    fun alpha_equate_pairs ((subst1,subst2), (var_list1,var_list2)) =
      let
	fun flip (var1,var2,context) = (context,(var1,var2))
      in
	ListPair.foldl (alpha_equate_pair o flip) (subst1,subst2) (var_list1,var_list2) 
      end
    
    (*
     val alpha_pair_eq : (alpha_context * alpha_context) * (var * var) -> bool
	 alpha_pair_eq ((ctxt1, ctxt2), (v1, v2)) ==> true iff v1 under ctxt1's renamings and v2 under ctxt2's renamings are
	     equal
    *)
    fun alpha_pair_eq ((subst1,subst2),(var1,var2)) = 
      eq_var (substitute (subst1,var1),substitute (subst2,var2))
     
  end
