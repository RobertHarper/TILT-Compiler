functor Alpha(structure ArgNil : NIL) :(*>*)
   ALPHA where Nil = ArgNil =
  struct

    structure Nil = ArgNil

    structure VarMap = Name.VarMap

    open Nil

    type var = Name.var
    val eq_var  = Name.eq_var
    val fresh_var = Name.fresh_var
    val foldl_acc = Listops.foldl_acc


    type alpha_context = var VarMap.map

    fun renamed (context,var) = 
      (case VarMap.find (context,var)
	 of NONE => false
	  | SOME var' => not (eq_var (var,var')))

    val bound : alpha_context * var -> bool = isSome o VarMap.find

    fun substitute (bindings,var) = (getOpt (VarMap.find (bindings,var),var))

    val rename : alpha_context * var * var -> alpha_context = VarMap.insert 

    fun bind (context,var) = VarMap.insert (context,var,var)

    fun first (a,b) = a

    val unbind : alpha_context * var -> alpha_context = first o VarMap.remove

    fun empty_context () : alpha_context = VarMap.empty

    fun alpha_bind (context,var) = 
      let 
	val newvar = fresh_var ()
      in (VarMap.insert (context,var,newvar),newvar)
      end

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

    fun alpha_equate_pairs ((subst1,subst2), (var_list1,var_list2)) =
      let
	fun flip (var1,var2,context) = (context,(var1,var2))
      in
	ListPair.foldl (alpha_equate_pair o flip) (subst1,subst2) (var_list1,var_list2) 
      end
    
    fun alpha_pair_eq ((subst1,subst2),(var1,var2)) = 
      eq_var (substitute (subst1,var1),substitute (subst2,var2))
     
  end