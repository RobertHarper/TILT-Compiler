    (* Here we define the abstract interface for substitutions.  This section 
     * defines the bits that are generic to all levels in a functor parameterized 
     * by the actual substitution functions, which are level specific. 
     *)

    functor SubstFn(type item                                         (*What is it: e.g. con, exp *)
		    type item_subst = item Name.VarMap.map                 (*The substitution type  *)
		    val substItemInItem : item_subst -> item -> item  (*For composition *)
		    val renameItem : item -> item)                    (*To avoid shadowing *)
      :> SUBST where type item = item
		 and type item_subst = item_subst =
    struct
      
      type var = Name.var
      type item = item
      type item_subst = item_subst

      structure VarMap = Name.VarMap

      fun empty () : item_subst = VarMap.empty
	
      fun substitute subst var = VarMap.find (subst,var)
	
      fun toList (subst : item_subst) = VarMap.listItemsi subst
	
      fun sim_add subst (var,value) : item_subst = VarMap.insert (subst,var,renameItem value) 
      
      fun addl (var,item,subst) = 
	let
	  val item = renameItem item
	  val map_subst = VarMap.insert (empty(),var,item)
	in VarMap.insert (VarMap.map (substItemInItem map_subst) subst,var,item)
	end
      
      fun addr  (subst,var,item) = 
	VarMap.insert (subst,var,substItemInItem subst (renameItem item))

      fun is_empty subst = (VarMap.numItems subst) = 0
	
      fun compose (subst1,subst2) = 
	let
	  val subst2 = VarMap.map (substItemInItem subst1) subst2
	  val subst = VarMap.unionWith #2 (subst1,subst2)
	in subst
	end
      
      fun merge (subst1,subst2) = VarMap.unionWith #2 (subst1,subst2)

      fun simFromList (list : (var * item) list) : item_subst = 
	let
	  fun fold ((var,value),subst) = 
	    VarMap.insert(subst,var,renameItem value)
	    
	  val subst =  List.foldl fold VarMap.empty list
	in subst
	end

      fun seqFromList (list : (var * item) list) : item_subst = 
	let
	  fun fold ((var,value),subst) = 
	    VarMap.insert (subst,var,substItemInItem subst (renameItem value))
	  val subst =  List.foldl fold VarMap.empty list
	in subst
	end

      fun printf (printer : item -> unit) (subst: item_subst) = 
	let
	  fun print1 (v,a) = 
	    (TextIO.print (Name.var2string v);
	     TextIO.print "->";
	     printer a;
	     TextIO.print "\n")
	in
	  (Util.lprintl "Substitution is";
	   VarMap.appi print1 subst;
	   Util.printl "")
	end

    end
