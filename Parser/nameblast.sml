(*$import Name NAMEBLAST Blaster Util Symbol Int32 Word31 *)

structure NameBlast :> NAMEBLAST =
  struct

    open Util
    open Name
    open Blaster
    val error = fn s => error "nameblast.sml" s

    val varMap = ref (Name.VarMap.empty : Name.var Name.VarMap.map)
    fun resetVarMap() = varMap := Name.VarMap.empty
    fun addMap(v,v') = (case (Name.VarMap.find(!varMap,v)) of
			    NONE => varMap := (Name.VarMap.insert(!varMap,v,v'))
			  | SOME _ => (error "addMap called on variable already in map"))
    fun lookupMap v = Name.VarMap.find(!varMap,v)


    fun blastOutVar os v =
	let val (n,str) = deconstruct_var v
	in  blastOutInt os n
	end

    fun blastInVar is = 
	let val n = blastInInt is
	    val (inUse, v) = construct_var(n, "")
	in  (case lookupMap v of
		 SOME v' => v'
	       | NONE => let val v' = if inUse then Name.derived_var v else v
			     val _ = addMap(v,v')
(*
			     val _ = if (eq_var(v,v'))
					 then ()
				     else (print "blastInVar alpha-varying: ";
					   print (Name.var2string v); 
					   print "  -> "; 
					   print (Name.var2string v'); 
					   print "\n")
*)
			 in  v'
			 end)
	end
    
    fun blastOutTag os t =
	let val (n,str) = deconstruct_tag t
	in  (blastOutInt os n;
	     blastOutString os str)
	end

    fun blastInTag is = 
	let val n = blastInInt is
	    val str = blastInString is
	in  construct_tag(n, str)
	end

    fun blastOutLabel os label = 
	let val (n,str) = deconstruct_label label
	in  (blastOutInt os n;
	     blastOutString os str)
	end

    fun blastInLabel is = 
	let val n = blastInInt is
	    val str = blastInString is
	in  construct_label(n, str)
	end
    
    fun blastOutPath os (v,labs) = (blastOutVar os v; blastOutList blastOutLabel os labs)

    fun blastInPath is = let val v = blastInVar is
			     val labs = blastInList blastInLabel is
			 in  (v,labs)
			 end

    fun blastOutVarmap os blaster vmap = 
	blastOutList (blastOutPair blastOutVar blaster) os (VarMap.listItemsi vmap)

    fun blastOutPathmap os blaster pmap = 
	blastOutList (blastOutPair blastOutPath blaster) os (PathMap.listItemsi pmap)

    fun blastInVarmap is blaster =
	let val ls = blastInList (blastInPair blastInVar blaster)  is
	    fun folder((v,item),acc) = VarMap.insert(acc,v,item)
	    in  foldl folder VarMap.empty ls
	end

    fun blastInPathmap is blaster =
	let val ls = blastInList (blastInPair blastInPath blaster)  is
	    fun folder((p,item),acc) = PathMap.insert(acc,p,item)
	    in  foldl folder PathMap.empty ls
	end
    
    fun blastOutLabelmap os blaster vmap = 
	blastOutList (blastOutPair blastOutLabel blaster) os (LabelMap.listItemsi vmap)
    fun blastInLabelmap is blaster =
	let val ls = blastInList (blastInPair blastInLabel blaster)  is
	    fun folder((v,item),acc) = LabelMap.insert(acc,v,item)
	in  foldl folder LabelMap.empty ls
	end
    

    fun blastOutTagmap os blaster vmap = 
	blastOutList (blastOutPair blastOutTag blaster) os (TagMap.listItemsi vmap)
    fun blastInTagmap is blaster =
	let val ls = blastInList (blastInPair blastInTag blaster)  is
	    fun folder((v,item),acc) = TagMap.insert(acc,v,item)
	in  foldl folder TagMap.empty ls
	end

    
end


