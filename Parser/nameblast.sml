(*$import Name NAMEBLAST Blaster Util Symbol Int Word31 *)

structure NameBlast :> NAMEBLAST =
  struct

    open Util
    open Name
    open Blaster
    val error = fn s => error "nameblast.sml" s

    fun blastOutVar os v =
	let val n = var2int v
	in  blastOutInt os n
	end

    fun blastInVar int2var is = 
	let val n = blastInInt is
	in  int2var n
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

    fun blastInPath int2var is = let val v = blastInVar int2var is
			     val labs = blastInList blastInLabel is
			 in  (v,labs)
			 end

    fun blastOutVarmap os blaster vmap = 
	blastOutList (blastOutPair blastOutVar blaster) os (VarMap.listItemsi vmap)

    fun blastOutPathmap os blaster pmap = 
	blastOutList (blastOutPair blastOutPath blaster) os (PathMap.listItemsi pmap)

    fun blastInVarmap int2var is blaster =
	let val ls = blastInList (blastInPair (blastInVar int2var) blaster)  is
	    fun folder((v,item),acc) = VarMap.insert(acc,v,item)
	    in  foldl folder VarMap.empty ls
	end

    fun blastInPathmap int2var is blaster =
	let val ls = blastInList (blastInPair (blastInPath int2var) blaster)  is
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


