(*$import Name NAMEBLAST Blaster Util Symbol Int32 Word31 *)

structure NameBlast :> NAMEBLAST =
  struct

    structure Util = Util
    open Util
    open Name
    open Blaster

    val error = fn s => error "nameblast.sml" s

	
    fun blastOutVar os v =
	let val (n,str) = deconstruct_var v
	in  (blastOutInt os n;
	     if (!useOldBlast)
		 then blastOutString os str
	     else blastOutString os "")
	end

    fun blastInVar is = 
	let val n = blastInInt is
	    val _ = update_var_counter n
	    val str = blastInString is
	in  construct_var(n, str)
	end
    
    fun blastOutTag os t =
	let val (n,str) = deconstruct_tag t
	in  (blastOutInt os n;
	     if (!useOldBlast)
		 then blastOutString os str
	     else blastOutString os "")
	end


    fun blastInTag is = 
	let val n = blastInInt is
	    val _ = update_label_counter n
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
	    val _ = update_label_counter n
	    val str = blastInString is
	in  construct_label(n, str)
	end
    
    fun blastOutVarmap os blaster vmap = 
	blastOutList (blastOutPair blastOutVar blaster) os (VarMap.listItemsi vmap)

    fun blastInVarmap is blaster =
	let val ls = blastInList (blastInPair blastInVar blaster)  is
	    fun folder((v,item),acc) = VarMap.insert(acc,v,item)
	    in  foldl folder VarMap.empty ls
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


