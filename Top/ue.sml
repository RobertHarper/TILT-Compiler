(*$import UNIT_ENVIRONMENT Crc TopHelp Util *)

structure UnitEnvironment :> UNIT_ENVIRONMENT =
struct

    structure Map = Util.StringMap
	
    type ue = Crc.crc Map.map

    val empty      = Map.empty
    val insert     = Map.insert
    val find       = Map.find
    val listItemsi = Map.listItemsi
    val appi       = Map.appi
    val foldi      = Map.foldli

    datatype ue_result =
	VALID of ue
      | WITNESS of string

    exception Fail of string
    
    (* confine : ue * ue -> ue_result *)
    fun confine (UE1, UE2) =
	let
	    fun folder (name, crc, acc) =
		(case Map.find (UE2, name)
		   of NONE => Map.insert (acc, name, crc)
		    | SOME crc' =>
		       if crc = crc' then acc
		       else raise Fail name)
	in
	    VALID (Map.foldli folder Map.empty UE1)
	    handle (Fail name) => WITNESS name
	end

    (* confine' : ue * ue -> ue *)
    fun confine' (UE1, UE2) =
	let fun keeper (name, _) = case Map.find (UE2, name)
				     of NONE => true
				      | SOME _ => false
	in  Map.filteri keeper UE1
	end

    (* plus_overlap : ue * ue -> ue_result *)
    fun plus_overlap (UE1, UE2) =
	let fun handleCommonName (name, crc, crc') = if crc = crc' then crc
						     else raise Fail name
	in
	    VALID (Map.unionWithi handleCommonName (UE1, UE2))
	    handle (Fail name) => WITNESS name
	end

    (* plus_no_overlap : ue * ue -> ue_result *)
    fun plus_no_overlap (UE1, UE2) =
	let fun handleCommonName (name, _, _) = raise Fail name
	in
	    VALID (Map.unionWithi handleCommonName (UE1, UE2))
	    handle (Fail name) => WITNESS name
	end

    (* isEmpty : ue -> bool *)
    fun isEmpty ue = Map.numItems ue = 0

    (* isSubset : ue * ue -> bool *)
    fun isSubset arg = case confine arg
			 of VALID ue => isEmpty ue
			  | WITNESS _ => false

    (* equal : ue * ue -> bool *)
    fun equal (ue1, ue2) = isSubset (ue1, ue2) andalso isSubset (ue2, ue1)
	
end
