structure UnitEnvironment :> UNIT_ENVIRONMENT =
struct

    val error = fn s => Util.error "ue.sml" s

    fun say (s : string) : unit =
	if !Blaster.BlastDebug then (print s; print "\n") else ()

    structure Map = Util.StringMap
    structure B = Blaster

    type equiv = Crc.crc * Crc.crc -> bool
    type ue = Crc.crc Map.map

    fun blastOutEntry (os : B.outstream) (e : string * Crc.crc) : unit =
	(say "blastOutEntry";
	 B.blastOutPair B.blastOutString Crc.blastOutCrc os e)
    fun blastInEntry (is : B.instream) : string * Crc.crc =
	(say "blastInEntry";
	 B.blastInPair B.blastInString Crc.blastInCrc is)

    fun blastOutUe (os : B.outstream) (ue : ue) : unit =
	(say "blastOutUe"; B.blastOutList blastOutEntry os (Map.listItemsi ue))
    fun blastInUe (is : B.instream) : ue =
	let val _ = say "blastInUe"
	    val items = B.blastInList blastInEntry is
	    fun folder ((u,crc),acc) = Map.insert(acc,u,crc)
	in  foldl folder Map.empty items
	end
    val (blastOutUe, blastInUe) =
	Blaster.magic (blastOutUe, blastInUe, "ue $Revision$")

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

    fun confine (eq : equiv) (UE1 : ue, UE2 : ue) : ue_result =
	let
	    fun folder (name, crc, acc) =
		(case Map.find (UE2, name)
		   of NONE => Map.insert (acc, name, crc)
		    | SOME crc' =>
		       if eq(crc, crc') then acc
		       else raise Fail name)
	in
	    VALID (Map.foldli folder Map.empty UE1)
	    handle (Fail name) => WITNESS name
	end

    fun confine' (UE1 : ue, UE2 : ue) : ue =
	let fun keeper (name, _) = case Map.find (UE2, name)
				     of NONE => true
				      | SOME _ => false
	in  Map.filteri keeper UE1
	end

    fun plus_overlap (eq : equiv) (UE1 : ue, UE2 : ue) : ue_result =
	let fun handleCommonName (name, crc, crc') = if eq(crc,crc') then crc
						     else raise Fail name
	in
	    VALID (Map.unionWithi handleCommonName (UE1, UE2))
	    handle (Fail name) => WITNESS name
	end

    fun plus_no_overlap (UE1 : ue, UE2 : ue) : ue_result =
	let fun handleCommonName (name, _, _) = raise Fail name
	in
	    VALID (Map.unionWithi handleCommonName (UE1, UE2))
	    handle (Fail name) => WITNESS name
	end

    fun isEmpty (ue : ue) : bool = Map.numItems ue = 0

    fun isSubset (eq : equiv) (arg : ue * ue) : bool =
	(case confine eq arg
	   of VALID ue => isEmpty ue
	    | WITNESS _ => false)

    fun equal (eq : equiv) (ue1 : ue, ue2 : ue) =
	isSubset eq (ue1,ue2) andalso isSubset eq (ue2, ue1)

end
