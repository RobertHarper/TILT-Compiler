structure Info :> INFO =
struct

    structure F = Formatter
    structure B = Blaster
    structure Ue = UnitEnvironment

    fun error s = Util.error "info.sml" s

    fun say (s : string) : unit =
	if !Blaster.BlastDebug then (print s; print "\n") else ()

    type unitname = string
    type crc = Crc.crc
    type context = Ue.ue
    type imports = unitname list

    datatype info =
	SRCI of context * imports * crc
      | PRIMU
      | SRCU of context * imports * crc * crc option

    fun blastOutImports (os : B.outstream) (I : imports) : unit =
	(say "blastOutImports"; B.blastOutList B.blastOutString os I)
    fun blastInImports (is : B.instream) : imports =
	(say "blastInImports"; B.blastInList B.blastInString is)

    fun blastOutInfo (os : B.outstream) (info : info) : unit =
	(say "blastOutInfo";
	 (case info
	    of SRCI (context, imports, src) =>
		(B.blastOutInt os 0; Ue.blastOutUe os context;
		 blastOutImports os imports; Crc.blastOutCrc os src)
	     | PRIMU => B.blastOutInt os 1
	     | SRCU (context, imports, src, iface) =>
		(B.blastOutInt os 2; Ue.blastOutUe os context;
		 blastOutImports os imports; Crc.blastOutCrc os src;
		 B.blastOutOption Crc.blastOutCrc os iface)))

    fun blastInInfo (is : B.instream) : info =
	(say "blastInInfo";
	 (case B.blastInInt is
	    of 0 => SRCI (Ue.blastInUe is, blastInImports is, Crc.blastInCrc is)
	     | 1 => PRIMU
	     | 2 => SRCU (Ue.blastInUe is, blastInImports is, Crc.blastInCrc is,
			  B.blastInOption Crc.blastInCrc is)
	     | _ => error "bad info"))

    val (blastOutInfo, blastInInfo) =
	B.magic (blastOutInfo, blastInInfo, "info $Revision$")

    val pp_imports : imports -> F.format =
	F.pp_list' F.Hbox F.String

    val Com = F.String ","
    fun pp_info (info : info) : F.format =
	(case info
	   of SRCI (context,imports,crc) =>
		F.HOVbox [F.String "SRCI", F.Break,
			  F.String "context = ", Ue.pp_ue context, Com, F.Break,
			  F.String "imports = ", pp_imports imports, Com, F.Break,
			  F.String "source crc = ", Crc.pp_crc crc]
	    | PRIMU => F.String "PRIMU"
	    | SRCU (context, imports, crc, crcopt) =>
		F.HOVbox [F.String "SRCU", F.Break,
			  F.String "context = ", Ue.pp_ue context, Com, F.Break,
			  F.String "imports = ", pp_imports imports, Com, F.Break,
			  F.String "source crc = ", Crc.pp_crc crc, Com, F.Break,
			  F.String "iface crc = ",
			  F.pp_option Crc.pp_crc crcopt])

end
