structure Info :> INFO =
struct

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

    structure B = Blaster

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

end
