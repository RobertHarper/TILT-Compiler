(*
	An info records the state in which an interface or unit was
	compiled.  A file is out of date if this state changes.  Note
	that no context and imports are recorded for the primitive
	unit because it is always assumed to be out of date.
*)
signature INFO =
sig

    type unitname = string
    type crc = Crc.crc
    type context = UnitEnvironment.ue
    type imports = unitname list

    datatype info =
	SRCI of context * imports * crc
      | PRIMU
      | SRCU of context * imports * crc * crc option

    val blastOutInfo : Blaster.outstream -> info -> unit
    val blastInInfo : Blaster.instream -> info

end
