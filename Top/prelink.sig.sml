(*
	The manager checks that unit interface CRCs are consistent
	prior to combining them into elaboration contexts, linking
	unit object files into an executable, and packing unit object
	files and interfaces into a library.
*)
signature PRELINK =
sig

    val PrelinkDebug : bool ref

    type crc = Crc.crc
    type ue = UnitEnvironment.ue
    type equiv = crc * crc -> bool

    datatype entry =
	UNIT of {name:string, iface:crc * ue, objue:ue}
      | IMPORT of {name:string, iface:crc * ue}
      | IFACE of {name:string, ue:ue}

    val check : equiv * entry list -> unit

end
