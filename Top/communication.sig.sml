(*
	Communication between master and slave processes.
*)
signature COMMUNICATION =
sig
    type identity
    val master : identity
    val slave : unit -> identity
    val compare : identity * identity -> order
    val name : identity -> string

    type channel
    val eq : channel * channel -> bool
    val toMaster : identity -> channel
    val fromMaster : identity -> channel
    val reverse : channel -> channel
    val source : channel -> identity
    val destination : channel -> identity

    type in_channel
    type out_channel
    val openIn : channel -> in_channel
    val openOut : channel -> out_channel
    val closeIn : in_channel -> unit
    val closeOut : out_channel -> unit

    type label = Name.label

    datatype message =
	READY
      | ACK_INTERFACE of label
      | ACK_FINISHED of label * Stats.delta
      | ACK_UNFINISHED of label * Stats.delta
      | ACK_REJECT of label * string
      | BOMB of string
      | INIT of Platform.objtype * Stats.stats * IntSyn.desc
      | COMPILE of label

    (* Debugging support *)
    val blastInMessages : Blaster.instream -> message list
    val pp_messages : message list -> Formatter.format

    (* These do not block. *)
    val canReceive : in_channel -> bool
    val receive : in_channel -> message option
    val send : message -> out_channel -> unit

    val destroyAllChannels : unit -> unit
    val hasMsg : channel -> bool
    val findSlaves : unit -> identity list

end
