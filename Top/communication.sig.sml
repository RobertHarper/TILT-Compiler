(*
	Provides an abstraction for communication between master and
	slave processes.

	Slaves send READY to ask for a job, ACK_INTERFACE when a
	unit's interface is up to date but the job is still
	proceeding, ACK_DONE when a job is complete, and ACK_ERROR
	when an error occurrs.

	The master sends FLUSH_ALL to initialize; REQUEST to start a
	job; and FLUSH to indicate that another slave is going to
	modify some files.
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

    type job = int * string
    type plan = Update.plan
    type flags = (string * bool) list
    type platform = Target.platform

    datatype message =
	READY
      | ACK_INTERFACE of job
      | ACK_DONE of job * plan
      | ACK_ERROR of job * string
      | FLUSH_ALL of platform * flags
      | FLUSH of job * plan
      | REQUEST of job * plan

    (* These do not block. *)
    val canReceive : in_channel -> bool
    val receive : in_channel -> message option
    val send : message -> out_channel -> unit

    val destroyAllChannels : unit -> unit
    val hasMsg : channel -> bool
    val findSlaves : unit -> identity list

    val getFlags : unit -> flags
    val doFlags : flags -> unit
end
