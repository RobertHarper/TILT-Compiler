(*$import Time Crc *)

(* ---- Provides an abstraction for communication between master and slave processes ---- *)
signature COMMUNICATION = 
sig
    type channel
    val eq : channel * channel -> bool
    val toMaster : channel    (* Channel from self/slave to master.  *)
    val fromMaster : channel  (* Channel from master to self/slave.  *)
    val reverse : channel -> channel (* Reverse directionality of channel. *)
    val source : channel -> string   (* Gives name to sender of channel. *)
    val destination : channel -> string   (* Gives name to sender of channel. *)

    type job = string list
    datatype message = READY                 (* Slave signals readiness *)
		     | ACK_INTERFACE of job  (* Slave signals that interface has compiled *)
		     | ACK_ASSEMBLY of job   (* Slave signals that asm file has compiled but cannot assemble *)
		     | ACK_OBJECT of job     (* Slave signals that object has compiled *)
		     | ACK_ERROR of job      (* Slave signals that an error occurred during given job *)
                     | FLUSH of job          (* Master signals that slaves should flush file cache and set boolean flags -
					        each flag is a pair of the flag name and "true" or "false" *)
	             | REQUEST of job        (* Master requests slave to compile file *)

    (* These are all non-blocking. *)
    val erase : channel -> unit             (* Delete given channel. *)
    val exists : channel -> bool            (* Does a channel have a message. *)
    val send : channel * message -> unit    (* Write a message on given channel, even if channel exists. *)
    val receive : channel -> message option (* Get a message from a given channel, if it exists. *)
    val findToMasterChannels : unit -> channel list  (* Find all ready channels to master. *)
    val findFromMasterChannels : unit -> channel list  (* Find all ready channels from master. *)

    val getFlags : unit -> string list
    val doFlags : string list -> unit
end

