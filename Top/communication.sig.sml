(*$import Paths Target Update *)

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

    datatype message =
	READY					(* Slave signals readiness. *)
      | ACK_INTERFACE of string			(* Slave signals that interface has been compiled.  The job
						   is still in progress.  This message can be skipped. *)
      | ACK_DONE of string * Update.plan	(* Slave gives up on job, informing master what steps are left. *)
      | ACK_ERROR of string			(* Slave signals that an error occurred during job. *)
      | FLUSH of (Target.platform *		(* Master signals that slave should flush file cache and set boolean flags - *)
		  (string * bool) list)		(* each flag is a pair of the flag name and value.  Currently skipped when
						   master and slave are the same. *)
      | REQUEST of (Paths.unit_paths *		(* Master request slave to compile. *)
		    Paths.unit_paths list *
		    Update.plan)
    (* These are all non-blocking. *)
    val erase : channel -> unit             (* Delete given channel. *)
    val exists : channel -> bool            (* Does a channel have a message. *)
    val send : channel * message -> unit    (* Write a message on given channel, even if channel exists. *)
    val receive : channel -> message option (* Get a message from a given channel, if it exists. *)
    val findToMasterChannels : unit -> channel list  (* Find all ready channels to master. *)
    val findFromMasterChannels : unit -> channel list  (* Find all ready channels from master. *)

    val getFlags : unit -> (string * bool) list
    val doFlags : (string * bool) list -> unit
end

