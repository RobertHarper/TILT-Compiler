(*$import Prelude Paths Target Update *)

(* ---- Provides an abstraction for communication between master and slave processes ---- *)
signature COMMUNICATION = 
sig
    type identity
    val master : identity			(* Identity of the master. *)
    val slave : int option -> identity		(* Identity of slave/self processor. *)
    val compare : identity * identity -> order
    val name : identity -> string

    type channel
    val eq : channel * channel -> bool
    val toMaster : identity -> channel		(* Channel from given slave to master. *)
    val fromMaster : identity -> channel	(* Channel from master to given slave. *)
    val reverse : channel -> channel		(* Reverse directionality of channel. *)
    val source : channel -> identity		(* Identity of sender on channel. *)
    val destination : channel -> identity	(* Identity of recipient on channel. *)
	
    type in_channel
    type out_channel
    val openIn : channel -> in_channel		(* Become reader on channel. *)
    val openOut : channel -> out_channel	(* Become writer on channel. *)
    val closeIn : in_channel -> unit
    val closeOut : out_channel -> unit
	
    datatype message =
	READY					(* Slave signals readiness. *)
      | ACK_INTERFACE of string			(* Slave signals that interface has been compiled.  The job
						   is still in progress.  This message can be skipped. *)
      | ACK_DONE of string * Update.plan	(* Slave gives up on job, informing master what steps are left. *)
      | ACK_ERROR of string			(* Slave signals that an error occurred during job. *)
      | FLUSH_ALL of (Target.platform *		(* Master signals that slave should flush file cache and set boolean flags - *)
		  (string * bool) list)		(* each flag is a pair of the flag name and value. *)
      | FLUSH of (Paths.unit_paths *		(* Master signals that slave should flush files related to plan *)
		  Update.plan)			(* in anticipation of some other processor doing work. *)
      | REQUEST of (Paths.unit_paths *		(* Master request slave to compile. *)
		    (Paths.unit_paths * Update.import) list *
		    Update.plan)
    (* These are all non-blocking. *)
    (* Buffered access to channels. *)
    val canReceive : in_channel -> bool		(* Does a channel have a message? *)
    val receive : in_channel -> message option	(* Get message from given channel, if present.  *)
    val send : message -> out_channel -> unit	(* Write a message on given channel. *)
    (* Direct access to file system *)
    val destroyAllChannels : unit -> unit
    val exists : channel -> bool		(* Does a channel have a message on disk? *)
    val findSlaves : unit -> identity list	(* Find all potential slaves. *)

    val getFlags : unit -> (string * bool) list
    val doFlags : (string * bool) list -> unit
end

