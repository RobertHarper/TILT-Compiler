(*$import Time Crc *)


(* ---- Provides an abstraction for communication between master and slave processes ---- *)
signature COMMUNICATION = 
sig
    eqtype channel
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
                     | FLUSH                 (* Master signals that slaves should flush file cache *)
	             | REQUEST of job        (* Master requests slave to compile file *)

    (* These are all non-blocking. *)
    val erase : channel -> unit             (* Delete given channel. *)
    val exists : channel -> bool            (* Does a channel have a message. *)
    val send : channel * message -> unit    (* Write a message on given channel, even if channel exists. *)
    val receive : channel -> message option (* Get a message from a given channel, if it exists. *)
    val findToMasterChannels : unit -> channel list  (* Find all ready channels to master. *)
    val findFromMasterChannels : unit -> channel list  (* Find all ready channels from master. *)
end

signature HELP = 
    sig
	val base2int : string -> string
	val base2o : string -> string
	val base2s : string -> string
	val base2sml : string -> string
	val base2ui : string -> string
	val base2uo : string -> string
	val chat : string -> unit
	val chat_ref : bool ref
	val chat_strings : int -> string list -> int
	    
	val showTime : string -> unit
	val reshowTimes : unit -> unit
    end

signature FILECACHE = 
    sig
	type internal
	val flushAll : unit -> unit
	val flushSome : string list -> unit
	val exists : string -> bool
	val modTime : string -> Time.time
	val lastModTime : string list -> string option * Time.time
	val size : string -> int
	val read : string -> bool * internal    (* Was it cached? *) 
	val write : string * internal -> bool   (* Did we write?  *)
	val crc : string -> Crc.crc
	val tick : unit -> unit
    end 

signature SLAVE = 
sig
    datatype result = WORK of string | WAIT | READY
    val setup : unit -> unit
    val step : unit -> result
    val run : unit -> unit       (* run slave repeatedly and restart on termination *)
    val assemble : string * string * string list -> unit  (* (unit,base,importBases) *)
end

signature MASTER = 
sig
    type state
    datatype result = PROCESSING of state        (* All slaves utilized *)
                    | IDLE of state * int * string list * string list  
                                                 (* Number of idle slaves, waiting jobs, and pending jobs *)
	            | COMPLETE
    (* Takes the mapfile, a list of units that are desired,
       and a name of the executable.  Generates the executable
       and compiles the necessart units. *)
    val once : string * string list * string option -> 
	{setup : unit -> state,
	 step : state -> result,
	 complete : unit -> unit}
    val run : string * string list * string option -> unit 
    val purge : string -> unit

    type collapse = {maxWeight : int, maxParents : int, maxChildren : int}
    val makeGraph     : string * collapse option -> string  (* Creates a .dot file *)
    val makeGraphShow : string * collapse option -> string  (* Creates a .ps file and invokes gv on it *)

end

signature MANAGER = 
sig

  (* buildRuntime   build the runtime; if true, rebuild from scratch
     purge          takes a mapfile and removes all generated files of units named in mapfile
     slave          run a slave
     master         run a master on the given mapfile
     make           run a master on the given mapfile and interleave slave work in the same process
     pmake          run a master on the given mapfile and start slaves on the given machines
  *)

  val buildRuntime : bool -> unit
  val purge : string -> unit
  val slave : unit -> unit
  val master : string -> unit  
  val make : string -> unit
  val pmake : string * string list -> unit  

  val tilc   : string * bool * string option * string list -> unit  
                                              (* mapfile, don't link,
					         optional .exe name, list of files to compile *)

  val command : string * string list -> int  (* to be exported *)

end

