(*$import LinkIl *)


(* ---- Provides an abstraction for communication between master and slave processes ---- *)
signature COMMUNICATION = 
sig
    type job = string list

    (* Slaves can acknowledge completed jobs and request new jobs.  *)
    val acknowledge : job option -> unit    (* non-blocking *)
    val request : unit -> job option        (* blocking *)

    (* The master periodically calls findSlaves which polls for available slaves which are triples:
       (1) Name of the slave
       (2) The name of the last job the slave completed.  A NONE indicates a new slave.
       (3) A thunk for passing another job to the slave. A NONE indicates slave can terminate.
     *)
    val findSlaves : unit -> (string * job option * (job option -> unit)) list

    val masterTest : unit -> unit
    val slaveTest : unit -> unit
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
	val size : string -> int
	val read : string -> bool * internal    (* Was it cached? *) 
	val write : string * internal -> bool   (* Did we write?  *)
	val crc : string -> Crc.crc
	val tick : unit -> unit
    end 

signature SLAVE = 
sig
    datatype result = WORK of string | WAIT | READY
    val slaveTest : unit -> unit
    val setup : unit -> unit
    val step : unit -> result
    val run : unit -> unit       (* run slave repeatedly and restart on termination *)
end

signature MASTER = 
sig
    type state
    val masterTest : unit -> unit
    (* Takes the mapfile, a list of units that are desired,
       and a name of the executable.  Generates the executable
       and compiles the necessart units. *)
    val once : string * string list * string option -> 
	{setup : unit -> state,
	 step : state -> bool * state option,  	  (* true indicates all available slaves were utilized *)
	 complete : unit -> unit}
    val run : string * string list * string option -> unit 
    val graph : string -> unit
    val purge : string -> unit
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

