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
	structure StringSet : ORD_SET where type Key.ord_key = string
	type filebase = string
	type unitname = string
	val base2int : string -> string
	val base2o : string -> string
	val base2s : string -> string
	val base2sml : string -> string
	val base2ui : string -> string
	val base2uo : string -> string
	val cache_context : int ref
	val chat : string -> unit
	val chat_ref : bool ref
	val chat_strings : int -> string list -> int

	val forget_stat : string -> unit
	val exists : string -> bool
	val modTime : string -> Time.time

	val getContext : string list -> LinkIl.context
	val writeContext : string * Il.context -> unit
	val get_base : string -> string
	val get_import_direct : string -> string list
	val get_import_transitive : string -> string list
	val get_position : string -> int
	val get_ui_crc : string -> Crc.crc
	val parse_depend : string -> (string -> string list) -> string -> string list

	val setMapping : bool * string -> unit  (* true indicates master *)
	val list_units : unit -> string list
	val stat_each_file : bool ref
	val stop_early_compiling_sml_to_ui : bool ref

	(* Given a list of units, divide into 3 groups: WAITING, READY, PENDING, DONE.
	   Units are initially WAITING if they have imports and READY if they have none.
	   When a unit is marked DONE explicitly, more units will become READY. *)
	val partition : string list -> string list * string list * string list * string list
	val markPending : string -> unit
	val markDone  : string -> unit
  end

signature SLAVE = 
sig
    val slaveTest : unit -> unit
    val setup : unit -> unit
    val once : unit -> bool      (* true indicates some work was done *)
    val run : unit -> unit
end

signature MASTER = 
sig
    type state
    val masterTest : unit -> unit
    val once : string * string list -> string list * state * (state -> bool * state option)
	                                           (* true indicates all available slaves were utilized *)
    val run : string * string list -> string list  (* Takes the mapfile and a list of units that are desired,
						        returning a list of units that are required *)
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

