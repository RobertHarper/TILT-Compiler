(*$import Time Crc *)

signature MANAGER = 
sig

  (* buildRuntime   build the runtime; if true, rebuild from scratch
     purge          takes a mapfile and removes all generated files of units named in mapfile
     slave          run a slave
     slaves	    run some slaves on the given machines
     master         run a master on the given mapfile
     make           run a master on the given mapfile
                    also interleave slave work in the same process
  *)

  val buildRuntime : bool -> unit
  val purge : string -> unit
  val slave : unit -> unit
  val slaves : (int * string) list -> unit
  val master : string -> unit  
  val make : string -> unit
  val makeNoLink : string -> unit

end

