(*$import Prelude *)

signature STATS =
   sig

     val reset_stats : unit -> unit
     (*Just zero out all numerical data - don't remove entries*)
     val clear_stats : unit -> unit
     val print_stats : unit -> unit
     val print_timers : unit -> unit

     val timer : string * ('a -> 'b) -> ('a -> 'b)
     val subtimer : string * ('a -> 'b) -> ('a -> 'b)
     val counter : string -> (unit -> int)
     val int     : string -> int ref
     val bool    : string -> bool ref
     val tt      : string -> bool ref (* initialized to true *)
     val ff      : string -> bool ref (* initialized to false *)

     val fetch_timer_max : string -> real (* in seconds *)
     val fetch_timer_last : string -> real (* in seconds *)

   end

