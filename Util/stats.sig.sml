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

     (*You probably want these ones!
      * These have the property that they don't double count
      * on recursive calls: that is, if the timered function gets called
      * while the timer is still active, it will only count the time once
      * Note that this does not protect you from nested timers, just from
      * nested calls to the same timer
      *)
     val timer'    : string * ('a -> 'b) -> ('a -> 'b)
     val subtimer' : string * ('a -> 'b) -> ('a -> 'b)

     val counter : string -> (unit -> int)
     val int     : string -> int ref
     val bool    : string -> bool ref
     val tt      : string -> bool ref (* initialized to true *)
     val ff      : string -> bool ref (* initialized to false *)

     val fetch_timer_max : string -> real (* in seconds *)
     val fetch_timer_last : string -> real (* in seconds *)

     val fetch_timer : string ->
	  {count : int,
	   max   : real, (*seconds*)
	   last  : real, (*seconds*)
	   gc    : real, (*seconds*)
	   cpu   : real, (*seconds*)
	   real  : real  (*seconds*)
	   }

     val fetch_counter : string -> int
	     
   end

