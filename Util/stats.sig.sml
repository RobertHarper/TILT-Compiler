signature STATS =
   sig

     val reset_stats : unit -> unit
     (*Just zero out all numerical data - don't remove entries*)
     val clear_stats : unit -> unit
     val print_stats : unit -> unit

     val timer : string * ('a -> 'b) -> ('a -> 'b)
     val counter : string -> (unit -> unit)
     val int     : string -> int ref
     val bool    : string -> bool ref
   end

