(*
	Stats comprise flags and measurements.

	Flags are essentially static.  After you set them up, the
	master sends them to the slaves and no subsequent changes are
	communicated.

	Measurements are handled differently.  The slaves clear their
	measurements at the start of a job and send changes back to
	the master at the end.
*)
signature STATS =
sig

     val print_stats : unit -> unit
     val clear_measurements : unit -> unit	(* does not remove entries *)
     val print_measurements : unit -> unit

     (*
	Flags
     *)

     val int  : string * int -> int ref
     val bool : string -> bool ref
     val tt   : string -> bool ref (* initialized to true *)
     val ff   : string -> bool ref (* initialized to false *)

     (*
	Measurements
     *)

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
     val fetch_timer_last : string -> real (* in seconds *)

     type counter
     val counter : string -> counter	(* master takes sum of slave & master *)
     val counter' : string -> counter	(* master takes max of slave & master *)
     val counter_add : counter * int -> unit	(* nondecreasing *)
     val counter_max : counter * int -> unit
     val counter_inc : counter -> unit
     val counter_clear : counter -> unit
     val counter_fetch : counter -> int

     (*
	Communication
     *)

     type flags
     val get_flags : unit -> flags
     val set_flags : flags -> unit
     val blastOutFlags : Blaster.outstream -> flags -> unit
     val blastInFlags : Blaster.instream -> flags

     type measurements
     val get_measurements : unit -> measurements
     val add_measurements : measurements -> unit
     val blastOutMeasurements : Blaster.outstream -> measurements -> unit
     val blastInMeasurements : Blaster.instream -> measurements

end
