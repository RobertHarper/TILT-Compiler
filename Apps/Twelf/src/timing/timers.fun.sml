(* Timers collecting statistics about Twelf *)
(* Author: Frank Pfenning *)

functor Timers (structure Timing' : TIMING)
   : TIMERS =
struct

  structure Timing = Timing'

  val parsing  = Timing.newCenter ("Parsing       ")
  val recon    = Timing.newCenter ("Reconstruction")
  val abstract = Timing.newCenter ("Abstraction   ")
  val checking = Timing.newCenter ("Checking      ")
  val modes    = Timing.newCenter ("Modes         ")
  val subordinate = Timing.newCenter ("Subordination ")
  val terminate= Timing.newCenter ("Termination   ")
  val printing = Timing.newCenter ("Printing      ")
  val compiling= Timing.newCenter ("Compiling     ")
  val solving  = Timing.newCenter ("Solving       ")
  val filling  = Timing.newCenter ("Filling       ")
  val splitting= Timing.newCenter ("Splitting     ")
  val recursion= Timing.newCenter ("Recursion     ")
  val inference= Timing.newCenter ("Inference     ")

  val centers = [parsing, recon, abstract, checking, modes, subordinate,
		 terminate, printing, compiling, solving,
		 filling, splitting, recursion, inference]

  val total    = Timing.sumCenter ("Total         ", centers)

  val time = Timing.time

  fun reset () = List.app Timing.reset centers

  fun check () =
      (List.app (print o Timing.toString) centers;
       print (Timing.sumToString total);
       print "Remember that the success continuation counts into Solving!\n")

  fun show () = (check (); reset ()) 

end;  (* functor Timers *)
