(*$import *)

signature BACKGROUND =
sig

    val noFork : bool ref		(* Processing is done in the foreground *)
	
    (* (background f) launches a child process evaluating f(), returning a function isdone such that:
     * isdone() = false if child is still running.
     *          = true  if child finished successfully (because f termianted or called exit(0)).
     *          raises en exception if the child is killed or if f raises an exception or calles exit(nonzero).
     *)
    val background : (unit -> unit) -> unit -> bool
	
end
