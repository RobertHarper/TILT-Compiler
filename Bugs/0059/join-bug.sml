(*$import *)

(* No structures (ie, term) hides the bug. *)

(* One structure (ie, A.term) hides the bug. *)

(* Three structures (ie, A.B.C.term) with where A = Arg.A hides the
   bug.  Three structures with "where A.B = Arg.A.B" finds the bug
   again. *)

signature S =
    sig
	structure A :
	    sig
		structure B :
		    sig
			datatype term = T
		    end
	    end
    end

(* Eliminating the ascription or making it transparent hides the bug.
   Using "where type" in place of "where" hides the bug. *)

functor JoinWithArg (structure Arg : S) :> S where A = Arg.A =
    struct
	(* Using "open Arg" also hits the bug. *)
	structure A = Arg.A
    end
