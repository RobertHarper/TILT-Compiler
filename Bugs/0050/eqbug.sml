(*$import *)

(* Equality at A2.a is being re-written and not simply invoked to
   implement equality at notok.

   This came up with the type abbreviations "label" etc in Il.

   The signature ascription matters.  The bug occurs with transparent
   or opaque ascription.  The bug disappears with no ascription.

   It looks like the problem is signature ascription tosses equality functions.

*)

structure A = struct type a = int * int * int * int end
signature A2 = sig type a = int * int * int * int end
structure A2 : A2 = A

type ok = A.a

type notok = A2.a

functor alsoNotOk(A' : A2) =
struct
    type alsonotok = A'.a
end
