(*$import Prelude TopLevel *)

(* Since we  don't have polymorphic recursion at  the HIL level, there
   should be no  way to define equality at this  type.  We either want
   compilation to fail  or we want the compiler  to produce a datatype
   which doesn't admit equality. *)

datatype 'a foo = A of 'a | B of ('a list) foo

(* Currently, compilation succeeds because this is translated as if we
   had written datatype 'a foo = A of 'a | B of 'a foo. *)

