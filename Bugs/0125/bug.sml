(*$import Int List Vector Array Math Char TextIO *)

exception NoExceptionRaised

fun getExn (f : unit -> 'a) = 
    (f (); NoExceptionRaised) handle e => e

fun prExn(exnStr, exn) =
    (print "\nShould be `"; print exnStr; print "':\n  ";
     print (exnName exn); print "\n  ";
     print (exnMessage exn));

val _ = prExn ("Div",getExn(fn _ => 1 div 0));
