(*$import Int List Vector Array Math Char TextIO *)

(* Auxiliary functions for test cases *)

infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int 
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

fun tst0 s s' = print (s ^ "    \t" ^ s' ^ "\n");
fun tst  s b = tst0 s (check  b);
fun tst' s f = tst0 s (check' f);

fun tstrange s bounds = (tst s) o range bounds  


(* General -- incomplete 1996-04-19, 1996-09-30, 1997-03-12 *)


val _ = print "\nFile general.sml: Testing structure General...\n"

exception NoExceptionRaised

fun getExn (f : unit -> 'a) = 
    (f (); NoExceptionRaised) handle e => e

fun prExn(exnStr, exn) =
    (print "\nShould be `"; print exnStr; print "':\n  ";
     print (exnName exn); print "\n  ";
     print (exnMessage exn));

val _ = prExn ("Div",getExn(fn _ => 1 div 0));
