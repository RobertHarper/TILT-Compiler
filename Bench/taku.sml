(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* *)
structure Taku :> RUN = 
  struct
    fun tak (x, y, z) =
      if x > y then tak(tak (x-1, y, z), tak (y-1, z, x), tak (z-1, x, y))
      else z

    fun repeat n =
      if n <= 0 then 0 else tak(18,12,6) + repeat(n-1)
	
    fun run () = (print (Int.toString (repeat 500)); print "\n")
end
