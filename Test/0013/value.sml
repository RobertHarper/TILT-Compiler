val x : int * int -> unit =
    (fn (i,j) => (print (Int.toString i); print " "; print (Int.toString j); print "\n"))
