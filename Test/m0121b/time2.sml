open Time

val _ =
   if 1000000 = toMilliseconds (fromSeconds 1000)
      then print "OK\n"
   else print "ERROR\n"

val _ =
   if 1000000 = toMicroseconds (fromSeconds 1)
      then print "OK\n"
   else print "ERROR\n"

val _ =
   if 1000 = toSeconds (fromMilliseconds 1000000)
      then print "OK\n"
   else print "ERROR\n"

val _ =
   if 1 = toSeconds (fromMicroseconds 1000000)
      then print "OK\n"
   else print "ERROR\n"
