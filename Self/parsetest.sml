val _ = print "Test entered\n"
val (_,fp,str,dec) = LinkParse.parse_impl "Bench/hello.sml"
val _ = print "Test parsed itself!\n"
val _ = Stats.print_timers()
