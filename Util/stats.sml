structure Stats :> STATS =
   struct

       val error = fn s => Util.error "stats.sml" s
       type time = Time.time
       type time_entry = (int * bool * {gc : time, sys: time, usr: time})  ref
       type counter_entry = int ref
       type int_entry = int ref
       type bool_entry = bool ref
       datatype entry = TIME_ENTRY of time_entry
                      | COUNTER_ENTRY of counter_entry
		      | INT_ENTRY of int_entry
		      | BOOL_ENTRY of bool_entry
(*
       structure StringKey : ORD_KEY = struct
					type ord_key = string
					val compare = String.compare
				       end
       structure StringMap : ORD_MAP = LocalSplayMapFn(StringKey)
       val entries : (entry StringMap.map) ref = ref StringMap.empty
*)
	val entries : (string * entry) list ref = ref []
       val max_name_size = ref 10
       fun clear_stats() = 
	 let
	   fun reset (s,TIME_ENTRY entry) =
	       let val z = Time.zeroTime
	       in  entry := (0,#2 (!entry),{gc=z,sys=z,usr=z})
	       end
	     | reset (s,COUNTER_ENTRY entry) = entry := 0
	     | reset (s,INT_ENTRY entry) = entry := 0
	     | reset (s,BOOL_ENTRY entry) = () (*entry := true*)
	 in  List.app reset (!entries)
	 end

       fun reset_stats() = (max_name_size := 10; 
			      entries := [])  (* StringMap.empty) *)
       fun find_entry entry_maker s : entry =
(*	   (case (StringMap.find(!entries,s)) of *)
	   (case (Listops.assoc_eq((op =): string * string -> bool,
			s, !entries)) of
		SOME entry => entry
	      | NONE => let val _ = max_name_size := Int.max(size s, !max_name_size)
			    val entry = entry_maker()
(*			    val entries' = StringMap.insert(!entries,s,entry) *)
			    val entries' = (s,entry) :: (!entries)
			    val _ = entries := entries'
			in entry
			end)
      fun find_time_entry s disjoint = 
	let val z = Time.zeroTime
	    fun maker () = TIME_ENTRY(ref(0,disjoint,{gc=z,sys=z,usr=z}))
        in  case (find_entry maker s) of
		TIME_ENTRY res => res
	      | _ => error "find_time_entry: did not find a TIME_ENTRY"
	end
      fun find_counter_entry s = 
	let fun maker () = COUNTER_ENTRY(ref 0)
        in  case (find_entry maker s) of
		COUNTER_ENTRY res => res
	      | _ => error "find_time_entry: did not find a COUNTER_ENTRY"
        end
      fun find_int_entry s = 
	let fun maker () = INT_ENTRY(ref 0)
        in  case (find_entry maker s) of
		INT_ENTRY res => res
	      | _ => error "find_time_entry: did not find a INT_ENTRY"
        end
      fun find_bool_entry s = 
	let fun maker () = BOOL_ENTRY(ref true)
        in  case (find_entry maker s) of
		BOOL_ENTRY res => res
	      | _ => error "find_time_entry: did not find a BOOL_ENTRY"
        end
      fun counter str = let val intref = find_counter_entry str
                        in fn() => let val v = !intref
				   in intref := v + 1; v
				   end
			end
      val int = find_int_entry
      val bool = find_bool_entry

      fun timer_help disjoint (str,f) arg =
	   let val timer = Timer.startCPUTimer()
	       val r = f arg
	       val {gc,sys,usr} =  Timer.checkCPUTimer timer
	       val entry_ref = find_time_entry str disjoint
	       val (count,disjoint,{gc=gc',sys=sys',usr=usr'}) = !entry_ref
	       val new_entry = (count+1, disjoint, 
				{gc = Time.+(gc,gc'),
				 sys = Time.+(sys,sys'),
				 usr = Time.+(usr,usr')})
	       val _ = entry_ref := new_entry
	   in r
	   end

      val timer = fn arg => timer_help true arg
      val subtimer = fn arg => timer_help false arg

       fun fprint max_size str =
	   let fun loop i = if i < 0 then () else (print " "; loop (i-1))
	   in  print str;
	       loop (max_size - (size str))
	   end
       fun print_timers() =
	   let 
	       fun triple2real {gc,sys,usr} = Time.toReal(Time.+(gc,Time.+(sys,usr)))
(*               val entries = StringMap.listItemsi(!entries) *)
	       val entries = rev(!entries)
               fun folder ((_,TIME_ENTRY (ref(_,true,t))),acc) = acc+(triple2real t)
                 | folder (_,acc) = acc
	       val total_time = foldr folder 0.0 entries
	       fun real2string r = Real.fmt (StringCvt.FIX (SOME 3)) r
	       fun pritem (name,TIME_ENTRY(ref (count,disjoint,triple))) =
		   let 
		       val time_real = triple2real triple
		   in
		       fprint (!max_name_size) name;
		       print " : ";
		       fprint 8 (real2string time_real);
		       if disjoint
			   then (print " (";
				 print (real2string(time_real/total_time * 100.0));
				 print "%)")
		       else ();
		       print "\n"
		   end
	        | pritem _ = ()
	 in
	     print "Global timing in seconds\n";
	     print "-------------------------------------------\n";
	     app pritem entries;
	     print "-------------------------------------------\n";
	     fprint (!max_name_size) "TOTAL TIME";
	     print " : ";
	     fprint 8 (real2string total_time);
	     print "\n";
	     let val lines = !(int "SourceLines") 
	     in  if lines > 0
		 then (fprint (!max_name_size) "LINES/SEC";
		       print " : ";
		       fprint 8 (real2string ((Real.fromInt lines) / total_time));
		       print "\n")
		 else ()
	     end
	 end

      fun print_counters() = 
        let
	       fun pritem (name,COUNTER_ENTRY(ref count)) =
		       (fprint (!max_name_size) name;
		        print " : ";
		        fprint 8 (Int.toString count);
			print "\n")
	         | pritem _ = ()
	in
	     print "Global counters\n";
	     print "-------------------------------------------\n";
	     app pritem (rev(!entries));
(* (StringMap.listItemsi(!entries)); *)
	     print "-------------------------------------------\n"
	 end

      fun print_ints() =
        let
	       fun pritem (name,INT_ENTRY(ref count)) =
		       (fprint (!max_name_size) name;
		        print " : ";
		        fprint 8 (Int.toString count);
			print "\n")
	         | pritem _ = ()
	in
	    print "Global integer statistics\n";
	     print "-------------------------------------------\n";
	     app pritem (rev(!entries));
	     print "-------------------------------------------\n"
	 end

      fun print_bools() =
        let
	       fun pritem (name,BOOL_ENTRY(ref flag)) =
		       (fprint (!max_name_size) name;
		        print " : ";
		        print (Bool.toString flag);
			print "\n")
	         | pritem _ = ()
	in
	     print "Global flags\n";
	     print "-------------------------------------------\n";
	     app pritem (rev(!entries));
	     print "-------------------------------------------\n"
	 end

      fun print_stats() = (print "\n\n";
			   print_counters(); 	
			   print "\n\n";
			   print_ints(); 	
			   print "\n\n";
			   print_bools(); 	
			   print "\n\n";
			   print_timers(); 
			   print "\n\n")

   end
