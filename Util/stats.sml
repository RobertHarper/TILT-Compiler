(*$import TopLevel Util STATS Timer Listops Date *)

structure Stats :> STATS =
   struct


       val error = fn s => Util.error "stats.sml" s
       type time = Time.time
       type time_entry = (int * bool * {gc : time, sys: time, usr: time, real : time})  ref
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
	       in  entry := (0,#2 (!entry),{gc=z,sys=z,usr=z,real=z})
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
	    fun maker () = TIME_ENTRY(ref(0,disjoint,{gc=z,sys=z,usr=z,real=z}))
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
      fun find_bool_entry default s = 
	let fun maker () = BOOL_ENTRY(ref default)
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
      val bool = find_bool_entry false
      val tt = find_bool_entry true
      val ff = find_bool_entry false

      fun timer_help disjoint (str,f) arg =
	   let val cpu_timer = Timer.startCPUTimer()
	       val real_timer = Timer.startRealTimer()
	       val r = f arg
	       val {gc,sys,usr} =  Timer.checkCPUTimer cpu_timer
	       val real =  Timer.checkRealTimer real_timer
	       val entry_ref = find_time_entry str disjoint
	       val (count,disjoint,{gc=gc',sys=sys',usr=usr', real=real'}) = !entry_ref
	       val new_entry = (count+1, disjoint, 
				{gc = Time.+(gc,gc'),
				 sys = Time.+(sys,sys'),
				 usr = Time.+(usr,usr'),
				 real = Time.+(real,real')})
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
	       fun triple2cpu {gc,sys,usr,real} = Time.toReal(Time.+(gc,Time.+(sys,usr)))
	       fun triple2real {gc,sys,usr,real} = Time.toReal real

	       val entries = rev(!entries)
               fun folder ((_,TIME_ENTRY (ref(_,true,t))),(acc_cpu,acc_real)) = 
		   (acc_cpu+(triple2cpu t), acc_real+(triple2real t))
                 | folder (_,acc) = acc
	       val (total_cpu, total_real) = foldr folder (0.0,0.0) entries
	       fun real2string r = Real.fmt (StringCvt.FIX (SOME 3)) r
	       fun pritem (name,TIME_ENTRY(ref (count,disjoint,triple))) =
		   let 
		       val time_cpu = triple2cpu triple
		       val time_real = triple2real triple
		       val time_cpu_string = real2string time_cpu
		       val time_real_string = real2string time_real
		       val percent_cpu_string = real2string(time_cpu/total_cpu * 100.0)
		       val percent_real_string = real2string(time_real/total_real * 100.0)
		   in
		       fprint (!max_name_size) name;
		       print " : ";
		       fprint 8 time_cpu_string;
		       fprint 10 (if disjoint then ("(" ^ percent_cpu_string ^ "%)") else "");
		       fprint 4 "";
		       fprint 8 time_real_string;
		       fprint 10 (if disjoint then ("(" ^ percent_real_string ^ "%)") else "");
		       fprint 6  (if (time_real > time_cpu * 2.0)
				  then "****" else "");
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
	     fprint 8 (real2string total_cpu);
             fprint 8;
	     fprint 8 (real2string total_real);
	     print "\n";
	     let val lines = !(int "SourceLines") 
	     in  if lines > 0
		 then (fprint (!max_name_size) "LINES/SEC";
		       print " : ";
		       fprint 8 (real2string ((Real.fromInt lines) / total_real));
		       print "\n")
		 else ()
	     end;
	     print (Date.toString(Date.fromTimeLocal(Time.now())))
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
