structure Stats :> STATS =
   struct

       val error = fn s => Util.error "stats.sml" s
       type time = Time.time

       type time_entry = {count: int, max : real,last : real,
			  top_timer : bool, active : bool ref,
			  sum : {gc : time, sys: time, usr: time, real : time}} ref

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

       fun clear_stats() =
	 let
	   fun reset (s,TIME_ENTRY entry) =
	       let val z = Time.zeroTime
	       in  entry := {count = 0,
			     max = 0.0,
			     last = 0.0,
			     active = ref false,
			     top_timer = #top_timer (!entry),
			     sum = {gc=z,sys=z,usr=z,real=z}}
	       end
	     | reset (s,COUNTER_ENTRY entry) = entry := 0
	     | reset (s,INT_ENTRY entry) = entry := 0
	     | reset (s,BOOL_ENTRY entry) = () (*entry := true*)
	 in  List.app reset (!entries)
	 end

       fun reset_stats() = (entries := [])  (* StringMap.empty) *)

       fun fetch_entry name =
	 Listops.assoc_eq((op =): string * string -> bool,name, !entries)

       fun find_entry entry_maker s : entry =
	 (case fetch_entry s of
	    SOME entry => entry
	  | NONE => let val entry = entry_maker()
			val entries' = (s,entry) :: (!entries)
			val _ = entries := entries'
		    in entry
		    end)

      fun find_time_entry s disjoint =
	let val z = Time.zeroTime
	    fun maker () = TIME_ENTRY(ref{count = 0,
					  max = 0.0,
					  last = 0.0,active = ref false,
					  top_timer = disjoint,
					  sum = {gc=z,sys=z,usr=z,real=z}})
        in  case (find_entry maker s) of
		TIME_ENTRY res => res
	      | _ => error "find_time_entry: did not find a TIME_ENTRY"
	end

      fun find_counter_entry s =
	let fun maker () = COUNTER_ENTRY(ref 0)
        in  case (find_entry maker s) of
		COUNTER_ENTRY res => res
	      | _ => error "find_counter_entry: did not find a COUNTER_ENTRY"
        end
      fun find_int_entry s =
	let fun maker () = INT_ENTRY(ref 0)
        in  case (find_entry maker s) of
		INT_ENTRY res => res
	      | _ => error "find_int_entry: did not find a INT_ENTRY"
        end
      fun find_bool_entry maker s =
         case (find_entry maker s) of
		BOOL_ENTRY res => res
	      | _ => error "find_bool_entry: did not find a BOOL_ENTRY"

      fun fetch_timer_max name = #max(!(find_time_entry name false))
      fun fetch_timer_last name = #last(!(find_time_entry name false))

      fun fetch_timer name =
	let
	  val ref {count,max,last,sum as {gc, sys, usr, real},...} =
	    (case fetch_entry name
	       of SOME (TIME_ENTRY res) => res
		| _ => error ("fetch_timer: no TIME_ENTRY of name "^name))
	in
	  {count = count,
	   max   = max,
	   last  = last,
	   gc    = Time.toReal gc,
	   cpu   = Time.toReal(Time.+(sys,usr)),
	   real  = Time.toReal real}
	end

      fun fetch_counter name =
	let
	  val ref count =
	    (case fetch_entry name
	       of SOME (COUNTER_ENTRY res) => res
		| _ => error ("fetch_counter: no COUNTER_ENTRY of name "^name))
	in count
	end

      val int = find_int_entry
      fun bool str = find_bool_entry
	             (fn() => error ("trying to get an uninitialized bool " ^ str)) str
      val tt = find_bool_entry (fn() => BOOL_ENTRY(ref true))
      val ff = find_bool_entry (fn() => BOOL_ENTRY(ref false))

      fun counter str = let val intref = find_counter_entry str
                        in fn() => let val v = !intref
				   in intref := v + 1; v
				   end
			end
      fun update_entry {count,max,top_timer,last,active,sum as {gc=gc',sys=sys',usr=usr',real=real'}} {usr,sys,gc} real =
	let
	  val new_sum = {gc = Time.+(gc,gc'),
			 sys = Time.+(sys,sys'),
			 usr = Time.+(usr,usr'),
			 real = Time.+(real,real')}
	  val cur = Time.toReal(Time.+(gc,Time.+(sys,usr)))
	in {count = count+1,
	    last = cur,
	    active = active,
	    max = Real.max(cur,max),
	    top_timer = top_timer,
	    sum = new_sum}
	end

      fun timer_help (avoid_overlap,disjoint) (str,f) arg =
	let
	  val entry_ref = find_time_entry str disjoint
	  val (entry as {active,...}) = !entry_ref
	in
	  if avoid_overlap andalso !active then
	    f arg
	  else
	    let
	      val _ = active := true
	      val cpu_timer = Timer.startCPUTimer()
	      val real_timer = Timer.startRealTimer()
	      val r = f arg
	      val cpu =  Timer.checkCPUTimer cpu_timer
	      val real =  Timer.checkRealTimer real_timer
	      val _ = active := false
	      val new_entry = update_entry entry cpu real
	      val _ = entry_ref := new_entry
	    in r
	    end
	end

      val timer     = fn arg => timer_help (false,true) arg
      val subtimer  = fn arg => timer_help (false,false) arg
      val timer'    = fn arg => timer_help (true,true) arg
      val subtimer' = fn arg => timer_help (true,false) arg

      local
	val startCPUTimer = Timer.startCPUTimer
	val checkCPUTimer = Timer.checkCPUTimer

	fun add {usr,sys,gc} = Time.toReal(Time.+(sys,usr))

	val delta_t = ref NONE;

	val delta_timeout = 10000  (* If the timer doesn't advance after 10000 uses, we give up *)

	fun delta () =
	  (case (!delta_t) of
	       NONE =>
	       let
		 val cputimer = startCPUTimer ()
		 fun loop cnt =
		     if (cnt > delta_timeout)
			 then (print "Warning: timer does not advance after ";
			       print (Int.toString delta_timeout); print " uses; assuming 0 overhead\n";
			       0.0)
		     else let val d = add (checkCPUTimer cputimer)
			  in  if (Real.==(d,0.0))
				  then loop (cnt+1)
			      else d
			  end
		 val d = loop 0
		 val _ = delta_t := SOME d
	       in d
	       end
	     | (SOME r) => r)

	(*This times small functions.  This is just to get a handle on how much
	 * the timers slow things down.
	 *)
	fun ftime(f,epsilon) =
	  let
	    val d = delta()
	    val tmin = (d / epsilon) + d;

	    fun f_for c =
	      if c > 0 then
		(f();f_for(c-1))
	      else ()

	    fun loop cnt =
	      let
		val cputimer = startCPUTimer ()
		val _ = f_for cnt
		val tmeas = add (checkCPUTimer cputimer)
	      in
		if tmeas < tmin then
		  loop (cnt + cnt)
		else tmeas / (Real.fromInt cnt)
	      end
	  in
	    loop 1
	  end

	val timer_overhead = ref NONE

	fun test() = subtimer("TimerTimingCall",fn x=>x) 3

	fun get_overhead () =
	  (case timer_overhead
	     of ref (SOME r) => r
	      | ref NONE => (timer_overhead := SOME (ftime(test,0.01));get_overhead()))

      in
	fun timer_time n = (Real.fromInt n) * (get_overhead())
      end

      local
	  fun loop i = if i < 0 then () else (print " "; loop (i-1))
      in
	  fun lprint max_size str =
	      (print str; loop (max_size - (size str)))
	  fun rprint max_size str =
	      (loop (max_size - (size str)); print str)
      end

       fun print_timers'() =
	 let

	   (*gc time is included in the user time - don't overcount*)
	   fun triple2cpu  ({sys,usr,gc,real}) = Time.toReal(Time.+(sys,usr))
	   fun triple2real ({sys,usr,gc,real})    = Time.toReal real

	   val entries = rev(!entries)

	   fun folder ((_,TIME_ENTRY (ref{top_timer=true,sum,...})),(acc_cpu,acc_real)) =
	     (acc_cpu+(triple2cpu sum), acc_real+(triple2real sum))
	     | folder (_,acc) = acc

	   val (total_cpu, total_real) = foldr folder (0.0,0.0) entries


	   fun real2stringWith prec r = Real.fmt (StringCvt.FIX (SOME prec)) r

	   val max_name_size = foldl (fn ((n,TIME_ENTRY _),m) => Int.max(m,size n)
				       | (_,m) => m)
	                           10 entries

	   fun print_strings(name,count_string,per_call_string,max_string,
			     time_cpu_string,percent_cpu_string,time_gc_string,
			     time_real_string,percent_real_string, warning_flag) =
	     (lprint max_name_size name;
	      print " | ";
	      rprint 6 count_string;
	      rprint 7 per_call_string;
	      rprint 7 max_string;
	      print " | ";
	      rprint 7 time_cpu_string;
	      rprint 7 percent_cpu_string;
	      print " | ";
	      rprint 6 time_gc_string;
	      print " | ";
	      rprint 7 time_real_string;
	      rprint 7 percent_real_string;
	      print " | ";
	      lprint 4 warning_flag;
	      print "\n")

	   fun pritem (name,TIME_ENTRY(ref {count,max,top_timer,sum,active,...})) =
	     let
	       val time_cpu = triple2cpu sum
	       val time_real = triple2real sum
	       val per_call = (time_cpu * 1000.0)/(Real.fromInt count) (*In milliseconds!*)
	       val per_call = (Real.realFloor(per_call * 10.0)) / 10.0
	       val timer_overhead = timer_time count
	       val time_cpu_string = real2stringWith 2 time_cpu
	       val time_real_string = real2stringWith 2 time_real
	       val time_gc_string   = real2stringWith 2 (Time.toReal (#gc sum))
	       val count_string = Int.toString count
	       val per_call_string = real2stringWith 1 per_call
	       val max_string = Int.toString (Real.trunc (max*1000.0)) (*In milliseconds (truncate, since beyond resolution)*)
	       fun percent frac =
		   let val per = frac * 100.0
		       val str = "(" ^ (real2stringWith 1 per) ^ ")"
		   in  if (size str <= 5) then " " ^ str else str
		   end
	       val percent_cpu_string =
		   if top_timer then (percent (time_cpu/total_cpu)) else ""
	       val percent_real_string =
		 if top_timer then (percent (time_real/total_real)) else ""

	       (* If timer is active, print a warning flag.
		* If the timer overhead is greater than 1 second, print it.
		* Otherwise, if realtime is more than twice cpu time, print
		* a warning flag *)
	       val warning_flag = (if !active then "ON!"
	                           else if timer_overhead > 1.0 then real2stringWith 2 timer_overhead
				   else if (time_real > time_cpu * 2.0) then "***"
				   else "")
	     in
	       print_strings(name,count_string,per_call_string,max_string,
			     time_cpu_string,percent_cpu_string,time_gc_string,
			     time_real_string,percent_real_string, warning_flag)
	     end
	     | pritem _ = ()
	 in
	   print "\nGlobal timings\n";
	   print_strings("Timer Name","Calls","Avg","Max",
			 "cpu","cpu","gc","real"," real","flags");
	   print_strings("","","(ms)","(ms)",
			 "(s)","(%)","(s)","(s)"," (%)","");
	   print "----------------------------------------------------------------------------------------------------\n";
	   app pritem entries;
	   print "----------------------------------------------------------------------------------------------------\n";
	   lprint max_name_size "TOTAL CPU TIME";
	   print " : ";
	   print (real2stringWith 2 total_cpu);
	   print " seconds\n";
	   lprint max_name_size "TOTAL REAL TIME";
	   print " : ";
	   print (real2stringWith 2 total_real);
	   print " seconds\n";
	   let val lines = !(int "SourceLines")
	   in  if lines > 0
		 then (lprint max_name_size "OVERALL RATE";
		       print " : ";
		       print (real2stringWith 2 ((Real.fromInt lines) / total_real));
		       print " lines/second\n")
	       else ()
	   end;
	   print (Date.toString(Date.fromTimeLocal(Time.now())));
	   print "\n\n"
	 end

      fun print_counters'() =
        let
	       fun pritem (name,COUNTER_ENTRY(ref count)) =
		       (lprint 30 name;
		        print " : ";
		        rprint 8 (Int.toString count);
			print "\n")
	         | pritem _ = ()
	in
	     print "Global counters\n";
	     print "-------------------------------------------\n";
	     app pritem (rev(!entries));
(* (StringMap.listItemsi(!entries)); *)
	     print "-------------------------------------------\n"
	 end

      fun print_ints'() =
        let
	       fun pritem (name,INT_ENTRY(ref count)) =
		       (lprint 30 name;
		        print " : ";
		        rprint 8 (Int.toString count);
			print "\n")
	         | pritem _ = ()
	in
	    print "Global integer statistics\n";
	     print "-------------------------------------------\n";
	     app pritem (rev(!entries));
	     print "-------------------------------------------\n"
	 end

      fun print_bools'() =
        let
	       fun pritem (name,BOOL_ENTRY(ref flag)) =
		       (lprint 30 name;
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


      fun make_sorted () =
	let
	  val orig_entries = !entries
	  val sort_entries = ListMergeSort.sort (fn ((n1,_),(n2,_)) => String.<(n1,n2)) orig_entries
	in entries := sort_entries
	end

      fun print_bools ()    = (make_sorted();print_bools'())
      fun print_counters () = (make_sorted();print_counters'())
      fun print_ints ()     = (make_sorted();print_ints'())

      val sort_timers = ff "StatsSortTimers"

      fun print_timers ()   = (if !sort_timers then make_sorted() else ();
			       print_timers'() )

      fun print_stats() =
	let val orig_entries = !entries
	in
	  make_sorted();
	  print "\n\n";
	  print_bools'();
	  print "\n\n";
	  print_counters'();
	  print "\n\n";
	  print_ints'();
	  print "\n\n";
	  if !sort_timers then () else entries := orig_entries;
	  print_timers'();
	  print "\n\n"
	end

   end
