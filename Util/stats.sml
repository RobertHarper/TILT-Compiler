structure Stats :> STATS =
struct

       structure StringMap = Util.StringMap
       structure B = Blaster

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

       fun reset_stats() = (entries := [])

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
	      val new_entry = update_entry entry cpu real
	      val _ = entry_ref := new_entry
	      val _ = active := false
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

    type time_snap =
	{count:int, max:real, last:real,
	 sum:{gc:time, sys:time, usr:time, real:time}}
    type counter_snap = int
    type int_snap = int
    type bool_snap = bool
    datatype snap =
	TIME_SNAP of time_snap
      | COUNTER_SNAP of counter_snap
      | INT_SNAP of int_snap
      | BOOL_SNAP of bool_snap

    fun get' (entry:entry) : snap =
	(case entry
	   of TIME_ENTRY (ref {active=ref true,...}) => error "getting active timer"
	    | TIME_ENTRY (ref {count,max,last,sum,...}) =>
		TIME_SNAP {count=count, max=max, last=last, sum=sum}
	    | COUNTER_ENTRY (ref n) => COUNTER_SNAP n
	    | INT_ENTRY (ref n) => INT_SNAP n
	    | BOOL_ENTRY (ref b) => BOOL_SNAP b)

    fun set' (entry:entry, snap:snap) : unit =
	(case (entry, snap)
	   of (TIME_ENTRY r, TIME_SNAP b) =>
		let val {top_timer,active,...} = !r
		    val _ = if !active then error "setting active timer" else ()
		    val {count,max,last,sum} = b
		    val ent =
			{count=count, max=max, last=last,
			 top_timer=top_timer, active=active, sum=sum}
		in  r := ent
		end
	    | (COUNTER_ENTRY r, COUNTER_SNAP b) => r := b
	    | (INT_ENTRY r, INT_SNAP b) => r := b
	    | (BOOL_ENTRY r, BOOL_SNAP b) => r := b
	    | _ => error "entry mismatch in set'")

fun sub_time what (new,old) =
	if Time.<(new,old) then
	    error ("sub_time " ^ Time.toString new ^ " - " ^ Time.toString old ^ "\n")
	else Time.-(new,old)

    fun sub' (snap:snap, snap':snap) : snap option =
	(case (snap,snap')
	   of (TIME_SNAP a, TIME_SNAP b) =>
		let val {count, max, last, sum={gc,sys,usr,real}} = a
		    val {count=count', max=max', last=last',
			 sum={gc=gc', sys=sys', usr=usr', real=real'}} = b
		    val changed =
			count <> count' orelse
			Real.!= (max,max') orelse
			Real.!= (last,last') orelse
			gc <> gc' orelse
			sys <> sys' orelse
			usr <> usr' orelse
			real <> real'
		in  if changed then
			let val count = count - count'
			    val gc = sub_time "gc" (gc,gc')
			    val sys = sub_time "sys" (sys,sys')
			    val usr = sub_time "usr" (usr,usr')
			    val real = sub_time "real" (real,real')
			    val sum = {gc=gc, sys=sys, usr=usr, real=real}
			    val ent = {count=count, max=max, last=last, sum=sum}
			in  SOME (TIME_SNAP ent)
			end
		    else NONE
		end
	    | (COUNTER_SNAP n, COUNTER_SNAP n') =>
		if n <> n' then SOME (COUNTER_SNAP (n-n')) else NONE
	    | (INT_SNAP n, INT_SNAP n') =>
		if n <> n' then SOME (INT_SNAP n) else NONE
	    | (BOOL_SNAP b, BOOL_SNAP b') =>
		if b <> b' then SOME (BOOL_SNAP b) else NONE
	    | _ => error "entry mismatch in subtract")

    fun add' (snap:snap, snap':snap) : snap =
	(case (snap, snap')
	   of (TIME_SNAP a, TIME_SNAP b) =>
		let val {count,max,sum={gc,sys,usr,real},...} = a
		    val {count=count', max=max', last=last',
			 sum={gc=gc', sys=sys', usr=usr', real=real'}} = b
		    val count = count+count'
		    val max = Real.max (max,max')
		    val gc = Time.+(gc,gc')
		    val sys = Time.+(sys,sys')
		    val usr = Time.+(usr,usr')
		    val real = Time.+(real,real')
		    val ent =
			{count=count, max=max, last=last',
			 sum={gc=gc, sys=sys, usr=usr, real=real}}
		in  TIME_SNAP ent
		end
	    | (COUNTER_SNAP a, COUNTER_SNAP b) => COUNTER_SNAP(a+b)
	    | (INT_SNAP _, INT_SNAP _) => snap'
	    | (BOOL_SNAP _, BOOL_SNAP _) => snap'
	    | _ => error "entry mismatch in add")

    type stats = snap StringMap.map
    type delta = stats

    fun from_list (stats : (string * snap) list) : stats =
	let fun folder ((n,s),m) = StringMap.insert (m,n,s)
	in  foldl folder StringMap.empty stats
	end

    fun get () : stats =
	let val stats = map (fn (n,e) => (n,get' e)) (!entries)
	in  from_list stats
	end

    fun set (stats:stats) : unit =
	let fun apper (n,e) =
		(case StringMap.find (stats,n)
		   of SOME s => set'(e,s)
		    | NONE => ())
	in  app apper (!entries)
	end

    fun sub (a:stats, b:stats) : delta =
	let fun mapper (n,s) : snap option =
		(case StringMap.find (b,n)
		   of SOME s' => sub' (s,s')
		    | NONE => NONE)
	in  StringMap.mapPartiali mapper a
	end

    fun add (stats:stats, delta:delta) : stats =
	let fun mapper (n,s) : snap =
		(case StringMap.find (delta,n)
		   of SOME s' => add'(s,s')
		    | NONE => s)
	in  StringMap.mapi mapper stats
	end

    fun blastOutSnap (os:B.outstream) (s:snap) : unit =
	(case s
	   of TIME_SNAP {count,max,last,sum={gc,sys,usr,real}} =>
		(B.blastOutInt os 0; B.blastOutInt os count;
		 B.blastOutReal os max; B.blastOutReal os last;
		 B.blastOutTime os gc; B.blastOutTime os sys;
		 B.blastOutTime os usr; B.blastOutTime os real)
	    | COUNTER_SNAP n => (B.blastOutInt os 1; B.blastOutInt os n)
	    | INT_SNAP n => (B.blastOutInt os 2; B.blastOutInt os n)
	    | BOOL_SNAP b => (B.blastOutInt os 3; B.blastOutBool os b))
    fun blastInSnap (is:B.instream) : snap =
	(case B.blastInInt is
	   of 0 =>
		let val count = B.blastInInt is
		    val max = B.blastInReal is
		    val last = B.blastInReal is
		    val gc = B.blastInTime is
		    val sys = B.blastInTime is
		    val usr = B.blastInTime is
		    val real = B.blastInTime is
		    val ent =
			{count=count,max=max,last=last,
			 sum={gc=gc,sys=sys,usr=usr,real=real}}
		in  TIME_SNAP ent
		end
	    | 1 => COUNTER_SNAP (B.blastInInt is)
	    | 2 => INT_SNAP (B.blastInInt is)
	    | 3 => BOOL_SNAP (B.blastInBool is)
	    | _ => error "bad snap")

    fun blastOutStats (os:B.outstream) (s:stats) : unit =
	let val entries = StringMap.listItemsi s
	    val blastout = B.blastOutPair B.blastOutString blastOutSnap
	in  B.blastOutList blastout os entries
	end
    fun blastInStats (is:B.instream) : stats =
	let val blastin = B.blastInPair B.blastInString blastInSnap
	    val entries = B.blastInList blastin is
	in  from_list entries
	end

    val (blastOutDelta, blastInDelta) =
	B.magic (blastOutStats, blastInStats, "delta $Revision$")

    val (blastOutStats, blastInStats) =
	B.magic (blastOutStats, blastInStats, "stats $Revision$")

end
