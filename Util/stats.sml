structure Stats :> STATS =
struct

    structure Map = Util.StringMap
    structure B = Blaster

    val error = fn s => Util.error "stats.sml" s

    fun apply (f:'a -> 'b) (x:'a) : (unit -> 'b) =
        let val r = f x
        in  fn () => r
        end handle e => fn () => raise e

    (*
        Counters
    *)

    datatype combine = ADD | MAX

    fun blastOutCombine (os:B.outstream) (c:combine) : unit =
        (case c of
            ADD => B.blastOutInt os 0
        |   MAX => B.blastOutInt os 1)

    fun blastInCombine (is:B.instream) : combine =
        (case (B.blastInInt is) of
            0 => ADD
        |   1 => MAX
        |   _ => error "blastInCombine")

    (* Invariant: If {count,...} : counter, then !count >= 0. *)

    type counter =
        {count:int ref,
         combine:combine}

    fun make_counter (combine:combine) : counter =
        {count = ref 0,
         combine = combine}

    fun counter_fetch (counter:counter) : int =
        let val {count,...} = counter
        in  !count
        end

    fun counter_clear (counter:counter) : unit =
        let val {count,...} = counter
        in  count := 0
        end

    fun counter_changed (counter:counter) : bool =
        counter_fetch counter > 0

    (* Restricts i more than the invariant requires. *)
    fun counter_add (counter:counter, i:int) : unit =
        if i >= 0 then
            let val {count,...} = counter
            in  count := !count + i
            end
        else error "trying to decrease counter"

    fun counter_max (counter:counter, i:int) : unit =
        let val {count,...} = counter
        in  count := Int.max(!count,i)
        end

    fun counter_inc (counter:counter) : unit =
        let val {count,...} = counter
        in  count := !count + 1
        end

    fun counter_acc (master:counter, slave:int) : unit =
        let val {count,combine} = master
            val combine : int * int -> int =
                (case combine of
                    ADD => op +
                |   MAX => Int.max)
            val _ = count := combine (!count,slave)
        in  ()
        end

    (*
        Timers
    *)

    (* Invariant:  If {gc,cpu,real} : time, then gc,cpu,real >= 0 *)

    type time =
        {gc:real,
         cpu:real,  (* user and system *)
         real:real}

    (*
        Gc time is included in cpu time - don't overcount.
    *)
    fun time_combine (time:time) : real =
        let val {gc,cpu,real} = time
        in  cpu + real
        end

    val time_zero : time = {gc=0.0, cpu=0.0, real=0.0}

    fun time_add (a:time, b:time) : time =
        let val {gc=gca, cpu=cpua, real=reala} = a
            val {gc=gcb, cpu=cpub, real=realb} = b
        in  {gc = gca + gcb,
             cpu = cpua + cpub,
             real = reala + realb}
        end

    fun time_apply (f:'a -> 'b) (x:'a) : time * (unit -> 'b) =
        let val cpu = Timer.startCPUTimer()
            val real = Timer.startRealTimer()
            val r = apply f x
            val {usr,sys,gc} = Timer.checkCPUTimer cpu
            val real = Time.toReal(Timer.checkRealTimer real)
            val gc = Time.toReal gc
            val cpu = Time.toReal(Time.+(sys,usr))  (* usr includes gc *)
            val time = {gc=gc, cpu=cpu, real=real}
        in  (time,r)
        end

    fun blastOutTime (os:B.outstream) (time:time) : unit =
        let val {gc,cpu,real} = time
        in  B.blastOutReal os gc;
            B.blastOutReal os cpu;
            B.blastOutReal os real
        end

    fun blastInTime (is:B.instream) : time =
        {gc = B.blastInReal is,
         cpu = B.blastInReal is,
         real = B.blastInReal is}

    (* Invariant: If {max,last,count,...} : times, then max,last,count >= 0 *)

    type times =
        {total:time,
         max:real,  (* combined *)
         last:real, (* combined *)
         count:int}

    val times_zero : times =
        {total=time_zero, max=0.0, last=0.0, count=0}

    fun times_addtime (times:times, time:time) : times =
        let val {total, max, last, count} = times
            val total = time_add(total, time)
            val t = time_combine time
            val max = Real.max(max,t)
            val count = count + 1
        in  {total=total, max=max, last=t, count=count}
        end

    fun times_acc (master:times, slave:times) : times =
        let val {total, max, count, ...} = master
            val {total=stotal, max=smax, last, count=scount} = slave
            val total = time_add(total,stotal)
            val max = Real.max(max,smax)
            val count = count + scount
        in  {total=total, max=max, last=last, count=count}
        end

    fun blastOutTimes (os:B.outstream) (times:times) : unit =
        let val {total,max,last,count} = times
        in  blastOutTime os total;
            B.blastOutReal os max;
            B.blastOutReal os last;
            B.blastOutInt os count
        end

    fun blastInTimes (is:B.instream) : times =
        {total = blastInTime is,
         max = B.blastInReal is,
         last = B.blastInReal is,
         count = B.blastInInt is}

    fun times_is_zero (times:times) : bool =
        let val {count,...} = times
        in  count = 0
        end

    type timer =
        {toplevel:bool,
         avoid_overlap:bool,
         times:times ref,
         active:bool ref}

    fun make_timer (toplevel:bool, avoid_overlap:bool) : timer =
        {toplevel=toplevel,
         avoid_overlap=avoid_overlap,
         times=ref times_zero,
         active=ref false}

    fun timer_clear (timer:timer) : unit =
        let val {active,times,...} = timer
            val _ = active := false
            val _ = times := times_zero
        in  ()
        end

    fun timer_changed (timer:timer) : bool =
        let val {times, ...} = timer
        in  not(times_is_zero (!times))
        end

    fun timer_active (timer:timer) : bool =
        let val {active,...} = timer
        in  !active
        end

    fun timer_apply (timer:timer) (f:'a -> 'b) (x:'a) : 'b =
        let val {avoid_overlap, active, times, ...} = timer
        in  if avoid_overlap andalso !active then
                f x
            else
                let val _ = active := true
                    val (time,r) = time_apply f x
                    val _ = active := false
                    val _= times := times_addtime(!times,time)
                in  r()
                end
        end

    fun timer_fetch (timer:timer) : times =
        let val {times, ...} = timer
        in  !times
        end

    fun timer_acc (timer:timer, slave:times) : unit =
        let val {times,...} = timer
            val _ = times := times_acc(!times, slave)
        in  ()
        end

    (*
        Estimate timer overhead; this is just used to warn the
        user so we do not try to account for differences
        between the master and slaves.  It is not worth fixing
        this bug.  Run your master and slave on the same
        machine when you want an accurate warning.
    *)
    fun timer_overhead () : real =
        let
            (* Estimate the smallest measurable time. *)
            val cputimer = Timer.startCPUTimer()
            fun elapsed () : real =
                let val {sys,usr,...} = Timer.checkCPUTimer cputimer
                in  Time.toReal(Time.+(sys,usr))
                end
            fun find_min_time (timeout:int) : real =
                if timeout = 0 then 0.0
                else
                    let val t = elapsed()
                    in  if Real.==(t,0.0) then
                            find_min_time(timeout-1)
                        else t
                    end
            val min_time = find_min_time 10000

            (* Measure a reasonable number of timed function applications. *)
            fun iter (f:'a -> 'b, x:'a, n:int) : unit =
                if n = 0 then ()
                else (f x; iter(f,x,n-1))
            val target_time = 30.0 * min_time
            fun measure (f:'a -> 'b, x:'a) : int * real =
                let fun loop (n:int) : int * real =
                        let val timer = make_timer(true,false)
                            val _ = iter (timer_apply timer f,x,n)
                            val {total,...} = timer_fetch timer
                            val {cpu,...} = total
                        in  if cpu >= target_time then
                                (n,cpu)
                            else loop(n+n)
                        end
                in  loop 1
                end
            fun trivial () = ()
            val (count,slow) = measure (trivial,())

            (* Measure the same number of normal function applications. *)
            fun measure (f:'a -> 'b, x:'a) : real =
                let val (time,_) = time_apply iter (f,x,count)
                    val {cpu,...} = time
                in  cpu
                end
            val fast = measure (trivial,())

            val overhead =
                if count = 0 orelse slow < fast then 0.0
                else (slow - fast)/real count
        in  overhead
        end
    val timer_overhead = Util.memoize timer_overhead

    (*
        Stats
    *)

    datatype flag =
        INT of int ref
    |   BOOL of bool ref

    datatype meas =
        COUNTER of counter
    |   TIMER of timer

    datatype stat =
        FLAG of flag
    |   MEAS of meas

    (*
        Invariant:
        If {map,order} : stats,
        then order=dom(map)
        and (rev order) is the order of stat creation.
    *)
    type stats =
        {map:stat Map.map,
         order:string list} (* backwards *)

    val stats_empty : stats =
        {map=Map.empty,
         order=nil}

    val stats : stats ref = ref stats_empty

    fun clear_measurements () : unit =
        let
            fun clear (stat:stat) : unit =
                (case stat of
                    FLAG _ => ()
                |   MEAS (COUNTER c) => counter_clear c
                |   MEAS (TIMER t) => timer_clear t)
            val {map,...} = !stats
            val _ = Map.app clear map
        in  ()
        end

    fun lookup_stat (name:string) : stat option =
        let val {map,...} = !stats
        in  Map.find (map,name)
        end

    fun require_stat (what:string, name:string) : stat =
        (case (lookup_stat name) of
            SOME stat => stat
        |   NONE => error ("trying to use nonexistent " ^ what ^ ": " ^ name))

    fun get_stat (maker:unit -> stat, name:string) : stat =
        (case (lookup_stat name) of
            SOME stat => stat
        |   NONE =>
                let val stat = maker()
                    val {map,order} = !stats
                    val map = Map.insert(map,name,stat)
                    val order = name::order
                    val _ = stats := {map=map, order=order}
                in  stat
                end)

    val Int = "int"
    val Bool = "bool"
    val Counter = "counter"
    val Timer = "timer"

    fun stat_ty (stat:stat) : string =
        (case stat of
            FLAG (INT _) => Int
        |   FLAG (BOOL _) => Bool
        |   MEAS (COUNTER _) => Counter
        |   MEAS (TIMER _) => Timer)

    fun mismatch (name:string, expected:string, found:stat) : 'a =
        let val found = stat_ty found
            val msg = concat [
                "stat ", name, " mismatch; expected ", expected,
                " found ", found
            ]
        in  error msg
        end

    fun get_int (default:int) (name:string) : int ref =
        let fun maker () = FLAG(INT (ref default))
        in
            (case (get_stat(maker,name)) of
                FLAG (INT r) => r
            |   stat => mismatch (name,Int,stat))
        end

    fun require_int (name:string) : int ref =
        (case (require_stat (Int,name)) of
            FLAG (INT r) => r
        |   stat => mismatch (name,Int,stat))

    fun get_bool (default:bool) (name:string) : bool ref =
        let fun maker () = FLAG(BOOL(ref default))
        in
            (case (get_stat(maker,name)) of
                FLAG (BOOL r) => r
            |   stat => mismatch (name,Bool,stat))
        end

    fun require_bool (name:string) : bool ref =
        (case (require_stat(Bool,name)) of
            FLAG (BOOL r) => r
        |   stat => mismatch (name,Bool,stat))

    fun get_counter (combine:combine) (name:string) : counter =
        let fun maker () = MEAS(COUNTER(make_counter combine))
        in
            (case (get_stat(maker,name)) of
                MEAS (COUNTER c) => c
            |   stat => mismatch(name,Counter,stat))
        end

    fun require_counter (name:string) : counter =
        (case (require_stat (Counter,name)) of
            MEAS (COUNTER c) => c
        |   stat => mismatch(name,Counter,stat))

    fun get_timer (options:bool * bool) (name:string) : timer =
        let fun maker () = MEAS(TIMER(make_timer options))
        in
            (case (get_stat(maker,name)) of
                MEAS(TIMER timer) => timer
            |   stat => mismatch(name,Timer,stat))
        end

    fun require_timer (name:string) : timer =
        (case (require_stat(Timer,name)) of
            MEAS(TIMER timer) => timer
        |   stat => mismatch(name,Timer,stat))

    fun int (name:string, default:int) : int ref =
        get_int default name

    val bool : string -> bool ref = require_bool

    val tt : string -> bool ref =
        get_bool true

    val ff : string -> bool ref =
        get_bool false

    val counter : string -> counter =
        get_counter ADD

    val counter' : string -> counter =
        get_counter MAX

    fun timer_help (options:bool*bool) (name:string, f:'a -> 'b) : 'a -> 'b =
        let val timer = get_timer options name
        in  timer_apply timer f
        end

    val timer : string * ('a -> 'b) -> 'a -> 'b =
        fn x => timer_help (true,false) x

    val subtimer : string * ('a -> 'b) -> 'a -> 'b =
        fn x => timer_help (false,false) x

    val timer' : string * ('a -> 'b) -> 'a -> 'b =
        fn x => timer_help (true,true) x

    val subtimer' : string * ('a -> 'b) -> 'a -> 'b =
        fn x => timer_help (false,true) x

    fun fetch_timer_times (name:string) : times =
        let val timer = require_timer name
            val times = timer_fetch timer
        in  times
        end

    val fetch_timer_last : string -> real =
        #last o fetch_timer_times

    (*
        Printing
    *)
    local
        fun loop (i:int) : unit = if i < 0 then () else (print " "; loop (i-1))
    in
        fun lprint (max_size:int) (str:string) : unit =
            (print str; loop (max_size - (size str)))
        fun rprint (max_size:int) (str:string): unit=
            (loop (max_size - (size str)); print str)
    end

    fun entries (sort:bool) : string list =
        let val {order,map} = !stats
        in  if sort then
                Map.foldri (fn (name,_,acc) => name::acc) nil map
            else
                rev order
        end

    val StatsSortTimers = ff "StatsSortTimers"

    fun print_timers () : unit =
        let val order = entries (!StatsSortTimers)
            fun timer (name:string) : (string * timer) option =
                (case (lookup_stat) name of
                    SOME (MEAS(TIMER timer)) => SOME (name,timer)
                |   _ => NONE)
            val timers = List.mapPartial timer order

            fun folder ((name:string,timer:timer), (cpu:real, real:real)) : real * real =
                if #toplevel timer then
                    let val times = timer_fetch timer
                        val {total,...} = times
                        val {cpu=cpu',real=real',...} = total
                    in  (cpu+cpu',real+real')
                    end
                else (cpu,real)
            val (total_cpu, total_real) = foldl folder (0.0,0.0) timers

            fun real2string (prec:int) (r:real) : string =
                Real.fmt (StringCvt.FIX (SOME prec)) r

            val max_name_size =
                foldl (fn ((n,_),m) => Int.max(m,size n))
                10 timers
            val max_name_size = max_name_size + 1
            fun layout (name:string,count:string,per_call:string,
                max:string,time_cpu:string,percent_cpu:string,
                time_gc:string,time_real:string,percent_real:string,
                warning:string) : unit =
                (lprint max_name_size name;
                 print " | ";
                 rprint 6 count;
                 rprint 7 per_call;
                 rprint 7 max;
                 print " | ";
                 rprint 7 time_cpu;
                 rprint 7 percent_cpu;
                 print " | ";
                 rprint 6 time_gc;
                 print " | ";
                 rprint 7 time_real;
                 rprint 7 percent_real;
                 print " | ";
                 lprint 4 warning;
                 print "\n")
            fun print_timer (name:string, timer:timer) : unit =
                let val {toplevel,times,active,...} = timer
                    val {total,max,last,count} = !times
                    val {cpu,real,gc} = total
                    val count' = Real.fromInt count
                    val per_call = ((time_combine total)*1000.0)/count' (* ms *)
                    val per_call = (Real.realFloor(per_call*10.0))/10.0
                    val overhead = timer_overhead() * count'
                    val max = Int.toString(trunc (max*1000.0))  (* ms, truncated since beyond resolution *)
                    val warn_active = !active
                    val warn_overhead = overhead > 1.0
                    val warn_time = real > cpu * 2.0
                    fun percent (frac:real) : string =
                        if toplevel then
                            let val per = frac*100.0
                                val str = "(" ^ real2string 1 per ^ ")"
                            in  if size str <= 5 then " " ^ str else str
                            end
                        else ""
                    val percent_cpu = percent (cpu/total_cpu)
                    val percent_real = percent (real/total_real)
                    val cpu = real2string 2 cpu
                    val real = real2string 2 real
                    val gc = real2string 2 gc
                    val count = Int.toString count
                    val per_call = real2string 1 per_call
                    (*
                        Warn if timer is active, overhead is greater than 1
                        second, or real time is more than twice cpu time.
                    *)
                    val warning =
                        if warn_active then "ON!"
                        else if warn_overhead then real2string 2 overhead
                        else if warn_time then "***"
                        else ""
                in  layout(name,count,per_call,max,
                        cpu,percent_cpu,gc,
                        real,percent_real,warning)
                end
        in
            print "\nGlobal timings\n";
            layout("Timer Name","Calls","Avg","Max",
                "cpu","cpu","gc","real"," real","flags");
            layout("","","(ms)","(ms)",
                "(s)","(%)","(s)","(s)"," (%)","");
            print "----------------------------------------------------------------------------------------------------\n";
            app print_timer timers;
            print "----------------------------------------------------------------------------------------------------\n";
            lprint max_name_size "TOTAL CPU TIME";
            print " : ";
            print (real2string 2 total_cpu);
            print " seconds\n";
            lprint max_name_size "TOTAL REAL TIME";
            print " : ";
            print (real2string 2 total_real);
            print " seconds\n";
            let val lines = counter_fetch(counter"SourceLines")
            in  if lines > 0 andalso total_real > 0.0 then
                    (lprint max_name_size "OVERALL RATE";
                     print " : ";
                     print (real2string 2 ((Real.fromInt lines) / total_real));
                    print " lines/second\n")
                else ()
            end;
            print (Date.toString(Date.fromTimeLocal(Time.now())));
            print "\n\n"
        end

    fun print_counters () : unit =
        let val order = entries true
            fun counter (name:string) : (string * counter) option =
                (case (lookup_stat) name of
                    SOME (MEAS(COUNTER c)) => SOME (name,c)
                |   _ => NONE)
            val counters = List.mapPartial counter order

            fun print_counter (name:string, counter : counter) : unit =
                (lprint 30 name;
                 print " : ";
                 rprint 8 (Int.toString (counter_fetch counter));
                 print "\n")

        in
            print "Global counters\n";
            print "-------------------------------------------\n";
            app print_counter counters;
            print "-------------------------------------------\n"
        end

    fun print_ints () : unit =
        let val order = entries true
            fun int (name:string) : (string * int ref) option =
                (case (lookup_stat) name of
                    SOME (FLAG (INT r)) => SOME (name,r)
                |   _ => NONE)
            val ints = List.mapPartial int order
            fun print_int (name:string, ref n:int ref) : unit =
                (lprint 30 name;
                 print " : ";
                 rprint 8 (Int.toString n);
                 print "\n")
        in
            print "Global integer flags\n";
            print "-------------------------------------------\n";
            app print_int ints;
            print "-------------------------------------------\n"
        end

    fun print_bools () : unit =
        let val order = entries true
            fun bool (name:string) : (string * bool ref) option =
                (case (lookup_stat) name of
                    SOME (FLAG (BOOL r)) => SOME (name,r)
                |   _ => NONE)
            val bools = List.mapPartial bool order

            fun print_bool (name:string, ref flag:bool ref) : unit =
                (lprint 30 name;
                 print " : ";
                 print (Bool.toString flag);
                 print "\n")
        in
            print "Global boolean flags\n";
            print "-------------------------------------------\n";
            app print_bool bools;
            print "-------------------------------------------\n"
        end

    fun print_stats() =
        (print "\n\n";
         print_bools();
         print "\n\n";
         print_ints();
         print "\n\n";
         print_counters();
         print "\n\n";
         print_timers();
         print "\n\n")

    fun print_measurements() =
        (print "\n\n";
         print_counters();
         print "\n\n";
         print_timers();
         print "\n\n")
    (*
        Communication
    *)
    datatype flagv =
        INTV of int
    |   BOOLV of bool

    type flags = (string * flagv) list

    fun get_flags () : flags =
        let val order = entries false
            fun flag (name:string) : (string * flagv) option =
                (case (lookup_stat) name of
                    SOME (FLAG (INT (ref i))) => SOME (name,INTV i)
                |   SOME (FLAG (BOOL (ref b))) => SOME (name,BOOLV b)
                |   _ => NONE)
            val flags = List.mapPartial flag order
        in  flags
        end

    fun set_flags (flags:flags) : unit =
        let fun set (name:string, flagv:flagv) : unit =
                (case flagv of
                    INTV n =>
                        let val r = get_int 0 name
                        in  r := n
                        end
                |   BOOLV b =>
                        let val r = get_bool true name
                        in  r := b
                        end)
            val _ = app set flags
        in  ()
        end

    fun blastOutFlagv (os:B.outstream) (flagv:flagv) : unit =
        (case flagv of
            INTV i => (B.blastOutInt os 0; B.blastOutInt os i)
        |   BOOLV b => (B.blastOutInt os 1; B.blastOutBool os b))

    fun blastInFlagv (is:B.instream) : flagv =
        (case (B.blastInInt is) of
            0 => INTV (B.blastInInt is)
        |   1 => BOOLV (B.blastInBool is)
        |   _ => error "blastInFlagv")

    val blastOutFlags : B.outstream -> flags -> unit =
        B.blastOutList (B.blastOutPair B.blastOutString blastOutFlagv)

    val blastInFlags : B.instream -> flags =
        B.blastInList (B.blastInPair B.blastInString blastInFlagv)

    val (blastOutFlags,blastInFlags) =
        B.magic (blastOutFlags,blastInFlags,"flags $Revision$")

    datatype measv =
        COUNTERV of combine * int
    |   TIMERV of (bool * bool) * times

    type measurements = (string * measv) list

    fun get_measurements () : measurements =
        let val order = entries false
            fun measv (name:string) : (string * measv) option =
                (case (lookup_stat) name of
                    SOME (MEAS (COUNTER c)) =>
                        if counter_changed c then
                            let val {combine,...} = c
                                val count = counter_fetch c
                                val counterv = COUNTERV (combine,count)
                            in  SOME (name,counterv)
                            end
                        else NONE
                |   SOME (MEAS (TIMER t)) =>
                        if timer_changed t then
                            let val {toplevel,avoid_overlap,...} = t
                                val times = timer_fetch t
                                val timerv =
                                    TIMERV ((toplevel,avoid_overlap),times)
                            in  SOME (name, timerv)
                            end
                        else NONE
                |   _ => NONE)
            val changed = List.mapPartial measv order
        in  changed
        end

    fun add_measurements (changed:measurements) : unit =
        let fun add (name:string, measv:measv) : unit =
                (case measv of
                    COUNTERV (options,count) =>
                        let val counter = get_counter options name
                        in  counter_acc(counter,count)
                        end
                |   TIMERV (options,times) =>
                        let val timer = get_timer options name
                        in  timer_acc(timer,times)
                        end)
        in  app add changed
        end

    fun blastOutMeasv (os:B.outstream) (measv:measv) : unit =
        (case measv of
            COUNTERV (combine,i) =>
                (B.blastOutInt os 0; blastOutCombine os combine; B.blastOutInt os i)
        |   TIMERV ((toplevel,avoid_overlap),times) =>
                (B.blastOutInt os 1; B.blastOutBool os toplevel;
                 B.blastOutBool os avoid_overlap; blastOutTimes os times))

    fun blastInMeasv (is:B.instream) : measv =
        (case (B.blastInInt is) of
            0 => COUNTERV (blastInCombine is, B.blastInInt is)
        |   1 => TIMERV ((B.blastInBool is, B.blastInBool is), blastInTimes is)
        |   _ => error "blastInMeasv")

    val blastOutMeasurements : B.outstream -> measurements -> unit =
        B.blastOutList (B.blastOutPair B.blastOutString blastOutMeasv)

    val blastInMeasurements : B.instream -> measurements =
        B.blastInList (B.blastInPair B.blastInString blastInMeasv)

    val (blastOutMeasurements,blastInMeasurements) =
        B.magic (blastOutMeasurements,blastInMeasurements,"meas $Revision$")

end
