structure Timer :> TIMER =
  struct

    type cpu_timer = {user : Time.time, sys : Time.time, gc : Time.time}
    type real_timer = {real : Time.time}

    fun ccall (f : ('a, 'b cresult) -->, a:'a) : 'b =
	(case (Ccall(f,a)) of
	    Normal r => r
	|   Error e => raise e)

    fun getCPUTimer() : cpu_timer =
	let val (user_sec, user_usec, sys_sec, sys_usec) = ccall(posix_time_getrusage_self,())
	in  {user = Time.+(Time.fromSeconds(user_sec),
			   Time.fromMicroseconds(user_usec)),
	     sys =  Time.+(Time.fromSeconds(sys_sec),
			   Time.fromMicroseconds(sys_usec)),
	     gc = Time.zeroTime}
	end
    fun getRealTimer() : real_timer =
	let val (sec, msec) = Ccall(posix_time_ftime,())
	in  {real = Time.+(Time.fromSeconds(sec),
			   Time.fromMilliseconds(msec))}
	end

    fun totalCPUTimer() = {user = Time.zeroTime, sys = Time.zeroTime, gc = Time.zeroTime}
    fun startCPUTimer() = getCPUTimer()
    fun checkCPUTimer ({user = user1, sys = sys1, gc = gc1} : cpu_timer) =
	let val {user = user2, sys = sys2, gc = gc2} = getCPUTimer()
	in  {usr = Time.-(user2, user1),
	     sys = Time.-(sys2, sys1),
	     gc = Time.-(gc2, gc1)}
	end

    fun totalRealTimer() = {real = Time.zeroTime}
    fun startRealTimer() = getRealTimer()
    fun checkRealTimer ({real = real1} : real_timer) =
	let val {real = real2} = getRealTimer()
	in  Time.-(real2, real1)
	end

  end
