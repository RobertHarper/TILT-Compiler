(*$import TIMER *)

structure Timer :> TIMER =
  struct

    type cpu_timer = int
    type real_timer = int

    fun totalCPUTimer() = 1
    fun startCPUTimer() = 1
    fun checkCPUTimer (_ : cpu_timer) = 
	{usr = Time.zeroTime,
	sys = Time.zeroTime,
	gc = Time.zeroTime}


    fun totalRealTimer() = 5
    fun startRealTimer() = 5
    fun checkRealTimer(_ : real_timer) = Time.zeroTime

  end 