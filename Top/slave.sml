structure Slave :> SLAVE =
struct
    val error = fn s => Util.error "slave.sml" s
    val TimeEachFile = Stats.ff "TimeEachFile"
    val SlaveDiag = Stats.ff "SlaveDiag"
    fun msg str = if (!SlaveDiag) then print str else ()

    type state = Comm.identity * Comm.in_channel * Comm.out_channel

    fun wait (s : state) : unit -> unit =
	let val (identity,_,_) = s
	    val from = Comm.fromMaster identity
	    val to = Comm.toMaster identity
	    val timer : Time.time option ref = ref NONE
	    val delay = Time.fromSeconds 10
	    fun startTimer () : unit = timer := SOME (Time.+(Time.now(),delay))
	    fun stopTimer () : unit = timer := NONE
	    fun timeout () : bool =
		(case !timer
		   of NONE => false
		    | SOME t' => Time.>(Time.now(),t'))
	    fun loop () =
		if Comm.hasMsg from then ()
		else
		    if Comm.hasMsg to then
			let val _ =
				if timeout() then
				    (msg "[waiting for work]\n";
				     stopTimer())
				else ()
			    val _ = Platform.sleep 1.0
			in  loop()
			end
		    else ()
	in  fn () => (startTimer(); loop())
	end

    (*
	It's okay for the first acknowledgement to be missed.  In
	fact, we skip the acknowledgement if the expected compilation
	time is small to avoid communication traffic.
    *)
    fun compile (state : state, job : Comm.job, plan : Update.plan) : unit =
	let val _ = Name.reset_varmap()
	    val out_channel = #3 state
	    val start = Time.now()
	    val finish = Update.compile plan
	    val diff = Time.-(Time.now(), start)
	    val slow = Time.toReal diff > 0.5
	    val _ = if slow andalso Update.ackInterface plan then
			Comm.send (Comm.ACK_INTERFACE job) out_channel
		    else ()
	    val plan' = finish()
	    val _ = if (!TimeEachFile)
			then (Stats.print_timers();
			      Stats.clear_stats())
		    else ()
	    val _ = Comm.send (Comm.ACK_DONE (job, plan')) out_channel
	in  ()
	end

    fun setup () : state =
	let val identity = Comm.slave ()
	    val _ = Update.flushAll()
	    val in_channel = Comm.openIn (Comm.fromMaster identity)
	    val out_channel = Comm.openOut (Comm.toMaster identity)
	in  (identity, in_channel, out_channel)
	end

    fun step (state as (identity, in_channel, out_channel) : state) : unit =
	(case Comm.receive in_channel of
	     NONE => if (Comm.hasMsg (Comm.toMaster identity))
			 then ()
		     else Comm.send Comm.READY out_channel
	   | SOME Comm.READY => error "Slave got a ready message"
	   | SOME (Comm.ACK_INTERFACE _) => error "Slave got an ack_interface message"
	   | SOME (Comm.ACK_DONE _) => error "Slave got an ack_done message"
	   | SOME (Comm.ACK_ERROR _) => error "Slave got an ack_error message"
	   | SOME (Comm.FLUSH_ALL (platform, flags)) =>
			 (Update.flushAll();
			  Target.setTargetPlatform platform;
			  Comm.doFlags flags)
	   | SOME (Comm.FLUSH ((_,id),plan)) => Update.flush plan
	   | SOME (Comm.REQUEST (job, plan)) =>
		   compile (state,job,plan) handle e =>
		   if !ExnHandler.Interactive then raise e
		   else
			(Comm.send (Comm.ACK_ERROR (job, ExnHandler.errorMsg e)) out_channel;
			 Comm.send Comm.READY out_channel))

    fun complete ((identity, in_channel, out_channel) : state) : unit =
	(Comm.closeIn in_channel;
	 Comm.closeOut out_channel)

    fun slave () = {setup = fn () => setup (),
		    step = fn state => (step state; state),
		    complete = complete}

    fun run () : 'a =
	let val state = setup()
	    val wait = wait state
	    fun loop () = (step state; wait(); loop())
	in  loop()
	end

end
