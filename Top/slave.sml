(*$import Util Time Stats SLAVE Update TopHelp Communication OS List Platform Target Paths *)

(* This is a slave step which gives a result:

     Is there a message from the master?
     (A) Yes, fetch the job j from from the channel. Compile j.  Send acknowledgement. 
         This results in a WORK j.
     (B) No, Is there a message from me to the master?
         (1) Yes, do nothing.  Master has yet to consume this message. Result is WAIT.
         (2) No, send ready message.  Result is READY.

*)

structure Slave :> SLAVE =
struct
    val error = fn s => Util.error "slave.sml" s

    type state = Comm.identity * Comm.in_channel * Comm.out_channel
    datatype result = WORK of string | WAIT | READY
	    
    fun setup tid =
	let val identity = Comm.slave tid
	    val _ = Help.chat ("Starting slave " ^ Comm.name identity ^ ".\n")
	    val _ = Update.flushAll()
	    val in_channel = Comm.openIn (Comm.fromMaster identity)
	    val out_channel = Comm.openOut (Comm.toMaster identity)
	in  (identity, in_channel, out_channel)
	end

    fun uptoElaborate (Update.ELABORATE :: after, acc) = (rev (Update.ELABORATE :: acc), after)
      | uptoElaborate (todo :: rest, acc) = uptoElaborate (rest, todo :: acc)
      | uptoElaborate (nil, acc) = (nil, rev acc)

    fun uptoAssemble (Update.ASSEMBLE :: after, acc) = (rev acc, Update.ASSEMBLE :: after)
      | uptoAssemble (todo :: rest, acc) = uptoAssemble (rest, todo :: acc)
      | uptoAssemble (nil, acc) = (rev acc, nil)
	
    fun compile (target, imports, plan) =
	let
	    val (makeInterface, makeRest) = uptoElaborate (plan, nil)
	    val (makeRestSlave, makeRestMaster) =
		if Target.native() then (makeRest, nil)
		else uptoAssemble (makeRest, nil)
	    val ack_interface = not (null makeInterface) andalso not (null makeRestSlave)
	    fun elaborate () = foldl Update.execute (Update.init (target, imports)) makeInterface
	    fun generate state =
		(ignore (foldl Update.execute state makeRestSlave);
		 makeRestMaster)
	in
	    (elaborate, ack_interface, generate)
	end
    
    fun step (identity, in_channel, out_channel) =
	(case Comm.receive in_channel of
	     NONE => if (Comm.exists (Comm.toMaster identity))
			 then WAIT
		     else (Comm.send Comm.READY out_channel; READY)
	   | SOME Comm.READY => error "Slave got a ready message"
	   | SOME (Comm.ACK_INTERFACE _) => error "Slave got an ack_interface message"
	   | SOME (Comm.ACK_DONE _) => error "Slave got an ack_done message"
	   | SOME (Comm.ACK_ERROR _) => error "Slave got an ack_error message"
	   | SOME (Comm.FLUSH_ALL (platform, flags)) =>
			 (Help.chat "Slave received FLUSH_ALL.\n";
			  Update.flushAll();
			  Target.setTargetPlatform platform;
			  Comm.doFlags flags;
			  READY)
	   | SOME (Comm.FLUSH (arg as (target, _))) =>
			 (Help.chat ("Flushing " ^ Paths.unitName target ^ ".\n");
			  Update.flush arg;
			  READY)
	   | SOME (Comm.REQUEST (job as (target, imports, plan))) =>
		       (* It's okay for the first acknowledgement to be missed. 
			  In fact, we skip the acknowledgement if the expected compilation 
			  time was small to avoid communication traffic. *)
		       let val start = Time. now()
			   val unit = Paths.unitName target
			   val (elaborate, ack_interface, generate) = compile (target, imports, plan)
			   val state = (elaborate()
					handle e => 
					    (Comm.send (Comm.ACK_ERROR unit) out_channel;
					     raise e))
			   val diff = Time.-(Time.now(), start)
			   val _ =
			       if ack_interface andalso (Time.toReal diff > 0.5) then
				   (if (!Help.chatVerbose)
					then (Help.chat "Sending ACK_INTERFACE: interface took ";
					      Help.chat (Time.toString diff);
					      Help.chat " seconds \n")
				    else ();
				    Comm.send (Comm.ACK_INTERFACE unit) out_channel)
			       else ()
			   val plan' = (generate state
					handle e =>
					    (Comm.send (Comm.ACK_ERROR unit) out_channel;
					     raise e))
			   val _ = if (!Help.chat_ref andalso !Help.statEachFile)
				       then (Stats.print_timers();
					     Stats.clear_stats())
				   else ()
			   val _ = Comm.send (Comm.ACK_DONE (unit, plan')) out_channel
		       in  WORK unit
		       end)

    fun complete (identity, in_channel, out_channel) =
	(Comm.closeIn in_channel;
	 Comm.closeOut out_channel)
	 
    fun slave tid = {setup = fn () => setup tid,
		     step = fn state => (state, step state),
		     complete = complete}

    fun run() = 
	let val {setup,step,complete} = slave NONE
	    val lastState = ref READY
	    val lastTime = ref (Time.now())
	    fun loop commState = 
		let val prevState = !lastState
		    val (commState, curState) = step commState
		    val _ = lastState := curState
		in  (case curState of
			 WORK job => Help.chat ("Slave compiled " ^ job ^ "\n")
		       | _ => (* READY or WAIT *)
			     let val curTime = Time.now()
				 val diff = Time.toReal(Time.-(curTime, !lastTime))
				 val _ = if (diff > 10.0)
					     then (Help.chat ("Slave waiting for master to send work.\n");
						   lastTime := curTime)
					 else ()
			     in	 if curState = WAIT then Platform.sleep 0.1
				 else ()
			     end);
		    loop commState
		end
	in loop (setup())
	end

end

