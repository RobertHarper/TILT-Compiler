(*$import SLAVE Update TopHelp Communication OS List Platform Target Paths *)

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
	
    structure Comm = Comm(val slaveTidOpt = NONE)

    datatype result = WORK of string | WAIT | READY
	    
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
    
    fun setup () = (Help.chat "Starting slave.\n";
		    Update.flushAll())

    fun step () = 
	(case Comm.receive Comm.fromMaster of
	     NONE => if (Comm.exists Comm.toMaster)
			 then WAIT
		     else (Comm.send (Comm.toMaster, Comm.READY); READY)
	   | SOME Comm.READY => error "Slave got a ready message"
	   | SOME (Comm.ACK_INTERFACE _) => error "Slave got an ack_interface message"
	   | SOME (Comm.ACK_DONE _) => error "Slave got an ack_done message"
	   | SOME (Comm.ACK_ERROR _) => error "Slave got an ack_error message"
	   | SOME (Comm.FLUSH (platform, flags)) =>
			 (Help.chat "Slave received FLUSH.\n";
			  Update.flushAll();
			  Target.setTargetPlatform platform;
			  Comm.doFlags flags;
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
					    (Comm.send(Comm.toMaster, Comm.ACK_ERROR unit);
					     raise e))
			   val diff = Time.-(Time.now(), start)
			   val _ =
			       if ack_interface andalso (Time.toReal diff > 0.5) then
				   (if (!Help.chatVerbose)
					then (Help.chat "Sending ACK_INTERFACE: interface took ";
					      Help.chat (Time.toString diff);
					      Help.chat " seconds \n")
				    else ();
				    Comm.send (Comm.toMaster, Comm.ACK_INTERFACE unit))
			       else ()
			   val plan' = (generate state
					handle e =>
					    (Comm.send(Comm.toMaster, Comm.ACK_ERROR unit);
					     raise e))
			   val _ = if (!Help.chat_ref andalso !Help.statEachFile)
				       then (Stats.print_timers();
					     Stats.clear_stats())
				   else ()
			   val _ = Comm.send(Comm.toMaster, Comm.ACK_DONE (unit, plan'))
		       in  WORK unit
		       end)

    fun run() = 
	let val _ = setup()
	    val lastState = ref READY
	    val lastTime = ref (Time.now())
	    fun loop() = 
		let val prevState = !lastState
		    val curState = step()
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
			     in	 Platform.sleep 0.1
			     end);
		    loop()
		end
	in loop()
	end

end

