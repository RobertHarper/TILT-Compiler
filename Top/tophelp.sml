(*$import Time String Util Stats TextIO Date Compiler *)

signature HELP = 
    sig
	val ui2base : string -> string
	val base2int : string -> string
	val base2o : string -> string
	val base2s : string -> string
	val base2sml : string -> string
	val base2ui : string -> string
	val base2uo : string -> string
	val chat : string -> unit
	val chat_ref : bool ref
	val chat_verbose : bool ref
	val chat_strings : int -> string list -> int
	    
	val startTime : string -> unit
	val showTime : bool * string -> unit  (* if false, show only elapsed time since StartTime *)
	val reshowTimes : unit -> unit
    end


structure Help :> HELP = 
struct
    val error = fn s => Util.error "manager.sml" s
    val eager = ref true

    (* ---- Some diagnostic message helper functions ---- *)
    val chat_ref = Stats.tt("ManagerChat")
    val chat_verbose = Stats.ff("ManagerVerbose")
    fun chat s = if !chat_ref then (print s; TextIO.flushOut TextIO.stdOut)
		 else ()
    fun chat_strings skip imports =
	let fun f(str,acc) = 
	    let val cur = 2 + size str
	    in  if (acc + cur > 120)
		    then (chat "\n        "; chat str; 6 + cur)
		else (chat "  "; chat str; acc + cur)
	    end
	in  if (!chat_ref) then foldl f skip imports else 0
	end

    type unitname = string
    type filebase = string
    fun base2sml (f : string) = f ^ ".sml"
    fun base2int (f : string) = f ^ ".int"
    val ui2base = Til.ui2base
    val base2ui = Til.base2ui
    val base2s = Til.base2s
    val base2o = Til.base2o
    val base2uo = Til.base2uo

    val start = ref (NONE : Time.time option)
    val msgs = ref ([] : string list)
    fun showTime (printTime,str) = 
	let val cur = Time.now()
	    val curString = if (printTime)
				then let 
					 val temp = (Date.fromTimeLocal cur)
					 val res = Date.toString temp
				     in  res
				     end
			    else ""
	    val diff = Time.-(cur, (case !start of
					NONE => error "no start time"
				      | SOME t => t))
	    val diff = Time.toReal diff
	    val diff = (Real.realFloor(diff * 100.0)) / 100.0
	    (* XXX should move this and the space-making function into util *)
	    fun loop i = if (i<0) then "" else " " ^ (loop (i-1))
	    val padding = loop (30 - size str)
	    val msg = (str ^ padding ^ ": " ^ curString ^ "   " ^ 
		       (Real.toString diff) ^ " sec\n")
	in  msgs := msg :: (!msgs); 
	    if (!chat_verbose) then chat msg else ()
	end
    fun startTime str = (msgs := []; 
			 start := SOME(Time.now()); 
			 showTime (true,str))
    fun reshowTimes() = (chat "\n\n"; app chat (rev (!msgs)); msgs := []; start := NONE)
end


