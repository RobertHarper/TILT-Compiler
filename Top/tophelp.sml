(*$import Time String Util Stats TextIO Date ORD_MAP ORD_SET Real *)

signature HELP = 
    sig
	val chat_ref : bool ref			(* Chat? *)
	val chatVerbose : bool ref		(* Chat more? *)
	val uptoElaborate : bool ref		(* .il and .info files are generated *)
	val uptoPhasesplit : bool ref
	val uptoClosureConvert : bool ref
	val uptoRtl : bool ref
	val uptoAsm : bool ref			(* .s and .s.gz files are generated *)
						(* all false: .o and .exe files are generated *)
	val keepAsm : bool ref			(* Keep assembler files. *)
	val statEachFile : bool ref		(* Print and clear statistics after each file. *)
	val makeBackups : bool ref		(* Write foo.BACKUP before overwriting file foo. *)
	    
	val chat : string -> unit
	val chat_strings : int -> string list -> int

	val startTime : string -> unit
	val showTime : bool * string -> unit  (* if false, show only elapsed time since StartTime *)
	val reshowTimes : unit -> unit

	val wantAssembler : unit -> bool
	val wantBinaries : unit -> bool
    end


structure Help :> HELP = 
struct
    val error = fn s => Util.error "tophelp.sml" s

    val chat_ref           = Stats.tt "ManagerChat"
    val chatVerbose        = Stats.tt "ManagerVerbose"
    val uptoElaborate      = Stats.ff "UptoElaborate"
    val uptoPhasesplit     = Stats.ff "UptoPhasesplit"
    val uptoClosureConvert = Stats.ff "UptoClosureConvert"
    val uptoRtl            = Stats.ff "UptoRtl"
    val uptoAsm            = Stats.ff "UptoAsm"
    val keepAsm            = Stats.tt "keep_asm"
    val statEachFile       = Stats.ff "TimeEachFile"
    val makeBackups        = Stats.ff "makeBackups"
	
    (* ---- Some diagnostic message helper functions ---- *)
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
	    if (!chatVerbose) then chat msg else ()
	end
    fun startTime str = (msgs := []; 
			 start := SOME(Time.now()); 
			 showTime (true,str))
    fun reshowTimes() = (chat "\n\n"; app chat (rev (!msgs)); msgs := []; start := NONE)

    fun wantAssembler () = not (!uptoElaborate orelse
				!uptoPhasesplit orelse
				!uptoClosureConvert orelse
				!uptoRtl)

    fun wantBinaries () = not (!uptoElaborate orelse
			       !uptoPhasesplit orelse
			       !uptoClosureConvert orelse
			       !uptoRtl orelse
			       !uptoAsm)
end
