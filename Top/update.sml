structure Update :> UPDATE =
struct

    structure I = IntSyn
    structure C = Compiler
    structure Map = Name.LabelMap

    val error = fn s => Util.error "update.sml" s
    val reject = Util.reject

    val UpdateDebug = Stats.ff "UpdateDebug"
    fun debugdo f = if !UpdateDebug then (f(); ()) else ()

    val UpdateDiag = Stats.ff("UpdateDiag")
    fun msg str = if (!UpdateDiag) then print str else ()

    val ShowPlan = Stats.ff "ShowPlan"
    val ShowStatus = Stats.ff "ShowStatus"
    val Cutoff = Stats.tt "Cutoff"

    datatype plan =
	EMPTY
      | COMPILE
      | GENERATE		(* compile, but pinterface up to date *)
      | ASSEMBLE

    fun show_plan (plan:plan) : unit =
	if !ShowPlan then
	    (case plan
	       of EMPTY => ()
		| COMPILE => msg "  plan: compile\n"
		| GENERATE => msg "  plan: generate\n"
		| ASSEMBLE => msg "  plan: assemble\n")
	else ()

    datatype status =
	OK
      | Stale of string
      | Join of status list

    fun show_status' (status : status) : unit =
	(case status
	   of OK => ()
	    | Stale s => msg ("  stale: " ^ s ^ "\n")
	    | Join ss => app show_status' ss)

    fun show_status (status : status) : unit =
	if !ShowStatus then show_status' status
	else ()

    fun status_uptodate (status : status) : bool =
	(case status
	   of OK => true
	    | Stale _ => false
	    | Join ss => List.all status_uptodate ss)

    (*
	Force recompilation if a unit's interface changes or a unit is
	dropped.
    *)
    fun context_status (current:I.ue, past:I.ue) : status =
	let fun add ((U,c),m) = Map.insert (m,U,c)
	    val past = foldl add Map.empty past
	    fun bad (U,crc) : string option =
		(case Map.find (past, U)
		   of SOME crc' =>
			if crc = crc' then NONE
			else SOME ("interface of " ^ Name.label2longname U ^
				   " has changed")
		    | NONE => SOME (Name.label2longname U ^
				    " no longer in context"))
	    val badunits = List.mapPartial bad current
	in  Join(map Stale badunits)
	end

    (*
	Force recompilation if units are added or taken away or the
	order of units changes.
    *)
    fun opened_status (current:I.opened, past:I.opened) : status =
	let val mismatch = Stale "list of opened units has changed"
	    fun loop (c:I.opened,p:I.opened) : status =
		(case (c,p)
		   of (nil,nil) => OK
		    | (u::c,u'::p) =>
			if Name.eq_label(u,u') then loop(c,p)
			else mismatch
		    | (_::_,nil) => mismatch
		    | (nil,_::_) => mismatch)
	in  loop(current,past)
	end

    (*
	Force recompilation if the source code has changed.
    *)
    fun source_status (current:Crc.crc, past:Crc.crc) : status =
	if current = past then OK
	else Stale "source has changed"


    (*
	Force recompilation if an ascribed interface has changed.
    *)
    fun interface_status (current:Crc.crc, past:Crc.crc) : status =
	if current = past then OK
	else Stale "ascribed interface has changed"

    (*
	Force recompilation if the type of definition has changed or
	if any of the compiler inputs have changed.
    *)
    fun info_status (current:I.info, past:I.info) : status =
	(case (current,past)
	   of (I.INFO_SRCI (ctx,opened,src), I.INFO_SRCI (ctx',opened',src')) =>
		Join[context_status(ctx,ctx'),
		     opened_status(opened,opened'),
		     source_status(src,src')]
	    | (I.INFO_SRCU (ctx,opened,src), I.INFO_SRCU (ctx',opened',src')) =>
		Join[context_status(ctx,ctx'),
		     opened_status(opened,opened'),
		     source_status(src,src')]
	    | (I.INFO_SSRCU (ctx,opened,src,I),
	       I.INFO_SSRCU (ctx',opened',src',I')) =>
		Join[context_status(ctx,ctx'),
		     opened_status(opened,opened'),
		     source_status(src,src'),
		     interface_status(I,I')]
	    | (I.INFO_PRIMU I, I.INFO_PRIMU I') => interface_status(I,I')
	    | _ => Stale "definition has changed")

    fun status (desc:I.desc, pdec:I.pdec) : status =
	(case pdec
	   of I.IDEC(_,I.PRIMI _,_) => Stale "primitive interface"
	    | I.IDEC(_,I.COMPI _,_) => OK
	    | I.SCDEC _ => OK
	    | I.UDEC(_,I.PRIMU _,_) => Stale "primitive unit"
	    | I.UDEC(_,I.COMPU _,_) => OK
	    | _ =>
		let val infoFile = I.P.info pdec
		in  if !Cutoff andalso Fs.exists infoFile then
			let val past = Fs.read I.blastInInfo infoFile
			    val current = C.info (desc,pdec)
			in  info_status (current, past)
			end handle Blaster.BadMagicNumber _ =>
			    Stale "bad magic number"
		    else
			Stale "first compile"
		end)

    fun uptodate (desc:I.desc, pdec:I.pdec) : bool =
	let val status = status(desc,pdec)
	    val _ = show_status status
	in  status_uptodate status
	end

    fun plan_interface (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	if uptodate then
	    let val (_,iexp,_) = I.D.Pdec.idec pdec
		val pinterface = I.P.I.pinterface iexp
	    in	if Fs.exists pinterface then EMPTY else COMPILE
	    end
	else COMPILE

    fun plan_unit (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	let val (U,uexp,pos) = I.D.Pdec.udec pdec
	    val uptoAsm = !C.UptoAsm
	    val keepAsm = !C.KeepAsm
	    val compressAsm = !C.CompressAsm
	    val uptoElaborate =
		!C.UptoElaborate orelse
		(uptoAsm andalso not keepAsm)
	    val remove : (I.uexp -> string) -> unit =
		if uptodate then fn _ => ()
		else fn path => Fs.remove (path uexp)
	    val _ =
		if uptoElaborate orelse uptoAsm then
		    remove I.P.U.obj
		else ()
	    val _ =
		if uptoElaborate then
		    app remove [I.P.U.asm, I.P.U.asmz, I.P.U.parm]
		else ()

	    fun have (path : I.uexp -> string) : bool =
		Fs.exists (path uexp)

	    fun select (l:(bool * plan) list) : plan =
		(case l
		   of nil => EMPTY
		    | (true,plan) :: _ => plan
		    | _ :: r => select r)

	    val ascribed = isSome (I.P.U.asc' uexp)
	    val rules =
		[(not uptodate andalso ascribed, GENERATE),
		 (not uptodate, COMPILE),

		 (* inferred interface file is missing *)
		 (not (ascribed orelse have I.P.U.pinterface),
		  COMPILE),

		 (uptoElaborate, EMPTY),

		 (* asm/asmz needed and both are missing *)
		 (not (have I.P.U.asm orelse have I.P.U.asmz) andalso
		  (keepAsm orelse
		   not (uptoAsm orelse have I.P.U.obj)),
		 GENERATE),

		 (* obj file is missing *)
		 (not (uptoAsm orelse have I.P.U.obj),
		  ASSEMBLE),

		 (not keepAsm, EMPTY),

		 (* missing asm or spurious asmz *)
		 (not compressAsm andalso
		  (have I.P.U.asmz orelse
		   not (have I.P.U.asm)),
		  ASSEMBLE),

		 (* missing asmz or spurious asm *)
		 (compressAsm andalso
		  (have I.P.U.asm orelse
		   not (have I.P.U.asmz)),
		  ASSEMBLE)]
	in  select rules
	end

    fun fail (pdec:I.pdec, msg:string) : 'a =
	let val l = I.P.label pdec
	    val pos = I.P.pos pdec
	    val msg = concat[Name.label2longname l, " (",
			     Pos.tostring pos, ") ", msg]
	in  reject msg
	end

    fun check_interface (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	if uptodate then
	    let val (_,iexp,_) = I.D.Pdec.idec pdec
		val pinterface = I.P.I.pinterface iexp
	    in	if Fs.exists pinterface then EMPTY
		else fail (pdec, "missing compiled file: " ^ pinterface)
	    end
	else fail (pdec, "no longer up to date")

    fun check_unit (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	if uptodate then
	    let val (_,uexp,_) = I.D.Pdec.udec pdec
		val obj = I.P.U.obj uexp
	    in	if Fs.exists obj then EMPTY
		else fail (pdec, "missing compiled file: " ^ obj)
	    end
	else fail (pdec, "no longer up to date")

    fun plan' (desc:I.desc, pdec:I.pdec) : plan =
	let val l = I.P.label pdec
	    val _ = msg ("[checking " ^ Name.label2longname l ^ "]\n")
	    val uptodate = uptodate(desc,pdec)
	    val arg = (desc,pdec,uptodate)
	    val plan =
		(case pdec
		   of I.IDEC (_,iexp,_) =>
			(case iexp
			   of I.SRCI _ => plan_interface arg
			    | I.PRIMI _ => plan_interface arg
			    | I.PRECOMPI _ => check_interface arg
			    | I.COMPI _ => check_interface arg)
		    | I.SCDEC _ => EMPTY
		    | I.UDEC (_,uexp,_) =>
			(case uexp
			   of I.SRCU _ => plan_unit arg
			    | I.SSRCU _ => plan_unit arg
			    | I.PRIMU _ => plan_unit arg
			    | I.PRECOMPU _ => check_unit arg
			    | I.COMPU _ => check_unit arg))
	    val _ = show_plan plan
	in  plan
	end

    fun plan (desc:I.desc, pdec:I.pdec) : plan =
	(case pdec
	   of I.SCDEC _ => EMPTY
	    | _ => plan' (desc,pdec))

    val plan = Stats.timer("planning cut-offs", plan)
end
