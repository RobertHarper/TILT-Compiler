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
    |	COMPILE of  (* flags indicate what interfaces are up to date *)
	    {pinterface:bool, tali:bool}
    |	ASSEMBLE

    fun show_plan (plan:plan) : unit =
	if !ShowPlan then
	    (case plan of
		EMPTY => ()
	    |	COMPILE {pinterface=true, tali=true} =>
		    msg "  plan: generate\n"
	    |	COMPILE _ =>
		    msg "  plan: compile\n"
	    |	ASSEMBLE => msg "  plan: assemble\n")
	else ()

    datatype status =
	OK
     |	Stale of string
     |	Join of status list

    fun show_status' (status : status) : unit =
	(case status of
	    OK => ()
	|   Stale s => msg ("  stale: " ^ s ^ "\n")
	|   Join ss => app show_status' ss)

    fun show_status (status : status) : unit =
	if !ShowStatus then show_status' status
	else ()

    fun status_uptodate (status : status) : bool =
	(case status of
	    OK => true
	|   Stale _ => false
	|   Join ss => List.all status_uptodate ss)

    (*
	Force recompilation if the interface of a unit in context
	changes or if a unit is added or dropped from context.
    *)
    fun context_status (current:I.ue, past:I.ue) : status =
	let fun add ((U,c),m) = Map.insert (m,U,c)
	    val past = foldl add Map.empty past
	    fun bad (U,crc) : string option =
		(case (Map.find (past, U)) of
		    SOME crc' =>
			if crc = crc' then NONE
			else
			    SOME ("interface of " ^ Name.label2longname U ^
				" has changed")
		    | NONE =>
			SOME (Name.label2longname U ^
			    " no longer in context"))
	    val badunits = List.mapPartial bad current
	in  Join(map Stale badunits)
	end

    (*
	Force recompilation if units are added or taken away or the
	order of units changes.
    *)
    fun opened_status (current:I.units, past:I.units) : status =
	let val mismatch = Stale "list of opened units has changed"
	    fun loop (c:I.units,p:I.units) : status =
		(case (c,p) of
		    (nil,nil) => OK
		|   (u::c,u'::p) =>
			if Name.eq_label(u,u') then loop(c,p)
			else mismatch
		|   (_::_,nil) => mismatch
		|   (nil,_::_) => mismatch)
	in  loop(current,past)
	end

    (*
	Force recompilation if the source code has changed.
    *)
    fun src_file_status (current:Crc.crc, past:Crc.crc) : status =
	if current = past then OK
	else Stale "source has changed"

    type src = I.units * Crc.crc

    fun src_status ((opened,crc):src, (opened',crc'):src) : status =
	Join[opened_status(opened,opened'),
	    src_file_status(crc,crc')]

    fun src_option_status (current:src option, past:src option) : status =
	(case (current,past) of
	    (NONE,NONE) => OK
	|   (SOME src, SOME src') => src_status(src,src')
	|   (NONE,SOME _) => Stale "source has been added"
	|   (SOME _,NONE) => OK)

    (*
	Force recompilation if an ascribed interface has changed.
    *)
    fun interface_status (current:Crc.crc option, past:Crc.crc option) : status =
	(case (current,past) of
	    (SOME current, SOME past) =>
		if current = past then OK
		else Stale "ascribed interface has changed"
	|   (SOME _, NONE) => Stale "ascribed interface has been removed"
	|   (NONE, SOME _) => Stale "ascribed interface has been added"
	|   (NONE, NONE) => OK)

    (*
	Force recompilation if the type of definition has changed or
	if any of the compiler inputs have changed.
    *)
    fun info_status (current:I.info, past:I.info) : status =
	(case (current,past) of
	    (I.INFO_I {ue,src}, I.INFO_I {ue=ue', src=src'}) =>
		Join[context_status(ue,ue'),
		    src_option_status(src,src')]
	|   (I.INFO_U {ue,src,pinterface},
	     I.INFO_U {ue=ue',src=src',pinterface=pinterface'}) =>
		Join[context_status(ue,ue'),
		     src_option_status(src,src'),
		     interface_status(pinterface,pinterface')]
	|   _ => Stale "definition has changed")

    fun auto_status (pdec:I.pdec) : status option =
	(case pdec of
	    I.IDEC {iexp=I.PRIMI _,...} => SOME(Stale "primitive interface")
	|   I.IDEC {iexp=I.COMPI _,...} => SOME OK
	|   I.SCDEC {stable,...} =>
		if stable orelse not(Target.tal()) then SOME OK else NONE
	|   I.UDEC {uexp=I.PRIMU _,...} => SOME(Stale "primitive unit")
	|   I.UDEC {uexp=I.COMPU _,...} => SOME OK
	|   _ => NONE)

    fun status (desc:I.desc, pdec:I.pdec) : status =
	(case (auto_status pdec) of
	    SOME status => status
	|   NONE =>
		let val infoFile = I.P.D.info pdec
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
	    let val {iexp,...} = I.D.D.i pdec
		val pinterface = I.P.I.pinterface iexp
	    in	if Fs.exists pinterface then
		    EMPTY
		else
		    COMPILE {pinterface=false,tali=true}
	    end
	else COMPILE {pinterface=false,tali=true}

    fun plan_sc (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	let val {tali,...} = I.D.D.sc pdec
	    val uptoElaborate =
		!C.UptoElaborate orelse
		(!C.UptoAsm andalso not (!C.KeepAsm))
	    val tal = Target.tal()
	    val remove : I.file -> unit =
		if uptodate then fn _ => ()
		else fn path => Fs.remove path
	    val _ =
		if uptoElaborate then
		    remove tali
		else ()
	    val have : I.file -> bool = Fs.exists

	    fun select (l:(bool * plan) list) : plan =
		(case l of
		    nil => EMPTY
		|   (true,plan) :: _ => plan
		|   _ :: r => select r)

	    val rules =
		[(not tal orelse
		  uptoElaborate, EMPTY),

		 (* tali needed and missing *)
		 (not (have tali),
		  COMPILE {pinterface=true,tali=false})]
	in  select rules
	end

    fun plan_unit (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	let val {name=U,uexp,...} = I.D.D.u pdec
	    val uptoAsm = !C.UptoAsm
	    val keepAsm = !C.KeepAsm
	    val compressAsm = !C.CompressAsm
	    val uptoElaborate =
		!C.UptoElaborate orelse
		(uptoAsm andalso not keepAsm)
	    val tal = Target.tal()
	    val remove : (I.uexp -> string) -> unit =
		if uptodate then fn _ => ()
		else fn path => Fs.remove (path uexp)
	    val _ =
		if uptoElaborate orelse uptoAsm then
		    remove I.P.U.obj
		else ()
	    val _ =
		if uptoElaborate then
		    app remove [I.P.U.asm, I.P.U.asmz, I.P.U.using_file, I.P.U.tali]
		else ()

	    fun have (path : I.uexp -> string) : bool =
		Fs.exists (path uexp)

	    fun select (l:(bool * plan) list) : plan =
		(case l of
		    nil => EMPTY
		|   (true,plan) :: _ => plan
		|   _ :: r => select r)

	    val ascribed = isSome (I.P.U.asc' uexp)
	    val rules =
		[(not uptodate andalso ascribed,
		  COMPILE {pinterface=true, tali=not tal}),

		 (not uptodate,
		  COMPILE {pinterface=false, tali=not tal}),

		 (* inferred interface file is missing *)
		 (not (ascribed orelse have I.P.U.pinterface),
		  COMPILE {pinterface=false, tali=not tal}),

		 (uptoElaborate, EMPTY),

		 (* tali needed and missing *)
		 (tal andalso not (have I.P.U.tali),
		  COMPILE {pinterface=true, tali=false}),

		 (* asm/asmz needed and both are missing *)
		 (not (have I.P.U.asm orelse have I.P.U.asmz) andalso
		  (keepAsm orelse
		   not (uptoAsm orelse have I.P.U.obj)),
		 COMPILE {pinterface=true, tali=true}),

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
	let val l = I.P.D.name pdec
	    val pos = I.P.D.pos pdec
	    val msg = concat[Name.label2longname l, " (", Pos.tostring pos, ") ", msg]
	in  reject msg
	end

    fun check_interface (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	if uptodate then
	    let val iexp = I.P.D.iexp pdec
		val pinterface = I.P.I.pinterface iexp
	    in	if Fs.exists pinterface then EMPTY
		else fail (pdec, "missing compiled file: " ^ pinterface)
	    end
	else fail (pdec, "no longer up to date")

    fun check_sc (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	if Target.tal() then
	    if uptodate then
		let val {tali,...} = I.D.D.sc pdec
		in  if Fs.exists tali then EMPTY
		    else fail (pdec, "missing TAL interface: " ^ tali)
		end
	    else fail (pdec, "no longer up to date")
	else EMPTY

    fun check_unit (desc:I.desc, pdec:I.pdec, uptodate:bool) : plan =
	if uptodate then
	    let val uexp = I.P.D.uexp pdec
		val obj = I.P.U.obj uexp
	    in	if Fs.exists obj then EMPTY
		else fail (pdec, "missing compiled file: " ^ obj)
	    end
	else fail (pdec, "no longer up to date")

    fun plan_pdec (pdec:I.pdec) : I.desc * I.pdec * bool -> plan =
	(case pdec of
	    I.IDEC {iexp,...} =>
		if I.P.I.stable iexp then check_interface else plan_interface
	|   I.SCDEC {stable,...} =>
		if stable then check_sc else plan_sc
	|   I.UDEC {uexp,...} =>
		if I.P.U.stable uexp then check_unit else plan_unit)

    fun plan (desc:I.desc, pdec:I.pdec) : plan =
	let val l = I.P.D.name pdec
	    val _ = msg ("[checking " ^ Name.label2longname l ^ "]\n")
	    val uptodate = uptodate(desc,pdec)
	    val arg = (desc,pdec,uptodate)
	    val plan = plan_pdec pdec arg
	    val _ = show_plan plan
	in  plan
	end

    val plan = Stats.timer("planning cut-offs", plan)
end
