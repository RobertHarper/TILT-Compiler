structure Update :> UPDATE =
struct

    structure I = IntSyn
    structure C = Compiler
    structure S = I.S
    structure D = S.D
    structure P = S.P
    structure Map = Name.LabelMap

    type label = I.label

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
	Speed up lookup from a project description summary.  Also
	checks that the summary description is well-formed, but this
	is not essential due to the the magic number on the file and
	the manager invariants.
    *)
    structure Desc :>
    sig
	type desc
	val desc : D.desc -> desc
	val bound_interface : desc * label -> bool
	val bound_unit : desc * label -> bool
	val lookup_interface : desc * label -> D.iexp
	val lookup_unit : desc * label -> label
    end =
    struct

	fun malformed (what:label, msg:string) : 'a =
	    error ("malformed summary: " ^
		Name.label2longname what ^ ": " ^ msg ^ "\n")

	type desc = D.pdec Map.map

	fun lookup_interface' (desc:desc, I:label) : D.iexp option =
	    (case (Map.find(desc,I)) of
		SOME (D.IDEC(_,iexp)) => SOME iexp
	    |	SOME _ => malformed(I,"bound to unit")
	    |	NONE => NONE)

	fun lookup_interface (desc:desc,I:label) : D.iexp =
	    (case (lookup_interface'(desc,I)) of
		SOME r => r
	    |	NONE => malformed(I,"not bound"))

	fun lookup_unit' (desc:desc, U:label) : label option =
	    (case (Map.find(desc,U)) of
		SOME (D.SCDEC(_,I)) => SOME I
	    |	SOME _ => malformed(U,"bound to interface")
	    |	NONE => NONE)

	fun lookup_unit (desc:desc,U:label) : label =
	    (case (lookup_unit'(desc,U)) of
		SOME r => r
	    |	NONE => malformed(U,"not bound"))

	val bound_interface : desc * label -> bool =
	    isSome o lookup_interface'

	val bound_unit : desc * label -> bool =
	    isSome o lookup_unit'

	val bound : desc * label -> bool =
	    isSome o Map.find

	fun iexp_ok (desc:desc,iexp:D.iexp) : unit =
	    (case iexp of
		D.PRECOMPI (units,_) =>
		    List.app (fn U => ignore(lookup_unit(desc,U))) units
	    |	D.COMPI _ => ())

	fun name_ok (desc:desc, l:label) : unit =
	    if not(bound(desc,l)) then ()
	    else malformed(l,"already bound")

	fun pdec_ok (desc:desc, pdec:D.pdec) : unit =
	    (case pdec of
		D.IDEC (I,iexp) =>
		    let val _ = name_ok(desc,I)
			val _ = iexp_ok(desc,iexp)
		    in	()
		    end
	    |	D.SCDEC(U,I) =>
		    let val _ = name_ok(desc,U)
			val _ = ignore(lookup_interface(desc,I))
		    in	()
		    end)

	fun name (pdec:D.pdec) : label =
	    (case pdec of
		D.IDEC (I,_) => I
	    |	D.SCDEC (U,_) => U)

	val empty : desc = Map.empty

	fun add (pdec:D.pdec, desc:desc) : desc =
	    let val _ = pdec_ok(desc,pdec)
	    in	Map.insert(desc,name pdec,pdec)
	    end

	val desc : D.desc -> desc =
	    foldl add empty

    end

    (*
	Compare interfaces.  For compiled interfaces, this is a poor
	approximation but it is sufficient if the compiler is
	deterministic.
    *)
    fun eq_iexp (iexp:D.iexp, iexp':D.iexp) : bool =
	(case (iexp,iexp') of
	    (D.PRECOMPI (opened,src),
	     D.PRECOMPI (opened',src')) =>
		Listops.eq_list(Name.eq_label,opened,opened') andalso
		src = src'
	|   (D.COMPI pinterface,
	     D.COMPI pinterface') => pinterface = pinterface'
	|   _ => false)


    (*
	Force recompilation if there exists a unit U declared in both
	the current and previous project descriptions such that the
	old and new interfaces for U are not equivalent.  We do not
	warn about units that are in one project description and not
	the other.  Such units imply that either an interface has
	changed or the project declaration that is being compiled has
	changed.  We check for those conditions and the resulting
	warnings are friendlier than a list of missing/adding units.
    *)
    fun desc_status (current:D.desc, current':Desc.desc, past:Desc.desc) : status =
	let fun check_unit (U:label, iexp:D.iexp) : status =
		let val I = Desc.lookup_unit(past,U)
		    val iexp' = Desc.lookup_interface(past,I)
		in  if eq_iexp(iexp,iexp') then
			OK
		    else
			Stale ("interface of " ^
			    Name.label2longname U ^
			    " has changed")
		end
	    fun check_pdec (pdec:D.pdec) : status =
		(case pdec of
		    D.IDEC _ => OK
		|   D.SCDEC (U,I) =>
			if Desc.bound_unit (past,U) then
			    let val iexp = Desc.lookup_interface(current',I)
			    in	check_unit(U,iexp)
			    end
			else
			    OK)
	in  Join (map check_pdec current)
	end

    fun pdec_status (desc:Desc.desc, desc':Desc.desc, pdec:P.pdec, pdec':P.pdec) : status =
	let fun check_opened (opened:I.units, opened':I.units) : status =
		if Listops.eq_list(Name.eq_label,opened,opened') then OK
		else Stale "list of opened units has changed"
	    fun check_src' (src:Crc.crc, src':Crc.crc) : status =
		if src = src' then OK
		else Stale "source has changed"
	    fun check_src ((opened,src),(opened',src')) : status =
		Join [check_opened(opened,opened'),
		    check_src'(src,src')]
	    fun check_asc (I:label, I':label) : status =
		let val iexp = Desc.lookup_interface(desc,I)
		    val iexp' = Desc.lookup_interface(desc',I')
		in  if eq_iexp(iexp,iexp') then OK
		    else Stale "ascribed interface has changed"
		end
	    fun check_name (U:label, U':label) =
		if Name.eq_label(U,U') then OK
		else Stale "unit name has changed"
	    fun check_iexp (iexp:P.iexp, iexp':P.iexp) : status =
		(case (iexp,iexp') of
		    (P.SRCI src, P.SRCI src') => check_src (src,src'))
	    fun check_uexp (uexp:P.uexp, uexp':P.uexp) : status =
		(case (uexp,uexp') of
		    (P.SRCU src, P.SRCU src') => check_src(src,src')
		|   (P.SSRCU (I,opened,src), P.SSRCU (I',opened',src')) =>
			Join [check_asc(I,I'),
			    check_src((opened,src),(opened',src'))]
		|   (P.SRCU src, P.SSRCU (_,opened',src')) =>
			Join [Stale "interface ascription has been removed",
			    check_src(src,(opened',src'))]
		|   (P.SSRCU (_,opened,src), P.SRCU src') =>
			Join [Stale "interface ascription has been added",
			    check_src((opened,src),src')])
	in  (case (pdec,pdec') of
		(P.IDEC(_,iexp), P.IDEC(_,iexp')) => check_iexp(iexp,iexp')
	    |	(P.SCDEC(U,I), P.SCDEC(U',I')) =>
		    Join [check_name(U,U'),
			check_asc(I,I')]
	    |	(P.UDEC(U,uexp),P.UDEC(U',uexp')) =>
		    Join [check_name(U,U'),
			check_uexp(uexp,uexp')]
	    |	(P.SCDEC(U,I), P.UDEC(U',P.SSRCU(I',_,_))) =>
		    Join [check_name(U,U'),
			check_asc(I,I')]
	    |	(P.UDEC(U,P.SSRCU(I,_,_)), P.SCDEC(U',I')) =>
		    Join [Stale "implementation has been added",
			check_name(U,U'),
			check_asc(I,I')]
	    |	_ => Stale "definition has changed")
	end

    fun summary_status (current:S.summary, past:S.summary) : status =
	let val (desc,pdec) = current
	    val (desc',pdec') = past
	    val d = Desc.desc desc
	    val d' = Desc.desc desc'
	in  Join [desc_status(desc,d,d'),
		pdec_status(d,d',pdec,pdec')]
	end

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
		let val info = I.P.D.info pdec
		in  if !Cutoff andalso Fs.exists info then
			let val past = Fs.read S.blastInSummary info
			    val current = C.summarize (desc,pdec)
			in  summary_status (current, past)
			end handle
			    Blaster.BadMagicNumber _ =>
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
		    app remove [I.P.U.obj, I.P.U.tobj]
		else ()
	    val _ =
		if uptoElaborate then
		    app remove [I.P.U.asm, I.P.U.asmi, I.P.U.asme, I.P.U.asmz, I.P.U.using_file, I.P.U.tali]
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

		 (* asme needed and missing *)
		 (tal andalso not (have I.P.U.asme),
		  COMPILE {pinterface=true, tali=true}),

		 (* asmi needed and missing *)
		 (tal andalso not (have I.P.U.asmi),
		  COMPILE {pinterface=true, tali=true}),

		 (* asm/asmz needed and both are missing *)
		 (not (have I.P.U.asm orelse have I.P.U.asmz) andalso
		  (keepAsm orelse
		   not (uptoAsm orelse have I.P.U.obj)),
		 COMPILE {pinterface=true, tali=true}),

		 (* obj file is missing *)
		 (not (uptoAsm orelse have I.P.U.obj),
		  ASSEMBLE),

		 (* tobj file is missing *)
		 (tal andalso not (uptoAsm orelse have I.P.U.tobj),
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
		val tobj = I.P.U.tobj uexp
	    in	
	      if Fs.exists obj then 
		if not(Target.tal()) orelse Fs.exists tobj then EMPTY
		else fail (pdec, "missing compiled file: " ^ tobj)
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
