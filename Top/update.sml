(*
	Values of type Info.info are compared to determine when
	contexts, imports, or sources change.  These hold CRCs that
	are compared to notice changes.

	Parameterized interface file CRCs, together with an
	equivalence relation provided by the master, permit us to
	efficiently determine when interfaces change.

	Source file CRCs (compared with op=) permit us to determine
	when source files change.  CRCs are more robust than time
	stamps.  Some shared file systems get time stamps wrong
	because of clock skew.  Interface files may be older than the
	source files they describe.

	XXX: Packed libraries do not include sources yet.  The group
	file should include sources and the additional imports they
	drag in.  Info files should have a stable flag to prevent
	compilation.  When the flag is set, the planner should reject
	when targets are missing or extant targets are out of date.
	[I do not like the idea of putting sources into libraries. -dave]
*)
structure Update :> UPDATE =
struct

    val error = fn s => Util.error "update.sml" s
    val reject = fn s => raise Compiler.Reject s

    fun say (s : string) : unit =
	if !Blaster.BlastDebug then (print s; print "\n") else ()

    structure Ue = UnitEnvironment
    structure FileCache = Compiler.FileCache
    structure SM = Util.StringMap
    structure W = TilWord64

    val UpdateDebug = Stats.ff "UpdateDebug"
    fun debugdo f = if !UpdateDebug then (f(); ()) else ()

    val UpdateDiag = Stats.ff("UpdateDiag")
    fun msg str = if (!UpdateDiag) then print str else ()

    val ShowPlan = Stats.ff "ShowPlan"
    val ShowStale = Stats.ff "ShowStale"
    val KeepAsm = Stats.tt "KeepAsm"
    val CompressAsm = Stats.ff "CompressAsm"
    val UptoElaborate = Stats.ff "UptoElaborate"
    val UptoAsm = Stats.ff "UptoAsm"

    type iface = Compiler.iface
    type context = (string * Paths.iface) list
    type precontext = Compiler.precontext
    type imports = Compiler.imports
    type ue = UnitEnvironment.ue
    type equiv = Crc.crc * Crc.crc -> bool	(* interface CRC equivalence *)

    (*
	Some plans have a bit mask.
    *)
    val Elaborate =	W.fromInt 1
    val Generate =	W.fromInt 2
    val Uncompress =	W.fromInt 4
    val Assemble =	W.fromInt 8
    val Link =		W.fromInt 16
    val Compress =	W.fromInt 32
    val Remove =	W.fromInt 64

    fun test (flags : W.word, bits : W.word) : bool =
	W.nequal (W.andb(flags,bits), W.zero)

    datatype pack =
	PACKU of Paths.compunit * ExtSyn.id list
      | PACKI of Paths.iface * ExtSyn.id list
      | PACKV of ExtSyn.id * ExtSyn.exp

    type inputs = (Compiler.precontext * imports * iface option) option
    (*
	Invariants:

	If UPATE_SRCU (_,inputs,flags,_) : plan,
	then flags is non-zero
	and inputs is NONE iff Elaborate and Generate are clear
	and Elaborate is clear iff the unit's interface is up to date.

	If LINK (_,_,flags) : plan,
	then flags is non-zero.
    *)
    datatype plan =
	EMPTY_PLAN
      | ELAB_SRCI of Paths.iface * precontext * imports * Info.info
      | COMPILE of Paths.compunit * inputs * W.word * Info.info
      | CHECK of precontext * Paths.compunit * Paths.iface
      | LINK of Paths.exe * Paths.compunit list * W.word
      | PACK of Paths.lib * pack list

    val empty_plan : plan = EMPTY_PLAN

    fun isEmpty (plan : plan) : bool =
	(case plan
	   of EMPTY_PLAN => true
	    | _ => false)

    fun isReady (plan : plan) : bool =
	(case plan
	   of EMPTY_PLAN => true
	    | ELAB_SRCI _ => false
	    | COMPILE (_,_,flags,_) => not(test(flags,Elaborate))
	    | CHECK _ => false
	    | LINK _ => true
	    | PACK _ => true)

    fun sendToSlave (plan : plan) : bool =
	(case plan
	   of EMPTY_PLAN => false
	    | ELAB_SRCI _ => true
	    | COMPILE (_,_,flags,_) =>
		test(flags, W.orb(Elaborate,Generate))
	    | CHECK _ => true
	    | LINK _ => false
	    | PACK _ => false)

    (*
	We skip ACK_INTERFACE when there is nothing to do after
	elaboration.
    *)
    fun ackInterface (plan : plan) : bool =
	(case plan
	   of EMPTY_PLAN => false
	    | ELAB_SRCI _ => false
	    | COMPILE (_,_,flags,_) =>
		test(flags,Elaborate) andalso
		test(flags,W.notb Elaborate)
	    | CHECK _ => false
	    | LINK _ => false
	    | PACK _ => false)

    structure B = Blaster
    structure E = ExtSyn

    val blastOutStrings : B.outstream -> string list -> unit =
	B.blastOutList B.blastOutString
    val blastInStrings : B.instream -> string list =
	B.blastInList B.blastInString

    fun blastOutValue (os : B.outstream) (value : E.exp) : unit =
	(say "blastOutValue";
	 (case value
	    of E.EXP_STR s => (B.blastOutInt os 0; B.blastOutString os s)
	     | E.EXP_INT i => (B.blastOutInt os 1; B.blastOutInt os i)
	     | E.EXP_BOOL b => (B.blastOutInt os 2; B.blastOutBool os b)
	     | _ => error "blasting out bad value"))
    fun blastInValue (is : B.instream) : E.exp =
	(say "blastInValue";
	 (case B.blastInInt is
	    of 0 => E.EXP_STR (B.blastInString is)
	     | 1 => E.EXP_INT (B.blastInInt is)
	     | 2 => E.EXP_BOOL (B.blastInBool is)
	     | _ => error "bad value"))

    fun blastOutPack (os : B.outstream) (pack : pack) : unit =
	(say "blastOutPack";
	 (case pack
	    of PACKU (u,imports) =>
		 (B.blastOutInt os 0; Paths.blastOutUnit os u;
		  blastOutStrings os imports)
	     | PACKI (I,imports) =>
		 (B.blastOutInt os 1; Paths.blastOutIface os I;
		  blastOutStrings os imports)
	     | PACKV (id,value) =>
		 (B.blastOutInt os 2; B.blastOutString os id;
		  blastOutValue os value)))
    fun blastInPack (is : B.instream) : pack =
	(say "blastInPack";
	 (case B.blastInInt is
	    of 0 => PACKU (Paths.blastInUnit is, blastInStrings is)
	     | 1 => PACKI (Paths.blastInIface is, blastInStrings is)
	     | 2 => PACKV (B.blastInString is, blastInValue is)
	     | _ => error "bad pack"))

    fun blastOutContext (os : B.outstream) (c : precontext) : unit =
	(say "blastOutContext";
	 B.blastOutList (B.blastOutPair B.blastOutString B.blastOutString) os c)

    fun blastInContext (is : B.instream) : precontext =
	(say "blastInContext";
	 B.blastInList (B.blastInPair B.blastInString B.blastInString) is)

    fun blastOutImports (os : B.outstream) (I : imports) : unit =
	(say "blastOutImports"; B.blastOutList B.blastOutString os I)
    fun blastInImports (is : B.instream) : imports =
	(say "blastInImports"; B.blastInList B.blastInString is)

    fun blastOutInputs (os : B.outstream) (I : inputs) : unit =
	(say "blastOutInputs";
	 B.blastOutOption (B.blastOutTriple
			   blastOutContext blastOutImports
			   (B.blastOutOption B.blastOutString)) os I)
    fun blastInInputs (is : B.instream) : inputs =
	(say "blastInInputs";
	 B.blastInOption (B.blastInTriple
			  blastInContext blastInImports
			  (B.blastInOption B.blastInString)) is)

    fun blastOutPlan (os : B.outstream) (plan : plan) : unit =
	(say "blastOutPlan";
	 (case plan
	    of EMPTY_PLAN => B.blastOutInt os 0
	     | ELAB_SRCI (iface,context,imports,info) =>
		(B.blastOutInt os 1; Paths.blastOutIface os iface;
		 blastOutContext os context; blastOutImports os imports;
		 Info.blastOutInfo os info)
	     | COMPILE (unit,inputs,flags,info) =>
		(B.blastOutInt os 2; Paths.blastOutUnit os unit;
		 blastOutInputs os inputs; B.blastOutWord64 os flags;
		 Info.blastOutInfo os info)
	     | CHECK (context,U,I) =>
		(B.blastOutInt os 3; blastOutContext os context;
		 Paths.blastOutUnit os U; Paths.blastOutIface os I)
	     | LINK (exe,units,flags) =>
		(B.blastOutInt os 4; Paths.blastOutExe os exe;
		 B.blastOutList Paths.blastOutUnit os units;
		 B.blastOutWord64 os flags)
	     | PACK (lib, exports) =>
		(B.blastOutInt os 5; Paths.blastOutLib os lib;
		 B.blastOutList blastOutPack os exports)))
    fun blastInPlan (is : B.instream) : plan =
	(say "blastInPlan";
	 (case B.blastInInt is
	    of 0 => EMPTY_PLAN
	     | 1 => ELAB_SRCI (Paths.blastInIface is, blastInContext is,
			      blastInImports is, Info.blastInInfo is)
	     | 2 => COMPILE (Paths.blastInUnit is, blastInInputs is,
			     B.blastInWord64 is, Info.blastInInfo is)
	     | 3 => CHECK (blastInContext is, Paths.blastInUnit is,
			  Paths.blastInIface is)
	     | 4 => LINK (Paths.blastInExe is,
			 B.blastInList Paths.blastInUnit is,
			 B.blastInWord64 is)
	     | 5 => PACK (Paths.blastInLib is, B.blastInList blastInPack is)
	     | _ => error "bad plan"))

    val (blastOutPlan, blastInPlan) =
	B.magic (blastOutPlan, blastInPlan, "plan $Revision$")

    val elaborate = "elaborate"

    fun flagStrings (flags : W.word) : string list =
	let fun add ((bit, name), r) =
		if test(flags,bit) then name :: r else r
	in  foldr add nil
	    [(Elaborate,elaborate), (Generate,"generate"),
	     (Uncompress,"uncompress"), (Assemble,"assemble"),
	     (Link,"link"), (Compress,"compress"), (Remove,"remove")]
	end

    fun planStrings (plan : plan) : string list =
	(case plan
	   of EMPTY_PLAN => []
	    | ELAB_SRCI _ => [elaborate]
	    | COMPILE (_,_,flags,_) => flagStrings flags
	    | CHECK _ => ["check"]
	    | LINK (_,_,flags) => flagStrings flags
	    | PACK _ => ["pack"])

    fun showPlan (what : string, plan : plan) : unit =
	if isEmpty plan orelse not (!ShowPlan) then ()
	else
	    let val plan = concat(Listops.join " " (planStrings plan))
	    in  msg ("  Plan: " ^ plan ^ "\n")
	    end

    fun ifaceData (iface : Paths.iface) : Crc.crc * ue =
	let val crc = FileCache.crc (Paths.ifaceFile iface)
	    val ue = FileCache.read_ue (Paths.ifaceUeFile iface)
	in  (crc,ue)
	end

    fun unitEntry (unit : Paths.compunit) : Prelink.entry =
	let val name = Paths.unitName unit
	    val iface = ifaceData (Paths.unitIface unit)
	    val objue = FileCache.read_ue (Paths.ueFile unit)
	in  Prelink.UNIT {name=name, iface=iface, objue=objue}
	end

    fun importEntry (name : string, iface : Paths.iface) : Prelink.entry =
	let val iface = ifaceData iface
	in  Prelink.IMPORT {name=name, iface=iface}
	end

    fun ifaceEntry (iface : Paths.iface) : Prelink.entry =
	let val name = Paths.ifaceName iface
	    val ue = FileCache.read_ue (Paths.ifaceUeFile iface)
	in  Prelink.IFACE {name=name, ue=ue}
	end

    datatype status =
	OK
      | Bad of string
      | Join of status list

    fun uptodate (status : status) : bool =
	(case status
	   of OK => true
	    | Bad _ => false
	    | Join ss => List.all uptodate ss)

    fun showStale' (status : status) : unit =
	(case status
	   of OK => ()
	    | Bad s => msg ("  Stale: " ^ s ^ "\n")
	    | Join ss => app showStale' ss)

    fun showStale (status : status) : unit =
	if !ShowStale then showStale' status
	else ()

    fun check_ue (what : string, equiv, ue, uefile : string) : unit =
	let val _ = msg ("[checking " ^ what ^ "]\n")
	    val ue' = FileCache.read_ue uefile
	in  (case Ue.confine equiv (ue',ue)
	       of Ue.VALID ue'' =>
		    if Ue.isEmpty ue'' then ()
		    else error (what ^ " compiled against units that are\
				\ no longer available")
		| Ue.WITNESS u =>
		    reject (what ^ " compiled against unit " ^ u ^
			    " whose interface has changed"))
	end

    fun plan_compi (equiv, ue, iface : Paths.iface) : plan =
	(check_ue ("compiled interface " ^ Paths.ifaceName iface,
		   equiv, ue, Paths.ifaceUeFile iface);
	 EMPTY_PLAN)

    fun plan_compu (equiv, ue, unit : Paths.compunit) : plan =
	(check_ue ("compiled unit " ^ Paths.unitName unit,
		   equiv, ue, Paths.ueFile unit);
	 EMPTY_PLAN)

    fun check_context (eq : equiv, current : ue, past : ue) : status =
	(case Ue.confine eq (past,current)
	   of Ue.VALID ue'' =>
		if Ue.isEmpty ue'' then OK
		else
		    Join (map (fn (u,_) =>
			       Bad ("unit " ^ u ^ " no longer available"))
			  (Ue.listItemsi ue''))
	    | Ue.WITNESS u =>
		Bad ("interface of unit " ^ u ^ " has changed"))

    fun check_imports (current : Info.imports, past : Info.imports) : status =
	let val mismatch = Bad "import length/order mismatch"
	    fun loop (c,p) =
		(case (c,p)
		   of (nil,nil) => OK
		    | (u :: c, u' :: p) =>
			if u = u' then loop(c,p)
			else mismatch
		    | (_ :: _, nil) => mismatch
		    | (nil, _ :: _) => mismatch)
	in  loop(current,past)
	end

    fun check_source (current : Crc.crc, past : Crc.crc) : status =
	if current = past then OK
	else Bad "source file changed"

    fun check_iface (eq : equiv, current : Crc.crc option,
		     past : Crc.crc option) : status =
	(case (current, past)
	   of (SOME crc, SOME crc') =>
		if eq(crc,crc') then OK
		else Bad "exlicit interface changed"
	    | (NONE, NONE) => OK
	    | (SOME _, NONE) => Bad "explicit interface added"
	    | (NONE, SOME _) => Bad "explicit interface removed")

    fun check_info (eq : equiv, current : Info.info,
		    past : Info.info) : status =
	(case (current, past)
	   of (Info.SRCI (context,imports,src),
	       Info.SRCI (context',imports',src')) =>
		Join [check_context (eq, context, context'),
		      check_imports (imports, imports'),
		      check_source (src, src')]
	    | (Info.SRCU (context,imports,src,iface),
	       Info.SRCU (context',imports',src',iface')) =>
		Join [check_context (eq, context, context'),
		      check_imports (imports, imports'),
		      check_source (src, src'),
		      check_iface (eq, iface, iface')]
	    | (Info.PRIMU, Info.PRIMU) => Bad "primitive unit"
	    | _ => Bad "bad info file")

    fun check_info_file (eq : equiv, infoFile : string,
			 current : Info.info) : status =
	if FileCache.exists infoFile then
	    check_info (eq, current, FileCache.read_info infoFile)
	else Bad "first compilation"

    fun check (eq : equiv, infoFile : string, current : Info.info) : bool =
	let val status = check_info_file (eq,infoFile,current)
	    val _ = showStale status
	in  uptodate status
	end

    fun make_ue precontext : ue =
	(foldl (fn ((u,i),ue) => Ue.insert(ue,u,FileCache.crc i))
	 Ue.empty precontext)

    fun srci_info (precontext, imports, iface : Paths.iface) : Info.info =
	Info.SRCI (make_ue precontext, imports,
		   FileCache.crc (Paths.ifaceSourceFile iface))

    fun srcu_info (precontext, imports, iface : iface option,
		   unit : Paths.compunit) : Info.info =
	Info.SRCU (make_ue precontext, imports,
		   FileCache.crc (Paths.sourceFile unit),
		   Option.map FileCache.crc iface)

    fun writeInfo (infoFile : string, info : Info.info) : unit =
	if FileCache.exists infoFile then ()
	else FileCache.write_info (infoFile, info)

    fun make_precontext (eq : equiv, context : context) : precontext =
	(Prelink.check (eq, map importEntry context);
	 map (fn (name, iface) => (name, Paths.ifaceFile iface)) context)

    fun plan_srci (equiv, context, imports, iface : Paths.iface) : plan =
	let val what = Paths.ifaceName iface
	    val _ = msg ("[checking interface " ^ what ^ "]\n")
	    val infoFile = Paths.ifaceInfoFile iface
	    val precontext = make_precontext (equiv, context)
	    val info = srci_info (precontext, imports, iface)
	    val uptodate = check (equiv, infoFile, info)
	    val exists = FileCache.exists (Paths.ifaceFile iface)
	    val plan = if uptodate andalso exists
		       then (writeInfo (infoFile,info); EMPTY_PLAN)
		       else ELAB_SRCI (iface,precontext,imports,info)
	    val _ = showPlan (what, plan)
	in  plan
	end

    fun plan_compile (equiv, context, imports, unit : Paths.compunit) : plan =
	let val what = Paths.unitName unit
	    val _ = msg ("[checking unit " ^ what ^ "]\n")
	    val infoFile = Paths.infoFile unit
	    val src = Paths.isSrcUnit unit
	    val iface : Paths.iface = Paths.unitIface unit
	    val iface : iface option =
		if Paths.isUnitIface iface then NONE
		else SOME (Paths.ifaceFile iface)
	    val precontext = make_precontext (equiv, context)
	    val info = if Paths.isSrcUnit unit
		       then srcu_info (precontext, imports, iface, unit)
		       else Info.PRIMU
	    val uptodate = check (equiv, infoFile, info)
	    val uptoElaborate = !UptoElaborate
	    val uptoAsm = !UptoAsm
	    val keepAsm = !KeepAsm
	    val compressAsm = !CompressAsm

	    val remove : (Paths.compunit -> string) -> unit =
		if uptodate then fn path => ()
		else fn path => FileCache.remove (path unit)
	    val _ =
		if uptoElaborate orelse uptoAsm then
		    remove Paths.objFile
		else ()
	    val _ =
		if uptoElaborate then
		    app remove [Paths.asmFile, Paths.asmzFile]
		else ()

	    val have : (Paths.compunit -> string) -> bool =
		if uptodate then fn path => FileCache.exists (path unit)
		else fn path => false
	    val dont_elaborate =
		isSome iface orelse
		have (Paths.ifaceFile o Paths.unitIface)
	    val dont_generate =
		uptoElaborate orelse
		(uptoAsm andalso not keepAsm) orelse
		have Paths.asmFile orelse
		have Paths.asmzFile
	    val inputs : inputs =
		(case (dont_elaborate,dont_generate)
		   of (true,true) => NONE
		    | _ => SOME (precontext,imports,iface))

	    fun add_flag ((bit : W.word, dont_add : bool),
			  flags : W.word) : W.word =
		if dont_add then flags else W.orb (flags,bit)
	    val flags =
		foldl add_flag W.zero
		[(Elaborate,dont_elaborate),
		 (Generate,dont_generate),
		 (Uncompress,
		  uptoElaborate orelse
		  uptoAsm orelse
		  have Paths.objFile orelse
		  not (have Paths.asmzFile) orelse
		  have Paths.asmFile),
		 (Assemble,
		  uptoElaborate orelse
		  uptoAsm orelse
		  have Paths.objFile),
		 (Compress,
		  uptoElaborate orelse
		  not keepAsm orelse
		  not compressAsm orelse
		  have Paths.asmzFile),
		 (Remove,
		  uptoElaborate orelse
		  keepAsm)]
	    val plan =
		if W.equal(W.zero, flags) then
		    (writeInfo (infoFile, info); EMPTY_PLAN)
		else COMPILE (unit,inputs,flags,info)
	    val _ = showPlan (what, plan)
	in  plan
	end

    fun plan_checku (eq : equiv, context, U : Paths.compunit,
		     I : Paths.iface) : plan =
	let val what = Paths.unitName U
	    val _ = msg ("[checking import " ^ what ^ "]\n")
	    val Uiface = Paths.ifaceFile (Paths.unitIface U)
	    val Iiface = Paths.ifaceFile I
	    val Ucrc = FileCache.crc Uiface
	    val Icrc = FileCache.crc Iiface
	    val precontext = make_precontext (eq, context)
	    val plan = if eq (Ucrc,Icrc) then EMPTY_PLAN
		       else CHECK (precontext, U, I)
	    val _ = showPlan (what, plan)
	in  plan
	end

    (*
	XXX: We could be a lot smarter about how we generate
	executables.  For example, noticing when extant targets are up
	to date and tuning the plan to avoid unnecessary work.  It
	would be especially nice to skip the link completely when the
	executable is up to date.
    *)
    fun plan_link (eq : equiv, units : Paths.compunit list,
		   exe : Paths.exe) : plan =
	let val what = Paths.exeFile exe
	    val _ = msg ("[checking " ^ what ^ "]\n")
	    val uptoElaborate = !UptoElaborate
	    val uptoAsm = !UptoAsm
	    val keepAsm = !KeepAsm
	    val compressAsm = !CompressAsm
	    val _ = Prelink.check (eq, map unitEntry units)

	    fun remove (p : Paths.exe -> string) : unit =
		FileCache.remove (p exe)
	    val _ =
		if uptoElaborate orelse uptoAsm then
		    app remove [Paths.exeObjFile, Paths.exeFile]
		else ()
	    val _ =
		if uptoElaborate then
		    app remove [Paths.exeAsmFile, Paths.exeAsmzFile]
		else ()

	    fun add_flag ((bit : W.word, dont_add : bool),
			  flags : W.word) : W.word =
		if dont_add then flags else W.orb (flags,bit)
	    val flags =
		foldl add_flag W.zero
		[(Generate,
		  uptoElaborate orelse
		  (uptoAsm andalso not keepAsm)),
		 (Link,
		  uptoElaborate orelse
		  uptoAsm),
		 (Compress,
		  uptoElaborate orelse
		  not keepAsm orelse
		  not compressAsm),
		 (Remove,
		  uptoElaborate orelse
		  keepAsm)]
	    val plan =
		if W.equal(W.zero,flags) then EMPTY_PLAN
		else LINK (exe,units,flags)
	    val _ = showPlan (what, plan)
	in  plan
	end

    (*
	XXX: We could be a lot smarter about how we generate
	libraries.  For example, not packing stuff that has not
	changed since the last pack.  For example, supporting multiple
	runs to pack different targets into the same directory.
    *)
    fun plan_pack (eq : equiv, packlist : pack list, lib : Paths.lib) : plan =
	let val what = Paths.libDir lib
	    val _ = msg ("[checking " ^ what ^ "]\n")
	    val entries =
		(List.mapPartial
		 (fn pack =>
		  (case pack
		     of PACKU (Paths.IMPORTU ui,_) => SOME (importEntry ui)
		      | PACKU (u,_) => SOME (unitEntry u)
		      | PACKI (i,_) => SOME (ifaceEntry i)
		      | PACKV _ => NONE))
		 packlist)
	    val _ = Prelink.check (eq, entries)
	    val plan =
		if null packlist then EMPTY_PLAN
		else PACK (lib, packlist)
	    val _ = showPlan (what, plan)
	in  plan
	end

    val done = fn () => EMPTY_PLAN

    fun elab_srci (iface : Paths.iface, precontext, imports,
		   info : Info.info) : unit -> plan =
	let val name = Paths.ifaceName iface
	    val _ = msg ("[compiling interface " ^ name ^ "]\n")
	    val source = Paths.ifaceSourceFile iface
	    val ifaceTarget = Paths.ifaceFile iface
	    val ueTarget = Paths.ifaceUeFile iface
	    val infoTarget = Paths.ifaceInfoFile iface
	    val args = {precontext=precontext,
			imports=imports,
			ifacename=name,
			source=source,
			ifaceTarget=ifaceTarget,
			ueTarget=ueTarget}
	    val _ = Compiler.elaborate_srci args
	    val _ = FileCache.write_info (infoTarget,info)
	in  done
	end

    fun getinputs (inputs : inputs) : precontext * imports * iface option =
	(case inputs
	   of SOME r => r
	    | NONE => error "plan invokes compiler with no inputs")

    fun elab (unit : Paths.compunit, ready : bool,
	      inputs : inputs) : Compiler.il_module =
	let val unitname = Paths.unitName unit
	    val source =
		if Paths.isSrcUnit unit then
		    SOME (Paths.sourceFile unit)
		else NONE
	    val (precontext,imports,iface) = getinputs inputs
	    val iface = Paths.unitIface unit
	    val ifaceTarget = Paths.ifaceFile iface
	    val ueTarget = Paths.ifaceUeFile iface
	    val (il, written) =
		(case (source,Paths.isUnitIface iface)
		   of (SOME source, false) =>
			(Compiler.elaborate_srcu' {precontext=precontext,
						   imports=imports,
						   unitname=unitname,
						   source=source,
						   interface=ifaceTarget},
			 false)
		    | (SOME source, true) =>
			Compiler.elaborate_srcu {precontext=precontext,
						 imports=imports,
						 unitname=unitname,
						 source=source,
						 ifaceTarget=ifaceTarget,
						 ueTarget=ueTarget}
		    | (NONE, _) =>
			Compiler.elaborate_primu {precontext=precontext,
						 imports=imports,
						 unitname=unitname,
						 ifaceTarget=ifaceTarget,
						 ueTarget=ueTarget})
	in  
	    if written andalso ready then
		error ("overwrote up to date interface file: " ^ ifaceTarget)
	    else il
	end

    fun gen (unit : Paths.compunit, il : Compiler.il_module,
	     inputs : inputs) : unit =
	let val unitname = Paths.unitName unit
	    val nilmod = Compiler.il_to_nil (unitname,il)
	    val rtl = Compiler.nil_to_rtl (unitname,nilmod)
	    val asmTarget = Paths.asmFile unit
	    val ueTarget = Paths.ueFile unit
	    val precontext = #1 (getinputs inputs)
	in  Compiler.rtl_to_asm {precontext=precontext,
				 asmTarget=asmTarget,
				 ueTarget=ueTarget,
				 rtl_module=rtl}
	end

    fun uncompress (asmzFile : string, asmFile : string) : unit =
	let val _ = Tools.uncompress {src=asmzFile, dest=asmFile}
	    val _ = FileCache.flushSome [asmFile]
	in  ()
	end

    fun assemble' (asmFile : string, objFile : string) : unit =
	let val _ = Timestamp.timestamp()
	    val _ = Tools.assemble (asmFile, objFile)
	    val _ = FileCache.flushSome [objFile]
	in  ()
	end

    fun compress (asmFile : string, asmzFile : string) : unit =
	let val _ = Tools.compress {src=asmFile, dest=asmzFile}
	    val _ = FileCache.flushSome [asmzFile]
	    val _ = FileCache.remove asmFile
	in  ()
	end

    fun remove (asmFile : string, asmzFile : string) : unit =
	app FileCache.remove [asmFile, asmzFile]

    fun finish (U : Paths.compunit, flags : W.word, info : Info.info) : plan =
	let val _ = if test(flags,Compress) then
			compress (Paths.asmFile U,Paths.asmzFile U)
		    else ()
	    val _ = if test(flags,Remove) then
			remove (Paths.asmFile U,Paths.asmzFile U)
		    else ()
	    val infoTarget = Paths.infoFile U
	    val _ = FileCache.write_info (infoTarget, info)
	in  EMPTY_PLAN
	end

    fun assemble (U : Paths.compunit, flags : W.word,
		  info : Info.info) : plan =
	let val _ = if test(flags,Uncompress) then
			uncompress (Paths.asmzFile U,Paths.asmFile U)
		    else ()
	in  if test(flags,Assemble) then
		if Target.native() then
		    (assemble' (Paths.asmFile U,Paths.objFile U);
		     finish (U,flags,info))
		else
		    let val inputs = NONE
			val mask = W.orb(Compress,Remove)
			val flags = W.andb(flags,mask)
		    in  COMPILE (U, inputs, flags, info)
		    end
	    else finish (U,flags,info)
	end

    fun compile' (U : Paths.compunit, inputs : inputs, flags : W.word,
		  info : Info.info) : unit -> plan =
	let val _ = msg ("[compiling unit " ^ Paths.unitName U ^ "]\n")
	    val ready = not (test(flags,Elaborate))
	    val il = Util.memoize (fn () => elab (U, ready, inputs))
	    val _ = if ready then () else ignore(il())
	in  fn () =>
	    let val _ = if test(flags,Generate) then
			    gen (U,il(),inputs)
			else ()
	    in  assemble (U,flags,info)
	    end
	end

    fun check (precontext, U : Paths.compunit,
	       I : Paths.iface) : unit -> plan =
	let val what = Paths.unitName U
	    val _ = msg ("[checking interface of import " ^ what ^ "]\n")
	    val Uiface = Paths.ifaceFile (Paths.unitIface U)
	    val Iiface = Paths.ifaceFile I
	    val _ =
		if Compiler.eq (what,precontext,Iiface,Uiface) then ()
		else reject ("unit " ^ what ^ " does not match interface " ^
			     Paths.ifaceName I)
	in  done
	end

    fun gen_link (exe : Paths.exe, units : Paths.compunit list) : unit =
	let val what = Paths.exeFile exe
	    val asmFile = Paths.exeAsmFile exe
	    val unitnames = map Paths.unitName units
	    val _ = Compiler.link {asmTarget = asmFile,
				   unitnames = unitnames}
	in  ()
	end

    fun link_link (exe : Paths.exe, units : Paths.compunit list) : unit =
	let val exeFile = Paths.exeFile exe
	    val objFile = Paths.exeObjFile exe
	    val objectFiles = (map (Paths.objFile) units) @ [objFile]
	    val _ = Tools.link (objectFiles, exeFile)
	in  ()
	end

    fun finish_link (exe : Paths.exe, flags : W.word) : plan =
	let val _ = if test(flags,Compress) then
			compress (Paths.exeAsmFile exe,
				  Paths.exeAsmzFile exe)
		    else ()
	    val _ = if test(flags,Remove) then
			remove (Paths.exeAsmFile exe,
				Paths.exeAsmzFile exe)
		    else ()
	in  EMPTY_PLAN
	end

    fun link (exe : Paths.exe, units : Paths.compunit list, flags : W.word)
	     () : plan =
	let val _ = if test(flags,Generate) then
			gen_link (exe,units)
		    else ()
	in  if test(flags,Link) then
		if Target.native() then
		    (msg ("[linking " ^ Paths.exeFile exe ^ "]\n");
		     assemble' (Paths.exeAsmFile exe,Paths.exeObjFile exe);
		     link_link (exe,units);
		     finish_link (exe,flags))
		else
		    let val mask = W.orb(Compress,Remove)
			val flags = W.andb(flags,mask)
		    in  LINK (exe, units, flags)
		    end
	    else finish_link (exe,flags)
	end

    fun ifaceFiles (i : Paths.iface) : (Paths.iface -> string) list =
	(case i
	   of Paths.SRCI _ => [Paths.ifaceSourceFile, Paths.ifaceInfoFile,
			       Paths.ifaceFile, Paths.ifaceUeFile]
	    | Paths.COMPI _ => [Paths.ifaceFile, Paths.ifaceUeFile]
	    | Paths.UNITI _ => [Paths.ifaceFile, Paths.ifaceUeFile])

    fun unitFiles (u : Paths.compunit) : (Paths.compunit -> string) list =
	(case u
	   of Paths.SRCU _ => [Paths.sourceFile, Paths.infoFile, Paths.ueFile,
			       Paths.objFile]
	    | Paths.COMPU _ => [Paths.ueFile, Paths.objFile]
	    | Paths.PRIMU _ => [Paths.infoFile, Paths.ueFile, Paths.objFile]
	    | Paths.IMPORTU _ => [])

    fun ifaceEntry (file : string -> E.exp, i : Paths.iface,
		    imports : E.id list) : E.entry =
	(case i
	   of Paths.SRCI (I,_,src) => E.SRCI (I, file src, imports)
	    | Paths.COMPI (I,iface,ue) => E.COMPI (I, file iface, file ue)
	    | Paths.UNITI _ => error "pack saw a UNITI")

    fun unitEntry (file : string -> E.exp, u : Paths.compunit,
		   imports : E.id list) : E.entry =
	(case u
	   of Paths.SRCU (U,_,src,i) =>
		let val Iopt = if Paths.isUnitIface i then NONE
			       else SOME (Paths.ifaceName i)
		in  E.SRCU (U, Iopt, file src, imports)
		end
	    | Paths.COMPU (U,obj,ue,i) =>
		let val I = Paths.ifaceName i
		in  E.COMPU (U, I, file obj, file ue)
		end
	    | Paths.PRIMU (U,_,_) => E.PRIMU (U, imports)
	    | Paths.IMPORTU (U, i) =>
		let val I = Paths.ifaceName i
		in  E.IMPORTU (U, I)
		end)

    fun copyFiles (old : 'a, new : 'a, files : ('a -> string) list) =
	let fun copy file : unit = File.copy (file old, file new)
	in  app copy files
	end

    fun pack' (lib : Paths.lib, packs : pack list) : ExtSyn.groupfile =
	let val iface = Paths.libIface lib
	    val unit = Paths.libUnit lib
	    val dir = Paths.libDir lib
	    val n = size dir + 1
	    fun file (p : string) : E.exp =
		E.EXP_STR (String.extract(p,n,NONE))
	    fun pack (p : pack) : ExtSyn.entry =
		(case p
		   of PACKU (u,imports) =>
			let val u' = unit u
			    val _ = copyFiles (u, u', unitFiles u)
			in  unitEntry (file,u',imports)
			end
		    | PACKI (i,imports) =>
			let val i' = iface i
			    val _ = copyFiles (i, i', ifaceFiles i)
			in  ifaceEntry (file,i',imports)
			end
		    | PACKV arg => E.VAL arg)
	in  map pack packs
	end

    fun pack (lib : Paths.lib, packs : pack list) () : plan =
	let val _ = msg("[packing " ^ Paths.libDir lib ^ "]\n")
	    val group = pack' (lib, packs)
	    val groupFile = Paths.libGroupFile lib
	    val _ = Group.write (groupFile, group)
	in  EMPTY_PLAN
	end

    fun compile (plan : plan) : unit -> plan =
	(case plan
	   of EMPTY_PLAN => done
	    | ELAB_SRCI arg => elab_srci arg
	    | COMPILE arg => compile' arg
	    | CHECK arg => check arg
	    | LINK arg => link arg
	    | PACK arg => pack arg)

    fun add_path (flags : W.word)
		 ((bit : W.word, path : 'a -> string),
		  acc : ('a -> string) list) : ('a -> string) list =
	if test(flags,bit) then path :: acc else acc

    fun targets (plan : plan) : string list =
	(case plan
	   of EMPTY_PLAN => []
	    | ELAB_SRCI (iface,_,_,_) =>
		(map (fn p => p iface)
		 [Paths.ifaceInfoFile, Paths.ifaceUeFile, Paths.ifaceFile])
	    | COMPILE (unit,_,flags,_) =>
		let val paths =
			foldl (add_path flags) nil
			[(Elaborate, Paths.ifaceUeFile o Paths.unitIface),
			 (Elaborate, Paths.ifaceFile o Paths.unitIface),
			 (Generate, Paths.asmFile),
			 (Generate, Paths.ueFile),
			 (Uncompress, Paths.asmFile),
			 (Assemble, Paths.objFile),
			 (Compress, Paths.asmzFile),
			 (Remove, Paths.asmFile),
			 (Remove, Paths.asmFile)]
		in  map (fn p => p unit) paths
		end
	    | CHECK _ => []
	    | LINK (exe,_,flags) =>
		let val paths =
			foldl (add_path flags) nil
			[(Generate, Paths.exeAsmFile),
			 (Link, Paths.exeObjFile),
			 (Link, Paths.exeFile),
			 (Compress, Paths.exeAsmzFile),
			 (Remove, Paths.exeAsmFile),
			 (Remove, Paths.exeAsmFile)]
		in  map (fn p => p exe) paths
		end
	    | PACK _ => []) (* XXX: each item in packlist is several targets *)

    fun flush (plan : plan) : unit = FileCache.flushSome (targets plan)

    fun flushAll () : unit =
	(File.flush_dir_cache();
	 FileCache.flushAll();
	 Name.reset_varmap())
end
