structure Manager :> MANAGER =
struct

    val error = fn s => Util.error "manager.sml" s

    val DiagLevel = Stats.int ("DiagLevel",0)
    val PrintStats = Stats.ff "PrintStats"
    val ResetStats = Stats.tt "ResetStats"

    type label = Name.label
    type targets = label list

    val unit : string -> label = Name.unit_label
    val interface : string -> label = Name.interface_label

    fun Slave (f : 'a -> 'b) (with_slave : bool) (x : 'a) : 'b =
	if with_slave then
	    let fun run_slave () = (Slave.Standalone := false; Slave.slave())
		val kill_slave = Util.background' run_slave
		val r = Util.apply(f,x)
		val _ = kill_slave()
	    in  r()
	    end
	else
	    f x

    fun Stats (f : 'a -> 'b) (x : 'a) : 'b =
	let val _ = if !ResetStats then Stats.clear_measurements() else ()
	    val _ = if !PrintStats then Stats.SendMeasurements := true else ()
	    val r = Util.apply(f,x)
	    val _ = if !PrintStats then Stats.print_stats() else ()
	in  r()
	end

    (*
	The significant values for DiagLevel are, roughly:
	0: Print warnings and errors only
	1: Message for each compile/scan
	2: Explain cut-off logic
	3: Make master and compiler verbose
	4: Make master and compiler very verbose
    *)
    val flags : bool ref list list =
	[nil,
	 [Update.UpdateDiag, Compiler.CompilerDiag, Slave.SlaveDiag],
	 [Update.ShowPlan, Update.ShowStatus, Master.MasterDiag],
	 [Master.MasterVerbose, Compiler.CompilerVerbose,
	  LinkParse.LinkParseDiag, Compiler.CompilerDiag,
	  Linknil.LinkNilDiag, Linkrtl.LinkRtlDiag, Linksparc.diag,
	  Linkalpha.diag],
	 [Master.MasterVVerbose, LinkIl.LinkIlDiag, Tonil.diag,
	  IlContext.IlcontextDiag, Hoist.HoistDiag, Inline.InlineDiag,
	  NilStatic.NilStaticDiag, Specialize.SpecializeDiag,
	  Normalize.NormalizeDiag, Tools.ToolsDiag, LilTypecheck.diag]]

    fun setDiagLevel (i : int) : unit =
	let val i = Int.max (i+1, 0)
	    val i = Int.min (i, length flags)
	    val enable = List.concat(List.take(flags,i))
	in  app (fn r => r := true) enable
	end

    fun Diag (f : 'a -> 'b) (x : 'a) : 'b =
	(setDiagLevel (!DiagLevel); f x)

    fun Exn (f : 'a -> 'b) (x : 'a) : 'b =
	(f x handle e =>
	    if !ExnHandler.Interactive then
		(ExnHandler.print e; raise e)
	    else
		raise e)

    val make' = Exn (Diag (Stats (Slave Master.make)))
    val make_exe' = Exn (Diag (Stats (Slave Master.make_exe)))
    val make_lib' = Exn (Diag (Stats (Slave Master.make_lib)))

    val make = Exn (Diag (Stats (Slave Master.make true)))
    val make_exe = Exn (Diag (Stats (Slave Master.make_exe true)))
    val make_lib = Exn (Diag (Stats (Slave Master.make_lib true)))
    val purge = Exn (Diag Master.purge)
    val purgeAll = Exn (Diag Master.purgeAll)

    val slave = fn x => Diag Slave.slave x

    fun slaves (slaveList : (int * string) list) : unit =
	let val til_slave = IntSyn.F.til_slave()
	    val commdir = IntSyn.F.commdir()
	    fun cmdline (num:int, count:int, machine:string) : string list =
		[til_slave,Int.toString num,
		 Int.toString count,machine,commdir]
	    fun startSlave (arg:int * int * string) : unit =
		let fun run () : unit = Util.run (cmdline arg)
		in  ignore(Util.background' run)
		end
	    fun loop (cur : int, slaveList : (int * string) list) : unit =
		(case slaveList
		   of nil => ()
		    | (count,machine) :: rest =>
			(startSlave (cur,count,machine);
			 loop (cur+count, rest)))
	in  loop (0,slaveList)
	end

    val slaves = (Exn o Diag) slaves

end
