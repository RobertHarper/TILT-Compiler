structure Manager :> MANAGER =
struct

	val DiagLevel = Stats.int ("DiagLevel",0)
	val PrintStats = Stats.ff "PrintStats"
	val ResetStats = Stats.tt "ResetStats"
	val NumSlaves = Stats.int ("NumSlaves",~1)

	val reject = Util.reject

	fun Slaves (slaveonly:bool) (f : 'a -> 'b) (x : 'a) : 'b =
		let	val nproc = if !NumSlaves = ~1 then Tools.nproc() else !NumSlaves
			val nproc = nproc - (if slaveonly then 1 else 0)
			fun run_slave (n : int) () : 'a =
				let	val () = UtilError.Interactive := false
					val () = if n = 0 andalso not slaveonly
						then Slave.PauseTime := 100
						else ()
				in	Slave.slave()
				end
			val killers = List.tabulate (nproc, fn n => Util.background' (run_slave n))
			val r = Util.apply(f,x)
			val _ = app (fn killer => killer()) killers
		in	r()
		end

	fun Stats (f : 'a -> 'b) (x : 'a) : 'b =
		let	val _ = if !ResetStats then Stats.clear_measurements() else ()
			val _ = if !PrintStats then Stats.SendMeasurements := true else ()
			val r = Util.apply(f,x)
			val _ = if !PrintStats then Stats.print_stats() else ()
		in	r()
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
		  Linknil.LinkNilDiag, Linklil.LinkLilDiag, Linkrtl.LinkRtlDiag, Linksparc.diag,
		  Tools.ToolsDiag, LinkTAL.diag],
		 [Master.MasterVVerbose, LinkIl.LinkIlDiag, Tonil.diag,
		  IlContext.IlcontextDiag, Hoist.HoistDiag, Inline.InlineDiag,
		  NilStatic.NilStaticDiag, Specialize.SpecializeDiag,
		  Normalize.NormalizeDiag, Tools.ShowTools, LilTypecheck.diag]]

	fun setDiagLevel (i : int) : unit =
		let	val i = Int.max (i+1, 0)
			val i = Int.min (i, length flags)
			val enable = List.concat(List.take(flags,i))
		in	app (fn r => r := true) enable
		end

	fun Diag (f : 'a -> 'b) (x : 'a) : 'b =
		(setDiagLevel (!DiagLevel); f x)

	fun Exn (f : 'a -> 'b) (x : 'a) : 'b =
		(f x handle e =>
			if !UtilError.Interactive then
				(UtilError.print e; raise e)
			else
				raise e)

	fun usage () : 'a =
		let
			val msg = concat [
				"TILT version ", Version.version,
				((" using basis " ^ IntSyn.F.basisdesc())
				 handle _ => ""),
				"\n\
				\usage: tilt project ...\n\
				\       tilt -o exe project ...\n\
				\       tilt -l lib project ...\n\
				\       tilt -p project ...\n\
				\       tilt -s\n\
				\options:\n\
				\  -v       increase diagnostics level\n\
				\  -f F[=v] set compiler flag F (to v)\n\
				\  -j n     start n slaves\n\
				\  -c U     operate on unit U\n\
				\  -C I     operate on interface I\n\
				\  -t T     set target architecture to T (sparc or talx86)\n"
			]
			val () = TextIO.output (TextIO.stdErr, msg)
		in	reject "usage"
		end

	fun inc (r : int ref) : unit = r := !r + 1

	fun aton (s : string) : int =
		(case (Int.fromString s) of
			NONE => usage()
		|	SOME n => if n < 0 then usage() else n)

	fun atoobjtype (s : string) : Platform.objtype =
		(case (Platform.fromString s) of
			SOME objtype => objtype
		|	NONE => usage())

	fun setflag (arg : string) : unit =
		let	val arg = Substring.all arg
			val (n,v) = Substring.position "=" arg
			val n =
				if Substring.size n = 0
				then usage()
				else Substring.string n
			fun check (s:substring) : unit =
				if Substring.size s <> 0 then usage() else ()
		in	(case (Substring.size v) of
				0 => Stats.bool n := true
			|	1 => usage()	(* "n=" *)
			|	_ =>
					let	val v = Substring.slice(v,1,NONE)
					in	(case (Bool.scan Substring.getc v) of
							SOME (b,s) =>
								(check s; Stats.bool n := b)
						|	NONE =>
								(case (Int.scan StringCvt.DEC Substring.getc v) of
									SOME (i,s) =>
										(check s; Stats.int' n := i)
								|	NONE => usage()))
					end)
		end

	fun make (args : string list) : unit =
		let
			val exe : string option ref = ref NONE
			val lib : string option ref = ref NONE
			val purge : int ref = ref 0
			val slaveonly : bool ref = ref false
			val targets : Name.label list ref = ref nil

			fun parseopt ({argc,argf,eargf}:Arg.arg) : unit =
				(case argc of
					#"o" => exe := SOME (eargf usage)
				|	#"l" => lib := SOME (eargf usage)
				|	#"p" => inc purge
				|	#"s" => slaveonly := true
				|	#"v" => inc DiagLevel
				|	#"f" => setflag (eargf usage)
				|	#"j" => NumSlaves := aton (eargf usage)
				|	#"c" => targets := (Name.unit_label (eargf usage)) :: !targets
				|	#"C" => targets := (Name.interface_label (eargf usage)) :: !targets
				|	#"t" => Target.setTarget (atoobjtype (eargf usage))
				|	_ => usage())

			val projects : string list = Arg.args parseopt args
			val targets = rev (!targets)

			fun check_projects () : unit =
				if null projects then usage()
				else ()

			val () = Slave.Standalone := !slaveonly

			val run : unit -> unit =
				(case (!exe, !lib, !purge, !slaveonly) of
					(NONE, NONE, 0, false) =>
						let	val () = check_projects()
							val run = fn () => Master.make (projects,targets)
						in	Exn(Diag(Stats(Slaves false run)))
						end
				|	(SOME exe, NONE, 0, false) =>
						let	val () = check_projects()
							val run = fn () => Master.make_exe (projects,exe,targets)
						in	Exn(Diag(Stats(Slaves false run)))
						end
				|	(NONE, SOME lib, 0, false) =>
						let	val () = check_projects()
							val run = fn () => Master.make_lib (projects,lib,targets)
						in	Exn(Diag(Stats(Slaves false run)))
						end
				|	(NONE, NONE, 1, false) =>
						let	val () = check_projects()
							val run = fn () => Master.purge (projects,targets)
						in	Exn(Diag run)
						end
				|	(NONE, NONE, 2, false) =>
						let	val () = check_projects()
							val run = fn () => Master.purgeAll (projects,targets)
						in	Exn(Diag run)
						end
				|	(NONE, NONE, 0, true) =>
						let	val () = if null projects andalso null targets
								then () else usage()
						in	Diag(Slaves true Slave.slave)
						end
				|	_ => usage())
		in	run()
		end
end
