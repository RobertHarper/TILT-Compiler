functor Manager (structure Master : MASTER)
    :> MANAGER =
struct

  val error = fn s => Util.error "manager.sml" s

  (*
	This is not Stats.int "DiagLevel" because
	we do not want reset_stats to trash the value.
  *)
  val DiagLevel = ref 0

  val TimeFinal = Stats.ff "TimeFinal"
  val ResetStats = Stats.tt "ResetStats"

  val slave = Slave.run

  fun helper runner groupfile =
	let val _ = if !ResetStats then Stats.clear_stats() else ()
	in  runner groupfile;
	    if (!TimeFinal)
		then Stats.print_stats()
	    else ()
	end

  fun master groupfile = helper Master.run groupfile
  fun slaves (slaveList : (int * string) list) =
      let val commDir = Dirs.commDir()
	  val script = OS.Path.joinDirFile {dir=Dirs.binDir(), file="til_slave"}
	  fun cmdline (num, count, machine) =
	      String.concat [script, " ", Int.toString num, " ", Int.toString count, " ",
			     machine, " ", commDir, "&"]
	  fun startSlave ncm =
	      (OS.Process.system (cmdline ncm); ())
	  fun loop _ [] = ()
	    | loop cur ((count,machine)::rest) = (startSlave(cur,count,machine); loop (cur+count) rest)
      in  loop 0 slaveList
      end

  fun tilc arg =
      let fun runner args =
	  let val {setup,step,complete} = Master.once args
	      val {setup=setup', step=step', complete=complete'} = Slave.slave ()
	      fun slaveStep state' = step' state'
	      fun loop (state,state') =
		  (case (step state) of
		       Master.COMPLETE t => (complete(); complete' state')
		     | Master.PROCESSING (t,state) => loop (state, slaveStep state')
		     | Master.IDLE (t,state, _) => loop (state, slaveStep state'))
	  in  loop (setup(), setup'())
	  end
      in  helper runner arg
      end
  fun make groupfile = tilc groupfile

  fun purgeAll groupfile = Master.purgeAll groupfile
  fun purge groupfile = Master.purge groupfile
  fun clean groupfile = Master.clean groupfile

  (*
	The significant values for DiagLevel are, roughly:
	0: Print warnings and errors only
	1: Message for each compile/scan
	2: Explaination for each compile
	3: Message for each phase of each compile
	4: Compilation diagnostics
	5: Master diagnostics
  *)
  val flags : bool ref list list =
	[nil,
	 [Update.UpdateDiag, Slave.SlaveDiag],
	 [Update.ShowPlan, Update.ShowStale],
	 [Master.Checkpoint, LinkParse.LinkParseDiag, Compiler.CompilerDiag,
	  Linknil.LinkNilDiag, Linkrtl.LinkRtlDiag, Linksparc.diag,
	  Linkalpha.diag],
	 [Master.CheckpointVerbose, LinkIl.LinkIlDiag, Tonil.diag,
	  IlContext.IlcontextDiag, Hoist.HoistDiag, Inline.InlineDiag,
	  NilStatic.NilStaticDiag, Specialize.SpecializeDiag,
	  Normalize.NormalizeDiag, Tools.ToolsDiag],
	 [Master.MasterDiag, Master.ShowEnable, Master.ShowUptodate,
	  Master.ShowFinalReport]]

  fun setDiagLevel (i : int) : unit =
    let val i = Int.max (i+1, 0)
	val i = Int.min (i, length flags)
	val enable = List.concat(List.take(flags,i))
    in  app (fn r => r := true) enable
    end

  fun wrap (f : 'a -> unit) (x : 'a) : unit =
      ((setDiagLevel (!DiagLevel); f x) handle e =>
	  if !ExnHandler.Interactive then
	      (ExnHandler.print e; raise e)
	  else
	      raise e)

  val clean = wrap clean
  val purge = wrap purge
  val purgeAll = wrap purgeAll
  val slave = wrap slave
  val slaves = wrap slaves
  val master = wrap master
  val make = wrap make

end

local
    structure BootMaster = Master (val basis = NONE)
    structure E = ExtSyn
    fun libFile (name : string) : unit -> string =
	Util.memoize (fn () =>
		      (case OS.Process.getEnv "TILT_LIBDIR"
			 of NONE =>
			     raise Compiler.Reject "TILT_LIBDIR is not set"
			  | SOME dir => OS.Path.joinDirFile {dir=dir,file=name}))

    (*
	Each group file imports the basis library group file, each
	source unit imports the standard top-level environment, and
	each executable pulls in units to satisfy ../Runtime/client.h.
    *)
    val group : unit -> string = libFile "Lib/basis/group"
    fun fixup (groupfile : E.groupfile) : E.groupfile =
	let
	    fun fix (imports : E.imports) : E.imports =
		("Firstlude"::"TiltPrim"::"Prelude"::"TopLevel"::imports)
	    fun fix' (units : E.id list) : E.id list =
		("TiltExn"::units)
	    fun fixEntry (ent : E.entry) : E.entry =
		(case ent
		   of E.SRCI (I,spec,imports) => E.SRCI (I,spec,fix imports)
		    | E.SRCU (U,I,dec,imports) => E.SRCU (U,I,dec,fix imports)
		    | E.MAKE_EXE (exe,units) => E.MAKE_EXE (exe,fix' units)
		    | E.MARK (pos,ent) => E.MARK (pos,fixEntry ent)
		    | _ => ent)
	    val groupfile = map fixEntry groupfile
	    val import = E.IMPORT (E.EXP_STR (group()))
	in  import :: groupfile
	end
    structure Master = Master (val basis = SOME {group=group, fixup=fixup})
in
    structure Boot = Manager (structure Master = BootMaster)
    structure Manager = Manager (structure Master = Master)
end
