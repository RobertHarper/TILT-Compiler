(*$import Prelude POSIX_Error POSIX_Signal POSIX_Process POSIX_FileSys POSIX_IO_Str POSIX_Sys_DB POSIX_Tty POSIX_ProcEnv POSIX *)
(* posix.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 binding
 *
 *)

structure Posix :>
    sig
	include POSIX
	  where type Error.syserror = int
	  
	sharing type Process.pid = ProcEnv.pid = TTY.pid
	    and type Process.signal = Signal.signal
	    and type ProcEnv.file_desc = FileSys.file_desc = TTY.file_desc = IO.file_desc
	    and type FileSys.open_mode = IO.open_mode
	    and type ProcEnv.uid = FileSys.uid = SysDB.uid
	    and type ProcEnv.gid = FileSys.gid = SysDB.gid
	    and type FileSys.O.flags = IO.O.flags
    end =
  struct

    structure Error   = POSIX_Error
    structure Signal  = POSIX_Signal
    structure Process = POSIX_Process
    structure ProcEnv = POSIX_ProcEnv
    structure FileSys = POSIX_FileSys
    structure IO      = POSIX_IO
    structure SysDB   = POSIX_Sys_DB
    structure TTY     = POSIX_TTY

  end (* structure Posix *)

