(*$import POSIX_ERROR POSIX_SIGNAL POSIX_PROCESS POSIX_PROC_ENV POSIX_FILE_SYS POSIX_IO POSIX_SYS_DB POSIX_TTY *)
(* posix-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for POSIX 1003.1 binding
 *
 *)

signature POSIX =
  sig

    structure Error   : POSIX_ERROR
    structure Signal  : POSIX_SIGNAL
    structure Process : POSIX_PROCESS

    structure ProcEnv : POSIX_PROC_ENV
    structure FileSys : POSIX_FILE_SYS
    structure IO      : POSIX_IO
    structure SysDB   : POSIX_SYS_DB
    structure TTY     : POSIX_TTY

    (* These hold for TILT but are not part of this signature.
        type Error.syserror = int
	  
	sharing type Process.pid = ProcEnv.pid = TTY.pid
	    and type Process.signal = Signal.signal
	    and type ProcEnv.file_desc = FileSys.file_desc = TTY.file_desc = IO.file_desc
	    and type FileSys.open_mode = IO.open_mode
	    and type ProcEnv.uid = FileSys.uid = SysDB.uid
	    and type ProcEnv.gid = FileSys.gid = SysDB.gid
	    and type FileSys.O.flags = IO.O.flags
     *)
  end (* signature POSIX *)

