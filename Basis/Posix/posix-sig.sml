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

(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:40  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:53:35  pscheng
 * added basis
 *
 * Revision 1.3  1997/06/07  15:27:42  jhr
 *   SML'97 Basis Library changes (phase 3; Posix changes)
 *
 * Revision 1.2  1997/05/20  12:15:50  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
