(*$import POSIX_Error POSIX_Signal POSIX_Process POSIX_FileSys POSIX_IO POSIX_Sys_DB POSIX_Tty POSIX_ProcEnv POSIX *)
(* posix.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 binding
 *
 *)

structure Posix :> POSIX =
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

(*
 * $Log$
# Revision 1.1  98/03/09  19:53:46  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
