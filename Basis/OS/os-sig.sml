(*$import OS_FILE_SYS OS_PATH OS_IO_SIG OS_PROCESS *)
(* os-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature OS =
  sig
    eqtype syserror

    val errorName : syserror -> string
    val syserror : string -> syserror option
    val errorMsg : syserror -> string

    exception SysErr of (string * syserror option)


    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure IO : OS_IO
    structure Process : OS_PROCESS

  end;


(*
 * $Log$
# Revision 1.1  98/03/09  19:53:08  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:21  george
 *   Version 109.24
 *
 *)
