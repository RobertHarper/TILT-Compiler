(*$import Prelude OS_FILE_SYS OS_PATH OS_IO OS_PROCESS *)
(* os-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature OS =
  sig
    eqtype syserror

    exception SysErr of string * syserror option
    
    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror : string -> syserror option

    structure FileSys : OS_FILE_SYS
    structure Path : OS_PATH
    structure Process : OS_PROCESS
    structure IO : OS_IO

  end;


(*
 * $Log$
# Revision 1.3  2000/11/27  22:36:38  swasey
# *** empty log message ***
# 
 * Revision 1.2  2000/09/12 18:54:35  swasey
 * Changes for cutoff compilation
 *
# Revision 1.1  98/03/09  19:53:08  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:21  george
 *   Version 109.24
 *
 *)
