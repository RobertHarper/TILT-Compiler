(*$import Firstlude TiltPrim Prelude OS_FILE_SYS OS_PATH OS_IO OS_PROCESS *)
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


