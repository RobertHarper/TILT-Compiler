(* os.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Generic OS interface (NEW BASIS)
 *
 *)

structure OS :> OS where type IO.iodesc = PreOS.IO.iodesc
                     and type IO.poll_desc = PreOS.IO.poll_desc
                     and type IO.poll_info = PreOS.IO.poll_info
		     and type syserror = Posix.Error.syserror =
  struct

    open PreOS (* open type-only structure to get types *)

    val errorMsg = Posix.Error.errorMsg
    val errorName = Posix.Error.errorName
    val syserror = Posix.Error.syserror

    structure FileSys = OS_FileSys
    structure Path = OS_Path
    structure Process : OS_PROCESS = OS_Process
    structure IO = OS_IO

  end (* OS *)


