(*$import Prelude OS_PROCESS Posix PreOS OS_FileSys OS_Path OS_Process OS_IO_Str OS_SIG *)
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


(*
 * $Log$
# Revision 1.5  2001/12/13  16:31:33  swasey
# *** empty log message ***
# 
# Revision 1.4  2000/11/27  22:36:44  swasey
# *** empty log message ***
# 
 * Revision 1.3  2000/09/21 01:08:39  pscheng
 * *** empty log message ***
 *
# Revision 1.2  2000/09/12  18:55:04  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/03/09  19:54:22  pscheng
# added basis
#
 * Revision 1.2  1997/06/30  19:36:33  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
