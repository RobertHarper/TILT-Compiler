(*$import Prelude POSIX_ERROR POSIX_extern *)
(* posix-error.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX error codes.
 *
 *)

structure POSIX_Error :> POSIX_ERROR  where type syserror = int =
  struct

    type syserror = int

    fun toWord se = int32touint32 se
    fun fromWord w = uint32toint32 w
    fun errorMsg i    = Ccall(posix_error_msg, i)
    fun errorName err = Ccall(posix_error_name, err)
    val posix_error_num = fn (str : string) => Ccall(posix_error_num, str)
    fun syserror str = let val s : int = posix_error_num str
		       in if s > 536870912
			      then SOME s
			  else NONE
		       end

    val toobig      = posix_error_num "toobig"
    val acces       = posix_error_num "acces"
    val again       = posix_error_num "again"
    val badf        = posix_error_num "badf"
    val badmsg      = posix_error_num "badmsg"
    val busy        = posix_error_num "busy"
    val canceled    = posix_error_num "canceled"
    val child       = posix_error_num "child"
    val deadlk      = posix_error_num "deadlk"
    val dom         = posix_error_num "dom"
    val exist       = posix_error_num "exist"
    val fault       = posix_error_num "fault"
    val fbig        = posix_error_num "fbig"
    val inprogress  = posix_error_num "inprogress"
    val intr        = posix_error_num "intr"
    val inval       = posix_error_num "inval"
    val io          = posix_error_num "io"
    val isdir       = posix_error_num "isdir"
    val loop        = posix_error_num "loop"
    val mfile       = posix_error_num "mfile"
    val mlink       = posix_error_num "mlink"
    val msgsize     = posix_error_num "msgsize"
    val nametoolong = posix_error_num "nametoolong"
    val nfile       = posix_error_num "nfile"
    val nodev       = posix_error_num "nodev"
    val noent       = posix_error_num "noent"
    val noexec      = posix_error_num "noexec"
    val nolck       = posix_error_num "nolck"
    val nomem       = posix_error_num "nomem"
    val nospc       = posix_error_num "nospc"
    val nosys       = posix_error_num "nosys"
    val notdir      = posix_error_num "notdir"
    val notempty    = posix_error_num "notempty"
    val notsup      = posix_error_num "notsup"
    val notty       = posix_error_num "notty"
    val nxio        = posix_error_num "nxio"
    val perm        = posix_error_num "perm"
    val pipe        = posix_error_num "pipe"
    val range       = posix_error_num "range"
    val rofs        = posix_error_num "rofs"
    val spipe       = posix_error_num "spipe"
    val srch        = posix_error_num "srch"
    val xdev        = posix_error_num "xdev"

  end (* structure POSIX_Error *)
