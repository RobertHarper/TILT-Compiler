(* posix-error.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX error codes.
 *
 *)

structure POSIX_Error :>
	POSIX_ERROR  where type syserror = int =
struct

	val errorMsg : int -> string = fn e => Ccall(syserror_msg, e)
	val errorName : int -> string = ccall1 posix_error_name
	val errorNum : string -> int = ccall1 posix_error_num

	type syserror = int

	val toWord : syserror -> word = TiltPrim.int32touint32
	val fromWord : word -> syserror = TiltPrim.uint32toint32

	fun syserror (str:string) : int option =
		SOME(errorNum str) handle _ => NONE

	val toobig	= errorNum "toobig"
	val acces	= errorNum "acces"
	val again	= errorNum "again"
	val badf	= errorNum "badf"
	val badmsg	= errorNum "badmsg"
	val busy	= errorNum "busy"
	val canceled	= errorNum "canceled"
	val child	= errorNum "child"
	val deadlk	= errorNum "deadlk"
	val dom	= errorNum "dom"
	val exist	= errorNum "exist"
	val fault	= errorNum "fault"
	val fbig	= errorNum "fbig"
	val inprogress	= errorNum "inprogress"
	val intr	= errorNum "intr"
	val inval	= errorNum "inval"
	val io	= errorNum "io"
	val isdir	= errorNum "isdir"
	val loop	= errorNum "loop"
	val mfile	= errorNum "mfile"
	val mlink	= errorNum "mlink"
	val msgsize	= errorNum "msgsize"
	val nametoolong	= errorNum "nametoolong"
	val nfile	= errorNum "nfile"
	val nodev	= errorNum "nodev"
	val noent	= errorNum "noent"
	val noexec	= errorNum "noexec"
	val nolck	= errorNum "nolck"
	val nomem	= errorNum "nomem"
	val nospc	= errorNum "nospc"
	val nosys	= errorNum "nosys"
	val notdir	= errorNum "notdir"
	val notempty	= errorNum "notempty"
	val notsup	= errorNum "notsup"
	val notty	= errorNum "notty"
	val nxio	= errorNum "nxio"
	val perm	= errorNum "perm"
	val pipe	= errorNum "pipe"
	val range	= errorNum "range"
	val rofs	= errorNum "rofs"
	val spipe	= errorNum "spipe"
	val srch	= errorNum "srch"
	val xdev	= errorNum "xdev"

end
