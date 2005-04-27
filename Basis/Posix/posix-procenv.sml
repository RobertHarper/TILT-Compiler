(* posix-procenv.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for POSIX 1003.1 process environment submodule
 *
 *)

structure POSIX_ProcEnv :> POSIX_PROC_ENV
	where type pid = POSIX_Process.pid
	and   type file_desc = POSIX_FileSys.file_desc
	and   type uid = PrePosix.uid
	and   type gid = PrePosix.gid =
struct

    val posix_procenv_getpid : unit -> int = fn () => Ccall(posix_procenv_getpid,())
    val posix_procenv_getppid : unit -> int = fn () => Ccall(posix_procenv_getppid,())
    val posix_procenv_getuid : unit -> word = fn () => Ccall(posix_procenv_getuid,())
    val posix_procenv_geteuid : unit -> word = fn () => Ccall(posix_procenv_geteuid,())
    val posix_procenv_getgid : unit -> word = fn () => Ccall(posix_procenv_getgid,())
    val posix_procenv_getegid : unit -> word = fn () => Ccall(posix_procenv_getegid,())
    val posix_procenv_setuid : word -> unit = ccall1 posix_procenv_setuid
    val posix_procenv_setgid : word -> unit = ccall1 posix_procenv_setgid
    val posix_procenv_getgroups : unit -> uct = ccall0 posix_procenv_getgroups
    val posix_procenv_getgroups_size : uct -> int = fn p => Ccall(posix_procenv_getgroups_size,p)
    val posix_procenv_getgroups_nth : uct * int -> word = fn (p,i) => Ccall(posix_procenv_getgroups_nth,p,i)
    val posix_procenv_getgroups_free : uct -> unit = fn p => Ccall(posix_procenv_getgroups_free,p)
    val getlogin : unit -> string = ccall0 posix_procenv_getlogin
    val posix_procenv_getpgrp : unit -> int = fn () => Ccall(posix_procenv_getpgrp,())
    val posix_procenv_setsid : unit -> int = ccall0 posix_procenv_setsid
    val posix_procenv_setpgid : int * int -> unit = ccall2 posix_procenv_setpgid
    val posix_procenv_uname : unit -> uct = ccall0 posix_procenv_uname
    val posix_procenv_uname_sysname : uct -> string = fn uct => Ccall(posix_procenv_uname_sysname,uct)
    val posix_procenv_uname_nodename : uct -> string = fn uct => Ccall(posix_procenv_uname_nodename,uct)
    val posix_procenv_uname_release : uct -> string = fn uct => Ccall(posix_procenv_uname_release,uct)
    val posix_procenv_uname_version : uct -> string = fn uct => Ccall(posix_procenv_uname_version,uct)
    val posix_procenv_uname_machine : uct -> string = fn uct => Ccall(posix_procenv_uname_machine,uct)
    val posix_procenv_uname_free : uct -> unit = fn uct => Ccall(posix_procenv_uname_free,uct)
    val posix_procenv_time : unit -> int = ccall0 posix_procenv_time
    val times' : unit -> timesrep = ccall0 posix_procenv_times
    val getenv : string -> string option = fn s => Ccall(posix_procenv_getenv,s)
    val posix_procenv_environ_size : unit -> int = fn () => Ccall(posix_procenv_environ_size,())
    val posix_procenv_environ_nth : int -> string = fn i => Ccall(posix_procenv_environ_nth,i)
    val ctermid : unit -> string = ccall0 posix_procenv_ctermid
    val ttyname' : int -> string = ccall1 posix_procenv_ttyname
    val isatty' : int -> bool = fn fd => Ccall(posix_procenv_isatty,fd)

    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32

    structure FS = POSIX_FileSys
    structure P  = POSIX_Process


    type pid = P.pid
    type uid = PrePosix.uid
    type gid = PrePosix.gid
    type file_desc = FS.file_desc

    type s_int = SysInt.int

    val uidToWord = PrePosix.uidToWord
    val wordToUid = PrePosix.wordToUid

    val gidToWord = PrePosix.gidToWord
    val wordToGid = PrePosix.wordToGid


    fun getpid () = P.wordToPid(int32touint32(posix_procenv_getpid()))
    fun getppid () = P.wordToPid(int32touint32(posix_procenv_getpid()))

    fun getuid () = wordToUid(posix_procenv_getuid())
    fun geteuid () = wordToUid(posix_procenv_geteuid())
    fun getgid () = wordToGid(posix_procenv_getgid())
    fun getegid () = wordToGid(posix_procenv_getegid())

    fun setuid uid = posix_procenv_setuid(uidToWord uid)
    fun setgid gid = posix_procenv_setgid(gidToWord gid)

    fun getgroups () =
	let val uct = posix_procenv_getgroups()
	    val size = posix_procenv_getgroups_size uct
	    fun nth n = wordToGid(posix_procenv_getgroups_nth(uct,n))
	    val grouplist = List.tabulate(size,nth)
	    val () = posix_procenv_getgroups_free uct
	in  grouplist
	end

    fun getpgrp () = P.wordToPid(int32touint32(posix_procenv_getpgrp()))
    fun setsid () = P.wordToPid(int32touint32(posix_procenv_setsid()))
    fun setpgid {pid : pid option, pgid : pid option} = let
          fun cvt NONE = 0
            | cvt (SOME(pid)) = uint32toint32(P.pidToWord pid)
          in
            posix_procenv_setpgid(cvt pid, cvt pgid)
          end

    fun uname () : (string * string) list =
	let val uct = posix_procenv_uname()
	    val r = [
		("sysname", posix_procenv_uname_sysname uct),
		("nodename", posix_procenv_uname_nodename uct),
		("release", posix_procenv_uname_release uct),
		("version", posix_procenv_uname_version uct),
		("machine", posix_procenv_uname_machine uct)
	    ]
	    val () = posix_procenv_uname_free uct
	in  r
	end

    val sysconf = PrePosix.sysconf

    fun time () = Time.fromSeconds(posix_procenv_time())

      (* times in clock ticks *)
    val ticksPerSec = Real.fromInt (SysWord.toIntX (sysconf "CLK_TCK"))
    fun times () = let
          fun cvt ticks = Time.fromReal ((Real.fromInt ticks)/ticksPerSec)
          val (e,u,s,cu,cs) = times' ()
          in
            { elapsed = cvt e,
              utime = cvt u,
              stime = cvt s,
              cutime = cvt cu,
              cstime = cvt cs }
          end

    fun environ () : string list =
	let val n = posix_procenv_environ_size()
	in  List.tabulate(n,posix_procenv_environ_nth)
	end

    fun ttyname fd = ttyname' (uint32toint32(FS.fdToWord fd))

    fun isatty fd = isatty' (uint32toint32(FS.fdToWord fd))

end
