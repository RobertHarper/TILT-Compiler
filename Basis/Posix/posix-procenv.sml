(*$import Prelude Time Real64 Int List POSIX_FileSys POSIX_Process Word32 POSIX_PROC_ENV POSIX_Process POSIX_extern *)
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
	and   type uid = POSIX_FileSys.uid 
	and   type gid = POSIX_FileSys.gid =
  struct

    structure FS = POSIX_FileSys
    structure P  = POSIX_Process


    type pid = P.pid
    type uid = FS.uid
    type gid = FS.gid
    type file_desc = FS.file_desc

    type s_int = SysInt.int

    val uidToWord = FS.uidToWord
    val wordToUid = FS.wordToUid

    val gidToWord = FS.gidToWord
    val wordToGid = FS.wordToGid


    fun getpid () = P.wordToPid(int32touint32(Ccall(posix_procenv_getpid,())))
    fun getppid () = P.wordToPid(int32touint32(Ccall(posix_procenv_getpid,())))

    fun getuid () = wordToUid(Ccall(posix_procenv_getuid,()))
    fun geteuid () = wordToUid(Ccall(posix_procenv_geteuid,()))
    fun getgid () = wordToGid(Ccall(posix_procenv_getgid,()))
    fun getegid () = wordToGid(Ccall(posix_procenv_getegid,()))

    fun setuid uid = Ccall(posix_procenv_setuid,uidToWord uid)
    fun setgid gid = Ccall(posix_procenv_setgid, gidToWord gid)

    fun getgroups () = List.map wordToGid (Ccall(posix_procenv_getgroups,()))
    fun getlogin () : string = Ccall(posix_procenv_getlogin, ())

    fun getpgrp () = P.wordToPid(int32touint32(Ccall(posix_procenv_getpgrp, ())))
    fun setsid () = P.wordToPid(int32touint32(Ccall(posix_procenv_setsid, ())))
    fun setpgid {pid : pid option, pgid : pid option} = let
          fun cvt NONE = 0
            | cvt (SOME(pid)) = uint32toint32(P.pidToWord pid)
          in
            Ccall(posix_procenv_setpgid,cvt pid, cvt pgid)
          end

    fun uname () : (string * string) list = Ccall(posix_procenv_uname, ())

    val sysconf = P.sysconf

    fun time () = Time.fromSeconds(Ccall(posix_procenv_time, ()))

      (* times in clock ticks *)
    fun times' () :  int * int * int * int * int = Ccall(posix_procenv_times, ())
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

    fun getenv  (s : string) :  string option = Ccall(posix_procenv_getenv, s)
    fun environ () :  string list = Ccall(posix_procenv_environ, ())

    fun ctermid () : string = Ccall(posix_procenv_ctermid, ())

    fun ttyname' (x : s_int) : string = Ccall(posix_procenv_ttyname, x)
    fun ttyname fd = ttyname' (uint32toint32(FS.fdToWord fd))

    fun isatty' (x : s_int) : bool = Ccall(posix_procenv_isatty, x)
    fun isatty fd = isatty' (uint32toint32(FS.fdToWord fd))

  end (* structure POSIX_Proc_Env *)

(*
 * $Log$
# Revision 1.3  2000/09/12  18:54:40  swasey
# Changes for cutoff compilation
# 
 * Revision 1.2  1999/09/22 15:45:12  pscheng
 * *** empty log message ***
 *
# Revision 1.1  1998/03/09  19:53:32  pscheng
# added basis
#
 * Revision 1.1.1.1  1997/01/14  01:38:23  george
 *   Version 109.24
 *
 *)
