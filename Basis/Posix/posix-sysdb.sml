(* posix-sysdb.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 system data-base operations
 *
 *)

structure POSIX_Sys_DB :> POSIX_SYS_DB
	where type uid = PrePosix.uid
	where type gid = PrePosix.gid =
struct

    val posix_sysdb_getgrgid : word -> uct = ccall1 posix_sysdb_getgrgid
    val posix_sysdb_getgrnam : string -> uct = ccall1 posix_sysdb_getgrnam
    val posix_sysdb_gr_name : uct -> string = fn uct => Ccall(posix_sysdb_gr_name,uct)
    val posix_sysdb_gr_gid : uct -> word = fn uct => Ccall(posix_sysdb_gr_gid,uct)
    val posix_sysdb_gr_mem_size : uct -> int = fn uct => Ccall(posix_sysdb_gr_mem_size,uct)
    val posix_sysdb_gr_mem_nth : uct * int -> string = fn (uct,n) => Ccall(posix_sysdb_gr_mem_nth,uct,n)
    val posix_sysdb_gr_free : uct -> unit = fn uct => Ccall(posix_sysdb_gr_free,uct)
    val posix_sysdb_getpwuid : word -> uct = ccall1 posix_sysdb_getpwuid
    val posix_sysdb_getpwnam : string -> uct = ccall1 posix_sysdb_getpwnam
    val posix_sysdb_pw_name : uct -> string = fn uct => Ccall(posix_sysdb_pw_name,uct)
    val posix_sysdb_pw_uid_gid : uct -> word * word = fn uct => Ccall(posix_sysdb_pw_uid_gid,uct)
    val posix_sysdb_pw_dir : uct -> string = fn uct => Ccall(posix_sysdb_pw_dir,uct)
    val posix_sysdb_pw_shell : uct -> string = fn uct => Ccall(posix_sysdb_pw_shell,uct)
    val posix_sysdb_pw_free : uct -> unit = fn uct => Ccall(posix_sysdb_pw_free,uct)

    type word = SysWord.word
    type uid = PrePosix.uid
    type gid = PrePosix.gid

    structure Passwd =
      struct
        datatype passwd = PWD of {
             name : string,
             uid : uid,
             gid : gid,
             home : string,
             shell : string
           }

        fun name (PWD{name,...}) = name
        fun uid (PWD{uid,...}) = uid
        fun gid (PWD{gid,...}) = gid
        fun home (PWD{home,...}) = home
        fun shell (PWD{shell,...}) = shell

      end

    structure Group =
      struct
        datatype group = GROUP of {
             name : string,
             gid : gid,
             members : string list
           }

        fun name (GROUP{name,...}) = name
        fun gid (GROUP{gid,...}) = gid
        fun members (GROUP{members,...}) = members

      end

    fun uct2grp (uct:uct) : Group.group =
	let val name = posix_sysdb_gr_name uct
	    val gid = posix_sysdb_gr_gid uct
	    val n = posix_sysdb_gr_mem_size uct
	    fun ith_mem i = posix_sysdb_gr_mem_nth(uct,i)
	    val members = List.tabulate(n,ith_mem)
	    val () = posix_sysdb_gr_free uct
	in  Group.GROUP {
		name = name,
		gid = PrePosix.wordToGid gid,
		members = members
	    }
	end

    fun getgrgid gid = uct2grp(posix_sysdb_getgrgid(PrePosix.gidToWord gid))

    fun getgrnam gname = uct2grp(posix_sysdb_getgrnam gname)

    fun uct2pw (uct:uct) : Passwd.passwd =
	let val name = posix_sysdb_pw_name uct
	    val (uid,gid) = posix_sysdb_pw_uid_gid uct
	    val dir = posix_sysdb_pw_dir uct
	    val shell = posix_sysdb_pw_shell uct
	    val () = posix_sysdb_pw_free uct
	in  Passwd.PWD {
		name = name,
		uid = PrePosix.wordToUid uid,
		gid = PrePosix.wordToGid gid,
		home = dir,
		shell = shell
	    }
	end

    fun getpwuid uid = uct2pw(posix_sysdb_getpwuid(PrePosix.uidToWord uid))

    fun getpwnam name = uct2pw(posix_sysdb_getpwnam name)

end
