(*$import Prelude PrePosix SysWord POSIX_SYS_DB POSIX_extern *)
(* posix-sysdb.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 system data-base operations
 *
 *)

structure POSIX_Sys_DB :> POSIX_SYS_DB 
    where type uid = PrePosix.uid
      and type gid = PrePosix.gid =
  struct

    type word = SysWord.word
    type uid = PrePosix.uid
    type gid = PrePosix.gid
    
    structure Passwd =
      struct
        datatype passwd = PWD of {             (* extensible *)
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
        datatype group = GROUP of {              (* extensible *)
             name : string,
             gid : gid,
             members : string list
           }

        fun name (GROUP{name,...}) = name
        fun gid (GROUP{gid,...}) = gid
        fun members (GROUP{members,...}) = members
    
      end
    
    fun getgrgid gid = let val gid = PrePosix.gidToWord gid
          val (name,gid,members) = Ccall(posix_sysdb_getgrgid, gid)
          in
            Group.GROUP { name = name,
              gid = PrePosix.wordToGid gid,
              members = members
            }
          end
    fun getgrnam gname = let
          val (name,gid,members) = Ccall(posix_sysdb_getgrnam, gname)
          in
            Group.GROUP { name = name,
              gid = PrePosix.wordToGid gid,
              members = members
            }
          end

    fun getpwuid uid = let val uid = PrePosix.uidToWord uid
          val (name,uid,gid,dir,shell) = Ccall(posix_sysdb_getpwuid, uid)
          in
            Passwd.PWD { name = name,
              uid = PrePosix.wordToUid uid,
              gid = PrePosix.wordToGid gid,
              home = dir,
              shell = shell
            }
          end
    fun getpwnam name = let
          val (name,uid,gid,dir,shell) = Ccall(posix_sysdb_getpwnam, name)
          in
            Passwd.PWD { name = name,
              uid = PrePosix.wordToUid uid,
              gid = PrePosix.wordToGid gid,
              home = dir,
              shell = shell
            }
          end

  end (* structure POSIX_Sys_DB *)

