(*$import Word32 PreOS POSIX_extern List Time POSIX_FILE_SYS *)
(* posix-filesys.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 file system operations
 *
 *)

structure POSIX_FileSys :> POSIX_FILE_SYS =
  struct
    val ++ = Word.orb
    val & = Word.andb
    infix ++ &

    fun w_osval (str : string) : int = Ccall(posix_filesys_num,str)

    datatype uid = UID of word
    datatype gid = GID of word

    fun uidToWord (UID i) = i
    fun wordToUid i = UID i

    fun gidToWord (GID i) = i
    fun wordToGid i = GID i

    datatype file_desc = FD of {fd : int}
    fun intOf (FD{fd,...}) = fd
    fun fd fd = FD{fd=fd}
    fun fdToWord (FD{fd,...}) = Word.fromInt fd
    fun wordToFD fd = FD{fd = Word.toInt fd}

  (* conversions between OS.IO.iodesc values and Posix file descriptors. *)
    fun fdToIOD (FD{fd,...}) = PreOS.IO.IODesc fd
    fun iodToFD (PreOS.IO.IODesc fd) = SOME(FD{fd = fd})

    val o_rdonly = w_osval "O_RDONLY"
    val o_wronly = w_osval "O_WRONLY"
    val o_rdwr = w_osval "O_RDWR"

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
    fun omodeFromWord omode =
          if omode = o_rdonly then O_RDONLY
          else if omode = o_wronly then O_WRONLY
          else if omode = o_rdwr then O_RDWR
          else raise Fail ("POSIX_FileSys.omodeFromWord: unknown mode "^
                                  (Word32.toString omode))

    fun omodeToWord O_RDONLY = o_rdonly
      | omodeToWord O_WRONLY = o_wronly
      | omodeToWord O_RDWR = o_rdwr

    fun uidToWord (UID i) = i
    fun wordToUid i = UID i
    fun gidToWord (GID i) = i
    fun wordToGid i = GID i

    type c_dirstream = int   (* the underlying C DIRSTREAM - which is a DIR pointer *)

    datatype dirstream = DS of {
	dirStrm : c_dirstream,
	isOpen : bool ref
      }

    val opendir' (* : string -> c_dirstream  *) = fn arg => Ccall(posix_filesys_opendir,arg)
    val readdir' (* : c_dirstream -> string  *) = fn arg => Ccall(posix_filesys_readdir,arg)
    val rewinddir' (* : c_dirstream -> unit  *) = fn arg => Ccall(posix_filesys_rewinddir,arg)
    val closedir' (* : c_dirstream -> unit   *) = fn arg => Ccall(posix_filesys_closedir,arg)
    fun opendir path = DS{
	    dirStrm = opendir' path,
	    isOpen = ref true
	  }
    fun readdir (DS{dirStrm, isOpen = ref false}) =
	  raise Fail "readdir on closed directory stream"
	  (* PreOS.error "readdir on closed directory stream" ??? *)
      | readdir (DS{dirStrm, ...}) = readdir' dirStrm
    fun rewinddir (DS{dirStrm, isOpen = ref false}) =
	  raise Fail "rewinddir on closed directory stream"
	  (* PreOS.error "rewinddir on closed directory stream" ??? *)
      | rewinddir (DS{dirStrm, ...}) = rewinddir' dirStrm
    fun closedir (DS{dirStrm, isOpen = ref false}) = ()
      | closedir (DS{dirStrm, isOpen}) = (
	  isOpen := false;
	  closedir' dirStrm)

    fun chdir  (s : string) : unit = Ccall(posix_filesys_chdir, s)
    fun getcwd () : string = Ccall(posix_filesys_getcwd,())

    val stdin  = fd 0
    val stdout = fd 1
    val stderr = fd 2

    structure S =
      struct
        datatype flags = MODE of word
        type mode = flags

        fun wordTo w = MODE w
        fun toWord (MODE w) = w

        fun flags ms = MODE(List.foldl (fn (MODE m,acc) => m ++ acc) 0w0 ms)
        fun anySet (MODE m, MODE m') = (m & m') <> 0w0
        fun allSet (MODE m, MODE m') = (m & m') = m
 
        val irwxu = MODE(w_osval "irwxu")
        val irusr = MODE(w_osval "irusr")
        val iwusr = MODE(w_osval "iwusr")
        val ixusr = MODE(w_osval "ixusr")
        val irwxg = MODE(w_osval "irwxg")
        val irgrp = MODE(w_osval "irgrp")
        val iwgrp = MODE(w_osval "iwgrp")
        val ixgrp = MODE(w_osval "ixgrp")
        val irwxo = MODE(w_osval "irwxo")
        val iroth = MODE(w_osval "iroth")
        val iwoth = MODE(w_osval "iwoth")
        val ixoth = MODE(w_osval "ixoth")
        val isuid = MODE(w_osval "isuid")
        val isgid = MODE(w_osval "isgid")

      end

    structure O =
      struct
        datatype flags = OFL of word

        fun wordTo w = OFL w
        fun toWord (OFL w) = w

        fun flags ms = OFL(List.foldl (fn (OFL m,acc) => m ++ acc) 0w0 ms)
        fun anySet (OFL m, OFL m') = (m & m') <> 0w0
        fun allSet (OFL m, OFL m') = (m & m') = m

        val append   = OFL(w_osval "O_APPEND")
        val dsync    = OFL(w_osval "O_DSYNC")
        val excl     = OFL(w_osval "O_EXCL")
        val noctty   = OFL(w_osval "O_NOCTTY")
        val nonblock = OFL(w_osval "O_NONBLOCK")
        val rsync    = OFL(w_osval "O_RSYNC")
        val sync     = OFL(w_osval "O_SYNC")
        val o_trunc  = w_osval "O_TRUNC"
        val trunc    = OFL o_trunc
        val o_creat  = w_osval "O_CREAT"
        val crflags  = o_wronly ++ o_creat ++ o_trunc

      end


    fun openf (fname : string, omode, O.OFL flags) =
          fd(Ccall (posix_filesys_openf,fname, flags ++ (omodeToWord omode), 0w0))
    fun createf (fname:string, omode, O.OFL oflags, S.MODE mode) = let
          val flags = O.o_creat ++ oflags ++ (omodeToWord omode)
          in
            fd(Ccall(posix_filesys_openf,fname, flags, mode))
          end
    fun creat (fname:string, S.MODE mode) =
          fd(Ccall(posix_filesys_openf,fname, O.crflags, mode))

    fun umask (S.MODE mode) = S.MODE(Ccall(posix_filesys_umask, mode))

    fun link {old : string, new : string} = Ccall(posix_filesys_link,old,new)
    fun rename {old : string, new : string} = Ccall(posix_filesys_rename,old,new)
    fun symlink {old : string, new : string} = Ccall(posix_filesys_symlink,old,new)
    fun mkdir (dirname : string, S.MODE mode) = Ccall(posix_filesys_mkdir,dirname,mode)
    fun mkfifo (name : string, S.MODE mode) = Ccall(posix_filesys_mkfifo,name,mode)

    fun unlink name = Ccall(posix_filesys_unlink,name)
    fun rmdir name = Ccall(posix_filesys_rmdir,name)
    fun readlink name = Ccall(posix_filesys_readlink,name)
    fun ftruncate (FD{fd,...}, len) = Ccall(posix_filesys_ftruncate,fd, len)

    datatype dev = DEV of word
    fun devToWord (DEV i) = i
    fun wordToDev i = DEV i

    datatype ino = INO of word
    fun inoToWord (INO i) = i
    fun wordToIno i = INO i

    structure ST =
      struct
        datatype stat = ST of {
                 ftype : int,
                 mode  : S.mode,
                 ino   : ino,
                 dev   : dev,
                 nlink : int,
                 uid   : uid,
                 gid   : gid,
                 size  : Position.int,
                 atime : Time.time,
                 mtime : Time.time,
                 ctime : Time.time
               }
      (* The following assumes the C stat functions pull the
       * file type from the mode field and return the
       * integer below corresponding to the file type.
       *) 
	fun isDir  (ST{ftype, ...}) = (ftype = 0x4000)
	fun isChr  (ST{ftype, ...}) = (ftype = 0x2000)
	fun isBlk  (ST{ftype, ...}) = (ftype = 0x6000)
	fun isReg  (ST{ftype, ...}) = (ftype = 0x8000)
	fun isFIFO (ST{ftype, ...}) = (ftype = 0x1000)
	fun isLink (ST{ftype, ...}) = (ftype = 0xA000)
	fun isSock (ST{ftype, ...}) = (ftype = 0xC000)

        fun mode (ST{mode,...}) = mode
        fun ino (ST{ino,...}) = ino
        fun dev (ST{dev,...}) = dev
        fun nlink (ST{nlink,...}) = nlink
        fun uid (ST{uid,...}) = uid
        fun gid (ST{gid,...}) = gid
        fun size (ST{size,...}) = size
        fun atime (ST{atime,...}) = atime
        fun mtime (ST{mtime,...}) = mtime
        fun ctime (ST{ctime,...}) = ctime
      end (* structure ST *) 

  (* this layout needs to track c-libs/posix-filesys/stat.c *)
    type statrep = (int * word * word * word * word * word * 
                    word * Position.int * int * int * int)
    fun mkStat (sr : statrep) = ST.ST{
	    ftype = #1 sr,
            mode = S.MODE (#2 sr),
            ino = INO (#3 sr),
            dev = DEV (#4 sr),
            nlink = SysWord.toInt(#5 sr),	(* probably should be an int in
						 * the run-time too.
						 *)
            uid = UID(#6 sr),
            gid = GID(#7 sr),
            size = #8 sr,
            atime = Time.fromSeconds (#9 sr),
            mtime = Time.fromSeconds (#10 sr),
            ctime = Time.fromSeconds (#11 sr)
          }


    fun stat fname = mkStat (Ccall(posix_filesys_stat,fname))
    fun lstat fname = mkStat (Ccall(posix_filesys_lstat,fname)) (* POSIX 1003.1a *)
    fun fstat (FD{fd}) = mkStat (Ccall(posix_filesys_fstat,fd))

    datatype access_mode = A_READ | A_WRITE | A_EXEC
    val a_read = w_osval "A_READ"	(* R_OK *)
    val a_write = w_osval "A_WRITE"	(* W_OK *)
    val a_exec = w_osval "A_EXEC"	(* X_OK *)
    val a_file = w_osval "A_FILE"	(* F_OK *)
    fun amodeToWord [] = a_file
      | amodeToWord l = let
          fun amtoi (A_READ,v) = a_read ++ v
            | amtoi (A_WRITE,v) = a_write ++ v
            | amtoi (A_EXEC,v) = a_exec ++ v
          in
            List.foldl amtoi a_file l
          end

    fun access (fname, aml) = Ccall(posix_filesys_access,fname, amodeToWord aml)
    fun chmod (fname, S.MODE m) = Ccall(posix_filesys_chmod,fname, m)
    fun fchmod (FD{fd}, S.MODE m) = Ccall(posix_filesys_fchmod,fd, m)
    fun chown (fname, UID uid, GID gid) = Ccall(posix_filesys_chown,fname, uid, gid)
    fun fchown (fd, UID uid, GID gid) = Ccall(posix_filesys_fchown,intOf fd, uid, gid)

    fun utime (file, NONE) = Ccall(posix_filesys_utime,file, ~1, 0)
      | utime (file, SOME{actime, modtime}) = let
          val atime = Time.toSeconds actime
          val mtime = Time.toSeconds modtime
          in
            Ccall(posix_filesys_utime,file,atime,mtime)
          end
    
(* xxxx can't get option in basis 
    val pathconf  : (string * string) -> word option = cfun "pathconf"
    val fpathconf'  : (int * string) -> word option = cfun "fpathconf"
*)
    fun pathconf  (str1, str2) : word option = 
	(case Ccall(posix_filesys_pathconf,str1,str2) of
	    (0,i) => SOME (int32touint32 i)
	  | _ => NONE)
    fun fpathconf  (FD{fd}, s : string) : word option = 
	(case Ccall(posix_filesys_fpathconf,fd,s) of
	    (0,i) => SOME (int32touint32 i)
	  | _ => NONE)

  end (* structure POSIX_FileSys *)
