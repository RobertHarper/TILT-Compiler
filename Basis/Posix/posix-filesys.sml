(* posix-filesys.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 file system operations
 *
 *)

structure POSIX_FileSys :> POSIX_FILE_SYS
	where type uid = PrePosix.uid
	where type gid = PrePosix.gid
	where type open_mode = PrePosix.open_mode =
struct

    val w_osval : string -> word = ccall1 posix_filesys_num
    val opendir' : string -> uct = ccall1 posix_filesys_opendir
    val readdir' : uct -> string = ccall1 posix_filesys_readdir
    val rewinddir' : uct -> unit = fn p => Ccall(posix_filesys_rewinddir,p)
    val closedir' : uct -> unit = ccall1 posix_filesys_closedir
    val chdir : string -> unit = ccall1 posix_filesys_chdir
    val getcwd : unit -> string = ccall0 posix_filesys_getcwd
    val posix_filesys_openf : string * word * word -> int = ccall3 posix_filesys_openf
    val posix_filesys_umask : word -> word = fn w => Ccall(posix_filesys_umask,w)
    val posix_filesys_link : string * string -> unit = ccall2 posix_filesys_link
    val posix_filesys_rename : string * string -> unit = ccall2 posix_filesys_rename
    val posix_filesys_symlink : string * string -> unit = ccall2 posix_filesys_symlink
    val posix_filesys_mkdir : string * word -> unit = ccall2 posix_filesys_mkdir
    val posix_filesys_mkfifo : string * word -> unit = ccall2 posix_filesys_mkfifo
    val unlink : string -> unit = ccall1 posix_filesys_unlink
    val rmdir : string -> unit = ccall1 posix_filesys_rmdir
    val readlink : string -> string = ccall1 posix_filesys_readlink
    val posix_filesys_ftruncate : int * int -> unit = ccall2 posix_filesys_ftruncate
    val posix_filesys_stat : string -> statrep = ccall1 posix_filesys_stat
    val posix_filesys_lstat : string -> statrep = ccall1 posix_filesys_lstat
    val posix_filesys_fstat : int -> statrep = ccall1 posix_filesys_fstat
    val posix_filesys_access : string * word -> bool = ccall2 posix_filesys_access
    val posix_filesys_chmod : string * word -> unit = ccall2 posix_filesys_chmod
    val posix_filesys_fchmod : int * word -> unit = ccall2 posix_filesys_fchmod
    val posix_filesys_chown : string * word * word -> unit = ccall3 posix_filesys_chown
    val posix_filesys_fchown : int * word * word -> unit = ccall3 posix_filesys_fchown
    val posix_filesys_utime : string * int * int -> unit = ccall3 posix_filesys_utime
    val posix_filesys_pathconf_name : string -> int = ccall1 posix_filesys_pathconf_name
    val posix_filesys_pathconf : string * int -> word = ccall2 posix_filesys_pathconf
    val posix_filesys_fpathconf : int * int -> word = ccall2 posix_filesys_fpathconf

    val int32touint32 = TiltPrim.int32touint32
    val op^ = String.^

    val ++ = Word.orb
    val & = Word.andb
    infix ++ &

    type uid = PrePosix.uid
    type gid = PrePosix.gid

    datatype file_desc = FD of {fd : int}
    fun intOf (FD{fd,...}) = fd
    fun fd fd = FD{fd=fd}
    fun fdToWord (FD{fd,...}) = Word.fromInt fd
    fun wordToFD fd = FD{fd = Word.toInt fd}

  (* conversions between OS.IO.iodesc values and Posix file descriptors. *)
    fun fdToIOD (FD{fd,...}) = PreOS.IO.IODesc fd
    fun iodToFD (PreOS.IO.IODesc fd) = SOME(FD{fd = fd})

    datatype open_mode = datatype PrePosix.open_mode

    datatype dirstream = DS of {
	dirStrm : uct,	(* DIR* *)
	isOpen : bool ref
      }

    fun opendir path = DS{
	    dirStrm = opendir' path,
	    isOpen = ref true
	  }
    fun readdir (DS{dirStrm, isOpen = ref false}) =
	  raise TiltExn.SysErr ("readdir on closed directory stream", NONE)
      | readdir (DS{dirStrm, ...}) = readdir' dirStrm
    fun rewinddir (DS{dirStrm, isOpen = ref false}) =
	  raise TiltExn.SysErr ("rewinddir on closed directory stream", NONE)
      | rewinddir (DS{dirStrm, ...}) = rewinddir' dirStrm
    fun closedir (DS{dirStrm, isOpen = ref false}) = ()
      | closedir (DS{dirStrm, isOpen}) = (
	  isOpen := false;
	  closedir' dirStrm)

    val stdin  = fd 0
    val stdout = fd 1
    val stderr = fd 2

    val o_wronly = PrePosix.omodeToWord(PrePosix.O_WRONLY)

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
          fd(posix_filesys_openf(fname, flags ++ (PrePosix.omodeToWord omode), 0w0))
    fun createf (fname:string, omode, O.OFL oflags, S.MODE mode) = let
          val flags = O.o_creat ++ oflags ++ (PrePosix.omodeToWord omode)
          in
            fd(posix_filesys_openf(fname, flags, mode))
          end
    fun creat (fname:string, S.MODE mode) =
          fd(posix_filesys_openf(fname, O.crflags, mode))

    fun umask (S.MODE mode) = S.MODE(posix_filesys_umask mode)

    fun link {old : string, new : string} = posix_filesys_link(old,new)
    fun rename {old : string, new : string} = posix_filesys_rename(old,new)
    fun symlink {old : string, new : string} = posix_filesys_symlink(old,new)
    fun mkdir (dirname : string, S.MODE mode) = posix_filesys_mkdir(dirname,mode)
    fun mkfifo (name : string, S.MODE mode) = posix_filesys_mkfifo(name,mode)

    fun ftruncate (FD{fd,...}, len) = posix_filesys_ftruncate(fd,len)

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
            uid = PrePosix.wordToUid(#6 sr),
            gid = PrePosix.wordToGid(#7 sr),
            size = #8 sr,
            atime = Time.fromSeconds (#9 sr),
            mtime = Time.fromSeconds (#10 sr),
            ctime = Time.fromSeconds (#11 sr)
          }


    fun stat fname = mkStat (posix_filesys_stat fname)
    fun lstat fname = mkStat (posix_filesys_lstat fname) (* POSIX 1003.1a *)
    fun fstat (FD{fd}) = mkStat (posix_filesys_fstat fd)

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

    fun access (fname, aml) = posix_filesys_access(fname, amodeToWord aml)
    fun chmod (fname, S.MODE m) = posix_filesys_chmod(fname,m)
    fun fchmod (FD{fd}, S.MODE m) = posix_filesys_fchmod(fd,m)
    fun chown (fname, uid, gid) = posix_filesys_chown(fname,PrePosix.uidToWord uid,PrePosix.gidToWord gid)
    fun fchown (fd, uid, gid) = posix_filesys_fchown(intOf fd,PrePosix.uidToWord uid,PrePosix.gidToWord gid)

    fun utime (file, NONE) = posix_filesys_utime(file,~1,0)
      | utime (file, SOME{actime, modtime}) = let
          val atime = Time.toSeconds actime
          val mtime = Time.toSeconds modtime
          in
            posix_filesys_utime(file,atime,mtime)
          end

    fun towordopt (r:word) : word option =
	if r = 0wxFFFFFFFF then NONE else SOME r

    fun pathconf (p:string, n:string) : word option =
	towordopt(posix_filesys_pathconf(p,posix_filesys_pathconf_name n))

    fun fpathconf  (FD{fd}, n : string) : word option =
	towordopt(posix_filesys_fpathconf(fd,posix_filesys_pathconf_name n))

end
