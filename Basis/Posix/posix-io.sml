(*$import Prelude PrePosix SysInt Int Position Word8Vector Word8Array List POSIX_IO POSIX_FileSys POSIX_Process SysWord POSIX_extern String *)
(* posix-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Structure for POSIX 1003.1 primitive I/O operations
 *
 *)

structure POSIX_IO :> POSIX_IO where type open_mode = PrePosix.open_mode
				 and type file_desc = POSIX_FileSys.file_desc
				 and type O.flags = POSIX_FileSys.O.flags =

  struct
      
    val int32touint32 = TiltPrim.int32touint32
    val uint32toint32 = TiltPrim.uint32toint32
	
    val unsafe_vector2array = TiltPrim.unsafe_vector2array
    val op^ = String.^

    structure FS = POSIX_FileSys

    datatype open_mode = datatype PrePosix.open_mode

    type word = SysWord.word
    type s_int = SysInt.int

    val ++ = SysWord.orb
    val & = SysWord.andb
    infix ++ &

    fun osval (s : string) : s_int = Ccall(posix_io_num,s)
    val w_osval = SysWord.fromInt o osval
    fun fail (fct,msg) = raise TiltExn.LibFail ("POSIX_IO."^fct^": "^msg)

    type file_desc = FS.file_desc
    type pid = POSIX_Process.pid
    
    fun fs_intof fd = uint32toint32(FS.fdToWord fd)
    fun fs_fd i = FS.wordToFD(int32touint32 i)

   fun pipe () = let
          val (ifd, ofd) = Ccall(posix_io_pipe,())
          in
            {infd = fs_fd ifd, outfd = fs_fd ofd}
          end

     fun dup fd = fs_fd(Ccall(posix_io_dup,fs_intof fd))
    fun dup2 {old, new} = Ccall(posix_io_dup2,fs_intof old, fs_intof new)

    fun close fd = Ccall(posix_io_close, fs_intof fd)

    fun read' (x: int, y : int) : Word8Vector.vector = Ccall(posix_io_read,x,y)
    fun readbuf' (x : int, b : Word8Array.array, y : int, z :  int) : int = Ccall(posix_io_readbuf,x,b,y,z)
    fun readArr (fd, {buf, i, sz=NONE}) = let
          val alen = Word8Array.length buf
          in
            if 0 <= i andalso i <= alen
              then readbuf'(fs_intof fd, buf, alen - i, i)
              else raise Subscript
          end
      | readArr (fd, {buf, i, sz=SOME sz}) = let
          val alen = Word8Array.length buf
          in
            if 0 <= i andalso 0 <= sz andalso i + sz <= alen
              then readbuf'(fs_intof fd, buf, sz, i)
              else raise Subscript
          end
    fun readVec (fd,cnt) = 
          if cnt < 0 then raise Subscript else read'(fs_intof fd, cnt)

    fun writearr' (x : int, v : Word8Array.array, y : int, z : int) : int = Ccall(posix_io_writebuf,x,v,y,z)
    fun writevec' (x : int, v : Word8Vector.vector, y : int, z : int) : int = writearr'(x,unsafe_vector2array v,
											y,z)
    fun writeArr (fd,{buf, i, sz=NONE}) = let
          val alen = Word8Array.length buf
          in
            if 0 <= i andalso i <= alen
              then writearr'(fs_intof fd, buf, alen-i, i)
              else raise Subscript
          end
      | writeArr (fd,{buf, i, sz=SOME sz}) = let
          val alen = Word8Array.length buf
          in
            if 0 <= i andalso 0 <= sz andalso i + sz <= alen
              then writearr'(fs_intof fd, buf, sz, i)
              else raise Subscript
          end
    
    fun writeVec (fd,{buf, i, sz=NONE}) = let
          val vlen = Word8Vector.length buf
          in
            if 0 <= i andalso i <= vlen
              then writevec'(fs_intof fd, buf, vlen-i, i)
              else raise Subscript
          end
      | writeVec (fd,{buf, i, sz=SOME sz}) = let
          val vlen = Word8Vector.length buf
          in
            if 0 <= i andalso 0 <= sz andalso i + sz <= vlen
              then writevec'(fs_intof fd, buf, sz, i)
              else raise Subscript
          end
    
    datatype whence = SEEK_SET | SEEK_CUR | SEEK_END
    val seek_set = osval "SEEK_SET"
    val seek_cur = osval "SEEK_CUR"
    val seek_end = osval "SEEK_END"
    fun whToWord SEEK_SET = seek_set
      | whToWord SEEK_CUR = seek_cur
      | whToWord SEEK_END = seek_end
    fun whFromWord wh =
          if wh = seek_set then SEEK_SET
          else if wh = seek_cur then SEEK_CUR
          else if wh = seek_end then SEEK_END
          else fail ("whFromWord","unknown whence "^(Int.toString wh))
    
    structure FD =
      struct
        datatype flags = FDF of word

        fun wordTo w = FDF w
        fun toWord (FDF w) = w

        fun flags ms = FDF(List.foldl (fn (FDF m,acc) => m ++ acc) 0w0 ms)
        fun anySet (FDF m, FDF m') = (m & m') <> 0w0
        fun allSet (FDF m, FDF m') = (m & m') = m

        val cloexec = FDF(w_osval "cloexec")
      end

    structure O = POSIX_FileSys.O
	
    fun fcntl_d (x : s_int, y : s_int) : s_int = Ccall(posix_io_fcntl_d,x,y)
    fun fcntl_gfd (x : s_int) : word = Ccall(posix_io_fcntl_gfd, x)
    fun fcntl_sfd (x : s_int, y : word) : unit = Ccall(posix_io_fcntl_sfd, x, y)
    fun fcntl_gfl (x : s_int) : (word * word) = Ccall(posix_io_fcntl_gfl, x)
    fun fcntl_sfl (x : s_int, y : word) : unit = Ccall(posix_io_fcntl_sfl, x, y)
    fun dupfd {old, base} = fs_fd (fcntl_d (fs_intof old, fs_intof base))
    fun getfd fd = FD.FDF (fcntl_gfd (fs_intof fd))
    fun setfd (fd, FD.FDF fl) = fcntl_sfd(fs_intof fd, fl)
    fun getfl fd = let
          val (sts, omode) = fcntl_gfl (fs_intof fd)
          in
            (O.wordTo sts, PrePosix.omodeFromWord omode)
          end
    fun setfl (fd, sts) = fcntl_sfl (fs_intof fd, O.toWord sts)

    datatype lock_type = F_RDLCK | F_WRLCK | F_UNLCK

    structure FLock =
      struct
        datatype flock = FLOCK of {
             l_type : lock_type,
             l_whence : whence,
             l_start : Position.int,
             l_len : Position.int,
             l_pid : pid option
           }

        fun flock fv = FLOCK fv
        fun ltype (FLOCK{l_type,...}) = l_type
        fun whence (FLOCK{l_whence,...}) = l_whence
        fun start (FLOCK{l_start,...}) = l_start
        fun len (FLOCK{l_len,...}) = l_len
        fun pid (FLOCK{l_pid,...}) = l_pid
      end

    type flock_rep = s_int * s_int * Position.int * Position.int * s_int

    fun fcntl_l (x : s_int, y : s_int, z : flock_rep) : flock_rep = Ccall(posix_io_fcntl_l, x, y, z)
    val f_getlk = osval "F_GETLK"
    val f_setlk = osval "F_SETLK"
    val f_setlkw = osval "F_SETLKW"
    val f_rdlck = osval "F_RDLCK"
    val f_wrlck = osval "F_WRLCK"
    val f_unlck = osval "F_UNLCK"

    fun flockToRep (FLock.FLOCK{l_type,l_whence,l_start,l_len,...}) = let
          fun ltypeOf F_RDLCK = f_rdlck
            | ltypeOf F_WRLCK = f_wrlck
            | ltypeOf F_UNLCK = f_unlck
          in
            (ltypeOf l_type,whToWord l_whence, l_start, l_len, 0)
          end
    fun flockFromRep (usepid,(ltype,whence,start,len,pid)) = let
          fun ltypeOf ltype = 
                if ltype = f_rdlck then F_RDLCK
                else if ltype = f_wrlck then F_WRLCK
                else if ltype = f_unlck then F_UNLCK
                else fail ("flockFromRep","unknown lock type "^(Int.toString ltype))
          in
            FLock.FLOCK { 
              l_type = ltypeOf ltype,
              l_whence = whFromWord whence,
              l_start = start,
              l_len = len,
              l_pid = if usepid then SOME(POSIX_Process.wordToPid(int32touint32 pid)) else NONE
            }
          end

    fun getlk (fd, flock) =
          flockFromRep(true,fcntl_l(fs_intof fd,f_getlk,flockToRep flock))
    fun setlk (fd, flock) =
          flockFromRep(false,fcntl_l(fs_intof fd,f_setlk,flockToRep flock))
    fun setlkw (fd, flock) =
          flockFromRep(false,fcntl_l(fs_intof fd,f_setlkw,flockToRep flock))

    fun lseek (fd,offset,whence) = Ccall(posix_io_lseek,fs_intof fd,offset, whToWord whence)
    fun fsync fd = Ccall(posix_io_fsync, fs_intof fd)

  end (* structure POSIX_IO *)

