(*$import Prelude POSIX_FLAGS Word8Vector Word8Array Int32 *)
(* posix-io-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Signature for POSIX 1003.1 primitive I/O operations
 *
 *)

signature POSIX_IO =
  sig

    eqtype file_desc
    eqtype pid
    
    val pipe : unit -> {infd : file_desc, outfd : file_desc}
    val dup : file_desc -> file_desc
    val dup2 : {old : file_desc, new : file_desc} -> unit
    val close : file_desc -> unit
    val readVec : (file_desc * int) -> Word8Vector.vector
    val readArr : (file_desc * 
                   {buf : Word8Array.array, i : int, sz : int option}) -> int
    val writeVec : (file_desc * 
                    {buf : Word8Vector.vector, i : int, sz : int option}) -> int
    val writeArr : (file_desc * 
                    {buf : Word8Array.array, i : int, sz : int option}) -> int
    
    datatype whence = SEEK_SET | SEEK_CUR | SEEK_END
    
    structure FD :
      sig
        include POSIX_FLAGS

        val cloexec : flags

      end

    structure O :
      sig
        include POSIX_FLAGS

        val append   : flags
        val dsync    : flags
        val nonblock : flags
        val rsync    : flags
        val sync     : flags

      end

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
    
    val dupfd : {old : file_desc, base : file_desc} -> file_desc
    val getfd : file_desc -> FD.flags
    val setfd : (file_desc * FD.flags) -> unit
    val getfl : file_desc -> (O.flags * open_mode)
    val setfl : (file_desc * O.flags) -> unit
    
    datatype lock_type = F_RDLCK | F_WRLCK | F_UNLCK

    structure FLock :
      sig
        type flock

        val flock : { l_type : lock_type,
                      l_whence : whence,
                      l_start : Position.int,
                      l_len : Position.int,
                      l_pid : pid option} -> flock

        val ltype    : flock -> lock_type
        val whence   : flock -> whence
        val start    : flock -> Position.int
        val len      : flock -> Position.int
        val pid      : flock -> pid option
      end

    val getlk  : (file_desc * FLock.flock) -> FLock.flock
    val setlk  : (file_desc * FLock.flock) -> FLock.flock
    val setlkw : (file_desc * FLock.flock) -> FLock.flock
    
    val lseek : (file_desc * Position.int * whence) -> Position.int

    val fsync : file_desc -> unit

  end (* signature POSIX_IO *)

(*
 * $Log$
# Revision 1.1  98/03/09  19:53:22  pscheng
# added basis
# 
 * Revision 1.3  1997/06/07  15:27:40  jhr
 *   SML'97 Basis Library changes (phase 3; Posix changes)
 *
 * Revision 1.2  1997/05/20  12:15:25  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:22  george
 *   Version 109.24
 *
 *)
