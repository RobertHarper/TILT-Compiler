(* pre-os.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This the OS structure(s) with only types, so that the signatures can compile.
 *
 *)

structure PreOS =
  struct
    type syserror = int                     (* the integer code; we may need to beef this up *)

    exception SysErr = TiltExn.SysErr


    structure Process =
      struct
	type status = int (* should this be Word8.word ?*)
      end

    structure IO =
      struct
	datatype iodesc = IODesc of int
(** This probably should be
	datatype iodesc = IODesc of Posix.FileSys.file_desc
 **)
	type poll_flags = {rd : bool, wr : bool, pri : bool}
	datatype poll_desc = PollDesc of (iodesc * poll_flags)
	datatype poll_info = PollInfo of (iodesc * poll_flags)
      end

  end;




