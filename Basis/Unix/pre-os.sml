(*$import Prelude *)
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

(*    exception SysErr of (string * syserror option) (* = Assembly.SysErr *) *)


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
      end

  end;




(*
 * $Log$
# Revision 1.1  98/03/09  19:54:31  pscheng
# added basis
# 
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
