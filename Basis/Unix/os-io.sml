(*$import Prelude PreTime List Word SysWord Int OS_IO PreOS Posix POSIX_extern *)
(* os-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this interface has been proposed, but not yet adopted by the
 * Standard basis committee.
 *
 *)

structure OS_IO :> OS_IO where type iodesc = PreOS.IO.iodesc 
			   and type poll_desc = PreOS.IO.poll_desc 
                           and type poll_info = PreOS.IO.poll_info = 
  struct

  (* an iodesc is an abstract descriptor for an OS object that
   * supports I/O (e.g., file, tty device, socket, ...).
   *)
    type iodesc = PreOS.IO.iodesc

    datatype iodesc_kind = K of string

  (* return a hash value for the I/O descriptor. *)
    fun hash (PreOS.IO.IODesc fd) = Word.fromInt fd

  (* compare two I/O descriptors *)
    fun compare (PreOS.IO.IODesc fd1, PreOS.IO.IODesc fd2) = Int.compare(fd1, fd2)

    structure Kind =
      struct
	val file = K "FILE"
	val dir = K "DIR"
	val symlink = K "LINK"
	val tty = K "TTY"
	val pipe = K "PIPE"
	val socket = K "SOCK"
	val device = K "DEV"
      end

  (* return the kind of I/O descriptor *)
    fun kind (PreOS.IO.IODesc fd) = let
	  val fd = Posix.FileSys.wordToFD(SysWord.fromInt fd)
	  val stat = Posix.FileSys.fstat fd
	  in
	    if      (Posix.FileSys.ST.isReg stat) then Kind.file
	    else if (Posix.FileSys.ST.isDir stat) then Kind.dir
	    else if (Posix.FileSys.ST.isChr stat) then Kind.tty
	    else if (Posix.FileSys.ST.isBlk stat) then Kind.device (* ?? *)
	    else if (Posix.FileSys.ST.isLink stat) then Kind.symlink
	    else if (Posix.FileSys.ST.isFIFO stat) then Kind.pipe
	    else if (Posix.FileSys.ST.isSock stat) then Kind.socket
	    else K "UNKNOWN"
	  end

    type poll_flags = {rd : bool, wr : bool, pri : bool}
    datatype poll_desc = PollDesc of (iodesc * poll_flags)
    datatype poll_info = PollInfo of (iodesc * poll_flags)

  (* create a polling operation on the given descriptor; note that
   * not all I/O devices support polling, but for the time being, we
   * don't test for this.
   *)
    fun pollDesc iod = SOME(PollDesc(iod, {rd=false, wr=false, pri=false}))

  (* return the I/O descriptor that is being polled *)
    fun pollToIODesc (PollDesc(iod, _)) = iod

    exception Poll

  (* set polling events; if the polling operation is not appropriate
   * for the underlying I/O device, then the Poll exception is raised.
   *)
    fun pollIn (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=true, wr=wr, pri=pri})
    fun pollOut (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=rd, wr=true, pri=pri})
    fun pollPri (PollDesc(iod, {rd, wr, pri})) =
	  PollDesc(iod, {rd=rd, wr=wr, pri=true})

  (* polling function *)
    local
(*      val poll' : ((int * word) list * (int * int) option) -> (int * word) list =
	    CInterface.c_function "POSIX-OS" "poll" *)
      fun join (false, _, w) = w
        | join (true, b, w) = Word.orb(w, b)
      fun test (w, b) = (Word.andb(w, b) <> 0w0)
      val rdBit = 0w1 and wrBit = 0w2 and priBit = 0w4
      fun fromPollDesc (PollDesc(PreOS.IO.IODesc fd, {rd, wr, pri})) =
	    ( fd,
	      join (rd, rdBit, join (wr, wrBit, join (pri, priBit, 0w0)))
	    )
      fun toPollInfo (fd, w) = PollInfo(PreOS.IO.IODesc fd, {
	      rd = test(w, rdBit), wr = test(w, wrBit), pri = test(w, priBit)
	    })
    in
    fun poll (pds, timeOut) = let
	  val timeOut = (case timeOut
		 of SOME(PreTime.TIME{sec, usec}) => SOME(sec, usec)
		  | NONE => NONE
		(* end case *))
	  val info = Ccall(posix_os_poll, List.map fromPollDesc pds, timeOut)
	  in
	    List.map toPollInfo info
	  end
    end (* local *)

  (* check for conditions *)
    fun isIn (PollInfo(_, flgs)) = #rd flgs
    fun isOut (PollInfo(_, flgs)) = #wr flgs
    fun isPri (PollInfo(_, flgs)) = #pri flgs
    fun infoToPollDesc  (PollInfo arg) = PollDesc arg

  end (* OS_IO *)


(*

 * $Log$
# Revision 1.4  2000/11/27  22:36:42  swasey
# *** empty log message ***
# 
 * Revision 1.3  2000/09/21 01:08:38  pscheng
 * *** empty log message ***
 *
# Revision 1.2  2000/09/12  18:55:02  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/03/09  19:54:17  pscheng
# added basis
#
 * Revision 1.3  1997/06/07  15:27:51  jhr
 *   SML'97 Basis Library changes (phase 3; Posix changes)
 *
 * Revision 1.2  1997/06/02  19:16:19  jhr
 *   SML'97 Basis Library changes (phase 2)
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
