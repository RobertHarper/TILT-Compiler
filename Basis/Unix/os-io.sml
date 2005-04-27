(* os-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * NOTE: this interface has been proposed, but not yet adopted by the
 * Standard basis committee.
 *
 *)

structure OS_IO :> OS_IO
	where type iodesc = PreOS.IO.iodesc
	where type poll_desc = PreOS.IO.poll_desc
	where type poll_info = PreOS.IO.poll_info =
struct

    val os_io_poll : int * (int * word) list * (int * int) option -> uct = ccall3 os_io_poll
    val os_io_poll_nth : uct * int -> int * word = fn (p,n) => Ccall(os_io_poll_nth,p,n)
    val os_io_poll_free : uct -> unit = fn p => Ccall(os_io_poll_free,p)

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
    datatype poll_desc = datatype PreOS.IO.poll_desc       (* PollDesc of (iodesc * poll_flags) *)
    datatype poll_info = datatype PreOS.IO.poll_info       (* PollInfo of (iodesc * poll_flags) *)

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
      fun join (false, _, w) = w
        | join (true, b, w) = Word.orb(w, b)
      fun test (w, b) = (Word.andb(w, b) <> 0w0)
      val rdBit = 0w1 and wrBit = 0w2 and priBit = 0w4
      fun fromWord (w:word) : poll_flags =
	 {rd=test(w,rdBit), wr=test(w,wrBit), pri=test(w,priBit)}
      fun toWord ({rd,wr,pri} : poll_flags) : word =
	 join (rd, rdBit, join (wr, wrBit, join (pri, priBit, 0w0)))
      fun toEvent (PollDesc(PreOS.IO.IODesc fd, flags)) = (fd, toWord flags)
      fun fromEvent (fd:int, w:word) = PollInfo(PreOS.IO.IODesc fd, fromWord w)
    in
    fun poll (pds, timeOut) =
	let
	  val timeOut = (case timeOut
		 of SOME t =>
		     let val sec = Time.toSeconds t
			 val t' = Time.-(t, Time.fromSeconds sec)
			 val usec = Time.toMicroseconds t'
		     in  SOME(sec, usec)
		     end
		  | NONE => NONE
		(* end case *))
	  val nevents = List.length pds
	  val events = List.map toEvent pds
          val uct = os_io_poll(nevents,events,timeOut)
	  val revents = List.tabulate(nevents, fn n => os_io_poll_nth(uct,n))
	  val () = os_io_poll_free uct
	in  List.map fromEvent revents
	end
    end (* local *)

  (* check for conditions *)
    fun isIn (PollInfo(_, flgs)) = #rd flgs
    fun isOut (PollInfo(_, flgs)) = #wr flgs
    fun isPri (PollInfo(_, flgs)) = #pri flgs
    fun infoToPollDesc  (PollInfo arg) = PollDesc arg

end
