(* posix-text-prim-io.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This implements the UNIX version of the OS specific text primitive
 * IO structure.  It is implemented by a trivial translation of the
 * binary operations (see posix-bin-prim-io.sml).
 *
 *)

structure PosixTextPrimIO = PosixPrimIO(structure PrimIO = TextPrimIO)

structure PosixTextPrimIO : sig

    include OS_PRIM_IO
      where PrimIO = TextPrimIO
      where type file_desc = Posix.ProcEnv.file_desc

    val stdIn  : unit -> PrimIO.reader
    val stdOut : unit -> PrimIO.writer
    val stdErr : unit -> PrimIO.writer

    val strReader : string -> PrimIO.reader

  end
  = struct

    structure PF = Posix.FileSys
    structure BinPrimIO = PosixBinPrimIO
    structure PrimIO = TextPrimIO

    type file_desc = PF.file_desc

    open PosixTextPrimIO

    fun stdIn () = mkReader{
	    fd		= PF.stdin,
	    name	= "<stdIn>",
	    initBlkMode	= true (* Bug!  Should check! *)
	  }

    fun stdOut () = mkWriter{
	    fd		= PF.stdout,
	    name	= "<stdOut>",
	    initBlkMode	= true (* Bug!  Should check! *),
	    appendMode	= false (* Bug!  Should check! *),
	    chunkSize	= bufferSzB
	  }

    fun stdErr () = mkWriter{
	    fd		= PF.stderr,
	    name	= "<stdErr>",
	    initBlkMode	= true, (* Bug!  Should check! *)
	    appendMode	= false, (* Bug!  Should check! *)
	    chunkSize	= bufferSzB
	  }

    fun strReader src = let
	  val pos = ref 0
	  val closed = ref false
	  fun checkClosed () = if !closed then raise IO.ClosedStream else ()
	  val len = String.size src
	  fun avail () = (len - !pos)
	  fun readV n = let
		val p = !pos
		val m = Int.min(n, len-p)
		in
		  checkClosed ();
		  pos := p+m;
(** NOTE: could use unchecked operations here **)
		  String.substring (src, p, m)
		end
	  fun readA {buf, i, sz} = let
		val p = !pos
		val m = (case sz
		       of NONE => Int.min(CharArray.length buf-i, len-p)
			| (SOME n) => Int.min(n, len-p)
		      (* end case *))
		in
		  checkClosed ();
		  pos := p+m;
		  CharArray.copyVec {src=src, si=p, len=SOME m, dst=buf, di=i};
		  m
		end
	  fun getPos () = (checkClosed(); !pos)
	  in
	    PrimIO.RD{
		name      = "<string>",
		chunkSize = len,
		readVec   = SOME(readV),
        	readArr   = SOME(readA),
		readVecNB = SOME(SOME o readV),
		readArrNB = SOME(SOME o readA),
		block     = SOME(checkClosed),
		canInput  = SOME(fn () => (checkClosed(); true)),
		avail     = SOME o avail,
		getPos    = SOME getPos,
		setPos    = SOME(fn i => (
				checkClosed();
				if (i < 0) orelse (len < i)
				  then raise Subscript
				  else ();
				pos := i)),
        	endPos    = SOME(fn () => (checkClosed(); len)),
		verifyPos = SOME getPos,
		close     = fn () => closed := true,
		ioDesc    = NONE
	      }
	  end

  end; (* PosixTextPrimIO *)


