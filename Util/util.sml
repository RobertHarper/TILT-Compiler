structure Util :> UTIL =
struct

    val error = UtilError.error
    val reject = UtilError.reject

    exception UNIMP

    (* avoid shadowing error since we export it at a different type! *)
    fun localerror s = error "util.sml" s

    fun loop a b = if (a>b) then [] else a::(loop (a+1) b)
    fun count n = loop 0 (n-1)

    local
	val precomputeMax = 10
	fun rawSpaces 0 = ""
	  | rawSpaces 1 = " "
	  | rawSpaces n =
	    let val _ = if (n < 2) then localerror "rawSpaces given negative number" else ()
		val a = n div 2
		val b = n - a
		val aSpace = rawSpaces a
		val bSpace = if (a = b) then aSpace
			     else (aSpace ^ " ")
	    in  aSpace ^ bSpace
	    end
	val precompute = Array.tabulate(precomputeMax, rawSpaces)
    in  fun spaces n =
	if (n < 0)
	    then ""
	else if (n < precomputeMax)
		 then Array.sub(precompute,n)
	     else rawSpaces n
    end

    fun printl s = print (s^"\n")
    fun lprint s = print ("\n"^s)
    fun lprintl s = print ("\n"^s^"\n")

    fun printem strings = app print strings

    fun mapopt f NONE = NONE
      | mapopt f (SOME x) = SOME(f x)
    fun eq_opt (f, NONE, NONE) = true
      | eq_opt (f, SOME a, SOME b) = f(a,b)
      | eq_opt _ = false
    fun split_opt (NONE) = (NONE,NONE)
      | split_opt (SOME (a,b)) = (SOME a,SOME b)

    fun CharStr2char s = String.sub(s,0)

    type 'a oneshot = 'a option ref
    fun oneshot () = ref NONE
    fun oneshot_init a = ref (SOME a)
    fun oneshot_set (ref (SOME _), _) = localerror "oneshot_set called on something already set"
      | oneshot_set (r, a) = r := SOME a
    val oneshot_deref = !
    val eq_oneshot = op =

    fun substring (pattern,target) =
	let val pattern = explode pattern
	    val target = explode target
	    fun match [] _ = true
	      | match (a::arest) (b::brest) = (a = b) andalso (match arest brest)
	      | match _ _ = false
	    fun loop n cur = if (match pattern cur)
				 then SOME n
			     else (case cur of
				       [] => NONE
				     | (_::rest) => loop (n+1) rest)
	in  loop 0 target
	end

    fun curry2 f = fn a => fn b => f (a,b)
    fun curry3 f = fn a => fn b => fn c => f (a,b,c)

    fun all_pairs p =
      let
	fun loop [] = true
	  | loop [_]= true
	  | loop (fst::snd::rest) =
	  (p (fst,snd)) andalso (loop (snd::rest))
      in
	loop
      end

    fun apply (f:'a -> 'b, x:'a) : (unit -> 'b) =
	let val r = f x
	in  fn () => r
	end handle e => fn () => raise e

    fun memoize (f:unit -> 'a) : unit -> 'a =
	let val cell = ref f
	    val _ = cell := (fn () =>
			     let val r = apply(f,())
				 val _ = cell := r
			     in  r()
			     end)
	in  fn () => !cell()
	end

    structure IO = Posix.IO
    structure FS = Posix.FileSys
    structure P = Posix.Process
    structure S = Posix.Signal

    val statusToString : Word8.word -> string =
	Int.toString o Word8.toInt

    val pidToString : P.pid -> string =
	Int.toString o SysWord.toInt o P.pidToWord

    fun pidFromString (s : string) : P.pid =
	(case Int.fromString s
	   of NONE => localerror ("unrecognized process ID: " ^ s)
	    | SOME i =>
		let val w = SysWord.fromInt i
		in  P.wordToPid w
		end)

    val signalToString : S.signal -> string =
	Int.toString o SysWord.toInt o S.toWord

    fun check_status (pid : P.pid, s : P.exit_status) : unit =
	(case s
	   of P.W_EXITED => ()
	    | P.W_EXITSTATUS s =>
		if s = 0w0 then ()
		else
		    localerror ("child " ^ pidToString pid ^
				" exitted with status " ^
				statusToString s)
	    | P.W_SIGNALED s =>
		localerror ("child " ^ pidToString pid ^
			    " killed with signal " ^ signalToString s)
	    | P.W_STOPPED _ =>
		localerror ("child " ^ pidToString pid ^
			    " had status W_STOPPED without WUNTRACED"))

    (*
	There is a bug either in SML/NJ's runtime or in the digital
	unix 4.0D implementation of waitpid().  The runtime sometimes
	raises an exception because it doesn't understand the status
	value returned by waitpid().
    *)
    fun waitpid_nh (pid : P.pid) : P.exit_status option =
	(Option.map #2 (P.waitpid_nh (P.W_CHILD pid, []))
	 handle OS.SysErr ("unknown child status", _) =>
	    (print "Warning: caught and ignored an unknown child status\n";
	     SOME P.W_EXITED))

    fun waitpid (pid : P.pid) : P.exit_status =
	(#2 (P.waitpid (P.W_CHILD pid, []))
	 handle OS.SysErr ("unknown child status", _) =>
	    (print "Warning: caught and ignored an unknown child status\n";
	     P.W_EXITED))

    (* Make fd like tmpfd and close tmpfd. *)
    fun redirect (fd : IO.file_desc, tmpfd : IO.file_desc) : unit =
	(IO.dup2 {old=tmpfd, new=fd};
	 IO.close tmpfd)

    (* Connect fd to /dev/null. *)
    fun supress (fd : IO.file_desc, mode : FS.open_mode) : unit =
	redirect (fd, FS.openf("/dev/null",mode,FS.O.flags nil))

    fun input (fd : IO.file_desc) : string =
	let fun loop (acc : string list) : string =
		let val vec = IO.readVec (fd, 1024)
		    val s = Byte.bytesToString vec
		in  if size s = 0
		    then concat (rev acc)
		    else loop (s :: acc)
		end
	in  loop nil
	end

    fun output (fd : IO.file_desc, s : string) : unit =
	let val vec = Byte.stringToBytes s
	    fun loop (_,0) = ()
	      | loop (offset,remain) =
		let val n = IO.writeVec (fd, {buf=vec,i=offset,sz=SOME remain})
		in  loop(offset+n,remain-n)
		end
	in  loop(0,Word8Vector.length vec)
	end

    (* Prevent both processes writing same (buffered) text. *)
    fun flush () : unit =
	(TextIO.flushOut (TextIO.stdOut);
	 TextIO.flushOut (TextIO.stdErr))

    fun run (program : string list) : unit =
	(case program
	   of (path :: _) =>
		let val _ = flush()
		in  case P.fork()
		      of NONE => (* we are the child *)
			    (supress (FS.stdin, FS.O_RDONLY);
			     P.execp (path, program)
			     handle e =>
				(output(FS.stderr,"exec of "^path^" failed: "^
					exnMessage e);
				 P.exit 0w1))
			| SOME pid =>	(* we are the parent *)
			    let val status = waitpid pid
				val _ = check_status (pid,status)
			    in	()
			    end
		end
	 | nil => ())

    fun outputOf (program : string list) : string =
	(case program
	   of (path :: _) =>
		let val _ = flush()
		    val {infd, outfd} = IO.pipe()
		in
		    (case P.fork()
		       of NONE => (* we are the child *)
			    (IO.close infd;
			     redirect (FS.stdout, outfd);
			     supress (FS.stderr, FS.O_WRONLY);
			     supress (FS.stdin, FS.O_RDONLY);
			     P.execp (path, program)
			     handle e =>
				(output(outfd,"exec of "^path^" failed: "^
				 exnMessage e);
			     IO.close outfd;
			     P.exit 0w1))
			| SOME pid =>	(* we are the parent *)
			    let
				val _ = IO.close outfd
				val output = input infd
				val _ = IO.close infd
				val status = waitpid pid
				val _ = check_status (pid,status)
			    in  output
			    end)
		end
	   | nil => "")

    fun isdone (pid : P.pid) : unit -> bool =
	let
	    val s : P.exit_status option ref = ref NONE
	    fun poll () : P.exit_status option =
		if isSome (!s) then !s
		else (s := waitpid_nh pid; !s)
	    fun isdone () =
		(case poll()
		   of NONE => false
		    | SOME s => (check_status (pid,s); true))
	in  isdone
	end

    (* Prevent both processes writing same (buffered) text. *)
    fun flush () : unit =
	(TextIO.flushOut (TextIO.stdOut);
	 TextIO.flushOut (TextIO.stdErr))

    fun errorMsg (e : exn) : string =
	(case e of
	    UtilError.BUG {file,msg} => file ^ ": " ^ msg
	|   UtilError.Reject {msg} => msg
	|   e => "uncaught exception: " ^ exnMessage e)

    fun uncaught (e:exn) : 'a =
	(output(FS.stderr,"child uncaught exn: " ^ errorMsg e);
	 P.exit 0w1)

    fun background (f : unit -> 'a) : unit -> bool =
	let val _ = flush()
	in  (case P.fork()
	       of NONE => (* child *)
		    let val _ = f() handle e => uncaught e
		    in  P.exit 0w0
		    end
		| SOME pid => isdone pid)
	end

    (*
	To implement background', we fork twice so that we do not have
	to wait for the process evaluating f.  We use a pipe to transmit
	the process id so that we can kill it.
    *)
    fun grandchild (f : unit -> 'a, outfd : IO.file_desc) : 'b =
	let val _ = IO.close outfd
	    val _ = f() handle e => uncaught e
	in  P.exit 0w0
	end

    fun child (f : unit -> 'a, infd : IO.file_desc, outfd : IO.file_desc) : 'b =
	let val _ = IO.close infd
	    val _ = supress (FS.stdin, FS.O_RDONLY)
	in  (case P.fork()
	       of NONE => grandchild (f,outfd)
		| SOME pid =>
		    let val s = pidToString pid
			val _ = output (outfd, s)
			val _ = IO.close outfd
		    in  P.exit 0w0
		    end)
	end

    fun parent (f : unit -> 'a) : P.pid =
	let val _ = flush()
	    val {infd,outfd} = IO.pipe()
	in  (case P.fork()
		of NONE => child (f,infd,outfd)
		 | SOME childpid =>
		    let val _ = IO.close outfd
			val s = input infd
			val _ = IO.close infd
			val grandchildpid = pidFromString s
			val status = waitpid childpid
			val _ = check_status (childpid,status)
		    in  grandchildpid
		    end)
	end

    fun background' (f : unit -> 'a) : unit -> unit =
	let val pid = parent f
	    fun kill () = P.kill (P.K_PROC pid, S.kill) handle _ => ()
	in  memoize kill
	end

    structure V = Vector

    fun extract (f:real * real -> real) (v:real vector) : real =
	if V.length v >= 1
	then V.foldl f (V.sub (v, 0)) v
	else raise Domain

    val min : real vector -> real = extract Real.min
    val max : real vector -> real = extract Real.max

    fun sum (v:real vector, f:real -> real) : real =
	V.foldl (fn (x,y) => y + f x) 0.0 v

    val len : real vector -> real = Real.fromInt o V.length

    fun mean (v:real vector) : real =
	if V.length v >= 1 then sum(v, fn x => x) / len v
	else raise Domain

    (* The err term and the division by n-1 are taken from Numerical Recipes *)
    fun variance (v:real vector) : real =
        if V.length v >= 2
        then
	    let val mean = mean v
		val n = len v
		fun square x = x*x
		val var = sum (v, fn x => square (x - mean))
		val err = (square (sum (v, fn x => x - mean))) / n
            in
		(var - err) / (n - 1.0)
            end
        else raise Domain

    val stddev : real vector -> real = Math.sqrt o variance

    fun absdev (v : real vector) : real =
	if V.length v >= 1
	then
	    let val mean = mean v
	    in  sum(v, fn x => Real.abs (x - mean)) / len v
	    end
	else raise Domain

    local
	structure StringKey =
	struct
	    type ord_key = string
	    val compare = String.compare
	end
    in
	structure StringMap = SplayMapFn(StringKey)
	structure StringSet = SplaySetFn(StringKey)
    end

end
