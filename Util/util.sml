structure Util :> UTIL =
struct

    val error = UtilError.error
    val reject = UtilError.reject

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
	    in	aSpace ^ bSpace
	    end
	val precompute = Array.tabulate(precomputeMax, rawSpaces)
    in	fun spaces n =
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
			     let    val r = apply(f,())
				 val _ = cell := r
			     in	 r()
			     end)
	in  fn () => !cell()
	end

    (*
	NB We need to redirect stdin in every child process to prevent
	any confusion caused by the parent and child interleaving
	input.	Also, interactive SML/NJ apparantly handles SIGINT in
	the child and uses isatty(0) to determine whether or not the
	process should terminate.  If we redirect stdin, then a user
	interrupt kills children.
    *)

    structure IO = Posix.IO
    structure FS = Posix.FileSys
    structure P = Posix.Process
    structure PE = Posix.ProcEnv
    structure S = Posix.Signal

    val status2string : Word8.word -> string =
	Int.toString o Word8.toInt

    val pid2string : P.pid -> string =
	Int.toString o SysWord.toInt o P.pidToWord

    val signal2string : S.signal -> string =
	Int.toString o SysWord.toInt o S.toWord

    fun check_status (pid : P.pid, s : P.exit_status) : unit =
	(case s of
	    P.W_EXITED => ()
	|   P.W_EXITSTATUS 0w0 => ()
	|   P.W_EXITSTATUS s =>
		localerror (concat["child ", pid2string pid,
		    " exitted with status ", status2string s])
	|   P.W_SIGNALED s =>
		localerror (concat["child ", pid2string pid,
		    " killed with signal ", signal2string s])
	|   P.W_STOPPED _ =>
		localerror (concat["child ", pid2string pid,
		    " had status W_STOPPED without WUNTRACED"]))

    fun exec (command:string list) : 'a =
	(case command of
	    nil => localerror "exec given null argument list"
	|   path :: _ => P.execp (path,command))

    (*
	There is a bug either in SML/NJ's runtime or in the digital
	unix 4.0D implementation of waitpid().	The runtime sometimes
	raises an exception because it doesn't understand the status
	value returned by waitpid().
    *)
    fun waitpid_nh (pid : P.pid) : P.exit_status option =
	(Option.map #2 (P.waitpid_nh (P.W_CHILD pid, []))
	 handle OS.SysErr ("unknown child status", _) => SOME P.W_EXITED)

    fun waitpid (pid : P.pid) : P.exit_status =
	(#2 (P.waitpid (P.W_CHILD pid, []))
	 handle OS.SysErr ("unknown child status", _) => P.W_EXITED)

    fun connect (fd : IO.file_desc, tmpfd : IO.file_desc) : unit =
	(IO.dup2 {old=tmpfd, new=fd};
	 IO.close tmpfd)

    fun readonly (file:string) : IO.file_desc =
	FS.openf(file,FS.O_RDONLY,FS.O.flags nil)

    fun writeappend (file:string) : IO.file_desc =
	FS.openf(file,FS.O_WRONLY,FS.O.flags nil)

    local
	open FS.S
	val m644 : mode =   (* stupid abstraction *)
	    flags[irusr, iwusr, irgrp, iwgrp, iroth]
    in
	fun writetrunc (file:string) : IO.file_desc =
	    FS.creat(file,m644)
    end

    fun readall (fd : IO.file_desc) : string =
	let fun loop (acc : string list) : string =
		let val vec = IO.readVec (fd, 1024)
		    val s = Byte.bytesToString vec
		in  if size s = 0
		    then concat (rev acc)
		    else loop (s :: acc)
		end
	    val s = loop nil
	    val _ = IO.close fd
	in  s
	end

    fun writeall (fd : IO.file_desc, s : string) : unit =
	let val vec = Byte.stringToBytes s
	    fun loop (offset:int, remain:int) : unit =
		if remain = 0 then ()
		else
		    let val n = IO.writeVec (fd, {buf=vec,i=offset,sz=SOME remain})
		    in	loop(offset+n,remain-n)
		    end
	    val _ = loop(0,Word8Vector.length vec)
	    val _ = IO.close fd
	in  ()
	end

    (*
	A child created by fork' prints information about uncaught
	exceptions.  A child created by fork pipes uncaught exception
	information to the parent.  Fork returns a function (that can
	only be called once) to read the pipe and raise a related
	exception in the parent.
    *)
    fun flush_buffers () : unit =
	(TextIO.flushOut (TextIO.stdOut);
	 TextIO.flushOut (TextIO.stdErr))

    fun fork' (child:unit -> 'a) : P.pid =
	let val _ = flush_buffers()
	in  (case (P.fork()) of
		NONE =>
		    ((child(); P.exit 0w0)
		     handle e => UtilError.print_and_exit e)
	    |	SOME childpid => childpid)
	end

    fun fork (child : unit -> 'a) : P.pid * (unit -> unit) =
	let val {infd,outfd} = IO.pipe()
	    fun child' () : unit =
		let val _ = IO.close infd
		    val _ =
			(child() handle e =>
			    (writeall(outfd, UtilError.errormsg e);
			     raise e))
		    val _ = IO.close outfd
		in  ()
		end
	    val pid = fork' child'
	    val _ = IO.close outfd
	    fun waitexn () : unit =
		let val exnmsg = readall infd
		in  if exnmsg = "" then ()
		    else error ("child " ^ pid2string pid) exnmsg
		end
	in  (pid, waitexn)
	end

    (*
	See CVS revision 1.38 for a version of run that can run command in
	the background.
    *)
    fun run {command:string list, stdin:string option, stdout:string option} : unit =
	let fun child () : unit =
		let val infile =
			(case stdin of
			    NONE => "/dev/null"
			|   SOME file => file)
		    val _ = connect(FS.stdin, readonly infile)
		    val _ =
			(case stdout of
			    NONE => ()
			|   SOME file => connect(FS.stdout, writetrunc file))
		in  exec command
		end
	    val (pid,waitexn) = fork child
	    val status = waitpid pid
	    val _ = waitexn ()
	    val _ = check_status (pid,status)
	in  ()
	end

    fun outputOf (program : string list) : string =
	let val {infd,outfd} = IO.pipe()
	    fun child () : unit =
		let val _ = IO.close infd
		    val _ = connect(FS.stdout, outfd)
		    val _ = connect(FS.stdin, readonly "/dev/null")
		in  exec program
		end
	    val (pid,waitexn) = fork child
	    val _ = IO.close outfd
	    val output = readall infd
	    val status = waitpid pid
	    val _ = waitexn()
	    val _ = check_status(pid,status)
	in  output
	end

    fun nostdin (f : unit -> 'a) () : 'a =
	let val _ = connect(FS.stdin, readonly "/dev/null")
	in  f()
	end

    fun background (f : unit -> 'a) : unit -> bool =
	let val (pid,waitexn) = fork (nostdin f)
	    fun finished (status:P.exit_status) : bool =
		let val _ = waitexn()
		    val _ = check_status(pid,status)
		in  true
		end
	    val r : (unit -> bool) option ref = ref NONE
	    fun isdone () : bool =
		(case !r of
		    SOME f => f()
		|   NONE =>
			(case (waitpid_nh pid) of
			    SOME status =>
				let val f = memoize(fn () => finished status)
				    val _ = r := SOME f
				in  f()
				end
			|   NONE => false))
	in  isdone
	end

    fun background' (f:unit -> 'a) : unit -> unit =
	let val (pid,waitexn) = fork (nostdin f)
	    fun kill () : unit =
		let val _ = P.kill (P.K_PROC pid, S.kill) handle _ => ()
		    val status = waitpid pid
		    val _ = waitexn()
		    val _ =
			(case status of
			    P.W_SIGNALED s =>
				if s = S.kill then ()
				else check_status (pid,status)
			|   _ => check_status (pid,status))
		in  ()
		end
	    val kill = memoize kill
	in  kill
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
	    in	sum(v, fn x => Real.abs (x - mean)) / len v
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
