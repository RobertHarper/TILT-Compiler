(*$import Posix TextIO Getopt CharVector Byte Word8 OS List CommandLine Substring *)

(*
NAME
	runtest, runall, tilt, tilt-nj, platform
		- regression test harness for the TILT compiler

SYNOPSIS
	runtest [-fnc] testdir ...

	runall [-fnc]

	tilt [tilt-options]
	tilt-nj [tilt-options]

DESCRIPTION
	Runtest performs the tests in testdirs, stopping when a test
	fails.  Each testdir defines a single test, as described in
	the file README.  With the -f option, runtest will keep going
	after failure.  The -n option causes runtest to use a version
	of TILT hosted under the SML/NJ runtime.  Runtest purges
	compiler-generated files before each test and after each
	successful test.  The -c option prevents this cleanup.

        A test comprises some SML code and an expected result.
	Runtest uses TILT to compile the test code then runs the
	generated executable.  If the observed result does not match
	the expected result, then the test fails and runtest prints
	the observed result.  Possible results are:

	Bomb	TILT failed while compiling the test code
	Reject	TILT rejected the test code as invalid
	Exit s	compiled code exitted with output s
	Suicide	compiled code died because of an uncaught signal

	Runtest executes programs with stdin connected to /dev/null and
	stdout and stderr connected to one end of a pipe.  TILT must
	exit with status 0 when it accepts a program and with status 10
	when it rejects a program.  Any other behavior is considered a
	Bomb.  TILT's output and the exit status of a test's executable
	are ignored.

	Runall runs the tests listed in testlist.txt.

	Tilt and tilt-nj are the TILT executables used by runtest.

	Platform sets the environment variable $platform which is used
	by runall and tilt to help find TILT executables.

SEE ALSO
	../Doc/tilt.1
	README

BUGS
	TILT does not yet use exit code 10 to signal when source is
	rejected.

	In order to use the harness, you must build command-line
	versions of TILT.  The glorious details are:

	1. Check out the sources.
	2. Compile the runtime with gmake runtime inside Runtime.
	3. Compile TILT under SML/NJ with gmake heap at the top-level.
	4. Compile the Basis with ./Test/tilt-nj -b at the top-level.
	5. Compile native TILT with ./Test/tilt-nj -m mapfile-all
	   at the top-level.

	If you use -n, you can skip step (5).
*)

signature BUF =
sig
    type t

    exception Seal
    
    val empty : unit -> t
    val append : t -> string -> unit	(* May raise Seal *)
    val seal : t -> string
end

structure Buf :> BUF =
struct
    structure V = CharVector
	
    exception Seal

    datatype buf =
	Closed of V.vector
      | Open of V.vector list		(* backwards *)

    type t = buf ref
	
    fun empty () : t =
	ref (Open nil)

    fun append (r : t) (v : V.vector) : unit =
	(case !r
	   of Open vs => r := Open (v :: vs)
	    | Closed _ => raise Seal)
	     
    fun seal (r : t) : V.vector =
	(case !r
	   of Closed v => v
	    | Open vs =>
	       let val v = V.concat (rev vs)
		   val _ = r := Closed v
	       in  v
	       end)
end

signature TEST =
sig
    exception Crash of string

    datatype status =
	Success
      | Failure of string

    (* May raise Crash *)
    val run_test : {tilt : string,
		    clean : bool,
		    mapfile : string,
		    resultfile : string,
		    binary : string} -> status
end

structure Test :> TEST =
struct

    structure IO = Posix.IO
    structure FS = Posix.FileSys
    structure P = Posix.Process

    exception Crash of string

    fun fail (msg : string) : 'a = raise Crash msg

    fun input_all (fd : IO.file_desc) : string =
	let val blocksize = 1024*8	(* generous, arbitrary *)
	    val buf = Buf.empty()
	    fun loop () = 
		let val bytes = IO.readVec (fd, blocksize)
		    val text = Byte.bytesToString bytes
		in
		    if size text > 0 then
			(Buf.append buf text;
			 loop())
		    else Buf.seal buf
		end
	in  loop ()
	end

    (* Make fd like tmpfd and close tmpfd. *)
    fun redirect (fd : IO.file_desc, tmpfd : IO.file_desc) : unit =
	(IO.dup2 {old=tmpfd, new=fd};
	 IO.close tmpfd)

    (* Connect fd to /dev/null. *)
    fun supress (fd : IO.file_desc, mode : FS.open_mode) : unit =
	redirect (fd, FS.openf("/dev/null",mode,FS.O.flags nil))

    datatype raw_result =
	Exit of int * string		(* exit status, stdout *)
      | Suicide				(* uncaught signal *)

    fun run_program (program : string list) : raw_result =
	(case program
	   of (path :: _) =>
	       let val {infd, outfd} = IO.pipe()
	       in
		   (case P.fork()
		      of NONE => (* we are the child *)
			  (IO.close infd;
			   IO.dup2 {old=outfd, new=FS.stdout};
			   redirect (FS.stderr, outfd);
			   supress (FS.stdin, FS.O_RDONLY);
			   P.execp (path, program))
			| SOME pid =>	(* we are the parent *)
			  let
			      val _ = IO.close outfd
			      val output = input_all infd
			      val _ = IO.close infd
			      val (_, status) = P.waitpid (P.W_CHILD pid, nil)
			  in
			      (case status
				 of P.W_EXITED => Exit (0,output)
				  | P.W_EXITSTATUS status =>
				     Exit (Word8.toInt status,output)
				  | P.W_SIGNALED _ => Suicide
				  | P.W_STOPPED _ => fail (path ^ " stopped"))
			  end)
	       end
	 | _ => fail "no program")

    fun run_for_effect (program : string list) : unit =
	(case run_program program
	   of Exit (0, _) => ()
	    | _ => fail (hd program ^ " failed"))
	     
    datatype result =
	Bomb				(* TILT bombed *)
      | Reject				(* TILT rejected test code *)
      | Raw of raw_result		(* test program ran to result *)

    fun result_string (r : result) : string =
	(case r
	   of Bomb => "Bomb\n"
	    | Reject => "Reject\n"
	    | Raw (Exit (_,s)) => "Exit\n" ^ s
	    | Raw Suicide => "Suicide\n")
	     
    fun same_result (r : result, r' : result) : bool =
	(case (r, r')
	   of (Bomb, Bomb) => true
	    | (Reject, Reject) => true
	    | (Raw r, Raw r') =>
	       (case (r, r')
		  of (Exit (_,s), Exit (_,s')) => s=s'
		   | (Suicide, Suicide) => true
		   | _ => false)
	    | _ => false)

    fun read_result (resultfile : string) : result =
	let
	    val ins = TextIO.openIn resultfile
	    val status = TextIO.inputLine ins
	    val rest = TextIO.inputAll ins
	    val _ = TextIO.closeIn ins
	    val code = 0		(* arbitrary *)
	in
	    (case (status, rest)
	       of ("Bomb\n","") => Bomb
		| ("Reject\n","") => Reject
		| ("Suicide\n","") => Raw Suicide
		| ("Exit\n",output) => Raw (Exit (code,output))
		| _ => fail (resultfile ^ " is malformed"))
	end

    datatype status =
	Success
      | Failure of string

    fun run_test {tilt : string,
		  clean : bool,
		  mapfile : string,
		  resultfile : string,
		  binary : string} : status =
	let
	    val cleanup = if clean
			      then fn () => run_for_effect [tilt,"-C",mapfile]
			  else fn () => ()
	    val _ = cleanup()
	    val expected = read_result resultfile
	    val actual =
		(case run_program [tilt,"-m",mapfile]
		   of Exit (0, _) => Raw (run_program [binary])
		    | Exit (10, _) => Reject
		    | Exit _ => Bomb
		    | Suicide => Bomb)
	in
	    if same_result (expected, actual)
		then (cleanup(); Success)
	    else
		Failure (result_string actual)
	end
end

structure Main =
struct
    structure G = Getopt
    structure P = OS.Path
    structure S = Substring
	
    fun fail (msg : string) : 'a = raise Test.Crash msg
	
    fun uname (name : string) : string =
	let fun match (n,_) = n = name
	in
	    case List.find match (Posix.ProcEnv.uname ())
	      of NONE => fail ("uname doesn't provide " ^ name)
	       | SOME (_,value) => value
	end

    fun platform () : string =
	(case uname "sysname"
	   of "SunOS" => "sparc"
	    | "OSF1" => "alpha"
	    | _ => fail "can not run TILT binaries on this platform")

    fun eprint (s : string) : unit =
	TextIO.output(TextIO.stdErr, s)

    fun fixup (path : string) : string = path
    fun run_test {tilt : string,
		  clean : bool,
		  platform : string,
		  fail : unit -> unit} (testdir : string) : unit =
	let
	    val _ = print ("running test " ^ testdir ^ "\n")
	    val mapfile = P.joinDirFile{dir=testdir, file="mapfile"}
	    val result = P.joinDirFile{dir=testdir, file="result"}
	    val binary = fixup(P.joinDirFile{dir=testdir, file="Test."^platform^".exe"})
	    val arg = {tilt=tilt,
		       clean=clean,
		       mapfile=mapfile,
		       resultfile=result,
		       binary=binary}
	in
	    (case Test.run_test arg
	       of Test.Success => ()
		| Test.Failure msg =>
		   (eprint ("FAILURE in " ^ testdir ^ "\n" ^ msg);
		    fail()))
	end

    fun harness (name : string) : string =
	P.joinDirFile {dir = P.dir (CommandLine.name()),
		       file = name}

    fun main () : unit =
	(let
	     val opts = [G.Noarg (#"f",#"f"),
			 G.Noarg (#"n",#"n"),
			 G.Noarg (#"c",#"c")]
	     val args = CommandLine.arguments()
	 in
	     (case G.getopt (opts,args)
		of G.Error msg =>
		    raise fail (msg ^ "\nusage: " ^ CommandLine.name() ^
				" [-fnc] testdir ...")
		 | G.Success (flags, args) =>
			let
			    fun has (flag : char) : bool =
				List.exists (fn c => c=flag) flags
			    val tilt = harness (if has #"n"
						    then "tilt-nj"
						else "tilt")
			    val fail = if has #"f" then fn () => ()
				       else fn () => fail "test failed"
			    val run = run_test {tilt=tilt,
						clean=not (has #"c"),
						platform=platform(),
						fail=fail}
			in
			    app run args
			end)
	 end)
	     handle e => (eprint (CommandLine.name());
			  eprint ": ";
			  eprint (case e
				    of Test.Crash msg => msg
				     | _ => exnMessage e);
			  eprint "\n";
			  OS.Process.exit OS.Process.failure)

end

val _ = Main.main()
