(*
NAME
	regress, runall, runtest, tilt, tilt-nj
		- regression test harness for the TILT compiler

SYNOPSIS
	../Bin/regress [all | nj | tilt] [outfile]

	../Bin/runall [-fnc]

	../Bin/runtest [-fncFS] testdir ...

	../Bin/tilt [tilt-options]
	../Bin/tilt-nj [tilt-options]

DESCRIPTION
	Runtest performs the tests in testdirs, stopping when a test
	fails.  Each testdir defines a single test, as described in
	the file README.  With the -f option, runtest will keep going
	after failure.  The -n option causes runtest to use a version
	of TILT compiled by SML/NJ.  Runtest purges compiler-generated
	files before and after each test.  The -c option prevents this
	cleanup.  The mutually exclusive -F (should fail) and -S
	(should succeed) options tell runtest the expected outcome of
	the tests, so that it can generate an appropriate warning if a
	test unexepectedly fails or succeeds.

	A test comprises some SML code and an expected result.
	Runtest uses TILT to compile the test code then runs the
	generated executable.  If the observed result does not match
	the expected result, then the test fails and runtest prints
	the observed result.  Possible results are:

	Bomb	TILT failed while compiling the test code
	Reject	TILT rejected the test code as invalid
	Exit s	compiled code exitted with output s
	Suicide	compiled code died because of an uncaught signal

	Runtest executes programs with stdin and stderr connected to
	/dev/null and stdout connected to one end of a pipe.  TILT
	must exit with status 0 when it accepts a program and with
	status 10 when it rejects a program.  Any other behavior is
	considered a Bomb.  TILT's output and the exit status of a
	test's executable are ignored.

	Runall runs the tests listed in testlist.txt.

	Regress is a one-step tool to compile the compiler, compile
	runtest, and run the tests.  The first option determines what
	version(s) of TILT to test:

	nj	TILT compiled by SML/NJ (default)
	tilt	TILT compiled by TILT
	all	both versions

	The second option says where to put runtest output.  The
	default is RegressionResults.`date`.txt.

	Tilt and tilt-nj are the TILT executables used by runtest.

EXAMPLES
	In order to use the harness, you must build command-line
	versions of TILT and compile the test harness.  The glorious
	details (which are handled by ../Bin/regress) are:

	1. Check out the sources.
	2. Compile the runtime with make runtime inside ml96.
	3. Compile TILT under SML/NJ with make tilt_heap inside ml96.
	4. Compile the Basis with make basis inside ml96.
	5. Compile native TILT with make all inside ml96 (optional).
	6. Compile runtest with make runtest inside ml96.

	If you use -n, you can skip step (5).

	Once these steps are complete, you can run individual tests:

		cd ml96/Test
		../Bin/runtest -n 0001 0003

	Or you can run them all:

		cd ml96/Test
		../Bin/runall -n

SEE ALSO
	../Doc/tilt.1
	README
*)

(*
	XXX: TILT's Util can now handle child processes as well as runtest. Runtest
	should use that.

	XXX: Runtest should use TILT's Platform to check the platform.
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
		    corefile : string,
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

    fun output (fd : IO.file_desc, s : string) : unit =
	let
	    fun loop (_,0) = ()
	      | loop (offset,remain) =
		let val n = IO.writeVec (fd, {buf=s,i=offset,sz=SOME remain})
		in  loop(offset+n,remain-n)
		end
	in  loop(0,size s)
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
			   redirect (FS.stdout, outfd);
(* XXX: We want to send stderr to stdout so harness prints runtime messages. *)
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
	    | _ => fail ((foldr (fn (s,acc) => s ^ " " ^ acc) "" program) ^ " failed"))
	     
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
		  corefile : string,
		  resultfile : string,
		  binary : string} : status =
	let
	    val cleanup = if clean
			      then fn () => app run_for_effect [[tilt,"-pp",mapfile],["/bin/rm", "-f","core",corefile,binary]]
			  else fn () => ()
	    val _ = cleanup()
	    val expected = read_result resultfile
	    val actual =
		(case run_program [tilt,"-fTypecheck","-fIlcontextChecks","-o",binary,mapfile]
		   of Exit (0, _) => Raw (run_program [binary])
		    | Exit (10, _) => Reject
		    | Exit _ => Bomb
		    | Suicide => Bomb)
	    val _ = cleanup() (* Core files take up too much space, so always cleanup. 
			       * If necessary, could add an option to keep trash in
			       * failure cases, but shouldn't be default.  -leaf *)
	in
	    if same_result (expected, actual)
	      then Success
	    else
	      Failure (result_string actual)
	end
end

structure Main =
struct
    structure P = OS.Path

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

    fun run_test {tilt : string,
		  clean : bool,
		  platform : string,
		  succeed : unit -> unit,
		  fail : unit -> unit} (testdir : string) : unit =
	let
	    val _ = print ("running test " ^ testdir ^ "\n")
	    val mapfile = P.joinDirFile{dir=testdir, file="mapfile"}
	    val corefile = P.joinDirFile{dir=testdir, file="core"}
	    val result = P.joinDirFile{dir=testdir, file="result"}
	    val binary = P.joinDirFile{dir=testdir, file="Test."^platform^".exe"}
	    val arg = {tilt=tilt,
		       clean=clean,
		       mapfile=mapfile,
		       corefile=corefile,
		       resultfile=result,
		       binary=binary}
	in
	    (case Test.run_test arg
	       of Test.Success => succeed ()
		| Test.Failure msg =>
		   (eprint ("FAILURE in " ^ testdir ^ ", actual results were:\n");
		    eprint msg;
		    fail()))
	end

    local
	val op/ : string * string -> string =
	    fn (dir,file) => P.joinDirFile{dir=dir,file=file}
    in
	fun bindir (name : string) : string =
	    P.dir (CommandLine.name())/".."/name
    end

    fun pblock b = 
      let fun pline s = eprint ("\t"^s^"\n")
      in app pline b
      end
 
    fun warn_on_success () = pblock
      ["CONGRATULATIONS: This test was expected to fail, but succeeded.",
       "Please change the status of this test from \"fail\" to \"pass\"",
       "and close out applicable bug reports."
       ]

    fun warn_on_failure () = pblock
      ["WARNING: This test was expected to succeed, but failed!!!",
       "This probably means that you have broken something: please fix it.",
       "If this is not possible, you should at least issue a bug report and",
       "change its status from \"pass\" to \"fail\" before checking in."
       ]

    fun usage () : 'a =
	fail ("usage: " ^ CommandLine.name() ^ " [-fncFS] testdir ...")

    fun main () : unit =
	let val dashn : bool ref = ref false
	    val dashf : bool ref = ref false
	    val dashS : bool ref = ref false
	    val dashF : bool ref = ref false
	    val dashc : bool ref = ref false
	    fun option ({argc,...} : Arg.arg) : unit =
		(case argc
		   of #"n" => dashn := true
		    | #"f" => dashf := true
		    | #"S" => dashS := true
		    | #"F" => dashF := true
		    | #"c" => dashc := true
		    | _ => usage())
	    val testdirs = Arg.args option (CommandLine.arguments())
	    val tilt : string = bindir (if !dashn then "tilt-nj" else "tilt")
	    val onfail : unit -> unit =
		if !dashf then fn () => ()
		else fn () => fail "test failed"
	    val (fail : unit -> unit, succeed : unit -> unit) =
		(case (!dashF, !dashS)
		   of (true,true) => fail "-S and -F are mutually exclusive"
		    | (false,false) => (onfail,fn ()=>())
		    | (true,false)  => (onfail,warn_on_success)
		    | (false,true)  => (onfail o warn_on_failure,fn () => ()))
	    val run : string -> unit =
		run_test {tilt=tilt,
			  clean=not (!dashc),
			  platform=platform(),
			  succeed=succeed,
			  fail=fail}
	in  app run testdirs
	end handle e =>
		(eprint (CommandLine.name());
		 eprint ": ";
		 eprint (case e
			   of Test.Crash msg => msg
			    | _ => exnMessage e);
		 eprint "\n";
		 OS.Process.exit OS.Process.failure)
end

val _ = Main.main()
