(*$import Prelude TextIO OS *)
(* unix-sig.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature UNIX =
  sig
    type proc
    type signal

      (* executeInEnv (path, args, env)
       *   forks/execs new process given by path
       *   The new process will have environment env, and
       *   arguments args prepended by the last arc in path
       *   (following the Unix convention that the first argument
       *   is the command name).
       *   Returns an abstract type proc, which represents
       *   the child process plus streams attached to the
       *   the child process stdin/stdout.
       *
       *   Simple command searching can be obtained by using
       *     executeInEnv ("/bin/sh", "-c"::args, env)
       *)
    val executeInEnv : string * string list * string list -> proc

      (* execute (path, args) 
       *       = executeInEnv (path, args, Posix.ProcEnv.environ())
       *)
    val execute : string * string list -> proc

      (* streamsOf proc
       * returns an instream and outstream used to read
       * from and write to the stdout and stdin of the 
       * executed process.
       *
       * The underlying files are set to be close-on-exec.
       *)
    val streamsOf : proc -> TextIO.instream * TextIO.outstream

      (* reap proc
       * This closes the associated streams and waits for the
       * child process to finish, returns its exit status.
       *
       * Note that even if the child process has already exited,
       * so that reap returns immediately,
       * the parent process should eventually reap it. Otherwise,
       * the process will remain a zombie and take a slot in the
       * process table.
       *)
    val reap : proc -> OS.Process.status

      (* kill (proc, signal)
       * sends the Posix signal to the associated process.
       *)
    val kill : proc * signal -> unit

  end


(*
 * $Log$
# Revision 1.2  2000/11/27  22:36:45  swasey
# *** empty log message ***
# 
 * Revision 1.1  1998/03/09 19:54:32  pscheng
 * added basis
 *
 * Revision 1.1.1.1  1997/01/14  01:38:25  george
 *   Version 109.24
 *
 *)
