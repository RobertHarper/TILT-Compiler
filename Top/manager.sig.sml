(*$import Prelude *)
(* The intended external syntax for tilc is as follows:
 *
 * The following three options, if they occur, must be the only switch:
 *    -?, -h         -- Display help message printed by Help.help
 *    -mkdep <file>  -- Compute the dependencies for the current directory and
 *                      append the results to the specified file
 *
 * The next three options can each occur at most once on the command line, but
 * they can be mixed in any combination.  In all cases, the rest of the 
 * command line must be a list of source and/or object (.sml, .int, .uo, and .ui)
 * files.
 *
 *    -c             -- Compile the .sml or .int source files to .uo or .ui files.
 *                      This option does not link them.  It ignores .uo and .ui
 *                      arguments.
 *    -r <file>      -- This option links the specified source or object files and
 *                      places the resulting uo in <file>.  It first compiles
 *                      any .sml or .int files in the source list.
 *    -o <file>      -- Create an executable from the specified source or object
 *                      files and place the resulting executable in <file> it
 *                      first compiles and links the sources.
 * If none of the above arguments is specified, i.e. tilc <file1>...<fileN>, it
 * is equivalent to tilc -o a.out <file1>...<fileN>.
 * 
 * Note that these options can be interspersed throughout the files, but
 * the relative ordering of the source files is important. For example:
 *
 *    tilc -o output.exe file1.sml file2.int file3.sml  
 * is the same as 
 *    tilc file1.sml file2.int -o output.exe file3.sml
 * but both are different from
 *    tilc -o output.exe file2.sml file1.int file3.sml
 * all three of those are different from
 *    tilc file1.sml -o file2.int output.exe file3.sml 
 * This last one will cause an error (output.exe is not a valid source file)
 *
 * -o without the -r switch creates a tempory .uo file that is deleted at the
 * end of compilation.
 * 
 *)

signature MANAGER = sig

  val chat_ref : bool ref
  val diag_ref : bool ref

  val eager : bool ref
  val cache_context : int ref
  val flush_cache : unit -> unit

  val stat_each_file : bool ref
  val tilc : string * bool * string option * string option * string list  -> unit  (* mapfile * args  *)
  val command : string * string list  -> int  (* to be exported *)
end
