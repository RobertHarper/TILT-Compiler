(*$MEASURE: *)

signature MEASURE =
sig

  exception Fail of string

  val iters : int ref
  val resetflags : unit -> unit

 (* inline: measure effects of various inline sizes.  
  *
  * Creates a list of log files of the form inlinex, where x is
  * the inline size.
  *)
    
  val inline : int list -> unit

  (* test inlining sizes between 0 and 1000, stepped by 100 *)

  val inline_all : unit -> unit

  (* process inline log files and print a table of results:
           * arg 1: directory containing log files
           * arg 2: list of inline sizes
           * arg 3: true = create tex chart, otherwise create 
                    chart with white space between columns
   *)

  val inline_process : string -> int list ->  bool -> unit

  (* record containing performance statistics *)

  type stat

  (* record containing averaged performance statistics *)

  type avg_stat

  (* scan_stats: read in performance statistics printed out by the
     runtime system when a program run ends.

     Takes a string s, an integer i which is the position in s to
     to start reading at.   Returns the statistics and the next
     position in s to start scanning at.*)

  val scan_stats : string * int -> stat * int

  (* scan_file: scan a log file of performance statistics *)

  val scan_file : string -> stat list

  (* print_stat: print out performance statistics *)

  val print_stat : outstream -> stat -> unit

  (* avg_stats: given a list of statistics, compute the arithmetic mean
     and standard deviation for each field in the list.*)

  val avg_stats : stat list -> avg_stat 

   (* ratio: given two performance statistics, compute the ratio
      of each field in the statistics.*)

   val ratio : stat * stat -> stat

   (* run : run a program n times, logging performance statistics.

        * arg 1 : name of log
        * arg 2 : number of times to run program
        * arg 3 : name of programs
   *)

  val run : string -> int -> string -> unit

  (* process_log : given a log of performance statistics, computes the
     arithemtic mean and standard deviation for each field.*)
   

  val process_log : string -> avg_stat

  (* measure_opts: measure the effect of a list of optimizations.
     Turns off each optimization, and then turns one on at a time.
     Log summaries of each optimization to a log file.

        * arg 1 : name of log file

        * arg 2 : true = reuse existing performance log file for
                  each optimization if it exists.   false = recompile
		  and remeasure

        * arg 3 : string is the name of the optimization (should contain
                  no white space), bool ref is the flag enabling the
                  optimization.

		  Logs performance of benchmark with these optimizations
		  to the file {string}-{arg5}, where {string} is the string,
		  and {arg5} is the name of the benchmark.

        * arg 4 : name of bench mark.  It is assumed to be in the file
		  bench/{arg5}

     
   *)
  
  val measure_opts : string -> bool -> 
                     (string * bool ref) list -> string -> 
                     (string * avg_stat) list

  (* measure_loop_opts: measure the effect of loop optimizations for
     a given benchmark.

         * arg 1 : name of log file

         * arg 2 : name of error log file

         * arg 3 : true if we should reuse existing performance logs
                   for individual optimization, false if we should
		   re-run each optimization.

         * arg 4 : name of benchmark 
   *)
          
  val measure_loop_opts : string -> string -> bool -> string ->
                                (string * avg_stat) list

  (* print_ratios: print performance ratios.
   
        * arg 1 : name of log file
        * arg 2 : string is name of the optimization setting, avg_stat is
                  the is average performance for that optimization.
 
    The ratios all relative to the first entry in the list.*)
     
  val print_ratios : string -> (string * avg_stat) list -> stat list

  (* restart_bmarks: start measuring loop opts for benchmarks.
     Reuse existing log information.*)

  val restart_bmarks : unit -> (string * avg_stat) list list

  (* start_bmarks: start measuring loop opts.  Don't reuse existing
     log information.*)

  val start_bmarks : unit -> (string * avg_stat) list list

  (* restart_cm_bmarks: start measuring code motion opts for benchmarks.
     Reuse existing log information.    bool determines whether expressions
     are moved.*)

  val restart_cm_bmarks : bool -> (string * avg_stat) list list

  (* start_bmarks: analogous version, throwing away existing log info *)

  val start_cm_bmarks : bool -> (string * avg_stat) list list


  (* compute the arithmetic mean of a list of floating point numbers *)

  val r_arith_mean : real list -> real

   (* compute the standard deviation of a list of floating point numbers *)

  val r_std_dev : real list -> real

end (* signature MEASURE *)



  




