(*

Here are examples demonstrating the use of "load.sml" to run
the compiler.

Set the environment variable TILTROOT to the root of the ml96
hierarchy:

	csh% setenv TILTROOT `pwd`
	sh$ TILTROOT=`pwd`; export TILTROOT


Run SML/NJ with "sml"; all of the examples below should be entered at
the SML/NJ prompt "-".

use "load.sml";	(* compile TILT and the Load interface with NJ *)
Load.toExe();	(* arrange for TILT to generate executables *)
		(* see load.sml for other options *)
Boot.boot();	(* compile Basis with TILT *)
Manager.make "mapfile-all";
		(* compile TILT with TILT *)
Manager.make "Bench/mapfile-timings";
		(* compile benchmarks with TILT *)
Load.make();    (* recompile the compiler after some source *)
	         * has been changed. *)



A brief introduction to TILT's compilation manager is Doc/tilt.1 (a
manual page).  More details can be found in Doc/TM.txt.
*)

(*
 * This file provides some useful functions for managing the compiler 
 * from the SML/NJ prompt during development.  When an interactive session 
 * is first begun, typing   use "load.sml";   will load the compiler
 * and establish some sane default settings for development purposes.  In addition
 * a structure Load with the following signature will be defined.
 * - Leaf (8/10/02)
 *)

signature LOAD = 
  sig

    (*If set to true, then the settings managed by the load file will be
    * persistent across compiles.
    *)
    val sticky : bool ref

    (* Have SML/NJ recompile the compiler as necessary, and relink it in any case.  
     * Note: all compiler flags except those managed by this structure
     * will be restored to their defaults.
     *)
    val make : unit -> unit


    (*
     * Stop compiling after translation to assembly.
     *)
    val toAsm : unit -> unit

    (*
     * Go all the way to an executable. (default)
     *)
    val toExe : unit -> unit

    (* typecheck true turns on all NIL typechecking,
     * typecheck false turns off all NIL typechecking.
     *)
    val typecheck : bool -> unit

    (* show true turns on all printing, from HIL to RTL
     * show false turns off all printing, from HIL to RTL
     *)
    val show : bool -> unit

    (* Return all load controlled values to their defaults.
     *)
    val reset : unit -> unit

    (* typecheck_some true turns on some NIL typechecking 
     * typecheck_some false turns off some NIL typechecking.
     *
     * Which phases are checked is controlled by the contents 
     * of typecheck_which (below)
     *
     * By default, the output of Phasesplit, Optimize1, Optimize2 
     * and ClosureConv will be checked.
     *)
    val typecheck_some : bool -> unit

    (* show_some true turns on some NIL printing.
     * show_some false turns off some NIL printing.
     *
     * Which phases are printed is controlled by the contents 
     * of show_which (below)
     *
     * By default, the output of Phasesplit, Optimize1, Optimize2 
     * and ClosureConv will be printed.
     *)
    val show_some : bool -> unit

    (* show_hil true turns on HIL printing.
     * show_hil false turns off HIL printing.
     *)
    val show_hil : bool -> unit

    (* Manage the sets of phases controlled by the typecheck_some and show_some
     * functions. For example:
     *
     * also_show "Phasesplit" adds Phasesplit to the list of passes
     * whose output is printed by show_some.
     *
     * dont_show "Phasesplit" removes Phasesplit from the list of passes
     * whose output is printed by show_some.
     *)
    val also_show : string -> unit
    val also_typecheck : string -> unit
    val dont_show : string -> unit
    val dont_typecheck : string -> unit

end;



structure LoadVars = 
  struct
    val sticky = ref true
    val first  = ref true

    val show = ref false;
    val show_some = ref false;
    val typecheck = ref false;
    val typecheck_some = ref false

    (* Contains the Stats.bool flags for the phases to be shown.
     * See load2.sml for an example of setting this variable.
     *)

    val show_which : string list ref = ref []

    (* Contains the Stats.bool flags for the phases to be typechecked.
     * See load2.sml for an example of setting this variable.
     *)
    val typecheck_which : string list ref =  ref []

    val show_hil = ref false

    val toAsm = ref false;
    val toExe = ref false;
  end;

val _ = use "load2.sml";
