(* ../../Basis/Pextern.sml *)
(* ../../Basis/extern.sml *)
(*
	C library functions used by the basis.
*)

(* ../Runtime/port/portexn.c *)
extern exnNameRuntime : (exn, string) -->
extern exnMessageRuntime : (exn, string) -->

(* ../Runtime/port/syserror.c *)
extern syserror_msg : (int, string) -->

(* ../Runtime/port/portexn.c *)
extern registerSubExnRuntime : (exn,unit) -->
extern registerDivExnRuntime : (exn,unit) -->
extern registerOvflExnRuntime : (exn,unit) -->
extern registerSysErrExnRuntime : (exn,unit) -->
extern registerLibFailExnRuntime : (exn,unit) -->

(* ../Runtime/port/ccall.c *)
extern ccall_errmsg  : (word,string) -->

(* libc; used by Numeric/real64.sml *)
extern sqrt : (real, real) -->
extern exp : (real, real) -->
extern ln : (real, real) -->
extern log10 : (real, real) -->
extern sin : (real, real) -->
extern cos : (real, real) -->
extern tan : (real, real) -->
extern atan : (real, real) -->
extern asin : (real, real) -->
extern acos : (real, real) -->
extern tanh : (real, real) -->
extern sinh : (real, real) -->
extern cosh : (real, real) -->

(* ../Runtime/port/real.c *)
extern real_logb : (real, int) -->
extern real_scalb : (real, int, real) -->

(*
	../Runtime/sparc/fc.c
	../Runtime/talx86/fc.c
*)
extern setfc : (cerr, int, int, unit) -->
extern getfc : (cerr, int * int) -->

(* ../Runtime/port/time.c *)
extern time_gettimeofday : (cerr, int * int) -->

(* ../Runtime/port/timer.c *)
extern timer_getrusage_self : (cerr, rusagerep) -->
extern timer_ftime : (unit, int * int) -->

(* ../Runtime/port/printer.c *)
extern printer_print : (cerr, string, unit) -->
