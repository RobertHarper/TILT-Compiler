(*
	C library functions needed prior to the TiltExtern unit.
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
