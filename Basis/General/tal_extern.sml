(* The runtime needs to know about these exceptions.  
 * we register them with the runtime by calling these c functions.
 * Note that all that matters is the stamp: the values passed 
 * with SysErr and LibFail are ignored.
 *)
extern registerDivExnRuntime : (exn,unit) -->
extern registerOvflExnRuntime : (exn,unit) -->
extern registerSysErrExnRuntime : (exn,unit) -->
extern registerLibFailExnRuntime : (exn,unit) -->


extern exnNameRuntime : (exn, string) -->
extern exnMessageRuntime : (exn, string) -->
