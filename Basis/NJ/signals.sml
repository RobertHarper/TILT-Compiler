(* signals.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * These are the two basic interfaces to the run-time system signals support.
 * The interface covers the basic signals operations, plus a small collection
 * of standard signals that should be portable to non-UNIX systems.
 *
 *)

structure Signals :> SIGNALS =
  struct

    structure CI = CInterface

    fun signalFn x = CI.c_function "SMLNJ-Signals" x

    datatype signal = SIG of CI.system_const

    datatype sig_action
      = IGNORE
      | DEFAULT
      | HANDLER of (signal * int * unit cont) -> unit cont

    fun sigToConst (SIG sc) = sc
    fun constToSig sc = SIG sc

  (* the list of supported signals, its length, and the maximum signal code.
   * We assume that the signal codes do not change, but that the number of
   * supported signals might vary between versions of the run-time system.
   *)
    type sig_info = {act : sig_action, mask : int, signal : signal}
    local
      val listSignals' : unit -> CI.system_const list = signalFn "listSignals"
      fun findMax sigs =
	    List.foldl
	      (fn (SIG(sigId, _), id) => if (id < sigId) then sigId else id)
		~1 sigs
    in
    val sigList : signal list ref = ref []
    val numSigs = ref 0
    val maxSig = ref ~1
    val sigTbl : sig_info option Array.array ref =
	  ref(Array.fromList[])
(** DEBUG **)
val debug : string -> unit = CInterface.c_function "SMLNJ-RunT" "debug"
fun getInfo sigId = (case (Array.sub(!sigTbl, sigId))
       of NONE => (
	    debug(String.concat[
		"\n*** Internal error: undefined sigTbl entry for signal ",
		Int.toString sigId, " ***\n"
	      ]);
	    raise Option.Option)
	| (SOME info) => info
      (* end case *))
(****
    fun getInfo sigId = (case (Array.sub(!sigTbl, sigId))
	   of NONE => raise Option.Option
	    | (SOME info) => info
	  (* end case *))
****)
    fun setInfo (sigId, info) = Array.update(!sigTbl, sigId, SOME info)
    fun resetList () = (
	  sigList := List.map constToSig (listSignals' ());
	  numSigs := List.length(! sigList);
	  maxSig := findMax(!sigList);
	  sigTbl := Array.array(!maxSig + 1, NONE))
    end

  (* list the signals (and their names) supported by this version *)
    fun listSignals () = (! sigList)

  (* return the name of a signal *)
    fun toString (SIG(_, name)) = name

  (* return the signal with the corresponding name; returns NONE, if
   * no such signal exists.
   *)
    fun fromString name = (
	  case CI.findSysConst (name, List.map sigToConst (!sigList))
	   of NONE => NONE
	    | (SOME sc) => SOME(SIG sc)
	  (* end case *))

  (* these run-time functions deal with the state of a signal in the system. *)
    val getSigState : CI.system_const -> int		= signalFn "getSigState"
    val setSigState : (CI.system_const * int) -> unit	= signalFn "setSigState"
  (* The states are defined as: *)
    val ignoreSigState = 0
    val defaultSigState = 1
    val enabledSigState = 2

  (* clear the signal table of handlers *)
    fun clearSigTbl _ = Array.modify (fn _ => NONE) (!sigTbl)

  (* initialize the signal table to the inherited process environment *)
    fun initSigTbl _ = let
	  fun initSig (s as (SIG sigId)) = let
		val state = getSigState sigId
		fun setState st = setInfo (#1 sigId, {act=st, mask=0, signal=s})
		in
		  if (state = ignoreSigState) then setState IGNORE
		  else if (state = defaultSigState) then setState DEFAULT
		  else (* state = enabledSigState *)
		    raise Fail "unexpected signal handler"
		end
	  in
	    resetList ();
	    List.app initSig (! sigList)
	  end

  (* reset the signal environment to agree with the signal table *)
    fun resetSigTbl _ = let
	  val oldSigTbl = !sigTbl
	  val oldList = !sigList
	  fun copy (SIG sigId) = (case (Array.sub(oldSigTbl, #1 sigId))
		 of NONE => ()
		  | (SOME info) => (
		      setInfo (#1 sigId, info);
		      case (#act info)
		       of IGNORE => setSigState(sigId, ignoreSigState)
			| DEFAULT => setSigState(sigId, defaultSigState)
			| (HANDLER _) => setSigState(sigId, enabledSigState)
		      (* end case *))
		(* end case *))
(** NOTE: we should probably notify the user that old signal handlers
 ** are being lost, but there is no good way to do this right now.
 **)
		  handle _ => ()
	  in
	    resetList ();
	    List.app copy oldList
	  end

  (* signal masking. *)

    datatype sigmask
      = MASKALL
      | MASK of signal list

  (* increment the masking level of the given signal by one, and return true.
   *)
    fun incMask (SIG(sigId, _)) = let
	  val {act, mask, signal} = getInfo sigId
	  in
	    setInfo(sigId, {act=act, mask=mask+1, signal=signal});
	    true
	  end
  (* decrement the masking level of the given signal by one; return true,
   * if the signal is still masked.
   *)
    fun decMask (SIG(sigId, _)) = let
	  val {act, mask, signal} = getInfo sigId
	  in
	    (mask <> 0) andalso (
	      setInfo(sigId, {act=act, mask=mask-1, signal=signal});
	      (mask <> 1))
	  end

    local
    (* Run-time system API:
     *   NONE   -- empty mask
     *   SOME[] -- mask all signals
     *   SOME l -- mask the signals in l
     *)
      val setSigMask   : CI.system_const list option -> unit = signalFn "setSigMask"
      val getSigMask : unit -> CI.system_const list option = signalFn "getSigMask"
      fun applyMask maskFn mask = let
	  (* a simple insertion sort to eliminate duplicates *)
	    fun insert (s as SIG(id, _), []) = [s]
	      | insert (s as SIG(id, _), (s' as SIG(id', _))::r) =
		  if (id < id')
		    then s :: s' :: r
		  else if (id = id')
		    then s' :: r
		    else s' :: insert(s, r)
	    val sort = List.foldl insert []
	  (* apply the masking operations, return a list of signals that must
	   * be masked/unmasked at the OS level.
	   *)
	    fun f ([], l, n) = (l, n)
	      | f (s::r, l, n) =
		  if (maskFn s)
		    then f (r, (sigToConst s)::l, n+1)
		    else f (r, l, n)
	    val (l', numMasked) = (case mask
		  of MASKALL => f (!sigList, [], 0)
		   | (MASK l) => f (sort l, [], 0)
		(* end case *))
	    in
	      if (numMasked = 0) then NONE
	      else if (numMasked = !numSigs) then SOME[]
	      else SOME l'
	    end
    in
    fun maskSignals mask = (case (applyMask incMask mask)
	   of NONE => ()
	    | m => setSigMask m
	  (* end case *))
    fun unmaskSignals mask = (case (applyMask decMask mask)
	   of SOME[] => ()
	    | m=> setSigMask m
	  (* end case *))
    fun masked () = (case getSigMask()
	   of NONE => MASK[]
	    | SOME[] => MASKALL
	    | SOME l => MASK(List.map constToSig l)
	  (* end case *))
    end (* local *)

  (* set the handler for a signal, returning the previous action. *)
    fun setHandler (s as (SIG sigId), act) = let
	  val _ = maskSignals MASKALL
	  val {act=oldAct, mask, ...} = getInfo(#1 sigId)
	  in
	    case (act, oldAct)
	     of (IGNORE, IGNORE) => ()
	      | (DEFAULT, DEFAULT) => ()
	      | (HANDLER _, HANDLER _) =>
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s})
	      | (IGNORE, _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, ignoreSigState))
	      | (DEFAULT, _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, defaultSigState))
	      | (HANDLER _, _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, enabledSigState))
	    (* end case *);
	    unmaskSignals MASKALL;
	    oldAct
	  end

  (* if a signal is not being ignored, then set the handler.  This
   * returns the previous handler (if IGNORE, then the current handler
   * is still IGNORE).
   *)
    fun overrideHandler (s as (SIG sigId), act) = let
	  val _ = maskSignals MASKALL
	  val {act=oldAct, mask, ...} = getInfo(#1 sigId)
	  in
	    case (oldAct, act)
	     of (IGNORE, _) => ()
	      | (DEFAULT, DEFAULT) => ()
	      | (HANDLER _, HANDLER _) =>
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s})
	      | (_, IGNORE) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, ignoreSigState))
	      | (_, DEFAULT) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, defaultSigState))
	      | (_, HANDLER _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, enabledSigState))
	    (* end case *);
	    unmaskSignals MASKALL;
	    oldAct
	  end

  (* get the current action for the given signal *)
    fun inqHandler (SIG(sigId, _)) = #act(getInfo sigId)

  (* sleep until the next signal; if called when signals are masked,
   * then signals will still be masked when pause returns.
   *)
    val pause : unit -> unit = signalFn "pause"

  (* install some clean-up routines.  When we export/import ML images, we want
   * the signal environment to persist, but when we export an ML function, then
   * we want to start with a clean signal state on restart (actually, we want
   * to use the inherited signal envirnoment, so that background jobs work).
   *)
    val _ = let
	  open CleanUp
	  in
	  (* initialize the table to the inherited state *)
	    initSigTbl();
	  (* install cleaning actions *)
	    addCleaner ("Signals.exportFn", [AtExportFn], clearSigTbl);
	    addCleaner ("Signals.initFn", [AtInitFn], initSigTbl);
	    addCleaner ("Signals.init", [AtInit], resetSigTbl)
	  end

  (* Here is the ML handler that gets invoked by the run-time system.
   * It is responsible for dispatching the appropriate ML handler.
   *)
    fun sigHandler (code, count, resume_k) = (
	  case (Array.sub(!sigTbl, code))
	    of (SOME{act=HANDLER handler, mask=0, signal}) =>
		  handler(signal, count, resume_k)
	     | _ => raise Fail "inconsistent internal signal state"
	  (* end case *))

  (* Install the root handler *)
    val _ = (Assembly.sighandler := sigHandler)

  (* these signals should be supported even on non-UNIX platforms. *)
    val sigINT  = Option.valOf(fromString "INT")
    val sigALRM = Option.valOf(fromString "ALRM")
    val sigTERM = Option.valOf(fromString "TERM")
    val sigGC   = Option.valOf(fromString "GC")

  end; (* Signals *)


(*
 * $Log$
# Revision 1.1  98/03/09  19:52:27  pscheng
# added basis
# 
 * Revision 1.3  1997/02/26  21:00:28  george
 *    Defined a new top level Option structure. All 'a option related
 *    functions have been moved out of General.
 *
 * Revision 1.2  1997/01/31  20:39:48  jhr
 * Replaced uses of "abstraction" with opaque signature matching.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:20  george
 *   Version 109.24
 *
 *)
