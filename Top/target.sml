(*$import TARGET Stats Word32 Util Platform Linkrtl *)

structure Target :> TARGET =
struct

    (* Platform string depends on a few flags which lead to
     * incompatibilities.  We can support 32 flags.  *)
    val importantFlags =
	[("PtrWriteBarrier", Linkrtl.ptrWriteBarrier, true),
	 ("FullWriteBarrier", Linkrtl.fullWriteBarrier, true),
	 ("MirrorGlobal", Linkrtl.mirrorGlobal, true),
	 ("MirrorPtrArray", Linkrtl.mirrorPtrArray, false)]

    (* flagsString : (bool ref * bool) list -> string *)
    fun flagsString flags =
	let
	    fun flagBit (flag, default) = if !flag = default then 0w0 else 0w1
	    fun flagsWord (nil, _, acc) = acc
	      | flagsWord (flag :: flags, i, acc) = flagsWord (flags, i+0w1,
							       Word32.orb (acc, Word32.<< (flagBit flag, i)))
	    val w = flagsWord (flags, 0w0, 0w0)
	in
	    if w = 0w0 then ""
	    else "-" ^ Word32.toString w
	end

    val error = fn x => Util.error "target.sml" x
    val littleEndian = Stats.tt("littleEndian")
	
    datatype platform = TIL_ALPHA | TIL_SPARC (* | MLRISC_ALPHA | MLRISC_SPARC *)

    (* defaultPlatform : unit -> platform *)
    fun defaultPlatform () =
	(case Platform.platform()
	   of Platform.SOLARIS => (print "Sun detected.  Using Til-Sparc.\n";
				   TIL_SPARC)
	    | Platform.DUNIX => (print "Alpha detected.  Using Til-Alpha.\n";
				 TIL_ALPHA)
	    | _ => (print "Unsupported platform detected.  Using Til-Alpha.\n";
		    TIL_ALPHA))
	      
    val targetPlatform = ref (defaultPlatform ())

    (* getTargetPlatform : unit -> platform *)
    fun getTargetPlatform() = !targetPlatform

    (* setTargetPlatform : platform -> unit *)
    fun setTargetPlatform p = 
	let val little = (case p of
			      TIL_ALPHA => true
			    | TIL_SPARC => false)
(*			    | MLRISC_ALPHA => true  *)
(*			    | MLRISC_SPARC => false *)
	in  targetPlatform := p;
	    littleEndian := little
	end

    (* platformName : platform -> string *)
    fun platformName TIL_ALPHA = "alpha"
      | platformName TIL_SPARC = "sparc"
(*       | platformString' MLRISC_ALPHA = "mlrisc-alpha" *)
(*       | platformString' MLRISC_SPARC = "mlrisc-sparc" *)

    (* platformFromName : string -> platform *)
    fun platformFromName "alpha" = TIL_ALPHA
      | platformFromName "sparc" = TIL_SPARC
      | platformFromName platform = error ("unknown platform name " ^ platform)
	
    (* platformString : unit -> string *)
    val importantFlags' : (bool ref * bool) list =
	map (fn (_,ref_cell,default) => (ref_cell, default)) importantFlags
    fun platformString () = (platformName (getTargetPlatform()) ^ flagsString importantFlags')

    (* native : unit -> bool *)
    fun native () = 
	(case (getTargetPlatform(), Platform.platform()) of
	     (TIL_ALPHA, Platform.DUNIX) => true
	   | (TIL_ALPHA, _) => false
	   | (TIL_SPARC, Platform.SOLARIS) => true
	   | (TIL_SPARC, _) => false)
(* 	   | _ => error "MLRISC not supported" *)

    fun checkNative () = if native() then ()
			 else error "No backend exists for this platform."
			     
end
