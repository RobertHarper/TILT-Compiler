structure Target :> TARGET =
struct

    (* Platform string depends on a few flags which lead to
     * incompatibilities.  We can support 32 flags.  *)
    val importantFlags =
	[("PtrWriteBarrier", Linkrtl.ptrWriteBarrier, true),
	 ("FullWriteBarrier", Linkrtl.fullWriteBarrier, true),
	 ("MirrorGlobal", Linkrtl.mirrorGlobal, false),
	 ("MirrorPtrArray", Linkrtl.mirrorPtrArray, false),
	 ("BranchingTraps", Core.branchingTraps, true)]

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

    datatype platform = TIL_ALPHA | TIL_SPARC | TIL_TALx86

    structure B = Blaster
    fun blastOutPlatform (os : B.outstream) (p : platform) : unit =
	(case p
	   of TIL_ALPHA  => B.blastOutInt os 0
	    | TIL_SPARC  => B.blastOutInt os 1
	    | TIL_TALx86 => B.blastOutInt os 2)
    fun blastInPlatform (is : B.instream) : platform =
	(case B.blastInInt is
	   of 0 => TIL_ALPHA
	    | 1 => TIL_SPARC
	    | 2 => TIL_TALx86
	    | _ => error "bad platform")
    val (blastOutPlatform,blastInPlatform) =
	B.magic (blastOutPlatform,blastInPlatform,"platform $Revision$")

    (* defaultPlatform : unit -> platform *)
    fun defaultPlatform () =
	(case Platform.platform()
	   of Platform.SOLARIS => TIL_SPARC
	    | Platform.DUNIX   => TIL_ALPHA
	    | Platform.LINUX    => TIL_TALx86
	    | Platform.GENERIC=> TIL_SPARC)

    val targetPlatform = ref (defaultPlatform ())

    (* getTargetPlatform : unit -> platform *)
    fun getTargetPlatform() = !targetPlatform

    (* setTargetPlatform : platform -> unit *)
    fun setTargetPlatform p =
	let val little = (case p of
			      TIL_ALPHA => true
			    | TIL_SPARC => false
			    | TIL_TALx86 => true)
	in  targetPlatform := p;
	    littleEndian := little
	end

    (* platformName : platform -> string *)
    fun platformName TIL_ALPHA  = "alpha"
      | platformName TIL_SPARC  = "sparc"
      | platformName TIL_TALx86 = "talx86"

    fun platformFromName (name : string) : platform option =
	(case name
	   of "alpha"  => SOME TIL_ALPHA
	    | "sparc"  => SOME TIL_SPARC
	    | "talx86" => SOME TIL_TALx86
	    | _ => NONE)
	   
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
	   | (TIL_SPARC, _) => false
	   | (TIL_TALx86, Platform.LINUX) => true
	   | (TIL_TALx86, _) => false)

    fun checkNative () = if native() then ()
			 else error "No backend exists for this platform."

end
