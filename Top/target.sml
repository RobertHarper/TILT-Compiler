structure Target :> TARGET =
struct

    val error = fn x => Util.error "target.sml" x

    val target : Platform.objtype ref =
	ref Platform.SPARC	(* temporary *)

    fun setTarget (objtype : Platform.objtype) : unit =
	let val _= target := objtype
	    val _ =
		(case objtype of
		    Platform.TALx86 => CompilerControl.LilDefaults()
		|   _ => CompilerControl.RtlDefaults())
	in  ()
	end

    fun getTarget () : Platform.objtype = !target

    val _ =
	let val default =
		(case (Platform.cputype()) of
		    Platform.SUPPORTED objtype => objtype
		|   Platform.UNSUPPORTED => Platform.SPARC)
	in  setTarget default
	end

    (*
	Target string depends on a few flags which lead to binary
	incompatibilities.  We can support 32 flags.
    *)
    val importantFlags : (bool ref * bool) list =
	[(Linkrtl.ptrWriteBarrier, true),
	 (Linkrtl.fullWriteBarrier, true),
	 (Linkrtl.mirrorGlobal, false),
	 (Linkrtl.mirrorPtrArray, false),
	 (Core.branchingTraps, true)]

    fun flagsString (flags : (bool ref * bool) list) : string =
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

    fun targetString () : string = (Platform.toString (!target) ^ flagsString importantFlags)

    fun native () : bool =
	(case (!target, Platform.cputype())
	   of (objtype, Platform.SUPPORTED objtype') => objtype = objtype'
	    | (_, Platform.UNSUPPORTED) => false)

    fun checkNative () : unit =
	if native() then ()
	else error ("can not link " ^ Platform.toString (!target) ^ " binaries")

end
