(* This structure is to try and unify some of the 
 * control flags needed to differentiate LIL and RTL compilation
 * into one location, instead of a tangle of calls to Stats.bool.
 * This is especially useful because NJ doesn't let us control
 * the order of effects.
 *)

signature COMPILERCONTROL = 
  sig

    val ClosureConvertNil : bool ref
    val NilTargetsLil : bool ref
    val ReifyTraces : bool ref
    val ArrowIsTaglike : bool ref
    val RefIsArray : bool ref
    val EliminateSingletons : bool ref 
    val UncurryPushCbnds : bool ref
    val UncurryPolymorphism : bool ref
    val SpecializeArrayofChar : bool ref

    (* By default, the RtlDefaults are set *)

    (* Set the control flags appropriately for compiling to Rtl *)
    val RtlDefaults : unit -> unit

    (* Set the control flags appropriately for compiling to LIL/TAL *)
    val LilDefaults : unit -> unit
  end