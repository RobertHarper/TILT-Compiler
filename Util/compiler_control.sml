(* This structure is to try and unify some of the 
 * control flags needed to differentiate LIL and RTL compilation
 * into one location, instead of a tangle of calls to Stats.bool.
 * This is especially useful because NJ doesn't let us control
 * the order of effects.
 *)

structure CompilerControl :> COMPILERCONTROL = 
  struct
    val ClosureConvertNil = Stats.tt "doClosureConv"
    val NilTargetsLil = Stats.ff "NilTargetsLil"
    val ReifyTraces = Stats.tt "ReifyTraces"
    val ArrowIsTaglike = Stats.ff "ArrowIsTaglike"
    val RefIsArray = Stats.tt "RefIsArray"

    (* This is optional for the MIL, but seems to produce slightly
     * better code.
     *)
    val EliminateSingletons = Stats.tt "doSingElim" 
    val UncurryPushCbnds = Stats.ff "UncurryPushCbnds"
    val UncurryPolymorphism = Stats.ff "doPolyUncurry"
    val SpecializeArrayofChar = Stats.tt "SpecializeArrayofChar"

    fun RtlDefaults () = 
      (
       ClosureConvertNil := true;
       NilTargetsLil := false;
       ReifyTraces := true;
       ArrowIsTaglike := false;
       RefIsArray := true;
       EliminateSingletons := true;
       UncurryPushCbnds := false;
       UncurryPolymorphism := false;
       SpecializeArrayofChar := true
       )
    fun LilDefaults () = 
      (
       ClosureConvertNil := false;
       NilTargetsLil := true;
       ReifyTraces := false;
       ArrowIsTaglike := true;
       RefIsArray := false;
       EliminateSingletons := true;
       UncurryPushCbnds := true;
       UncurryPolymorphism := true;
       SpecializeArrayofChar := false
       )
  end