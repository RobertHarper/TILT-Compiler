(*
	Update plans compilation and implements cut-off recompilation.
	There are two reasons update will decide to compile/assemble a
	unit or interface.  First, if a unit or interface has changed
	since the last compilation, then it needs to be recompiled.  A
	unit or interface changes when its source code is changed, its
	list of opened units is changed, a supporting unit's interface
	is changed, or there was no previous compilation.  The info
	files written by the compiler are used to decide this
	question.  Second, if a unit or interface has not changed but
	some of its targets (asm, obj, etc files) do not exist, then
	those targets need to be created.  In this case, it is safe
	for the manager to use the unit's pinterface (if it exists)
	because it will not change during recompilation.  This
	situation can arise, for example, if the user changes Compiler
	flags like UptoElaborate between compilations.

	Note that out of date targets are deleted if the user has set
	flags that prevent compilation.
*)
signature UPDATE =
sig

    val UpdateDiag : bool ref
    val ShowPlan : bool ref
    val ShowStatus : bool ref
    val Cutoff : bool ref

    datatype plan =
	EMPTY
      | COMPILE
      | GENERATE		(* compile, but pinterface up to date *)
      | ASSEMBLE

    val plan : IntSyn.desc * IntSyn.pdec -> plan
end
