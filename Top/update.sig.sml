(*
    This structure plans compilation and implements incremental
    recompilation.

    When an interface or unit is compiled, a summary of the compiler's
    inputs is saved to disk.  If this summary is not on disk, is
    malformed, or does not agree with the current project description,
    then the plan is to recompile.  Otherwise, any existing compiled
    files are up to date: The plan may be empty or may call for
    certain files to be regenerated.

    Out of date targets are deleted if the user has set flags that
    prevent compilation.
*)
signature UPDATE =
sig

    val UpdateDiag : bool ref
    val ShowPlan : bool ref
    val ShowStatus : bool ref
    val Cutoff : bool ref

    datatype plan =
	EMPTY
    |	COMPILE of  (* flags indicate what interfaces are up to date *)
	    {pinterface:bool, tali:bool}
    |	ASSEMBLE

    val plan : IntSyn.inputs -> plan
end
