signature LILCLOSURE_ANALYZE = 
  sig

    val staticalloc : bool ref
    val chatlev : int ref

    type funentry = {static : {code_lbl : Lil.label,     (* The new code name*)
			       venv_var : Lil.var        (* The new value env variable *)
			       },
		     escape : bool,               (*Escape in the sense that we will generate a closure
						   * for it.  This may or may not happen even for 
						   * "non-escaping" functions, depending on where
						   * we choose to generate direct calls.
						   *)
		     callee : (ClosureState.fid * ClosureState.State.state) list,  (*Functions which are called directly. *)
		     escapee : (ClosureState.fid * ClosureState.State.state) list, (*Functions for which we must create a local closure *)
		     frees : ClosureState.Frees.frees                              (* Free variables of the function *)
		     }

    (* Return a set of funentries describing
     * the functions to be closure converted, and a set of
     * variables that are expected to be globals (statically allocated)
     * by the analyzer.  Note that a function name may also appear in the
     * global set, implying that its environment and closure should also
     * be statically allocated.
     *)
    val findfv_module : Lil.var -> Lil.module -> (funentry Name.VarMap.map * Lil.label Name.VarMap.map)
  end