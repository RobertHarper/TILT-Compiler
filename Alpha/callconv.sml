functor DecalphaCallconv(structure Machineutils : MACHINEUTILS
			 structure Decalpha : DECALPHA
			 sharing Decalpha = Machineutils.Machine) : CALLCONV =
struct
  structure Machine = Machineutils.Machine
  structure Machineutils = Machineutils
  
  open Machineutils Machine Decalpha
  datatype actuals = 
	ACTUALS of {args : Machine.assign list,
	            results : Machine.assign list}

  datatype formals = 
	FORMALS of {args : Machine.register list,
		    results : Machine.register list}

  val error = fn s => Util.error "callconv.sml" s

   (* Return a list of positions corresponding to the given formal 
      arguments for a function using the "unknown" calling convention. *)

   fun argPositions (arg_iregs,arg_fregs) argOffset arg_pseudoregs =
     let fun loop (pseudoreg,iregs,fregs,arg_count) =
	       case pseudoreg
	       of [] => []
	        | (R _ :: rest) =>
		     (case iregs
		      of [] => ON_STACK (argOffset arg_count) ::
			       loop(rest,iregs,fregs,arg_count+1)
		       | h :: t => h :: loop(rest,t,fregs,arg_count))
		| (F _ :: rest) =>
		     (case fregs
		      of [] => ON_STACK (argOffset arg_count) ::
			       loop(rest,iregs,fregs,arg_count+1)
		       | h :: t => h :: loop(rest,iregs,t,arg_count))
     in loop(arg_pseudoregs,arg_iregs,arg_fregs,0)
     end

   (* Return a list of positions corresponding to the given formal
      arguments for a function with all it's arguments to be
      placed in the given registers *)

  val unknownArgPositions = argPositions(map IN_REG indirect_int_args,
					 map IN_REG indirect_fp_args)

  val knownArgPositions = argPositions

  fun assignRegsAmong [] _ _ = []
     | assignRegsAmong ((R _)::rest) (IReg :: restI) FReg = 
       IReg :: (assignRegsAmong rest restI FReg)
     | assignRegsAmong ((F _)::rest) IReg (FReg :: restF) =
       FReg :: (assignRegsAmong rest IReg restF)
     | assignRegsAmong ((R _) :: _) [] _ =
       error "assignRegsAmong:  Ran out of integer result registers"
     | assignRegsAmong ((F _) :: _) _ [] = 
       error "assignRegsAmong:  Ran out of fp result registers"

   (* Return a list of positions corresponding to the given formal
      arguments for a function with all it's arguments to be
      placed in the given registers, using the C convention that
      the nth integer and nth floating-point registers are never
      both used. *)

  fun assignRegsAmongC [] _ _ = []
    | assignRegsAmongC ((R _)::rest) (IReg :: restI) (_ :: restF) =
       IReg :: (assignRegsAmongC rest restI restF)
    | assignRegsAmongC ((F _)::rest) (_ :: restI) (FReg :: restF) =
       FReg :: (assignRegsAmongC rest restI restF)
    | assignRegsAmongC ((R _) :: _) _ _ =
       error "assignRegsAmongC:  Could not allocate integer"
    | assignRegsAmongC ((F _) :: _) _ _ = 
       error "assignRegsAmongC:  Could not allocate floating-point"

  fun std_c def (FORMALS {args,results}) =
     let val actual_args =
	   if (length args <= 6) then
	      map IN_REG (assignRegsAmongC args C_int_args C_fp_args)
 	   else
	      error ("allocateCall: More than 6 args" ^ "passed to C")
         val actual_results = 
	     map IN_REG (assignRegsAmongC results C_int_res C_fp_res)
     in ACTUALS{args=actual_args,
		results=actual_results}
     end

  fun unknown_ml def (FORMALS {args,results}) =
     let val stackloc = if def then CALLER_FRAME_ARG      
	                else THIS_FRAME_ARG
	 val actual_args = unknownArgPositions stackloc args 
	 val actual_results = 	map IN_REG (assignRegsAmong results 
					    indirect_int_res indirect_fp_res)
      in ACTUALS{args=actual_args,
		 results=actual_results}
      end
end


  



