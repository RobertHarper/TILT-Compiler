(*$import Listops Core Sparc SparcUtils CALLCONV Util *)

structure SparcCallconv
    :> CALLCONV where Machine = Sparc.Machine =
struct

  structure Machineutils = Sparcutils
  open Machineutils 
  open Sparc
  open Machine
  open Core

  datatype formals = 
	FORMALS of {args : register list,
		    results : register list}

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
      arguments for a function with all its arguments to be
      placed in the given registers.  
      If waste is true, use the C convention that coresponding 
		integer and floating-point registers are never both used. *)

  fun assignRegsAmong (waste,useI) actuals (iFormals, fFormals) =
    let fun folder (r, (iRegs, fRegs, pos)) =
	(case (r, iRegs, fRegs) of
	   (R _, ir :: irest, [])          => (IN_REG ir, (irest, [], pos))
	 | (R _, ir :: irest, 
		 fall as (_ :: frest))     => (IN_REG ir, (irest, if waste then frest else fall, pos))
         | (R _, [], [])                   => (ON_STACK (THIS_FRAME_ARG4 pos), ([], [], pos + 1))
	 | (R _, [], fall as (_ :: frest)) => (ON_STACK (THIS_FRAME_ARG4 pos), 
						([], if waste then frest else fall, pos + 1))
	 | (F _, [], fr :: frest)         => (IN_REG fr, ([], frest, pos))
	 | (F _, iall as (_ :: irest), fr :: frest) => (IN_REG fr, 
							(if waste then irest else iall, frest, pos))
	 | (F _, [], [])                  => (ON_STACK (THIS_FRAME_ARG8 pos), ([], [], pos + 2))
	 | (F _, iall as [_], [])         => (ON_STACK (THIS_FRAME_ARG8 pos), 
							(if waste then [] else iall, [], pos + 2))
	 | (F _, iall as (ir::_::irest), [])  => 
	       if useI
		   then (IN_REG ir, (irest, [], pos))
	       else (ON_STACK (THIS_FRAME_ARG8 pos), (if waste then irest else iall, [], pos + 2)))
    in  #1(Listops.foldl_acc folder (iFormals,fFormals,0) actuals)
    end

  fun std_c (FORMALS {args,results}) =
     let
	 val actual_args    = assignRegsAmong (true,true) args (C_int_args, [])
         val actual_results = assignRegsAmong (true,false) results (C_int_res, C_fp_res)
     in  LINKAGE{argCaller=actual_args,
		 resCaller=actual_results,
		 argCallee=actual_args,
		 resCallee=actual_results}
     end

  fun unknown_ml (FORMALS {args,results}) =
     let val actual_Caller_args = unknownArgPositions THIS_FRAME_ARG4 args 
	 val actual_Callee_args = unknownArgPositions CALLER_FRAME_ARG4 args 
	 val actual_results = assignRegsAmong (false,false) results (indirect_int_res, indirect_fp_res)
      in LINKAGE{argCaller=actual_Caller_args,
		 resCaller=actual_results,
		 argCallee=actual_Callee_args,
		 resCallee=actual_results}
      end


end


  



