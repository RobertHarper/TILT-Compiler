(*$import Nil *)

(*
 Measuring the size of a module, in terms of numbers of imports, bindings, and exports
*)

signature MEASURE = 
  sig 

    val chatlev : int ref
    (* Controls how much extra printed output to produce.
       Greater than 0 gets some output.
       Greater than 1 gets extra ouput, including counts of specific types of constructors. *)

    val cstring : Nil.con -> string
    (* Mapping from a constructor to the string used to identify it in keeping counts *)
      (* Default string translation.
	(case con of
	   Prim_c (Sum_c _,_)    => "Sum_c"
	 | Prim_c (Record_c _,_) => "Record_c"
	 | Prim_c(pc,clist)      => "Prim_c"
	 | AllArrow_c _          => "AllArrow_c"
	 | ExternArrow_c _       => "ExternArrow_c"
	 | Var_c _               => "Var_c"
	 | Let_c _               => "Let_c"
	 | Mu_c _                => "Mu_c"
	 | Proj_c _              => "Proj_c"
	 | Typeof_c _            => "Typeof_c"
	 | App_c _               => "App_c"
	 | Crecord_c _           => "Crecord_c"
	 | Closure_c _           => "Closure_c"
	 | Typecase_c _          => "Typecase_c"
	 | Annotate_c (_,c)      => "Annotate_c")
	   *)
    type measure = {cstring  : Nil.con -> string, 
		    count    : string list,
		    count_in : string list}
      (* cstring = map from constructors to names
         count = names of constructor varieties for which to count the numbers of occurrences
         count_in = names of constructor varieties for which to count the number of constructor nodes in children
	            of their occurrences *)

    type size = {kinds:int,
		 cons:int,
		 exps:int,
		 cvars:int,
		 evars:int,
		 total:int}
    (* measure count results *)

    val mod_size   :  measure -> Nil.module -> size
    (* mod_size meas module ==> counts of components of module, using the constructor string conversion function in meas.
	count and count_in in meas only matter if the chat level is high enough to generate printed output, in which case
	the counts of the constructor varieties they give will be printed.
       Effects: Prints count information with chat level above 1. *)

    val mod_size'   :  measure -> Nil.module -> (size*size*size)
    (* mod_size meas module ==> (impsize, bndsize, exportsize), where these sizes are the results of measuring the
	imports, bindings, and exports of module, respectively.
       Effects: Prints total size information with chat level above 0 *)
  end
