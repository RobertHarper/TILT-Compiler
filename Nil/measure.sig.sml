(*$import Prelude TopLevel Nil *)

signature MEASURE = 
  sig 

    val chatlev : int ref

    val cstring : Nil.con -> string
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

    type size = {kinds:int,
		 cons:int,
		 exps:int,
		 cvars:int,
		 evars:int,
		 total:int}

    val mod_size   :  measure -> Nil.module -> size

    (*Measure imports, bnds, exports separately *)
    val mod_size'   :  measure -> Nil.module -> (size*size*size)

  end