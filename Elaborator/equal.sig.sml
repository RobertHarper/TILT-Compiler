(*$import ILCONTEXT *)
(* Equality compiler *)
signature EQUAL = 
  sig

    structure IlContext : ILCONTEXT

    val debug : bool ref
	
    (* Equality at a type *)
    val compile : {polyinst_opt : 
		     IlContext.context * IlContext.Il.sdecs -> 
		       (IlContext.Il.sbnd list * IlContext.Il.sdecs * IlContext.Il.con list) option,
		   vector_eq : IlContext.context -> IlContext.Il.exp * IlContext.Il.con,
		   context : IlContext.context,
		   con : IlContext.Il.con} -> IlContext.Il.exp option

    (* Equality at a recursive type - returns a tuple of functions *)
    val compile_mu : {polyinst_opt :
		        IlContext.context * IlContext.Il.sdecs -> 
		          (IlContext.Il.sbnd list * IlContext.Il.sdecs * IlContext.Il.con list) option,
		      vector_eq : IlContext.context -> IlContext.Il.exp * IlContext.Il.con,
		      context : IlContext.context,
		      confun  : IlContext.Il.con} -> IlContext.Il.exp option
 

end
