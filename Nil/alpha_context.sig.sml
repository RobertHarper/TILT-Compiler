(*$import Nil Name *)

signature ALPHA = 
  sig

    type var = Name.var
    
    type alpha_context

    val renamed : alpha_context * var -> bool 

    val bound : alpha_context * var -> bool 

    val substitute : alpha_context * var -> var

    val rename : alpha_context * var * var -> alpha_context 

    val bind : alpha_context * var -> alpha_context

    val unbind : alpha_context * var -> alpha_context 

    val empty_context : unit -> alpha_context

    val alpha_bind : (alpha_context * var) -> (alpha_context * var)
    val alpha_bind_list : (alpha_context * (var list)) -> (alpha_context * (var list))

    val alpha_equate_pair : (alpha_context * alpha_context) * (var * var)
      -> alpha_context * alpha_context

    val alpha_equate_pairs :
      (alpha_context * alpha_context) * (var list * var list)
           -> alpha_context * alpha_context
  
    val alpha_pair_eq : (alpha_context * alpha_context) * (var * var) -> bool
   
end