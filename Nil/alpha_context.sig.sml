(*
 Contexts and routines for alpha-renaming of variables
*)

signature ALPHA =
  sig

    type var = Name.var

    type alpha_context

    val renamed : alpha_context * var -> bool
    (* renamed (ctxt, v) ==> true iff v is renamed to a different variable in ctxt *)

    val is_empty : alpha_context -> bool
    (* is_empty ctxt ==> true iff ctxt is empty *)

    val bound : alpha_context * var -> bool
    (* bound (ctxt, v) ==> true iff v is bound in ctxt *)

    val substitute : alpha_context * var -> var
    (* substitute (ctxt, v) ==> the replacement variable assigned to v by ctxt *)

    val rename : alpha_context * var * var -> alpha_context
    (* rename (ctxt, v1, v2) ==> ctxt modified to replace v1 with v2 *)

    val bind : alpha_context * var -> alpha_context
    (* bind (ctxt, v) ==> ctxt modified to include v as a bound variable with no replacement *)

    val unbind : alpha_context * var -> alpha_context
    (* unbind (ctxt, v) ==> ctxt with any binding/renaming for v removed *)

    val empty_context : unit -> alpha_context
    (* empty_context () => an alpha context with no bound or renamed variables *)

    val alpha_bind : (alpha_context * var) -> (alpha_context * var)
    (* alpha_bind (ctxt, v) ==> (ctxt modified to rename v to a fresh variable,
	                         the fresh variable to which v is renamed) *)

    val alpha_bind_list : (alpha_context * (var list)) -> (alpha_context * (var list))
    (* alpha_bind_list (ctxt, vlist) ==> (ctxt modified to rename each variable in vlist to a fresh variable,
	                                  the variables to which the members of vlist have been renamed, in order) *)

    val alpha_equate_pair : (alpha_context * alpha_context) * (var * var)
      -> alpha_context * alpha_context
    (* alpha_equate_pair ((ctxt1, ctxt2), (v1, v2)) ==> (ctxt1', ctxt2'),
	where ctxt1' and ctxt2' are created from ctxt1 and ctxt2 by making any changes needed to ensure that
	ctxt1 treats v1 the same way that ctxt2 treats v2 *)

    val alpha_equate_pairs :
      (alpha_context * alpha_context) * (var list * var list)
           -> alpha_context * alpha_context
    (* alpha_equate_pairs ((ctxt1, ctxt2), (vlist1, vlist2)) ==> the result of folding alpha_equate_pair over vlist1 and vlist2
	  taken as successive pairs, using ctxt1 and ctxt2 as the starting contexts *)

    val alpha_pair_eq : (alpha_context * alpha_context) * (var * var) -> bool
    (* alpha_pair_eq ((ctxt1, ctxt2), (v1, v2)) ==> true iff v1 under ctxt1's renamings and v2 under ctxt2's renamings are
	equal *)
end
