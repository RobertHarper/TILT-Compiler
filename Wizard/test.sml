val x = Symbol.intern "x";
val y = Symbol.intern "y";

val el_unit = EL.Unit;
val el_uu = EL.Arrow (el_unit, el_unit);
val el_uuu = EL.Arrow (el_unit, el_uu);

val el_k = EL.Lam (x, el_unit, EL.Lam (y, el_unit, EL.Var x));
val el_k' = EL.Ascription (el_k, el_uuu);

val ec = Symbol.SymbolMap.empty;

val il_unit = TC.elab_tipe (ec, el_unit);
val il_uu = TC.elab_tipe (ec, el_uu);
val il_uuu = TC.elab_tipe (ec, el_uuu);

val (il_k, il_kt) = TC.elab_term (ec, el_k);
val (il_k', il_kt') = TC.elab_term (ec, el_k');

val il_kts = TC.synth (ec, il_k);
TC.equiv (ec, il_kts, il_kt);
TC.equiv (ec, il_kts, il_kt');

