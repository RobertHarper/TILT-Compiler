signature TOPLEVEL =
sig

    val opers : ((Eval.env * Eval.stack) -> (Eval.env * Eval.stack)) Envmap.map

end
