signature LINKLIL =
sig
    val LinkLilDiag : bool ref
    structure Pplil : PPLIL

      
    val nil_to_lil  : string * Nil.module -> Lil.module

end
