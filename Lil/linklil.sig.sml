signature LINKLIL =
sig
    val LinkLilDiag : bool ref
    structure Pplil : PPLIL

      
    val nil_to_lil  : string * Nil.module -> Lil.module
    val nilint_to_lilint  : string * Nil.interface -> Lil.interface

end
