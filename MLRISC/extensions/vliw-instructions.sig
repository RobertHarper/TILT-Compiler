(*
 * VLIW instructions involve functional unit assignments
 *)

signature VLIW_INSTRUCTIONS =
sig

   include INSTRUCTIONS
   structure FU : FUNITS       (* functional unit assignment *)
   structure X  : CROSSPATHS   (* for clustered architectures *)

end
