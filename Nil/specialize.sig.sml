(* A two-pass optimizer to remove unnecesssarily polymorphic code *)

signature SPECIALIZE =
  sig
      val SpecializeDiag : bool ref
      val debug : bool ref
      (* Print debugging information *)

      val optimize : Nil.module -> Nil.module
  end
