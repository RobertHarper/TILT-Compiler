(*$import Nil *)

(* A two-pass optimizer to remove unnecesssarily polymorphic code *)

signature SPECIALIZE =
  sig
      val debug : bool ref
      (* Print debugging information *)

      val optimize : Nil.module -> Nil.module
  end
