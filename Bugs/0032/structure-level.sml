(*$import Prelude TopLevel Word *)

signature PLUS = sig val + : Word.word * Word.word -> Word.word end

structure PlusWord : PLUS = struct val + = op + end	(* This should still work *)
structure PlusInt         = struct val + = op + end	(* Binding should constrain type. *)
structure PlusFail : PLUS = PlusInt			(* This should fail; used to work. *)
