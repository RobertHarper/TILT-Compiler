(*$import *)

(* Intermediate code fails to type-check.  Turn on doTypecheckAfterOpt
   to see error. *)

datatype 'a foo = A of 'a bar
     and 'a bar = B of 'a foo

(* This is fine: datatype foo = A of bar and bar = B of foo *)

(* This also fails (and is useful): datatype 'a foo = A of 'a bar and 'a bar = B of 'a foo | C of 'a *)

