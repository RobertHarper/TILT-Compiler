(*$import *)

(* polymorphic type declarations now only require equality types that are relevant *)
type 'a ref = 'a TiltPrim.ref
type a = TiltPrim.int32
type b = a
 and 'a c = a * 'a			(* eq 'a *)
 and ('a,'b) d = a * 'a * ('b ref)	(* eq 'a *)
 and ('a,'b,'c) e = a * 'a * ('b ref) * 'c (* eq 'a, eq 'c *)

(* how about polymorphic datatype declarations? *)
datatype 'a REF = REF of 'a TiltPrim.ref
datatype A = A of TiltPrim.int32
datatype B = datatype A
datatype 'a C = C of A * 'a
datatype ('a,'b) D = D of A * 'a * ('b REF)
datatype ('a,'b,'c) E = E of A * 'a * ('b REF) * 'c

(* Requires equality on 'b but not 'a *)
datatype ('a,'b) X = X of 'a ref * ('a,'b) Y
     and ('a,'b) Y = Y of 'b

(* Requires equality on 'a and 'b *)
(* Note, but U and V have the same sigpoly_eq *)
datatype ('a,'b) U = U of 'a
     and ('a,'b) V = V of 'b * ('a,'b) U
