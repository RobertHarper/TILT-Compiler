(*$import CompSyn Compile CPrint AbsMachine Trace TMachine GlobalStructs LambdaStructs NamesStructs TypeCheckStructs PrintStructs FormatterStructs IndexStructs *)
structure CompSyn =
  CompSyn (structure Global = Global
           structure IntSyn' = IntSyn
	   structure Names = Names);

structure Compile =
  Compile (structure IntSyn' = IntSyn
	   structure CompSyn' = CompSyn
	   structure Whnf = Whnf
	   structure TypeCheck = TypeCheck
	   structure Names = Names);

structure CPrint =
  CPrint (structure IntSyn' = IntSyn
	  structure CompSyn' = CompSyn
	  structure Print = Print
	  structure Formatter = Formatter
	  structure Names = Names);

(*
structure Assign =
  Assign (structure IntSyn' = IntSyn
	  structure Whnf = Whnf
	  structure Unify = UnifyTrail);
*)

signature HERE =
sig
    structure IntSyn : INTSYN
    structure CompSyn : COMPSYN
      sharing CompSyn.IntSyn = IntSyn
    structure CPrint : CPRINT
      sharing CPrint.CompSyn = CompSyn
      sharing CPrint.IntSyn = IntSyn
end;

structure ShouldFail : HERE =
struct
    structure IntSyn = IntSyn
    structure CompSyn = CompSyn
    structure CPrint = CPrint
end;
   
structure AbsMachine = 
  AbsMachine (structure IntSyn' = IntSyn
              structure CompSyn' = CompSyn
              structure Unify = UnifyTrail
	      (* structure Assign = Assign *)
	      structure Index = Index
              structure Trail = Trail
	      structure CPrint = CPrint
              structure Names = Names); 

structure Trace =
  Trace (structure IntSyn' = IntSyn
	 structure Names = Names
	 structure Whnf = Whnf
	 structure Abstract = Abstract
	 structure Print = Print);

structure TMachine =
  TMachine (structure IntSyn' = IntSyn
	    structure CompSyn' = CompSyn
	    structure Unify = UnifyTrail
	    structure Index = Index
	    structure Trail = Trail
(*XXX	    structure CPrint = CPrint*)
            structure Names = Names
	    structure Trace = Trace);
