(*$import WNIL *)

(* The interface to the curtain
 *)

signature CURTAIN = 
  sig
    structure Nil : NIL
      
    structure E : 
      sig 
	val expose : Nil.exp  -> Nil.exp_
	val hide   : Nil.exp_ -> Nil.exp

	val Var_e       : Name.var -> Nil.exp
	val Const_e     : (Nil.con,Nil.exp) Prim.value -> Nil.exp
	val Let_e       : Nil.letsort * Nil.bnd list * Nil.exp -> Nil.exp
	val Prim_e      : Nil.allprim * Nil.con list * Nil.exp list -> Nil.exp
	val Switch_e    : Nil.switch -> Nil.exp
	val App_e       : Nil.openness * Nil.exp * Nil.con list * Nil.exp list * Nil.exp list -> Nil.exp
	val ExternApp_e : Nil.exp * Nil.exp list -> Nil.exp
	val Raise_e     : Nil.exp * Nil.con -> Nil.exp
	val Handle_e    : {body :Nil.exp,bound : Nil.var,handler : Nil.exp,result_type : Nil.con} -> Nil.exp

	val eq     : Nil.exp * Nil.exp -> bool
	val equate : Nil.exp * Nil.exp -> unit
      end

    structure C : 
      sig 
	val expose     : Nil.con  -> Nil.con_
	val hide       : Nil.con_ -> Nil.con

	val Closure_c     : Nil.con * Nil.con -> Nil.con
	val Var_c         : Name.var -> Nil.con
	val Proj_c        : Nil.con * Nil.label -> Nil.con
	val Mu_c          : bool * (Nil.var,Nil.con) Nil.sequence -> Nil.con
	val Let_c         : Nil.letsort * Nil.conbnd list * Nil.con -> Nil.con
	val Prim_c        : Nil.primcon * Nil.con list -> Nil.con
	val Crecord_c     : (Nil.label * Nil.con) list -> Nil.con
	val App_c         : Nil.con * Nil.con list -> Nil.con
	val AllArrow_c    : {body_type:Nil.con, eFormals:(Nil.var option * Nil.con) list,
			     effect:Nil.effect, fFormals:Nil.w32, isDependent:bool,
			     openness:Nil.openness, tFormals:(Nil.var * Nil.kind) list} -> Nil.con
	val ExternArrow_c : Nil.con list * Nil.con -> Nil.con
	val Typeof_c      : Nil.exp  -> Nil.con
	val Typecase_c    : {arg : Nil.con,
			     arms : (Nil.primcon * (Nil.var * Nil.kind) list * Nil.con) list,
			     default : Nil.con,
			     kind : Nil.kind}      -> Nil.con

	val eq     : Nil.con * Nil.con -> bool
	val equate : Nil.con * Nil.con -> unit
      end

    structure K : 
      sig 
	val expose : Nil.kind  -> Nil.kind_
	val hide   : Nil.kind_ -> Nil.kind

	val Type_k       : Nil.kind
	val Single_k     : Nil.con -> Nil.kind
	val SingleType_k : Nil.con -> Nil.kind
	val Arrow_k      : Nil.openness * (Nil.var * Nil.kind) list * Nil.kind -> Nil.kind
	val Record_k     : (Nil.label * Nil.var,Nil.kind) Nil.sequence -> Nil.kind

	val eq     : Nil.kind * Nil.kind -> bool
	val equate : Nil.kind * Nil.kind -> unit
      end

  end