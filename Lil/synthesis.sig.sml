signature SYNTHESIS = 
  sig

    structure Kindof :
      sig
	val primcon : Lil.primcon -> Lil.kind
	val con : LilContext.context -> Lil.con -> Lil.kind
      end
    structure Typeof : 
      sig
	val coercion : LilContext.context -> Lil.coercion -> Lil.con
	val primarg  : LilContext.context -> Lil.primarg -> Lil.con
	val function : Lil.function -> Lil.con
	val code     : Lil.function -> Lil.con
	val sv64     : LilContext.context -> Lil.sv64 -> Lil.con 
	val sv32     : LilContext.context -> Lil.sv32 -> Lil.con
	val op64     : LilContext.context -> Lil.op64 -> Lil.con
	val op32     : LilContext.context -> Lil.op32 -> Lil.con
	val bnd      : LilContext.context -> Lil.bnd -> LilContext.context * LilSubst.con_subst
	val switch   : LilContext.context -> Lil.switch -> Lil.con
	val bind_functions : LilContext.context * (Lil.var * Lil.function) list -> LilContext.context
	val bind_op32      : LilContext.context * (Lil.var * Lil.op32) -> LilContext.context
	val bind_op64      : LilContext.context * (Lil.var * Lil.op64) -> LilContext.context
	val bind_unpack    : LilContext.context * (Lil.var * Lil.var * Lil.sv32) -> LilContext.context
	val bind_split     : LilContext.context * (Lil.var * Lil.var * Lil.con) -> LilContext.context * LilSubst.con_subst
	val bind_inj       : LilContext.context * (Lil.w32 * Lil.var * Lil.con * Lil.sv32) -> LilContext.context * LilSubst.con_subst
	val bind_unfold    : LilContext.context * (Lil.var * Lil.con) -> LilContext.context * LilSubst.con_subst

	val pexp : LilContext.context * ('a P.pexp) -> LilContext.context * LilSubst.con_subst
	val sv32pexp : LilContext.context * (Lil.sv32 P.pexp) -> LilContext.context * LilSubst.con_subst * Lil.con
	  
      end
  end      

