signature  PEXP = 
  sig
    
    (* Monadic type of partial expressions:
     * An 'a pexp is a value (bnds,v) where
     *  bnds : Lil.bnd list, (kept in reverse order)
     *  v    : 'a
     *
     * The pexp (bnds,v) corresponds intuitively to
     * Let (rev bnds) in v
     *)
    type 'a pexp 

    (* Inject into the monad *)
    val ret : 'a -> 'a pexp
      
    (* Bind the value portion of the monad in a computation, 
     * and conglomerate the resulting bnds into the new partial
     * expression.  
     * 
     * bind (bnds1,v1) f  ==  Let (rev bnds1) in f v1
     *)
    val bind : 'a pexp -> ('a -> 'b pexp) -> 'b pexp 


    val lift :  ('a -> ('b pexp)) -> ('a pexp -> 'b pexp )
    val map  :  ('a -> 'b ) -> ('a pexp -> 'b pexp )
    val idem : 'a pexp pexp -> 'a pexp

    val bind_first : 'a pexp -> ('a -> 'b pexp * 'c) -> 'b pexp * 'c

    val out  : 'a pexp -> 'a
    val out1 : ('a * 'b) pexp -> 'a * ('b pexp)
    val out2 : ('a * 'b) pexp -> ('a pexp) * 'b
    val pair : ('a * 'b) pexp -> 'a pexp * 'b pexp

    structure List :
      sig 
	val concat : 'a pexp list -> 'a list pexp
	val map : ('a -> 'b pexp) ->  'a list pexp -> 'b list pexp 
	val map_from_list : ('a -> 'b pexp) -> 'a list -> 'b list pexp
	val foldl_from_list : ('a * 'b -> 'b pexp) -> 'b -> 'a list -> 'b pexp
      end

    structure Bind : 
      sig
	val op32    : Lil.op32 pexp -> (Lil.var -> 'a pexp) -> 'a pexp 
	val op32'   : Lil.var -> Lil.op32 pexp -> 'a pexp -> 'a pexp 
	val op64    : Lil.op64 pexp -> (Lil.var -> 'a pexp) -> 'a pexp 
	val op64'   : Lil.var -> Lil.op64 pexp -> 'a pexp -> 'a pexp 
	val fixcode': (Lil.var * Lil.function) list pexp -> 'a pexp -> 'a pexp
	val split   : Lil.con pexp -> (Lil.var * Lil.var -> 'a pexp) -> 'a pexp
	val split'  : Lil.var * Lil.var -> Lil.con pexp -> 'a pexp -> 'a pexp
	val unfold  : Lil.con pexp -> (Lil.var -> 'a pexp) -> 'a pexp
	val unfold' : Lil.var -> Lil.con pexp -> 'a pexp -> 'a pexp
	val inj     : (Lil.w32 * Lil.con * Lil.sv32) pexp -> (Lil.var -> 'a pexp) -> 'a pexp
	val inj'    : Lil.var -> (Lil.w32 * Lil.con * Lil.sv32) pexp -> 'a pexp -> 'a pexp
	val unpack  : Lil.sv32 pexp -> (Lil.var * Lil.var -> 'a pexp) -> 'a pexp
	val unpack' : Lil.var * Lil.var -> Lil.sv32 pexp -> 'a pexp -> 'a pexp
      end

    (* Special operations for values of type unit pexp
     *)
    structure Unit : 
      sig
	(* sequence pexp1 pexp1 == let _ = pexp1 in pexp2 
	 *)
	val sequence : unit pexp -> 'a pexp ->  'a pexp 

	val concat : unit pexp list -> unit pexp
	(* ignore pexp1 == let _ = pexp1 in ()
	 *)
	val ignore : 'a pexp -> unit pexp

	val to_bnds : unit pexp -> Lil.bnd list
      end

    structure SV32 :
      sig

	(* to_unit v pexp == Let v = pexp in ()
	 *)
	val to_unit : Lil.var -> Lil.sv32 pexp -> unit pexp

	(* to_var' v pexp == Let v = pexp in v
	 *)
	val to_var' : Lil.var -> Lil.sv32 pexp -> Lil.var pexp

	(* to_var pexp == Let v = pexp in v
	 *  where v is fresh
	 *)
	val to_var : Lil.sv32 pexp -> Lil.var pexp 

	(* to_named_var s pexp == Let v = pexp in v
	 *   where v is fresh, and looks like s  (v = fresh_named_var s)
	 *)
	val to_named_var : string -> Lil.sv32 pexp -> Lil.var pexp 


	(* bind2var pexp f == Let v = pexp in f v 
	 *           where v is fresh
	 *)
	val bind2var : Lil.sv32 pexp -> (Lil.var -> Lil.sv32 pexp) -> Lil.sv32 pexp 

	(* bind2var v pexp1 pexp1 == Let v = pexp1 in pexp2
	 * 
	 *)
	val bind2var' : Lil.var -> Lil.sv32 pexp -> 'a pexp -> 'a pexp 
	  

	val to_op : Lil.sv32 pexp -> Lil.op32 pexp

	val from_op : Lil.op32 pexp -> Lil.sv32 pexp
      
	val from_exp : Lil.exp pexp -> Lil.sv32 pexp

      end


    structure SV64 :
      sig

	(* to_unit v pexp == Let v = pexp in ()
	 *)
	val to_unit : Lil.var -> Lil.sv64 pexp -> unit pexp

	(* to_var' v pexp == Let v = pexp in v
	 *)
	val to_var' : Lil.var -> Lil.sv64 pexp -> Lil.var pexp

	(* to_var pexp == Let v = pexp in v
	 *  where v is fresh
	 *)
	val to_var : Lil.sv64 pexp -> Lil.var pexp 

	(* to_named_var s pexp == Let v = pexp in v
	 *   where v is fresh, and looks like s  (v = fresh_named_var s)
	 *)
	val to_named_var : string -> Lil.sv64 pexp -> Lil.var pexp 


	(* bind2var pexp f == Let v = pexp in f v 
	 *           where v is fresh
	 *)
	val bind2var : Lil.sv64 pexp -> (Lil.var -> Lil.sv64 pexp) -> Lil.sv64 pexp 

	(* bind2var v pexp1 pexp1 == Let v = pexp1 in pexp2
	 * 
	 *)
	val bind2var' : Lil.var -> Lil.sv64 pexp -> 'a pexp -> 'a pexp 

	val to_op : Lil.sv64 pexp -> Lil.op64 pexp

	val from_op : Lil.op64 pexp -> Lil.sv64 pexp
      
      end


    (* Interface to ordinary Lil constructs.
     *)
    structure Lili :
      sig
	
	(* Inject an expression Let bnds in sv32 into the monad as (rev bnds, sv32) 
	 *  (Will recursively flatten bnds as well)
	 *)
	val from_exp : Lil.exp -> Lil.sv32 pexp
	  
	(* Escape!!!! from the monad.... *)
	val to_exp : Lil.sv32 pexp -> Lil.exp

	val exp_to_exp : Lil.exp pexp -> Lil.exp

	val op_to_exp : Lil.op32 pexp -> Lil.exp	  

	val from_bnds : Lil.bnd list * 'a -> 'a pexp
	val to_bnds : 'a pexp -> Lil.bnd list * 'a 

	val fold : (Lil.bnd * 'state -> 'state) -> ('a * 'state -> 'result) -> 'state -> 'a pexp -> 'result 

	val mapPartial : (Lil.bnd -> Lil.bnd option) -> 'a pexp -> 'a pexp
	(* map in reverse order.  more efficient - use if no effects in f *)
	val revMapPartial : (Lil.bnd -> Lil.bnd option) -> 'a pexp -> 'a pexp
      end
  end
