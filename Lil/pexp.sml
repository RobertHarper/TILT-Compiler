structure P :> PEXP = 
  struct
    open Lil
    structure L = List
    val chatter = Stats.ff "PexpChatter"

    type 'a pexp = Lil.bnd list * 'a

    fun ret (v : 'a) : 'a pexp = ([],v)

    (* 'a pexp -> ('a -> 'b pexp) -> 'b pexp *)
    fun bind (bnds1,v) f = 
      let
	val (bnds2,v) = f v
      in (bnds2@bnds1,v)
      end

    (* 'a pexp -> ('a -> 'b pexp * 'c) -> 'b pexp * 'c *)
    fun bind_first (bnds1,v) f = 
      let
	val ((bnds2,v),c) = f v
      in ((bnds2@bnds1,v),c)
      end

    (* ('a -> 'b pexp) -> 'a pexp -> 'b pexp *)
    fun lift f (bnds1,v) = 
      let
	val (bnds2,v) = f v
      in (bnds2@bnds1,v)
      end

    (* ('a -> 'b ) -> 'a pexp -> 'b pexp *)
    fun map f (bnds1,v) = (bnds1,f v)

    fun idem ((bnds1,(bnds2,a)) : 'a pexp pexp) : 'a pexp = (bnds2@bnds1,a)

    fun out ((_,a) : 'a pexp) : 'a = a
    fun out1 (bnds,(a,b)) = (a,(bnds,b))
    fun out2 (bnds,(a,b)) = ((bnds,a),b)

    fun pair ((bnds,(a,b)) : ('a * 'b) pexp) = ((bnds,a),(bnds,b))

    structure List = 
      struct 

	fun concat (pexps : 'a pexp list) : 'a list pexp = 
	  let 
	    fun loop ([],acc) = (acc,[])
	      | loop ((bnds,a)::pexps,acc) = 
	      let val (rbnds,aa) = loop (pexps,bnds @ acc) 
	      in (rbnds,a :: aa) end
	  in loop (pexps,[])
	  end

	fun map (f : 'a -> 'b pexp) (pexp : 'a list pexp) : 'b list pexp =
	  let
	    val pexps = bind pexp (concat o (L.map f))
	  in pexps
	  end

	fun map_from_list  (f : 'a -> 'b pexp) (l : 'a list) : 'b list pexp = 
	  concat (L.map f l)

	fun foldl_from_list (f : 'a * 'b -> 'b pexp) (b : 'b) (l : 'a list) : 'b pexp = 
	  let 
	    fun loop ([],b) = b
	      | loop (a::aa,b) = loop(aa,bind b (fn b => f (a,b)))
	  in loop (l,ret b)
	  end

      end

    structure Bind = 
      struct

	local
	  fun to_var' v (rbnds,oper) = 
	    let 
	      val b = Exp32_b (v,oper)
	      val _ = if !chatter then
		(PpLil.pp_bnd b;
		 print "\n"
		 )
		      else ()

	    in (b::rbnds,v)
	    end
	    
	  fun to_var (pexp as (rbnds,oper))= 
	    (case oper
	       of Val(Var_32 v) => (rbnds,v)
		| _ => to_var' (Name.fresh_named_var "x_tmp")  pexp)
	in
	  fun op32 oper f = bind (to_var oper) f
	  fun op32' v oper a = bind (to_var' v oper) (fn _ => a)
	end

	local
	  fun to_var' v (rbnds,oper) = 
	    let val b = Exp64_b (v,oper)
	    in (b::rbnds,v)
	    end
	    
	  fun to_var (pexp as (rbnds,oper))= 
	    (case oper
	       of Val_64(Var_64 v) => (rbnds,v)
		| _ => to_var' (Name.fresh_named_var "xf_tmp")  pexp)
	in
	  fun op64 oper f = bind (to_var oper) f
	  fun op64' v oper a = bind (to_var' v oper) (fn _ => a)
	end

	local
	  fun fix (rbnds,vfs) = 
	    let val b = Fixcode_b vfs
	    in (b::rbnds,())
	    end
	in
	  fun fixcode' vfs a = bind (fix vfs) (fn _ => a)
	end

	local
	  fun to_var' (a,b) (rbnds,c) = 
	    let val bnd = Split_b (a,b,c)
	    in (bnd::rbnds,(a,b))
	    end
	  
	  fun to_var pexp =  
	    let
	      val a = Name.fresh_named_var "split1"
	      val b = Name.fresh_named_var "split2"
	    in to_var' (a,b) pexp
	    end
	in
	  fun split c f = bind (to_var c) f
	  fun split' (a,b) c pexp = bind (to_var' (a,b) c) (fn _ => pexp)
	end


	local
	  fun to_var' a (rbnds,c) = 
	    let val bnd = Unfold_b (a,c)
	    in (bnd::rbnds,a)
	    end
	  
	  fun to_var pexp =  
	    let
	      val a = Name.fresh_named_var "unfold"
	    in to_var' a pexp
	    end
	in
	  fun unfold c f = bind (to_var c) f
	  fun unfold' a c pexp = bind (to_var' a c) (fn _ => pexp)
	end

	local
	  fun to_var' a (rbnds,(w,c,sv)) = 
	    let val bnd = Inj_b (w,a,c,sv)
	    in (bnd::rbnds,a)
	    end
	  
	  fun to_var pexp =  
	    let
	      val a = Name.fresh_named_var "inj"
	    in to_var' a pexp
	    end
	in
	  fun inj c f = bind (to_var c) f
	  fun inj' a c pexp = bind (to_var' a c) (fn _ => pexp)
	end

	local
	  fun to_var' (a,x) (rbnds,sv) = 
	    let val bnd = Unpack_b (a,x,sv)
	    in (bnd::rbnds,(a,x))
	    end
	  
	  fun to_var pexp =  
	    let
	      val a = Name.fresh_named_var "unpacked_type"
	      val x = Name.fresh_named_var "unpacked_var"
	    in to_var' (a,x) pexp
	    end
	in
	  fun unpack sv f = bind (to_var sv) f
	  fun unpack' (a,x) sv pexp = bind (to_var' (a,x) sv) (fn _ => pexp)
	end
      
      end


    structure Unit = 
      struct
	fun sequence ((bnds1,()) : unit pexp) ((bnds2,a): 'a pexp) : 'a pexp = 
	  (bnds2 @ bnds1, a)

	fun ignore (pexp : 'a pexp) : unit pexp = (#1 pexp,())

	fun to_bnds ((rbnds,()) : unit pexp) = rev rbnds

	fun concat pexps = ignore (List.concat pexps)

      end

    structure SV32 = 
      struct

	fun to_var' v ((rbnds,sv) : sv32 pexp) : var pexp = 
	  let 
	    val b = Exp32_b (v,Val sv)
	    val _ = if !chatter then
	      (PpLil.pp_bnd b;
	       print "\n"
	       )
		    else ()
	  in (b::rbnds,v)
	  end
	fun to_named_var s pexp = to_var' (Name.fresh_named_var s)  pexp

	fun to_var (pexp as (rbnds,sv) : sv32 pexp) : var pexp = 
	  (case sv
	     of Var_32 v => (rbnds,v)
	      | _ => to_named_var "x_tmp" pexp)

	fun to_unit (x : Lil.var) (p : Lil.sv32 pexp) : unit pexp = 
	  Unit.ignore (to_var' x p)
	(* sv32 pexp -> (var -> sv32 pexp) -> sv32 pexp *)
	val bind2var = fn pexp => fn f => bind (to_var pexp) f

	(* var -> sv32 pexp -> 'a pexp -> 'a pexp *)
	val bind2var' = fn (v : Lil.var) => fn (pexp1 : Lil.sv32 pexp) => fn (pexp2 : 'a pexp) : 'a pexp => bind (to_var' v pexp1) (fn _ => pexp2)


	fun to_op (revbnds,sv) = (revbnds,Val sv)

	fun from_op (bnds,op32) =
	  (case op32
	     of Val sv => (bnds,sv)
	      | _ => 
	       let
		 val tmp = Name.fresh_named_var "x_tmp"
		 val b = Exp32_b (tmp,op32)
		 val _ = if !chatter then
		   (PpLil.pp_bnd b;
		    print "\n"
		    )
			 else ()
	       in (b::bnds,Var_32 tmp)
	       end)

	fun from_exp (rbnds,e) = 
	  let
	    fun loop (e,acc) = 
	      (case #e e
		 of Val32_e sv => (acc,sv)
		  | Let_e (bnds,bdy) => loop(bdy,L.revAppend (bnds,acc)))
	  in loop (e,rbnds)
	  end

      end

    structure SV64 = 
      struct

	fun to_var' v ((rbnds,sv) : sv64 pexp) : var pexp = 
	  let val b = Exp64_b (v,Val_64 sv)
	  in (b::rbnds,v)
	  end
	fun to_named_var s pexp = to_var' (Name.fresh_named_var s)  pexp

	fun to_var (pexp as (rbnds,sv) : sv64 pexp) : var pexp = 
	  (case sv
	     of Var_64 v => (rbnds,v)
	      | _ => to_named_var "xf_tmp" pexp)

	fun to_unit (x : Lil.var) (p : Lil.sv64 pexp) : unit pexp = 
	  Unit.ignore (to_var' x p)
	(* sv64 pexp -> (var -> sv64 pexp) -> sv64 pexp *)
	val bind2var = fn pexp => fn f => bind (to_var pexp) f

	(* var -> sv64 pexp -> 'a pexp -> 'a pexp *)
	val bind2var' = fn (v : Lil.var) => fn (pexp1 : Lil.sv64 pexp) => fn (pexp2 : 'a pexp) : 'a pexp => bind (to_var' v pexp1) (fn _ => pexp2)


	fun bind2fun' vfs (bnds,a) = ((Fixcode_b vfs)::bnds,a)

	fun to_op (revbnds,sv) = (revbnds,Val_64 sv)

	fun from_op (bnds,op64) =
	  (case op64
	     of Val_64 sv => (bnds,sv)
	      | _ => 
	       let
		 val tmp = Name.fresh_named_var "xf_tmp"
	       in ((Exp64_b (tmp,op64)::bnds),Var_64 tmp)
	       end)

      end

    structure Lili = 
      struct       
	fun to_exp (revbnds,sv) = 
	  let
	    val body = Lil.mk_exp (Lil.Val32_e sv)
	  in
	    (case revbnds 
	       of [] => body
		| _ => Lil.mk_exp(Lil.Let_e (rev revbnds,body)))
	  end

	fun exp_to_exp p = to_exp (SV32.from_exp p)

	val from_exp = SV32.from_exp o ret 

	fun op_to_exp (p : Lil.op32 pexp) :  Lil.exp = to_exp (SV32.from_op p)

	fun fold (bndsfn : Lil.bnd * 'state -> 'state) (afn : 'a * 'state -> 'result) (state : 'state) ((rbnds,a) : 'a pexp) = 
	  let
	    val state = foldr bndsfn state rbnds
	    val result = afn (a,state)
	  in result
	  end

	fun fold_acc (bndsfn : Lil.bnd * 'state -> 'state pexp)  (afn : 'a * 'state -> 'result pexp)  (state : 'state) ((rbnds,a) : 'a pexp) : 'result pexp * 'state = 
	  let
	    fun folder (bnd,(state,racc)) = 
	      let
		val (rbnds,state) = bndsfn (bnd,state)
	      in (state,rbnds@racc)
	      end
	    val (state,rbnds) = foldr folder (state,[]) rbnds
	    val (resbnds,res) = afn (a,state)
	  in ((resbnds@rbnds,res),state)
	  end

	fun mapPartial mapper (bnds,sv) = 
	  let
	    fun loop [] = []
	      | loop (a::aa) = 
	      let
		val aa = loop aa
	      in
		case mapper a
		  of SOME a => a::aa
		   | NONE => aa
	      end
	  in (loop bnds,sv)
	  end
	fun revMapPartial mapper (bnds,sv) = (L.mapPartial mapper bnds,sv)

	fun from_bnds ((bnds, a) : Lil.bnd list * 'a ) : 'a pexp = (rev bnds, a)
	fun to_bnds ((bnds,a)) = (rev bnds,a)
      end

	

  end
