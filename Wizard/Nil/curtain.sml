(*$import CURTAIN *)

structure Curtain :> CURTAIN = 
  struct 


    fun error s = Util.error "curtain.sml:" s

    type info = unit

    structure VarMap = Name.VarMap
    structure IL =
      struct
	type var = Name.var
	type label = Name.label
	  
	type w32 = Word32.word
	type prim = Prim.prim
	type ('a,'b) sequence = ('a,'b) Sequence.sequence

	datatype openness = Open | Code | Closure
	  
	datatype effect = Total | Partial

	datatype recursive = Leaf | NonRecursive | Arbitrary

	datatype niltrace = TraceUnknown
	                  | TraceKnown of TraceInfo.traceinfo
	                  | TraceCompute of var

	datatype letsort = Sequential | Parallel
	datatype phase = Runtime | Compiletime
	datatype kind_ = 
	  Type_k                        (* constructors that are types *)
	| SingleType_k of con           (* singleton-kind at kind type *)
	| Single_k of con               (* singleton-kind at any kind *)
                                        (* dependent record kind *)

	| Record_k of ((label * var),kind) sequence
	                                (* dependent arrow kinds for open 
					 constr funs, closed funs, or closures *)
	| Arrow_k of openness * (var * kind) list * kind 


        and primcon =                             (* classifies term-level ... *)
	  Int_c of Prim.intsize                   (* register integers *)
	| Float_c of Prim.floatsize               (* register floating-points *)
	| BoxFloat_c of Prim.floatsize            (* boxed floating-points *)
	| Exn_c                                   (* exceptions *)
	| Array_c                                 (* arrays *)
	| Vector_c                                (* vectors *)
	| Loc_c                                   (* locatives *)
	| Exntag_c                                (* exception tags *)
	| Record_c of label list * var list option  (* records *)
	| Sum_c of {tagcount : w32,
		    totalcount : w32,
		    known : w32 option}           (* sum types *)
	| Vararg_c of openness * effect           (* classifies make_vararg and make_onearg *)

	and conbnd = 
	  Con_cb  of (var * con)
	| Open_cb of (var * (var * kind) list * con)
	| Code_cb of (var * (var * kind) list * con)

	and con_ = 
	  Prim_c of primcon * con list                (* Classify term-level values 
							 of primitive types *)
	| Mu_c of bool * (var,con) sequence           (* Constructors that classify values of
						       a recursive type; bool indicates if it
						       is really recursive *)
	| AllArrow_c of {openness : openness, (* open functions, code functions, and closures *)
			 effect : effect,
			 isDependent : bool,
			 tFormals : (var * kind) list,
			 eFormals : (var option * con) list,
			 fFormals : w32,
			 body_type : con}
	| ExternArrow_c of con list * con
	| Var_c of var
	| Let_c of letsort * conbnd list * con        (* Constructor-level bindings *)
	| Typeof_c of exp                             (* is equivalent to type of given expression *)
	| Crecord_c of (label * con) list             (* Constructor-level records *)
	| Proj_c of con * label                       (* Constructor-level record projection *)
	| Closure_c of con * con                      (* Constructor-level closure: 
						       code and environment *)
	| App_c of con * con list                     (* Constructor-level application 
						   of open or closed constructor function *)
	| Typecase_c of {arg : con,
			 arms : (primcon * (var * kind) list * con) list,
			 default : con,
			 kind : kind}        (* Constructor-level typecase *)
	and nilprim = 
	  record of label list       (* record intro *)
	| partialRecord of label list * int (* record with a missing zero-indexed field *)
	| select of label            (* record field selection; takes the record type *)
	| inject of TilWord32.word   (* slow; must be given one type that is
				      reducible to a sum type *)
	| inject_known of TilWord32.word   (* fast; the injected field is a non-carrier or else
					      its type is reducible to HNF *)
	| inject_known_record of TilWord32.word 
	  (* fast; the type of the injected field must be reducible to a record type
	   further, the term arguments consituting the record are passed separately *)
	  
	| project of TilWord32.word  (* corresponds to inject *)
	| project_known of TilWord32.word  (* corresponds to inject_known *)
	| project_known_record of TilWord32.word * label (* corresponds to inkject_known_record *)
	  
	  
	| box_float of Prim.floatsize   (* boxing floating-points *)
	| unbox_float of Prim.floatsize (* unboxing floating-points *)
	| roll | unroll              (* coerce to/from recursive type *) 
	| make_exntag                (* generate new exn tag *)
	| inj_exn of string          (* takes tag + value and give exn *)
	| make_vararg of openness * effect  (* given a function in onearg calling conv, convert to vararg *)
	| make_onearg of openness * effect  (* given a function in vararg calling conv, convert to onearg *)
	| peq                        (* polymorphic equality: unused since HIL compiles away equality *)
	  

	and allprim = 
	  NilPrimOp of nilprim
	| PrimOp of prim

	and switch =                                 (* Switching on / Elim Form *)
	  Intsw_e of {arg  : exp, 
		      size : Prim.intsize,
		      result_type : con,
		      arms : (w32 * exp) list,
		      default : exp option}             (* integers *)
	| Sumsw_e of {arg : exp,
		      sumtype : con,
		      result_type : con,
		      bound : var,
		      arms : (w32 * niltrace * exp) list,
		      default : exp option}             (* sum types *)
	| Exncase_e of {arg : exp,
			result_type : con,
			bound : var,
			arms : (exp * niltrace * exp) list,
			default : exp option}           (* exceptions *)
	| Typecase_e of {arg : con,
			 result_type : con,
			 arms : (primcon * (var * kind) list * exp) list,
			 default : exp}                 (* typecase *)

	and function = Function of {effect      : effect,
				    recursive   : recursive,
				    isDependent : bool,
				    tFormals    : (var * kind) list,
				    eFormals    : (var * niltrace * con) list,
				    fFormals    : (var list),
				    body        : exp,
				    body_type   : con}

	and bnd =                                 (* Term-level Bindings with optional classifiers *)
	  Con_b of phase * conbnd               (* Binds constructors *)
	| Exp_b of var * niltrace * exp         (* Binds expressions *)
	| Fixopen_b of (var,function) sequence  (* Binds mutually recursive open functions *)
	| Fixcode_b of (var,function) sequence  (* Binds mutually recursive code functions *)
                                          (* Allows the creation of term and for-all closures;
					   bool indicates if it is really recursive *)
	| Fixclosure_b of bool * (var , {code:var, cenv:con, venv:exp, tipe:con}) sequence

	and exp_ =                                          (* Term-level constructs *)
	  Var_e of var                                   (* Term-level variables *)
	| Const_e of (con,exp) Prim.value                (* Term-level constants *)
	| Let_e of letsort * bnd list * exp              (* Binding construct *)
	| Prim_e of allprim * (con list) * (exp list)    (* primops must be fully applied *)
	| Switch_e of switch                             (* Switch statements *)
	| App_e of openness * exp * con list *           (* application of open funs, code, or closures *)
	  exp list * exp list           (* in the case of code, the first exp must be a var *)
	| ExternApp_e of exp * exp list
	| Raise_e of exp * con                                
	| Handle_e of {body :exp,
		       bound : var,
		       handler : exp,
		       result_type : con}
	and kind__    = KIND of node_k | KCON_SUBST of con VarMap.map * kind | KRENAME of kind
	and con__     = CON  of node_c | CCON_SUBST of con VarMap.map * con  | CRENAME of con
	and exp__     = EXP  of node_e | ECON_SUBST of con VarMap.map * exp  | ERENAME of exp
	withtype node_k = {kind:kind_,info:info}
	and node_c      = {con:con_  ,info:info}
	and node_e      = {exp:exp_  ,info:info}
	and kind        = kind__ ref
	and con         = con__  ref
	and exp         = exp__  ref

	datatype import_entry = 
	  ImportValue of label * var * niltrace * con
	| ImportType  of label * var * kind
	  
	datatype export_entry = 
	  ExportValue of label * var
	| ExportType  of label * var
	  
	datatype module = MODULE of {bnds : bnd list,
				     imports : import_entry list,
				     exports : export_entry list}


      end      

    structure Nil = 
      struct 
	type var = Name.var
	type label = Name.label
	  
	type w32 = Word32.word
	type prim = Prim.prim
	type ('a,'b) sequence = ('a,'b) Sequence.sequence

	val flattenThreshold = ref 6

	datatype openness  = datatype IL.openness
	datatype effect    = datatype IL.effect
	datatype recursive = datatype IL.recursive
	datatype niltrace  = datatype IL.niltrace
	datatype letsort   = datatype IL.letsort
	datatype phase     = datatype IL.phase
	datatype primcon   = datatype IL.primcon
        datatype conbnd    = datatype IL.conbnd
	datatype nilprim   = datatype IL.nilprim
        datatype allprim   = datatype IL.allprim
	datatype switch    = datatype IL.switch
	datatype function  = datatype IL.function
	datatype bnd       = datatype IL.bnd
        type exp           = IL.exp
	type con           = IL.con
	type kind          = IL.kind

	datatype kind_ = 
	  Type_k                        (* constructors that are types *)
	| SingleType_k of con           (* singleton-kind at kind type *)
	| Single_k of con               (* singleton-kind at any kind *)
	                                (* dependent record kind *)
	| Record_k of ((label*var),kind) sequence
	                                (* dependent arrow kinds for open 
	                                   constr funs, closed funs, or closures *)
	| Arrow_k of openness * (var * kind) list * kind 

	and con_ = 
	  Prim_c of primcon * con list                (* Classify term-level values 
							 of primitive types *)
	| Mu_c of bool * (var,con) sequence           (* Constructors that classify values of
						       a recursive type; bool indicates if it
						       is really recursive *)
	| AllArrow_c of {openness : openness, (* open functions, code functions, and closures *)
			 effect : effect,
			 isDependent : bool,
			 tFormals : (var * kind) list,
			 eFormals : (var option * con) list,
			 fFormals : w32,
			 body_type : con}
	| ExternArrow_c of con list * con
	| Var_c of var
	| Let_c of letsort * conbnd list * con        (* Constructor-level bindings *)
	| Typeof_c of exp                             (* is equivalent to type of given expression *)
	| Crecord_c of (label * con) list             (* Constructor-level records *)
	| Proj_c of con * label                       (* Constructor-level record projection *)
	| Closure_c of con * con                      (* Constructor-level closure: 
						       code and environment *)
	| App_c of con * con list                     (* Constructor-level application 
						   of open or closed constructor function *)
	| Typecase_c of {arg : con,
			 arms : (primcon * (var * kind) list * con) list,
			 default : con,
			 kind : kind}        (* Constructor-level typecase *)



	and exp_ =                                          (* Term-level constructs *)
	  Var_e of var                                   (* Term-level variables *)
	| Const_e of (con,exp) Prim.value                (* Term-level constants *)
	| Let_e of letsort * bnd list * exp              (* Binding construct *)
	| Prim_e of allprim * (con list) * (exp list)    (* primops must be fully applied *)
	| Switch_e of switch                             (* Switch statements *)
	| App_e of openness * exp * con list *           (* application of open funs, code, or closures *)
	  exp list * exp list           (* in the case of code, the first exp must be a var *)
	| ExternApp_e of exp * exp list
	| Raise_e of exp * con                                
	| Handle_e of {body :exp,
		       bound : var,
		       handler : exp,
		       result_type : con}
	  
	(* result types are needed for recursive definitions in order to make
	 * type-checking syntax directed.  Also note that I've forced all functions
	 * to be named -- that is, they do not appear as exp forms.  We need
	 * recursive expressions involving records and closures in order to
	 * closure-convert recursive functions.
	 *)
	  
	  
	datatype import_entry = datatype IL.import_entry
	datatype export_entry = datatype IL.export_entry
	datatype module       = datatype IL.module
      end


    val map_second = Listops.map_second
    val foldl_acc  = Listops.foldl_acc
    val zip        = Listops.zip

    val mapopt     = Util.mapopt

    (*Note that there is an implicit decision here.  Do you 
     * explicitly combine composed substitutions, or do you
     * unroll them sequentially.  The former puts bound on the incremental
     * work you do on any unroll, and gives you hope that some
     * substitutions may cancel each other out.  However, you 
     * pay the price by not being able to memoize the result of the
     * internal substitution, since you are combining it before
     * carrying it out*)

    local
      fun is_empty s = (VarMap.numItems s) = 0
    in
    fun substConInKind subst k = 
      if is_empty subst then k
      else ref (IL.KCON_SUBST (subst,k))

    fun substConInCon  subst c = 
      if is_empty subst then c
      else ref (IL.CCON_SUBST (subst,c))

    fun substConInExp  subst e = 
      if is_empty subst then e
      else ref (IL.ECON_SUBST (subst,e))


    fun renameKind k = ref (IL.KRENAME k)
    fun renameCon  c = ref (IL.CRENAME c)
    fun renameExp  e = ref (IL.ERENAME e)
    end

    type con_subst = Nil.con Name.VarMap.map

	(* Instantiate the functor for constructors*)
    structure CSubst = SubstFn(type item = Nil.con
			       type item_subst = con_subst
			       val substItemInItem = substConInCon
			       val renameItem = renameCon)
      


    fun ::= (loc,value) = (loc := !value;value)

    infix 3 ::=

    structure K = 
      struct 

	fun KIND (k : IL.node_k) : IL.kind = ref (IL.KIND k)

	val Type_k = KIND {kind = IL.Type_k,info = ()}
	  
	fun SingleType_k arg = KIND {kind=IL.SingleType_k arg,
				     info = ()}
	  
	fun Single_k     arg = KIND {kind=IL.Single_k arg,
				     info = ()}
	  
	fun Record_k lvkseq =
	  KIND {kind = IL.Record_k lvkseq,
		info = ()}
	  
	fun Arrow_k (args as (_,vks,k)) = 
	  KIND {kind = IL.Arrow_k args,
		info = ()}

	fun push (f_c,f_k) (node as {kind,info}) = 
	  (case kind
	     of IL.Type_k         => node
	      | IL.SingleType_k c => 
	       {kind = IL.SingleType_k (f_c c),
		info = info}
	      | IL.Single_k c     => 
	       {kind = IL.Single_k (f_c c),
		info = info}
	      | IL.Record_k lvks  => 
	       {kind = IL.Record_k (Sequence.map_second f_k lvks),
		info = info}
	      | IL.Arrow_k (openness,vks,return)    => 
	       {kind = IL.Arrow_k (openness,map_second f_k vks,f_k return),
		info = info}
	       )

	fun Var_c v = ref (IL.CON {con = IL.Var_c v,
				   info = ()})

	fun push_rename_vks' (vks,subst) = 
	  let 
	    fun folder ((v,k),subst) = 
	      let 
		val v' = Name.derived_var v
		val k = substConInKind subst (renameKind k)
	      in ((v',k),CSubst.sim_add subst (v,Var_c v'))
	      end
	  in foldl_acc folder subst vks
	  end
	
	fun push_rename_vks vks = push_rename_vks' (vks,CSubst.empty())
	  
	(*node_k -> node_k*)
	fun push_rename' (node as {kind,info}) =
	  (case kind
	     of IL.Record_k lvks  => 
	       let 
		 fun folder (((l,v),k),subst) = 
		   let 
		     val v' = Name.derived_var v
		     val k = substConInKind subst (renameKind k)
		   in (((l,v'),k),CSubst.sim_add subst (v,Var_c v'))
		   end
		 val (lvks,_) = Sequence.foldl_acc folder (CSubst.empty()) lvks
	       in
		 {kind = IL.Record_k lvks,
		  info = info}
	       end
	      | IL.Arrow_k (openness,vks,return)    => 
	       let 
		 val (vks,subst) = push_rename_vks vks
		 val return = substConInKind subst (renameKind return)
	       in {kind = IL.Arrow_k (openness,vks,return),
		   info = info}
	       end
	      | _ => push (renameCon,renameKind) node
	       )


	(*push_rename pushes a renaming down one level.  Note that since
	 * renamings cross substitutions, the result is either a substitution
	 * or a real node*)
	fun push_rename k = 
	  (case !k
	     of IL.KCON_SUBST (subst,k) => ref (IL.KCON_SUBST(subst,push_rename k))   (*Push across*)
	      | IL.KRENAME k'           => k ::= push_rename k'                       (*Stacked collapse.  Can memoize*)
	      | IL.KIND node            => KIND (push_rename' node))                  (*Do the actual push*)


	(*Result is not a RENAME *)
	fun rename_elim k =
	  (case !k
	     of IL.KRENAME k' => k ::= push_rename k'
	      | _ => k)


	fun push_subst' (subst,node) = push (substConInCon subst,substConInKind subst) node
	     
	(*push_subst pushes a subst down one level.  Substitutions
	 * do not cross renamings or other substitutions, so the
	 * result must be a node.
	 *)
	fun push_subst(subst,k) = 
	  (case !(rename_elim k)
	     of IL.KCON_SUBST (subst',k) => push_subst(CSubst.compose(subst,subst'),k)
	      | IL.KIND node             => KIND (push_subst' (subst,node)))
	  
	(*Result is not a CON_SUBST. 
	 * Hence, if argument is not a RENAME, result is a KIND
	 *)
	fun subst_elim k = 
	  (case !k
	     of IL.KCON_SUBST (subst,k') => k ::= push_subst(subst,k')
	      | _ => k)

	(*Return the top level node of the kind*)
	fun getNode k = 
	  (case !(subst_elim(rename_elim k))
	     of IL.KIND node => node)

	  
	fun hide (k:Nil.kind_): Nil.kind  =
	  (case k
	     of Nil.Type_k            => Type_k
	      | Nil.SingleType_k args => SingleType_k args
	      | Nil.Single_k args     => Single_k args
	      | Nil.Record_k args     => Record_k args
	      | Nil.Arrow_k args      => Arrow_k args)

	fun toNil_k (k_ : IL.kind_) : Nil.kind_ = 
	  (case k_
	     of IL.Type_k            => Nil.Type_k
	      | IL.SingleType_k args => Nil.SingleType_k args
	      | IL.Single_k args     => Nil.Single_k args
	      | IL.Record_k args     => Nil.Record_k args
	      | IL.Arrow_k args      => Nil.Arrow_k args)

	fun expose (k:Nil.kind) : Nil.kind_ = 
	  ((*print "Exposing kind...";*)
	   let val res = toNil_k (#kind (getNode k))
	   in (*print "...done\n";*)res end)
		  
	fun eq (k1:Nil.kind,k2:Nil.kind) = k1 = k2

	fun equate (k1:Nil.kind,k2:Nil.kind) = ()(*k1 := !k2*)
      end

    structure C = 
      struct 

	fun CON node = ref (IL.CON node)

	fun Var_c v = CON {con = IL.Var_c v,
			   info = ()}

	fun Proj_c (args as (con,l)) = CON {con = IL.Proj_c args,
					    info = ()}
	  
	fun Mu_c (args as (flag,defs)) = 
	  CON {con = IL.Mu_c args,
	       info = ()}

	fun Let_c (args as (letsort, cbnds, body)) =
	  CON {con = IL.Let_c args,
	       info = ()}

	fun Prim_c (args as (pcon,cons)) = 
	   CON {con= IL.Prim_c args,
		info = ()}

	fun Crecord_c entries = 
	  CON  {con = IL.Crecord_c entries,
		info = ()}

	fun App_c (args as (con,actuals))     = 
	  CON {con = IL.App_c args,
	       info = ()}

	fun AllArrow_c (args as {openness, effect, isDependent, tFormals, 
				 eFormals, fFormals, body_type}) =
	  CON {con = IL.AllArrow_c args,
	       info = ()}

	fun ExternArrow_c (args as (cons,return)) = 
	  CON {con = IL.ExternArrow_c args,
	       info = ()}

	fun Typeof_c exp = CON {con = IL.Typeof_c exp,
				info = ()}

	fun Typecase_c (args as {arg, arms, default, kind}) =
	  CON {con = IL.Typecase_c args,
	       info = ()}

	fun Closure_c (args as (code,env)) = 
	  CON {con = IL.Closure_c args,
	       info = ()}

	fun hide   (c:Nil.con_): Nil.con = 
	  (case c 
	     of Nil.Prim_c args        => Prim_c args
	      | Nil.Mu_c   args        => Mu_c args
	      | Nil.AllArrow_c args    => AllArrow_c args
	      | Nil.ExternArrow_c args => ExternArrow_c args
	      | Nil.Var_c args         => Var_c args
	      | Nil.Let_c args         => Let_c args
	      | Nil.Typeof_c args      => Typeof_c args
	      | Nil.Closure_c args     => Closure_c args
	      | Nil.Crecord_c args     => Crecord_c args
	      | Nil.Proj_c args        => Proj_c args
	      | Nil.App_c args         => App_c args
	      | Nil.Typecase_c args    => Typecase_c args)
	     
	fun push_cbnd (f_c,f_k) cbnd = 
	  (case cbnd 
	     of IL.Con_cb (var,con)    => IL.Con_cb(var,f_c con)
	      | IL.Open_cb (v,vks,con) => IL.Open_cb(v,map_second f_k vks,f_c con)
	      | IL.Code_cb (v,vks,con) => IL.Code_cb(v,map_second f_k vks,f_c con))

	fun push (f_e,f_c,f_k) (node as {con,info}) = 
	  (case con
	     of IL.Prim_c (pcon,args) => 
	       {con = IL.Prim_c(pcon,map f_c args),
		info = info}
	      | IL.Var_c var                => node
	      | IL.Mu_c (flag,defs)   => 
	       {con=IL.Mu_c (flag,Sequence.map_second f_c defs),
		info = info}
	      | IL.AllArrow_c {openness, effect, isDependent, tFormals,eFormals, fFormals, body_type} =>
	       {con = IL.AllArrow_c{openness = openness, effect = effect, isDependent = isDependent,
				     tFormals = Listops.map_second f_k tFormals, 
				     eFormals = Listops.map_second f_c eFormals, 
				     fFormals = fFormals, body_type = f_c body_type},
		info = info}
	      | IL.ExternArrow_c (cons,con) => 
	       {con=IL.ExternArrow_c (map f_c cons,f_c con),
		info = info}
	      | IL.Let_c (letsort, cbnds, body) => 
	       {con=IL.Let_c(letsort,map (push_cbnd (f_c,f_k)) cbnds,f_c body),
		info = info}
	      | IL.Typeof_c exp             => 
	       {con=IL.Typeof_c (f_e exp),
		info = info}
	      | IL.Closure_c (code,env)     => 
	       {con=IL.Closure_c (f_c code,f_c env),
		info = info}
	      | IL.Crecord_c entries        => 
	       {con=IL.Crecord_c (Listops.map_second f_c entries),
		info = info}
	      | IL.Proj_c (con,lbl)         => 
	       {con=IL.Proj_c (f_c con,lbl),
		info = info}
	      | IL.App_c (cfun,actuals)     => 
	       {con=IL.App_c (f_c cfun,map f_c actuals),
		info = info}
	      | IL.Typecase_c {arg, arms, default, kind} => 
	       let fun doarm (pc,vks,body) = (pc,Listops.map_second f_k vks,f_c body)
	       in {con = IL.Typecase_c {arg=f_c arg, arms = map doarm arms, 
					 default = f_c default, 
					 kind = f_k kind},
		   info = info}
	       end)
	     
	fun push_rename_cbnd (cbnd,subst) = 
	  let
	    fun do_fun (Maker,v,vks,con,subst) = 
	      let
		val (vks,subst2) = K.push_rename_vks' (vks,subst)
		val con = substConInCon subst2 (renameCon con)
		val v' = Name.derived_var v
	      in (Maker(v',vks,con),CSubst.sim_add subst (v,Var_c v'))
	      end
	  in case cbnd 
	       of IL.Con_cb (v,con)    => 
		 let 
		   val v' = Name.derived_var v
		   val con = substConInCon subst (renameCon con)
		 in (IL.Con_cb(v',con),CSubst.sim_add subst (v,Var_c v'))
		 end
		| IL.Open_cb (v,vks,con) => do_fun(IL.Open_cb,v,vks,con,subst)
		| IL.Code_cb (v,vks,con) => do_fun(IL.Code_cb,v,vks,con,subst)
	  end


	(*node_c -> node_c*)
	fun push_rename' (node as {con,info}) =
	  (case con
	     of IL.Mu_c (flag,defs)   => 
	       let
		 val vars = Sequence.maptolist (fn (v,_) => v) defs
		 val vars' = map Name.derived_var vars
		 val vcons = map Var_c vars'
		 val subst = CSubst.simFromList (zip vars vcons)
		 val cons  = Sequence.maptolist (fn (_,c) => substConInCon subst (renameCon c)) defs
		 val defs = Sequence.fromList (ListPair.zip (vars',cons)) 
	       in 
		 {con=IL.Mu_c (flag,defs), info = info}
	       end
	      | IL.AllArrow_c {openness, effect, isDependent, tFormals,eFormals, fFormals, body_type} =>
	       let
		 val (tFormals,subst) = K.push_rename_vks tFormals
		 val eFormals = map_second ((substConInCon subst) o renameCon) eFormals
		 val body_type = substConInCon subst (renameCon body_type)
	       in
		 {con = IL.AllArrow_c{openness = openness, effect = effect, isDependent = isDependent,
				       tFormals = tFormals, eFormals = eFormals, 
				       fFormals = fFormals, body_type = body_type},
		  info = info}
	       end
	      | IL.Let_c (letsort, cbnds, body) => 
	       let 
		 val (cbnds,subst) = foldl_acc push_rename_cbnd (CSubst.empty()) cbnds
		 val body = substConInCon subst (renameCon body)
	       in {con=IL.Let_c(letsort,cbnds,body),
		   info = info}
	       end
	      | IL.Typecase_c {arg, arms, default, kind} => 
	       let 
		 fun doarm (pc,vks,body) = 
		   let val (vks,subst) = K.push_rename_vks vks
		   in (pc,vks,substConInCon subst (renameCon body))
		   end
	       in {con = IL.Typecase_c {arg     = renameCon arg, 
					 arms    = map doarm arms, 
					 default = renameCon default, 
					 kind    = renameKind kind},
		   info = info}
	       end
	      | _ => push (renameExp,renameCon,renameKind) node)


	(*push_rename pushes a renaming down one level.  Note that since
	 * renamings cross substitutions, the result is either a substitution
	 * or a real node*)
	fun push_rename c = 
	  (case !c
	     of IL.CCON_SUBST (subst,c) => ref (IL.CCON_SUBST(subst,push_rename c))   (*Push across*)
	      | IL.CRENAME c'           => c ::= push_rename c'                       (*Stacked collapse.  Can memoize*)
	      | IL.CON node             => CON (push_rename' node))                   (*Do the actual push*)


	(*Result is not a RENAME *)
	fun rename_elim c =
	  (case !c
	     of IL.CRENAME c' => c ::= push_rename c'
	      | _ => c)

	fun push_subst' (subst,node : IL.node_c) = 
	  (case #con node
	     of IL.Var_c var => 
	       (case CSubst.substitute subst var
		  of SOME c => getNode c
		   | NONE   => node)
	      | _ => push (substConInExp subst,substConInCon subst,substConInKind subst) node)

	(*push_subst pushes a subst down one level.  Substitutions
	 * do not cross renamings or other substitutions, so the
	 * result must be a node.
	 *)
	and push_subst(subst,c) = 
	  (case !(rename_elim c)
	     of IL.CCON_SUBST (subst',c) => push_subst  (CSubst.compose(subst,subst'),c)
	      | IL.CON node              => CON (push_subst' (subst,node)))
	  
	(*Result is not a CON_SUBST. 
	 * Hence, if argument is not a RENAME, result is a KIND
	 *)
	and subst_elim c = 
	  (case !c
	     of IL.CCON_SUBST (subst,c') => c ::= push_subst(subst,c')
	      | _ => c)

	(*Return the top level node of the kind*)
	and getNode c = 
	  (case !(subst_elim(rename_elim c))
	     of IL.CON node => node)

	  
	fun toNil (c_ : IL.con_) : Nil.con_ = 
	  (case c_
	     of IL.Prim_c args        => Nil.Prim_c args
	      | IL.Mu_c   args        => Nil.Mu_c args
	      | IL.AllArrow_c args    => Nil.AllArrow_c args
	      | IL.ExternArrow_c args => Nil.ExternArrow_c args
	      | IL.Var_c args         => Nil.Var_c args
	      | IL.Let_c args         => Nil.Let_c args
	      | IL.Typeof_c args      => Nil.Typeof_c args
	      | IL.Closure_c args     => Nil.Closure_c args
	      | IL.Crecord_c args     => Nil.Crecord_c args
	      | IL.Proj_c args        => Nil.Proj_c args
	      | IL.App_c args         => Nil.App_c args
	      | IL.Typecase_c args    => Nil.Typecase_c args)

	fun expose (con:Nil.con) : Nil.con_ = 
	  ((*print "Exposing con...";*)
	   let val res = toNil (#con (getNode con))
	   in(* print "...done\n";*)res end)

	fun eq (c1:Nil.con,c2:Nil.con) = c1 = c2

	fun equate (c1:Nil.con,c2:Nil.con) = ()(*c1 := !c2*)
      end


    fun substConInTrace subst (trace : IL.niltrace) : IL.niltrace = 
      let
	fun loop c labs = 
	  (case C.expose c
	     of (Nil.Var_c v) => Nil.TraceKnown (TraceInfo.Compute (v,labs))
	      | (Nil.Proj_c (c,l)) => loop c (l::labs)
	      | _ => error "Non path returned from rewriting trace info")
	     
      in case trace 
	   of Nil.TraceCompute var => 
	     (case CSubst.substitute subst var
		of SOME c => loop c []
		     | NONE => trace)
	    | Nil.TraceKnown (TraceInfo.Compute (var,labels)) => 
		(case CSubst.substitute subst var
		   of SOME c => loop c labels
		    | NONE => trace)
	    | _ => trace
      end

    structure E = 
      struct 
	fun EXP node = ref (IL.EXP node)

	fun Var_e var = EXP {exp=IL.Var_e var,
			     info = ()}

	fun Const_e v = 
	  EXP {exp = IL.Const_e v,
	       info = ()}

	fun Let_e (args as (_,bnds,exp)) = 
	  EXP {exp = IL.Let_e args,
	       info = ()}

	fun Prim_e (args as (_,cons,exps)) = 
	  EXP {exp = IL.Prim_e args,
	       info = ()}

	fun Switch_e (args as switch) = 
	  EXP {exp = IL.Switch_e args,
	       info = ()}
		 
	fun App_e (args as (p,exp,cons,exps,fexps)) = 
	  EXP {exp = IL.App_e args,
	       info = ()}

	fun ExternApp_e (args as (exp,actuals)) = 
	  EXP {exp = IL.ExternApp_e args,
	       info = ()}

	fun Raise_e (args as (exp,con)) = 
	  EXP {exp = IL.Raise_e args,
	       info = ()}

	fun Handle_e (args as{body,bound,handler,result_type}) = 
	  EXP {exp = IL.Handle_e args,
	       info = ()}

	fun push_bnd (f_t,f_e,f_c,f_k) bnd = 
	  let
	    fun do_function (IL.Function{effect, recursive, isDependent,
					 tFormals, eFormals, fFormals,
					 body, body_type}) = 
	      IL.Function({effect = effect, recursive = recursive, isDependent = isDependent,
			   tFormals = Listops.map_second f_k tFormals,
			   eFormals = map (fn (v,t,c) => (v,f_t t,f_c c)) eFormals,
			   fFormals = fFormals,
			   body = f_e body, 
			   body_type = f_c body_type})
	    fun do_closure {code:IL.var, cenv:IL.con, venv:IL.exp, tipe:IL.con} = 
	      {code = code,cenv = f_c cenv,venv = f_e venv,tipe = f_c tipe}
	  in case bnd
	       of IL.Con_b (phase,conbnd)  => IL.Con_b(phase,C.push_cbnd (f_c,f_k) conbnd)
		| IL.Exp_b(v,trace,e)      => IL.Exp_b(v,f_t trace,f_e e) 
		| IL.Fixopen_b vfset       => IL.Fixopen_b (Sequence.map_second do_function vfset)
		| IL.Fixcode_b vfset       => IL.Fixcode_b (Sequence.map_second do_function vfset)
		| IL.Fixclosure_b (r,vc)   => IL.Fixclosure_b (r,Sequence.map_second do_closure vc)
	  end


	fun push (f_t,f_e,f_c,f_k) (node as {exp,info}) =
	  (case exp 
	     of IL.Var_e v   => node
	      | IL.Const_e v =>
	       let
		 val v = 
		   (case v of
		      (Prim.int s)   => v
		    | (Prim.uint s)  => v
		    | (Prim.float s) => v
		    | (Prim.array (c,array)) => error "arrays shouldn't happen"
		    | (Prim.vector (c,array)) => 
			let val array = Array.tabulate (Array.length array,fn i => f_e(Array.sub(array,i)))
			in Prim.vector(f_c c,array)
			end
		    | Prim.refcell (r as (ref e)) => error "refcells shouldn't happen"
		    | Prim.tag (t,c) => Prim.tag (t,f_c c))
	       in {exp=IL.Const_e v,info = info}
	       end
	      | IL.Let_e (sort,bnds,body)  => 
	       {exp=IL.Let_e(sort,map (push_bnd (f_t,f_e,f_c,f_k)) bnds,f_e body),info = info}
	      | IL.Prim_e (ap,clist,elist) => 
		{exp=IL.Prim_e (ap,map f_c clist,map f_e elist),
		 info = info}
	      | IL.Switch_e switch         => 
		let val switch = 
		  (case switch 
		     of IL.Intsw_e {arg, size, arms, default, result_type} =>
		       IL.Intsw_e {arg = f_e arg,size = size,
				   arms = Listops.map_second f_e arms,
				   default = mapopt f_e default,
				   result_type = f_c result_type}
		      | IL.Sumsw_e {arg, sumtype, bound, arms, default, result_type} =>
		       IL.Sumsw_e {arg = f_e arg, 
				   sumtype = f_c sumtype,
				   bound = bound,
				   arms = map (fn (w,t,e) => (w,f_t t,f_e e)) arms,
				   default = mapopt f_e default,
				   result_type = f_c result_type}
		      | IL.Exncase_e {arg, bound, arms, default, result_type} =>
		       IL.Exncase_e {arg = f_e arg, 
				     bound = bound,
				     arms = map (fn (e1,t,e2) => (f_e e1,f_t t,f_e e2)) arms,
				     default = mapopt f_e default,
				     result_type = f_c result_type}
		      | IL.Typecase_e {arg,arms,default, result_type} => 
		       IL.Typecase_e {arg = f_c arg, 
				      arms = map (fn (pcon,vks,e) => (pcon,map_second f_k vks,f_e e)) arms,
				      default = f_e default,
				      result_type = f_c result_type})
		in
		  {exp=IL.Switch_e switch,info = info}
		end
	      | IL.App_e (openness,func,clist,elist,eflist) => 
		{exp=IL.App_e(openness,f_e func,map f_c clist,map f_e elist,map f_e eflist),
		 info = info}
	      | IL.ExternApp_e (exp,args) => {exp=IL.ExternApp_e (f_e exp,map f_e args),info = info}
	      | IL.Raise_e (e,c) => {exp=IL.Raise_e(f_e e,f_c c),info = info}
	      | IL.Handle_e {body,bound,handler,result_type} =>
		{exp=IL.Handle_e {body = f_e body,bound = bound,
				  handler = f_e handler, result_type = f_c result_type},
		 info=info})

	fun push_rename' (node as {exp,info}) =
	  (case exp
	     of IL.Let_e (sort,bnds,body)  => 
	       let 
		 fun do_function subst (IL.Function{effect, recursive, isDependent,
						    tFormals, eFormals, fFormals,
						    body, body_type}) = 
		   let
		     val (tFormals,subst) = K.push_rename_vks' (tFormals,subst)
		     val eFormals = map (fn (v,t,c) => (v,t,substConInCon subst (renameCon c))) eFormals
		     val body      = substConInExp subst (renameExp body)
		     val body_type = substConInCon subst (renameCon body_type)
		   in
		     IL.Function({effect = effect, recursive = recursive, isDependent = isDependent,
				  tFormals  = tFormals,
				  eFormals  = eFormals,
				  fFormals  = fFormals,
				  body      = body, 
				  body_type =  body_type})
		   end
		 fun do_closure subst ({code:IL.var, cenv:IL.con, venv:IL.exp, tipe:IL.con}) = 
		   {code = code,
		    cenv = substConInCon subst (renameCon cenv),
		    venv = substConInExp subst (renameExp venv),
		    tipe = substConInCon subst (renameCon tipe)}
		   
		 fun do_bnd (bnd,subst) = 
		   (case bnd
		      of IL.Con_b (phase,conbnd)  => 
			let val (conbnd,subst) = C.push_rename_cbnd (conbnd,subst)
			in (IL.Con_b(phase,conbnd),subst)
			end
		       | IL.Exp_b(v,trace,e)      => (IL.Exp_b(v,trace,substConInExp subst (renameExp e)),subst)
		       | IL.Fixopen_b vfset       => (IL.Fixopen_b (Sequence.map_second (do_function subst) vfset),subst)
		       | IL.Fixcode_b vfset       => (IL.Fixcode_b (Sequence.map_second (do_function subst) vfset),subst)
		       | IL.Fixclosure_b (r,vc)   => (IL.Fixclosure_b (r,Sequence.map_second (do_closure subst) vc),subst))
		 val (bnds,subst) = foldl_acc do_bnd (CSubst.empty()) bnds
		 val body = substConInExp subst (renameExp body)
	       in
		 {exp=IL.Let_e(sort,bnds,body),info = info}
	       end
	      | IL.Switch_e (IL.Typecase_e {arg,arms,default, result_type}) => 
	       let
		 fun doarm (pcon,vks,e) = 
		   let val (vks,subst) = K.push_rename_vks vks
		   in (pcon,vks,substConInExp subst (renameExp e))
		   end
		 val switch = 
		   IL.Typecase_e {arg = renameCon arg, 
				  arms = map doarm arms,
				  default = renameExp default,
				  result_type = renameCon result_type}
	       in
		 {exp=IL.Switch_e switch,info = info}
	       end
	      | _ => push (fn x => x,renameExp,renameCon,renameKind) node)

	(*push_rename pushes a renaming down one level.  Note that since
	 * renamings cross substitutions, the result is either a substitution
	 * or a real node*)
	fun push_rename e = 
	  (case !e
	     of IL.ECON_SUBST (subst,e) => ref (IL.ECON_SUBST(subst,push_rename e))   (*Push across*)
	      | IL.ERENAME e'           => e ::= push_rename e'                       (*Stacked collapse.  Can memoize*)
	      | IL.EXP node             => EXP (push_rename' node))                   (*Do the actual push*)

	(*Result is not a RENAME *)
	fun rename_elim e =
	  (case !e
	     of IL.ERENAME e' => e ::= push_rename e'
	      | _ => e)


	fun push_subst' (subst,node) = 
	  push (substConInTrace subst,substConInExp subst,substConInCon subst,substConInKind subst) node

	(*push_subst pushes a subst down one level.  Substitutions
	 * do not cross renamings or other substitutions, so the
	 * result must be a node.
	 *)
	fun push_subst(subst,e) = 
	  (case !(rename_elim e)
	     of IL.ECON_SUBST (subst',e) => push_subst  (CSubst.compose(subst,subst'),e)
	      | IL.EXP node              => EXP (push_subst' (subst,node)))
	  
	(*Result is not a CON_SUBST. 
	 * Hence, if argument is not a RENAME, result is a KIND
	 *)
	fun subst_elim e = 
	  (case !e
	     of IL.ECON_SUBST (subst,e') => e ::= push_subst(subst,e')
	      | _ => e)

	(*Return the top level node of the kind*)
	fun getNode e = 
	  (case !(subst_elim(rename_elim e))
	     of IL.EXP node => node)

	fun hide (e:Nil.exp_): Nil.exp  = 
	  (case e
	     of Nil.Var_e var        => Var_e var
	      | Nil.Const_e (v)      => Const_e v
	      | Nil.Let_e args       => Let_e args
	      | Nil.Prim_e args      => Prim_e args
	      | Nil.Switch_e args    => Switch_e args
	      | Nil.App_e args       => App_e args
	      | Nil.ExternApp_e args => ExternApp_e args
	      | Nil.Raise_e args     => Raise_e args
	      | Nil.Handle_e args    => Handle_e args)

	fun toNil (e_ : IL.exp_) : Nil.exp_ = 
	  (case e_
	     of IL.Var_e var        => Nil.Var_e var
	      | IL.Const_e (v)      => Nil.Const_e v
	      | IL.Let_e args       => Nil.Let_e args
	      | IL.Prim_e args      => Nil.Prim_e args
	      | IL.Switch_e args    => Nil.Switch_e args
	      | IL.App_e args       => Nil.App_e args
	      | IL.ExternApp_e args => Nil.ExternApp_e args
	      | IL.Raise_e args     => Nil.Raise_e args
	      | IL.Handle_e args    => Nil.Handle_e args)

	fun expose (e:Nil.exp) : Nil.exp_ = 
	  ((*print "Exposing exp...";*)
	   let val res = toNil(#exp (getNode e))
	   in (*print "...done\n";*)res end)

	fun eq (e1:Nil.exp,e2:Nil.exp) = e1 = e2

	fun equate (e1:Nil.exp,e2:Nil.exp) = ()(*e1 := !e2*)

      end
    
    structure CurtainSubst = 
      struct
	type con_subst = con_subst 
	structure CSubst = CSubst
	val substConInExp   = substConInExp
	val substConInCon   = substConInCon
	val substConInKind  = substConInKind
	val substConInTrace = substConInTrace

	fun substConInCBnd subst cbnd = 
	  if CSubst.is_empty subst then cbnd
	  else C.push_cbnd (substConInCon subst,substConInKind subst) cbnd
	    
	fun substConInBnd subst bnd = 
	  if CSubst.is_empty subst then bnd
	  else E.push_bnd (substConInTrace subst,substConInExp subst,substConInCon subst,substConInKind subst) bnd
      end
  end
