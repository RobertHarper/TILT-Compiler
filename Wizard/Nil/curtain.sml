(*$import CURTAIN *)

structure Curtain :> CURTAIN = 
  struct 


    val error = Util.error

    type info = unit

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

	| Record_k of ((label*var),kind) sequence
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
	  
	withtype kind = {kind:kind_,info:info} ref
	and con       = {con:con_  ,info:info} ref
	and exp       = {exp:exp_  ,info:info} ref

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


    structure K = 
      struct 

	val Type_k = ref {kind = IL.Type_k,info = ()}

	fun SingleType_k arg = ref {kind=IL.SingleType_k arg,
						    info = ()}

	fun Single_k     arg = ref {kind=IL.Single_k arg,
						    info = ()}
						
	fun Record_k lvkseq =
	  ref {kind = IL.Record_k lvkseq,
	       info = ()}
	
	fun Arrow_k (args as (_,vks,k)) = ref {kind = IL.Arrow_k args,
					       info = ()}
	  
	fun hide (k:Nil.kind_): Nil.kind  =
	  (case k
	     of Nil.Type_k            => Type_k
	      | Nil.SingleType_k args => SingleType_k args
	      | Nil.Single_k args     => Single_k args
	      | Nil.Record_k args     => Record_k args
	      | Nil.Arrow_k args      => Arrow_k args)

	fun expose (k:Nil.kind) : Nil.kind_ = 
	  (case #kind (!k)
	     of IL.Type_k            => Nil.Type_k
	      | IL.SingleType_k args => Nil.SingleType_k args
	      | IL.Single_k args     => Nil.Single_k args
	      | IL.Record_k args     => Nil.Record_k args
	      | IL.Arrow_k args      => Nil.Arrow_k args)

	fun eq (k1:Nil.kind,k2:Nil.kind) = k1 = k2

	fun equate (k1:Nil.kind,k2:Nil.kind) = () (*k1 := !k2*)
      end

    structure C = 
      struct 
	

	fun Var_c v = ref {con = IL.Var_c v,
			   info = ()}

	fun Proj_c (args as (con,l)) = ref {con = IL.Proj_c args,
					    info = ()}
	  
	fun Mu_c (args as (flag,defs)) = 
	  ref {con = IL.Mu_c args,
	       info = ()}

	fun Let_c (args as (letsort, cbnds, body)) =
	  ref {con = IL.Let_c args,
	       info = ()}

	fun Prim_c (args as (pcon,cons)) = 
	   ref {con= IL.Prim_c args,
		info = ()}

	fun Crecord_c entries = 
	  ref  {con = IL.Crecord_c entries,
		info = ()}

	fun App_c (args as (con,actuals))     = 
	  ref {con = IL.App_c args,
	       info = ()}

	fun AllArrow_c (args as {openness, effect, isDependent, tFormals, 
				 eFormals, fFormals, body_type}) =
	  ref {con = IL.AllArrow_c args,
	       info = ()}

	fun ExternArrow_c (args as (cons,return)) = 
	  ref {con = IL.ExternArrow_c args,
	       info = ()}

	fun Typeof_c exp = ref {con = IL.Typeof_c exp,
				info = ()}

	fun Typecase_c (args as {arg, arms, default, kind}) =
	  ref {con = IL.Typecase_c args,
	       info = ()}

	fun Closure_c (args as (code,env)) = 
	  ref {con = IL.Closure_c args,
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

	fun expose (con:Nil.con) : Nil.con_ = 
	  (case #con (!con)
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


	fun eq (c1:Nil.con,c2:Nil.con) = c1 = c2

	fun equate (c1:Nil.con,c2:Nil.con) = ()(*c1 := !c2*)
      end


    structure E = 
      struct 
	fun Var_e var = ref {exp=IL.Var_e var,
			     info = ()}

	fun Const_e v = 
	  ref {exp = IL.Const_e v,
	       info = ()}

	fun Let_e (args as (_,bnds,exp)) = 
	  ref {exp = IL.Let_e args,
	       info = ()}

	fun Prim_e (args as (_,cons,exps)) = 
	  ref {exp = IL.Prim_e args,
	       info = ()}

	fun Switch_e (args as switch) = 
	  ref {exp = IL.Switch_e args,
	       info = ()}
		 
	fun App_e (args as (p,exp,cons,exps,fexps)) = 
	  ref {exp = IL.App_e args,
	       info = ()}

	fun ExternApp_e (args as (exp,actuals)) = 
	  ref {exp = IL.ExternApp_e args,
	       info = ()}

	fun Raise_e (args as (exp,con)) = 
	  ref {exp = IL.Raise_e args,
	       info = ()}

	fun Handle_e (args as{body,bound,handler,result_type}) = 
	  ref {exp = IL.Handle_e args,
	       info = ()}

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

	fun expose (e:Nil.exp) : Nil.exp_ = 
	  (case #exp (!e)
	     of IL.Var_e var        => Nil.Var_e var
	      | IL.Const_e (v)      => Nil.Const_e v
	      | IL.Let_e args       => Nil.Let_e args
	      | IL.Prim_e args      => Nil.Prim_e args
	      | IL.Switch_e args    => Nil.Switch_e args
	      | IL.App_e args       => Nil.App_e args
	      | IL.ExternApp_e args => Nil.ExternApp_e args
	      | IL.Raise_e args     => Nil.Raise_e args
	      | IL.Handle_e args    => Nil.Handle_e args)

	fun eq (e1:Nil.exp,e2:Nil.exp) = e1 = e2

	fun equate (e1:Nil.exp,e2:Nil.exp) = () (*e1 := !e2*)

      end
    
  end