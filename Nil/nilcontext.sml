(*$import NIL PPNIL NILUTIL NILSUBST Stats NILCONTEXT *)
functor NilContextFn(structure ArgNil : NIL
		      structure PpNil : PPNIL
		      structure NilUtil : NILUTIL
		      structure Subst : NILSUBST
		      sharing PpNil.Nil = NilUtil.Nil = ArgNil
		        and type Subst.con = ArgNil.con
		        and type Subst.exp = ArgNil.exp
		        and type Subst.kind = ArgNil.kind) 
  :> NILCONTEXT where Nil = ArgNil 
                where type 'a subst = 'a Subst.subst = 
struct
  structure Nil = ArgNil
  open Nil Util Listops Name
  open Prim

  type 'a subst = 'a Subst.subst

  val var2string = Name.var2string
  val selfify = NilUtil.selfify    



  fun error s = Util.error "nilcontext.sml" s

  val profile = Stats.bool "nil_profile"
  val (nilcontext_kinds_bound,
       nilcontext_kinds_renamed) =
    (Stats.counter "nilcontext_kinds_bound",
     Stats.counter "nilcontext_kinds_renamed")
    
  structure V = Name.VarMap
    
  type 'a map = 'a V.map
    
  structure S = Name.VarSet

  type set = S.set
  exception Unbound

  (*The con in the kindmap is the result of pulling
   * and normalizing the kind.  
   *)
  type context = {equation: con map,
		  kindmap : (kind * kind option ref) map,
		  conmap  : con map}




  fun print_con_kind (var,(con,kind)) =
    (print (Name.var2string var);
     print "=";
     PpNil.pp_con con;
     print "::";
     PpNil.pp_kind kind;
     print "\n")

  fun print_con (var,con) =
    (print (Name.var2string var);
     print ":";
     PpNil.pp_con con;
     print "\n")

  fun print_kind (var,(kind,_)) =
    (print (Name.var2string var);
     print ":";
     PpNil.pp_kind kind;
     print "\n")

  fun print_kinds ({kindmap,...}:context) = 
    (print "\n Constructor variables and kinds are :\n";
     V.appi print_kind kindmap)

  fun print_cons ({conmap,...}:context) = 
    (print "\n Expression variables and constructors are :\n";
     V.appi print_con conmap)

  fun print_context ({kindmap,conmap,equation}:context) = 
    (print "\n Constructor variables and kinds are :\n";
     V.appi print_kind kindmap;
     print "\n Equations are :\n";
     V.appi print_con equation;
     print "\n Expression variables and constructors are :\n";
     V.appi print_con conmap)

  val empty : context = {equation = V.empty,
			 kindmap = V.empty,
			 conmap = V.empty}


  fun insert_con ({conmap,equation,kindmap}:context,var,con) = 
    {conmap = V.insert (conmap, var, con), 
     kindmap = kindmap,
     equation=equation}

  fun insert_con_list (C:context,defs : (var * con) list) =
    List.foldl (fn ((v,c),C) => insert_con (C,v,c)) C defs

  fun find_con ({conmap,...}:context,var) = 
      (case V.find (conmap, var) of
	   SOME con => con
	 | NONE => raise Unbound)
    
  fun remove_con ({conmap,equation,kindmap}:context,var) = 
    let val conmap = #1 (V.remove (conmap, var))
    in  {conmap = conmap, kindmap = kindmap, equation=equation}
    end

  fun insert_kind ({conmap,kindmap,equation}:context,var,kind) = 
    {conmap = conmap, 
     equation=equation,
     kindmap = V.insert (kindmap, var, (kind, ref NONE))}

  fun insert_kind_equation ({conmap,kindmap,equation}:context,var,con,kind) = 
    {conmap = conmap, 
     equation = V.insert(equation, var, con),
     kindmap = V.insert(kindmap, var, (kind, ref NONE))}

  fun insert_kind_list ({conmap,kindmap,equation}:context,vklist) = 
    {conmap = conmap, 
     equation=equation,
     kindmap = foldl (fn ((v,k),kindmap) => V.insert (kindmap, v, (k, ref NONE))) kindmap vklist}


  fun bind_kind (D:context,var,kind) =
    let val (var',subst) = (case V.find(#kindmap D,var) of
			     NONE => (var,Subst.empty())
			   | SOME _ => 
			      let val var' = Name.derived_var var
			      in  (var',Subst.add (Subst.empty ()) (var,Var_c var'))
			      end)
    in (insert_kind(D,var',kind), var', subst)
    end


  fun bind_kind_list (D:context,vklist) = 
    let
      fun folder ((v,k),(D,vklist,s)) = 
	let val (D,v,subst) = bind_kind(D,v,k)
	    val s = Subst.con_subst_compose(subst,s)
	in  (D,(v,k)::vklist,s)
        end
      val (ctxt,rev_vklist,subst) = foldl folder (D,[],Subst.empty()) vklist
    in  (ctxt,rev rev_vklist,subst)
    end


  fun make_shape D kind = 
    (case kind of
          Type_k => kind
	| Singleton_k con => get_shape D con
	| Record_k elts => 
	   let fun folder(((l,v),k),D) = (((l,v),make_shape D k),insert_kind(D,v,k))
	   in  Record_k (#1(Sequence.foldl_acc folder D elts))
	   end
	| Arrow_k (openness, formals, return) => 
	 let fun folder((v,k),D) = ((v,make_shape D k), insert_kind(D,v,k))
	     val (formals,D) = foldl_acc folder D formals
	     val return = make_shape D return
	 in  (Arrow_k (openness, formals,return))
	 end)

  and get_shape (D : context) (constructor : con) : kind = 
      (case constructor of
	 Prim_c (Int_c W64,_) => Type_k
       | Prim_c (Float_c F32,_) => Type_k (* error? *)
       | Prim_c (Float_c F64,_) => Type_k (* error? *)
       | Prim_c _ => Type_k

	| (Mu_c (recur,defs)) => let val len = Sequence.length defs
				 in  if len = 1
					 then Type_k
				     else NilUtil.kind_tuple(Listops.copy(len,Type_k))
				 end
	| (AllArrow_c (openness,effect,tformals,formals,numfloats,body)) => Type_k
	| (Var_c var) => find_shape_kind (D,var)
        | Let_c (sort,[Open_cb (var,formals,body,body_kind)],Var_c v) =>
	   if (eq_var(var,v)) then Arrow_k(Open,formals,body_kind) else get_shape D (Var_c v)
	| Let_c(sort,[Code_cb (var,formals,body,body_kind)],Var_c v) =>
	   if (eq_var(var,v)) then Arrow_k(Code,formals,body_kind) else get_shape D (Var_c v)
        | Let_c (sort,[Code_cb (var,formals,body,body_kind)],Closure_c(Var_c v,_)) =>
	   if (eq_var(var,v)) then Arrow_k(Closure,Listops.butlast formals,body_kind) 
	   else get_shape D (Var_c v)
        | Let_c (sort,[],body) => get_shape D body
        | Let_c (sort,cbnd::rest,body) => 
	       let val D = (case cbnd of
				Con_cb(v,c) => insert_kind(D,v,Singleton_k c)
			      | Open_cb(v,formals,_,body_kind) => insert_kind(D,v,Arrow_k(Open,formals,body_kind))
			      | Code_cb(v,formals,_,body_kind) => insert_kind(D,v,Arrow_k(Code,formals,body_kind)))
	       in  get_shape D (Let_c(sort,rest,body))
	       end
	| (Closure_c (code,env)) => 
	    let val (vklist,body_kind) = 
		(case get_shape D code of
	          Arrow_k (Code,vklist,body_kind) => (vklist,body_kind)
		| Arrow_k (ExternCode,vklist,body_kind) => (vklist,body_kind)
		| k => (print "Invalid closure: code component does not have code kind\n";
			PpNil.pp_kind k; print "\n";
			error "Invalid closure: code component does not have code kind" 
			handle e => raise e))		      
		val (first,(v,klast)) = split vklist
		val kind = Arrow_k(Closure,first,body_kind)
	    in kind
	    end

	| (Crecord_c entries) => 
	 let
	   val (labels,cons) = unzip entries
	   val kinds = (map (get_shape D) cons)
	   val k_entries = 
	     map2 (fn (l,k) => ((l,fresh_named_var "crec_norm"),k)) (labels,kinds)
	   val entries = zip labels cons
	 in Record_k (Sequence.fromList k_entries)
	 end
	| (Proj_c (rvals,label)) => 
	 let
	   val record_kind = get_shape D rvals
	   fun find D [] = error "could not find field in record kind"
	     | find D (((l,v),k)::rest) = 
	       if (eq_label(l,label))
		   then make_shape D k (* there may have been dependencies *)
	       else find (insert_kind(D,v,k)) rest
	 in  (case record_kind of
		Record_k kinds => find D (Sequence.toList kinds)
	      | other => 
		    (print "Non-record kind returned from get_shape in projection:\n";
		     PpNil.pp_kind other; print "\n";
		     error "Non-record kind returned from get_shape in projection" 
		     handle e => raise e))
	 end
	| (App_c (cfun,actuals)) => 
	    let
	      val cfun_kind = get_shape D cfun
	      val (formals,body_kind) = 
		case cfun_kind of
		  (Arrow_k (_,formals,body_kind)) => (formals,body_kind)
		| _ => (print "Invalid (non-arrow) kind for constructor application\n";
			PpNil.pp_kind cfun_kind; print "\n";
			(error "Invalid kind for constructor application" handle e => raise e))
	      fun folder (((v,k),c),D) = insert_kind_equation(D,v,c,k)
	      val D = foldl folder D (zip formals actuals)
	    in make_shape D body_kind
	    end
	| (Typecase_c {arg,arms,default,kind}) => kind
	| (Annotate_c (annot,con)) => get_shape D con)




  and find_kind ({kindmap,...}:context,var) = 
    (case (V.find (kindmap, var)) of
          SOME (k,_) => k
	| NONE => raise Unbound)

  and find_shape_kind (ctxt as {kindmap,equation,...}:context,var) = 
    (case (V.find (kindmap, var)) of
          SOME (k,r) => (case !r of
			     SOME k => k
			   | NONE => let val k = (case V.find(equation,var) of
						      SOME c => get_shape ctxt c
						    | NONE => make_shape ctxt k)
				     in (r := SOME k; k)
				     end)
	| NONE => raise Unbound)

  fun remove_kind ({conmap,kindmap,equation},var) = 
    let val equation = #1 (V.remove (equation, var))
    in  {conmap = conmap, 
         kindmap = kindmap,
         equation = equation}
    end

  fun find_kind_equation(s as {equation,...}:context,con) : con option = 
    let open Nil
	datatype result = CON of con | KIND of kind
	exception Opaque
        fun project_kind(k,c,l,subst) = 
	  (case k of
	     Record_k lvk_seq => 
		let val lvk_list = Sequence.toList lvk_seq
		    fun loop _ [] = error "find_kind_equation.project_kind: missing label"
		      | loop subst (((l',v),k)::rest) = 
			  if (Name.eq_label(l,l'))
			    then (KIND k, subst)
			  else loop (Subst.add subst (v,Proj_c(c,l'))) rest
		in  loop subst lvk_list
		end
	   | Singleton_k c => (CON(Proj_c(c,l)), subst)
	   | _ => error "bad kind to project_kind")
        fun app_kind(k,c1,clist,subst) = 
	  (case k of
	     Arrow_k (openness,vklist, k) => 
		let fun folder(((v,k),c),subst) = Subst.add subst (v,c)
		in  (KIND k, foldl folder subst (Listops.zip vklist clist))
		end
	   | Singleton_k c => (CON(App_c(c,clist)), subst)
	   | _ => error "bad kind to app_kind")
        fun traverse c : result * con subst = 
	  case c of
             (Var_c v) => (case V.find(equation,v) of
			  SOME c => (CON c, Subst.empty())
			| NONE => (KIND (find_kind(s,v)), Subst.empty()))
           | (Proj_c (c,l)) => 
		let val (res,subst) = traverse c
		in  case res of
		      CON c => (CON(Proj_c(c,l)), subst)
		    | KIND k => project_kind(k,c,l,subst)
		end
           | (App_c(c1,clist)) =>
		let val (res,subst) = traverse c1
		in  case res of
		      CON c => (CON(App_c(c,clist)), subst)
		    | KIND k => app_kind(k,c1,clist,subst)
		end
          | _ => ((* print "traverse given non-path = \n";
		     PpNil.pp_con c; print "\n"; *)
		  raise Opaque)
     in (case traverse con of
	   (CON c, _) => SOME c
	 | (KIND (Singleton_k c),subst) => SOME (Subst.substConInCon subst c)
	 | _ => NONE)
	handle Opaque => NONE
	    | e => (print "find_kind_equation given = \n";
		  PpNil.pp_con con; print "\n"; raise e)
     end

end