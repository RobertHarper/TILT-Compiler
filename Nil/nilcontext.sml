functor NilContextFn(structure ArgNil : NIL
		      structure PpNil : PPNIL
		      structure NilUtil : NILUTIL
		      structure Subst : NILSUBST
		      sharing PpNil.Nil = NilUtil.Nil = ArgNil
		        and type Subst.con = ArgNil.con
		        and type Subst.exp = ArgNil.exp
		        and type Subst.kind = ArgNil.kind) :(*>*)
  NILCONTEXT where Nil = ArgNil 
  and type 'a subst = 'a Subst.subst = 
struct
  structure Nil = ArgNil

  type kind = Nil.kind
  type con = Nil.con
  type var = Nil.var
  type 'a subst = 'a Subst.subst

  val var2string = Name.var2string
  val mapsequence = Util.mapsequence
  val get_phase = NilUtil.get_phase
  val selfify = NilUtil.selfify    


  val Var_c = Nil.Var_c

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
		  kindmap : kind map,
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

  fun print_kind (var,kind) =
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
     kindmap = V.insert (kindmap, var, kind)}

  fun insert_kind_equation ({conmap,kindmap,equation}:context,var,con,kind) = 
    {conmap = conmap, 
     equation = V.insert(equation, var, con),
     kindmap = V.insert(kindmap, var, kind)}

  fun insert_kind_list ({conmap,kindmap,equation}:context,vklist) = 
    {conmap = conmap, 
     equation=equation,
     kindmap = foldl (fn ((v,k),kindmap) => V.insert (kindmap, v, k)) kindmap vklist}


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

  fun find_kind ({kindmap,...}:context,var) = 
    (case (V.find (kindmap, var)) of
          SOME k => k
	| NONE => raise Unbound)
    
  fun remove_kind ({conmap,kindmap,equation},var) = 
    let val equation = #1 (V.remove (equation, var))
    in  {conmap = conmap, 
         kindmap = kindmap,
         equation = equation}
    end

  fun find_kind_equation(s as {kindmap,equation,...}:context,con) : con option = 
    let open Nil
	datatype result = CON of con | KIND of kind
	exception Opaque
        fun project_kind(k,c,l,subst) = 
	  (case k of
	     Record_k lvk_seq => 
		let val lvk_list = Util.sequence2list lvk_seq
		    fun loop _ [] = error "find_kind_equation.project_kind: missing label"
		      | loop subst (((l',v),k)::rest) = 
			  if (Name.eq_label(l,l'))
			    then (KIND k, subst)
			  else loop (Subst.add subst (v,Proj_c(c,l'))) rest
		in  loop subst lvk_list
		end
	   | Singleton_k(_,_,c) => (CON(Proj_c(c,l)), subst)
	   | _ => error "bad kind to project_kind")
        fun app_kind(k,c1,clist,subst) = 
	  (case k of
	     Arrow_k (openness,vklist, k) => 
		let fun folder(((v,k),c),subst) = Subst.add subst (v,c)
		in  (KIND k, foldl folder subst (Listops.zip vklist clist))
		end
	   | Singleton_k(_,_,c) => (CON(App_c(c,clist)), subst)
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
          | _ => (print "traverse given non-path = \n";
		  PpNil.pp_con c; print "\n";
		  error "traverse given non-path")
     in (case traverse con of
	   (CON c, _) => SOME c
	 | (KIND (Singleton_k(_,_,c)),subst) => SOME (Subst.substConInCon subst c)
	 | _ => NONE)
	handle Opaque => NONE
	    | e => (print "find_kind_equation given = \n";
		  PpNil.pp_con con; print "\n"; raise e)
     end

end