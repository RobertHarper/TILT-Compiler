functor Nilutil(structure Nil : NIL) : NILUTIL =
struct
    structure Nil = Nil

    open Nil

    fun error s = Util.error "nilutil.sml"

    fun extend f (x,y) z = if (Name.eq_var(x,z)) then y else f z

    (* collections *)
 
    type 'a collection = 'a list

    val emptyCollection = nil

    fun collMember eqitem (lst, x) =
	let
	    fun loop [] = false
	      | loop (y::ys) = (eqitem (x,y)) orelse (loop ys)
	in
	    loop lst
	end

    fun collInsert (lst, x) = x :: lst


    datatype state =
	STATE of {boundcvars : var collection,
		  boundevars : var collection,
		  conmap : var -> con option,
                  expmap : var -> exp option}

    fun stateBindcvar (STATE{boundcvars,boundevars,conmap,expmap}) v =
	(STATE{boundcvars = collInsert (boundcvars, v),
	       boundevars = boundevars,
	       conmap = conmap,
	       expmap = expmap})

    fun stateBindcvars state nil = state
      | stateBindcvars state (v::vs)= stateBindcvars (stateBindcvar state v) vs

    fun stateBindevar (STATE{boundcvars,boundevars,conmap,expmap}) v =
	(STATE{boundcvars = boundcvars,
	       boundevars = collInsert (boundevars, v),
	       conmap = conmap,
	       expmap = expmap})

    fun stateBindevars state nil = state
      | stateBindevars state (v::vs)= stateBindevars (stateBindcvar state v) vs
		  
    fun doSubstCon state (Prim_c (prim, cons)) =
	  Prim_c(prim, map (doSubstCon state) cons)

      | doSubstCon state (Mu_c(set, var)) = 
	let
	    val names = map (fn (name,_) => name) (Util.set2list set)
	    val state' = stateBindcvars state names
		
	    fun fixMubnd (name, con) = (name, doSubstCon state' con)
	in
	    Mu_c(Util.mapset fixMubnd set, var)
	end

      | doSubstCon state (Arrow_c(openness, confun)) =
	Arrow_c(openness, doSubstConfun state confun)

      | doSubstCon state (Code_c confun) =
	Code_c (doSubstConfun state confun)

      | doSubstCon (STATE{boundcvars,conmap,...}) (con as Var_c cvar) =
	if (collMember Name.eq_var (boundcvars, cvar)) then
	    con
	else
	    (case (conmap cvar) of
		 NONE => con
	       | SOME subst_con => subst_con)

      | doSubstCon state (Let_c (Parallel, bnds, body)) =
	let
	    val cvars  = map (fn (cvar, _) => cvar) bnds
	    val bnds'  = map (fn (var, con) => (var, doSubstCon state con))
		            bnds
	    val state' = stateBindcvars state cvars
	    val body'  = doSubstCon state' body
	in
	    Let_c (Parallel, bnds', body')
	end

      | doSubstCon state (Let_c (Sequential, bnds, body)) =
	let
	    val (state', bnds') =
		let fun loop state [] = (state, [])
		      | loop state ((cvar, con)::rest) =
			let
			    val con'   = doSubstCon state con
			    val state' = stateBindcvar state cvar
			    val (state'', rest') = loop state' rest
			in
			    (state'', (cvar, con') :: rest)
			end
		in
		    loop state bnds
		end
	    val body' = doSubstCon state' body
	in
	    Let_c (Sequential, bnds', body')
	end

      | doSubstCon state (Fun_c(openness, args, body)) =
	let
	    val vars   = map (fn (var,_) => var) args
	    val args'  = map (fn (var,knd) => (var, doSubstKind state knd))
		           args
	    val state' = stateBindcvars state vars
	    val body'  = doSubstCon state' body
	in
	    Fun_c(openness, args', body')
	end

      | doSubstCon state (Crecord_c fields) =
	let
	    val fields' = map (fn (lbl,con) => (lbl, doSubstCon state con))
                             fields
	in
	    Crecord_c fields'
	end

      | doSubstCon state (Proj_c (con, lbl)) =
	let
	    val con' = doSubstCon state con
	in
	    Proj_c (con', lbl)
	end

      | doSubstCon state (Closure_c (ccode, cenv)) = 
	let
	    val ccode' = doSubstCon state ccode
	    val cenv' = doSubstCon state cenv
	in
	    Closure_c(ccode', cenv')
	end

      | doSubstCon state (App_c (f, args)) =
	let
	    val f' = doSubstCon state f
	    val args' = map (doSubstCon state) args
	in
	    App_c (f', args')
	end

      | doSubstCon state (Annotate_c (annot, con)) =
	let
	    val con' = doSubstCon state con
	in
	    Annotate_c (annot, con)
	end

    and doSubstConfun state (effect, knds, cons, result) =
	let
	    val poly_vars = map (fn (var,_) => var) knds
	    val knds'  = map (fn (var,knd) => (var,doSubstKind state knd)) knds
	    val state' = stateBindcvars state poly_vars
	    val cons'  = map (doSubstCon state') cons
	    val result' = doSubstCon state' result
	in
	    (effect, knds', cons', result')
	end

    and doSubstKind state (kind as Type_k) = kind

      | doSubstKind state (kind as Word_k) = kind

      | doSubstKind state (Singleton_k(kind, con)) =
	let
	    val kind' = doSubstKind state kind
	    val con' = doSubstCon state con
	in
	    Singleton_k(kind', con')
	end

      | doSubstKind state (Record_k fieldseq) =
	let
	    fun fixFields state [] = []
              | fixFields state (((lbl, var), kind)::rest) = 
		let
		    val kind'  = doSubstKind state kind
		    val state' = stateBindcvar state var
		    val rest'  = fixFields state' rest
		in
		    ((lbl, var), kind') :: rest'
		end
	in
	    (* hack *)
	    Record_k (fixFields state (Util.sequence2list fieldseq))
	end
		
      | doSubstKind state (Arrow_k (openness, args, result)) =
	let
	    val (args', result') = doSubstKindfun state (args, result)
	in
	    Arrow_k (openness, args, result)
	end

      | doSubstKind state (Code_k kindfun) =
	Code_k (doSubstKindfun state kindfun)

    and doSubstKindfun state (args, result) =
	let
	    val cvars  = map (fn (cvar,_) => cvar) args
	    val args'  = map (fn (cvar,kind) => (cvar, doSubstKind state kind))
		           args
	    val state' = stateBindcvars state cvars
	    val result' = doSubstKind state' result
	in
	    (args', result')
	end

    fun substConInCon conmap =
        let
	    val state = STATE{boundcvars = emptyCollection,
			      boundevars = emptyCollection,
			      conmap = conmap,
			      expmap = fn _ => NONE}
	in
	    doSubstCon state
	end
end
