functor Nilutil(structure Nil : NIL) : NILUTIL =
struct
    structure Nil = Nil

    open Nil

    fun error s = Util.error "nilutil.sml"

    fun extend f (x,y) z = if (Name.eq_var(x,z)) then y else f z

    (* collections *)
 
    type 'a collection = 'a list

    val emptyCollection = nil
    val collMember = Listops.member_eq
    fun collInsert (lst, x) = x :: lst

    datatype 'a changeopt = NOCHANGE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
    datatype state =
	STATE of {boundcvars : var collection,
		  boundevars : var collection,
		  conhandler : var collection * con -> con changeopt,
                  exphandler : var collection * exp -> exp changeopt}

    fun stateBindcvar (STATE{boundcvars,boundevars,conhandler,exphandler}) v =
	(STATE{boundcvars = collInsert (boundcvars, v),
	       boundevars = boundevars,
	       conhandler = conhandler,
	       exphandler = exphandler})

    fun stateBindcvars state nil = state
      | stateBindcvars state (v::vs)= stateBindcvars (stateBindcvar state v) vs

    fun stateBindevar (STATE{boundcvars,boundevars,conhandler,exphandler}) v =
	(STATE{boundcvars = boundcvars,
	       boundevars = collInsert (boundevars, v),
	       conhandler = conhandler,
	       exphandler = exphandler})

    fun stateBindevars state nil = state
      | stateBindevars state (v::vs)= stateBindevars (stateBindcvar state v) vs
		  
    fun doSubstCon (state as (STATE{boundcvars,conhandler,...})) c =
	let 
	    fun do_cbnd state (Con_cb(var, k, con)) = 
		(var,Con_cb(var, doSubstKind state k,
			    doSubstCon state con))
	      | do_cbnd state (Fun_cb(var,openness, args, body, kind)) = 
		let
		    val vars   = map (fn (var,_) => var) args
		    val args'  = map (fn (var,knd) => (var, doSubstKind state knd))
			args
		    val state' = stateBindcvars state vars
		    val body'  = doSubstCon state' body
		    val kind'  = doSubstKind state' kind
		in
		    (var,Fun_cb(var,openness, args', body', kind'))
		end
	    fun docon (Prim_c (prim, cons)) =
		Prim_c(prim, map (doSubstCon state) cons)
	      | docon (Mu_c(set, var)) = 
		let
		    val names = map (fn (name,_) => name) (Util.set2list set)
		    val state' = stateBindcvars state names
			
		    fun fixMubnd (name, con) = (name, doSubstCon state' con)
		in
		    Mu_c(Util.mapset fixMubnd set, var)
		end
	      | docon (Arrow_c(openness, confun)) =
		Arrow_c(openness, doSubstConfun state confun)
	      | docon (Code_c confun) =
		Code_c (doSubstConfun state confun)
	      | docon (con as Var_c cvar) = con
	      | docon (Let_c (Parallel, bnds, body)) =
		let
		    val var_bnds'  = map (do_cbnd state) bnds
		    val cvars = map #1 var_bnds'
		    val bnds' = map #2 var_bnds'
		    val state' = stateBindcvars state cvars
		    val body'  = doSubstCon state' body
		in
		    Let_c (Parallel, bnds', body')
		end
	      | docon (Let_c (Sequential, bnds, body)) =
		let
		    val (state', bnds') =
			let fun loop state [] = (state, [])
			      | loop state (cbnd::rest) =
				let
				    val (cvar,cbnd') = do_cbnd state cbnd
				    val state' = stateBindcvar state cvar
				    val (state'', rest') = loop state' rest
				in
				    (state'', cbnd' :: rest)
				end
			in
			    loop state bnds
			end
		    val body' = doSubstCon state' body
		in
		    Let_c (Sequential, bnds', body')
		end
	      | docon (Crecord_c fields) =
		let fun dorbnd(lbl,con) = (lbl, doSubstCon state con)
		    val fields' = map dorbnd fields
		in  Crecord_c fields'
		end
	      | docon (Proj_c (con, lbl)) =
		let  val con' = doSubstCon state con
		in   Proj_c (con', lbl)
		end
	      | docon (Closure_c (ccode, cenv)) = 
		let val ccode' = doSubstCon state ccode
		    val cenv' = doSubstCon state cenv
		in  Closure_c(ccode', cenv')
		end
	      | docon (App_c (f, args)) =
		let val f' = doSubstCon state f
		    val args' = map (doSubstCon state) args
		in  App_c (f', args')
		end
	      | docon (Annotate_c (annot, con)) =
		let val con' = doSubstCon state con
		in  Annotate_c (annot, con)
		end
	    
	in
	    case (conhandler(boundcvars,c)) of
		NOCHANGE => docon c
	      | CHANGE_NORECURSE c' => c'
	      | CHANGE_RECURSE c' => docon c'
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

    and doSubstKind state (kind as (Type_k _ | Word_k _)) = kind

      | doSubstKind state (Singleton_k(p, kind, con)) =
	let
	    val kind' = doSubstKind state kind
	    val con' = doSubstCon state con
	in
	    Singleton_k(p, kind', con')
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
	    fun conhandler (boundcvars,Var_c v) = 
		if (collMember(Name.eq_var,v,boundcvars))
		    then NOCHANGE
		else (case (conmap v) of
			  NONE => NOCHANGE
			| SOME c => CHANGE_RECURSE c)
	      | conhandler _ = NOCHANGE
	    val state = STATE{boundcvars = emptyCollection,
			      boundevars = emptyCollection,
			      conhandler = conhandler,
			      exphandler = fn _ => NOCHANGE}
	in
	    doSubstCon state
	end

    fun generate_tuple_label _ = raise Div
    fun exp_tuple _ = raise Div
    fun con_tuple _ = raise Div
    fun con_tuple_inject _ = raise Div
    fun kind_tuple _ = raise Div

end
