(* Mode Checking *)
(* Author: Carsten Schuermann *)
(* Modified: Frank Pfenning *)

functor ModeCheck (structure ModeSyn' : MODESYN
		   structure Paths' : PATHS)
  : MODECHECK =
struct
  structure ModeSyn = ModeSyn'
  structure Paths = Paths'

  exception Error of string

   
  local 
    structure M = ModeSyn
    structure I = ModeSyn.IntSyn
    structure P = Paths
      
    datatype Info =                     (* Groundness information      *)
        Bot				(* I ::= Bot  (unknown)        *)
      | Top                             (*     | Top  (will be ground) *)

    datatype Status =                   (* Variable status             *)
        Existential of			(* S ::= Existential (I, xOpt) *)
	  Info * string option
      | Universal                       (*     | Universal             *)


    (* Representation invariant:

       Groundness information:   
       T stands for ground, B stands for unknown
       Ex  for a named existential variable
       Par for a parameter

       Mode context   D ::= . | D, S

       If D contains Status information for variables in 
       a context G, we write G ~ D
    *)

    exception Error' of P.occ * string

    fun error (r, msg) = raise Error (P.wrap (r, msg))

    (* lookup (a, occ) = mS
     
       Invariant: mS are the argument modes for a
       Raises an error if no mode for a has declared.
       (occ is used in error message)
    *)
    fun lookup (a, occ) =  
        case M.modeLookup a
	  of NONE => raise Error' (occ, "No mode declaration for " ^ I.conDecName (I.sgnLookup a))
           | SOME sM => sM 

    (* nameOf S, selects a name for S *)
    fun nameOf (Existential (_, NONE)) = "?"
      | nameOf (Existential (_, SOME name)) = name
      | nameOf _ = "?"
	   
    (* unique (k, S) = B

       Invariant:
       B iff k does not occur in S
    *)
    fun unique (k, I.Nil) = true
      | unique (k, (I.App (I.Root (I.BVar (k'), I.Nil), S))) =
          (k <> k') andalso unique (k, S)

    (* isUniversal S = B
       
       Invariant:
       B iff S = Par
    *)
    fun isUniversal Universal = true
      | isUniversal _ = false

    (* isTop S = B
       
       Invariant:
       B iff S = Ex (T x)
    *)
    fun isTop (Existential (Top, _)) = true
      | isTop _ = false

      
    (* isPattern (D, k, mS) = B
     
       Invariant: 
       B iff k > k' for all k' in mS
	 and for all k in mS: k is parameter
         and for all k', k'' in mS: k' <> k''
    *)
    fun isPattern (D, k, I.Nil) = true
      | isPattern (D, k, I.App (I.Root (I.BVar (k'), I.Nil), S)) =
          (k > k') 
	  andalso isUniversal (I.ctxLookup (D, k'))
	  andalso isPattern (D, k, S)
	  andalso unique (k', S) 
      | isPattern _ = false


    (* ------------------------------------------- mode context update *)

    (* updateExpN (D, U) = D'
     
       If   G |- U : V     (U in nf)
       and  D ~ G
       then D' = D where all existential variables k
	    with a strict occurrence in U are
	    updated according to the following table
	 
	 D(k)  | D'(k)     
	 -------------
	 par   | par 
	 top   | top 
	 bot   | top 
    *)
    fun updateExpN (D, I.Root (I.BVar (k), S)) =
          if isUniversal (I.ctxLookup (D, k)) 
	    then updateSpineN (D, S)              
	  else 
	    if isPattern (D, k, S)
	      then updateVarD (D, k)
	    else D
      | updateExpN (D, I.Root (I.Const c, S)) =
	  updateSpineN (D, S)
      | updateExpN (D, I.Root (I.Def d, S)) =
	  updateSpineN (D, S)
      | updateExpN (D, I.Lam (_, U)) =
	  I.ctxPop (updateExpN (I.Decl (D, Universal), U))

    (* updateSpineN (D, S) = D'    
     
       If   G |- S : V1 > V2      (S in nf)
       and  D ~ G
       then D' = D where all existenial variables k
            with a strict occurrence in S are
	    updated as described in  updateExpN
    *)
    and updateSpineN (D, I.Nil) = D
      | updateSpineN (D, I.App (U, S)) = 
          updateSpineN (updateExpN (D, U), S)

    (* updateVarD (D, k) = D'

       If   G |- k : V
       and  D ~ G
       and  k is an existential variable
       then D' = D where k is updated as described in  updateExpN
    *)
    and updateVarD (I.Decl (D, Existential (_, name)), 1) =
          I.Decl (D, Existential (Top, name))
      | updateVarD (I.Decl (D, status), k) =
	  I.Decl (updateVarD (D, k-1), status)

    (* ----------------------- mode context update by argument modes *)

    (* updateAtom (D, m, S, mS) = D'
      
       If   G |- S : V > V'   ( S = U1 ; .. ; Un)
       and  D ~ G 
       and  S ~ mS            (mS = m1 , .. , mn)
       and  m mode
       then D' = D where 
            all Ui are updated if mi = m
    *)
    fun updateAtom (D, mode, I.Nil, M.Mnil) = D
      | updateAtom (D, M.Plus, I.App (U, S), M.Mapp (M.Marg (M.Plus, _), mS)) =
          updateAtom (updateExpN (D, U), M.Plus, S, mS) 
      | updateAtom (D, M.Minus, I.App (U, S), M.Mapp (M.Marg (M.Minus, _), mS)) =
          updateAtom (updateExpN (D, U), M.Minus, S, mS) 
      | updateAtom (D, mode, I.App (U, S), M.Mapp (_, mS)) =
          updateAtom (D, mode, S, mS) 


    (* ------------------------------------------- groundness check *)

    (* groundExpN (D, mode, U, occ)  = () 

       If   G |- U : V    (U in nf)
       and  G ~ D
       then groundExpN terminates with () if  D |- U ground 
       else exception Error' is raised

       (occ and mode are used in error messages)
    *)

    fun groundExpN (D, mode, I.Root (I.BVar k, S), occ) =
          (groundVar (D, mode, k, P.head occ);
	   groundSpineN (D, mode, S, (1, occ)))
      | groundExpN (D, mode, I.Root (I.Const c, S), occ) =
	  groundSpineN (D, mode, S, (1, occ))
      | groundExpN (D, mode, I.Root (I.Def d, S), occ) =
	  groundSpineN (D, mode, S, (1, occ))
      | groundExpN (D, mode, I.Lam (_, U), occ) =
	  groundExpN (I.Decl (D, Universal), mode, U, P.body occ)

    (* groundSpineN (D, mode, S, occ)  = () 

       If   G |- S : V1  > V2   (S in nf)
       and  G ~ D
       then groundSpineN terminates with () if  D |- S ground 
       else exception Error' is raised

       (occ and mode are used in error messages)
    *)
    and groundSpineN (D, mode, I.Nil, _) = ()
      | groundSpineN (D, mode, I.App (U, S), (p, occ)) =
          (groundExpN (D, mode, U, P.arg (p, occ));
	   groundSpineN (D, mode, S, (p+1, occ)))

    (* groundVar (D, mode, k, occ)  = () 

       If   G |- k : V1  
       and  G ~ D
       then groundVar terminates with () if  D |- S ground 
       else exception Error' is raised

       (occ and mode are used in error messages)
    *)

    and groundVar (D, mode, k, occ) =
        let
	  val status = I.ctxLookup (D, k) 
	in 
	  if isTop status orelse isUniversal status
	    then ()
	  else raise Error' (occ, "Occurrence of variable " ^ (nameOf status) ^ 
			     " in " ^ (M.modeToString mode) ^ " argument not necessarily ground")
	end

    (* ------------------------------------------- groundness check by polarity *)

    (* groundAtom (D, m, S, mS, occ)  = () 

       If   G |- S : V > V'   ( S = U1 ; .. ; Un)
       and  D ~ G 
       and  S ~ mS            (mS = m1 , .. , mn)
       and  m mode
       then groundAtom terminates with () if 
	    if all Ui are ground for all i, s.t. mi = m
       else exception Error' is raised

       (occ used in error messages)
    *)
    fun groundAtom (D, _, I.Nil, M.Mnil, _) = ()
      | groundAtom (D, M.Plus, I.App (U, S), M.Mapp (M.Marg (M.Plus, _), mS), (p, occ)) =
          (groundExpN (D, M.Plus, U, P.arg (p, occ));
	   groundAtom (D, M.Plus, S, mS, (p+1, occ)))
      | groundAtom (D, M.Minus, I.App (U, S), M.Mapp (M.Marg (M.Minus, _), mS), (p, occ)) =
          (groundExpN (D, M.Minus, U, P.arg (p, occ));
	   groundAtom (D, M.Minus, S, mS, (p+1, occ)))
      | groundAtom (D, mode, I.App (U, S), M.Mapp (_, mS), (p, occ)) =
          groundAtom (D, mode, S, mS, (p+1, occ))


    (* ------------------------------------------- mode checking first phase *)

    (* checkD1 (D, V, occ)  = (D', k) 

       If   G |- V : L
       and  V does not contain Skolem constants 
       and  D ~ G
       then D' is the result of checking V
       and  D' ~ G
       and  forall D'', s.t. D'' ~ G,   
	    (k D'') terminates with () 
	      if all output variables in the head of V are ground wrt D''
	      otherwise exception Error is raised.
	      
       (occ used in error messages)
    *)
    fun checkD1 (D, I.Pi ((I.Dec (name, _), I.Maybe), V), occ) = 
        let 
          val (I.Decl (D', m), k) =
	      checkD1 (I.Decl (D, Existential (Bot, name)), V, P.body occ)
        in
          (D', fn D'' => k (I.Decl (D'', m)))
        end          
      | checkD1 (D, I.Pi ((I.Dec (name, V1), I.No), V2), occ) = 
        let 
          val (I.Decl (D', m), k) =
	      checkD1 (I.Decl (D, Existential (Bot, name)), V2, P.body occ)
        in
          (checkG1 (D', V1, P.label occ), 
	   fn D'' => k (I.Decl (D'', m)))
        end 
      | checkD1 (D, I.Root (I.Const a, S), occ) =
	let 
	  val mS = lookup (a, occ)
	in
	  (updateAtom (D, M.Plus, S, mS), 
	   fn D' => groundAtom (D', M.Minus, S, mS, (1, occ)))
	end
      | checkD1 (D, I.Root (I.Def a, S), occ) =
	let 
	  val mS = lookup (a, occ)
	in
	  (updateAtom (D, M.Plus, S, mS), 
	   fn D' => groundAtom (D', M.Minus, S, mS, (1, occ)))
	end

    (* checkDlocal (D, V, occ) = ()

       Invariant:
       If   G |- V : L
       and  D ~ G
       then checkD terminates with ()  iff V is mode correct.
       otherwise exception Error is raised

       (occ used in error messages)
    *)
    and checkDlocal (D, V, occ) =
        let 
	  val (D', k) = checkD1 (D, V, occ)
	in
	  k D'				(* check output arguments in clause head *)
	end
 
    (* checkG1 (D, V)  = D' 

       If   G |- V : L
       and  V does not contain Skolem constants 
       and  D ~ G
       then D' ~ G 
       and  D' is the result of checking V 

       Effect: raises exception Error if not mode-correct.
       (occ used in error messages)
    *)
    and checkG1 (D, I.Pi ((_, I.Maybe), V), occ) =
          I.ctxPop (checkG1 (I.Decl (D, Universal), V, P.body occ)) 
      | checkG1 (D, I.Pi ((I.Dec (_, V1) , I.No), V2), occ) = 
	  (checkDlocal (D, V1, P.label occ); 
	   I.ctxPop (checkG1 (I.Decl (D, Universal), V2, P.body occ)))
      | checkG1 (D, I.Root (I.Const a, S), occ) =
	let 
	  val mS = lookup (a, occ)
	in 
	  (groundAtom (D, M.Plus, S, mS, (1, occ)); 
	   updateAtom (D, M.Minus, S, mS))
	end
      | checkG1 (D, I.Root (I.Def d, S), occ) =
        let 
	  val mS = lookup (d, occ)
	in 
	  (groundAtom (D, M.Plus, S, mS, (1, occ)); 
	   updateAtom (D, M.Minus, S, mS))
	end


    (* --------------------------------------------------------- mode checking *)


    (* checkD (ConDec, ocOpt)  = () 

       checkD terminates with () if ConDec is mode correct
       otherwise exception Error is raised

       (ocOpt is used in error messages)
    *)
    fun checkD (conDec, ocOpt) = 
        let 
	  fun checkable (I.Root (I.Const (a), _)) = 
	      (case (M.modeLookup a) 
		 of NONE => false
	          | SOME _ => true)
	    | checkable (I.Uni _) = false
	    | checkable (I.Pi (_, V)) = checkable V
	  val V = I.conDecType conDec
	in
	  if (checkable V)
	    then checkDlocal (I.Null, V, P.top) 
	         handle Error' (occ, msg) =>   
		   (case ocOpt 
		      of NONE => raise Error (msg)
		       | SOME occTree => error (P.occToRegionDec occTree occ, msg))
	  else ()
	end

  in
    val checkD = checkD
  end
end;  (* functor ModeCheck *)
