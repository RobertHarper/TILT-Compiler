(* Parsing Thm Declarations *)
(* Author: Carsten Schuermann *)

functor ParseThm
  (structure Paths : PATHS
   structure Parsing' : PARSING
     sharing Parsing'.Lexer.Paths = Paths
   structure ThmExtSyn' : THMEXTSYN
     sharing ThmExtSyn'.Paths = Paths
   structure ParseTerm : PARSE_TERM
     sharing ParseTerm.Parsing.Lexer = Parsing'.Lexer
     sharing ParseTerm.ExtSyn = ThmExtSyn'.ExtSyn)
     : PARSE_THM =
struct
  structure Parsing = Parsing'
  structure ThmExtSyn = ThmExtSyn'

  local
    structure L = Parsing.Lexer
    structure LS = Parsing.Lexer.Stream  
    structure E = ThmExtSyn
    structure P = Paths

    (*--------------------------*)
    (* %terminates declarations *)
    (*--------------------------*)

    (* idToNat (region, (idCase, name)) = n
       where n an natural number indicated by name, which should consist
       of all digits.  Raises error otherwise, or if integer it too large
    *)
    fun idToNat (r, name) =
      L.stringToNat (name)
      handle Overflow => Parsing.error (r, "Integer too large")
	   | L.NotDigit _ => Parsing.error (r, "Identifier not a natural number")

    fun stripRParen (LS.Cons ((L.RPAREN, r), s')) = LS.expose s'
      | stripRParen (LS.Cons ((t, r), _))  = 
          Parsing.error (r, "Expected `)', found " ^ L.toString t)

    fun decideRBrace (r0, (orders, LS.Cons ((L.RBRACE, r), s'))) =
          (SOME(E.lex (r0, orders)), LS.expose s')
      | decideRBrace (r0, (order, LS.Cons ((t, r), _)))  = 
          Parsing.error (P.join(r0,r), "Expected `}', found " ^ L.toString t)

    fun decideRBracket (r0, (orders, LS.Cons ((L.RBRACKET, r), s'))) =
          (SOME(E.simul (r0, orders)), LS.expose s')
      | decideRBracket (r0, (order, LS.Cons ((t, r), _)))  = 
          Parsing.error (P.join(r0,r), "Expected `]', found " ^ L.toString t)

    fun decideRParen (r0, (ids, LS.Cons ((L.RPAREN, r), s'))) =
          (SOME(E.varg (r,ids)), LS.expose s')
      | decideRParen (r0, (order, LS.Cons ((t, r), _)))  = 
          Parsing.error (P.join(r0,r), "Expected `)', found " ^ L.toString t)

    (* parseIds "id ... id" = ["id",...,"id"] *)
    (* terminated by non-identifier token *)
    fun parseIds (LS.Cons ((L.ID (L.Upper, id), r), s')) =
        let 
	  val (ids, f') = parseIds (LS.expose s') 
	in 
	  (id :: ids, f')
	end
      | parseIds (LS.Cons ((t as L.ID (_, id), r), s')) =
	Parsing.error (r, "Expecter upper case identifier, found " ^ L.toString t)
      | parseIds f = (nil, f)

    (* parseArgPat "_id ... _id" = [idOpt,...,idOpt] *)
    (* terminated by token different from underscore or id *)
    fun parseArgPat (LS.Cons ((L.ID (L.Upper, id), r), s')) =
        let 
	  val (idOpts, f') = parseArgPat (LS.expose s') 
	in 
	  (SOME id :: idOpts, f')
	end
      | parseArgPat (LS.Cons ((L.ID (_, id), r), s')) =
	Parsing.error (r, "Expected upper case identifier, found " ^ id)
      | parseArgPat (LS.Cons ((L.UNDERSCORE, r), s')) =
        let 
	  val (idOpts, f') = parseArgPat (LS.expose s') 
	in 
	  (NONE :: idOpts, f')
	end
      | parseArgPat f = (nil, f)

    (* parseCallPat "id _id ... _id" = (id, region, [idOpt,...,idOpt]) *)
    fun parseCallPat (LS.Cons ((L.ID (_, id), r), s')) =
        let 
	  val (idOpts, f' as LS.Cons ((_ ,r'), _)) = parseArgPat (LS.expose s')
	in
	  ((id, idOpts, P.join (r, r')), f')
	end
      | parseCallPat (LS.Cons ((t, r), s)) = 
	Parsing.error (r, "Expected call pattern, found token " ^ L.toString t)

    (* parseCallPats "(id _id ... _id)...(id _id ... _id)." *)
    fun parseCallPats (LS.Cons ((L.LPAREN, r), s')) = 
        let 
	  val (cpat, f') = parseCallPat (LS.expose s')
	  val (cpats, f'') = parseCallPats (stripRParen f')
	in
	  (cpat::cpats, f'')
	end
      (* Parens around call patterns no longer optional *)
      | parseCallPats (f as LS.Cons ((L.DOT, r), s')) =
          (nil, f)
      | parseCallPats (LS.Cons ((t, r), s)) = 
	Parsing.error (r, "Expected call patterns, found token " ^ L.toString t)

    (* order ::= id | (id ... id)   virtual arguments = subterm ordering
               | {order ... order}  lexicgraphic order
               | [order ... order]  simultaneous order
    *)
    (* parseOrderOpt (f) = (SOME(order), f') or (NONE, f) *)
    (* returns an optional order and front of remaining stream *)
    fun parseOrderOpt (LS.Cons ((L.LPAREN, r), s')) =
          decideRParen (r, parseIds (LS.expose s'))
      | parseOrderOpt (LS.Cons ((L.LBRACE, r), s')) =
	  decideRBrace (r, parseOrders (LS.expose s'))
      | parseOrderOpt (LS.Cons ((L.LBRACKET, r), s')) =
	  decideRBracket (r, parseOrders (LS.expose s'))
      | parseOrderOpt (LS.Cons ((L.ID (L.Upper, id), r), s')) =
	  (SOME (E.varg (r, [id])), LS.expose s')
      | parseOrderOpt (f as LS.Cons (_, s')) = (NONE, f)

    (* parseOrders (f) = ([order1,...,ordern], f') *)
    (* returns a sequence of orders and remaining front of stream *)
    and parseOrders (f) = parseOrders' (parseOrderOpt f)
    and parseOrders' (SOME(order), f') =
        let
	  val (orders, f'') = parseOrders f'
	in
	  (order::orders, f'')
	end
      | parseOrders' (NONE, f') = (nil, f')

    (* parseOrder (f) = (order, f') *)
    (* returns an order and front of remaining stream *)
    fun parseOrder (f) = parseOrder' (parseOrderOpt f)
    and parseOrder' (SOME(order), f') = (order, f')
      | parseOrder' (NONE, LS.Cons ((t, r), s')) =
          Parsing.error (r, "Expected order, found " ^ L.toString t)

    (* parseTDecl "order callPats." *)
    (* parses Termination Declaration, followed by `.' *)
    fun parseTDecl f = 
        let
	  val (order, f') = parseOrder f
	  val (callpats, f'') = parseCallPats f'
	in
	  (E.tdecl (order, E.callpats callpats), f'')
	end

    (* parseTerminates' "%terminates tdecl." *)
    fun parseTerminates' (LS.Cons ((L.TERMINATES, r), s')) = 
          parseTDecl (LS.expose s')

    (* ------------------- *)
    (* %prove declarations *)
    (* ------------------- *)

    (* parsePDecl "id nat order callpats." *)
    fun parsePDecl (LS.Cons ((L.ID (_, id), r), s')) =
        let
	  val depth = idToNat (r, id)
	  val (t', f') = parseTDecl (LS.expose s')
	in
	  (E.prove (depth, t'), f')
	end
      | parsePDecl (LS.Cons ((t, r), s')) =
	Parsing.error (r, "Expected theorem identifier, found " ^ L.toString t)

    (* parseProve' "%prove pdecl." *)
    fun parseProve' (LS.Cons ((L.PROVE, r), s')) = 
          parsePDecl (LS.expose s')


    (* ----------------------- *)
    (* %establish declarations *)
    (* ----------------------- *)

    (* parseEDecl "id nat order callpats." *)
    fun parseEDecl (LS.Cons ((L.ID (_, id), r), s')) =
        let
	  val depth = idToNat (r, id)
	  val (t', f') = parseTDecl (LS.expose s')
	in
	  (E.establish (depth, t'), f')
	end
      | parseEDecl (LS.Cons ((t, r), s')) =
	Parsing.error (r, "Expected theorem identifier, found " ^ L.toString t)

    (* parseEstablish' "%establish pdecl." *)
    fun parseEstablish' (LS.Cons ((L.ESTABLISH, r), s')) = 
          parseEDecl (LS.expose s')

    (* -------------------- *)
    (* %assert declarations *)
    (* -------------------- *)

    (* parseAssert' "%assert cp" *)
    fun parseAssert' (LS.Cons ((L.ASSERT, r), s')) = 
        let
	  val (callpats, f'') = parseCallPats (LS.expose s')
        in 
	  (E.assert (E.callpats callpats), f'')
	end

    (* --------------------- *)
    (* %theorem declarations *)
    (* --------------------- *)

    fun stripRBrace (LS.Cons ((L.RBRACE, r), s')) = LS.expose s'
      | stripRBrace (LS.Cons ((t, r), _))  = 
          Parsing.error (r, "Expected `}', found " ^ L.toString t)

    (* parseDec "{id:term} | {id}" *)
    and parseDec (f) =
        let 
	  val (D, f') = ParseTerm.parseDec' f
	  val f'' = stripRBrace f'
	in
	  (D, f'')
	end

    (* parseDecs' "{id:term}...{id:term}", zero or more, ":term" optional *)
    and parseDecs' (Ds, LS.Cons (BS as ((L.LBRACE, r), s'))) = 
        let
	  val (D, f') = parseDec (LS.expose s')
	in
	  parseDecs' (E.decl (Ds, D), f')
	end
      | parseDecs' Ds = Ds

    (* parseDecs "{id:term}...{id:term}", one ore more, ":term" optional *)
    and parseDecs (LS.Cons (BS as ((L.LBRACE, r), s'))) = 
        let
	  val (D, f') = parseDec (LS.expose s')
	in
	  parseDecs' (E.decl (E.null, D), f')
	end
      | parseDecs (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `{', found " ^ L.toString t)

    fun parsePi (LS.Cons ((L.ID (_, "pi"), r), s')) =
          parseDecs (LS.expose s')
      | parsePi (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `pi', found " ^ L.toString t)

    fun parseSome (gbs, LS.Cons ((L.ID (_, "some"), r), s')) =
        let
	  val (g1, f') = parseDecs (LS.expose s')
	  val (g2, f'') = parsePi f'
	in
	  parseSome' ((g1,g2)::gbs, f'')
	end
      | parseSome (gbs, f as LS.Cons ((L.ID (_, "pi"), r), s')) =
	let
	  val (g2, f') = parsePi f
	in
	  parseSome' ((E.null, g2)::gbs, f')
	end
      | parseSome (gbs, LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `some' or `pi', found " ^ L.toString t)
      
    and parseSome' (gbs, f as LS.Cons ((L.RPAREN, r), s')) = (gbs, f)
      | parseSome' (gbs, LS.Cons ((L.ID (_, "|"), r), s')) =
          parseSome (gbs, LS.expose s')
      | parseSome' (gbs, LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `)' or `|', found " ^ L.toString t)

    fun stripParen (gbs, LS.Cons ((L.RPAREN, r), s')) = (gbs, LS.expose s')

    fun parseGBs (LS.Cons ((L.LPAREN, r), s')) =
          stripParen (parseSome (nil, LS.expose s'))
      | parseGBs (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `(', found " ^ L.toString t)

    fun forallG ((gbs', f'), r) =
        let
	  val (t'', f'') = parseForallStar f'
	in
	  (E.forallG (gbs', (r, t'')), f'')
	end

    and forallStar ((g', f'), r) = 
        let 
	  val (t'', f'') = parseForall f'
	in 
	  (E.forallStar (g', (r, t'')),  f'')
	end

    and forall ((g', f'), r) =      
        let 
	  val (t'', f'') = parseExists f'
	in 
	  (E.forall (g', (r, t'')), f'')
	end

    and exists ((g', f'), r) = 
        let 
	  val (t'', f'') = parseTrue f'
	in 
	  (E.exists (g', (r, t'')), f'')
	end

    and top (f', r) = (E.top r, f')

    (* parseTrue "true" *)
    and parseTrue (LS.Cons ((L.ID (_, "true"), r), s')) =
	  top (LS.expose s', r)
      | parseTrue (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `true', found " ^ L.toString t)

    (* parseExists "exists decs mform | mform" *)
    and parseExists (LS.Cons ((L.ID (_, "exists"), r), s')) =
	  exists (parseDecs (LS.expose s'), r)
      | parseExists (LS.Cons ((L.ID (_, "true"), r), s')) =
	  top (LS.expose s', r)
      | parseExists (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `exists' or `true', found " ^ L.toString t)

    (* parseForall "forall decs mform | mform" *)
    and parseForall (LS.Cons ((L.ID (_, "forall"), r), s')) =
	  forall (parseDecs (LS.expose s'), r)
      | parseForall (LS.Cons ((L.ID (_, "exists"), r), s')) =
	  exists (parseDecs (LS.expose s'), r)
      | parseForall (LS.Cons ((L.ID (_, "true"), r), s')) =
	  top (LS.expose s', r)
      | parseForall (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `forall', `exists', or `true', found " ^ L.toString t)

    (* parseForallStar "forall* decs mform | mform" *)
    and parseForallStar (LS.Cons ((L.ID (_, "forall*"), r), s')) =
          forallStar (parseDecs (LS.expose s'), r)
      | parseForallStar (LS.Cons ((L.ID (_, "forall"), r), s')) =
	  forall (parseDecs (LS.expose s'), r)
      | parseForallStar (LS.Cons ((L.ID (_, "exists"), r), s')) =
	  exists (parseDecs (LS.expose s'), r)
      | parseForallStar (LS.Cons ((L.ID (_, "true"), r), s')) =
	  top (LS.expose s', r)
      | parseForallStar (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `forall*', `forall', `exists', or `true', found "
			     ^ L.toString t)

    and parseCtxScheme (LS.Cons ((L.ID (_, "forallG"), r), s')) =
           forallG (parseGBs (LS.expose s'), r)
      | parseCtxScheme (LS.Cons ((L.ID (_, "forall*"), r), s')) =
	   forallStar (parseDecs (LS.expose s'), r)
      | parseCtxScheme (LS.Cons ((L.ID (_, "forall"), r), s')) =
	  forall (parseDecs (LS.expose s'), r)
      | parseCtxScheme (LS.Cons ((L.ID (_, "exists"), r), s')) =
	  exists (parseDecs (LS.expose s'), r)
      | parseCtxScheme (LS.Cons ((L.ID (_, "true"), r), s')) =
	  top (LS.expose s', r)
      | parseCtxScheme (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected `forallG', `forall*', `forall', `exists', or `true', found "
			     ^ L.toString t)

    (* parseColon ": mform" *)
    fun parseColon (LS.Cons ((L.COLON, r), s')) =
          parseCtxScheme (LS.expose s')
      | parseColon (LS.Cons ((t, r), s')) =   
	  Parsing.error (r, "Expected `:', found " ^ L.toString t)

    (* parseThDec "id : mform" *)
    fun parseThDec (LS.Cons ((L.ID (_, id), r), s')) =
        let 
	  val (t', f') = parseColon (LS.expose s')
	in
	  (E.dec (id, t'), f')
	end
      | parseThDec (LS.Cons ((t, r), s')) =
	Parsing.error (r, "Expected theorem identifier, found " ^ L.toString t)

    (* parseTheoremDec' "%theorem thdec." *)
    (* We enforce the quantifier alternation restriction syntactically *)
    fun parseTheoremDec' (LS.Cons ((L.THEOREM, r), s')) = 
          parseThDec (LS.expose s') 

  in
    val parseTerminates' = parseTerminates'
    val parseTheorem' = parseForallStar
    val parseTheoremDec' = parseTheoremDec'
    val parseProve' = parseProve'
    val parseEstablish' = parseEstablish'
    val parseAssert' = parseAssert'
  end  (* local ... in *)

end;  (* functor Parser *)



