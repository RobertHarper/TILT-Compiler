(* Parsing Mode Declarations *)
(* Author: Carsten Schuermann *)

functor ParseMode
  (structure Paths : PATHS
   structure Parsing' : PARSING
     sharing Parsing'.Lexer.Paths = Paths
   structure ExtModes' : EXTMODES
     sharing ExtModes'.Paths = Paths
   structure ParseTerm : PARSE_TERM
     sharing ParseTerm.Parsing.Lexer = Parsing'.Lexer
     sharing ParseTerm.ExtSyn = ExtModes'.ExtSyn)
     : PARSE_MODE =
struct
  structure Parsing = Parsing'
  structure ExtModes = ExtModes'

  local
    structure L = Parsing.Lexer
    structure LS = Parsing.Lexer.Stream  
    structure E = ExtModes
    structure P = Paths

    (* extract (s, i) = substring of s starting at index i
       Effect: raises Subscript if i > |s|
    *)
    fun extract (s, i) = 
        if i = String.size s
	  then NONE
	else SOME (String.extract (s, i, NONE))

    (* splitModeId (r, id) = (mode, idOpt) where id = "<mode><idOpt>"
       Invariant: id <> ""
    *)
    fun splitModeId (r, id) =
        case String.sub (id, 0)
	  of #"*" => (E.star r, extract (id, 1))
	   | #"-" => (E.minus r, extract (id, 1))
	   | #"+" => (E.plus r,  extract (id, 1))
	   | _ => Parsing.error (r, "Expected mode `+', `-', or `*', found " ^ id)

    fun validateMArg (r, mId as (mode, SOME (id))) =
        if L.isUpper id
	  then mId
	else Parsing.error (r, "Expected free uppercase variable, found " ^ id)
      | validateMArg (r, (_, NONE)) =
	  Parsing.error (r, "Missing variable following mode")

    fun validateMode (r, (mode, NONE)) = mode
      | validateMode (r, (_, SOME(id))) =
           Parsing.error (r, "Expected simple mode, found mode followed by identifier " ^ id) 

    (* parseShortSpine "modeid ... modeid." *)
    fun parseShortSpine (f as LS.Cons ((L.DOT, r), s')) =
	  (E.Short.mnil r, f)
      | parseShortSpine (LS.Cons ((L.ID (_, id), r), s')) =
	let 
	  val mId = validateMArg (r, splitModeId (r, id))
	  val (mS', f') = parseShortSpine (LS.expose s')
	in 
	  (E.Short.mapp (mId, mS'), f')
	end
      | parseShortSpine (LS.Cons ((t, r), s')) =
	  Parsing.error (r, "Expected mode or `.', found " ^ L.toString t)

    fun stripRBrace (LS.Cons ((L.RBRACE, r), s')) = LS.expose s'
      | stripRBrace (LS.Cons ((t, r), _))  = 
          Parsing.error (r, "Expected `}', found " ^ L.toString t)

    (* parseFull "mode {id:term} ... mode {x:term} term" *)
    fun parseFull (LS.Cons (t0 as (L.ID (c, id), r0), s'), r1) =
        (* Look ahead one token to decide if quantifier follows *)
	(case LS.expose s'
	   of LS.Cons ((L.LBRACE, r), s'') =>
	      (* found quantifier --- id must be mode *)
	      let 
		val mId = splitModeId (r0, id)
		val m = validateMode (r0, mId)
		val (d', f') = ParseTerm.parseDec' (LS.expose s'')
		val f'' = stripRBrace f'
		val (t', f''') = parseFull (f'', r1) 
	      in 
		(E.Full.mpi (m, d', t'), f''')
	      end
	    | LS.Cons TS => 
	      (* no quantifier --- parse atomic type *)
	      let 
		val (t', f' as LS.Cons ((_, r), s')) = 
		    ParseTerm.parseTerm' (LS.Cons (t0, LS.cons TS))
	      in
		(E.Full.mroot (t', P.join (r, r1)), f')
	      end)
      | parseFull (LS.Cons ((t, r), s'), _) =
          Parsing.error (r, "Expected mode or identifier, found " ^ L.toString t)

    (* parseMode2 switches between full and short mode declarations *)
    (* lexid could be mode or other identifier *)
    fun parseMode2 (lexid, LS.Cons (BS as ((L.LBRACE, r), s')), r1) = 
        let 
	  val (t', f') = parseFull (LS.Cons (lexid, LS.cons BS), r1)
	in
	  (E.Full.toModedec t', f')
	end
      | parseMode2 ((L.ID (_, name), r), f, _) = 
	let 
	  val (mS', f') = parseShortSpine f
	in
	  (E.Short.toModedec (E.Short.mroot (name, r, mS')), f')
	end

    (* parseMode1 parses mdecl *)
    fun parseMode1 (LS.Cons (lexid as (L.ID _, r), s')) = parseMode2 (lexid, LS.expose s', r)
      | parseMode1 (LS.Cons ((t, r), _)) =
          Parsing.error (r, "Expected identifier or mode, found " ^ L.toString t)

    (* parseMode' : lexResult front -> modedec * lexResult front
       Invariant: exposed input stream starts with MODE
    *)
    fun parseMode' (LS.Cons ((L.MODE, r), s')) = parseMode1 (LS.expose s')
  in
    val parseMode' = parseMode'
  end  (* local *)

end;  (* functor ParseMode *)
