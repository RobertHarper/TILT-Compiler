(* Groups. *)

structure Group :> GROUP =
struct

    structure E = ExtSyn
    structure VM = Name.VarMap
    structure VS = Name.VarSet
    structure SM = Util.StringMap

    val error = fn s => Util.error "group.sml" s

    exception Error

    fun printError (what : string) (msg : string) : unit =
	let fun print s = TextIO.output(TextIO.stdErr,s)
	in  print what; print ": "; print msg; print "\n"
	end

    fun fail (what : string) (msg : string) : 'a =
	(printError what msg; raise Error)

    type id = E.id
    type filename = string

    structure Pos :>
    sig
	type pos = string * int option (* filename, line number *)
	val no_pos : pos
	val toString : pos -> string
	val printError : pos -> string -> unit
	val fail : pos -> string -> 'a
    end =
    struct
	type pos = filename * int option (* line number *)
	val no_pos : pos = ("no file", NONE)
	fun toString (p : pos) : string =
	    (case p
	       of (filename, NONE) => filename
		| (filename, SOME lineno) => (filename ^ ":" ^
					      Int.toString lineno))
	val printError : pos -> string -> unit = printError o toString
	val fail : pos -> string -> 'a = fn x => fail(toString x)
    end

    structure Pos' :>
    sig
	type pos' = unit -> Pos.pos
	val no_pos : pos'
	val toString : pos' -> string
	val printError : pos' -> string -> unit
	val fail : pos' -> string -> 'a
    end =
    struct
	type pos' = unit -> Pos.pos
	val no_pos : pos' = fn () => Pos.no_pos
	fun toPos (p : pos') : Pos.pos = p()
	val toString : pos' -> string = Pos.toString o toPos
	val printError : pos' -> string -> unit = Pos.printError o toPos
	val fail : pos' -> string -> 'a = fn x => Pos.fail(toPos x)
    end

    type fileinfo =
	{lineno : E.pos -> int,
	 filename : filename,
	 dir : filename}

    structure Fp :>
    sig
	type filepos = fileinfo * E.pos option
	val set_pos : filepos * E.pos -> filepos
	val toPos : filepos -> Pos.pos
	val toString : filepos -> string
	val printError : filepos -> string -> unit
	val fail : filepos -> string -> 'a
    end =
    struct
	type filepos = fileinfo * E.pos option
	fun set_pos ((fi,_) : filepos, pos : E.pos) : filepos =
	    (fi,SOME pos)
	fun toPos (fp : filepos) : Pos.pos =
	    let val (fi,posOpt) = fp
		val filename = #filename fi
		val lineno = Option.map (#lineno fi) posOpt
	    in	(filename,lineno)
	    end
	val toString : filepos -> string = Pos.toString o toPos
	val printError : filepos -> string -> unit = Pos.printError o toPos
	val fail : filepos -> string -> 'a = fn x => Pos.fail(toPos x)
    end

    type pos = Pos.pos
    val posString = Pos.toString
    type pos' = Pos'.pos'
    type filepos = Fp.filepos

    type var = Name.var
    type varset = Name.VarSet.set

    datatype iface =
	SRCI of id * filename * filename * var list
      | COMPI of id * filename * filename * varset

    datatype compunit =
	SRCU of id * filename * filename * var list * var option
      | COMPU of id * filename * filename * varset * var
      | PRIMU of id * filename * var list
      | IMPORTU of id * var
      | CHECKU of {U:var, I:var}

    datatype cmd =
	LINK of filename * filename * varset
      | PACK of
	{lib : filename,
	 imports : varset,	(* units that may only be imported *)
	 units : varset,	(* units to pack *)
	 ifaces : varset,	(* interfaces to pack *)
	 values : (id * ExtSyn.exp) list} (* each exp is a value *)

    datatype entry =
	IFACE of iface
      | UNIT of compunit
      | CMD of cmd

    structure Group :>
    sig
	type group
	val empty : group
	val add : group * var * pos' * entry -> group	(* var fresh *)
	val lookup : group * var -> (pos' * entry) option
	val get_entry : group * var -> entry
	val get_pos : group * var -> pos'
	val size : group -> int
	val toList : group -> (var * entry) list
    end =
    struct
	(*
	    Invariant: Vars (listed backwards according to order) is
	    well-formed; that is, no variable is used before it is
	    defined.

	    Invariant: order = dom(vars).
	*)
	type group = {vars : (pos' * entry) VM.map,
		      order : var list}

	val empty : group =
	    {vars = VM.empty,
	     order = nil}

	fun add (g : group, v : var, p : pos', e : entry) : group =
	    let val {vars, order} = g
		val vars = VM.insert (vars,v,(p,e))
		val order = v :: order
		val g = {vars=vars, order=order}
	    in	g
	    end

	fun lookup (g : group, v : var) : (pos' * entry) option =
	    VM.find (#vars g, v)

	fun get (g : group, v : var) : pos' * entry =
	    (case lookup(g,v)
	       of SOME r => r
		| NONE => error "group inconsistent")

	val get_entry : group * var -> entry = #2 o get
	val get_pos : group * var -> pos' = #1 o get

	fun size (g : group) : int = length (#order g)

	fun toList (g : group) : (var * entry) list =
	    map (fn v => (v, get_entry (g,v))) (rev (#order g))

    end

    type group = Group.group

    fun list_entries (g : group) : {entries : (var * entry) list,
				    pos : var -> pos} =
	{entries = Group.toList g,
	 pos = fn v => Group.get_pos (g,v) ()}

    val size : group -> int = Group.size

    val get_entry : group -> var -> entry =
	fn g => fn v => Group.get_entry (g,v)
 
    structure Print :
    sig
	val write : filename * E.groupfile -> unit
    end =
    struct
	type s = TextIO.outstream

	fun p (s:s) (s' : string) : unit = TextIO.output (s,s')
	fun p' (s:s) (ss : string list) : unit = app (p s) ss

	open E

	fun atomic (e:exp) : bool =
	    (case e
	       of EXP_VAR _ => true
		| EXP_ENV _ => true
		| EXP_STR _ => true
		| EXP_INT _ => true
		| EXP_BOOL _ => true
		| EXP_MARK (_,e) => atomic e
		| _ => false)

	fun pexp (s:s) (e:exp) : unit =
	    if atomic e then pexp' s e
	    else (p s "("; pexp' s e; p s ")")

	and pexp' (s:s) (e:exp) : unit =
	    let val p    = p s
		val p'   = p' s
		val pexp = pexp s
		fun pinfix ((e1,e2),s) = (pexp e1; p s; pexp e2)
	    in
		(case e
		   of EXP_VAR x => p'["$",x]
		    | EXP_ENV X => p'["env ",X]
		    | EXP_STR s =>
		       let fun quote c = if c = #"\034"
					     then "\034\034"
					 else str c
		       in  p'["\034",String.translate quote s,"\034"]
		       end
		    | EXP_CAT es => pinfix(es,"^")
		    | EXP_INT i => p (Int.toString i)
		    | EXP_BOOL b => p (Bool.toString b)
		    | EXP_IF (e1,e2,e3) => (p"if "; pexp e1; p" then ";
					    pexp e2; p" else "; pexp e3)
		    | EXP_NOT e => (p"not "; pexp e)
		    | EXP_AND es => pinfix(es," andalso ")
		    | EXP_OR es => pinfix(es," orelse ")
		    | EXP_SEQ es => pinfix(es," S= ")
		    | EXP_BEQ es => pinfix(es," B= ")
		    | EXP_IEQ es => pinfix(es,"=")
		    | EXP_ILT es => pinfix(es,"<")
		    | EXP_ILE es => pinfix(es,"<=")
		    | EXP_IGT es => pinfix(es,">")
		    | EXP_IGE es => pinfix(es,">=")
		    | EXP_DEFU U => p'["defined ",U]
		    | EXP_DEFI I => p'["defined interface ",I]
		    | EXP_DEFV x => p'["defined $",x]
		    | EXP_DEFE X => p'["defined env ",X]
		    | EXP_MARK (_,e) => pexp' s e)
	    end

	fun plist (s:s) (px : s -> 'a -> unit) (xs : 'a list) : unit =
	    let datatype 'a t = S | X of 'a
		fun pt S = p s " "
		  | pt (X x) = px s x
		val ts = Listops.join S (map X xs)
	    in  (p s " {"; app pt ts; p s "}")
	    end
	fun pimports (s:s) (imports:imports) : unit = plist s p imports
	fun pexport (s:s) (e:export) : unit =
	    (case e
	       of EXPORTI I => p' s ["interface ",I]
		| EXPORTV x => p' s ["val ",x])
	fun pexports (s:s) (exports:exports) : unit = plist s pexport exports

	fun pentries (s:s) : entries -> unit =
	    app (pentry s)

	and pentry (s:s) (e:entry) : unit =
	    (pentry' s e; p s "\n")

	and pentry' (s:s) (e:entry) : unit =
	    let val p = p s
		val p' = p' s
		val pexp = pexp s
		val pimports = pimports s
		val pexports = pexports s
	    in
		(case e
		   of SRCI (I,spec,imports) =>
		       (p'["source interface ",I," = "]; pexp spec;
			pimports imports)
		    | COMPI (I,iobj,ue) =>
		       (p'["compiled interface ",I," = "]; pexp iobj;
			p" and "; pexp ue)
		    | SRCU (U,NONE,dec,imports) =>
		       (p'["source unit ",U," = "]; pexp dec; pimports imports)
		    | SRCU (U,SOME I,dec,imports) =>
		       (p'["source unit ",U," : ",I," = "]; pexp dec;
			pimports imports)
		    | COMPU (U,I,obj,ue) =>
		       (p'["compiled unit ",U," : ",I," = "]; pexp obj;
			p" and "; pexp ue)
		    | PRIMU (U,imports) =>
		       (p'["primitive unit ",U]; pimports imports)
		    | IMPORTU (U,I) => p'["import unit ",U," : ",I]
		    | INCLUDE group => (p"include group "; pexp group)
		    | IMPORT group => (p"import group "; pexp group)
		    | VAL (x,e) => (p'["val ",x," = "]; pexp e)
		    | MAKE_EXE (exe,imports) =>
		       (p"make executable "; pexp exe; pimports imports)
		    | MAKE_LIB (lib,exports) =>
		       (p"make library "; pexp lib; pexports exports)
		    | IF (e1,ents1,ents2) =>
		       (p"#if "; pexp e1; p"\n"; pentries s ents1;
			p"#else\n"; pentries s ents2;
			p"#endif")
		    | ERROR e => (p"#error "; pexp e)
		    | MARK (_,e) => (pentry' s e))
	    end

	fun writer (group : groupfile) (file : filename) : unit =
	    let val s = TextIO.openOut file
		val _ = pentries s group
		val _ = TextIO.closeOut s
	    in  ()
	    end

	fun write (file : filename, group : groupfile) : unit =
	    File.write' (writer group) file
    end

    structure Parse :
    sig
	val parse : filename -> E.groupfile * fileinfo
    end =
    struct
	fun parserState (filename : string) : E.lexarg * fileinfo =
	    let val newlines = ref (nil : E.pos list)
		val start = ref (NONE : (string * E.pos) option)
		val com = ref 0
		val str = ref (nil : string list)
		val errors = ref false

		fun lineno (pos : E.pos) : int =
		    let
			fun find nil = 1
			  | find (p :: ps) = if pos >= p then (length ps) + 2
					     else find ps
		    in	find (!newlines)
		    end

		val fi = {filename = filename,
			  lineno = lineno,
			  dir = OS.Path.dir filename}

		val lexarg =
		    {errors = fn () => !errors,
		     newline = fn pos => newlines := (pos :: !newlines),
		     start = fn () => !start,
		     startCom = fn pos => start := SOME ("comment",pos),
		     nestCom = fn () => com := !com + 1,
		     endCom = fn () => if !com = 0 then (start := NONE; true)
				       else (com := !com - 1; false),
		     startStr = fn pos => start := SOME ("string",pos),
		     addToStr = fn s => str := s :: (!str),
		     finishStr = (fn right =>
				  let val (_,left) = valOf (!start)
				      val s = String.concat (rev (!str))
				  in  start := NONE;
				      str := nil;
				      (s,left,right)
				  end),
		     parseError = (fn (m,p,_) =>
				   (errors := true;
				    Fp.printError (fi,SOME p) m))}

	    in	(lexarg, fi)
	    end

	structure LrVals = GroupLrValsFun (structure Token = LrParser.Token)
	structure Lex = GroupLexFun (structure Tokens = LrVals.Tokens)
	structure P = JoinWithArg (structure ParserData = LrVals.ParserData
				   structure LrParser = LrParser
				   structure Lex = Lex)

	fun parse (filename : string) : E.groupfile * fileinfo =
	    let val ins = TextIO.openIn filename
		val read = fn n => TextIO.inputN (ins,n)
		val (lexarg,fi) = parserState filename
		val s = P.makeLexer read lexarg
		val (group,_) = P.parse (15,s,#parseError lexarg,())
		val _ = TextIO.closeIn ins
	    in	if #errors lexarg () then raise Error
		else (group,fi)
	    end
	val parse = Stats.subtimer ("Parsing groups",parse)
    end

    structure Ev :
    sig
	datatype ty = STRING | INT | BOOL

	val tyof : (E.id -> ty option) -> filepos -> E.exp -> ty

	type value
	val valexp : value -> E.exp	(* EXP_STR, EXP_INT, or EXP_BOOL *)
	val mks : string -> value
	val mki : int -> value
	val mkb : bool -> value
	val gets : value -> string
	val geti : value -> int
	val getb : value -> bool

	type evparm =
	    {lookup : E.id -> value option,
	     defunit : E.id -> bool,
	     defiface : E.id -> bool}

	val ev : evparm -> E.exp -> value
    end =
    struct

	datatype ty = STRING | INT | BOOL

	fun tyname (ty : ty) : string =
	    (case ty
	       of STRING => "string"
		| INT => "int"
		| BOOL => "bool")

	fun eqty (ty1 : ty, ty2 : ty) : bool =
	    (case (ty1, ty2)
	       of (STRING,STRING) => true
		| (INT,INT) => true
		| (BOOL,BOOL) => true
		| _ => false)

	fun tyof (lookup : E.id -> ty option) (fp : filepos) (e : E.exp) : ty =
	    let val fail = Fp.fail fp
		val self : E.exp -> ty = tyof lookup fp
		fun binaryop rty (e1,e2,ty) =
		    if eqty(self e1,ty) andalso eqty(self e2,ty)
			then rty
		    else fail ("expected " ^ tyname ty)
		val stringop = binaryop STRING
		val boolop = binaryop BOOL
	    in
		(case e
		   of E.EXP_VAR x =>
		       (case lookup x
			  of SOME ty => ty
			   | NONE => fail ("undefined variable: " ^ x))
		    | E.EXP_ENV X =>
		       if isSome (OS.Process.getEnv X)
			   then STRING
		       else fail ("undefined environment variable: " ^ X)
		    | E.EXP_STR _ => STRING
		    | E.EXP_CAT (e1,e2) => stringop (e1,e2,STRING)
		    | E.EXP_INT _ => INT
		    | E.EXP_BOOL _ => BOOL
		    | E.EXP_IF (e1,e2,e3) =>
		       (case (self e1, self e2, self e3)
			  of (BOOL, ty2, ty3) =>
			      if eqty(ty2,ty3) then ty2
			      else fail "arm types differ"
			   | _ => fail "expected bool")
		    | E.EXP_NOT e =>
		       (case self e
			  of BOOL => BOOL
			   | _ => fail "expected bool")
		    | E.EXP_AND (e1,e2) => boolop (e1,e2,BOOL)
		    | E.EXP_OR (e1,e2) => boolop (e1,e2,BOOL)
		    | E.EXP_SEQ (e1,e2) => boolop (e1,e2,STRING)
		    | E.EXP_BEQ (e1,e2) => boolop (e1,e2,BOOL)
		    | E.EXP_IEQ (e1,e2) => boolop (e1,e2,INT)
		    | E.EXP_ILT (e1,e2) => boolop (e1,e2,INT)
		    | E.EXP_ILE (e1,e2) => boolop (e1,e2,INT)
		    | E.EXP_IGT (e1,e2) => boolop (e1,e2,INT)
		    | E.EXP_IGE (e1,e2) => boolop (e1,e2,INT)
		    | E.EXP_DEFU _ => BOOL
		    | E.EXP_DEFI _ => BOOL
		    | E.EXP_DEFV _ => BOOL
		    | E.EXP_DEFE _ => BOOL
		    | E.EXP_MARK (pos,e) =>
			let val fp = Fp.set_pos (fp, pos)
			in  tyof lookup fp e
			end)
	    end

	(*
	    Invariant: Carried expression is one of EXP_STR, EXP_INT, or
	    EXP_BOOL.
	*)
	datatype value = VAL of E.exp

	(* If v1 : t and v2 : t, then eqvalue(v1,v2) is true when v1 = v2. *)
	fun eqvalue (VAL e1, VAL e2) : bool =
	    (case (e1,e2)
	       of (E.EXP_STR s1, E.EXP_STR s2) => s1 = s2
		| (E.EXP_INT i1, E.EXP_INT i2) => i1 = i2
		| (E.EXP_BOOL b1, E.EXP_BOOL b2) => b1 = b2
		| _ => error "eqvalue types differ bad value")

	val mks : string -> value = VAL o E.EXP_STR
	val mki : int -> value	  = VAL o E.EXP_INT
	val mkb : bool -> value	  = VAL o E.EXP_BOOL

	fun valexp (VAL e) : E.exp = e

	fun gets (v : value) : string =
	    (case v
	       of VAL (E.EXP_STR s) => s
		| _ => error "expected string value")

	fun geti (v : value) : int =
	    (case v
	       of VAL (E.EXP_INT i) => i
		| _ => error "expected int value")

	fun getb (v : value) : bool =
	    (case v
	       of VAL (E.EXP_BOOL b) => b
		| _ => error "expected bool value")

	(* Evaluation of well-typed expressions can not go wrong. *)
	type evparm =
	    {lookup : E.id -> value option,
	     defunit : E.id -> bool,
	     defiface : E.id -> bool}

	fun ev (p : evparm) (e : E.exp) : value =
	    let val self : E.exp -> value = ev p
		fun binaryop inj proj (e1,e2,f) =
		    (inj (f (proj (self e1), proj (self e2))))
		val stringop = binaryop mks gets
		val scmp = binaryop mkb gets
		val bcmp = binaryop mkb getb
		val icmp = binaryop mkb geti
	    in
		(case e
		   of E.EXP_VAR x => valOf(#lookup p x)
		    | E.EXP_ENV X => mks (valOf (OS.Process.getEnv X))
		    | E.EXP_STR _ => VAL e
		    | E.EXP_CAT (e1,e2) => stringop (e1,e2,op^)
		    | E.EXP_INT _ => VAL e
		    | E.EXP_BOOL _ => VAL e
		    | E.EXP_IF (e1,e2,e3) => self(if getb(self e1) then e2 else e3)
		    | E.EXP_NOT e => mkb (not (getb(self e)))
		    | E.EXP_AND (e1,e2) => mkb(getb(self e1) andalso getb(self e2))
		    | E.EXP_OR (e1,e2) => mkb(getb(self e1) orelse getb(self e2))
		    | E.EXP_SEQ (e1,e2) => scmp (e1,e2,op=)
		    | E.EXP_BEQ (e1,e2) => bcmp (e1,e2,op=)
		    | E.EXP_IEQ (e1,e2) => icmp (e1,e2,op=)
		    | E.EXP_ILT (e1,e2) => icmp (e1,e2,op<)
		    | E.EXP_ILE (e1,e2) => icmp (e1,e2,op<=)
		    | E.EXP_IGT (e1,e2) => icmp (e1,e2,op>)
		    | E.EXP_IGE (e1,e2) => icmp (e1,e2,op>=)
		    | E.EXP_DEFU U => mkb (#defunit p U)
		    | E.EXP_DEFI I => mkb (#defiface p I)
		    | E.EXP_DEFV x => mkb (isSome (#lookup p x))
		    | E.EXP_DEFE X => mkb (isSome (OS.Process.getEnv X))
		    | E.EXP_MARK (_,e) => self e)
	    end
    end

    type ty = Ev.ty
    type value = Ev.value

    structure Ctx :
    sig
	type ctx
	val empty : ctx
	val add_iface : ctx * E.id * pos' * var -> ctx
	val add_unit : ctx * E.id * pos' * var * bool -> ctx (* import? *)
	val add_val : ctx * E.id * pos' * ty * value -> ctx
	val lookup_iface : ctx * E.id -> var option
	val lookup_unit : ctx * E.id -> var option
	val lookup_ty : ctx * E.id -> ty option
	val lookup_val : ctx * E.id -> value option
	(*
		Imports is the set of units imported in the current
		group file or defined by imported group files.

		Exports is the set of units defined by the current or
		included group files.
	*)
	val imports : ctx -> varset
	val exports : ctx -> varset
	type pctx
	val purge : ctx -> pctx			(* remove appended entries *)
	val import : pctx -> pctx		(* mark all units import *)
	val append : ctx * pctx -> ctx		(* append entries *)
    end =
    struct
	datatype label =
	    LAB_VAL of E.id
	  | LAB_UNIT of E.id
	  | LAB_IFACE of E.id

	fun labelNum (l : label) : word =
	    (case l
	       of LAB_VAL _ => 0w0
		| LAB_UNIT _ => 0w1
		| LAB_IFACE _ => 0w2)

	fun labelCompare (l1 : label, l2 : label) : order =
	    (case Word.compare (labelNum l1, labelNum l2)
	       of EQUAL =>
		    (case (l1,l2)
		       of (LAB_VAL x1, LAB_VAL x2) => String.compare (x1,x2)
			| (LAB_UNIT U1, LAB_UNIT U2) => String.compare (U1,U2)
			| (LAB_IFACE I1, LAB_IFACE I2) => String.compare (I1,I2)
			| _ => error "labelNum/labelCompare got it wrong")
		| r => r)

	structure LabelKey =
	struct
	    type ord_key = label
	    val compare = labelCompare
	end

	structure LM = SplayMapFn (LabelKey)

	datatype class =
	    CLASS_IFACE of pos' * var
	  | CLASS_UNIT of pos' * var * bool	(* imported? *)
	  | CLASS_VAL of pos' * ty * value

	(*
		Invariant:
		If (l,(a,c)) in ctx, then
		1. l = LAB_VAL _ iff c = CLASS_VAL _.
		2. l = LAB_IFACE _ iff c = CLASS_IFACE _.
		3. l = LAB_UNIT _ iff c = CLASS_UNIT _.
		4. a is true iff c comes from append.
	*)
	type ctx = (bool * class) LM.map	(* imported or included? *)

	val empty = LM.empty

	fun lookup_iface' (ctx : ctx, I : E.id) : (pos' * var * bool) option =
	    (case LM.find (ctx, LAB_IFACE I)
	       of NONE => NONE
		| SOME (e,CLASS_IFACE (p,v)) => SOME (p,v,e)
		| _ => error "context iface")

	fun lookup_unit' (ctx : ctx,
			  U : E.id) : (pos' * var * bool * bool) option =
	    (case LM.find (ctx, LAB_UNIT U)
	       of NONE => NONE
		| SOME (e,CLASS_UNIT (p,v,i)) => SOME (p,v,i,e)
		| _ => error "context unit")

	fun lookup_val' (ctx : ctx, x : E.id) : (pos' * ty * value) option =
	    (case LM.find (ctx, LAB_VAL x)
	       of NONE => NONE
		| SOME (_,CLASS_VAL r) => SOME r
		| _ => error "context value")

	fun add_iface (ctx : ctx, I : E.id, p : pos', v : var) : ctx =
	    let fun add() = LM.insert (ctx,LAB_IFACE I,(false,CLASS_IFACE (p,v)))
	    in
		(case lookup_iface' (ctx, I)
		   of NONE => add()
		    | SOME (_,_,true) => add()
		    | SOME (p',_,_) =>
			Pos'.fail p (I ^ " already named at " ^
				     Pos'.toString p'))
	    end

	fun add_unit (ctx : ctx, U : E.id, p : pos', v : var, i : bool) : ctx =
	    let fun add() = LM.insert (ctx,LAB_UNIT U,(false,CLASS_UNIT (p,v,i)))
	    in
		(case lookup_unit' (ctx, U)
		   of NONE => add()
		    | SOME (_,_,_,true) => add()
		    | SOME (p',_,_,_) =>
			if i then add()
			else Pos'.fail p (U ^ " already named at " ^
					  Pos'.toString p'))
	    end

	fun add_val (ctx : ctx, x : E.id, p : pos', t : ty, v : value) : ctx =
	    LM.insert (ctx,LAB_VAL x,(false,CLASS_VAL (p,t,v)))

	fun lookup (look : ctx * id -> 'a option,
		    f : 'a -> 'b) : ctx * id -> 'b option =
	    (fn (ctx,id) =>
	     (case look (ctx,id)
		of SOME a => SOME (f a)
		 | NONE => NONE))

	val lookup_iface : ctx * id -> var option = lookup (lookup_iface', #2)
	val lookup_unit : ctx * id -> var option = lookup (lookup_unit', #2)
	val lookup_ty : ctx * id -> ty option = lookup (lookup_val', #2)
	val lookup_val : ctx * id -> value option = lookup (lookup_val', #3)

	fun units (p : bool -> bool) (ctx : ctx) : varset =
	    let fun folder ((_,c),s) =
		    (case c
		       of CLASS_UNIT (_,v,i) => if p i then VS.add (s,v) else s
			| _ => s)
	    in  LM.foldl folder VS.empty ctx
	    end

	val imports : ctx -> varset = units (fn i => i)
	val exports : ctx -> varset = units not

	type pctx = class LM.map

	fun purge (c : ctx) : pctx =
	    LM.mapPartial (fn (a,c) => if a then NONE else SOME c) c

	fun import' (c : class) : class =
	    (case c
	       of CLASS_UNIT (p,v,false) => CLASS_UNIT (p,v,true)
		| _ => c)

	fun import (p : pctx) : pctx = LM.map import' p

	val lift : pctx -> ctx = LM.map (fn c => (true,c))

	fun append (c : ctx, p : pctx) : ctx = LM.unionWith #2 (c, lift p)

    end

    type ctx = Ctx.ctx
    type pctx = Ctx.pctx

    structure Fctx :
    sig
	type fctx
	val empty : fctx
	val start_group : fctx * filename * pos' -> fctx
	val finish_group : fctx * filename * pctx -> fctx
	val add_source : fctx * filename * pos' -> fctx
	val add_target : fctx * filename * pos' -> fctx
	val lookup_group : fctx * filename * pos' -> pctx option
    end =
    struct
	datatype class =
	    GROUP
	  | GROUP' of pctx
	  | SOURCE
	  | TARGET

	type fctx = (pos' * class) SM.map

	val empty : fctx = SM.empty

	fun add (class : class) (c : fctx, f : filename, p : pos') : fctx =
	    (case SM.find (c,f)
	       of NONE => SM.insert (c,f,(p,class))
		| SOME (p',class') =>
		    (case (class, class')
		       of (SOURCE, SOURCE) => c
			| _ => Pos'.fail p (f ^ " already named at " ^
					    Pos'.toString p')))

	val start_group : fctx * filename * pos' -> fctx = add GROUP

	fun finish_group (c : fctx, f : filename, pctx : pctx) : fctx =
	    (case SM.find (c,f)
	       of SOME (p,GROUP) => SM.insert (c,f,(p,GROUP' pctx))
		| _ => error "finish_group")

	val add_source : fctx * filename * pos' -> fctx = add SOURCE
	val add_target : fctx * filename * pos' -> fctx = add TARGET

	fun lookup_group (c : fctx, f : filename, p : pos') : pctx option =
	    (case SM.find (c,f)
	       of SOME (_, GROUP' pctx) => SOME pctx
		| SOME (p', GROUP) =>
		    Pos'.fail p' ("group file cycle at " ^ Pos'.toString p)
		| SOME (p', _) =>
		    Pos'.fail p (f ^ " already named at " ^ Pos'.toString p')
		| NONE => NONE)
    end

    type fctx = Fctx.fctx

    structure Global :>
    sig
	type global
	val empty : global
	val add_unit : global * id * pos' * var -> global
	val add_val : global * id * pos' -> global
	val has_unit : global * id -> (pos' * var) option
    end =
    struct
	type global =
	    {units : (pos' * var) SM.map,
	     vals : pos' SM.map}

	val empty : global =
	    {units = SM.empty,
	     vals = SM.empty}

	fun add_unit (g : global, U : id, p : pos', v : var) : global =
	    let val {units,vals} = g
	    in  (case SM.find (units, U)
		   of NONE => {units=(SM.insert(units,U,(p,v))), vals=vals}
		    | SOME (p',_) =>
			Pos'.fail p (U ^ " already named at " ^
				     Pos'.toString p'))
	    end

	fun add_val (g : global, x : id, p : pos') : global =
	    let val {units,vals} = g
	    in  (case SM.find (vals, x)
		   of NONE => {units=units, vals=(SM.insert(vals,x,p))}
		    | SOME p' => Pos'.fail p (x ^ " already named at " ^
					      Pos'.toString p'))
	    end

	fun has_unit (g : global, U : id) : (pos' * var) option =
	    SM.find (#units g, U)
    end

    type global = Global.global

    structure Group' :>
    sig
	type group'
	val empty : group'

	val add_global_unit : group' * id * pos' * var -> group'
	val add_global_val : group' * id * pos' -> group'
	val has_global_unit : group' * id -> (pos' * var) option

	val add_entry : group' * var * pos' * entry -> group'	(* var fresh *)

	val add_string_value : group' * id * string -> group'
	val add_bool_value : group' * id * bool -> group'
	val add_int_value : group' * id * int -> group'

	val start_group : group' * filename * pos' -> group'
	val finish_group : group' * filename * pctx -> group'
	val add_source : group' * filename * pos' -> group'
	val add_target : group' * filename * pos' -> group'

	val get_group : group' -> group
	val get_initial : group' -> pctx
	val lookup_group : group'  * filename * pos' -> pctx option
   end =
    struct
	type group' =
	    {group : group,
	     initial : ctx * (unit -> pctx),
	     fctx : fctx,
	     global : global}

	fun init (ctx : ctx) : ctx * (unit -> pctx) =
	    (ctx, Util.memoize (fn () => Ctx.purge ctx))

	val empty : group' =
	    {group = Group.empty,
	     initial = init Ctx.empty,
	     fctx = Fctx.empty,
	     global = Global.empty}

	fun add_global_unit (g : group', U : id, p : pos', v : var) : group' =
	    let val {group,initial,fctx,global} = g
		val global = Global.add_unit (global, U, p, v)
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	fun add_global_val (g : group', x : id, p : pos') : group' =
	    let val {group,initial,fctx,global} = g
		val global = Global.add_val (global, x, p)
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	fun has_global_unit (g : group', U : id) : (pos' * var) option =
	    Global.has_unit (#global g, U)

	fun add_entry (g : group', v : var, p : pos', e : entry) : group' =
	    let val {group,initial,fctx,global} = g
		val group = Group.add (group, v, p, e)
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	fun add_value (ty : ty, inj : 'a -> value)
		      (g : group', x : id, a : 'a) : group' =
	    let val {group,initial,fctx,global} = g
		val global = Global.add_val (global, x, Pos'.no_pos)
		val (ctx,_) = initial
		val ctx = Ctx.add_val (ctx, x, Pos'.no_pos, ty, inj a)
		val initial = init ctx
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	val add_string_value = add_value (Ev.STRING,Ev.mks)
	val add_bool_value = add_value (Ev.BOOL, Ev.mkb)
	val add_int_value = add_value (Ev.INT, Ev.mki)

	fun start_group (g : group', f : filename, p : pos') : group' =
	    let val {group,initial,fctx,global} = g
		val fctx = Fctx.start_group (fctx, f, p)
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	fun finish_group (g : group', f : filename, c : pctx) : group' =
	    let val {group,initial,fctx,global} = g
		val fctx = Fctx.finish_group (fctx, f, c)
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	fun add_source (g : group', f : filename, p : pos') : group' =
	    let val {group,initial,fctx,global} = g
		val fctx = Fctx.add_source (fctx, f, p)
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	fun add_target (g : group', f : filename, p : pos') : group' =
	    let val {group,initial,fctx,global} = g
		val fctx = Fctx.add_target (fctx, f, p)
	    in  {group=group, initial=initial, fctx=fctx, global=global}
	    end

	val get_group : group' -> group = #group

	fun get_initial (g : group') : pctx =
	    let val (_,f) = #initial g
	    in  f()
	    end

	fun lookup_group (g : group', f : filename, p : pos') : pctx option =
	    Fctx.lookup_group (#fctx g,f,p)
    end

    type group' = Group'.group'

    structure Gf :>
    sig
	type group_file

	val empty : group' * fileinfo -> group_file
	val set_pos : group_file * E.pos -> group_file
	val get_pos : group_file -> pos'
	val dir : group_file -> filename
	val filename : group_file -> filename
	val printError : group_file -> string -> unit
	val fail : group_file -> string -> 'a

	val imports : group_file -> varset
	val exports : group_file -> varset
	val finish : group_file -> group' * pctx

	val add_unit : group_file * id * compunit -> group_file
	val add_import : group_file * id * var -> group_file
	val add_iface : group_file * id * iface -> group_file
	val add_cmd : group_file * cmd -> group_file
	val add_value : group_file * id * ty * value -> group_file

	val add_group : group_file * filename * bool *
	    (group' -> group' * pctx) -> group_file (* import? *)
	val add_source : group_file * filename -> group_file
	val add_target : group_file * filename -> group_file

	val lookup_iface : group_file * E.id -> var option
	val lookup_unit : group_file * E.id -> var option
	val lookup_ty : group_file * E.id -> ty option
	val lookup_val : group_file * E.id -> value option

	val tyof : group_file * E.exp -> ty
	val ev : group_file * E.exp -> value
    end =
    struct
	type group_file =
	    {pos : filepos * pos',
	     group : group',
	     ctx : ctx}

	fun empty (group : group', fi : fileinfo) : group_file =
	    let val fp = (fi,NONE)
		val pos' = Util.memoize (fn () => Fp.toPos fp)
		val pos = (fp,pos')
		val group = group
		val pctx = Group'.get_initial group
		val ctx = Ctx.append(Ctx.empty, pctx)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun set_pos (gf : group_file, p : E.pos) : group_file =
	    let val {pos,group,ctx} = gf
		val (fp,_) = pos
		val fp = Fp.set_pos (fp, p)
		val pos' = Util.memoize (fn () => Fp.toPos fp)
		val pos = (fp,pos')
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun get_pos (gf : group_file) : pos' = #2 (#pos gf)
	fun getFp (gf : group_file) : filepos = #1 (#pos gf)
	fun getFi (gf : group_file) : fileinfo = #1 (getFp gf)

	fun dir (gf : group_file) : filename = #dir (getFi gf)
	fun filename (gf : group_file) : filename = #filename (getFi gf)

	val printError : group_file -> string -> unit = Fp.printError o getFp
	val fail : group_file -> string -> 'a = fn x => Fp.fail(getFp x)

	val imports : group_file -> varset = Ctx.imports o (#ctx)
	val exports : group_file -> varset = Ctx.exports o (#ctx)

	fun finish (gf : group_file) : group' * pctx =
	    let val {ctx,group,...} = gf
		val pctx = Ctx.purge ctx
	    in  (group,pctx)
	    end

	fun add_unit (gf : group_file, U : id, c : compunit) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val v = Name.fresh_named_var U
		val group = Group'.add_global_unit (group,U,p,v)
		val group = Group'.add_entry (group,v,p,UNIT c)
		val ctx = Ctx.add_unit (ctx,U,p,v,false)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun add_import (gf : group_file, U : id, I : var) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val v = Name.fresh_named_var U
		val (group,c) =
		    (case Group'.has_global_unit (group,U)
		       of NONE => (Group'.add_global_unit (group,U,p,v),
				   IMPORTU (U,I))
			| SOME (_,v') => (group,CHECKU{U=v',I=I}))
		val group = Group'.add_entry (group,v,p,UNIT c)
		val ctx = Ctx.add_unit (ctx,U,p,v,true)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun add_iface (gf : group_file, I : id, i : iface) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val v = Name.fresh_named_var I
		val group = Group'.add_entry (group,v,p,IFACE i)
		val ctx = Ctx.add_iface (ctx,I,p,v)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun add_cmd (gf : group_file, c : cmd) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val v = Name.fresh_var()
		val group = Group'.add_entry (group,v,p,CMD c)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun add_value (gf : group_file, x : id, t : ty, v : value) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val group = Group'.add_global_val (group,x,p)
		val ctx = Ctx.add_val (ctx,x,p,t,v)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun add_group (gf : group_file, f : filename, import : bool,
		       xlate : group' -> group' * pctx) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val (group,pctx) =
		    (case Group'.lookup_group (group,f,p)
		       of SOME pctx => (group,pctx)
			| NONE =>
			    let val group = Group'.start_group(group,f,p)
				val (group,pctx) = xlate group
				val group = Group'.finish_group(group,f,pctx)
			    in  (group,pctx)
			    end)
		val pctx = if import then Ctx.import pctx else pctx
		val ctx = Ctx.append (ctx,pctx)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun add_source (gf : group_file, f : filename) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val group = Group'.add_source (group,f,p)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun add_target (gf : group_file, f : filename) : group_file =
	    let val {pos,group,ctx} = gf
		val (_,p) = pos
		val group = Group'.add_target (group,f,p)
	    in  {pos=pos, group=group, ctx=ctx}
	    end

	fun lookup (l : ctx * id -> 'a option) : group_file * id -> 'a option =
	    (fn (gf,id) => l (#ctx gf,id))

	val lookup_iface : group_file * id -> var option =
	    lookup Ctx.lookup_iface

	val lookup_unit : group_file * id -> var option =
	    lookup Ctx.lookup_unit

	val lookup_ty : group_file * id -> ty option =
	    lookup Ctx.lookup_ty

	val lookup_val : group_file * id -> value option =
	    lookup Ctx.lookup_val

	fun tyof (gf : group_file, e : E.exp) : ty =
	    let val ctx = #ctx gf
		val lookup = fn x => Ctx.lookup_ty(ctx,x)
		val (fp,_) = #pos gf
	    in  Ev.tyof lookup fp e
	    end

	fun ev (gf : group_file, e : E.exp) : value =
	    let val ctx = #ctx gf
		val lookup = fn x => Ctx.lookup_val(ctx,x)
		val defunit = fn U => isSome (Ctx.lookup_unit(ctx,U))
		val defiface = fn I => isSome (Ctx.lookup_unit(ctx,I))
		val evparm = {lookup=lookup, defunit=defunit, defiface=defiface}
	    in  Ev.ev evparm e
	    end
    end

    type group_file = Gf.group_file

    fun xid (what : string,
	     lookup : group_file * id -> 'a option) : group_file -> id -> 'a =
	(fn gf => fn id =>
	 (case lookup (gf,id)
	    of SOME a => a
	     | NONE => Gf.fail gf ("undefined " ^ what ^ ": " ^ id)))

    val xunit : group_file -> id -> var = xid ("unit",Gf.lookup_unit)

    val xiface : group_file -> id -> var = xid ("iface",Gf.lookup_iface)

    val xvar : group_file -> id -> E.exp =
	fn gf => fn id => Ev.valexp (xid ("variable",Gf.lookup_val) gf id)

    fun xfile (gf : group_file) (e : E.exp) : string =
	(case Gf.tyof (gf, e)
	   of Ev.STRING =>
	       let val path = Ev.gets(Gf.ev(gf, e))
		   val path =
			if OS.Path.isRelative path then
			    let val dir = Gf.dir gf
			    in  OS.Path.joinDirFile {dir=dir, file=path}
			    end
			else path
		   val path = OS.Path.mkCanonical path
	       in  path
	       end
	    | _ => Gf.fail gf "expected filename")

    val ximports : group_file -> E.id list -> var list =
	fn gf => map (xunit gf)

    fun xsrci (gf : group_file)
	      (I : id, spec : E.exp, imports : E.imports) : group_file =
	let val n = Gf.filename gf
	    val spec = xfile gf spec
	    val imports = ximports gf imports
	    val gf = Gf.add_source (gf, spec)
	in  Gf.add_iface (gf, I, SRCI(I,n,spec,imports))
	end

    fun xue (gf : group_file) (uefile : filename) : varset =
	let val ue = (Compiler.FileCache.read_ue uefile
		      handle e =>
			(Gf.printError gf ("bad unit environment: " ^ uefile);
			 raise e))
	    val ids = map #1 (UnitEnvironment.listItemsi ue)
	    fun xparm (U : E.id) : var =
		(case Gf.lookup_unit (gf,U)
		   of SOME v => v
		    | NONE =>
			Gf.fail gf ("unit environment file refers to \
				    \undefined unit: " ^ U))
	    val parms = map xparm ids
	in  VS.addList (VS.empty, parms)
	end

    fun xcompi (gf : group_file)
	       (I : id, iobj : E.exp, ue : E.exp) : group_file =
	let val iobj = xfile gf iobj
	    val ue = xfile gf ue
	    val parms = xue gf ue
	    val gf = Gf.add_source (gf, iobj)
	    val gf = Gf.add_source (gf, ue)
	in  Gf.add_iface (gf, I, COMPI(I,iobj,ue,parms))
	end

    fun xsrcu (gf : group_file)
	      (U : id, I : E.id option, dec : E.exp,
	       imports : E.imports) : group_file =
	let val n = Gf.filename gf
	    val I = Option.map (xiface gf) I
	    val dec = xfile gf dec
	    val imports = ximports gf imports
	    val gf = Gf.add_source (gf, dec)
	in  Gf.add_unit (gf, U, SRCU(U,n,dec,imports,I))
	end

    fun xcompu (gf : group_file)
	       (U : id, I : E.id, obj : E.exp, ue : E.exp) : group_file =
	let val I = xiface gf I
	    val obj = xfile gf obj
	    val ue = xfile gf ue
	    val parms = xue gf ue
	    val gf = Gf.add_source (gf, obj)
	    val gf = Gf.add_source (gf, ue)
	in  Gf.add_unit (gf, U, COMPU(U,obj,ue,parms,I))
	end

    fun xprimu (gf : group_file) (U : id, imports : E.imports) : group_file =
	let val n = Gf.filename gf
	    val imports = ximports gf imports
	in  Gf.add_unit (gf, U, PRIMU(U,n,imports))
	end

    fun ximportu (gf : group_file) (U : id, I : E.id) : group_file =
	Gf.add_import (gf, U, xiface gf I)

    fun xval (gf : group_file) (x : id, e : E.exp) : group_file =
	Gf.add_value (gf, x, Gf.tyof (gf,e), Gf.ev (gf,e))

    fun xmake_exe (gf : group_file)
		  (exe : E.exp, targets : id list) : group_file =
	let val n = Gf.filename gf
	    val exe = xfile gf exe
	    val targets = ximports gf targets
	    val targets = VS.addList (VS.empty, targets)
	    val gf = Gf.add_target (gf, exe)
	in  Gf.add_cmd (gf, LINK (n, exe,targets))
	end

    fun xexports (gf : group_file) (exports : E.exports)
	    : varset * (id * ExtSyn.exp) list =
	let
	    fun folder (e : E.export, (ifaces,values)) =
		(case e
		   of E.EXPORTI I => (VS.add(ifaces, xiface gf I), values)
		    | E.EXPORTV x => (ifaces, (x, xvar gf x) :: values))
	in  foldr folder (VS.empty,nil) exports
	end

    fun xmake_lib (gf : group_file)
		  (lib : E.exp, exports : E.exports) : group_file =
	let val lib = xfile gf lib
	    val (ifaces,values) = xexports gf exports
	    val imports = Gf.imports gf
	    val units' = Gf.exports gf
	    val units = VS.difference(units',imports)
	    val cmd = PACK {lib=lib, imports=imports, units=units,
			    ifaces=ifaces, values=values}
	    val gf = Gf.add_target (gf, lib)
	in  Gf.add_cmd (gf, cmd)
	end

    fun xif (gf : group_file, check : group_file -> E.entries -> unit,
	     xlate : group_file -> E.entries -> group_file)
	    (e : E.exp, e1 : E.entries, e2 : E.entries) : group_file =
	let val e = (case Gf.tyof(gf,e)
		       of Ev.BOOL => Ev.getb(Gf.ev(gf,e))
			| _ => Gf.fail gf "bool expected")
	    val (used,unused) = if e then (e1,e2) else (e2,e1)
	    val gf' = xlate gf used
	    val _ = check gf unused
	in  gf'
	end

    fun xerror (gf : group_file, checking : bool) (e : E.exp) : unit =
	(case Gf.tyof(gf,e)
	   of Ev.STRING =>
		if checking then ()
		else Gf.fail gf (Ev.gets (Gf.ev(gf,e)))
	    | _ => Gf.fail gf "expected string")

    fun xentry (xsubgroup : group_file * bool -> E.exp -> group_file,
		checking : bool)
	       (ent : E.entry, gf : group_file) : group_file =
	(case ent
	   of E.SRCI arg => xsrci gf arg
	    | E.COMPI arg => xcompi gf arg
	    | E.SRCU arg => xsrcu gf arg
	    | E.COMPU arg => xcompu gf arg
	    | E.PRIMU arg => xprimu gf arg
	    | E.IMPORTU arg => ximportu gf arg
	    | E.INCLUDE arg => xsubgroup (gf, false) arg
	    | E.IMPORT arg => xsubgroup (gf, true) arg
	    | E.VAL arg => xval gf arg
	    | E.MAKE_EXE arg => xmake_exe gf arg
	    | E.MAKE_LIB arg => xmake_lib gf arg
	    | E.IF arg =>
		let val xlate = foldl (xentry (xsubgroup,checking))
		    fun check g e =
			ignore (foldl (xentry (xsubgroup,true)) g e)
		in  xif (gf,check,xlate) arg
		end
	    | E.ERROR arg => (xerror (gf,checking) arg; gf)
	    | E.MARK (pos,ent) =>
		xentry (xsubgroup,checking) (ent, Gf.set_pos (gf,pos)))

    fun xlate (g : group', name : filename, p : pos',
	       fixup : ExtSyn.groupfile -> ExtSyn.groupfile)
	    : group' * pctx =
	let
	    fun xsubgroup (gf : group_file, import : bool)
			  (subgroup : E.exp) : group_file =
		let val name = xfile gf subgroup
		    val xlate = fn g => xlate (g, name, Gf.get_pos gf, fixup)
		in  Gf.add_group (gf, name, import, xlate)
		end
	    val (entries, fi) = (Parse.parse name handle (e as IO.Io _) =>
				 Pos'.fail p (exnMessage e))
	    val entries = fixup entries
	    val gf = Gf.empty (g, fi)
	    val gf = foldl (xentry (xsubgroup, false)) gf entries
	in  Gf.finish gf
	end

    val empty_group' = Group'.empty
    val get_group = Group'.get_group
    val add_string_value = Group'.add_string_value
    val add_bool_value = Group'.add_bool_value
    val add_int_value = Group'.add_int_value

    fun read (g : group', f : filename,
	      fixup : E.groupfile -> E.groupfile) : group' =
	let val g = Group'.start_group(g,f,Pos'.no_pos)
	    val pos' = fn () => (f,NONE)
	    val (g,pctx) = xlate (g,f,pos',fixup)
	    val g = Group'.finish_group(g,f,pctx)
	in  g
	end

    val read = Stats.timer ("Reading group files",read)

    val write : filename * E.groupfile -> unit = Print.write
end
