(* Project description files. *)
(*
	Accrete well-formed project descriptions from a collection of
	mutually inter-dependent project description files.  Performs
	the checks described in ../Doc/tm and extsyn.sml; see those
	files for more information.
*)
(*
	XXX: If there is an error, finish processing the current file to generate
	more errors and then stop.
*)
structure Project :> PROJECT =
struct

    structure I = IntSyn
    structure E = ExtSyn
    structure Map = Name.LabelMap
    structure Set = Name.LabelSet
    structure StringMap = Util.StringMap

    val error = fn s => Util.error "project.sml" s
    val reject = Util.reject

    val Bootstrap = Stats.ff "Bootstrap"

    type file = string
    type pos = Pos.pos
    type label = Name.label

    val Linking : string = "linking"

    val label_name : label -> string = Name.label2longname

    (*
	We use error() for bugs.  We use reject() for user errors.
	Note the parser prints a series of error messages before
	calling reject() and most of the code here calls reject()
	via fail().
    *)

    fun print_error (pos:pos) (msg:string) : unit =
	let fun print s = TextIO.output(TextIO.stdErr,s)
	in  print (concat[Pos.tostring pos,": ",msg,"\n"])
	end

    fun raise_error () : 'a = reject "project description error"

    fun fail (pos:pos) (msg:string) : 'a =
	(print_error pos msg;
	 raise_error())

    datatype ty = STRING | INT | BOOL

    datatype value =
	SVAL of string
      | IVAL of int
      | BVAL of bool

    datatype entry =
	PDEC of I.pdec
      | VDEC of ty * value * pos

    fun entry_pdec (e:entry) : I.pdec =
	(case e
	   of PDEC pdec => pdec
	    | _ => error "entry_pdec")

    fun entry_pos (e:entry) : pos =
	(case e
	   of PDEC pdec => I.P.pos pdec
	    | VDEC (_,_,pos) => pos)

    datatype file_entry =
	ELABORATING of pos		(* starting point *)
      | ELABORATED of Set.set	(* identifers defined in file *)

    (*
	(rev desc_) is a well-formed project description.  Entries
	contains desc (to speed up lookup) and any variables that have
	been defined.  Files records the project description files
	that constitute desc.  We compares filenames (in canonical
	form) to manage dependencies between project description
	files.  We assume that distinct filenames map to distinct
	files; in particular, we can be confused by symbolic links or
	similar mechanisms.  Vars names the variables that are defined
	by empty() and made visible to every project description file.

	Other fields relate to a specific project description as it is
	being elaborated.  Pos is for error messages.  Visible records
	the identifiers that may appear free in the file.  Ck is true
	if #error directives should be ignored.

	We write DESC(d) for (rev (#desc_ d)).

	We write VDESC(d) for the project description obtained from
	DESC(d) by discarding those units and interfaces that are not
	in (#visible d).

	Invariants:
	If (d as {desc_,entries,files,pos,visible}:desc), then

	|- DESC(d) ok

	|- VDESC(d) ok

	If (L,pdec) in desc_,
	then (L,PDEC pdec) in entries.

	L in dom(entries)\dom(desc_) iff entries(L) = VDEC _.

	files(file) = ELABORATING iff file is being elaborated.

	files(file) = ELABORATED names iff file has been elaborated
	into entries and desc_ and names subset dom(entries) are the
	definitions in file.
    *)
    type desc =
	{desc_:I.desc,
	 entries:entry Map.map,
	 files:file_entry StringMap.map,
	 vars:Set.set,
	 pos:Pos.pos,
	 visible:Set.set,
	 ck:bool}

    fun predefined_vars {linking:bool} : entry Map.map =
	let fun add_var (ty:ty,inj:'a -> value)
			(entries:entry Map.map,
			 name:string,a:'a) : entry Map.map =
		let val x = Name.symbol_label(Symbol.varSymbol name)
		    val value = inj a
		    val entry = VDEC(ty,value,Pos.nopos)
		in  Map.insert(entries,x,entry)
		end
	    val bool = add_var (BOOL,BVAL)
	    val int = add_var (INT,IVAL)
	    val string = add_var (STRING,SVAL)

	    (* Eg: cputype=unsupported, objtype=sparc, target=sparc-8 *)
	    val cputype = Platform.cputypeToString (Platform.cputype ())
	    val objtype = Platform.toString (Target.getTarget ())
	    val target = Target.targetString()
	    val littleEndian = Platform.littleEndian (Target.getTarget ())
	    val libdir = I.F.libdir()
	    val e = Map.empty
	    val e = bool (e, Linking, linking)
	    val e = bool (e, "bootstrapping", !Bootstrap)
	    val e = string (e, "cputype", cputype)
	    val e = string (e, "objtype", objtype)
	    val e = string (e, "target", target)
	    val e = bool (e, "littleEndian", littleEndian)
	    val e = int (e, "majorVersion", Version.majorVersion)
	    val e = int (e, "minorVersion", Version.minorVersion)
	    val e = string (e, "version", Version.version)
	    val e = string (e, "libdir", libdir)
	in  e
	end

    fun empty (args:{linking:bool}) : desc =
	let val entries = predefined_vars args
	    val vars =
		Map.foldli (fn (x,_,vs) => Set.add(vs,x)) Set.empty entries
	in
	    {desc_=nil, entries=entries, files=StringMap.empty, vars=vars,
	     pos=Pos.nopos, visible=Set.empty, ck=false}
	end

    (*
	Add an entry to desc.
    *)
    fun insert (desc:desc, l:label, e:entry) : desc =
	let val {desc_,entries,files,vars,pos,visible,ck} = desc
	    val desc_ =
		(case e
		   of PDEC pdec => pdec::desc_
		    | VDEC _ => desc_)
	    val entries = Map.insert (entries,l,e)
	    val visible = Set.add(visible,l)
	in  {desc_=desc_,entries=entries,files=files,vars=vars,
	     pos=pos,visible=visible,ck=ck}
	end

    (*
	Reveal any labels that are not already visible.
    *)
    fun reveal (desc:desc, labels:label list) : desc =
	let val {desc_,entries,files,vars,pos,visible,ck} = desc
	    fun add (l:label) : bool = not(Set.member(visible,l))
	    val add = List.filter add labels
	in  if null add then desc
	    else
		let val visible = Set.addList(visible,add)
		in  {desc_=desc_,entries=entries,files=files,vars=vars,
		     pos=pos,visible=visible,ck=ck}
		end
	end

    fun update_pos (desc:desc, pos:pos) : desc =
	let val {desc_,entries,files,vars,visible,ck,...} = desc
	in  {desc_=desc_,entries=entries,files=files,vars=vars,
	     pos=pos,visible=visible,ck=ck}
	end

    fun update_checking (desc:desc, ck:bool) : desc =
	let val {desc_,entries,files,vars,pos,visible,...} = desc
	in  {desc_=desc_,entries=entries,files=files,vars=vars,
	     pos=pos,visible=visible,ck=ck}
	end

    fun start_file (desc:desc, file:file) : desc =
	let val {desc_,entries,files,vars,pos,ck,...} = desc
	    val files = StringMap.insert(files,file,ELABORATING pos)
	    val pos = Pos.pos' file
	    val visible = vars
	in  {desc_=desc_,entries=entries,files=files,vars=vars,
	     pos=pos,visible=visible,ck=ck}
	end

    fun finish_file (desc:desc, file:file, desc':desc) : desc =
	let val {pos,visible,ck,...} = desc
	    val {desc_,entries,files,vars,visible=visible',...} = desc'
	    val new = Set.difference(visible',vars)
	    val files = StringMap.insert(files,file,ELABORATED new)
	    val visible = Set.union (visible,new)
	in  {desc_=desc_,entries=entries,files=files,vars=vars,
	     pos=pos,visible=visible,ck=ck}
	end

    (*
	Lookup functions.
    *)

    fun global_lookup (desc:desc, l:label) : entry option =
	Map.find (#entries desc,l)

    fun lookup (desc:desc, l:label) : entry option =
	if Set.member (#visible desc,l) then
	    global_lookup (desc,l)
	else
	    NONE

    fun lookup_file (desc:desc, file:file) : file_entry option =
	StringMap.find (#files desc,file)

    fun undefined (l:label) : string =
	label_name l ^ " not defined"

    fun known_value (desc:desc, x:label) : ty * value * pos =
	(case global_lookup (desc,x)
	   of SOME (VDEC r) => r
	    | _ => error (undefined x))

    fun unknown_value (desc:desc, x:label) : ty * value * pos =
	(case lookup (desc,x)
	   of SOME (VDEC r) => r
	    | _ => fail (#pos desc) (undefined x))

    fun known_env (X:label) : string =
	(case OS.Process.getEnv (Name.label2name X)
	   of SOME s => s
	    | NONE => error (undefined X))

    fun check_env (desc:desc, X:label) : unit =
	(case OS.Process.getEnv (Name.label2name X)
	   of SOME _ => ()
	    | NOEN => fail (#pos desc) (undefined X))

    fun known_interface (desc:desc, I:label) : label * I.iexp * pos =
	(case global_lookup (desc,I)
	   of SOME (PDEC (I.IDEC r)) => r
	    | _ => error (undefined I))

    fun unknown_interface (desc:desc, I:label) : label * I.iexp * pos =
	(case lookup (desc,I)
	   of SOME (PDEC (I.IDEC r)) => r
	    | _ => fail (#pos desc) (undefined I))

    fun check_bound (desc:desc, l:label) : unit =
	if Set.member(#visible desc, l) then ()
	else fail (#pos desc) (undefined l)

    fun check_opened (desc:desc, opened:E.opened) : unit =
	app (fn U => check_bound (desc,U)) opened

    fun check_unbound (desc:desc, l:label) : unit =
	(case global_lookup(desc,l)
	   of NONE => ()
	    | SOME e =>
		fail (#pos desc)
		(concat[label_name l," already defined at ",
			Pos.tostring (entry_pos e)]))

    fun check_using (desc:desc, using:I.using) : unit =
	let val {visible,pos,...} = desc
	    fun check (U:label) : unit =
		if Set.member(visible,U) then ()
		else fail pos ("compiled file uses undefined " ^ label_name U)
	in  app check using
	end

    (*
	Print external syntax.
    *)
    fun print_ents (s:TextIO.outstream, ents:E.ents) : unit =
	let open E

	    fun p (s' : string) : unit = TextIO.output (s,s')
	    fun p' (ss : string list) : unit = app p ss

	    fun pid (l:label) : unit = p (Name.label2name' l)

	    fun pid' (l:label) : unit =
		if Name.is_unit l then pid l
		else if Name.is_interface l then (p"interface "; pid l)
		else if Name.is_env l then (p"env "; pid l)
		else (p"$"; pid l)

	    fun atomic (e:exp) : bool =
		(case e
		   of EXP_VAR _ => true
		    | EXP_ENV _ => true
		    | EXP_STR _ => true
		    | EXP_INT _ => true
		    | EXP_BOOL _ => true
		    | EXP_MARK (_,e) => atomic e
		    | _ => false)

	    fun quote (c:char) : string =
		(case c
		   of #"\034" => "\034\034"
		    | _ => str c)

	    fun pstring (s':string) : unit =
		p'["\034",String.translate quote s',"\034"]

	    fun pinfix ((e1:exp,e2:exp),s':string) : unit =
		(pexp e1; p s'; pexp e2)

	    and pexp (e:exp) : unit =
		if atomic e then pexp' e
		else (p"("; pexp' e; p")")

	    and pexp' (e:exp) : unit =
		(case e
		   of EXP_VAR x => (p"$"; pid x)
		    | EXP_ENV X => (p"env "; pid X)
		    | EXP_STR s => pstring s
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
		    | EXP_DEF l => (p"defined "; pid' l)
		    | EXP_MARK (_,e) => pexp' e)

	    fun popened (opened:opened) : unit =
		let val names = map Name.label2name' opened
		in  p' [" {",Listops.concatWith " " names,"}"]
		end

	    fun popopt (opopt:opopt) : unit =
		(case opopt
		   of NONE => ()
		    | SOME opened => popened opened)

	    fun pasc (I:label) : unit =
		(p" : "; pid I)

	    fun pascopt (I:label option) : unit =
		(case I
		   of NONE => ()
		    | SOME I => pasc I)

	    fun pie (ie:ie) : unit =
		(case ie
		   of SRCI (src,opened) => (pexp src; popopt opened)
		    | PRIMI => p"primitive"
		    | PRECOMPI (src,opened) =>
			(p"compiled "; pexp src; popened opened)
		    | COMPI => p"compiled")

	    fun pents (ents:ents) : unit = app pent ents

	    and pent (e:ent) : unit =
		(pent' e; p"\n")

	    and pent' (e:ent) : unit =
		(case e
		   of INTERFACE(I,ie) => (p"interface "; pid I; p" = "; pie ie)
		    | UNIT(U,SRCU (src,opopt,ascopt)) =>
			(p"unit "; pid U; pascopt ascopt; p" = "; pexp src;
			 popopt opopt)
		    | UNIT(U,PRIMU asc) =>
			(p"unit "; pid U; pasc asc; p" = primitive")
		    | UNIT(U,PRECOMPU (src,opened,asc)) =>
			(p"unit "; pid U; pasc asc; p" = compiled "; pexp src;
			 popened opened)
		    | UNIT(U,COMPU asc) =>
			(p"unit "; pid U; pasc asc; p" = compiled")
		    | SC (U,I) => (p"unit "; pid U; pasc I)
		    | VAL (x,e) => (p"val "; pid x; p" = "; pexp e)
		    | INCLUDE file => (p"include "; pexp file)
		    | LOCAL file => (p"local "; pexp file)
		    | IF (e1,ents1,ents2) =>
		       (p"#if "; pexp e1; p"\n"; pents ents1;
			p"#else\n"; pents ents2;
			p"#endif")
		    | ERROR e => (p"#error "; pexp e)
		    | MARK (_,e) => (pent' e))

	in  pents ents
	end

    (*
	Parse external syntax.
    *)
    local
	fun lexarg (file : file) : E.lexarg =
	    let val lineno = ref 1
		val curpos = ref (Pos.pos (file,1))
		val start = ref (NONE : (string * pos) option)
		val com = ref 0
		val str = ref (nil : string list)
		val errors = ref false
		fun newline () : unit =
		    let val n = !lineno + 1
		    in  lineno := n;
			curpos := Pos.pos (file,n)
		    end
	    in
		{errors = fn () => !errors,
		 curpos' = fn () => !curpos,
		 curpos = fn () => (!curpos,!curpos),
		 newline = newline,
		 start = fn () => !start,
		 startCom = fn () => start := SOME ("comment",!curpos),
		 nestCom = fn () => com := !com + 1,
		 endCom = fn () => if !com = 0 then (start := NONE; true)
				   else (com := !com - 1; false),
		 startStr = fn () => start := SOME ("string",!curpos),
		 addToStr = fn s => str := s :: (!str),
		 finishStr = (fn () =>
			      let val (_,startPos) = valOf (!start)
				  val s = String.concat (rev (!str))
			      in  start := NONE;
				  str := nil;
				  (s,startPos,!curpos)
			      end),
		 parseError = (fn (m,p,_) =>
			       (errors := true;
				print_error p m))}
	    end

	structure LrVals = ProjectLrValsFun (structure Token = LrParser.Token)
	structure Lex = ProjectLexFun (structure Tokens = LrVals.Tokens)
	structure P = JoinWithArg (structure ParserData = LrVals.ParserData
				   structure LrParser = LrParser
				   structure Lex = Lex)

	val Toplevel : label list =
	    map Name.unit_label
	    ["TiltPrim","TiltVectorEq","TiltPrelude","TiltTop"]

	(*
		The lists of opened units associated with precompiled
		units and interfaces already mention the basis.
	*)
	fun fix (opened : E.opened) : E.opened = Toplevel @ opened
	fun fix' (opopt : E.opopt) : E.opopt = Option.map fix opopt

	fun fix_ie (ie:E.ie) : E.ie =
	    (case ie
	       of E.SRCI (src,opopt) => E.SRCI (src,fix' opopt)
		| _ => ie)

	fun fix_ue (ue:E.ue) : E.ue =
	    (case ue
	       of E.SRCU (src,opopt,asc) => E.SRCU (src,fix' opopt,asc)
		| _ => ue)

	fun fix_ent (ent:E.ent) : E.ent =
	    (case ent
	       of E.INTERFACE (I,ie) => E.INTERFACE (I,fix_ie ie)
		| E.UNIT (U,ue) => E.UNIT (U,fix_ue ue)
		| E.IF (c,ents1,ents2) => E.IF (c,fix_ents ents1,fix_ents ents2)
		| E.MARK (p,ent) => E.MARK (p,fix_ent ent)
		| _ => ent)

	and fix_ents (ents : E.ents) : E.ents = map fix_ent ents

	fun add_basis (ents : E.ents) : E.ents =
	    let val ents = fix_ents ents
	    in  (E.LOCAL (E.EXP_STR (I.F.basisdesc()))) :: ents
	    end

	fun parse (pos : pos, file : file) : E.ents =
	    let val ins = (TextIO.openIn file
			   handle (e as IO.Io _) =>
				fail pos (exnMessage e))
		val read = fn n => TextIO.inputN (ins,n)
		val lexarg = lexarg file
		val s = P.makeLexer read lexarg
		val (ents,_) = P.parse (15,s,#parseError lexarg,())
		val _ = TextIO.closeIn ins
		val ents =
		    if !Bootstrap orelse I.F.is_basisdesc file then ents
		    else add_basis ents
	    in	if #errors lexarg () then raise_error()
		else ents
	    end handle P.ParseError => raise_error()
    in
	val parse : pos * file -> E.ents =
	    Stats.subtimer ("Parsing project descriptions",parse)
    end

    (*
	Type-check and evaluate expressions.
    *)
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

    fun tyof (desc:desc, e:E.exp) : ty =
	let val fail : string -> 'a = fn s => fail (#pos desc) s
	    fun self (e':E.exp) : ty = tyof(desc,e')
	    fun binaryop rty (e1,e2,ty) =
		if eqty(self e1,ty) andalso eqty(self e2,ty)
		    then rty
		else fail ("expected " ^ tyname ty)
	    val stringop = binaryop STRING
	    val boolop = binaryop BOOL
	in
	    (case e
	       of E.EXP_VAR x => #1(unknown_value (desc,x))
		| E.EXP_ENV X => (check_env (desc,X); STRING)
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
		| E.EXP_DEF _ => BOOL
		| E.EXP_MARK (pos,e) => tyof (update_pos(desc,pos), e))
	end

    fun value2exp (v:value) : E.exp =
	(case v
	   of SVAL s => E.EXP_STR s
	    | IVAL i => E.EXP_INT i
	    | BVAL b => E.EXP_BOOL b)

    fun gets (v : value) : string =
	(case v
	   of SVAL s => s
	    | _ => error "expected string value")

    fun geti (v : value) : int =
	(case v
	   of IVAL i => i
	    | _ => error "expected int value")

    fun getb (v : value) : bool =
	(case v
	   of BVAL b => b
	    | _ => error "expected bool value")

    fun ev (desc:desc, e:E.exp) : value =
	let fun bad () : 'a = error "ev given unchecked expression"
	    fun self (e':E.exp) : value = ev(desc,e')
	    fun binaryop inj proj (e1,e2,f) =
		(inj (f (proj (self e1), proj (self e2))))
	    val stringop = binaryop SVAL gets
	    val scmp = binaryop BVAL gets
	    val bcmp = binaryop BVAL getb
	    val icmp = binaryop BVAL geti
	in
	    (case e
	       of E.EXP_VAR x => #2(known_value (desc,x))
		| E.EXP_ENV X => SVAL(known_env X)
		| E.EXP_STR s => SVAL s
		| E.EXP_CAT (e1,e2) => stringop (e1,e2,op^)
		| E.EXP_INT i => IVAL i
		| E.EXP_BOOL b => BVAL b
		| E.EXP_IF (e1,e2,e3) => self(if getb(self e1) then e2 else e3)
		| E.EXP_NOT e => BVAL (not (getb(self e)))
		| E.EXP_AND (e1,e2) => BVAL(getb(self e1) andalso getb(self e2))
		| E.EXP_OR (e1,e2) => BVAL(getb(self e1) orelse getb(self e2))
		| E.EXP_SEQ (e1,e2) => scmp (e1,e2,op=)
		| E.EXP_BEQ (e1,e2) => bcmp (e1,e2,op=)
		| E.EXP_IEQ (e1,e2) => icmp (e1,e2,op=)
		| E.EXP_ILT (e1,e2) => icmp (e1,e2,op<)
		| E.EXP_ILE (e1,e2) => icmp (e1,e2,op<=)
		| E.EXP_IGT (e1,e2) => icmp (e1,e2,op>)
		| E.EXP_IGE (e1,e2) => icmp (e1,e2,op>=)
		| E.EXP_DEF l => BVAL(isSome(lookup(desc,l)))
		| E.EXP_MARK (_,e) => self e)
	end

    (*
	Translate filenames.
    *)

    fun xfile (arg as (desc,_) : desc * E.exp) : file =
	(case tyof arg
	   of STRING =>
	       let val file = gets(ev arg)
		   val file =
			if OS.Path.isRelative file then
			    let val pos = #pos desc
				val descfile = Pos.file pos
				val dir = OS.Path.dir descfile
			    in  OS.Path.joinDirFile {dir=dir, file=file}
			    end
			else file
		   val file = OS.Path.mkCanonical file
	       in  file
	       end
	    | _ => fail (#pos desc) "expected filename")

    (*
	Decide HS interface equivalence.

	If HS_eq(_,pinterface1,_,pinterface2) = true,
	then |- pinterface1 == pinterface2 HS
    *)

    fun HS_interface (pos:pos, pinterface:file) : LinkIl.pinterface =
	let fun bad (msg:string) : 'a =
		fail pos (concat["compiled file (",pinterface,") ",msg])
	in
	    if Fs.exists pinterface then
		(case Fs.read_pinterface' pinterface
		   of SOME pi => pi
		    | NONE => bad "corrupt")
	    else bad "missing"
	end

    fun HS_eq (pos1:pos, pi1:file, pos2:pos, pi2:file) : bool =
	let val i1 = HS_interface(pos1,pi1)
	    val i2 = HS_interface(pos2,pi2)
	in  LinkIl.eq(nil,i1,i2)
	end

    (*
	Read off interface source.

	If has_src(iexp) = SOME (opened,file)
	then |- iexp -> (opened,file)
    *)


    fun has_src (iexp:I.iexp) : (I.opened * I.file) option =
	(case iexp
	   of I.SRCI (opened,{src,...}) => SOME (opened,src)
	    | I.PRECOMPI (_,opened,{src,...}) => SOME (opened,src)
	    | _ => NONE)


    (*
	Decide interface equivalence.

	If check_interface ((_,iexp1,_), (_,iexp2,_)) = ()
	then |- iexp1 == iexp2
    *)

    fun check_interface ((I1:label, iexp1:I.iexp, pos1:pos),
			 (I2:label, iexp2:I.iexp, pos2:pos)) : unit =
	let
	    fun ineq (msg:string) : 'a =
		let val msg : string list =
			if Name.eq_label(I1,I2) then
			    [label_name I2,
			     " not equivalent to previous definition (",
			     Pos.tostring pos1,") because ",msg]
			else
			    [label_name I2,
			     " not equivalent to the unit's interface (",
			     label_name I1," at ",Pos.tostring pos1,
			     ") because ",msg]
		in  fail pos2 (concat msg)
		end
	in
	    (case (has_src iexp1, has_src iexp2)
	       of (SOME (opened1,src1), SOME (opened2,src2)) =>
		    if Listops.eq_list(Name.eq_label,opened1,opened2) then
			if Fs.crc src1 = Fs.crc src2 then ()
			else ineq "their source files differ"
		    else ineq "their opened units differ"
		| _ =>
		    (case (iexp1,iexp2)
		       of (I.COMPI (nil, {pinterface=pi1}),
			   I.COMPI (nil, {pinterface=pi2})) =>
			    if HS_eq(pos1,pi1,pos2,pi2) then ()
			    else ineq "their compiled interfaces differ"
			| _ => ineq "they are incomparable"))
	end

    (*
	Infer opened units.

	If infer_opened d = units,
	then |- VDESC(d) downarrow units.
    *)

    fun infer_opened (desc:desc) : I.opened =
	let val {desc_,visible,...} = desc
	    val desc_ = rev desc_
	    fun visible_unit (pdec:I.pdec) : label option =
		let val l = I.P.label pdec
		in  if Set.member(visible,l) andalso Name.is_unit l
		    then SOME l
		    else NONE
		end
	in  List.mapPartial visible_unit desc_
	end
 
    (*
	Read off ascribed interface.

	If uexp_asc uexp = SOME I
	then |- uexp : I.
    *)

    fun uexp_asc (uexp:I.uexp) : label option =
	(case uexp
	   of I.SSRCU(_,I,_) => SOME I
	    | I.PRIMU (I,_) => SOME I
	    | I.PRECOMPU(_,_,I,_) => SOME I
	    | I.COMPU(_,I,_) => SOME I
	    | _ => NONE)

    (*
	Read off ascribed interface.

	If unit_asc(desc,U) = SOME I
	then DESC(desc) |- U : I.
    *)

    fun unit_asc (desc:desc, U:label) : label option =
	(case global_lookup(desc,U)
	   of SOME (PDEC (I.SCDEC (_,I,_))) => SOME I
	    | SOME (PDEC (I.UDEC (_,uexp,_))) => uexp_asc uexp
	    | _ => NONE)


    (*
	Elaborate unit expressions.

	If xue(desc,_,ue) = uexp
	then VDESC(desc) |- ue => uexp.
    *)

    fun xue (desc:desc, U:label, ue:E.ue) : I.uexp =
	let val descfile = Pos.file(#pos desc)
	in  (case ue
	       of E.SRCU(src,NONE,NONE) =>
		    let val src = xfile (desc,src)
			val opened = infer_opened desc
		    in  I.C.U.src(descfile,U,opened,src)
		    end
		| E.SRCU(src,SOME opened,NONE) =>
		    let val src = xfile (desc,src)
			val _ = check_opened (desc,opened)
		    in  I.C.U.src(descfile,U,opened,src)
		    end
		| E.SRCU(src,NONE,SOME I) =>
		    let val src = xfile (desc,src)
			val opened = infer_opened desc
			val _ = check_bound (desc,I)
		    in  I.C.U.ssrc(descfile,U,I,opened,src)
		    end
		| E.SRCU(src,SOME opened,SOME I) =>
		    let val src = xfile (desc,src)
			val _ = check_opened (desc,opened)
			val _ = check_bound (desc,I)
		    in  I.C.U.ssrc(descfile,U,I,opened,src)
		    end
		| E.PRIMU I =>
		    let val _ = check_bound (desc,I)
		    in  I.C.U.prim(descfile,U,I)
		    end
		| E.PRECOMPU (src,opened,I) =>
		    let val src = xfile (desc,src)
			val _ = check_opened (desc,opened)
			val _ = check_bound (desc,I)
			val uexp = I.C.U.precomp(descfile,U,I,opened,src)
			val _ = check_using (desc,I.P.U.using uexp)
		    in  uexp
		    end
		| E.COMPU I =>
		    let val _ = check_bound (desc,I)
			val uexp = I.C.U.comp(descfile,U,I)
			val _ = check_using (desc,I.P.U.using uexp)
		    in  uexp
		    end)
	end

    (*
	Elaborate interface expressions.

	If xie(desc,_,ie) = iexp
	then VDESC(desc) |- ie => iexp.
    *)

    fun xie (desc:desc, I:label, ie:E.ie) : I.iexp =
	let val descfile = Pos.file(#pos desc)
	in  (case ie
	       of E.SRCI(src,NONE) =>
		    let val src = xfile (desc,src)
			val opened = infer_opened desc
		    in  I.C.I.src(descfile,I,opened,src)
		    end
		| E.SRCI(src,SOME opened) =>
		    let val src = xfile (desc,src)
			val _ = check_opened (desc,opened)
		    in  I.C.I.src(descfile,I,opened,src)
		    end
		| E.PRIMI => I.C.I.prim (descfile,I)
		| E.PRECOMPI(src,opened) =>
		    let val src = xfile (desc,src)
			val _ = check_opened (desc,opened)
			val iexp = I.C.I.precomp(descfile,I,opened,src)
			val _ = check_using (desc,I.P.I.using iexp)
		    in  iexp
		    end
		| E.COMPI => I.C.I.comp (descfile,I))
	end

    (*
	Elaborate project description files.
    *)

    fun xent (desc:desc, ent:E.ent) : desc =
	let val pos = #pos desc
	in  (case ent
	       of E.INTERFACE (I,ie) =>
		    (case global_lookup (desc,I)
		       of SOME entry =>
			    let val pdec = entry_pdec entry
				val idec1 = I.D.Pdec.idec pdec
				val iexp2 = xie (desc,I,ie)
				val idec2 = (I,iexp2,pos)
				val _ = check_interface(idec1,idec2)
			    in	reveal(desc,[I])
			    end
			| NONE =>
			    let val iexp = xie(desc,I,ie)
				val pdec = I.IDEC (I,iexp,pos)
			    in  insert(desc,I,PDEC pdec)
			    end)
		| E.SC (U,I) =>
		    (case unit_asc (desc,U)
		       of SOME I' =>
			    let val idec1 = known_interface (desc,I')
				val idec2 = unknown_interface (desc,I)
				val _ = check_interface(idec1,idec2)
			    in  reveal(desc,[U])
			    end
			| NONE =>
			    let val _ = check_bound(desc,I)
				val _ = check_unbound(desc,U)
				val pdec = I.SCDEC(U,I,pos)
			    in  insert(desc,U,PDEC pdec)
			    end)
		| E.UNIT (U,ue) =>
		    let val _ = check_unbound(desc,U)
			val uexp = xue(desc,U,ue)
			val pdec = I.UDEC(U,uexp,pos)
		    in  insert(desc,U,PDEC pdec)
		    end
		| E.VAL (x,exp) =>
		    let val _ = check_unbound(desc,x)
			val ty = tyof (desc,exp)
			val v = ev (desc,exp)
		    in  insert(desc,x,VDEC (ty,v,pos))
		    end
		| E.INCLUDE file => add_include (desc,xfile(desc,file))
		| E.LOCAL file => add_include (desc,xfile(desc,file))	(* XXX *)
		| E.IF (c,ents1,ents2) =>
		    (case tyof (desc,c)
		       of BOOL =>
			    let val (used,unused) =
				    if getb(ev(desc,c))
				    then (ents1,ents2)
				    else (ents2,ents1)
				val _ = check_ents(desc,unused)
			    in  xents(desc,used)
			    end
			| _ => fail pos "expected boolean")
		| E.ERROR msg =>
		    (case tyof (desc,msg)
		       of STRING =>
			    if #ck desc then desc
			    else fail pos (gets(ev(desc,msg)))
			| _ => fail pos "expected string")
		| E.MARK(pos,ent) => xent(update_pos(desc,pos), ent))
	end

    and xents (desc:desc, ents:E.ents) : desc =
	(case ents
	   of nil => desc
	    | ent::ents => xents(xent(desc,ent), ents))

    and check_ents (desc:desc, ents:E.ents) : unit =
	let val desc = update_checking(desc,true)
	in  ignore(xents(desc, ents))
	end

    and add_include (desc:desc, file:file) : desc =
	(case lookup_file (desc,file)
	   of SOME (ELABORATED labels) => reveal (desc, Set.listItems labels)
	    | NONE =>
		let val pos = #pos desc
		    val desc0 = desc
		    val desc = start_file (desc,file)
		    val desc = xents (desc, parse (pos,file))
		in  finish_file (desc0, file, desc)
		end
	    | SOME (ELABORATING pos') =>
		fail pos' ("include cycle at " ^ Pos.tostring (#pos desc)))

    fun finish (desc:desc) : I.desc =
	let val {desc_,...} = desc
	in  rev desc_
	end

end
