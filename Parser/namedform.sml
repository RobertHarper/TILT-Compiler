(*
  Rewrite declarations so all functor applications are in named form
  (i.e. all arguments are structure ids).
*)

structure NamedForm:
sig val namedForm: Ast.dec -> Ast.dec end =
struct
  open Ast

  datatype 'a return = Unaltered | Altered of 'a | Declare of strb list * 'a

  exception Impossible
  fun impossible msg =
    (print ("namedform.sml: Internal error in " ^ msg ^ "\n");
     raise Impossible)

  fun process (Unaltered, _) = Unaltered
    | process (Altered new, build) = Altered (build new)
    | process (Declare (strbs, new), build) = Declare (strbs, build new)

  fun grind_list namer forms =
    let
      val forms' = map namer forms
      fun level ([], best) = best
	| level ((declare as Declare _)::_, _) = declare
	| level ((altered as Altered _)::rest, best) = level (rest, altered)
	| level (_::rest, best) = level (rest, best)
      fun alter (Unaltered, x) = x
	| alter (Altered x, _) = x
	| alter _ = impossible "grind_list [alter]"
      fun declare (Unaltered, x)  = ([], x)
	| declare (Altered x, _)  = ([], x)
	| declare (Declare xy, _) = xy
      fun unzip strbformlist =
	case ListPair.unzip strbformlist of
	  (strblistlist, formlist) => (List.concat strblistlist, formlist)
    in
      case level (forms', Unaltered) of
	Unaltered => Unaltered
      | Altered _ => Altered (ListPair.map alter (forms', forms))
      | Declare _ => Declare (unzip (ListPair.map declare (forms', forms)))
    end

  val unique_n = ref 0 (* for gensym innamify_strexpbools *)

  fun namify_let (letter, name_dec, dec, namer, exp) =
        (case (name_dec dec, namer exp) of
	   (Unaltered, Unaltered) => Unaltered
	 | (Unaltered, Altered exp') => Altered (letter (dec, exp'))
	 | (Unaltered, Declare (strbs, exp')) => 
	     Altered (letter (SeqDec [dec, StrDec strbs], exp'))
	 | (Altered dec', Unaltered) => Altered (letter (dec', exp))
	 | (Altered dec', Altered exp') => Altered (letter (dec', exp'))
	 | (Altered dec', Declare (strbs, exp')) =>
	     Altered (letter (SeqDec [dec', StrDec strbs], exp'))
	 | (Declare _, _) => impossible "Declare found in dec")

  (* take a strexp*bool list and give new names to each strexp,
     returning a list of new strbs and a new strexp*bool list
     replacing the old strexps with their new names. *)
  fun nameify_strexpbools strexpbools =
    let
      fun is_varstr (VarStr _) = true
	| is_varstr (MarkStr (s, _)) = is_varstr s
	| is_varstr _ = false
      fun grind (strexp, _) =
	if is_varstr strexp then ([], (strexp, false)) else
	  let
	    val name = ("<Named_Structure" ^ Int.toString (!unique_n) ^ ">"
			before unique_n := !unique_n+1)
	    val symbol = Symbol.strSymbol name
	    val (strbs, strexp') = (case name_strexp strexp of
				      Unaltered => ([], strexp)
				    | Altered se => ([], se)
				    | Declare arg => arg)
	  in
	    (Strb {name=symbol, def=strexp', constraint=NoSig}::strbs,
	     (VarStr [symbol], false))
	  end
      val (newstrbs, newsebs) = ListPair.unzip (map grind strexpbools)
    in
      (List.concat newstrbs, newsebs)
    end

  and name_strexp (VarStr path) = Unaltered
    | name_strexp (StructStr dec) =
        process (name_dec dec, fn d => StructStr d)
    | name_strexp (AppStr (path, strexpbools)) =
	let val (newstrbs, newsebs) = nameify_strexpbools strexpbools
	in Declare (newstrbs, AppStr (path, newsebs))
	end
    | name_strexp (LetStr (dec, strexp)) =
	namify_let (LetStr, name_dec, dec, name_strexp, strexp)
    | name_strexp (MarkStr (strexp, region)) =
	process (name_strexp strexp, fn se => MarkStr (se, region))

  and name_fctexp (VarFct _) = Unaltered
    | name_fctexp (FctFct {params, body, constraint}) =
        process
	(name_strexp body,
	 fn se => FctFct {params=params, body=se, constraint=constraint})
    | name_fctexp (LetFct (dec, fctexp)) =
	namify_let (LetFct, name_dec, dec, name_fctexp, fctexp)
    | name_fctexp (AppFct (path, strexpbools, fsigconst)) =
	let val (newstrbs, newsebs) = nameify_strexpbools strexpbools
	in Declare (newstrbs, AppFct (path, newsebs, fsigconst)) end
    | name_fctexp (MarkFct (fctexp, region)) =
	process (name_fctexp fctexp, fn fe => MarkFct (fe, region))

  and dec_declare (Declare (strbs, dec)) =
        Altered (SeqDec (rev (map (fn strb => StrDec [strb]) strbs) @ [dec]))
    | dec_declare arg = arg

  and name_dec (StrDec strbs) =
	dec_declare (process (grind_list name_strb strbs, fn ss => StrDec ss))
    | name_dec (AbsDec strbs) =
	dec_declare (process (grind_list name_strb strbs, fn ss => AbsDec ss))
    | name_dec (FctDec fctbs) =
	dec_declare (process (grind_list name_fctb fctbs, fn fs => FctDec fs))
    | name_dec (LocalDec (dec1, dec2)) =
	process (grind_list name_dec [dec1, dec2],
		 fn [d1, d2] => LocalDec (d1, d2) | _ => impossible "name_dec")
    | name_dec (SeqDec decs) =
	process (grind_list name_dec decs, fn ds => SeqDec ds)
    | name_dec (MarkDec (dec, region)) =
	process (name_dec dec, fn d => MarkDec (d, region))
    (* No modules are allowed in the remaining constructions:
		ValDec | ValrecDec | FunDec | TypeDec | DatatypeDec |
		AbstypeDec | ExceptionDec | SigDec | FsigDec |
		OpenDec | OvldDec | FixDec | ImportDec *)
    | name_dec _ = Unaltered

  and name_strb (Strb {name, def, constraint}) =
        process (name_strexp def,
		 fn d => Strb {name=name, def=d, constraint=constraint})
    | name_strb (MarkStrb (strb, region)) =
	process (name_strb strb, fn sb => MarkStrb (sb, region))

  and name_fctb (Fctb {name, def}) =
        process (name_fctexp def, fn d => Fctb {name=name, def=d})
    | name_fctb (MarkFctb (fctb, region)) =
	process (name_fctb fctb, fn fb => MarkFctb (fb, region))

  fun namedForm dec =
        case name_dec dec of
	  Unaltered => dec
	| Altered d => d
	| _ => impossible "namedForm"

end
