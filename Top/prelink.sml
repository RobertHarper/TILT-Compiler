structure Prelink :> PRELINK =
struct

    structure F = Formatter

    val error = fn s => Util.error "prelink.sml" s

    val PrelinkDebug = Stats.ff "PrelinkDebug"
    fun debugdo (f : unit -> 'a) : unit = if !PrelinkDebug then (f(); ()) else ()
    
    structure Ue = UnitEnvironment
    type crc = Crc.crc
    type ue = Ue.ue
    type equiv = crc * crc -> bool

    datatype entry =
	UNIT of {name:string, iface:crc * ue, objue:ue}
      | IMPORT of {name:string, iface:crc * ue}
      | IFACE of {name:string, ue:ue}

    datatype error =
	SHADOW of ue * string
      | MISMATCH of string * ue * string * crc

    type errors = error list	(* backwards *)

    fun insert (ctx : ue, errors : errors, name : string,
		crc : crc) : ue * errors =
	let val errors =
		(case Ue.find (ctx,name)
		   of NONE => errors
		    | SOME _ => (SHADOW (ctx, name) :: errors))
	    val ctx = Ue.insert (ctx,name,crc)
	in  (ctx, errors)
	end

    fun checkImport (eq : equiv, what : string, ctx : ue)
		    (name : string, crc : crc, e : errors) : errors =
	(case Ue.find (ctx, name)
	   of NONE => (MISMATCH (what, ctx, name, crc) :: e)
	    | SOME ctxcrc =>
		if eq (ctxcrc, crc) then e
		else (MISMATCH (what, ctx, name, crc) :: e))

    fun checkUe (eq : equiv, what : string, ctx : ue, errors : errors,
		 ue : ue) : errors =
	Ue.foldi (checkImport (eq,what,ctx)) errors ue

    fun checkEntry (eq : equiv)
		   (entry : entry, (ctx : ue, e : errors)) : ue * errors =
	(case entry
	   of UNIT {name,iface=(crc,ifaceue),objue} =>
		let val what = "unit " ^ name
		    val e = checkUe (eq, what ^ " interface", ctx, e, ifaceue)
		    val e = checkUe (eq, what ^ " object", ctx, e, objue)
		in  insert (ctx, e, name, crc)
		end
	    | IMPORT {name,iface=(crc,ue)} =>
		let val e = checkUe (eq, "unit " ^ name, ctx, e, ue)
		in  insert (ctx, e, name, crc)
		end
	    | IFACE {name,ue} =>
		let val e = checkUe (eq, "interface " ^ name, ctx, e, ue)
		in  (ctx,e)
		end)

    fun checkEntries (eq : equiv) (entries : entry list) : errors =
	let val (_,errors) = foldl (checkEntry eq) (Ue.empty, nil) entries
	in  errors
	end

    fun pp_shadow (name : string) : F.format =
	F.String ("unit " ^ name ^ " not unique")

    fun pp_mismatch (what : string, name : string, crc : crc) : F.format =
	F.Hbox [F.String what, F.String " requires ", F.String name,
		F.String " : ", Crc.pp_crc crc]

    fun pp_error (e : error) : F.format =
	let val (ue, msg) =
		(case e
		   of SHADOW (ctx, name) => (ctx, pp_shadow name)
		    | MISMATCH (what, ctx, name, crc) =>
			(ctx, pp_mismatch (what, name, crc)))
	    val ue = F.Hbox[F.String "sees: ", Ue.pp_ue ue]
	in  F.Vbox[msg, F.Break, ue]
	end

    fun pp_errors (errors : error list) : unit =
	app (fn e => (F.print_fmt (pp_error e); print "\n")) errors

    fun check (eq : equiv, entries : entry list) : unit =
	(case checkEntries eq entries
	   of nil => ()
	    | errors =>
		(debugdo (fn () => pp_errors errors);
		 error "unit environments do not match up"))

end
