(*
signature LINKIL = 
  sig
      structure Tyvar : TYVAR
      structure Prim : PRIM
      structure Il : IL
      structure Ppprim : PPPRIM
      structure Ppil : PPIL
      structure IlUtil : ILUTIL
      structure IlPrimUtil : PRIMUTIL
      structure IlLookup : ILLOOKUP
      structure IlStatic : ILSTATIC 
      structure Datatype : DATATYPE
      structure Basis : BASIS
      structure InfixParse : INFIXPARSE
      structure Pat : PAT
      structure Toil : TOIL
      structure IlEval : ILEVAL

      val initial_sbnds : Il.sbnd list
      val initial_context : Il.context

      val elaborate       : string -> Il.sbnd list * Il.context (* uses the inital_context *)
      val elaborate_diff  : string -> Il.sbnd list * Il.context_entry list (* returns only the differential *)
      val elaborate'      : Il.context * string -> Il.sbnd list * Il.context
      val elaborate_diff' : Il.context * string -> Il.sbnd list * Il.context_entry list
      val evaluate  : Il.sbnd list -> Il.sbnd list
      val parse : string -> Ast.dec
      val setdepth : int -> unit
  end
*)
structure LinkIl (* : LINKIL *) =
    struct
	structure Tyvar = Tyvar();
	structure Prim = Prim();
	structure Il = Il(structure Prim = Prim
			  structure Tyvar = Tyvar);
	structure Formatter = Formatter;
	structure AstHelp = AstHelp;
	structure Ppprim = Ppprim(structure Prim = Prim);
	structure Ppil = Ppil(structure AstHelp = AstHelp
			      structure Ppprim = Ppprim
			      structure Il = Il); 
	structure IlUtil = IlUtil(structure Il = Il
				  structure AstHelp = AstHelp
				  structure Ppil = Ppil);
	structure IlPrimUtilParam = IlPrimUtilParam(structure IlUtil = IlUtil);
	structure IlPrimUtil = PrimUtil(structure Prim = Prim
					structure Ppprim = Ppprim
					structure PrimUtilParam = IlPrimUtilParam);
	structure IlLookup = IlLookup(structure Il = Il
				      structure AstHelp = AstHelp
				      structure IlUtil = IlUtil
				      structure Ppil = Ppil);
	structure IlStatic = IlStatic(structure Il = Il
				      structure IlLookup = IlLookup
				      structure PrimUtil = IlPrimUtil
				      structure IlUtil = IlUtil
				      structure Ppil = Ppil);
	structure Datatype = Datatype(structure Il = Il
				      structure IlLookup = IlLookup
				      structure AstHelp = AstHelp
				      structure IlStatic = IlStatic
				      structure IlUtil = IlUtil
				      structure Ppil = Ppil);
	structure Basis = Basis(structure Il = Il		
				structure Ppil = Ppil
				structure Datatype = Datatype      
				structure IlUtil = IlUtil);
	structure InfixParse = InfixParse(structure Il = Il
					  structure Ppil = Ppil
					  structure AstHelp = AstHelp);
	structure Pat = Pat(structure Il = Il
			    structure IlLookup = IlLookup
			    structure IlStatic = IlStatic
			    structure IlUtil = IlUtil
			    structure AstHelp = AstHelp
			    structure Datatype = Datatype
			    structure Ppil = Ppil
			    structure InfixParse = InfixParse);
	structure Toil = Toil(structure Il = Il
			      structure IlLookup = IlLookup
			      structure AstHelp = AstHelp
			      structure IlStatic = IlStatic
			      structure IlUtil = IlUtil
			      structure Ppil = Ppil
			      structure Basis = Basis
			      structure Pat = Pat
			      structure Datatype = Datatype
			      structure InfixParse = InfixParse);
	structure IlEval = IlEval(structure Il = Il
				  structure PrimUtil = IlPrimUtil
				  structure IlStatic = IlStatic
				  structure IlUtil = IlUtil
				  structure Ppil = Ppil
				  structure IlLookup = IlLookup);
	    
	open Il IlUtil Ppil IlStatic Formatter
	    
	structure Il = Il

	val _ = Compiler.Control.Print.printDepth := 15;
	val _ = Pagewidth := 100;

	fun setdepth d = Compiler.Control.Print.printDepth := d
	fun make_source s = Compiler.Source.newSource(s,0,TextIO.openIn s,true,
						      Compiler.ErrorMsg.defaultConsumer())
	fun parse s = Compiler.Compile.parse (make_source s)

	    
	val _ = Ppil.convar_display := Ppil.VALUE_ONLY
	    
	val (initial_context, initial_sbnds) = Basis.initial_context(fn e => GetExpCon([],e),
								     fn c => GetConKind([],c),
								     fn m => GetModSig([],m),
								     Toil.xty) 
	val initial_sbnds_len = length initial_sbnds
	    
	fun elaborate_diff'(context,s) = 
	    let
		val what = parse s
		val sbnd_ctxt_list = Toil.xdec(context,what) 
		    handle (e as IlUtil.NOTFOUND s) => (print "NOTFOUND: "; print s; print "\n"; raise e)
		val sbnds = List.mapPartial #1 sbnd_ctxt_list
		val ctxts = map #2 sbnd_ctxt_list
	    in	(sbnds,ctxts)
	    end;
	fun elaborate'(context,s) = 
	    let val (sbnds,context_diff) = elaborate_diff'(context,s)
	    in	(sbnds,add_context_entries(context,context_diff))
	    end
	fun elaborate s = elaborate'(initial_context,s)
	fun elaborate_diff s = elaborate_diff'(initial_context,s)
	fun evaluate target_sbnds =
	    let
		val allsbnds = initial_sbnds @ target_sbnds
		val module = Il.MOD_STRUCTURE allsbnds
		val allresmodule = IlEval.eval_mod module
		val ressbnds =
		    (case allresmodule of
			 Il.MOD_STRUCTURE allressbnds => List.drop(allressbnds,initial_sbnds_len)
		       | _ => Util.error "help.sml" "a structure evaluated to a non-structure")
	    in ressbnds
	    end
    end (* struct *)

