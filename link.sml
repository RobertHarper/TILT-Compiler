structure Linkml =
  struct
    structure Util = Util();
    structure GraphUtil = GraphUtil();
    structure Name = Name(structure Util = Util);
    structure Prim = Prim(structure Util = Util);
    structure Tyvar = Tyvar(structure Util = Util
			    structure Name = Name);
    structure Il = Il(structure Util = Util
		      structure Prim = Prim
		      structure Tyvar = Tyvar
		      structure Name = Name);
    structure Formatter = Formatter();
    structure AstHelp = AstHelp(structure Util = Util
				structure Formatter = Formatter
				structure Il = Il);
    structure Ppil = Ppil(structure Formatter = Formatter
			  structure AstHelp = AstHelp
			  structure Il = Il); 
    structure IlUtil = IlUtil(structure Il = Il
			      structure AstHelp = AstHelp
			      structure Ppil = Ppil);
    structure IlStatic = IlStatic(structure Il = Il
				  structure IlUtil = IlUtil
				  structure Ppil = Ppil);
    structure IlLookup = IlLookup(structure Il = Il
				  structure AstHelp = AstHelp
				  structure IlStatic = IlStatic
				  structure IlUtil = IlUtil
				  structure Ppil = Ppil);
    structure Datatype = Datatype(structure Il = Il
				  structure GraphUtil = GraphUtil
				  structure IlLookup = IlLookup
				  structure AstHelp = AstHelp
				  structure IlStatic = IlStatic
				  structure IlUtil = IlUtil
				  structure Ppil = Ppil);
    structure Basis = Basis(structure Il = Il		
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
			      structure IlStatic = IlStatic
			      structure IlUtil = IlUtil
			      structure Ppil = Ppil
			      structure IlLookup = IlLookup);
(*
    structure Pil = Pil(structure Il = Il);
    structure ToPil = ToPil(structure Il = Il
			    structure IlUtil = IlUtil
			    structure Pil = Pil);
*)
  end