structure Linkml =
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

    structure Annotation = Annotation;


    structure Nil = Nil(structure Annotation = Annotation
			structure Prim = Prim);

(*
    structure Ppnil = Ppnil(structure Nil = Nil
			    structure Ppprim = Ppprim)
*)
  end
