structure Linkml =
  struct
      structure Formatter = Formatter;
      structure AstHelp = AstHelp;
      structure Tyvar = LinkIl.Tyvar;
      structure Prim = LinkIl.Prim;
      structure Il = LinkIl.Il;
      structure Ppprim = LinkIl.Ppprim;
      structure Ppil = LinkIl.Ppil;
      structure IlUtil = LinkIl.IlUtil;
      structure IlPrimUtilParam = LinkIl.IlPrimUtilParam;
      structure IlPrimUtil = LinkIl.IlPrimUtil;
(*      structure IlLookup = LinkIl.IlLookup; *)
      structure IlStatic = LinkIl.IlStatic;
      structure Datatype = LinkIl.Datatype;
      structure Basis = LinkIl.Basis;
      structure InfixParse = LinkIl.InfixParse;
      structure Pat = LinkIl.Pat;
      structure Toil = LinkIl.Toil;
      structure IlEval = LinkIl.IlEval;
      structure Annotation = Annotation;


    structure Nil = Nil(structure Annotation = Annotation
			structure Prim = Prim);

(*
    structure Ppnil = Ppnil(structure Nil = Nil
			    structure Ppprim = Ppprim)
*)
  end
