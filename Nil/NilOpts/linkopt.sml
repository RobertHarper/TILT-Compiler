functor DoOpts (structure PpNil : PPNIL
		structure Nil : NIL
		structure NilContext : NILCONTEXT
		structure NilEval : NILEVAL
		structure NilPrimUtil : PRIMUTIL
		structure NilStatic : NILSTATIC
		structure NilSubst : NILSUBST
		structure NilUtil : NILUTIL
		sharing PpNil.Nil = Nil = NilContext.Nil = NilEval.Nil = NilStatic.Nil 
		     = NilUtil.Nil

		sharing Nil.Prim = NilPrimUtil.Prim
		sharing type Nil.con = NilSubst.con = NilPrimUtil.con 
		sharing type Nil.exp = NilSubst.exp = NilPrimUtil.exp 
	        sharing type NilContext.context = NilStatic.context
		    ) = 
			 
struct
    structure Nil = Nil

    val do_flatten = NilOpts.do_flatten
    val do_reduce = NilOpts.do_reduce
	


    structure Squish = Squish(structure Nil = Nil )
	
    structure Reduce = Reduce (
			       structure Nil = Nil
			       structure Squish = Squish
			       structure Ppnil = PpNil
			       structure PrimUtil = NilPrimUtil
			       structure NilEval = NilEval
			       structure NilUtil = NilUtil
			       structure NilSubst = NilSubst
					   )
	
    structure FlattenArgs = FlattenArgs(
					structure Nil  = Nil
					structure Ppnil = PpNil
					structure Subst = NilSubst
					structure Squish = Squish)
    structure ExpTable = ExpTable(structure Nil = Nil)

    structure Anormalize = Anormalize (
				       structure ExpTable = ExpTable
				       structure Squish = Squish
				       structure NilUtil = NilUtil
				       structure Subst = NilSubst
				       structure NilStatic = NilStatic
				       structure NilContext = NilContext
				       structure Ppnil = PpNil
				       structure Nil = Nil
				       structure NilEval = NilEval
				       structure PrimUtil = NilPrimUtil)
    
    fun do_opts debug nilmod = 
	let 
	    val _ = NilOpts.init_clicks

	    val nilmod = (Stats.timer("Anormalize", Anormalize.doModule debug)) nilmod;
	    val _ =  if debug
			 then (print "\n\n=======================================\n\n";
			       print "A-normal form:\n";
			       PpNil.pp_module nilmod;
			       print "\n")
		     else print "Anormalization complete\n";
	    val nilmod = (Stats.timer("Anormalize2", Anormalize.doModule true)) nilmod;
	    val _ =  if debug
			 then (print "\n\n=======================================\n\n";
			       print "A-normal form2:\n";
			       PpNil.pp_module nilmod;
			       print "\n")
		     else print "Anormalization2 complete\n";
			 
	    val nilmod = if !do_flatten then 
		(Stats.timer("Flatten args", FlattenArgs.doModule debug)) nilmod
		else nilmod
	    val _ =  if debug andalso !do_flatten 
			 then (print "\n\n=======================================\n\n";
			       print "Flattened args:\n";
			       PpNil.pp_module nilmod;
			       print "\n") 
		     else print "Flatten args complete\n";
			 
	    val nilmod = if !do_reduce then
		(Stats.timer("Reduce", Reduce.doModule debug)) nilmod
		else nilmod
	    val _ =  if debug andalso !do_reduce
			 then (print "\n\n=======================================\n\n";
			       print "Reduced:\n";
			       PpNil.pp_module nilmod;
			       print "\n") 
		     else print "Reduction complete\n";

	    val _ = ( NilOpts.print_clicks() )
	in
	    nilmod
	end 
end 




