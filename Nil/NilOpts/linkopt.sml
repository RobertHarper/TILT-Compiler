functor NilOpts (structure PpNil : PPNIL
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

    structure Squish = Squish(structure Nil = Nil )
	
(*    structure NilEval = NilEvaluate(structure Nil = Nil
				    structure NilUtil = NilUtil
				    structure Subst = NilSubst
				    structure Ppnil = PpNil
				    structure PrimUtil = NilPrimUtil) *)
	
    structure Reduce = Reduce (structure Nil = Nil
			       structure Squish = Squish
			       structure Ppnil = PpNil
			       structure PrimUtil = NilPrimUtil
			       structure NilEval = NilEval
			       structure NilUtil = NilUtil
			       structure NilSubst = NilSubst
					   )
	
    structure FlattenArgs = FlattenArgs(structure Nil  = Nil
					structure Ppnil = PpNil
					structure Subst = NilSubst
					structure Squish = Squish)
    	
    structure Anormalize = Anormalize (structure Squish = Squish
				       structure NilUtil = NilUtil
				       structure Subst = NilSubst
				       structure NilStatic = NilStatic
				       structure NilContext = NilContext
				       structure Ppnil = PpNil
				       structure Nil = Nil
				       structure NilEval = NilEval
				       structure PrimUtil = NilPrimUtil)
    
	
    fun do_opts debug nilmod = 
	
	let val do_opts = Stats.bool "Optimization"
	in
	    if !do_opts then 
		let 
		    val nilmod = (Stats.timer("Anormalize", Anormalize.Anormalize)) nilmod;
		    val _ =  if debug
				 then (print "\n\n=======================================\n\n";
				       print "A-normal form:\n";
				       PpNil.pp_module nilmod;
				       print "\n")
			     else print "Anormalization complete\n";
				 
				 
		    val nilmod = (Stats.timer("Flatten args", FlattenArgs.reduceModule)) nilmod;  
		    val _ =  if debug 
				 then (print "\n\n=======================================\n\n";
				       print "Flattened args:\n";
				       PpNil.pp_module nilmod;
				       print "\n") 
			     else print "Flatten args complete\n";
				 
  	
	    
		    val nilmod = (Stats.timer("Reduce", Reduce.do_it debug)) nilmod;  
		    val _ =  if debug 
				 then (print "\n\n=======================================\n\n";
				       print "Reduced:\n";
				       PpNil.pp_module nilmod;
				       print "\n") 
			     else print "Reduction complete\n";
		in
		    nilmod
		end 
	    else
		nilmod
	end 
end 




