signature DOOPTS = 
    sig 
	structure Nil : NIL
	structure Anormalize : ANORMALIZE
    end 


functor DoOpts (structure Normalize : NORMALIZE
                structure PpNil : PPNIL
		structure Nil : NIL
		structure NilContext : NILCONTEXT
		structure NilEval : NILEVAL
		structure NilPrimUtil : PRIMUTIL
		structure NilStatic : NILSTATIC
		structure NilSubst : NILSUBST
		structure NilUtil : NILUTIL
		structure Linearize : LINEARIZE

		sharing PpNil.Nil = Nil = NilContext.Nil = NilEval.Nil = NilStatic.Nil 
		     = NilUtil.Nil = Linearize.Nil

		sharing Nil.Prim = NilPrimUtil.Prim
                sharing type Nil.kind = Normalize.kind = NilSubst.kind
		sharing type Nil.con = NilSubst.con = NilPrimUtil.con = Normalize.con 
		sharing type Nil.exp = NilSubst.exp = NilPrimUtil.exp 
	        sharing type NilContext.context = NilStatic.context = Normalize.context
		    ) = 
			 
struct
    structure Nil = Nil

    val do_flatten = NilOpts.do_flatten
    val do_reduce = NilOpts.do_reduce
    val do_inline = NilOpts.do_inline
    val do_uncurry = NilOpts.do_uncurry

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
   
    structure Uncurry = Uncurry (structure Nil = Nil
				 structure Ppnil = PpNil)

    structure ExpTable = ExpTable(structure Nil = Nil
				  structure NilUtil = NilUtil 
				  structure Ppnil = PpNil
				      )

    structure Anormalize = Anormalize (structure Normalize = Normalize
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
    structure Inline = Inline ( structure Nil = Nil
				structure Ppnil = PpNil
				structure Squish = Squish
				    )


    fun do_opts debug nilmod = 
	let val debug = debug orelse (!NilOpts.debug)
	   
	    val _ = ( NilOpts.init_clicks() )

	    val _ = if (!NilOpts.print_pre) 
		      then (print "\n\n=======================================\n\n";
			    print "Pre Optimization:\n";
			    PpNil.pp_module nilmod;
			    print "\n")
		  else () ;
		      
	    val nilmod = (Stats.timer("Anormalize", Anormalize.doModule debug)) nilmod;
	    val _ =  if !NilOpts.print_anormalize
			 then (print "\n\n=======================================\n\n";
			       print "A-normal form:\n";
			       PpNil.pp_module nilmod;
			       print "\n")
		     else print "Anormalization complete\n";

	    val nilmod = if !NilOpts.do_anormalize2 then 
		let val nilmod = (Stats.timer("Anormalize2", Anormalize.doModule true)) nilmod;
		    val _ =  if debug 
				 then (print "\n\n=======================================\n\n";
				       print "A-normal form2:\n";
				       PpNil.pp_module nilmod;
				       print "\n")
			     else print "Anormalization2 complete\n";
		in nilmod end
			 else nilmod

			 
	    (* val _ = PpNil.elide_bnd := true; *)

 
	   val nilmod =
		if (!do_flatten) then 
		    (let val nilmod = (Stats.timer("Flatten args", FlattenArgs.doModule debug)) nilmod
		     in (  if !NilOpts.print_flatten andalso !do_flatten 
			       then (print "\n\n=======================================\n\n";
				     print "Flattened args:\n";
				     PpNil.pp_module nilmod;
				     print "\n") 
			   else print "Flatten args complete\n"; nilmod )
		     end)
		else nilmod

	  	    
	    val nilmod = if !do_uncurry then 
		let val nilmod = (Stats.timer("Uncurry args", Uncurry.doModule debug)) nilmod
		in (  if !NilOpts.print_uncurry andalso !do_uncurry 
			 then (print "\n\n=======================================\n\n";
			       print "Uncurried args:\n";
			       PpNil.pp_module nilmod;
			       print "\n") 
		     else print "Uncurry args complete\n"; nilmod )
		end 
			 else nilmod
	    
	    val nilmod = if !do_inline then 
		(Stats.timer("Inline General", Inline.doModule debug)) nilmod
		else nilmod
	    val _ =  if !NilOpts.print_inline andalso !do_inline 
			 then (print "\n\n=======================================\n\n";
			       print "Inlined functions:\n";
			       PpNil.pp_module nilmod;
			       print "\n") 
		     else print "Functions inlined\n";

	    (* As I'm not doing renaming during the general inlining, should do it now.... *)
	    val nilmod = if !do_inline then 
		(Stats.timer("Linearization2",Linearize.linearize_mod)) nilmod
		else nilmod
	    val _ = if !NilOpts.print_inline andalso !do_inline 
			then (print "\n\n=======================================\n\n";
			      print "renaming results:\n";
			      PpNil.pp_module nilmod;
			      print "\n")
		    else print "Renaming complete\n"
			
	    val nilmod = if !do_reduce then
		let val nilmod = (Stats.timer("Reduce", Reduce.doModule debug)) nilmod
		in ( if !NilOpts.print_reduce 
			 then (print "\n\n=======================================\n\n";
			       print "Reduced:\n";
			       PpNil.pp_module nilmod;
			       print "\n") 
		     else print "Reduction complete\n"; nilmod )
		end 
			 else nilmod
			
	    val nilmod = if !do_reduce then
		let val nilmod = (Stats.timer("Reduce", Reduce.doModule debug)) nilmod
		in ( if !NilOpts.print_reduce 
			 then (print "\n\n=======================================\n\n";
			       print "Reduced:\n";
			       PpNil.pp_module nilmod;
			       print "\n") 
		     else print "Reduction complete\n"; nilmod )
		end 
			 else nilmod

	    val _ = ( NilOpts.print_clicks() )
	in
	    nilmod
	end 
end 




