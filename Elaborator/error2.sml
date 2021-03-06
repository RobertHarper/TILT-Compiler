(* Stateful module holding the error state of the elaboration process *)

structure Error
   :> ERROR =
struct

    datatype ErrorLevel = NoError | Warn | Error
    type region = Ast.srcpos * Ast.srcpos
    structure Il = Il
    open Il IlUtil Util

    val track = Stats.ff("ElaboratorTrack")

    fun tab n = print(spaces n)
    fun tab_region() = tab 10
    fun tab_region_with str = (tab 10; print str)

    val error_level = ref NoError
    val src_region = ref ([] : region list)

    fun nofilepos (_ : SourceMap.charpos) = ("nofilepos.sml",0,0)
    val filepos = ref nofilepos

    fun error_max (Error,_) = Error
      | error_max (_,Error) = Error
      | error_max (Warn,_) = Warn
      | error_max (_,Warn) = Warn
      | error_max (NoError,NoError) = NoError

    fun reset fp = (error_level := NoError;
		    src_region := [];
		    filepos := fp)

    fun get_error() = !error_level

    val pos0 = LinkParse.lexer_initial_position

    fun peek_region () : region =
	(case !src_region
	   of nil => (pos0,pos0)
	    | r :: _ => r)

    fun peek_filename() = let val fp = !filepos
			      val (f1,_,_) = fp pos0
			  in  f1
			  end
    fun peek_region_string () : string =
	let val (p1,p2) = peek_region()
	    val fp = !filepos
	    val (f1,r1,c1) = fp p1
	    val (f2,r2,c2) = fp p2
	in concat[f1,":",Int.toString r1,".",Int.toString c1,"-",
		  Int.toString r2,".",Int.toString c2]
	end
    fun push_region p = (src_region := (p :: (!src_region));
			 if (!track)
			     then (tab ((length (!src_region)) - 1);
				   print "PUSH_REGION: ";
				   print (peek_region_string()); print "\n")
			 else ())
    fun pop_region () = let val _ = if (!track)
					then (tab ((length (!src_region)) - 1);
					      print "POP_REGION: ";
					      print (peek_region_string()); print "\n")
				    else ()
			in src_region := (tl(!src_region))
			end

    (* message format is compatible with Parser/errormsg.sml *)
    fun error_region_string() = ((peek_region_string()) ^ " Error: ")
    fun warn_region_string() = ((peek_region_string()) ^ " Warning: ")
    fun error_region() = (error_level := error_max(!error_level,Error);
			  print (error_region_string()))
    fun warn_region() = (error_level := error_max(!error_level,Warn);
			 print (warn_region_string()))
    fun error_region_with str = (error_region(); print str)
    fun warn_region_with str = (warn_region(); print str)

    fun dummy_type(context,str) = fresh_named_con(context,str)
    fun dummy_exp (context,str) =
	let val c = fresh_named_con(context,str)
	    val e = RAISE(c,EXN_INJECT("elab_fail",NEW_STAMP con_unit,RECORD[]))
	in (e,c)
	end


end

