(*$import ERROR ILUTIL Il *)
(* Stateful module holding the error state of the elaboration process *)

functor Error(structure IlUtil : ILUTIL)
   :> ERROR =
struct

    datatype ErrorLevel = NoError | Warn | Error
    type region = Ast.srcpos * Ast.srcpos
    structure Il = Il
    open Il IlUtil

    val error_level = ref NoError
    val src_region = ref ([] : region list)

    fun nofilepos (_ : SourceMap.charpos) = ("nofilepos",0,0)
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

    fun push_region p = src_region := (p :: (!src_region))
    fun pop_region () = src_region := (tl(!src_region))
    fun peek_region () = (hd(!src_region))

    fun peek_region_str () : string = let val (p1,p2) = (hd(!src_region))
					  val fp = !filepos
					  val (f1,r1,c1) = fp p1
					  val (f2,r2,c2) = fp p2
				      in f1 ^ ":" ^ 
					  ((Int.toString r1) ^ "." ^ (Int.toString c1)) ^ "-" ^ 
					  ((Int.toString r2) ^ "." ^ (Int.toString c2))
				      end handle _ => "unknown"
    fun error_region_string() = ("Error at " ^ (peek_region_str()) ^ ", ")
    fun warn_region_string() = ("Warning at " ^ (peek_region_str()) ^ ", ")
    fun error_region() = (error_level := error_max(!error_level,Error);
			  print (error_region_string()))
    fun warn_region() = (error_level := error_max(!error_level,Warn);
			 print (warn_region_string()))
    fun spaces n = 
	let fun loop 0 = []
	      | loop n = #" "::(loop (n-1))
	in String.implode(loop n)
	end
    val tab = spaces 10
    fun tab_region() = print tab


    fun dummy_type(context,str) = fresh_named_con(context,str)
    fun dummy_exp (context,str) =
	let val c = fresh_named_con(context,str)
	    val e = RAISE(c,EXN_INJECT("elab_fail",NEW_STAMP con_unit,RECORD[]))
	in (e,c)
	end


end

