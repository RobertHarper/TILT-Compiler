signature LINKPARSE = 
    sig
	val parse_all : string -> ((SourceMap.charpos -> string * int * int) * Ast.dec)
    end

structure LinkParse : LINKPARSE =
    struct
	exception Parse of FrontEnd.parseResult
	
	fun make_source s = Source.newSource(s,0,TextIO.openIn s,true,
					     ErrorMsg.defaultConsumer())
	fun parse_help s = let val src = make_source s
			   in  (src,FrontEnd.parse src)
			   end
	fun parse_one s =
	    let val (src,thunker) = parse_help s 
	    in  (case thunker() of
		     (FrontEnd.PARSE dec) => (src,dec)
		   | (result) => raise Parse result)
	    end
	
	fun parse s =
	    let val (src,thunker) = parse_help s 
		fun loop () = 
		    (case thunker() of
			 FrontEnd.PARSE dec => dec :: (loop ())
		       | FrontEnd.EOF => []
		       | res => raise Parse res)
	    in  (src, (case loop() of
			   [dec] => dec
			 | decs => Ast.SeqDec decs))
	    end
	
	fun tvscope_dec dec = (TVClose.closeDec dec; dec)
	fun named_form_dec dec = NamedForm.namedForm dec
	    
	fun parse_all filename = 
	    let val (src,astdec) = parse filename
		val fp = Source.filepos src
		val astdec = tvscope_dec astdec
		val astdec = named_form_dec astdec
	    in (fp,astdec)
	    end
	val parse_all = Stats.timer("Parsing",parse_all)
    end


(*
structure X =
struct
  fun ppdec message dec =
    let val ppstream = PrettyPrint.mk_ppstream (ErrorMsg.defaultConsumer())
	val style = PrettyPrint.CONSISTENT
	val offset = 0
    in
      PrettyPrint.begin_block ppstream style offset;
      PrettyPrint.add_newline ppstream;
      PrettyPrint.add_newline ppstream;
      PrettyPrint.add_string ppstream message;
      PrettyPrint.add_newline ppstream;
      PPAst.ppDec ppstream (dec, 50);
      PrettyPrint.end_block ppstream
    end

  fun do_close dec =
    (ppdec "Declaration as parsed:" dec;
     TVClose.closeDec dec;
     ppdec "Declaration after type variable binding:" dec;
     dec)
end
*)
