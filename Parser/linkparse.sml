(*$import FrontEnd LINKPARSE NamedForm TVClose ErrorMsg Util Stats *)


structure LinkParse :> LINK_PARSE =
struct
  val error = fn s => Util.error "linkparse.sml" s

  fun parseError fileName expected =
      error ("File " ^ fileName ^ " could not be parsed as an " ^ expected ^ " file")

  val doNamedForm = Stats.tt "DoNamedForm"

  local
    fun make_source s = 
	let val instream = TextIO.openIn s
	in  (instream,
	     Source.newSource(s,1,instream,true,
			      ErrorMsg.defaultConsumer()))
	end

    fun parse s = let val (instream,src) = make_source s
		  in  (instream,Source.filepos src, FrontEnd.parse src)
		  end
		  
    fun tvscope_dec dec = (TVClose.closeDec dec; dec)
    fun named_form_dec dec = NamedForm.namedForm dec
  in
    type filepos = SourceMap.charpos -> string * int * int
    fun parse_impl s =
      case parse s of 
	(ins,fp,FrontEnd.PARSE_IMPL (lines,imports,dec)) => 
	  let val dec = tvscope_dec dec
	      val dec = if (!doNamedForm) then named_form_dec dec else dec
	      val _ = TextIO.closeIn ins
	  in (lines,fp,imports,dec)
	  end
      | (ins,_,result) => (TextIO.closeIn ins; parseError s "implementation")
    fun parse_inter s =
      case parse s of 
	(ins,fp,FrontEnd.PARSE_INTER (lines,includes,specs)) => 
	    (TextIO.closeIn ins; (lines,fp,includes,specs))
      | (ins,_,result) => (TextIO.closeIn ins; parseError s "interface")
  end

  val parse_impl = Stats.timer("Parsing", parse_impl)
  val parse_inter = Stats.timer("Parsing", parse_inter)
end;



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
