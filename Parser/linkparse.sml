structure LinkParse =
struct
  exception Parse of FrontEnd.parseResult

  fun make_source s = Source.newSource(s,0,TextIO.openIn s,true,
				       ErrorMsg.defaultConsumer())
  local
      fun parse s = FrontEnd.parse (make_source s)
  in
      fun parse_one s =
	  case parse s () of 
	      FrontEnd.PARSE dec => dec
	    | result => raise Parse result

      fun tvscope_dec dec = (TVClose.closeDec dec; dec)
      fun named_form_dec dec = NamedForm.namedForm dec
  end
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
