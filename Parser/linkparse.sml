structure LinkParse :> LINK_PARSE =
struct
  val error = fn s => Util.error "linkparse.sml" s

  val LinkParseDiag = Stats.ff("LinkParseDiag")
  fun msg str = if (!LinkParseDiag) then print str else ()

  val doNamedForm = Stats.tt "DoNamedForm"

  type filepos = SourceMap.charpos -> string * int * int
  type 'a parser = string * string -> (filepos * 'a) option

  val lexer_initial_position = Source.lexer_initial_position

  fun make_source s =
      let val instream = TextIO.openIn s
      in  (instream,
	   Source.newSource(s,1,instream,true,
			    ErrorMsg.defaultConsumer()))
      end

  fun parse (parser, cleanup) (what, filename) =
      let
	  val _ = msg ("===== Parsing " ^ what ^ " =====\n")
	  val (instream,src) = make_source filename
	  val fp = Source.filepos src
	  val fp = (fn pos => (fp pos handle _ => (filename,0,1)))
      in
	  case parser src
	    of FrontEnd.SUCCESS contents =>
		let val contents = cleanup contents
		    val _ = TextIO.closeIn instream
		in  SOME (fp,contents)
		end
	     | _ => (TextIO.closeIn instream; NONE)
      end

  fun tvscope_dec dec = (TVClose.closeDec dec; dec)
  fun named_form_dec dec = NamedForm.namedForm dec

  fun cleanup_impl dec =
      let val dec = tvscope_dec dec
	  val dec = if (!doNamedForm) then named_form_dec dec else dec
      in  dec
      end

  val parse_topdec = parse (FrontEnd.parse_impl, cleanup_impl)
  val parse_topspec = parse (FrontEnd.parse_inter, fn specs => specs)

  val parse_topdec = Stats.timer("Parsing", parse_topdec)
  val parse_topspec = Stats.timer("Parsing", parse_topspec)
end
