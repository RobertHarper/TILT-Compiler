structure LinkParse :> LINK_PARSE =
struct
  val error = fn s => Util.error "linkparse.sml" s

  val LinkParseDiag = Stats.ff("LinkParseDiag")
  fun msg str = if (!LinkParseDiag) then print str else ()

  val doNamedForm = Stats.tt "DoNamedForm"

  type filepos = SourceMap.charpos -> string * int * int
  type 'a parser = string * string -> (filepos * 'a) option

  fun make_source s =
      let val instream = TextIO.openIn s
      in  (instream,
	   Source.newSource(s,1,instream,true,
			    ErrorMsg.defaultConsumer()))
      end

  fun parse (name, parser, cleanup) (what, filename) =
      let
	  val _ = msg ("===== Parsing " ^ name ^ ": " ^ what ^ " =====\n")
	  val (instream,src) = make_source filename
	  val fp = Source.filepos src
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

  val parse_impl = parse ("unit",
			  FrontEnd.parse_impl,
			  cleanup_impl)
  val parse_inter = parse ("interface",
			   FrontEnd.parse_inter,
			   fn specs => specs)

  val parse_impl = Stats.timer("Parsing", parse_impl)
  val parse_inter = Stats.timer("Parsing", parse_inter)
end;
