(*$import Prelude TopLevel TextIO Source SourceMap FrontEnd LINKPARSE NamedForm TVClose ErrorMsg Util Stats *)


structure LinkParse :> LINK_PARSE =
struct
  val error = fn s => Util.error "linkparse.sml" s

  val doNamedForm = Stats.tt "DoNamedForm"

  type filepos = SourceMap.charpos -> string * int * int
  type 'a parser = string -> (int * filepos * string list * 'a) option
      
  fun make_source s = 
      let val instream = TextIO.openIn s
      in  (instream,
	   Source.newSource(s,1,instream,true,
			    ErrorMsg.defaultConsumer()))
      end

  fun parse (name, parser, cleanup) s =
      let
	  val (instream,src) = make_source s
	  val fp = Source.filepos src
      in
	  case parser src
	    of FrontEnd.SUCCESS (lines, imports, contents) =>
		let val contents = cleanup contents
		    val _ = TextIO.closeIn instream
		in  SOME (lines,fp,imports,contents)
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

  val parse_impl = parse ("implementation",
			  FrontEnd.parse_impl,
			  cleanup_impl)
  val parse_inter = parse ("interface",
			   FrontEnd.parse_inter,
			   fn specs => specs)
      
  val parse_impl = Stats.timer("Parsing", parse_impl)
  val parse_inter = Stats.timer("Parsing", parse_inter)
end;
