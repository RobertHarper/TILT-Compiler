(*$import SOURCE TextIO PathNames *)

(* <source.sml>=                                                            *)
(* source.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Source : SOURCE =
struct

  type inputSource = {
        sourceMap: SourceMap.sourcemap,
        fileOpened: string,
        anyErrors: bool ref,
        errConsumer: PrettyPrint.ppconsumer,
        interactive: bool,
        sourceStream: TextIO.instream
      }

  fun say (msg : string) = Control.Print.say msg

  val lexer_initial_position = 2 (* position of first char according to ml-lex *)

  fun newSource(fileName,lineNum,sourceStream,interactive, errConsumer) =
      {sourceMap=SourceMap.newmap(lexer_initial_position, 
                                  {fileName=fileName, line=lineNum, column=1}),
       sourceStream=sourceStream,interactive=interactive,fileOpened=fileName,
       errConsumer=errConsumer,anyErrors=ref false}

  fun closeSource ({interactive=true, ...} : inputSource) = ()
    | closeSource ({sourceStream, ...}) = (
        (* app say ["[closing ", (Pathnames.trim fileName), "]\n"];*)
        TextIO.closeIn sourceStream handle IO.Io _ => ())

  fun filepos({sourceMap,...}: inputSource) pos = 
    let val {fileName, line, column} = SourceMap.filepos sourceMap pos
    in  (fileName, line, column)
    end

end (* structure Source *)

(*
 * $Log$
# Revision 1.2  98/01/21  20:40:47  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  14:12:36  pscheng
# added copy of SMLNJ parser files
# 
 *)
