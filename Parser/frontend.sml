(*$import Prelude TopLevel ErrorMsg TextIO SourceMap FRONTEND Join MLLrValsFun MLLexFun Ast Source Control LrParser Stats *)

(* Copyright 1996 by AT&T Bell Laboratories *)
(* frontend.sml *)


structure FrontEnd :> FRONT_END =
struct 

structure MLLrVals = MLLrValsFun(structure Token = LrParser.Token)
structure Lex = MLLexFun(structure Tokens = MLLrVals.Tokens)
structure MLP = JoinWithArg(structure ParserData = MLLrVals.ParserData
                            structure Lex=Lex
                            structure LrParser = LrParser)

open ErrorMsg

(* the following two functions are also defined in build/computil.sml *)
fun debugmsg  (msg : string) =
  let val printit = !Control.debugging
   in if printit then app Control.Print.say[msg, "\n"] else ();
      printit
  end

fun addLines lines = let val r = Stats.int("SourceLines") 
		     in  r := !r + lines
		     end

datatype 'a parseResult
  = EOF   (* end of file reached *)
  | ERROR (* parsed successfully, but with syntactic or semantic errors *)
  | ABORT (* could not even parse to end of declaration *)
  | SUCCESS of 'a

type 'a parser = Source.inputSource -> (int * string list * 'a) parseResult

val dummyEOF = MLLrVals.Tokens.EOF(0,0)
val dummySEMI = MLLrVals.Tokens.SEMICOLON(0,0)

fun parse (start, cleanup) (source as {sourceStream,errConsumer,interactive,
				       sourceMap, anyErrors,...}: Source.inputSource) =
  let val err = ErrorMsg.error source
      val complainMatch = ErrorMsg.matchErrorString source

      fun parseerror(s,p1,p2) = err (p1,p2) COMPLAIN s nullErrorBody

      val lexarg = {comLevel = ref 0,
                    sourceMap = sourceMap,
                    charlist = ref (nil : string list),
                    stringtype = ref false,
                    stringstart = ref 0,
                    err = err,
                    brack_stack = ref (nil: int ref list)}

(*      val doprompt = ref true *)
      val doprompt = ref false
      val prompt = ref (!Control.primaryPrompt)

      fun inputc_sourceStream _ = TextIO.input(sourceStream)

      exception AbortLex
      fun getline k =
        (if !doprompt
         then (if !anyErrors then raise AbortLex else ();
               Control.Print.say
                (if !(#comLevel lexarg) > 0
                    orelse !(#charlist lexarg) <> nil
                 then !Control.secondaryPrompt
                 else !prompt);
               Control.Print.flush();
               doprompt := false)
         else ();
         let val s = inputc_sourceStream k
          in 
(*
	      doprompt := ((String.sub(s,size s - 1) = #"\n")
                          handle _ => false);
*)
             s
         end)

      val lexer = 
        Lex.makeLexer (if interactive then getline 
                       else inputc_sourceStream) lexarg
      val lexer = LrParser.Stream.streamify lexer
      val lexer = LrParser.Stream.cons (start, lexer)
      val lexer' = ref(lexer)
      val lookahead = if interactive then 0 else 30

      fun oneparse () =
        let val _ = prompt := !Control.primaryPrompt
            val (nextToken,rest) = LrParser.Stream.get(!lexer') 

            val startpos = SourceMap.lastChange sourceMap
            fun linesRead() = SourceMap.newlineCount sourceMap 
                      (startpos, SourceMap.lastChange sourceMap)
         in (*if interactive then SourceMap.forgetOldPositions sourceMap 
              else ();*)
            if MLP.sameToken(nextToken,dummySEMI) 
            then (lexer' := rest; oneparse ())
            else if MLP.sameToken(nextToken,dummyEOF)
                 then EOF
                 else let val _ = prompt := !Control.secondaryPrompt;
                          val (result, lexer'') =
                            MLP.parse(lookahead,!lexer',parseerror,err)
			  val _ = addLines(linesRead()) 
                          val _ = lexer' := lexer''
			  val Ast.MarkTop(top, _) = result
		      in if !anyErrors then ERROR
			 else cleanup (linesRead(), top)
                      end 
        end handle LrParser.ParseError => ABORT
                 | AbortLex => ABORT
            (* oneparse *)
   in anyErrors := false; oneparse ()
  end


val dummyIMPL = MLLrVals.Tokens.IMPL(0,0)
val dummyINTER = MLLrVals.Tokens.INTER(0,0)
    
val parse_impl = parse (dummyIMPL,
			fn (lines, Ast.ImplTop (imports,dec)) => SUCCESS (lines,imports,dec)
			 | _ => ERROR)
    
val parse_inter = parse (dummyINTER,
			 fn (lines, Ast.InterTop (imports,specs)) => SUCCESS (lines,imports,specs)
			  | _ => ERROR)

end (* structure FrontEnd *)
