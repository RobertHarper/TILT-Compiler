(*$import FRONTEND Join MLLrValsFun MLLexFun Ast Source Control LrParser Stats *)

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

datatype parseResult
  = EOF   (* end of file reached *)
  | ERROR (* parsed successfully, but with syntactic or semantic errors *)
  | ABORT (* could not even parse to end of declaration *)
  | PARSE_IMPL of int * string list * Ast.dec
  | PARSE_INTER of int * string list * Ast.spec list

val dummyEOF = MLLrVals.Tokens.EOF(0,0)
val dummySEMI = MLLrVals.Tokens.SEMICOLON(0,0)

fun parse (source as {sourceStream,errConsumer,interactive,
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
      val lexer' = ref(LrParser.Stream.streamify lexer)
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
			 else case top of
				   Ast.ImplTop (imports,dec) => 
				    PARSE_IMPL(linesRead(),imports,dec)
			         | Ast.InterTop (imports,spec) => 
				    PARSE_INTER(linesRead(),imports,spec)
				 | _ => ERROR 
                      end 
        end handle LrParser.ParseError => ABORT
                 | AbortLex => ABORT
            (* oneparse *)
   in anyErrors := false; oneparse ()
  end

end (* structure FrontEnd *)


(*
 * $Log$
# Revision 1.9  99/02/17  20:31:05  pscheng
# *** empty log message ***
# 
# Revision 1.8  1998/04/06  21:19:40  pscheng
# update: Typeof_c, dependent arrow/record types
#
# Revision 1.7  1998/02/15  22:43:26  pscheng
# bootstrapping changes
#
# Revision 1.6  1998/02/01  01:27:59  pscheng
# Changes to facilitate bootstrapping:
#   Added ascription in various places
#   Split up files into signature and code
#
# Revision 1.5  1998/01/21  20:40:18  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.4  1997/11/19  16:48:48  pscheng
# changed timers to subtimers
#
# Revision 1.3  1997/10/21  18:06:56  pscheng
# added copies of files from ml-yacc
#
# Revision 1.2  97/07/02  22:03:04  jgmorris
# Modified syntax to allow for interfaces and implementations.
# 
 * Revision 1.3  1997/06/09 15:37:31  mael
 * Added parsing of interfaces
 *
 * Revision 1.2  1997/05/30 14:12:31  zdance
 * Added support for (*$import...*) and (*$include...*) directives.
 * Also changed the grammar to allow for (possibly) semicolon-separated sequences
 * at the top level declarations.
 *
 * Revision 1.1.1.1  1997/05/23 14:53:50  til
 * Imported Sources
 *
# Revision 1.1  97/03/24  13:02:18  pscheng
# added frontend
# 
 *)
