(*$import Prelude SourceMap PrettyPrint TextIO *)

(* <source.sig>=                                                            *)
(* source.sig
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)
(* <source.sig>=                                                            *)
signature SOURCE =
  sig
    type inputSource = {
        sourceMap: SourceMap.sourcemap,
        fileOpened: string,
        interactive: bool,
        sourceStream: TextIO.instream, 
        anyErrors: bool ref,
        errConsumer: PrettyPrint.ppconsumer
      }

    val newSource : (string * int * TextIO.instream * bool * PrettyPrint.ppconsumer)
          -> inputSource
    val closeSource: inputSource -> unit
    val filepos: inputSource -> SourceMap.charpos -> string * int * int

  end
(* The [[fileOpened]] field contains the name of the file that was opened   *)
(* to produce a particular [[inputSource]].                                 *)
(* It is used only to derive related file names.                            *)
(* (For an example, see [[CompileF.codeopt]] and [[CompileF.parse]] in      *)
(* \texttt{build/compile.sml}.)                                             *)
(*                                                                          *)
(* [[newSource]] has some old warts build in.  It takes as argument a       *)
(* file and line number, and it assumes column~1.  The reason we don't      *)
(* simply pass a [[SourceMap.sourcemap]] is that we have to hide the        *)
(* awful truth about the beginning position according to ml-lex (it's~2).   *)
(* That position, and therefore the creation of the source map, are         *)
(* encapsulated inside [[newSource]].                                       *)
(*                                                                          *)
(* [[filepos]] is kept around for historical reasons, to avoid having to    *)
(* change lots of code elsewhere in the compiler; it wraps a                *)
(* call to [[SourceMap.filepos]] and massages the return type.              *)
(* It probably should be eliminated, but then somebody would have to fix    *)
(* all those call sites.                                                    *)
(*                                                                          *)
(* <source.sig>=                                                            *)

(*
 * $Log$
# Revision 1.4  2001/12/13  16:32:48  swasey
# *** empty log message ***
# 
# Revision 1.3  2000/09/12  18:57:11  swasey
# Changes for cutoff compilation
# 
# Revision 1.2  98/02/01  01:28:15  pscheng
# Changes to facilitate bootstrapping:
#   Added ascription in various places
#   Split up files into signature and code
# 
# Revision 1.1  1998/01/21  20:40:47  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  18:16:05  pscheng
# added the sig file
# 
 *)
