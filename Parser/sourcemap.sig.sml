(*$import Prelude *)

(* \section{Source locations}                                               *)
(*                                                                          *)
(* The goal of this interface is to map character positions to              *)
(* locations in source files, where a location is described in              *)
(* ``file-line-column'' format.                                             *)
(* The major type exported by this interface is [[sourcemap]], which        *)
(* maintains the mapping.                                                   *)
(* This way, most of a compiler can work with character positions, but we   *)
(* can use real source locations in error messages.                         *)
(*                                                                          *)
(* A \emph{region} represents a contiguous span of                          *)
(* source locations as seen by the compiler.                                *)
(* Because of preprocessing, any region could be spread out over multiple   *)
(* overlapping regions in the original source.                              *)
(*                                                                          *)
(* A source map is maintained as mutable state.                             *)
(* We create such a map by giving the initial character position, file      *)
(* name, line, and column number.                                           *)
(* Column numbers are obtained by counting characters from the beginning    *)
(* of the line; the first character on the line is deemed to be in          *)
(* column~1.                                                                *)
(* Tabs are given no special treatment.                                     *)
(*                                                                          *)
(* Character positions increase as the compiler moves through the source,   *)
(* and the lexer mutates the source map any time something interesting      *)
(* happens.                                                                 *)
(* The two interesting events are:                                          *)
(* \begin{itemize}                                                          *)
(* \item                                                                    *)
(* The lexer encounters a newline (changing the line number in the source   *)
(* file).                                                                   *)
(* \item                                                                    *)
(* The lexer encounters \verb+#line+ or its equivalent, changing the        *)
(* source coordinates.                                                      *)
(* By analogy with the \texttt{{\tt lcc}} implementation, I call this       *)
(* event a \emph{resynchronization}.                                        *)
(* A resynchronization must change the line number.                         *)
(* It may change the file name and column number; if not specified they     *)
(* default to the current file name and~1, respectively.                    *)
(* As suggested by John Reppy,                                              *)
(* a resynchronization can specify a line number of~0 (in order to make     *)
(* the numbering of the following line come out right).                     *)
(* \end{itemize}                                                            *)
(* Character positions must be nonnegative, and they must                   *)
(* increase in successive mutations of a single                             *)
(* [[sourcemap]] (where the initialization counts as a mutation).           *)
(*                                                                          *)
(* [[forgetOldPositions]] causes the sourcemap to discard information       *)
(* about positions already known to the source map.                         *)
(* Subsequent queries may refer only to new positions (which must still     *)
(* be larger than the old ones).                                            *)
(* The only reason to call [[forgetOldPositions]] is to avoid space leaks.  *)
(*                                                                          *)
(* [[lastChange]] returns the position of the last mutation, or the         *)
(* initial position if no mutations have taken place.                       *)
(*                                                                          *)
(* [[filepos]] and [[fileregion]] map character positions and regions       *)
(* back to the source level.                                                *)
(* If the null region is passed to [[fileregion]], it returns the empty list. *)
(* In any pair returned by [[fileregion]], the two source locations are     *)
(* guaranteed to have the same file name.                                   *)
(* [[newlineCount]] returns the number of newlines that occurred in the given *)
(* region.                                                                  *)
(*                                                                          *)
(* <sourcemap.sig>=                                                         *)
(* sourcemap.sig *)
(* <sig RCS log>=                                                           *)
(* 
 * changed ErrorMsg to use SourceMap to get source locations; only the
 * formatting is done internally
 *
 * added SourceMap structure
 *
 * .sig and .sml for sourcemap, source, and errormsg are derived from .nw
 * files.  to extract, try
 *   for base in sourcemap source errormsg
 *   do
 *     for suffix in sml sig
 *     do
 *       $cmd -L'(*#line %L "%F"*)' -R$base.$suffix $base.nw > $base.$suffix
 *     done
 *   done
 * where
 *   cmd=notangle
 * or
 *   cmd="nountangle -ml"
 *
 * At some point, it may be desirable to move noweb support into CM
 * *)
signature SOURCE_MAP = sig
  type charpos (* = int *)
  type 'a pair (* = 'a * 'a *)
  type region  (* = charpos pair *)
  val span : region * region -> region (* smallest region containing the two regions *)
  val nullRegion : region              (* left and right identity of span *)

  type sourceloc (* = {fileName:string, line:int, column:int} *)

  type sourcemap (* = opaque mutable *)
  val newmap  : charpos * sourceloc -> sourcemap
  val newline : sourcemap -> charpos -> unit
  val resynch : sourcemap -> 
                charpos * {fileName:string option, line:int, column:int option} -> unit
  val forgetOldPositions : sourcemap -> unit

  val filepos     : sourcemap -> charpos -> sourceloc
  val fileregion  : sourcemap -> region  -> sourceloc pair list
  val positions   : sourcemap -> sourceloc -> charpos list

  val lastChange  : sourcemap -> charpos
  val newlineCount: sourcemap -> region -> int
end

(*
 * $Log$
# Revision 1.2  2000/09/12  18:57:11  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/01/21  20:40:48  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  18:16:06  pscheng
# added the sig file
# 
 *)
