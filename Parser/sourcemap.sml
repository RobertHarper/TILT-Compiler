(*$import Prelude SOURCEMAP TopLevel *)

(* I can imagine at least three implementations: one that doesn't           *)
(* support resynchronization, one that supports resynchronization only at   *)
(* column 1, and one that supports arbitrary resynchronization.             *)
(*                                                                          *)
(*                                                                          *)
(* \section{Implementation}                                                 *)
(* This implementation supports arbitary resynchronization.                 *)
(*                                                                          *)
(* <sourcemap.sml>=                                                         *)
(* sourcemap.sml *)
(* <RCS log>=                                                               *)
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
structure SourceMap : SOURCE_MAP = struct
  (* A character position is an integer.  A region is delimited by the        *)
  (* position of the start character and one beyond the end.                  *)
  (* It might help to think of Icon-style positions, which fall between       *)
  (* characters.                                                              *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  type charpos = int
  type 'a pair = 'a * 'a
  type region = charpos pair
  val nullRegion : region = (0,0)
  type sourceloc = {fileName:string, line:int, column:int}
  (* The empty region is conventional.                                        *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  fun span ((0,0), r) = r
    | span (r, (0,0)) = r
    | span ((l1, h1), (l2, h2)) = if l1 < h2 then (l1, h2) else (l2, h1)
  (* The representation is a pair of lists.                                   *)
  (* [[linePos]] records line numbers for newlines \emph{and}                 *)
  (* resynchronization.                                                       *)
  (* [[resynchPos]] records file name and column for resynchronization.       *)
  (* The representation satisfies these invariants:                           *)
  (* \begin{itemize}                                                          *)
  (* \item                                                                    *)
  (* The lists are never empty (initialization is treated as a resynchronization). *)
  (* \item                                                                    *)
  (* Positions decrease as we walk down the lists.                            *)
  (* \item                                                                    *)
  (* The last element in each list contains the smallest valid position.      *)
  (* \item                                                                    *)
  (* For every element in [[resynchPos]], there is a corresponding element in *)
  (* [[linePos]] with the same position.                                      *)
  (* \end{itemize}                                                            *)
  (* We could get even more clever and store file names only when they        *)
  (* differ, but it doesn't seem worth it---we would have to get very         *)
  (* clever about tracking column numbers and resynchronizations.             *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  type sourcemap = {resynchPos: (charpos * string * int) list ref,
                    linePos:    (charpos * int)          list ref}

  fun newmap (pos, {fileName, line, column}: sourceloc) : sourcemap =
    {resynchPos = ref [(pos, fileName, column)], linePos = ref [(pos, line)]}

  fun resynch ({resynchPos, linePos}: sourcemap) (pos, {fileName, line, column}) =
    let val curFile = #2 (hd (!resynchPos))
        fun thefile (SOME file) = if file = curFile then curFile else file
                                     (* pathetic attempt at hash-consing *)
          | thefile NONE        = #2 (hd (!resynchPos))
        fun thecol NONE     = 1
          | thecol (SOME c) = c
    in  resynchPos := (pos, thefile fileName, thecol column) :: !resynchPos;
        linePos := (pos, line) :: !linePos
    end
  (* Since [[pos]] is the position of the newline, the next line doesn't      *)
  (* start until the succeeding position.                                     *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  fun newline ({resynchPos, linePos}: sourcemap) pos =
    let val (_, line) = hd (!linePos)
    in  linePos := (pos+1, line+1) :: !linePos
    end

  fun lastChange({linePos, ...}: sourcemap) = #1 (hd (!linePos))
  (* A generally useful thing to do is to remove from the lists the initial   *)
  (* sequences of tuples                                                      *)
  (* whose positions satisfy some predicate:                                  *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  fun remove p ({resynchPos,linePos}: sourcemap) =
    let fun strip (l as (pos, _   )::rest) = if p pos then strip  rest else l
          | strip [] = []
        fun strip'(l as (pos, _, _)::rest) = if p pos then strip' rest else l
          | strip'[] = []
    in  (strip'(!resynchPos), strip (!linePos))
    end
  (* We find file and line number by linear search.                           *)
  (* The first position less than [[p]] is what we want.                      *)
  (* The initial column depends on whether we resynchronized.                 *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  fun column ((pos, file, col), (pos', line), p) =
    if pos = pos' then p - pos + col else p - pos' + 1

  fun filepos smap p : sourceloc =
      let val (files, lines) = remove (fn pos : int => pos > p) smap
          val xx as (_, file, _) = hd files
          val yy as (_, line)    = hd lines
      in  {fileName = file, line = line, column = column(xx, yy, p)}
      end
  (* Searching regions is a bit trickier, since we track file and line        *)
  (* simultaneously.  We exploit the invariant that every file entry has a    *)
  (* corresponding line entry.                                                *)
  (* We also exploit that only file entries correspond to new regions.        *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  fun fileregion smap (lo, hi) =
    if (lo,hi) = nullRegion then [] else
    let exception Impossible
        fun gather((p, file, col)::files, (p', line)::lines, region_end, answers) =
          if p' <= lo then (* last item *)
            ({fileName=file, line=line, column=column((p, file, col), (p', line), lo)}, 
             region_end) :: answers
          else
            if p < p' then
              gather((p, file, col)::files, lines, region_end, answers)
            else (* p = p'; new region *)
              gather(files, lines, end_of (p, hd files, hd lines), 
                   ({fileName = file, line = line, column = col}, region_end) :: answers)
          | gather _ = raise Impossible
        and end_of(lastpos, xx as (p, file, col), yy as (p', line)) = 
               {fileName=file, line=line, column=column(xx, yy, lastpos)}
        val (files, lines) = remove (fn pos : int => pos >= hi andalso pos > lo) smap
        val _ = if null files orelse null lines then raise Impossible else ()
        val answer = gather(files, lines, end_of(hi, hd files, hd lines), [])
        fun validate(({fileName=f,  line=l,  column=c}, 
                      {fileName=f', line=l', column=c'}) :: rest) = 
              if f = f' andalso (l' > l orelse (l' = l andalso c' >= c)) then
                validate rest 
              else 
                raise Impossible
          | validate [] = ()
    in  validate answer; answer
    end
  (* [[validate]] checks the invariant that single regions occupy a           *)
  (* single source file and that coordinates are nondecreasing.               *)
  (* We have to be careful not to remove the entry for [[lo]] when            *)
  (* [[pos = hi = lo]].                                                       *)
  (*                                                                          *)
  (*                                                                          *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  fun positions({resynchPos,linePos}: sourcemap) (src:sourceloc) =
    let exception Unimplemented
    in  raise Unimplemented
    end
  (* When discarding old positions, we have to be careful to maintain the     *)
  (* last part of the invariant.                                              *)
  (*                                                                          *)
  (* <toplevel>=                                                              *)
  fun forgetOldPositions ({resynchPos, linePos} : sourcemap) =
    let val r as (p,  file, col) = hd (!resynchPos)
        val l as (p', line)      = hd (!linePos)
    in  linePos := [l];
        resynchPos := [if p = p' then r else (p', file, 1)]
    end
  (* <toplevel>=                                                              *)
  fun newlineCount smap (lo, hi) =
    let val (hifiles, hilines) = remove (fn pos : int => pos >= hi andalso pos > lo) smap
        val (lofiles, lolines) = remove (fn pos : int =>                   pos > lo) smap
    in  length hilines - length hifiles - (length lolines - length lofiles)
    end
end

(*
 * $Log$
# Revision 1.4  2001/12/13  16:32:48  swasey
# *** empty log message ***
# 
# Revision 1.3  2000/09/12  18:57:12  swasey
# Changes for cutoff compilation
# 
# Revision 1.2  98/01/21  20:40:49  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  14:12:37  pscheng
# added copy of SMLNJ parser files
# 
 *)
