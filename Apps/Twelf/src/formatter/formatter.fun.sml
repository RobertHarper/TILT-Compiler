(*
% ForML Version 0.6 - 25 January 1993 - er@cs.cmu.edu
%************************************************************************
{\bf File {\tt formatter.fun} defines the functor {\tt Formatter}
  with the formatting and printing routines.}
%************************************************************************
*)
functor Formatter(): FORMATTER =
   struct
(*
\subsection{Setting default values}
*)
      val Indent  = ref 3
      and Skip    = ref 1
      and Blanks  = ref 1
      and Pagewidth = ref 80

      val Bailout = ref true
      val BailoutIndent = ref 0
      val BailoutSpot = ref 40
(*
%************************************************************************
\subsection{Auxiliary functions} 

A collection of miscellaneous functions which come in handy in different
places.

\subsubsection{String functions}
The {\tt Spmod} function is used when {\tt Bailout} is active.
*)
      local
         fun Spaces' 0 s = s
          |  Spaces' n s = Spaces' (n-1) (s^" ")
         fun Spaces n = if n>0 then Spaces' n "" else ""
         fun Newlines' 0 s = s
          |  Newlines' n s = Newlines' (n-1) (s^"\n")
         fun Newlines n = if n>0 then Newlines' n "" else ""
      in
        val Sp = Spaces (* return a number of spaces *)
        fun Spmod n = Spaces (n mod (!Pagewidth))
        val Nl = Newlines (* return a number of newlines *)
        fun Np() = "\n\012\n" (* CTRL_L == "\012" *)
      end 



(*
\subsubsection{Arithmetic functions}
*)
fun Max(x,y) = if (x:int)>y then x else y
fun sumpair ((a,b),(c,d)) = ((a:int)+c,(b:int)+d)

(*
\subsubsection{Pair functions}
*)
fun fst(a,b) = a and snd(a,b) = b

(*
%*************************************************************************
\subsection{The datatype {\ml format}}
The datatype {\ml format} specifies the data structure into which abstract
syntax trees are unparsed and which ---ultimately--- is to be output by the
printing routines.
In order to simplify the formatting, we precompute the
minimum and maximum width of the printed format for each node in the format
tree. These numbers are independent of the actual page width (but they take
the actual and default indentation width into account).
*)
datatype mode= Hori | Vert       (* are we in horizontal or vertical mode? *)
type width = int * int           (* the minimum/maximum width of boxes *)
type widthmode = mode * mode     (* remember mode in which minimum/maximum was gotten *)
datatype format =
         Str of int * string                    (* length, string *)
     |   Brk of int * int                       (* blanks, indent *)
     |   Dbk                                    (* Default Break *)
     |   Ebk                                    (* Empty Break *)
     |   Hbx of width * int * format list       (* Width, blanks, ... *)
     |   Vbx of width * int * int * format list (* Width, indent, skip, ... *)
     |   Hvx of (width * widthmode) * int * int * int * format list 
                                                (* Width, blanks, indent, skip, ... *)
     |   Hov of (width * widthmode) * int * int * int * format list
                                                (* Width, blanks, indent, skip, ... *)
(*
The function {\ml Width0} extracts the minimum and maximum width
of {\ml formats}.
The argument {\ml m} is the current mode in effect, {\ml b} is the
horizontal blanks and {\ml i} is the indent currently in effect.
These are used to determine the width of breaks and default breaks.
*)
fun Width0(m,   b,i, Str(n,_)) = (n,n)
 |  Width0(Hori,b,i, Brk(m,_)) = (m,m)
 |  Width0(Vert,b,i, Brk(_,n)) = (n,n)
 |  Width0(Hori,b,i, Dbk)      = (b,b)
 |  Width0(Vert,b,i, Dbk)      = (i,i)
 |  Width0(m,b,i, Ebk)      = (0,0)
 |  Width0(m,b,i, Vbx((min,max),_,_,_))      = (min,max)
 |  Width0(m,b,i, Hbx((min,max),_,_))        = (min,max)
 |  Width0(m,b,i, Hvx(((min,max),_),_,_,_,_)) = (min,max)
 |  Width0(m,b,i, Hov(((min,max),_),_,_,_,_)) = (min,max)

fun Width fmt = Width0(Hori,!Blanks,!Indent,fmt)

val Unused = ~9999  (* a bad value to mark unused arguments of Width0 *)
(*
{\bf Caution:}
The function {\ml Width} assumes horizontal mode.
This should only make a difference, if you are looking at a break.

{\bf Improvements:}
% At the moment, breaks are denoted by their indentation and
% skip values which are used for vertical mode.
% We might also want to add a third integer---the horizontal tab to be used for
% this break in horizontal mode.
The use of {\em negative} numbers.
However %---particular in the vertical case---
this would need an appropriate support by
the output device.


%   We also can use an auxiliary function that preprocesses format lists to deal
%   with separators. Specifically:
%   \begin{itemize}
%     \item if a separator occurs in the middle of the box, its content is
%       directly inserted into the box
%     \item however, if the separator is the last item in the box, it will be`
%       suppressed
%   fun preprocess(res,[Sep(_)]) = rev res
%     | preprocess(res, Sep(sl)::t) = preprocess((rev sl) @ res, t)
%     | preprocess(res,h::t) = preprocess(h::res, t)
%     | preprocess(res,nil) = rev res
%
%*************************************************************************
\subsection{Constructing a {\ml format}-structure}

The format structure outlined above is very minimal. In practice we
want to simplify the handling of the default cases,
and we also want to have an automatic calculation of the minimum and
maximum widths for the formats.

\subsubsection{Functions to determine the width of {\ml formats}}

Let us first tackle {\bf vertical boxes}.
We start out by defining an auxiliary function
{\ml vlistWidth'} of the form:
\begin{ml}
     vlistWidth'(i,format,(tmin,tmax),(cmin,cmax))
\end{ml}
where:
\begin{description}
  \item[{\ml i}] - horizontal tab to be used for breaks
  \item[{\ml format}] - format-list of which to determine horizontal weight
  \item[{\ml (tmin,tmax)}] - minimum/maximum as determined so far
  \item[{\ml (cmin,cmax)}] - current minimum and maximum of ``group''
                             where group is a list of formats up to but not
                              including a break or to the end of the list
\end{description}
And this is how the function works:
\begin{itemize}
  \item at the end of the list: the widest entry is either 
        the last group or the maximum up to the last group
  \item at a break we compute the widest entry from the last group and
        the last maximum.
        The new group will start out having the width of the break
  \item for all other list entries, we add their width to the width
        of the current group
\end{itemize}
The function that actually determines the width of vertical boxes
then simply starts the auxiliary function
 with a total maximum width of the list so far as (0,0) and the width in
   the current group so far as (0,0).
*)
local
   fun vlistWidth'(i,nil,(totmin,totmax),(tmmin,tmmax)) =
                  (Max(totmin,tmmin), Max(totmax,tmmax))
    |  vlistWidth'(i,Dbk::t, (totmin,totmax), (tmmin,tmmax)) = 
                    vlistWidth'(i,t, (Max(totmin,tmmin),Max(totmax,tmmax)),
                                Width0(Vert,Unused,i,Dbk))
    |  vlistWidth'(i,(b as (Brk(_)))::t, (totmin,totmax), (tmmin,tmmax)) = 
                    vlistWidth'(i,t, (Max(totmin,tmmin),Max(totmax,tmmax)),
                                Width0(Vert,Unused,i,b))
    |  vlistWidth'(i,x::t, (totmin,totmax), (tmmin,tmmax)) = 
                    vlistWidth'(i,t,(totmin,totmax),
                                sumpair((Width0(Vert,Unused,i,x)),
                                         (tmmin,tmmax)))
in
   fun vlistWidth(l,indent) = vlistWidth'(indent,l,(0,0),(0,0))
end
(*
Now to the purely {\bf horizontal boxes}:
these are pretty easy --- we merely sum up the widths of all entries.
However, we also need to take into account the ``default width'' of
horizontal tabs at the time, which we need to provide as an argument
to the {\ml Width0} function.
*)
fun hlistWidth(l,blanks) = 
              List.foldr (fn (fmt,(x,y)) =>
                        sumpair(Width0(Hori,blanks,Unused,fmt),(x,y)))
                    (0,0) l 
(*
When we have a box that can be treated as a 
{\bf horizontal-or-vertical box}, we need to take both, a horizontal
   {\ml format} and a vertical {\ml format} into account.\\
%{\bf Caution:}
Note that we cannot assume assume that the horizontal list will
always turn out to give the maximum width, nor can the vertical
   list be expected to give the minimum
 (e.g.\ if the indent
   for vertical lists is bigger than the horizontal tab for horizontal
   lists).
Thus we do keep track
of which mode (horizontal or vertical) corresponds to the {\it min}
entry, and we will always use the horizontal mode for the {\it max}
entry (if we have enough space left in the page we will always prefer to
use horizontal mode over vertical mode).
*)
fun hovlistWidth(l,blanks,indent) =
              let val (vmin,vmax) = vlistWidth(l,indent)
                  and (hmin,hmax) = hlistWidth(l,blanks)
                  val (min,mmode) = if vmin<hmin then (vmin,Vert)
                                                 else (hmin,Hori)
                in 
                 ( (min,hmax), (mmode,Hori) )
              end
(*
Lastly we have to treat {\bf horizontal-vertical boxes}, where each break can
{\em individually} 
be horizontal or vertical in such a manner as to use as
much of the linewidth as possible.
Since we do not know the margin width in effect when the box is
printed out, we can only take the values
   computed for horizontal-or-vertical boxes as maximum and
minimum boundaries for horizontal-vertical boxes.
In practice we would expect horizontal-vertical boxes to ``fill out'' more of
the available linewidth and thus use fewer lines than
horizontal-or-vertical boxes.
By the way: a horizontal-vertical box that contains just one break should
always behave exactly like the corresponding horizontal-or-vertical box.
*)
val hvlistWidth = hovlistWidth

(*
\subsubsection{Constructing the actual {\ml format}-structure}

The {\ml format} structure that we defined above is very basic.
Most of the time we will want to use default boxes.
   We define several functions to perform this syntactic sugaring
   for us.
As we put the boxes together, we compute their maximum and minimum widths
with the functions defined above.

{\bf Improvements:}
In a later version we might also want to have default boxes with an
indentation of 0 that would be suitable for indicating corresponding
breakpoints.

Two notes:
 we take the length of the string to be its print length, and
 we can ``emulate'' CAML's {\ml V1box}es by starting a vertical box with a 
break.  This ensures that the first item is indented as much as all the others.
*)

val Break    = Dbk
fun Break0 b i = Brk(b,i)
fun String s = Str(size s,s)
fun String0 i s = Str(i, s)
val Space    = Str(1, Sp 1)
fun Spaces n = Str(n, Sp(n))
fun Newline()  = Str(0, Nl 1)
fun Newlines n = Str(0, Nl(n))
fun Vbox l = Vbx( vlistWidth(l,(!Indent)), (!Indent), (!Skip), l) 
and Vbox0  i s l = Vbx( vlistWidth(l,i), i, s, l) 
and Hbox   l = Hbx( hlistWidth(l,(!Blanks)), (!Blanks), l) 
and Hbox0  b l = Hbx( hlistWidth(l,b), b, l) 
and HVbox  l = Hvx( hvlistWidth(l,(!Blanks),(!Indent)), 
                       (!Blanks), (!Indent), (!Skip), l) 
and HVbox0 b i s l = 
               Hvx( hvlistWidth(l,b,i), b, i, s, l) 
and HOVbox l =  Hov( hovlistWidth(l,(!Blanks),(!Indent)), 
                        (!Blanks), (!Indent), (!Skip), l) 
and HOVbox0 b i s l =
                Hov( hovlistWidth(l,b,i), b, i, s, l) 

fun Newpage() = Str(0, Np())

(*
%***********************************************************************
\subsection{Printing a {\ml format}-structure}

All ``printing'' functions other than the top-level one return the
following:
\begin{itemize}
  \item a number, which is the actual print width of the expression.
      This number is needed to determine the available print-width for
       subsequent expressions
  \item the string itself to be printed --- this is not necessary, however:
           a real print function of type
          {\ml string -> unit} could be used instead!
\end{itemize}

For efficiency reasons all the printing functions have been made
tail-recursive. Thus they take on an additional argument {\tt res}, a list
of the strings output so far in reverse order (i.e.\ what was output last is
at the head of the list), and these functions then also return such a string
list as a result, rather than just a string.

First let us define some
auxiliary printing functions
which provide printing for the different types of boxes.
We will provide the functions starting with the more difficult ones and
then getting to the easy ones---so relax!

{\bf Improvements:}
We could additionally return the print width of the last actual line that
was printed. This would give surrounding boxes better formatting control
since they would know more about what ``inner boxes'' exactly do.
See also several Caution remarks in this section.

\subsubsection{Printing a horizontal-vertical box}
For summing the maximum widths of the elements of a format list we employ the
auxiliary function {\ml summaxwidth}. Since the lists of which we want to
determine the maximum width do not contain breaks, all but the last
 argument to the {\ml Width0} function is actually irrelevant.
*)
  fun summaxwidth l = 
      (List.foldr (fn (fmt,ysum) =>
	      let val (_,y) = Width0(Hori,Unused,Unused,fmt)
	      in y + ysum end)
             0
             l)
(*
Let us now define a helper function for horizontal-vertical boxes:
\begin{ml}
     gh(curr_group, hvbox_list, res)
\end{ml}
where
\begin{description}
  \item[{\ml curr\_group}] - contains elements of the ``current'' group
  \item[{\ml hvbox\_list}] - is the formats list of the horizontal-vertical
                             box
\end{description}
This function determines the groups of the {\ml hvbox\_list} where each group
reaches up to (but not including) a break or else to the end of the list.
We want to get a list with elements of the form
\begin{ml}
    (width_of_group, [group elements], Break_after_End_of_Group)
\end{ml}
At the end of the list there might not be a break, so that the break at the
end of the last group becomes the pseudo-break {\ml Ebk}.

For our grouping function we distinguish the following cases:
\begin{itemize}
  \item we are at the end of the list and there is no ``open'' group to be
    terminated.
  \item we are at the end of the list and need to terminate an open group
    with {\ml Ebk}
  \item a break has occured, in which case we need to terminate the
    group with that break
  \item otherwise the first element is collected into the currently open
    group
\end{itemize}
*)
   
fun gh(nil,nil,_) = nil
  | gh(cg,nil,res) = rev ((summaxwidth cg,cg,Ebk)::res)
  | gh(cg,(Dbk::t),res) = gh(nil,t,(summaxwidth cg,cg,Dbk)::res)
  | gh(cg,((b as (Brk(_,_)))::t),res) =
                  gh(nil,t,(summaxwidth cg,cg,b)::res)
  | gh(cg,(h::t),res) = gh(cg@[h],t,res)
(*
Finally here comes the function {\ml pphv} to print a 
horizontal-vertical box. The format is:
\begin{ml}
  pphv(margin,leftindent,blanks,indent_step,skip_step,max_prlen,cur_hw,lastbreak,grlilst,res)
\end{ml}
where
\begin{description}
  \item[{\ml margin}] - is the position of rightmost column in which to print
  \item[{\ml leftindent}] - the leftmost column of the current
         horizontal-vertical box
  \item[{\ml blanks}] - the number of blanks to print for a horizontal tab
  \item[{\ml indent\_step}] - the indent tab to be used for vertical breaks
  \item[{\ml skip\_step}] - the vertical tab to be used for breaks
  \item[{\ml max\_prlen}] - the maximum horizontal width into which we have
       printed so far
  \item[{\ml cur\_hw}] - the current printwidth of the present horizontal
       line
  \item[{\ml lastbreak}] - the break that terminated the previous group
         ({\ml Ebk)} if no previous group exists)
  \item[{\ml grlist}] - the list containing the groups which are left to print
\end{description}
And here is the algorithm:
Instead of printing a list of formats (as the other box-printing functions do)
we print a filtered list of ``groups'' each of which is terminated
by a break (possibly the pseudo-break {\ml Ebk}). This break will
not be immediately printed, but handed over to the printing of the
next group which ---upon knowing the print width of itself, and the
available space on the page--- determines
whether to interpret the break as a vertical or a horizontal break.
We thus get:
\begin{itemize}
   \item if we reach the end of all groups in the horizontal-vertical box,
         we are finished and return the maximum actual
         printwidth of the box.\\
         {\bf Caution:} Do we print the last break command inside the box after
          the end of the last group?
         If so, we do not know whether to print it as a horizontal or as a
         vertical break, because no text is following it.
         Thus {\em if the last break in a horizontal-vertical box is the last
         token in that box, it will not be printed at all!}\\
         {\bf Caution:} The maximum box width returned by the printing
         function clearly can be much wider
         than the length of the last line in the horizontal-vertical box.
         If the enclosing box just continues printing from there, results
         might not be as expected!
   \item if there is at least one more group to print:
         \begin{itemize}
            \item if this group fits within the pagewidth, i.e.\ 
               {\it left indentation of vertical box}$+${\it current
                 printedwidth}$+${\it potential horizontal width of last
                 breakpoint}$+${\it group printwidth}$\leq${\it pagewidth}
              
               (Note that if the last
               break was a pseudo-break {\ml Ebk} we should always
               pretend, that the group fits on the page, since we have
               nothing to break on. In particular this will be the case
               at the beginning of the box.)
             \begin{itemize}
                \item then interpret the last break as a horizontal break 
                      and print it. Increase the horizontal column count
                      accordingly
                \item otherwise interpret the break as a vertical break
                      and print it. Adjust the horizontal column count to
                      the current left margin. Determine the new maximum
                      width of the box.
             \end{itemize}
            \item print all the elements of the group from the new
                  horizontal column
            \item now print the remaining groups in the list, passing on
               the breakpoint that terminated the group just printed
            \item the string to be printed is the ``concatenation'' from
               the individual printing routines, and its maximum width
               is returned by the last printing routine
         \end{itemize}
\end{itemize}
*)
fun pphv(mw,li,bl,is,ss,mp,ch,lb,nil,res)= (Max(mp,ch),res)
 |  pphv(mw,li,bl,is,ss,mp,ch,lb,((gpwdth,flist,brk)::t),res) =
    let val (ch1,s1,mp) =
        (* horizontal width, string to print, max print width *)
            if (lb=Ebk)
            orelse ( li+ch+(fst(Width0(Hori,bl,Unused,lb)))+gpwdth
                     <= mw )
            then (* OK - group fits within page or has to fit:
                    horizontal break *)
            let val (n,s)=print'p(mw,li,bl,is,ss,Hori,lb,res)
            in (ch+n,s,mp) end
            else (* group will not fit: vertical break. 
                    Was last line of maximum width? *)
            let val (n,s)=print'p(mw,li,bl,is,ss,Vert,lb,res) in
                (n,s,Max(mp,ch))
            end
 (* Now print the elements of the group using default for horizontal tabs *) 
       val (n2,s2) = pph(mw,(li+ch1),bl,is,ss,flist,0,s1)
       (* Now print rest of horizontal-vertical box *)
    in pphv(mw,li,bl,is,ss,mp,(ch1+n2),brk,t,s2) end
(*
%{\bf Improvements:}
%Instead of employing the default value {\ml Blanks} use the number of
%blanks specified in the {\ml Hvbox} by adding an extra argument to this
%function.

\subsubsection{Printing a vertical box}
To print a vertical box
we have the function.
\begin{ml}
 ppv(margin,leftindent,curindent,indent_step,skip_step,max_prlen,grlen,formlist,res)
\end{ml}
where
\begin{description}
  \item[{\ml margin}] - the rightmost column to print
  \item[{\ml leftindent}] - the leftmost indentation column of the vertical box
  \item[{\ml curindent}] - the current indentation ($\geq${\ml leftindent}) to
        be passed on to ``lower'' boxes
  \item[{\ml indent\_step}] - the horizontal tab to be used for breaks
  \item[{\ml skip\_step}] - the vertical tab to be used for breaks
  \item[{\ml max\_prlen}] - the maximum horizontal width in
                     which we have printed so far
  \item[{\ml grlen}] - the length of the current (horizontal) group,
           where a group reaches up to but not including a break
           or to the end of the list 
  \item[{\ml formlist}] - the list of forms in the vertical box to be printed 
\end{description}

And this is how the algorithm works:
\begin{itemize}
  \item if we reached end of the vertical box --- we are done,
        and we return the maximum width.\\
        {\bf Caution:} unless we do an immediate vertical break now, our
        current horizontal position could very well be less than the
        maximum width, contrary to what the surrounding box might believe!
  \item if we reached a break --- then the previous group is closed,
        and we start a new group:
    \begin{itemize}
      \item print vertical break, using the original indentation level
      \item print the rest of box with 
      \begin{itemize}
            \item a new overall width, also including data from the last group
            \item starting the new group with the width of the break
            \item increasing the indentation level currently for rest of group
      \end{itemize}
      \item this will finally return the overall (max) width of the vertical 
            list
    \end{itemize}
  \item else we remain inside of a group, and
     \begin{itemize}
        \item print the first element of the list in vertical mode,
               using the current indentation level
        \item  print the rest of list with increased current indentation level
               and increased group length
        \item this will finally return the overall (max) width of 
              the vertical box
     \end{itemize}
\end{itemize}
*)
and ppv(mw,li,ci,bl,is,ss,max,gw,nil,res) = 
                          (Max(max,gw),res)
  | ppv(mw,li,ci,bl,is,ss,max,gw, Dbk::t,res) =
        let val (n,s)   = print'p(mw,li,bl,is,ss,Vert,Dbk,res)
        in ppv(mw,li,(li+n),bl,is,ss,Max(max,gw), n,t, s) end
  |  ppv(mw,li,ci,bl,is,ss,max,gw,(b as (Brk(_,_)))::t,res) =
        let val (n,s)   = print'p(mw,li,bl,is,ss,Vert,b,res)
        in ppv(mw,li,(li+n),bl,is,ss,Max(max,gw), n,t,s) end
  |  ppv(mw,li,ci,bl,is,ss,max,gw,h::t,res) =
          let val (n,s)   = print'p(mw,ci,bl,is,ss,Vert,h,res)
          in ppv(mw,li,(ci+n),bl,is,ss,max,(gw+n),t,s) end

(*
\subsubsection{Printing a horizontal box}
A function to print a horizontal box (this is getting easier all the time)
\begin{ml}
  pph(margin, indentation, blanks, indent_step, skip_step, formlist, nres, res)
\end{ml}
   where
\begin{description}
    \item[{\ml margin}] - the rightmost column to print
    \item[{\ml indentation}] - the current indentation column
    \item[{\ml blanks}] - the horizontal tab to be used for breaks
    \item[{\ml indent\_step}] - the indentation for vertical breaks
    \item[{\ml skip\_step}] - the vertical skip
    \item[{\ml formlist}] - a list of forms in the horizontal box to be printed
    \item[{\ml nres}] - here we are also ``building up'' the format width, and
         thus we hand over the current horizontal width in {\tt nres}.
\end{description}
The algorithm:
\begin{itemize}
  \item the empty list: its width is 0 and nothing is to be printed 
  \item  a compound list: first print the first element in horizontal mode.
    (We also set the vertical tab to zero, which is unnecessary, however.)
    The current indentation is increased by the printwidth of the first element.
    Then we print the rest of the list at this new indentation level.
    The length of everything we printed is the sum of the head-length
    and tail-length.
\end{itemize}
*)
and pph(mw,id,bl,is,ss,nil,nres,sres) = (nres,sres)
  | pph(mw,id,bl,is,ss, h::t,nres,sres) = 
          let val (n,s)   = print'p(mw,id,bl,is,ss,Hori,h,sres)
          in pph(mw,(id+n),bl,is,ss,t,n+nres,s) end
(*
\subsubsection{Putting it all together: printing a {\ml format}}
Now to the main printing routine {\ml print'p} --- this
one is good for all {\ml format}s:
\begin{ml}
 print'p(marginwidth, indentation, blanks, indent_step, skip_step, mode, form, res)
\end{ml}
 where
\begin{description}
    \item[{\ml marginwidth}] - the rightmost column into which can be printed
                --- this does not change and could be turned into a ref value,
               were it not for the fact that if {\tt Bailout} is active, it may
               be dynamically increased in steps of {\tt Pagewidth}.
    \item[{\ml indentation}] - the current column which forms 
            the leftmost edge of printing
    \item[{\ml blanks}] - the number of blanks to print for a horizontal tab
    \item[{\ml indent\_step}] - the ``horizontal tab'' by which to indent boxes
    \item[{\ml skip\_step}] - the `vertical tab'' i.e.\ how many lines to skip
    \item[{\ml mode}]      - the current printing mode: {\ml Hori}
                            -- horizontal box
                                           {\ml Veri} -- vertical box.
                This argument is really only used by breakpoints to
                determine whether to skip and indent or merely to horizontally
                tab
    \item[{\ml form}] - is the form to be printed
\end{description}
And here is how it works:
\begin{itemize}
  \item Strings are easy and obvious
  \item Break points need to make a distinction according to their mode 
  \item Horizontal boxes will need to know how much 
         to horizontally tab on breaks
  \item Vertical boxes need to know how much to indent and how much to skip 
  \item HV-Boxes get their element list neatly filtered into groups
        terminated by breaks
  \item HOV-Boxes either figure as Hboxes (if the full width fits within the
    margin) or as Vboxes otherwise
\end{itemize}
And this is the story of what happens when {\tt Bailout} is turned on:
if {\tt indentation}$+$ minimum-width of format to be 
printed$>${\tt marginwidth}, then we know that the output will {\em not} fit
into the page. When we detect this, we immediately insert a newline and
start outputting with an indentation of {\tt BailoutIndent} from the left border
of the page
(i.e.\ $new~indentation=old~marginwidth+{\tt BailoutIndent}$).
At the same time, we increase the {\tt marginwidth} dynamically by
{\tt Pagewidth}.
Correspondingly, in Bailout-mode all actual indentation is printed modulo
{\tt Pagewidth}.

However, there is a problem with this schema: if we happen to be getting into a
deeply nested structure with a high display width, increasing the margin by
{\tt Pagewidth} still will not give us the needed space, and we would thus
immediately bail out again, outputting text ``flush left'' indented by
{\tt BailoutIndent}. To lend some relief to this problem, the bailout code
also tests the value of {\tt BailoutSpot}, and will only trigger when the left
margin of the text would be output {\em after} the {\tt BailoutSpot} on the
page:
$insert~{\tt mod}~ {\tt Pagewidth} \geq BailoutSpot$
%{\bf Caution:} 
%A horizontal breakpoint will {\em always} be printed with the
%default horizontal tab. Of course this renders all specifications
%for the horizontal tab width superfluous.
%(At least this seems to be a better idea than
%printing it as wide as the indent when verticalizing.)
%In order to actually be able to specify different horizontal tabs
%---at least at the box level--- we need to augment the printing functions
%with an additional argument.
*)
and print'p(mw,id,bl,is,ss,mo,  Str(n,s),res) = (n,s::res)
 |  print'p(mw,id,bl,is,ss,Hori,Brk(b,i),res) = 
           (b, (if (!Bailout) then Spmod(b) else Sp(b))::res)
 |  print'p(mw,id,bl,is,ss,Vert, Brk(b,i), res) =
           (i, (if (!Bailout) then Spmod(id+i) else Sp(id+i))::(Nl(ss))::res)
 |  print'p(mw,id,bl,is,ss,Hori, Dbk, res) = 
           (bl, (if (!Bailout) then Spmod(bl) else Sp(bl))::res)
 |  print'p(mw,id,bl,is,ss,Vert, Dbk, res)      = 
           (is,(if (!Bailout) then Spmod(id+is) else Sp(id+is))::(Nl(ss))::res)
 |  print'p(mw,id,bl,is,ss,mo,   Ebk,res)      = (0,res)
 |  print'p(mw,id,bl,is,ss,mo,   Hbx((min,max),blanks,l),res) =
            if (!Bailout) andalso (id+min) >= mw
               andalso (id mod (!Pagewidth) >= !BailoutSpot)
            then pph(mw+(!Pagewidth),mw + (!BailoutIndent),
                        blanks,is,ss,l,0,(Nl(ss))::res)
            else pph(mw,id,blanks,is,ss,l,0,res)
 |  print'p(mw,id,bl,is,ss,mo,   Vbx((min,max),indent,skip,l), res) =
           if (!Bailout) andalso (id+min) >= mw
               andalso (id mod (!Pagewidth) >= !BailoutSpot)
           then let val id = mw+(!BailoutIndent)
                in ppv(mw+(!Pagewidth),id,id,bl,indent,skip,0,0,l,(Nl(ss))::res) end
           else ppv(mw,id,id,bl,indent,skip,0,0,l,res)
 |  print'p(mw,id,bl,is,ss,mo,   Hvx(((min,max),(nmode,xmode)),blanks,indent,skip,l), res) =
            let val gl=gh(nil,l,nil) in
            if (!Bailout) andalso (id+min) >= mw
               andalso (id mod (!Pagewidth) >= !BailoutSpot)
            then pphv(mw+(!Pagewidth),mw+(!BailoutIndent),
                          blanks,indent,skip,0,0,Ebk,gl,(Nl(ss))::res)
            else pphv(mw,id,blanks,indent,skip,0,0,Ebk,gl,res)
            end
 |  print'p(mw,id,bl,is,ss,mo,   Hov(((min,max),(nmode,xmode)),blanks,indent,skip,l), res) =
             if (max<=(mw-id))
             then if xmode=Hori 
                     then pph(mw,id,blanks,is,ss,l,0,res)
                     else ppv(mw,id,id,blanks,indent,skip,0,0,l,res)
             else if (!Bailout) andalso (id+min >= mw)
                     andalso (id mod (!Pagewidth) >= !BailoutSpot)
                  then if nmode=Hori
                       then pph(mw+(!Pagewidth),mw + (!BailoutIndent),
                                  blanks,is,ss,l,0,(Nl(ss))::res)
                       else let val id = mw + (!BailoutIndent)
                            in ppv(mw+(!Pagewidth),id,id,
                                  blanks,indent,skip,0,0,l,(Nl(ss))::res) end
                  else if nmode=Hori
                       then pph(mw,id,blanks,is,ss,l,0,res)
                       else ppv(mw,id,id,blanks,indent,skip,0,0,l,res)
(*
%{\bf Improvements:}
%The pagewidth does not need to be an explicit argument to the printing
%functions. Rather it should be realized as a {\ml ref}-value.
% we should also make provision for a maximum print depth and the use of
% elisions. 

%*************************************************************************
\subsubsection{Printing routines}
Finally we have {\ml print\_fmt} and {\ml makestring\_fmt}
   These functions performs the actual printing --- they call on 
   {\ml print'p} to do the formatting work.
*)

      fun makestring_fmt fm =
          String.concat(rev(snd(print'p(!Pagewidth,0,!Blanks,!Indent,!Skip,Hori,fm,nil))))

      fun print_fmt fm = 
          List.foldr (fn (s,_) => print s)
                ()
                (snd(print'p(!Pagewidth,0,!Blanks,!Indent,!Skip,Hori,fm,nil)))

(*
\subsubsection{Output functions}
The output functions work on {\tt fmtstream}s which are just
packaged-up {\tt outstream}s.
The functions {\tt file\_open\_fmt} and {\tt with\_open\_fmt} endeavor to
make the use of {\tt fmtstreams} on files more convenient.
*)
      datatype fmtstream = Formatstream of TextIO.outstream

      fun open_fmt outs = Formatstream(outs)

      fun close_fmt (Formatstream outs) = outs

      fun output_fmt(Formatstream outs,fm) = 
          List.foldr (fn (s,_) => TextIO.output(outs, s))
	        ()
                (snd(print'p(!Pagewidth,0,!Blanks,!Indent,!Skip,Hori,fm,nil)))

      (*
      fun debug_output_fmt(Formatstream fs, fm) =
                  let val mw = (!Pagewidth)
                            val (min,max) = Width0(Hori,!Blanks,!Indent,fm)
                            val (w,s) = print'p(!Pagewidth,0,!Blanks,!Indent,!Skip,Hori,fm, nil)
                          in
                             output(fs,
                                    "\nMarginwidth: "^(makestring mw)^
                                    "\nFormatwidth: ("^(makestring min)^","^
                                                       (makestring max)^")"^
                                    "\nPrintwidth:  "^(makestring w)^"\n"^
                                    "0<<<.<<<<1<<<<.<<<<2<<<<.<<<<3<<<<.<<<<4<<<<.<\n"
                                    ^(implode (rev s))^"\n"^
                                    "0>>>.>>>>1>>>>.>>>>2>>>>.>>>>3>>>>.>>>>4>>>>.>\n"
                                  )
                          end
       *)
      
      fun file_open_fmt filename =
         let val fmt_stream = open_fmt(TextIO.openOut filename)
             val close_func = fn () => ( TextIO.closeOut(close_fmt(fmt_stream)) )
          in (close_func, fmt_stream) end
      
      fun with_open_fmt filename func =
         let val (close_func, fmt_stream) = file_open_fmt filename
             val result = func fmt_stream
                          handle exn => ( close_func () ; raise exn )
          in ( close_func () ; result ) end

   end; (* struct *)
