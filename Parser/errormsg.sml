(*$import Int Source ERRORMSG PrettyPrint SourceMap Control PathNames *)

(* <errormsg.sml>=                                                          *)
(* Copyright 1989 by AT&T Bell Laboratories *)

structure ErrorMsg :> ERRORMSG =
struct

  open PrettyPrint SourceMap

 (* error reporting *)

  exception Error  (* was Syntax, changed to Error in 0.92 *)

  datatype severity = WARN | COMPLAIN

  type complainer = severity -> string -> (ppstream -> unit) -> unit

  type errorFn = region -> complainer

  type errors = {error: region->complainer,
                 errorMatch: region->string,
                 anyErrors: bool ref}

  fun defaultConsumer () =
      {consumer = Control.Print.say,
       linewidth = !Control.Print.linewidth,
       flush = Control.Print.flush}

  val nullErrorBody = (fn (ppstrm: ppstream) => ())

  fun ppmsg(errConsumer,location,severity,msg,body) =
      with_pp errConsumer (fn ppstrm =>
        (begin_block ppstrm CONSISTENT 0;
         begin_block ppstrm CONSISTENT 2;
         add_string ppstrm location;
         add_string ppstrm  (* print error label *)
            (case severity
               of WARN => " Warning: "
                | COMPLAIN => " Error: ");
         add_string ppstrm msg;
         body ppstrm;
         end_block ppstrm;
         add_newline ppstrm;
         end_block ppstrm))

  fun record(COMPLAIN,anyErrors) = anyErrors := true
    | record(WARN,_) = ()

  fun impossible msg =
      (app Control.Print.say ["Error: Compiler bug: ",msg,"\n"];
       Control.Print.flush();
       raise Error)
(* With the advent of source-map resynchronization (a.k.a                   *)
(* [[( *#line...* )]]), a contiguous region as seen by the compiler can     *)
(* correspond to one or more contiguous regions in source code.             *)
(* We can imagine myriad ways of displaying such information, but we        *)
(* confine ourselves to two:                                                *)
(* \begin{itemize}                                                          *)
(* \item                                                                    *)
(* When there's just one source region, we have what we had in the old      *)
(* compiler, and we display it the same way:                                *)
(* \begin{quote}                                                            *)
(* {\tt \emph{name}:\emph{line}.\emph{col}} or\\                            *)
(* {\tt \emph{name}:\emph{line1}.\emph{col1}-\emph{line2}.\emph{col2}}      *)
(* \end{quote}                                                              *)
(* \item                                                                    *)
(* When there are two or more source regions, we use an ellipsis instead    *)
(* of a dash, and if not all regions are from the same file, we provide     *)
(* the file names of both endpoints (even if the endpoints are the same     *)
(* file).                                                                   *)
(* \end{itemize}                                                            *)
(*                                                                          *)
(* <errormsg.sml>=                                                          *)
  fun location_string ({sourceMap,fileOpened,...}:Source.inputSource) (p1,p2) =
      let fun shortpoint ({line, column,...}:sourceloc, l) = 
             Int.toString line :: "." :: Int.toString column :: l
          fun showpoint (p as {fileName,...}:sourceloc, l) = 
             Pathnames.trim fileName :: ":" :: shortpoint (p, l)
          fun allfiles(f, (src:sourceloc, _)::l) = 
                f = #fileName src andalso allfiles(f, l)
            | allfiles(f, []) = true
          fun lastpos [(_, hi)] = hi
            | lastpos (h::t) = lastpos t
            | lastpos [] = impossible "lastpos botch in ErrorMsg.location_string"
      in  concat (
            case fileregion sourceMap (p1, p2)
              of [(lo, hi)] => 
                    if p1+1 >= p2 then showpoint (lo, [])
                    else showpoint(lo, "-" :: shortpoint(hi, []))
               | (lo, _) :: rest =>
                    if allfiles(#fileName lo, rest) then
                      showpoint(lo, "..." :: shortpoint(lastpos rest, []))
                    else
                      showpoint(lo, "..." :: showpoint (lastpos rest, []))
               | [] => [Pathnames.trim fileOpened, ":<nullRegion>"]
          )
      end
(* Emulating my predecessors, I've gone to some trouble to avoid list appends (and the *)
(* corresponding allocations).                                              *)
(*                                                                          *)
(* <errormsg.sml>=                                                          *)
  fun error (source as {anyErrors, errConsumer,...}: Source.inputSource)
            (p1:int,p2:int) (severity:severity)
            (msg: string) (body : ppstream -> unit) = 
      (ppmsg(errConsumer,(location_string source (p1,p2)),severity,msg,body);
       record(severity,anyErrors))

  fun errorNoFile (errConsumer,anyErrors) ((p1,p2): region) severity msg body = 
      (ppmsg(errConsumer,
             if p2>0 then concat[Int.toString p1, "-", Int.toString p2] 
                     else "",
             severity, msg, body);
       record(severity,anyErrors))

  fun impossibleWithBody msg body =
      (with_pp (defaultConsumer()) (fn ppstrm =>
        (add_string ppstrm "Error: Compiler bug: ";
         add_string ppstrm msg;
         body ppstrm;
         add_newline ppstrm));
       raise Error)

  val matchErrorString = location_string

  fun errors source = 
      {error = error source,
       errorMatch = matchErrorString source,
       anyErrors = #anyErrors source}

  fun anyErrors{anyErrors,error,errorMatch} = !anyErrors

  fun errorsNoFile (consumer,any) =
      {error = errorNoFile (consumer,any),
       errorMatch = fn _ => "Match",
       anyErrors = any}

end  (* structure ErrorMsg *)

