(*$import Prelude SourceMap PrettyPrint Source *)

(* <errormsg.sig>=                                                          *)
(* Copyright 1989 by AT&T Bell Laboratories *)
signature ERRORMSG =
 sig
    datatype severity = WARN | COMPLAIN
    type complainer = severity -> string -> (PrettyPrint.ppstream -> unit)
                          -> unit 
    type errorFn = SourceMap.region -> complainer
    type errors = {error: errorFn,
                   errorMatch: SourceMap.region->string,
                   anyErrors : bool ref} 
    val anyErrors : errors -> bool
    exception Error
    val defaultConsumer : unit -> PrettyPrint.ppconsumer
    val nullErrorBody : PrettyPrint.ppstream -> unit
    val error : Source.inputSource -> SourceMap.region -> complainer
    val errorNoFile : PrettyPrint.ppconsumer * bool ref -> SourceMap.region
                      -> complainer

    val matchErrorString : Source.inputSource -> SourceMap.region -> string
    val errors : Source.inputSource -> errors
    val errorsNoFile : PrettyPrint.ppconsumer * bool ref -> errors

    val impossible : string -> 'a
    val impossibleWithBody : string -> (PrettyPrint.ppstream -> unit) -> 'a
 end

(*
 * $Log$
# Revision 1.4  2001/12/13  16:32:36  swasey
# *** empty log message ***
# 
# Revision 1.3  2000/09/12  18:56:50  swasey
# Changes for cutoff compilation
# 
# Revision 1.2  98/02/01  01:27:57  pscheng
# Changes to facilitate bootstrapping:
#   Added ascription in various places
#   Split up files into signature and code
# 
# Revision 1.1  1998/01/21  20:40:13  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  18:16:03  pscheng
# added the sig file
# 
 *)
