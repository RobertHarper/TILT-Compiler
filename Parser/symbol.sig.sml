(*$import Prelude *)

(* Copyright 1989 by AT&T Bell Laboratories *)
signature SYMBOL = sig
    type symbol
    datatype namespace =
       VALspace | TYCspace | SIGspace | STRspace | FCTspace | FIXspace |
       LABspace | TYVspace | FSIGspace
    val eq: symbol * symbol -> bool
    and symbolGt : symbol * symbol -> bool
    and symbolCMLt : symbol * symbol -> bool
    and varSymbol: string -> symbol
    and tycSymbol: string -> symbol
    and sigSymbol: string -> symbol
    and strSymbol: string -> symbol
    and fctSymbol: string -> symbol
    and fsigSymbol: string -> symbol
    and fixSymbol: string -> symbol
    and labSymbol: string -> symbol
    and tyvSymbol: string -> symbol
    and var'n'fix : string -> symbol * symbol
    and name: symbol -> string
    and number: symbol -> int
    val nameSpace : symbol -> namespace
    val nameSpaceToString : namespace -> string
    val symbolToString : symbol -> string

(* Probably should merge STRspace and FCTspace into one namespace.
   Similarly for SIGspace and FSIGspace. *)

end

(*
 * $Log$
# Revision 1.3  2001/12/13  16:32:50  swasey
# *** empty log message ***
# 
# Revision 1.2  2000/09/12  18:57:15  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/01/21  20:40:52  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  18:16:06  pscheng
# added the sig file
# 
 *)
