(*$import Prelude MONO_ARRAY String Word8Array *)
(* char-array.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Labs.
 *
 *)

structure CharArray : MONO_ARRAY =
  struct

    type elem = char
    type vector = string
    type array = Word8Array.array

    val maxLen = String.maxSize


    val array    = Word8Array.array    
    val tabulate = Word8Array.tabulate 
    val fromList = Word8Array.fromList 

    val length   = Word8Array.length   
    val sub      = Word8Array.sub      
    val update   = Word8Array.update   
    val extract  = Word8Array.extract  

    val copy = Word8Array.copy 
    val copyVec = Word8Array.copyVec 


    val app    = Word8Array.app    
    val foldl  = Word8Array.foldl  
    val foldr  = Word8Array.foldr  
    val modify = Word8Array.modify 

    val appi    = Word8Array.appi    
    val foldli  = Word8Array.foldli  
    val foldri  = Word8Array.foldri  
    val modifyi = Word8Array.modifyi 


  end (* CharArray *)


(*
 * $Log$
# Revision 1.2  2000/09/12  18:54:07  swasey
# Changes for cutoff compilation
# 
# Revision 1.1  98/03/09  19:50:10  pscheng
# added basis
# 
 * Revision 1.2  1997/02/11  15:15:36  george
 * got rid of structure rebinding, since inlining is now preserved
 *
 * Revision 1.1.1.1  1997/01/14  01:38:13  george
 *   Version 109.24
 *
 *)
