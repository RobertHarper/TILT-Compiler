(* asmStream.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(* AsmStream - this structure is available to all codegenerators.
 *             Typically asmOutStream is rebound to a file.
 *)

signature ASM_STREAM = sig
  val asmOutStream : TextIO.outstream ref
  val withStream : TextIO.outstream -> ('a -> 'b) -> 'a -> 'b
end

structure AsmStream : ASM_STREAM = struct
  val asmOutStream = ref TextIO.stdOut
  fun withStream stream body x = let
     val s = !asmOutStream 
     val _ = asmOutStream := stream
  in
    (body x before asmOutStream := s)
       handle e => (asmOutStream := s; raise e)
  end   
end



(*
 * $Log$
# Revision 1.1  99/02/17  21:15:28  pscheng
# *** empty log message ***
# 
# Revision 1.1  1999/02/17  20:07:03  pscheng
# *** empty log message ***
#
 * Revision 1.1.1.1  1998/11/16 21:48:19  george
 *  Version 110.10
 *
 * Revision 1.2  1998/10/06 14:07:43  george
 * Flowgraph has been removed from modules that do not need it.
 * Changes to compiler/CodeGen/*/*{MLTree,CG}.sml necessary.
 * 						[leunga]
 *
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
