(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(*                                                                    *)
(* Ported to SML by Leaf Petersen                                     *)
(* November, 2003                                                     *)
(*                                                                    *)
(**********************************************************************)

(* talout.sig.sml
 * Useful functions for programs to write TAL modules out and call an
 * external verifier/assembler/linker.
 *
 *)

signature TALOUT = 
  sig
(*
(* set this to produce an "annots" file containing disassembly information. *)
val do_write_dasm_info : bool ref;;

(* set this to register trusted runtime symbols with dynamic loader *)
val register_trusted_syms : bool ref;;

(* set this to pass the /DEBUG flag to the linker -- good for debugging *)
val link_debug : bool ref;;

val object_file_suffix : string;;
val library_suffix : string;;
val default_executable : string;;

val runtime : string ref;;
val set_runtime : string -> unit;;
val includes : string list ref;;     (* Stored in reverse order *)
val add_include : string -> unit;;

(* write_x modulename filename x *)
val write_options : Talpp.options ref;; (* TEMPORARY; CYCLONE/MASM *)

*)


    val write_imp_body : string -> Tal.tal_imp -> unit
    val write_imp_header : TextIO.outstream -> string -> Tal.int_ref vector -> Tal.int_ref vector -> unit
    val write_int : string -> string -> Tal.tal_int -> unit
    val write_pre_mod : string -> string -> Tal.tal_pre_mod -> unit

(*
(* print actual system commands executed to stdout or not *)
val verbose_sys_commands : bool ref;;
(* MS = use microsoft's assembler/linker, TALC = use ours *)
type bintool = MS | TALC | GNU;;
val asm_bintool : bintool ref;;
val link_bintool : bintool ref;; (* Only MS available *)
type binformat = COFF | ELF;;
val objformat : binformat ref;;

(* verify talfilename *)
val verify : string -> bool;;
(* asm talfilename objectfilename? *)
val asm : string -> string option -> bool;;
val verify_and_asm : string -> string option -> bool;;
(* link objectfilenames librariess executablefilename binary_rep_of_ints *)
val link : string list -> string list -> string -> string list option -> bool;;
*)
end
(* EOF: talout.mli *)
