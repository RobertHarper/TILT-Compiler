(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(*                                                                    *)
(* Ported to SML by Leaf Petersen                                     *)
(* November, 2003                                                     *)
(*                                                                    *)
(**********************************************************************)

signature PPTAL = 
  sig

    type options = { 
		    kinds          : bool,
		    cons           : bool, 
		    expand_abbrevs : bool,
		    expand_pr      : bool
		    }
    val std_options : options

    val print_scale :  Tal.scale -> Formatter.format
    val string_of_reg : Tal.reg -> string
    val print_reg_part :  options -> Tal.reg -> Tal.reg_part -> Formatter.format
    val print_reg :  options -> Tal.reg -> Formatter.format
    val print_kind :  options -> Tal.kind -> Formatter.format
    val print_ckind :  options -> Tal.kind -> Formatter.format
    val print_primcon :  Tal.primcon -> Formatter.format
    val print_variance :  Tal.variance -> Formatter.format
    val print_machine_state :
       options -> Tal.machine_state -> Formatter.format
    val print_fpstack :  options -> Tal.fpstack -> Formatter.format
    val print_con :  options -> Tal.con -> Formatter.format
    val print_ccon :  options -> Tal.con -> Formatter.format
    val print_coerce :
       ( options -> 'a -> Formatter.format) ->
      options -> 'a Tal.coerce -> Formatter.format
    val print_label_coerce :
       options -> Tal.label Tal.coerce -> Formatter.format
    val print_reg_coerce :  options -> Tal.reg Tal.coerce -> Formatter.format
    val print_genop :  options -> Tal.genop -> Formatter.format
    val print_genop_coerce :  options -> Tal.genop Tal.coerce -> Formatter.format
    val print_unary_op :  options -> Tal.genop -> Formatter.format
    val print_binop :  options -> Tal.genop -> Tal.genop -> Formatter.format
    val print_cc :  options -> Tal.condition -> Formatter.format
    val print_mallocarg :  options -> Tal.mallocarg -> Formatter.format
    val print_instruction :  options -> Tal.instruction -> Formatter.format
    val print_code_block :  options -> Tal.code_block -> Formatter.format
    val print_data_block :  options -> Tal.data_block -> Formatter.format



    val print_tal_imp_body :  options -> Tal.tal_imp -> Formatter.format
    val print_imp_header : options -> string -> Formatter.format

    val print_tal_int :  options -> string -> Tal.tal_int -> Formatter.format

    val print_tal_import_refs : options -> Tal.int_ref vector -> Formatter.format
    val print_tal_export_refs : options -> Tal.int_ref vector -> Formatter.format

    val print_tal_int_type :  options -> Tal.tal_int_type -> Formatter.format
    val print_tal_pre_mod  :  options -> string -> Tal.tal_pre_mod -> Formatter.format

  end (* EOF: talpp.mli *)
