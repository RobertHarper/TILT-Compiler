signature ERROR = 
  sig
    datatype ErrorLevel = Error | NoError | Warn
    type region = Ast.srcpos * Ast.srcpos

    (* reset the error state to NoError and set a file *)
    val nofilepos : SourceMap.charpos -> string * int * int
    val reset : (SourceMap.charpos -> string * int * int) -> unit

    (* pushing and popping the source region as we traverse the AST *)
    val push_region : region -> unit
    val pop_region : unit -> unit

    (* fetch the current error level *)
    val get_error : unit -> ErrorLevel

    (* Get the current region and the string associated with it *)
    val peek_region : unit -> region
    val peek_region_str : unit -> string

    (* Record a warning and return the string associated with it *)
    val warn_region : unit -> unit
    val warn_region_string : unit -> string

    (* Record an error and return the string associated with it *)
    val error_region : unit -> unit
    val error_region_string : unit -> string

    (* Tabbing for multi-line warning/error messages *)
    val tab_region : unit -> unit

  end