
(* =========================================================================
 * AlphaMLRISCPseudo.sml
 * ========================================================================= *)

structure AlphaMLRISCPseudo
	    :> MLRISC_PSEUDO
		 where type idSet = DenseIntSet.set
	    = struct

  (* -- structures --------------------------------------------------------- *)

  structure IntSet = DenseIntSet

  (* -- types -------------------------------------------------------------- *)

  type idSet = IntSet.set

  datatype pseudo_op =
    ModuleHeader
  | ModuleTrailer
  | TextHeader
  | TextTrailer
  | DataHeader
  | DataTrailer
  | TableHeader
  | TableTrailer
  | ClusterHeader
  | ClusterTrailer
  | ProcedureHeader of Label.label
  | ProcedureTrailer of Label.label
  | Export of Label.label
  | CallSite of idSet ref * (idSet -> unit)
  | Align of int * int
  | Comment of string
  | Integer of Word32.word
  | IntegerFloatSize of Word32.word
  | Float of string
  | Label of Label.label
  | IntegerArray of int * Word32.word
  | FloatArray of int * string
  | LabelArray of int * Label.label
  | String of string

  (* -- exceptions --------------------------------------------------------- *)

  exception Unimplemented

  (* -- functions ---------------------------------------------------------- *)

  (*
   * Return a label string with special characters escaped.
   * string -> the label string to escape
   * <- the escaped label string
   *)
  local
    fun fixChar #"'"  = "PRIME"
      | fixChar #"!"  = "BANG"
      | fixChar #"%"  = "PERCENT"
      | fixChar #"&"  = "AND"
      | fixChar #"$"  = "DOLLAR"
      | fixChar #"#"  = "HASH"
      | fixChar #"+"  = "PLUS"
      | fixChar #"-"  = "MINUS"
      | fixChar #"/"  = "SLASH"
      | fixChar #":"  = "COLON"
      | fixChar #"<"  = "LT"
      | fixChar #"="  = "EQ"
      | fixChar #">"  = "GT"
      | fixChar #"?"  = "QUEST"
      | fixChar #"@"  = "AT"
      | fixChar #"\\" = "BACKSLASH"
      | fixChar #"~"  = "TILDE"
      | fixChar #"`"  = "ANTIQUOTE"
      | fixChar #"^"  = "HAT"
      | fixChar #"|"  = "BAR"
      | fixChar #"*"  = "STAR"
      | fixChar char  = Char.toString char
  in
    fun fixLabel string = foldr op^ "" (map fixChar (explode string))
  end

  local
    fun fixFloat string =
	  let
	    fun fixSign #"~" = #"-"
	      | fixSign char = char
	  in
	    implode(map fixSign (explode string))
	  end

    fun fixString string = foldr op^ "" (map Char.toString (explode string))
  in
    fun toString(ModuleHeader) =
	  "\t.set reorder\n"^
	  "\t.set macro\n"^
	  "\t.set noat\n"
      | toString(ModuleTrailer) =
	  "\t.text\n" (* hack for overflow handler ??? *)
      | toString(TextHeader) =
	  "\t.text\n"
      | toString(TextTrailer) =
	  ""
      | toString(DataHeader) =
	  "\t.data\n"
      | toString(DataTrailer) =
	  ""
      | toString(TableHeader) =
	  "\t.data\n"
      | toString(TableTrailer) =
	  ""
      | toString(ClusterHeader) =
	  ""
      | toString(ClusterTrailer) =
	  ""
      | toString(ProcedureHeader label) =
	  let
	    val name = fixLabel(Label.nameOf label)
	  in
	    "\n"^
	    "\t.ent "^name^"\n"^
	    name^":\n"
	  end
      | toString(ProcedureTrailer label) =
	  let
	    val name = fixLabel(Label.nameOf label)
	  in
	    "\t.end "^name^"\n"
	  end
      | toString(Export label) =
	  let
	    val name = fixLabel(Label.nameOf label)
	  in
	    "\t.globl "^name^"\n"
	  end
      | toString(CallSite(ref live, emit)) =
	  (emit live; "")
      | toString(Align (size, offset)) =
	  "\t.align "^Int.toString size^", "^Int.toString offset^"\n"
      | toString(Comment message) =
	  "\t# "^message^"\n"
      | toString(Integer value) =
	  "\t.long 0x"^Word32.toString value^"\n"
      | toString(IntegerFloatSize value) =
	  "\t.quad 0x"^Word32.toString value^"\n"
      | toString(Float value) =
	  "\t.t_floating "^fixFloat value^"\n"
      | toString(Label label) =
	  "\t.long "^fixLabel(Label.nameOf label)^"\n"
      | toString(IntegerArray(size, value)) =
	  "\t.long 0x"^Word32.toString value^" : "^Int.toString size^"\n"
      | toString(FloatArray(size, value)) =
	  "\t.t_floating "^fixFloat value^" : "^Int.toString size^"\n"
      | toString(LabelArray(size, label)) =
	  "\t.long "^fixLabel(Label.nameOf label)^" : "^Int.toString size^"\n"
      | toString(String value) =
	  "\t.ascii \""^fixString value^"\"\n"
  end

  fun emitValue _ = raise Unimplemented (* ??? *)

  fun sizeOf _ = raise Unimplemented (* ??? *)

  fun adjustLabels _ = raise Unimplemented (* ??? *)

end

