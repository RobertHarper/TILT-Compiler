(* =========================================================================
 * SparcMLRISCPseudo.sml
 * ========================================================================= *)

structure SparcMLRISCPseudo
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
  | Align of int
  | AlignOdd of int
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
    fun pow(a,0) = 1
      | pow(a,n) = a * (pow(a,n-1))
    fun section str = "\t.section \"." ^ str ^ "\"\n"
  in
    fun toString(ModuleHeader) = ""
      | toString(ModuleTrailer) = section "text" (* hack for overflow handler ??? *)
      | toString(TextHeader) =	  section "text"
      | toString(TextTrailer) =	  ""
      | toString(DataHeader) =	  section "data"
      | toString(DataTrailer) =	  ""
      | toString(TableHeader) =	  section "data"
      | toString(TableTrailer) =  ""
      | toString(ClusterHeader) = ""
      | toString(ClusterTrailer) = ""
      | toString(ProcedureHeader label) =
	  let
	    val name = fixLabel(Label.nameOf label)
	  in
	    "\n"^
	    "\t.proc  07\n"^
	    "\t.align 4\n"^
	    name^":\n"
	  end
      | toString(ProcedureTrailer label) =
	  let
	    val name = fixLabel(Label.nameOf label)
	  in
	    "\t.size "^name^",(.-" ^ name ^ ")\n"
	  end
      | toString(Export label) =
	  let
	    val name = fixLabel(Label.nameOf label)
	  in
	    "\t.globl "^name^"\n"
	  end
      | toString(CallSite(ref live, emit)) =
	  (emit live; "")
      | toString(Align size) =
	  "\t.align "^Int.toString (pow(2,size))^"\n"
      | toString(AlignOdd 3) =
	  "\t.align 8\n\t.word 0\n"
      | toString(AlignOdd 4) =
	  "\t.align 16\n\t.word 0, 0, 0\n"
      | toString(AlignOdd _) =
	  raise Unimplemented (* ??? *)
      | toString(Comment message) =
	  "\t! "^message^"\n"
      | toString(Integer value) =
	  "\t.word 0x"^Word32.toString value^"\n"
      | toString(IntegerFloatSize value) =
	  "\t.quad 0x"^Word32.toString value^"\n"
      | toString(Float value) =
	  "\t.double "^fixFloat value^"\n"
      | toString(Label label) =
	  "\t.word "^fixLabel(Label.nameOf label)^"\n"
      | toString(IntegerArray(size, value)) =
	  "\t.word 0x"^Word32.toString value^" : "^Int.toString size^"\n"
      | toString(FloatArray(size, value)) =
	  "\t.double "^fixFloat value^" : "^Int.toString size^"\n"
      | toString(LabelArray(size, label)) =
	  "\t.word "^fixLabel(Label.nameOf label)^" : "^Int.toString size^"\n"
      | toString(String value) =
	  "\t.ascii \""^fixString value^"\"\n"
  end

  fun emitValue _ = raise Unimplemented (* ??? *)

  fun sizeOf _ = raise Unimplemented (* ??? *)

  fun adjustLabels _ = raise Unimplemented (* ??? *)

  fun removable _ = raise Unimplemented (* ??? *)

end

