structure LilToTal :> LILTOTAL = 
  struct
    structure LTE = LilToTalExp


    val error = fn s => Util.error "liltotal.sml" s
    val debug = Stats.ff "LilToTalDebug"

    val std_libs = ["stdlib.tali","support_e.tali"]

    fun files2intrefs files = Vector.fromList (map Tal.Int_filename files)

    fun allocateModule (asmfile : string) (asm_ifiles : (string * string) option) (unitname : string) (imports : string list) (exports : string list) (lilmod : Lil.module) : unit =
      let
	val imports = files2intrefs (std_libs @ imports)
	val exports = files2intrefs exports
	val link = not (isSome asm_ifiles)
	val (unitname,tal_imp,tal_i_int,tal_e_int) = LTE.modtrans link lilmod
	val () = TalOut.write_imp asmfile unitname imports exports tal_imp
	val () = case asm_ifiles 
		   of SOME (asm_i_file,asm_e_file) => (TalOut.write_int unitname asm_i_file tal_i_int
						       ; TalOut.write_int unitname asm_e_file tal_e_int)
		    | NONE => ()
      in ()
      end

    fun allocateInterface asmfile lilint = 
      let
	val (unitname,tal_int) = LTE.inttrans lilint
	val () = TalOut.write_int unitname asmfile tal_int
      in ()
      end

end  (* LilToTal *)