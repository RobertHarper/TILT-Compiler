structure LilToTal :> LILTOTAL = 
  struct
    structure LTE = LilToTalExp


    val error = fn s => Util.error "liltotal.sml" s
    val debug = Stats.ff "LilToTalDebug"

    fun files2intrefs files = Vector.fromList (map Tal.Int_filename files)

    fun allocateModule (asmfile : string) (unitname : string) (imports : string list) (exports : string list) (lilmod : Lil.module) : unit =
      let
	val imports = files2intrefs imports
	val exports = files2intrefs exports
	val (unitname,tal_int_i,tal_imp,tal_int_e) = LTE.modtrans lilmod
	val () = TalOut.write_imp asmfile unitname imports exports tal_imp
      in ()
      end

    fun allocateInterface asmfile lilint = 
      let
	val (unitname,tal_int) = LTE.inttrans lilint
	val () = TalOut.write_int unitname asmfile tal_int
      in ()
      end

end  (* LilToTal *)