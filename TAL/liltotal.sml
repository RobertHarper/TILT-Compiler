structure LilToTal :> LILTOTAL = 
  struct
    structure LTE = LilToTalExp


    val error = fn s => Util.error "liltotal.sml" s
    val debug = Stats.ff "LilToTalDebug"

    fun allocateModule asmfile lilmod = 
      let
	val (unitname,tal_int_i,tal_imp,tal_int_e) = LTE.modtrans lilmod
	val () = TalOut.write_imp_body asmfile tal_imp
      in ()
      end

    fun allocateInterface asmfile lilint = 
      let
	val (unitname,tal_int) = LTE.inttrans lilint
	val () = TalOut.write_int unitname asmfile tal_int
      in ()
      end

    fun files2intrefs files = Vector.fromList (map Tal.Int_filename files)

    fun write_imp_headers (oc : TextIO.outstream) (unitname : string) (imports : string list) (exports : string list) = 
      let
	val imports = files2intrefs imports
	val exports = files2intrefs exports
      in TalOut.write_imp_header oc unitname imports exports
      end
      

end  (* LilToTal *)