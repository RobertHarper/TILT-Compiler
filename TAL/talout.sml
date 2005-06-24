(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dan Grossman                        *)
(*     June 1998, all rights reserved.                                *)
(*                                                                    *)
(* Ported to SML by Leaf Petersen                                     *)
(* November, 2003                                                     *)
(*                                                                    *)
(**********************************************************************)

(* talout.sml
 * Useful functions for programs to write TAL modules out and call an
 * external verifier/assembler/linker.
 *
 *)

structure TalOut :> TALOUT = 
  struct

    fun error s = Util.error "talout.sml" s
    val debug = Stats.ff "TalOutDebug"

    fun abortIO (fileName, {cause = OS.SysErr (msg, _), function = f, name = n}) =
      error ("IO Error on file " ^ fileName ^ ":\n" ^ msg ^ "\n")
      | abortIO (fileName, {function = f, ...}) =
      error ("IO Error on file " ^ fileName ^ " from function "
	     ^ f ^ "\n")
      
    fun io_error (msg,fileName,exn) =
      (case exn
	 of IO.Io (ioError) => abortIO (fileName, ioError)
	  | _ => (print ("problem in "^fileName);
		  error msg))
	 
    fun openOutput outfilename =
      (if (!debug) then (print "about to open_out "; print outfilename; print "\n") else ();
	 (TextIO.openOut outfilename) handle exn => io_error ("openOutput", outfilename,exn))
	 
    fun openAppend outfilename =
      (if (!debug) then (print "about to open_append"; print outfilename; print "\n") else ();
	 (TextIO.openAppend outfilename) handle exn => io_error ("openAppend", outfilename,exn))
	 
    fun closeOutput oc =
      ((TextIO.closeOut oc) handle exn => io_error ("closeOutput", "some file",exn))
	
    fun wrapper pp outfile obj = 
      let 
	val oc = openOutput outfile
	val fmtstream = Formatter.open_fmt oc
	val fmt = ((pp obj)
		   handle any => (Formatter.close_fmt fmtstream;
				  closeOutput oc;
				  raise any))
      in 
	Formatter.output_fmt (fmtstream,fmt); 
	Formatter.close_fmt fmtstream;
	closeOutput oc
      end 

    fun wrapchannel pp oc obj = 
      let 
	val fmtstream = Formatter.open_fmt oc
	val fmt = ((pp obj)
		   handle any => (Formatter.close_fmt fmtstream;
				  raise any))
      in 
	Formatter.output_fmt (fmtstream,fmt); 
	Formatter.close_fmt fmtstream;
	()
      end 

    fun wrapchannel' pp oc obj = 
      let 
	val fmtstream = Formatter.open_fmt oc
	fun write fmt = Formatter.output_fmt (fmtstream,fmt); 
	val () = ((pp write obj)
		   handle any => (Formatter.close_fmt fmtstream;
				  raise any))
      in 
	Formatter.close_fmt fmtstream;
	()
      end 

    fun wrapfile pp outfile obj = 
      let 
	val oc = openOutput outfile
	val () = ((wrapchannel pp oc obj)
		  handle any => (closeOutput oc;
				 raise any))
      in 
	closeOutput oc
      end 

    fun set_formats () = 
      let
	val olddepth = !Formatter.DoDepth
	val oldindent = !Formatter.Indent
	val oldskip = !Formatter.Skip
	val oldwidth = !Formatter.Pagewidth

	val () = Formatter.DoDepth := false
	val () = Formatter.Indent := 0
	val () = Formatter.Skip := 0
	val () = Formatter.Pagewidth := 9999;
      in (olddepth,oldindent,oldskip,oldwidth)
      end

    fun reset_formats (olddepth,oldindent,oldskip,oldwidth) = 
      let
	val () = Formatter.DoDepth := false
	val () = Formatter.Indent := oldindent
	val () = Formatter.Skip := oldskip
	val () = Formatter.Pagewidth := oldwidth
      in ()
      end

    fun writer pp filename obj =
      let
	val fmtstate = set_formats()

	val () = (wrapfile pp filename obj) handle any => (reset_formats fmtstate;raise any)

	val () = reset_formats fmtstate
      in ()
      end

    val write_pre_mod = fn modname => writer (Pptal.print_tal_pre_mod Pptal.std_options modname)
    val write_imp_body = writer (Pptal.print_tal_imp_body Pptal.std_options)
    val write_int = fn modname => writer (Pptal.print_tal_int Pptal.std_options modname)

    fun channelwriter pp channel obj = 
      let
	val fmtstate = set_formats()
	  
	val () = (wrapchannel pp channel obj) handle any => (reset_formats fmtstate;raise any)
	  
	val () = reset_formats fmtstate
      in ()
      end

    fun channelwriter' pp channel obj = 
      let
	val fmtstate = set_formats()
	  
	val () = (wrapchannel' pp channel obj) handle any => (reset_formats fmtstate;raise any)
	  
	val () = reset_formats fmtstate
      in ()
      end

    fun write_imp'
      (oc : TextIO.outstream)
      (unitname : string)
      (imports : Tal.int_ref vector) 	(* imported interface files *)
      (exports : Tal.int_ref vector) 	(* exported interface files *)
      (imp : Tal.tal_imp)
      = 
      let
	val () = channelwriter (Pptal.print_imp_header Pptal.std_options) oc unitname
	val () = channelwriter (Pptal.print_tal_import_refs Pptal.std_options) oc imports
	val () = channelwriter (Pptal.print_tal_export_refs Pptal.std_options) oc exports
	val () = channelwriter' (Pptal.write_tal_imp_body Pptal.std_options) oc imp
      in ()
      end

    fun write_imp asmfile unitname imports exports imp =
      let 
	val oc = openOutput asmfile
	val () = ((write_imp' oc unitname imports exports imp)
		  handle any => (closeOutput oc;
				 raise any))
      in 
	closeOutput oc
      end 

  end (* EOF: talout.sml *)
