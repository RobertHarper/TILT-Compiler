(*$Distribute: Toplevel Linkall Linkalpha *)
local
    exception BAD
    val envvar_name = "SMLC_ARG"
    val envvar_name' = "SMLC_ARG="
    fun env_extract [] = (print "Sorry, there was no argument\n"; raise BAD)
      | env_extract (a::rest) =
	if (size a >= size envvar_name' andalso
	    substring(a,0,size envvar_name') = envvar_name')
	    then substring(a,size envvar_name',size a - size envvar_name')
	else env_extract rest
    fun compiler libname (_,[]) = (print "Error: expected a filename"; ~1)
      | compiler libname (_,file::_) = 
	let 
	  val _ = (print "file is *"; print file; print "*\n")
	  val _ = Linkalpha.comp_file file
	  val asmname = file ^ ".alpha.s"
	  val objname = file ^ ".o"
	  val exename = file ^ ".exe"
	  val command1 = "as -O1  -o " ^ objname ^ " " ^ asmname
	  val command2 = "ld  -o " ^ exename ^ 
	    " -D 40000000 -T 20000000 -non_shared /usr/lib/cmplrs/cc/crt0.o " ^ 
	    objname ^ " " ^ libname ^ " -lm -lc"
	  val _ = OS.Process.system command1
	  val _ = OS.Process.system command2
	in
	    0
	end
    fun make_lib lib_name = 
	let val command = "cd Runtime; gmake target_lib target_lib=" ^ lib_name
	in OS.Process.system command
	end
    fun make_script(script_name, heap_dir) = 
	let
	    val res = open_out script_name
	    val _ = output(res,"#!/bin/csh\n")
	    val _ = output(res,"echo 'Report problems to pscheng@cs.cmu.edu'\n")
	    val _ = output(res,"setenv HEAP_DIR \"" ^ heap_dir ^ "\"\n")
	    val _ = output(res,"setenv " ^ envvar_name ^ " $argv[1]\n")
	    val _ = output(res,"/afs/cs/project/fox/member/pscheng/til/bin/smlc\n")
	    val _ = close_out res
	    val _ = OS.Process.system ("chmod +x " ^ script_name)
	in
	    ()
	end
in
  val compiler = compiler
    fun create_smlc (path : string) = 
	let
	    local val temp = rev(explode path)
	    in    val target_dir = implode(rev(if ((hd temp) = #"/") 
					    then (tl temp) else temp))
	    end
	    val comp_name = target_dir ^ "/" ^ "smlc";
	    val lib_name = target_dir ^ "/" ^ "runtime.a";
	    val heap_name = target_dir ^ "/" ^ "smlc.alpha32"; 
	    val _ = make_script(comp_name,target_dir);
	    val _ = make_lib(lib_name);
	in
	    exportFn(heap_name,compiler lib_name)
	end
end