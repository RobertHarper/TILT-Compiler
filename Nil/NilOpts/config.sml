Linknil.do_opt:=true;
Linknil.PpNil.elide_bnd := false;

NilOpts.do_inline:= false;
NilOpts.do_reduce:= false;
NilOpts.do_uncurry:= false;
NilOpts.do_flatten:= false;

NilOpts.print_anormalize:=false;
NilOpts.print_pre := false;

fun d unit  = Linkall.compile_prelude(false, "Preludes/Prelude.sml");

