CM.make();


structure Load :> LOAD = 

struct


val sticky = LoadVars.sticky

fun make () = use "load2.sml";

fun toRtl () = (Stats.bool "UptoAsm" := false;Stats.bool "UptoRtl" := true;
		LoadVars.toRtl := true;LoadVars.toAsm := false;LoadVars.toExe := false)
fun toAsm () = (Stats.bool "UptoAsm" := true ;Stats.bool "UptoRtl" := false;
		LoadVars.toRtl := false;LoadVars.toAsm := true;LoadVars.toExe := false)
fun toExe () = (Stats.bool "UptoAsm" := false ;Stats.bool "UptoRtl" := false;
		LoadVars.toRtl := false;LoadVars.toAsm := false;LoadVars.toExe := true)

fun show b = 
  let in
    LoadVars.show := b;
    LinkIl.show_hil := b;
    Linkrtl.show_rtl := b;
    Stats.bool "showPhasesplit" := b;
    Stats.bool "showTypeofElim1"    := b;
    Stats.bool "showClosureConv" := b;
    Stats.bool "showOptimize1" := b;
    Stats.bool "showOptimize2" := b;
    Stats.bool "showOptimize3" := b;
    Stats.bool "showReify1" := b;
    Stats.bool "showReify2" := b;
    Stats.bool "showVararg" := b;
    Stats.bool "showSpecialize" := b;
    Stats.bool "showRename" := b;
    Stats.bool "showHoist" := b;
    Stats.bool "showInline1" := b;
    Stats.bool "showInline2" := b;
    Stats.bool "showInline3" := b
  end;

fun show_hil b = (LoadVars.show_hil := b;LinkIl.show_hil := b)

fun show_some b = 
  let in
    LoadVars.show_some := b;
    app (fn flag => flag := b) (!LoadVars.show_which)
  end;

fun typecheck b = (LoadVars.typecheck := b;
		   Stats.bool "Typecheck" := b)

fun typecheck_some b = (LoadVars.typecheck_some := b;
			app (fn flag => flag := b) (!LoadVars.typecheck_which)
			)

fun also_xxx pre which s = which := (Stats.bool (pre^s)) :: !which

fun dont_xxx pre which s = 
  let val b = Stats.bool (pre^s)
  in
    which := List.filter (fn b' => b <> b') (!which)
  end

val also_show      = also_xxx "show" LoadVars.show_which
val also_typecheck = also_xxx "check" LoadVars.typecheck_which
val dont_show      = dont_xxx "show" LoadVars.show_which
val dont_typecheck = dont_xxx "check" LoadVars.typecheck_which

val _ = 
  if !LoadVars.first then
    (
     LoadVars.show_which := [Stats.bool ("showPhasesplit"),
			     Stats.bool("showOptimize1"),
			     Stats.bool("showOptimize2"),
			     Stats.bool("showClosureConv")];
     LoadVars.typecheck_which := [Stats.bool ("checkPhasesplit"),
				  Stats.bool("checkOptimize1"),
				  Stats.bool("checkOptimize2"),
				  Stats.bool("checkClosureConv")];
     toExe();
     typecheck true;
     LoadVars.first := false
   )
  else if !LoadVars.sticky then
    (typecheck (!LoadVars.typecheck);
     typecheck_some (!LoadVars.typecheck_some);
     show_hil (!LoadVars.show_hil);
     show (!LoadVars.show);
     show_some (!LoadVars.show_some);
     if !LoadVars.toRtl then toRtl()
     else if !LoadVars.toAsm then toAsm()
     else if !LoadVars.toExe then toExe() else ()
       ) else ()

end

