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


fun typecheck b = (LoadVars.typecheck := b;
		   Stats.bool "Typecheck" := b)


fun do_some (flag,which,pre) b =   
  let in
    flag := b;
    app (fn phase => Stats.bool (pre^phase) := b) (!which)
  end;

val show_some      = do_some (LoadVars.show_some,LoadVars.show_which,"show")
val typecheck_some = do_some (LoadVars.typecheck_some,LoadVars.typecheck_which,"check")

fun also which s = which := s :: !which
fun dont which s =     which := List.filter (fn s' => s <> s') (!which)

val also_show      = also LoadVars.show_which
val also_typecheck = also LoadVars.typecheck_which
val dont_show      = dont LoadVars.show_which
val dont_typecheck = dont LoadVars.typecheck_which

val default_which = ["Phasesplit",
		     "Optimize1",
		     "Optimize2",
		     "ClosureConv"]
val _ = 
  if !LoadVars.first then
    (
     LoadVars.show_which := default_which;
     LoadVars.typecheck_which := default_which;
     toExe();
     typecheck true;
     LoadVars.first := false
   )
  else if !LoadVars.sticky then
    ((if !LoadVars.typecheck then typecheck true
      else if !LoadVars.typecheck_some then typecheck_some true
      else ());
     (if !LoadVars.show then show true
      else if !LoadVars.show_some then show_some true
      else if !LoadVars.show_hil then show_hil true 
      else ());
     (if !LoadVars.toRtl then toRtl()
      else if !LoadVars.toAsm then toAsm()
      else if !LoadVars.toExe then toExe() else ()))
  else ()

end

