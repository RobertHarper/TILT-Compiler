CM.make();


structure Load :> LOAD = 

struct


val sticky = LoadVars.sticky

fun make () = use "load2.sml";

fun toAsm () = (Stats.bool "UptoAsm" := true ;
		LoadVars.toAsm := true;LoadVars.toExe := false)
fun toExe () = (Stats.bool "UptoAsm" := false ;
		LoadVars.toAsm := false;LoadVars.toExe := true)

fun show b = 
  let in
    LoadVars.show := b;
    LinkIl.ShowHIL := b;
    Linkrtl.ShowRtl := b;
    Stats.bool "ShowNil" := b
  end;

fun show_hil b = (LoadVars.show_hil := b;LinkIl.ShowHIL := b)


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
fun dont which s = which := List.filter (fn s' => s <> s') (!which)

val also_show      = also LoadVars.show_which
val also_typecheck = also LoadVars.typecheck_which
val dont_show      = dont LoadVars.show_which
val dont_typecheck = dont LoadVars.typecheck_which

val default_which = ["Phasesplit",
		     "Optimize1",
		     "Optimize2",
		     "ClosureConv"]
fun reset () = 
    (
     LoadVars.show_which := default_which;
     LoadVars.typecheck_which := default_which;
     toExe();
     typecheck_some true;
     show_some false;
     typecheck false;
     show false;
     show_hil false
   )

val _ = 
  if !LoadVars.first then
    (
     reset();
     LoadVars.first := false
   )
  else if !LoadVars.sticky then
    (if !LoadVars.typecheck then typecheck true else ();
     if !LoadVars.typecheck_some then typecheck_some true else ();
     if !LoadVars.show_hil then show_hil true else ();
     if !LoadVars.show then show true else ();
     if !LoadVars.show_some then show_some true else ();
     if !LoadVars.toAsm then toAsm()
     else if !LoadVars.toExe then toExe() else ()
       ) else ()

end

