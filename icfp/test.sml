local
  val tests = 
    ["checked-cube.gml",
     "reflect.gml",
     "scene4.gml",
     "spheres.gml" ,
     "dice.gml",
     "scene1.gml",
     "scene5.gml",
     "spheres2.gml",
     "fib.gml",
     "scene2.gml",
     "scene6.gml",
     "spotlight.gml",
     "golf.gml",
     "scene3.gml",
     "scene7.gml",
     "cone2.gml",
     "cylinder.gml",
     "cylinders.gml"
     ]
  val failed = ref []
in
  fun test () = 
    (app (fn s => (ignore (Top.runFile s) handle _ => failed := (s :: !failed))) tests;
     app (fn s => print ("Failed test "^s^"\n")) (!failed);
     failed := [])
end
