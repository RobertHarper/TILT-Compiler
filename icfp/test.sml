local
  val tests = 
    ["scene1.gml",
     "scene2.gml",
     "scene3.gml",
     "scene4.gml",
     "scene5.gml",
     "scene6.gml",
     "scene7.gml",

     "spheres.gml" ,
     "spheres2.gml",
     "reflect.gml",
     "checked-cube.gml",
     "fib.gml",
     "dice.gml",
     "spotlight.gml",
     "golf.gml",

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
