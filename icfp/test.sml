let
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
     "scene7.gml"
     ]
in
  app (fn s => (ignore (Top.runFile s) handle _ => print ("Failed test "^s^"\n"))) tests
end
