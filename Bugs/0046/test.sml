(*$import *)
structure S1 = struct datatype foo = FOO end
structure S2 = struct datatype foo = FOO end
val x = case S1.FOO of S2.FOO => 3
