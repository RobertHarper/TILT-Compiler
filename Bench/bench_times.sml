(*$import Time Timer List String life leroy fft boyer simple tyan msort pia frank lexgen *)

val stringem = String.concat
val printem = List.app print

type result = {name : string, 
	       cpu : {usr : Time.time, sys : Time.time, gc : Time.time},
	       real : Time.time}

fun indent' n = 
  let 
    fun loop 0 acc = acc
      | loop n acc = loop (n-1) (#" "::acc)
    val l = loop n []
  in String.implode l
  end


fun indent 0 = ()
  | indent n = (print " ";indent (n-1))

fun timerToString ind {usr : Time.time, sys : Time.time, gc : Time.time} = 
  (stringem [indent' ind,indent' ind,"usr=    ",Time.toString usr,"\n",
	     indent' ind,indent' ind,"sys=    ",Time.toString sys,"\n",
	     indent' ind,indent' ind,"gc=     ",Time.toString gc,"\n",
	     indent' ind,"total=  ",Time.toString(Time.+(usr,sys)),"\n"])  (*User time includes gc*)
	     
fun print_result ({name,cpu,real} : result) : unit = 
  let
    val ind = 10
  in
    printem ["Timings for ",name,":\n"];
    print (timerToString ind cpu);
    indent ind;printem ["real =",Time.toString real,"\n"]
  end
    

fun run (f,name) : result = 
  let
    val _ = printem ["\n=============Running ",name," ======================\n"]
    val cpu_timer = Timer.startCPUTimer()
    val real_timer = Timer.startRealTimer()
    val _ = f ()
    val cpu =  Timer.checkCPUTimer cpu_timer
    val real =  Timer.checkRealTimer real_timer
  in {name = name,cpu = cpu,real=real}
  end

val benchmarks = 
  [(runLife,"Life"),
   (runLeroy,"Leroy"),
   (runFft,"Fft"),
   (runBoyer,"Boyer"),
   (runSimple,"Simple"),
   (runTyan,"Tyan"),
   (runTyan1,"Tyan1"),
   (runTyan2,"Tyan2"),
   (runTyan3,"Tyan3"),
   (runMsort,"Msort"),
   (runPia,"Pia"),
(* Data files don't exist.
 * (runPia1,"Pia1"),
 *   (runPia2,"Pia2"),
 *  (runPia3,"Pia3"),
 *) 
   (runLexgen,"Lexgen"),
(*  Data files don't exist
 *  (runLexgen1,"Lexgen1"),
 *  (runLexgen2,"Lexgen2"),
 *  (runLexgen3,"Lexgen3"),
 *)
   (runFrank,"Frank")
 
   ]

val results = map run benchmarks
val _ = app print_result results
