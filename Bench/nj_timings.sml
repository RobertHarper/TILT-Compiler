(* Top level declarations make CM fall over, so 
 * we use a use file
 *)

use "run.sig.sml";
use "nj-help.sml";
use "life.sml";
use "leroy.sml";
use "fft.sml";
use "boyer.sml";
use "simple.sml";
use "tyan-nj.sml";
use "msort.sml";
use "pia.sml";
use "lexgen.sml";
use "frank.sml";
use "f_arithmetic.sml";
use "arithmetic.sml";
use "arithmetic32.sml";
use "pqueens.sml";
(*use "nj_quicksort.sml";
use "nj_quicksort2.sml";*)
use "takc.sml";
use "taku.sml";
use "time_and_run.sig.sml";
use "time_and_run.sml";

structure NJRun :> RUN = 
  struct
    val benchmarks = 
      [(Life.run,"Life"),
       (Leroy.run,"Leroy"),
(*       (Fft.run,"Fft"),*)
(*       (Boyer.run,"Boyer"),*)
       (Simple.run,"Simple"),
       (Tyan.run,"Tyan"),
       (Msort.run,"Msort"),
       (Pia.run,"Pia"),
       (Lexgen.run,"Lexgen"),
(*       (Frank.run,"Frank"),*)
       (F_Arithmetic.run,"F_Arithmetic"),
       (Arithmetic.run,"Arithmetic"),
       (Arithmetic32.run,"Arithmetic32"),
       (PQueens.run,"PQueens"),
(*       (Quicksort.run,"Quicksort"),*)
(*       (Quicksort2.run,"Quicksort2"),*)
       (Takc.run,"Takc"),
       (Taku.run,"Taku")
       ]
      
    fun run () = TimeAndRun.report (3,benchmarks);
  end