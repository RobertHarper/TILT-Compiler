----------------------------------------
TM/any/bug2.sml.il
int2var adding 937  to  _262
int2var adding 938  to  _263
int2var adding 898  to  _264
int2var adding 940  to  _265
int2var adding 939  to  _266
int2var adding 942  to  _267
int2var adding 903  to  _268
int2var adding 931  to  _269
int2var adding 900  to  _270
int2var adding 899  to  _271
int2var adding 930  to  _272
int2var adding 915  to  _273
int2var adding 914  to  _274
int2var adding 933  to  _275
int2var adding 932  to  _276
int2var adding 913  to  _277
int2var adding 912  to  _278
context:
          ---- Fixity ----
*            
*            ---- Overload ----
*            
*            ---- Pathmap ----
*            B_SIG --> _262 : PC_SIG(_262 = [bar_TYC > _263 : 
**                                              KIND(1 -> TYPE) = CON_PATH(_264.foo_TYC), 
*                                            bar_sumarg_INT > _265 : 
**                                              TYPE = CON_PATH(_264.foo_sumarg_INT), 
*                                            bar_sum_INT > _266 : 
**                                              KIND(1 -> TYPE) = 
**        +1   CON_PATH(_264.foo_sum_INT), 
*                                            +O+Dbar_INT > _267 : 
**                                              [foo_INT > _268 : 
***                                                 KIND(1 -> TYPE) == 
***       +1   CON_PATH(_264.foo_TYC), 
**                                               expose_INT > _269 : 
***                                                  $POLY$ 
***       +1   SIGS_SELF(_264.+O+Dfoo_INT. expose_INT, 
***       +1      UNSELF_SIG = NONE
***       +1      SELF_SIG = SIGF(_270, ['a_TYV > _271 : TYPE] =>
***       +1                         [it_INT > _272 : 
****      +1                             (CON_APP(CON_PATH(_264.foo_TYC);
****      +1                                 CON_PATH(_270.'a_TYV)) =>
****      +1                                CON_APP(CON_PATH(_264.foo_sum_INT);
****      +1                                   CON_PATH(_270.'a_TYV))) == 
****      +2   /TOTALNONRECUR\_273
****      +2         (_274 : CON_APP(CON_PATH(_264.foo_TYC); CON_PATH(_270.'a_TYV)))
****      +2         : CON_APP(CON_PATH(_264.foo_sum_INT); CON_PATH(_270.'a_TYV)) =
****      +2      UNROLL(CON_APP(CON_PATH(_264.foo_TYC); CON_PATH(_270.'a_TYV)),
****      +3   CON_APP(CON_PATH(_264.foo_sum_INT);
****      +3      CON_PATH(_270.'a_TYV)),_274)])
***       +1      ), 
**                                               M > _275 : 
***                                                  $POLY$ 
***       +1   SIGS_SELF(_264.+O+Dfoo_INT. M, 
***       +1      UNSELF_SIG = NONE
***       +1      SELF_SIG = SIGF(_270, ['a_TYV > _271 : TYPE] =>
***       +1                         [it_INT > _276 : 
****      +1                             ({FLOAT64 *
****      +1                               CON_PATH(_270.'a_TYV)} =>
****      +1                                CON_APP(CON_PATH(_264.foo_TYC);
****      +1                                   CON_PATH(_270.'a_TYV))) == 
****      +2   /TOTALNONRECUR\_277
****      +2         (_278 : {FLOAT64 *
****      +2                  CON_PATH(_270.'a_TYV)})
****      +2         : CON_APP(CON_PATH(_264.foo_TYC); CON_PATH(_270.'a_TYV)) =
****      +2      ROLL(CON_APP(CON_PATH(_264.foo_TYC); CON_PATH(_270.'a_TYV)),
****      +3   INJ(0, CON_APP(CON_PATH(_264.foo_sum_INT); CON_PATH(_270.'a_TYV)),_278))])
***       +1      )]])
*            
free variables:
_264	A_STR


PROC #0: Allocated  =     1293 kb
         Copied     =      257 kb
         Work       =      237 kw
                                Sum (s)        Count     Min(ms)   Avg(ms)  Max(ms)
         --------------|-----------------------------------------------------------
          Total        |      0.167
          Scheduler    |      0.001 ( 0.6%)        7        0.00      0.14     0.98
          Mutator      |      0.126 (75.4%)        6        0.00     20.98    43.92
          GCWork       |      0.040 (24.0%)        3        2.93     13.34    20.50

       MinSurvRate     0.270 <  0.296 <  0.321
       HeapSize(kb)     1024 <   1024 <   1024
GC:    GCMethod      =      Gen     Allocated    =      1293 kb   NumCopied    =    16477    MaxStkDepth  =   23
       StackMethod   =   Normal     Copied       =       257 kb   NumShared    =     4888    AvgStkDepth  =   13
       NumGC         =        2     NumRoot      =         0      NumConflict  =        0    AvgFrameSize =   42
       NumMajorGC    =        0     NumWrite     =     19666      NumLocative  =        0
MISC:  Total time    =     0.31 s   maxPhysMem   =      4432      minPageFault =      633    invCtxtSwap  =     8
       Total Threads =        1     sharedMem    =       264      majPageFault =        0    volCtxtSwap  =    64
       Max Threads   =        1     unsharedData =       665                                 swapping     =     0
                                    unsharedStk  =         5
