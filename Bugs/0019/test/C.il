----------------------------------------
TM/any/bug3.sml.il
int2var adding 945  to  _262
int2var adding 937  to  _263
int2var adding 938  to  _264
int2var adding 898  to  _265
int2var adding 940  to  _266
int2var adding 939  to  _267
int2var adding 942  to  _268
int2var adding 903  to  _269
int2var adding 931  to  _270
int2var adding 900  to  _271
int2var adding 899  to  _272
int2var adding 930  to  _273
int2var adding 915  to  _274
int2var adding 914  to  _275
int2var adding 933  to  _276
int2var adding 932  to  _277
int2var adding 913  to  _278
int2var adding 912  to  _279
context:
          ---- Fixity ----
*            
*            ---- Overload ----
*            
*            ---- Pathmap ----
*            C_STR --> _262 : PC_MOD(_262: SIGS_SELF(_262, 
*                                             UNSELF_SIG = _263
*                                             SELF_SIG = [bar_TYC > _264 : 
**                                                           KIND(1 -> TYPE) = 
**        +1   CON_PATH(_265.foo_TYC), 
*                                                         bar_sumarg_INT > _266 : 
**                                                           TYPE = 
**        +1   CON_PATH(_265.foo_sumarg_INT), 
*                                                         bar_sum_INT > _267 : 
**                                                           KIND(1 -> TYPE) = 
**        +1   CON_PATH(_265.foo_sum_INT), 
*                                                         +O+Dbar_INT > _268 : 
**                                                           
**        +1   SIGS_SELF(_262.+O+Dbar_INT, 
**        +1      UNSELF_SIG = NONE
**        +1      SELF_SIG = [foo_INT > _269 : KIND(1 -> TYPE) == CON_PATH(_265.foo_TYC), 
**        +1                  expose_INT > _270 : 
***       +1                      $POLY$ SIGS_SELF(_265.+O+Dfoo_INT. expose_INT, 
***       +1                                UNSELF_SIG = NONE
***       +1                                SELF_SIG = SIGF(_271, ['a_TYV > _272 : TYPE] =>
***       +1                                                   [it_INT > _273 : 
****      +1                                                       
****      +2   (CON_APP(CON_PATH(_265.foo_TYC); CON_PATH(_271.'a_TYV)) =>
****      +2      CON_APP(CON_PATH(_265.foo_sum_INT); CON_PATH(_271.'a_TYV))) == /TOTALNONRECUR\_274
****      +2                                               (_275 : 
****      +2   CON_APP(CON_PATH(_265.foo_TYC);
****      +2      CON_PATH(_271.'a_TYV)))
****      +2                                               : CON_APP(
****      +2   CON_PATH(_265.foo_sum_INT);
****      +2                                                    CON_PATH(
****      +2   _271.'a_TYV)) =
****      +2                                            UNROLL(CON_APP(
****      +2   CON_PATH(_265.foo_TYC);
****      +2                                                      
****      +2   CON_PATH(_271.'a_TYV)),CON_APP(CON_PATH(_265.foo_sum_INT);
****      +3      CON_PATH(_271.'a_TYV)),_275)])
***       +1                                ), 
**        +1                  M > _276 : 
***       +1                      $POLY$ SIGS_SELF(_265.+O+Dfoo_INT. M, 
***       +1                                UNSELF_SIG = NONE
***       +1                                SELF_SIG = SIGF(_271, ['a_TYV > _272 : TYPE] =>
***       +1                                                   [it_INT > _277 : 
****      +1                                                       
****      +2   ({FLOAT64 *
****      +2     CON_PATH(_271.'a_TYV)} =>
****      +2      CON_APP(CON_PATH(_265.foo_TYC); CON_PATH(_271.'a_TYV))) == /TOTALNONRECUR\_278
****      +2                                           (_279 : {FLOAT64 *
****      +2                                                    CON_PATH(
****      +2   _271.'a_TYV)})
****      +2                                           : CON_APP(
****      +2   CON_PATH(_265.foo_TYC);
****      +2                                                CON_PATH(
****      +2   _271.'a_TYV)) =
****      +2                                        ROLL(CON_APP(
****      +2   CON_PATH(_265.foo_TYC);
****      +2                                                CON_PATH(
****      +2   _271.'a_TYV)),
****      +2   INJ(0, CON_APP(CON_PATH(_265.foo_sum_INT); CON_PATH(_271.'a_TYV)),_279))])
***       +1                                )]
**        +1      )]
*                                             ))
*            
free variables:
_263	B_SIG
_265	A_STR


PROC #0: Allocated  =     1358 kb
         Copied     =      292 kb
         Work       =      288 kw
                                Sum (s)        Count     Min(ms)   Avg(ms)  Max(ms)
         --------------|-----------------------------------------------------------
          Total        |      0.178
          Scheduler    |      0.002 ( 1.1%)        9        0.00      0.22     0.98
          Mutator      |      0.130 (73.1%)        8        0.00     16.23    41.97
          GCWork       |      0.046 (25.8%)        5        0.98      9.17    21.47

       MinSurvRate     0.082 <  0.224 <  0.319
       HeapSize(kb)     1024 <   1024 <   1024
GC:    GCMethod      =      Gen     Allocated    =      1358 kb   NumCopied    =    19019    MaxStkDepth  =  210
       StackMethod   =   Normal     Copied       =       292 kb   NumShared    =     5119    AvgStkDepth  =   77
       NumGC         =        3     NumRoot      =         0      NumConflict  =        0    AvgFrameSize =   33
       NumMajorGC    =        0     NumWrite     =     29419      NumLocative  =        0
MISC:  Total time    =     0.33 s   maxPhysMem   =      4464      minPageFault =      637    invCtxtSwap  =     8
       Total Threads =        1     sharedMem    =       280      majPageFault =        0    volCtxtSwap  =    54
       Max Threads   =        1     unsharedData =       710                                 swapping     =     0
                                    unsharedStk  =         5
