----------------------------------------
TM/any/bug.sml.il
int2var adding 898  to  _262
int2var adding 901  to  _263
int2var adding 917  to  _264
int2var adding 910  to  _265
int2var adding 906  to  _266
int2var adding 934  to  _267
int2var adding 907  to  _268
int2var adding 935  to  _269
int2var adding 905  to  _270
int2var adding 903  to  _271
int2var adding 931  to  _272
int2var adding 900  to  _273
int2var adding 899  to  _274
int2var adding 930  to  _275
int2var adding 915  to  _276
int2var adding 914  to  _277
int2var adding 933  to  _278
int2var adding 932  to  _279
int2var adding 913  to  _280
int2var adding 912  to  _281
context:
          ---- Fixity ----
*            
*            ---- Overload ----
*            
*            ---- Pathmap ----
*            A_STR --> _262 : PC_MOD(_262: SIGS_SELF(_262, 
*                                             UNSELF_SIG = NONE
*                                             SELF_SIG = [foo_TYC > _263 : 
**                                                           KIND(1 -> TYPE) = 
**        +1   /-\(_264)CON_MU(/-\(_265)(SUM[ (M); 0; {FLOAT64 *
**        +1                                           _264}]))#0, 
*                                                         foo_sumarg_INT > _266 : 
**                                                           TYPE = /-\(_267){FLOAT64 *
**                                                                            _267}, 
*                                                         foo_sum_INT > _268 : 
**                                                           KIND(1 -> TYPE) = 
**        +1   /-\(_269)SUM[ (M); 0; CON_APP(CON_PATH(_262.foo_sumarg_INT); _269)], 
*                                                         +O+Dfoo_INT > _270 : 
**                                                           
**        +1   SIGS_SELF(_262.+O+Dfoo_INT, 
**        +1      UNSELF_SIG = NONE
**        +1      SELF_SIG = [foo_INT > _271 : KIND(1 -> TYPE) == CON_PATH(_262.foo_TYC), 
**        +1                  expose_INT > _272 : 
***       +1                      $POLY$ SIGS_SELF(_262.+O+Dfoo_INT. expose_INT, 
***       +1                                UNSELF_SIG = NONE
***       +1                                SELF_SIG = SIGF(_273, ['a_TYV > _274 : TYPE] =>
***       +1                                                   [it_INT > _275 : 
****      +1                                                       
****      +2   (CON_APP(CON_PATH(_262.foo_TYC); CON_PATH(_273.'a_TYV)) =>
****      +2      CON_APP(CON_PATH(_262.foo_sum_INT); CON_PATH(_273.'a_TYV))) == /TOTALNONRECUR\_276
****      +2                                               (_277 : 
****      +2   CON_APP(CON_PATH(_262.foo_TYC);
****      +2      CON_PATH(_273.'a_TYV)))
****      +2                                               : CON_APP(
****      +2   CON_PATH(_262.foo_sum_INT);
****      +2                                                    CON_PATH(
****      +2   _273.'a_TYV)) =
****      +2                                            UNROLL(CON_APP(
****      +2   CON_PATH(_262.foo_TYC);
****      +2                                                      
****      +2   CON_PATH(_273.'a_TYV)),CON_APP(CON_PATH(_262.foo_sum_INT);
****      +3      CON_PATH(_273.'a_TYV)),_277)])
***       +1                                ), 
**        +1                  M > _278 : 
***       +1                      $POLY$ SIGS_SELF(_262.+O+Dfoo_INT. M, 
***       +1                                UNSELF_SIG = NONE
***       +1                                SELF_SIG = SIGF(_273, ['a_TYV > _274 : TYPE] =>
***       +1                                                   [it_INT > _279 : 
****      +1                                                       
****      +2   ({FLOAT64 *
****      +2     CON_PATH(_273.'a_TYV)} =>
****      +2      CON_APP(CON_PATH(_262.foo_TYC); CON_PATH(_273.'a_TYV))) == /TOTALNONRECUR\_280
****      +2                                           (_281 : {FLOAT64 *
****      +2                                                    CON_PATH(
****      +2   _273.'a_TYV)})
****      +2                                           : CON_APP(
****      +2   CON_PATH(_262.foo_TYC);
****      +2                                                CON_PATH(
****      +2   _273.'a_TYV)) =
****      +2                                        ROLL(CON_APP(
****      +2   CON_PATH(_262.foo_TYC);
****      +2                                                CON_PATH(
****      +2   _273.'a_TYV)),
****      +2   INJ(0, CON_APP(CON_PATH(_262.foo_sum_INT); CON_PATH(_273.'a_TYV)),_281))])
***       +1                                )]
**        +1      )]
*                                             ))
*            
free variables:


PROC #0: Allocated  =     1397 kb
         Copied     =      299 kb
         Work       =      332 kw
                                Sum (s)        Count     Min(ms)   Avg(ms)  Max(ms)
         --------------|-----------------------------------------------------------
          Total        |      0.183
          Scheduler    |      0.002 ( 1.1%)        9        0.00      0.22     0.98
          Mutator      |      0.133 (72.7%)        8        0.00     16.59    44.90
          GCWork       |      0.048 (26.2%)        5        0.98      9.56    21.47

       MinSurvRate     0.097 <  0.229 <  0.319
       HeapSize(kb)     1024 <   1024 <   1024
GC:    GCMethod      =      Gen     Allocated    =      1397 kb   NumCopied    =    19342    MaxStkDepth  =  514
       StackMethod   =   Normal     Copied       =       299 kb   NumShared    =     5509    AvgStkDepth  =  189
       NumGC         =        3     NumRoot      =         0      NumConflict  =        0    AvgFrameSize =   32
       NumMajorGC    =        0     NumWrite     =     28079      NumLocative  =        0
MISC:  Total time    =     0.35 s   maxPhysMem   =      4480      minPageFault =      639    invCtxtSwap  =     8
       Total Threads =        1     sharedMem    =       282      majPageFault =        0    volCtxtSwap  =    88
       Max Threads   =        1     unsharedData =       716                                 swapping     =     0
                                    unsharedStk  =         5
