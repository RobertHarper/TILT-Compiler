SBNDS:
[A_STR > strbindvar_68261 = 
    STR[a_TYC > a_68262 = {INT32 *
                           INT32 *
                           INT32 *
                           INT32}, 
        +Ea_INT > _68275 = 
           LET bool_68264 = CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0
               true_68265 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(1, SUM[(false, true); 2; ()]))
               false_68266 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(0, SUM[(false, true); 2; ()]))
               name_68267 = {INT32 *
                             INT32 *
                             INT32 *
                             INT32}
           IN  /TOTALNONRECUR\anonfun_68274
                     (_68268 : {name_68267 *
                                name_68267})
                     : bool_68264 =
               LET _68269 = (_68268)#1_~48
                   _68270 = (_68268)#2_~48
               IN  CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                     SUM[(false, true); 2; ()],
                                     CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                                       SUM[(false, true); 2; ()],
                                                       CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                                                         SUM[(false, true); 2; ()],
                                                                         eqI32[][(_68269)#1_~48, (_68270)#1_~48]):
                                                            SUM[(false, true); 2; ()],
                                                            resultType = bool_68264,
                                                            boundVar = unused_68271,
                                                            0: false_68266,
                                                            1: eqI32[][(_68269)#2_~48, (_68270)#2_~48],
                                                            Default: ---)):
                                          SUM[(false, true); 2; ()],
                                          resultType = bool_68264,
                                          boundVar = unused_68272,
                                          0: false_68266,
                                          1: eqI32[][(_68269)#3_~48, (_68270)#3_~48],
                                          Default: ---)):
                        SUM[(false, true); 2; ()],
                        resultType = bool_68264,
                        boundVar = unused_68273,
                        0: false_68266,
                        1: eqI32[][(_68269)#4_~48, (_68270)#4_~48],
                        Default: ---)
               END
           END], 
 A2_STR > strbindvar_68279 = 
    MOD_LET origModule_68280 = strbindvar_68261
    IN  MOD_LET coercedModule_68282 = STR[a_TYC > copy_a_TYC_68281 = CON_PATH(origModule_68280.a_TYC)]
        IN  STR[hiddenThinModule_INT > thinModule_68283 = STR[], 
                a_TYC > copy_a_TYC_68281 = CON_PATH(coercedModule_68282.a_TYC)]
        END
    END, 
 ok_TYC > ok_68284 = CON_PATH(strbindvar_68261.a_TYC), 
 +Eok_INT > _68290 = 
    LET bool_68286 = CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0
        true_68287 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(1, SUM[(false, true); 2; ()]))
        false_68288 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(0, SUM[(false, true); 2; ()]))
    IN  strbindvar_68261.+Ea_INT
    END, 
 notok_TYC > notok_68291 = CON_PATH(strbindvar_68279.a_TYC), 
 +Enotok_INT > _68305 = 
    LET bool_68293 = CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0
        true_68294 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(1, SUM[(false, true); 2; ()]))
        false_68295 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(0, SUM[(false, true); 2; ()]))
        name_68297 = {INT32 *
                      INT32 *
                      INT32 *
                      INT32}
    IN  /TOTALNONRECUR\anonfun_68304
              (_68298 : {name_68297 *
                         name_68297})
              : bool_68293 =
        LET _68299 = (_68298)#1_~48
            _68300 = (_68298)#2_~48
        IN  CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                              SUM[(false, true); 2; ()],
                              CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                                SUM[(false, true); 2; ()],
                                                CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                                                  SUM[(false, true); 2; ()],
                                                                  eqI32[][(_68299)#1_~48, (_68300)#1_~48]):
                                                     SUM[(false, true); 2; ()],
                                                     resultType = bool_68293,
                                                     boundVar = unused_68301,
                                                     0: false_68295,
                                                     1: eqI32[][(_68299)#2_~48, (_68300)#2_~48],
                                                     Default: ---)):
                                   SUM[(false, true); 2; ()],
                                   resultType = bool_68293,
                                   boundVar = unused_68302,
                                   0: false_68295,
                                   1: eqI32[][(_68299)#3_~48, (_68300)#3_~48],
                                   Default: ---)):
                 SUM[(false, true); 2; ()],
                 resultType = bool_68293,
                 boundVar = unused_68303,
                 0: false_68295,
                 1: eqI32[][(_68299)#4_~48, (_68300)#4_~48],
                 Default: ---)
        END
    END, 
 alsoNotOk_FCT > functor_var_68322 = 
    FUNC(funct_arg_68306: A2_68276 ->
    STR[alsonotok_TYC > alsonotok_68307 = CON_PATH(funct_arg_68306.a_TYC), 
        +Ealsonotok_INT > _68321 = 
           LET bool_68309 = CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0
               true_68310 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(1, SUM[(false, true); 2; ()]))
               false_68311 = ROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0, INJ(0, SUM[(false, true); 2; ()]))
               name_68313 = {INT32 *
                             INT32 *
                             INT32 *
                             INT32}
           IN  /TOTALNONRECUR\anonfun_68320
                     (_68314 : {name_68313 *
                                name_68313})
                     : bool_68309 =
               LET _68315 = (_68314)#1_~48
                   _68316 = (_68314)#2_~48
               IN  CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                     SUM[(false, true); 2; ()],
                                     CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                                       SUM[(false, true); 2; ()],
                                                       CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                                                         SUM[(false, true); 2; ()],
                                                                         eqI32[][(_68315)#1_~48, (_68316)#1_~48]):
                                                            SUM[(false, true); 2; ()],
                                                            resultType = bool_68309,
                                                            boundVar = unused_68317,
                                                            0: false_68311,
                                                            1: eqI32[][(_68315)#2_~48, (_68316)#2_~48],
                                                            Default: ---)):
                                          SUM[(false, true); 2; ()],
                                          resultType = bool_68309,
                                          boundVar = unused_68318,
                                          0: false_68311,
                                          1: eqI32[][(_68315)#3_~48, (_68316)#3_~48],
                                          Default: ---)):
                        SUM[(false, true); 2; ()],
                        resultType = bool_68309,
                        boundVar = unused_68319,
                        0: false_68311,
                        1: eqI32[][(_68315)#4_~48, (_68316)#4_~48],
                        Default: ---)
               END
           END]
    : 
    [alsonotok_TYC > alsonotok_68307 : TYPE = CON_PATH(funct_arg_68306.a_TYC), 
     +Ealsonotok_INT > _68321 : ({CON_PATH(funct_arg_68306.a_TYC) *
                                  CON_PATH(funct_arg_68306.a_TYC)} -> CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)])]
ENTRIES:
CONTEXT_SDEC: A_STR > strbindvar_68261 : 
                 [a_TYC > a_68262 : TYPE = {INT32 *
                                            INT32 *
                                            INT32 *
                                            INT32}, 
                  +Ea_INT > _68275 : ({{INT32 *
                                        INT32 *
                                        INT32 *
                                        INT32} *
                                       {INT32 *
                                        INT32 *
                                        INT32 *
                                        INT32}} -> CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)]
CONTEXT_SIGNAT: A2_SIG > A2_68276 = [a_TYC > a_68277 : TYPE = {INT32 *
                                                               INT32 *
                                                               INT32 *
                                                               INT32}]
CONTEXT_SDEC: A2_STR > strbindvar_68279 : [hiddenThinModule_INT > thinModule_68283 : [], 
                                           a_TYC > copy_a_TYC_68281 : TYPE = {INT32 *
                                                                              INT32 *
                                                                              INT32 *
                                                                              INT32}]
CONTEXT_SDEC: ok_TYC > ok_68284 : TYPE = CON_PATH(strbindvar_68261.a_TYC)
CONTEXT_SDEC: +Eok_INT > _68290 : 
                 ({CON_PATH(strbindvar_68261.a_TYC) *
                   CON_PATH(strbindvar_68261.a_TYC)} -> CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)
CONTEXT_SDEC: notok_TYC > notok_68291 : TYPE = CON_PATH(strbindvar_68279.a_TYC)
CONTEXT_SDEC: +Enotok_INT > _68305 : 
                 ({CON_PATH(strbindvar_68279.a_TYC) *
                   CON_PATH(strbindvar_68279.a_TYC)} -> CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)
CONTEXT_SDEC: alsoNotOk_FCT > functor_var_68322 : 
                 SIGF(funct_arg_68306, A2_68276 ->
                         [alsonotok_TYC > alsonotok_68307 : TYPE = CON_PATH(funct_arg_68306.a_TYC), 
                          +Ealsonotok_INT > _68321 : 
                             ({CON_PATH(funct_arg_68306.a_TYC) *
                               CON_PATH(funct_arg_68306.a_TYC)} -> CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)])
