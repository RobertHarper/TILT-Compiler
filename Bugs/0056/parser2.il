*****     +1                  $POLY$ FUNC(mpoly_var_1233: ['a_TYV > 'a_1232 : TYPE] =>
*****     +1                         STR[it_INT > _1283 = 
******    +1                                /TOTALNONRECUR\anonfun_1251
******    +1                                      (injectee_1250 : {CON_PATH(_term_1197.term_TYC) *
******    +1                                                        CON_PATH(mpoly_var_1233.'a_TYV)})
******    +1                                      : CON_APP(token_1234; CON_PATH(mpoly_var_1233.'a_TYV)) =
******    +1                                COERCE(token_in_1246,
******    +1                                       [CON_PATH(mpoly_var_1233.'a_TYV)],
******    +1                                       INJ(0, CON_APP(token_sum_1243; CON_PATH(mpoly_var_1233.'a_TYV)), injectee_1250))]
*****     +1                         : 
*****     +1                         [it_INT > _1283 : 
******    +1                             ({CON_PATH(_term_1197.term_TYC) *
******    +1                               CON_PATH(mpoly_var_1233.'a_TYV)} => CON_APP(token_1234; CON_PATH(mpoly_var_1233.'a_TYV))) == 
******    +1                                /TOTALNONRECUR\anonfun_1251
******    +1                                      (injectee_1250 : {CON_PATH(_term_1197.term_TYC) *
******    +1                                                        CON_PATH(mpoly_var_1233.'a_TYV)})
******    +1                                      : CON_APP(token_1234; CON_PATH(mpoly_var_1233.'a_TYV)) =
******    +1                                COERCE(token_in_1246,
******    +1                                       [CON_PATH(mpoly_var_1233.'a_TYV)],
******    +1                                       INJ(0, CON_APP(token_sum_1243; CON_PATH(mpoly_var_1233.'a_TYV)), injectee_1250))])]]
***                                                     END,
***                                                     [token_TYC > token_1234 : KIND(1 -> TYPE), 
***                                                      token_sumarg_INT > token_sumarg_1242 : 
****                                                        TYPE = 
****                                                           /-\('a_1288){CON_PATH(_term_1197.term_TYC) *
****                                                                        'a_1288}, 
***                                                      token_sum_INT > token_sum_1243 : 
****                                                        KIND(1 -> TYPE) = 
****                                                           /-\('a_1289)SUM[(TOKEN); 0; CON_APP(token_sumarg_1242; 'a_1289)], 
***                                                      +Ntoken_in_INT > token_in_1246 : 
****                                                        CON_COERCION(['a_1232],
****                                                                     CON_APP(token_sum_1243; 'a_1232),
****                                                                     CON_APP(token_1234; 'a_1232)), 
***                                                      +Ntoken_out_INT > token_out_1247 : 
****                                                        CON_COERCION(['a_1232],
****                                                                     CON_APP(token_1234; 'a_1232),
****                                                                     CON_APP(token_sum_1243; 'a_1232)), 
***                                                      +Etoken_INT > poly+Etoken_INT_1286 : 
****                                                         $POLY$ SIGF(mpoly_var_1233, ['a_TYV > 'a_1232 : TYPE, 
****                                                                                      +E'a_INT > _1249 : 
*****                                                                                        ({'a_1232 *
*****                                                                                          'a_1232} ->
*****                                                                                        CON_MU(/-\(_259)
*****                                                                                               (SUM[(false, true); 2; ()]))#0)] =>
****                                                                        [it_INT > +Etoken_INT_1285 : 
*****                                                                           ({CON_APP(token_1234;
*****                                                                                     CON_PATH(mpoly_var_1233.'a_TYV)) *
*****                                                                             CON_APP(token_1234;
*****                                                                                     CON_PATH(mpoly_var_1233.'a_TYV))} ->
*****                                                                           CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)]), 
***                                                      +O+Dtoken_INT > token_1241 : 
****                                                        
****      +1   [token_INT > copy_token_1236 : KIND(1 -> TYPE) == token_1234, 
****      +1    expose_INT > exposer_mod_1282 : 
*****     +1        $POLY$ SIGF(mpoly_var_1233, ['a_TYV > 'a_1232 : TYPE] =>
*****     +1                       [it_INT > exposer_1281 : 
******    +1                           (CON_APP(token_1234; CON_PATH(mpoly_var_1233.'a_TYV)) =>
******    +1                           CON_APP(token_sum_1243; CON_PATH(mpoly_var_1233.'a_TYV))) == 
******    +1                              /TOTALNONRECUR\anonfun_1253
******    +1                                    (exposee_1252 : CON_APP(token_1234; CON_PATH(mpoly_var_1233.'a_TYV)))
******    +1                                    : CON_APP(token_sum_1243; CON_PATH(mpoly_var_1233.'a_TYV)) =
******    +1                              COERCE(token_out_1247, [CON_PATH(mpoly_var_1233.'a_TYV)], exposee_1252)]), 
****      +1    TOKEN > _1284 : 
*****     +1        $POLY$ SIGF(mpoly_var_1233, ['a_TYV > 'a_1232 : TYPE] =>
*****     +1                       [it_INT > _1283 : 
******    +1                           ({CON_PATH(_term_1197.term_TYC) *
******    +1                             CON_PATH(mpoly_var_1233.'a_TYV)} => CON_APP(token_1234; CON_PATH(mpoly_var_1233.'a_TYV))) == 
******    +1                              /TOTALNONRECUR\anonfun_1251
******    +1                                    (injectee_1250 : {CON_PATH(_term_1197.term_TYC) *
******    +1                                                      CON_PATH(mpoly_var_1233.'a_TYV)})
******    +1                                    : CON_APP(token_1234; CON_PATH(mpoly_var_1233.'a_TYV)) =
******    +1                              COERCE(token_in_1246,
******    +1                                     [CON_PATH(mpoly_var_1233.'a_TYV)],
******    +1                                     INJ(0, CON_APP(token_sum_1243; CON_PATH(mpoly_var_1233.'a_TYV)), injectee_1250))])]]), 
**                                          -X+Cworks__INT > cluster_1319 = 
***                                             $POLY$ FUNC(var_poly_1290: ['a_TYV > 'a_TYV_1320 : TYPE] =>
***                                                    STR[them_INT > cluster_inner_1322 = 
****                                                          
****      +1   /\works_1293
****      +1         (mvar_1294 : {CON_PATH(_term_1197.term_TYC) *
****      +1                       CON_APP(CON_PATH(_token_1239.token_TYC); CON_PATH(var_poly_1290.'a_TYV))})
****      +1         : CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0 =
****      +1   LET shortResultType_1297 = CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0
****      +1       _1298 = (mvar_1294)#1_~48
****      +1       _1299 = (mvar_1294)#2_~48
****      +1   IN  CASE(arg = COERCE(_token_1239.+Ntoken_out_INT,
****      +1                         [CON_MPROJ(STR['a_TYV > 'a_1232 = CON_PATH(var_poly_1290.'a_TYV)], 'a_TYV)],
****      +1                         _1299):
****      +1            CON_APP(CON_PATH(_token_1239.token_sum_INT); CON_PATH(var_poly_1290.'a_TYV)),
****      +1            resultType = shortResultType_1297,
****      +1            boundVar = sumswitch_arg_1304,
****      +1            0: LET _1307 = 
****      +1                      SUM_TAIL(CON_APP(CON_PATH(_token_1239.token_sum_INT); CON_PATH(var_poly_1290.'a_TYV)),
****      +1                               sumswitch_arg_1304)
****      +1                   _1308 = (_1307)#1_~48
****      +1                   _1309 = (_1307)#2_~48
****      +1               IN  APP(_term_1197.+Eterm_INT, (_1298, _1308))
****      +1               END,
****      +1            Default: ---)
****      +1   END]
***                                                    : 
***                                                    [them_INT > cluster_inner_1322 : 
****                                                       ({CON_PATH(_term_1197.term_TYC) *
****                                                         CON_APP(CON_PATH(_token_1239.token_TYC);
****                                                                 CON_PATH(var_poly_1290.'a_TYV))} ->
****                                                       CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)]), 
**                                          works > works_1293 = 
***                                             $POLY$ FUNC(var_poly_1290: ['a_TYV > 'a_TYV_1320 : TYPE] =>
***                                                    STR[it_INT > works_inner_1323 = MAPP(cluster_1319,  var_poly_1290).them_INT]
***                                                    : 
***                                                    [it_INT > works_inner_1323 : 
****                                                       ({CON_PATH(_term_1197.term_TYC) *
****                                                         CON_APP(CON_PATH(_token_1239.token_TYC);
****                                                                 CON_PATH(var_poly_1290.'a_TYV))} ->
****                                                       CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)])]
**            IN  MOD_LET coercedModule_1355 = STR[term_TYC > copy_term_TYC_1325 = CON_PATH(origModule_1324.+O_term_INT. term_TYC), 
**                                                 term_sumarg_INT > copy_term_sumarg_INT_1326 = 
***                                                   CON_PATH(origModule_1324.+O_term_INT. term_sumarg_INT), 
**                                                 term_sum_INT > copy_term_sum_INT_1327 = 
***                                                   CON_PATH(origModule_1324.+O_term_INT. term_sum_INT), 
**                                                 +Nterm_in_INT > copy_+Nterm_in_INT_1328 = 
***                                                   MPROJ(origModule_1324,+O_term_INT).+Nterm_in_INT, 
**                                                 +Nterm_out_INT > copy_+Nterm_out_INT_1329 = 
***                                                   MPROJ(origModule_1324,+O_term_INT).+Nterm_out_INT, 
**                                                 +Eterm_INT > copy_+Eterm_INT_1330 = 
***                                                   MPROJ(origModule_1324,+O_term_INT).+Eterm_INT, 
**                                                 +O+Dterm_INT > copy_+O+Dterm_INT_1334 = 
***                                                   MPROJ(MPROJ(origModule_1324,+O_term_INT),+O+Dterm_INT), 
**                                                 token_TYC > copy_token_TYC_1335 = 
***                                                   CON_PATH(origModule_1324.+O_token_INT. token_TYC), 
**                                                 token_sumarg_INT > copy_token_sumarg_INT_1336 = 
***                                                   CON_PATH(origModule_1324.+O_token_INT. token_sumarg_INT), 
**                                                 token_sum_INT > copy_token_sum_INT_1337 = 
***                                                   CON_PATH(origModule_1324.+O_token_INT. token_sum_INT), 
**                                                 +Ntoken_in_INT > copy_+Ntoken_in_INT_1338 = 
***                                                   MPROJ(origModule_1324,+O_token_INT).+Ntoken_in_INT, 
**                                                 +Ntoken_out_INT > copy_+Ntoken_out_INT_1339 = 
***                                                   MPROJ(origModule_1324,+O_token_INT).+Ntoken_out_INT, 
**                                                 +Etoken_INT > copy_+Etoken_INT_1340 = 
***                                                    $POLY$ MPROJ(MPROJ(origModule_1324,+O_token_INT),+Etoken_INT), 
**                                                 +O+Dtoken_INT > copy_+O+Dtoken_INT_1354 = 
***                                                   MPROJ(MPROJ(origModule_1324,+O_token_INT),+O+Dtoken_INT)]
**                IN  STR[hiddenThinModule_INT > thinModule_1356 = 
***                          STR[+O_term_INT > _term_1197 = 
****                                STR[term_TYC > term_1192 = CON_PATH(origModule_1324.+O_term_INT. term_TYC), 
****                                    term_sumarg_INT > term_sumarg_1200 = 
*****                                      CON_PATH(origModule_1324.+O_term_INT. term_sumarg_INT), 
****                                    term_sum_INT > term_sum_1201 = CON_PATH(origModule_1324.+O_term_INT. term_sum_INT), 
****                                    +Nterm_in_INT > term_in_1204 = MPROJ(origModule_1324,+O_term_INT).+Nterm_in_INT, 
****                                    +Nterm_out_INT > term_out_1205 = MPROJ(origModule_1324,+O_term_INT).+Nterm_out_INT], 
***                              +O_token_INT > _token_1239 = 
****                                STR[token_TYC > token_1234 = CON_PATH(origModule_1324.+O_token_INT. token_TYC), 
****                                    token_sumarg_INT > token_sumarg_1242 = 
*****                                      CON_PATH(origModule_1324.+O_token_INT. token_sumarg_INT), 
****                                    token_sum_INT > token_sum_1243 = CON_PATH(origModule_1324.+O_token_INT. token_sum_INT), 
****                                    +Ntoken_in_INT > token_in_1246 = MPROJ(origModule_1324,+O_token_INT).+Ntoken_in_INT, 
****                                    +Ntoken_out_INT > token_out_1247 = MPROJ(origModule_1324,+O_token_INT).+Ntoken_out_INT]], 
**                        term_TYC > copy_term_TYC_1325 = CON_PATH(coercedModule_1355.term_TYC), 
**                        term_sumarg_INT > copy_term_sumarg_INT_1326 = CON_PATH(coercedModule_1355.term_sumarg_INT), 
**                        term_sum_INT > copy_term_sum_INT_1327 = CON_PATH(coercedModule_1355.term_sum_INT), 
**                        +Nterm_in_INT > copy_+Nterm_in_INT_1328 = coercedModule_1355.+Nterm_in_INT, 
**                        +Nterm_out_INT > copy_+Nterm_out_INT_1329 = coercedModule_1355.+Nterm_out_INT, 
**                        +Eterm_INT > copy_+Eterm_INT_1330 = coercedModule_1355.+Eterm_INT, 
**                        +O+Dterm_INT > copy_+O+Dterm_INT_1334 = 
***                          STR[term_INT > copy_term_1194 = CON_PATH(coercedModule_1355.+O+Dterm_INT. term_INT), 
***                              expose_INT > exposer_1226 = MPROJ(coercedModule_1355,+O+Dterm_INT).expose_INT, 
***                              T > _1228 = MPROJ(coercedModule_1355,+O+Dterm_INT).T], 
**                        token_TYC > copy_token_TYC_1335 = CON_PATH(coercedModule_1355.token_TYC), 
**                        token_sumarg_INT > copy_token_sumarg_INT_1336 = CON_PATH(coercedModule_1355.token_sumarg_INT), 
**                        token_sum_INT > copy_token_sum_INT_1337 = CON_PATH(coercedModule_1355.token_sum_INT), 
**                        +Ntoken_in_INT > copy_+Ntoken_in_INT_1338 = coercedModule_1355.+Ntoken_in_INT, 
**                        +Ntoken_out_INT > copy_+Ntoken_out_INT_1339 = coercedModule_1355.+Ntoken_out_INT, 
**                        +Etoken_INT > copy_+Etoken_INT_1340 =  $POLY$ MPROJ(coercedModule_1355,+Etoken_INT), 
**                        +O+Dtoken_INT > copy_+O+Dtoken_INT_1354 = 
***                          STR[token_INT > copy_token_1236 = CON_PATH(coercedModule_1355.+O+Dtoken_INT. token_INT), 
***                              expose_INT > exposer_mod_1282 =  $POLY$ MPROJ(MPROJ(coercedModule_1355,+O+Dtoken_INT),expose_INT), 
***                              TOKEN > _1284 =  $POLY$ MPROJ(MPROJ(coercedModule_1355,+O+Dtoken_INT),TOKEN)]]
**                END
**            END]
ENTRIES:
          CONTEXT_SIGNAT: TOKEN_SIG > TOKEN_1091 = [term_TYC > term_1093 : TYPE, 
*                                                   term_sumarg_INT > term_sumarg_1101 : TYPE = INT32, 
*                                                   term_sum_INT > term_sum_1102 : TYPE = SUM[(T); 0; term_sumarg_1101], 
*                                                   +Nterm_in_INT > term_in_1105 : CON_COERCION([], term_sum_1102, term_1093), 
*                                                   +Nterm_out_INT > term_out_1106 : CON_COERCION([], term_1093, term_sum_1102), 
*                                                   +Eterm_INT > +Eterm_INT_1131 : 
**                                                     ({term_1093 *
**                                                       term_1093} -> CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0), 
*                                                   +O+Dterm_INT > term_1100 : 
**                                                     [term_INT > copy_term_1095 : TYPE == term_1093, 
**                                                      expose_INT > exposer_1127 : 
***                                                        (term_1093 => term_sum_1102) == 
***                                                           /TOTALNONRECUR\anonfun_1111
***                                                                 (exposee_1110 : term_1093)
***                                                                 : term_sum_1102 =
***                                                           COERCE(term_out_1106, [], exposee_1110), 
**                                                      T > _1129 : 
***                                                        (INT32 => term_1093) == 
***                                                           /TOTALNONRECUR\anonfun_1109
***                                                                 (injectee_1108 : INT32)
***                                                                 : term_1093 =
***                                                           COERCE(term_in_1105, [], INJ(0, term_sum_1102, injectee_1108))], 
*                                                   token_TYC > token_1135 : KIND(1 -> TYPE), 
*                                                   token_sumarg_INT > token_sumarg_1143 : TYPE = /-\('a_1188){term_1093 *
**                                                                                                             'a_1188}, 
*                                                   token_sum_INT > token_sum_1144 : 
**                                                     KIND(1 -> TYPE) = 
**                                                        /-\('a_1189)SUM[(TOKEN); 0; CON_APP(token_sumarg_1143; 'a_1189)], 
*                                                   +Ntoken_in_INT > token_in_1147 : 
**                                                     CON_COERCION(['a_1133],
**                                                                  CON_APP(token_sum_1144; 'a_1133),
**                                                                  CON_APP(token_1135; 'a_1133)), 
*                                                   +Ntoken_out_INT > token_out_1148 : 
**                                                     CON_COERCION(['a_1133],
**                                                                  CON_APP(token_1135; 'a_1133),
**                                                                  CON_APP(token_sum_1144; 'a_1133)), 
*                                                   +Etoken_INT > poly+Etoken_INT_1186 : 
**                                                      $POLY$ SIGF(mpoly_var_1134, ['a_TYV > 'a_1133 : TYPE, 
**                                                                                   +E'a_INT > _1150 : 
***                                                                                     ({'a_1133 *
***                                                                                       'a_1133} ->
***                                                                                     CON_MU(/-\(_259)
***                                                                                            (SUM[(false, true); 2; ()]))#0)] =>
**                                                                     [it_INT > +Etoken_INT_1185 : 
***                                                                        ({CON_APP(token_1135; CON_PATH(mpoly_var_1134.'a_TYV)) *
***                                                                          CON_APP(token_1135; CON_PATH(mpoly_var_1134.'a_TYV))} ->
***                                                                        CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)]), 
*                                                   +O+Dtoken_INT > token_1142 : 
**                                                     [token_INT > copy_token_1137 : KIND(1 -> TYPE) == token_1135, 
**                                                      expose_INT > exposer_mod_1182 : 
***                                                         $POLY$ 
***       +1   SIGF(mpoly_var_1134, ['a_TYV > 'a_1133 : TYPE] =>
***       +1           [it_INT > exposer_1181 : 
****      +1               (CON_APP(token_1135; CON_PATH(mpoly_var_1134.'a_TYV)) =>
****      +1               CON_APP(token_sum_1144; CON_PATH(mpoly_var_1134.'a_TYV))) == 
****      +1                  /TOTALNONRECUR\anonfun_1154
****      +1                        (exposee_1153 : CON_APP(token_1135; CON_PATH(mpoly_var_1134.'a_TYV)))
****      +1                        : CON_APP(token_sum_1144; CON_PATH(mpoly_var_1134.'a_TYV)) =
****      +1                  COERCE(token_out_1148, [CON_PATH(mpoly_var_1134.'a_TYV)], exposee_1153)]), 
**                                                      TOKEN > _1184 : 
***                                                         $POLY$ 
***       +1   SIGF(mpoly_var_1134, ['a_TYV > 'a_1133 : TYPE] =>
***       +1           [it_INT > _1183 : 
****      +1               ({term_1093 *
****      +1                 CON_PATH(mpoly_var_1134.'a_TYV)} => CON_APP(token_1135; CON_PATH(mpoly_var_1134.'a_TYV))) == 
****      +1                  /TOTALNONRECUR\anonfun_1152
****      +1                        (injectee_1151 : {term_1093 *
****      +1                                          CON_PATH(mpoly_var_1134.'a_TYV)})
****      +1                        : CON_APP(token_1135; CON_PATH(mpoly_var_1134.'a_TYV)) =
****      +1                  COERCE(token_in_1147,
****      +1                         [CON_PATH(mpoly_var_1134.'a_TYV)],
****      +1                         INJ(0, CON_APP(token_sum_1144; CON_PATH(mpoly_var_1134.'a_TYV)), injectee_1151))])]]
          CONTEXT_SDEC: Token_STR > strbindvar_1190 : 
**                         [hiddenThinModule_INT > thinModule_1356 : 
***                            [+O_term_INT > _term_1197 : 
****                               [term_TYC > term_1192 : TYPE, 
****                                term_sumarg_INT > term_sumarg_1200 : TYPE = INT32, 
****                                term_sum_INT > term_sum_1201 : TYPE = SUM[(T); 0; term_sumarg_1200], 
****                                +Nterm_in_INT > term_in_1204 : CON_COERCION([], term_sum_1201, term_1192), 
****                                +Nterm_out_INT > term_out_1205 : CON_COERCION([], term_1192, term_sum_1201)], 
***                             +O_token_INT > _token_1239 : 
****                               [token_TYC > token_1234 : KIND(1 -> TYPE), 
****                                token_sumarg_INT > token_sumarg_1242 : TYPE = /-\('a_1288){CON_PATH(_term_1197.term_TYC) *
*****                                                                                          'a_1288}, 
****                                token_sum_INT > token_sum_1243 : 
*****                                  KIND(1 -> TYPE) = 
*****                                     /-\('a_1289)SUM[(TOKEN); 0; CON_APP(token_sumarg_1242; 'a_1289)], 
****                                +Ntoken_in_INT > token_in_1246 : 
*****                                  CON_COERCION(['a_1232], CON_APP(token_sum_1243; 'a_1232), CON_APP(token_1234; 'a_1232)), 
****                                +Ntoken_out_INT > token_out_1247 : 
*****                                  CON_COERCION(['a_1232], CON_APP(token_1234; 'a_1232), CON_APP(token_sum_1243; 'a_1232))]], 
**                          term_TYC > copy_term_TYC_1325 : TYPE = CON_PATH(thinModule_1356.+O_term_INT. term_TYC), 
**                          term_sumarg_INT > copy_term_sumarg_INT_1326 : 
***                            TYPE = 
***                               CON_PATH(thinModule_1356.+O_term_INT. term_sumarg_INT), 
**                          term_sum_INT > copy_term_sum_INT_1327 : TYPE = CON_PATH(thinModule_1356.+O_term_INT. term_sum_INT), 
**                          +Nterm_in_INT > copy_+Nterm_in_INT_1328 : 
***                            CON_COERCION([], SUM[(T); 0; INT32], CON_PATH(thinModule_1356.+O_term_INT. term_TYC)), 
**                          +Nterm_out_INT > copy_+Nterm_out_INT_1329 : 
***                            CON_COERCION([], CON_PATH(thinModule_1356.+O_term_INT. term_TYC), SUM[(T); 0; INT32]), 
**                          +Eterm_INT > copy_+Eterm_INT_1330 : 
***                            ({CON_PATH(thinModule_1356.+O_term_INT. term_TYC) *
***                              CON_PATH(thinModule_1356.+O_term_INT. term_TYC)} ->
***                            CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0), 
**                          +O+Dterm_INT > copy_+O+Dterm_INT_1334 : 
***                            [term_INT > copy_term_1194 : TYPE == CON_PATH(thinModule_1356.+O_term_INT. term_TYC), 
***                             expose_INT > exposer_1226 : 
****                               (CON_PATH(thinModule_1356.+O_term_INT. term_TYC) =>
****                               CON_PATH(thinModule_1356.+O_term_INT. term_sum_INT)) == 
****                                  /TOTALNONRECUR\anonfun_1210
****                                        (exposee_1209 : CON_PATH(thinModule_1356.+O_term_INT. term_TYC))
****                                        : CON_PATH(thinModule_1356.+O_term_INT. term_sum_INT) =
****                                  COERCE(MPROJ(thinModule_1356,+O_term_INT).+Nterm_out_INT, [], exposee_1209), 
***                             T > _1228 : 
****                               (INT32 => CON_PATH(thinModule_1356.+O_term_INT. term_TYC)) == 
****                                  /TOTALNONRECUR\anonfun_1208
****                                        (injectee_1207 : INT32)
****                                        : CON_PATH(thinModule_1356.+O_term_INT. term_TYC) =
****                                  COERCE(MPROJ(thinModule_1356,+O_term_INT).+Nterm_in_INT,
****                                         [],
****                                         INJ(0, CON_PATH(thinModule_1356.+O_term_INT. term_sum_INT), injectee_1207))], 
**                          token_TYC > copy_token_TYC_1335 : KIND(1 -> TYPE) = CON_PATH(thinModule_1356.+O_token_INT. token_TYC), 
**                          token_sumarg_INT > copy_token_sumarg_INT_1336 : 
***                            TYPE = 
***                               CON_PATH(thinModule_1356.+O_token_INT. token_sumarg_INT), 
**                          token_sum_INT > copy_token_sum_INT_1337 : 
***                            KIND(1 -> TYPE) = 
***                               CON_PATH(thinModule_1356.+O_token_INT. token_sum_INT), 
**                          +Ntoken_in_INT > copy_+Ntoken_in_INT_1338 : 
***                            CON_COERCION(['a_1232],
***                                         CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_sum_INT); 'a_1232),
***                                         CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_TYC); 'a_1232)), 
**                          +Ntoken_out_INT > copy_+Ntoken_out_INT_1339 : 
***                            CON_COERCION(['a_1232],
***                                         CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_TYC); 'a_1232),
***                                         CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_sum_INT); 'a_1232)), 
**                          +Etoken_INT > copy_+Etoken_INT_1340 : 
***                             $POLY$ SIGF(mpoly_var_1233, ['a_TYV > 'a_1133 : TYPE, 
***                                                          +E'a_INT > _1150 : 
****                                                            ({'a_1133 *
****                                                              'a_1133} -> CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)] =>
***                                            [it_INT > _1346 : 
****                                               ({CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_TYC);
****                                                         CON_PATH(mpoly_var_1233.'a_TYV)) *
****                                                 CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_TYC);
****                                                         CON_PATH(mpoly_var_1233.'a_TYV))} ->
****                                               CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)]), 
**                          +O+Dtoken_INT > copy_+O+Dtoken_INT_1354 : 
***                            [token_INT > copy_token_1236 : KIND(1 -> TYPE) == CON_PATH(thinModule_1356.+O_token_INT. token_TYC), 
***                             expose_INT > exposer_mod_1282 : 
****                                $POLY$ SIGF(mpoly_var_1233, ['a_TYV > 'a_1232 : TYPE] =>
****                                               [it_INT > exposer_1281 : 
*****                                                  (CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_TYC);
*****                                                           CON_PATH(mpoly_var_1233.'a_TYV)) =>
*****                                                  CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_sum_INT);
*****                                                          CON_PATH(mpoly_var_1233.'a_TYV))) == 
*****                                                     /TOTALNONRECUR\anonfun_1253
*****                                                           (exposee_1252 : CON_APP(CON_PATH(thinModule_1356.+O_token_INT.
*****                                                                                                            token_TYC);
*****                                                                                   CON_PATH(mpoly_var_1233.'a_TYV)))
*****                                                           : CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_sum_INT);
*****                                                                     CON_PATH(mpoly_var_1233.'a_TYV)) =
*****                                                     COERCE(MPROJ(thinModule_1356,+O_token_INT).+Ntoken_out_INT,
*****                                                            [CON_PATH(mpoly_var_1233.'a_TYV)],
*****                                                            exposee_1252)]), 
***                             TOKEN > _1284 : 
****                                $POLY$ SIGF(mpoly_var_1233, ['a_TYV > 'a_1232 : TYPE] =>
****                                               [it_INT > _1283 : 
*****                                                  ({CON_PATH(thinModule_1356.+O_term_INT. term_TYC) *
*****                                                    CON_PATH(mpoly_var_1233.'a_TYV)} =>
*****                                                  CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_TYC);
*****                                                          CON_PATH(mpoly_var_1233.'a_TYV))) == 
*****                                                     /TOTALNONRECUR\anonfun_1251
*****                                                           (injectee_1250 : {CON_PATH(thinModule_1356.+O_term_INT. term_TYC) *
*****                                                                             CON_PATH(mpoly_var_1233.'a_TYV)})
*****                                                           : CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_TYC);
*****                                                                     CON_PATH(mpoly_var_1233.'a_TYV)) =
*****                                                     COERCE(MPROJ(thinModule_1356,+O_token_INT).+Ntoken_in_INT,
*****                                                            [CON_PATH(mpoly_var_1233.'a_TYV)],
*****                                                            INJ(0,
*****                                                                CON_APP(CON_PATH(thinModule_1356.+O_token_INT. token_sum_INT);
*****                                                                        CON_PATH(mpoly_var_1233.'a_TYV)),
*****                                                                injectee_1250))])]]
