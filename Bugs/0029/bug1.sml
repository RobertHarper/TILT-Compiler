(*$import *)

type 'a ref = 'a TiltPrim.ref
(*
SDEC: ref_TYC > ref_898 : KIND(1 -> TYPE) = /-\('a_897)REF('a_897)
SBND: ref_TYC > ref_898                   = /-\('a_897)REF('a_897)
NIL: ref_975 = LET  _974 =  FUN('a_973 ::  TYPE) = ARRAY('a_973)
*              IN   _974
*              END
EXPORT: ref = ref_975

SDEC: +Eref_INT > _902 : $POLY$ SIGF(varpoly_899, [''a_TYV > _900 : TYPE] =>
**                                   [it_INT > _903 : ({REF(CON_PATH(varpoly_899.''a_TYV)) *
***                                                     REF(CON_PATH(varpoly_899.''a_TYV))}
***                                                 -> CON_MU(/-\(_259)(SUM[ (false, true); 2; ()]))#0)])
SBND: +Eref_INT > _902 = $POLY$ FUNC(varpoly_899: [''a_TYV > _900 : TYPE] =>
**                                   STR[it_INT > _903 = EQ_REF[CON_PATH(varpoly_899.''a_TYV)]] : XXX)
NIL: /LEAF\_r_977
     (varpoly_c_978 :: REC_K{''a > _980 :  TYPE} ;; 
*     varpoly_r_979 : Unknown : DEP_RECORD[ ] ;; )
*        => ALLARROW(Open;
                     PARTIAL;
		     (); 
*                    (RECORD[1, 2](ARRAY(varpoly_c_978.''a), ARRAY(varpoly_c_978.''a)));
		     0; 
*                    PROJ(MU_NR((_993=SUM(2,2)(CREC{}))),1)) =
*        Let  _988 : Unknown = Let  /NORECUR\anonfun_985
*                                   ( ;; _986 : Unknown : RECORD[1, 2](ARRAY(varpoly_c_978.''a),
                                                                       ARRAY(varpoly_c_978.''a)) ;; )
*                                      -> PROJ(MU_NR((_987=SUM(2,2)(CREC{}))),1) =
*                                      equal_unknownArray[varpoly_c_978.''a](select[1][](_986),select[2][](_986))
*                              In   anonfun_985
*                              End
*             _r_991 : Unknown = _988
*        In   _r_991
*        End
EXPORT: +Eref_INT_r = _r_977
*)
    
signature NODE =
sig
    type 'a hash_table
end
(*
SDEC: NODE_SIG > NODE_904 = [hash_table_TYC > hash_table_905 : KIND(1 -> TYPE)]
*)

functor GraphFn(A : NODE) =
struct
    type graph = TiltPrim.int32 A.hash_table
end
(*
SDEC: GraphFn_FCT > functor_var_914 : SIGF(funct_arg_911, NODE_904 ->
**                                         [graph_TYC > graph_912
**                                            : TYPE = CON_APP(CON_PATH(funct_arg_911.hash_table_TYC); INT32)])
SBND: GraphFn_FCT > functor_var_914 = FUNC(funct_arg_911: NODE_904->
**                                         STR[graph_TYC > graph_912
***                                              = CON_APP(CON_PATH(funct_arg_911.hash_table_TYC); INT32)] : XXX)
NIL: functor_var_c_995
        = FUN(funct_arg_c_997 :: REC_K{hash_table > hash_table_1000 : Arrow_k(Open; _999 : TYPE; TYPE)})
*             = LET  graph_1002 = APP(funct_arg_c_997.hash_table,(INT32))
*               IN   CREC{graph = graph_1002}
*               END
EXPORT: GraphFn_FCT_c = functor_var_c_995
NIL: /LEAF\DEPfunctor_var_r_996
*    (funct_arg_c_997 :: REC_K{hash_table > hash_table_1000 : Arrow_k(Open; _999 : TYPE; TYPE)} ;; 
*     funct_arg_r_998 : Unknown : DEP_RECORD[ ] ;; )
*        -> LET  graph_1002 = APP(funct_arg_c_997.hash_table,(INT32))
*                _c_1004 = CREC{graph = graph_1002}
*           IN   DEP_RECORD[ ]
*           END =
*        Let  graph_1002 = APP(funct_arg_c_997.hash_table,(INT32))
*             _c_1004 = CREC{graph = graph_1002}
*             _r_1005 : Unknown = record()
*        In   _r_1005
*        End
EXPORT: GraphFn_FCT_r = functor_var_r_996
*)

structure HashTable =
    struct
	type 'a hash_table = TiltPrim.unit
    end
(*
SDEC: HashTable_STR > strbindvar_915 : 
**            [hash_table_TYC > hash_table_917 : KIND(1 -> TYPE) = /-\('a_916)UNIT, 
**             +Ehash_table_INT > _925 : $POLY$ SIGF(varpoly_918, [''a_TYV > _919 : TYPE] =>
***                                                  [it_INT > _926 : ({UNIT * UNIT} -> bool)])]
SBND: HashTable_STR > strbindvar_915 = 
**         STR[hash_table_TYC > hash_table_917 = /-\('a_916)UNIT, 
**             +Ehash_table_INT > _925 =
**                 $POLY$ FUNC(varpoly_918: [''a_TYV > _919 : TYPE] =>
***                            STR[it_INT > _926 =
****                               /TOTALNONRECUR\anonfun_924 (_921 : {UNIT * UNIT}) : bool =
****                                LET _922 = (_921)#1_~48
****                                    _923 = (_921)#2_~48
****                                IN  ROLL(bool,INJ(1, SUM[(false,true); 2; ()]))
****                                END] : XXX)]
NIL: hash_table_1010 = LET  _1009 =  FUN('a_1008 ::  TYPE) = UNIT
*                      IN   _1009
*                      END
NIL: strbindvar_c_1006 = CREC{hash_table = hash_table_1010}
EXPORT: HashTable_STR_c = strbindvar_c_1006
NIL: /LEAF\_r_1012
*    (varpoly_c_1013 :: REC_K{''a > _1015 :  TYPE} ;; 
*     varpoly_r_1014 : Unknown : DEP_RECORD[ ] ;; )
*        => ALLARROW(Open; PARTIAL; (); (RECORD[1, 2](UNIT, UNIT)); 0; PROJ(MU_NR((_1027=SUM(2,2)(CREC{}))),1)) =
*        Let  _1022 : Unknown = Let  /NORECUR\anonfun_1016
*                                    ( ;; _1017 : Unknown : RECORD[1, 2](UNIT, UNIT) ;; )
*                                       => PROJ(MU_NR((_1018=SUM(2,2)(CREC{}))),1) =
*                                       Let  _1019 : Unknown = select[1][](_1017)
*                                            _1020 : Unknown = select[2][](_1017)
*                                       In   roll [PROJ(MU_NR((_1021=SUM(2,2)(CREC{}))),1)]
*                                                 (inject_dyn1[SUM(2,2)(CREC{})]())
*                                       End
*                               In   anonfun_1016
*                               End
*             _r_1025 : Unknown = _1022
*        In   _r_1025
*        End
NIL: strbindvar_r_1007 : Unknown = record(+Ehash_table>_r_1012)
EXPORT: HashTable_STR_r = strbindvar_r_1007
*)

structure Node = 
    struct
	open HashTable
    end
(*
SDEC: Node_STR > strbindvar_927 : [+OopenlblHashTable_INT > openvar_928 : SIGS_OF(strbindvar_915)]
SBND: Node_STR > strbindvar_927 = STR[+OopenlblHashTable_INT > openvar_928 = strbindvar_915]
NIL: openvar_c_1031 = strbindvar_c_1006 (* HashTable *)
NIL: strbindvar_c_1029 = CREC{+OopenlblHashTable = openvar_c_1031}
EXPORT: Node_STR_c = strbindvar_c_1029
NIL: /LEAF\_r_1012
*    (varpoly_c_1013 :: REC_K{''a > _1015 :  TYPE} ;; varpoly_r_1014 : Unknown : DEP_RECORD[ ] ;; )
*       => ALLARROW(Open; PARTIAL; (); 
*                   (RECORD[1, 2](UNIT, UNIT)); 0; 
*                   PROJ(MU_NR((_1027=SUM(2,2)(CREC{}))),1)) =
*       Let  _1022 : Unknown = Let  /NORECUR\anonfun_1016
*                                   ( ;; _1017 : Unknown : RECORD[1, 2](UNIT, UNIT) ;; )
*                                      => PROJ(MU_NR((_1018=SUM(2,2)(CREC{}))),1) =
*                                      Let  _1019 : Unknown = select[1][](_1017)
*                                           _1020 : Unknown = select[2][](_1017)
*                                      In   roll[PROJ(MU_NR((_1021=SUM(2,2)(CREC{}))),1)]
*                                               (inject_dyn1[SUM(2,2)(CREC{})]())
*                                      End
*                              In   anonfun_1016
*                              End
*            _r_1025 : Unknown = _1022
*       In   _r_1025
*       End
NIL: strbindvar_r_1007 : Unknown = record(+Ehash_table>_r_1012),
NIL: openvar_r_1032 : Unknown = strbindvar_r_1007,
NIL: strbindvar_r_1030 : Unknown = record(+OopenlblHashTable>openvar_r_1032),
EXPORT: Node_STR_r = strbindvar_r_1030
*)


    (* If Node.+OopenlblHashTable_INT is a substructure,
     * then all occurrences of CON_PATH(strbindvar_927.hash_table_TYC)
     * should be CON_PATH(strbindvar_927.+OopenlblHashTable_INT.hash_table_TYC).
     * (In Graph's signature, -X+Crefresh__INT's result type, and refresh's result type.)
     *)
 
structure Graph = GraphFn(Node)
(*
SDEC: Graph_STR > strbindvar_929 : [graph_TYC > graph_912 : TYPE
***                                    = CON_APP(CON_PATH(strbindvar_927.hash_table_TYC); INT32)]
SBND: Graph_STR > strbindvar_929 =
         MOD_LET coerced_Node_930 = STR[hash_table_TYC > copy_hash_table_TYC_931
	                                   = CON_PATH(strbindvar_927.+OopenlblHashTable_INT.hash_table_TYC)]
**       IN      MAPP(functor_var_914,  coerced_Node_930)
**       END,
NIL: copy_hash_table_TYC_1040 = strbindvar_c_1029.+OopenlblHashTable. hash_table,
NIL: coerced_Node_c_1035 = CREC{hash_table = copy_hash_table_TYC_1040},
NIL: strbindvar_c_1033 = APP(functor_var_c_995,(coerced_Node_c_1035)),
EXPORT: Graph_STR_c = strbindvar_c_1033
NIL: coerced_Node_r_1036 : Unknown = record(),
NIL: strbindvar_r_1034 : Unknown = App_Open(functor_var_r_996;  coerced_Node_c_1035; coerced_Node_r_1036;  )
EXPORT: Graph_STR_r = strbindvar_r_1034
*)

fun refresh (reverse : Graph.graph ref) = !reverse
(*
SDEC: -X+Crefresh__INT > cluster_947
         : (CON_APP(ref_898; CON_PATH(strbindvar_929.graph_TYC))
            -> CON_APP(CON_PATH(strbindvar_927.hash_table_TYC); INT32))
SBND: -X+Crefresh__INT > cluster_947
         = /\refresh_935
**         (mvar_936 : CON_APP(ref_898; CON_PATH(strbindvar_929.graph_TYC))) : result_type =
**            LET shortResultType_939 = CON_APP(CON_PATH(strbindvar_927.hash_table_TYC); INT32)
**            IN  DEREF[CON_APP(CON_PATH(strbindvar_927.hash_table_TYC); INT32)][mvar_936]
**            END
SDEC: refresh > refresh_935
         : (CON_APP(ref_898; CON_PATH(strbindvar_929.graph_TYC))
            -> CON_APP(CON_PATH(strbindvar_927.hash_table_TYC); INT32))
SBND: refresh > refresh_935
         = cluster_947
NIL: /\ refresh_1041
*    ( ;; mvar_1042 : Unknown : APP(ref_975,(strbindvar_c_1033.graph)) ;; )
*         -> APP(strbindvar_c_1029.hash_table,(INT32)) = (* BUG *)
*         Let  shortResultType_1043 = APP(strbindvar_c_1029.hash_table,(INT32))	(* BUG *)
*         In   sub_unknownArray[APP(strbindvar_c_1029.hash_table,(INT32))](mvar_1042, 0) (* BUG *)
*         End
EXPORT: refresh = refresh_1041
*)
