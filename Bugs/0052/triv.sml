(*$import *)

(* tyvars: 0, many
   sccs: 1, many *)

(* Monomorphic, single component.  Code is good.
   The only small complaint is the sum is unnecessarily repeated:
      one_TYC > one_910 = CON_MU(/-\(vdt_one_920)(SUM[(A, B, C); 3; ()]))#0, 
      one_sumarg_INT > one_sumarg_915 = (), 
      one_sum_INT > one_sum_916 = SUM[(A, B, C); 3; one_sumarg_915], 
   could have been written
      one_sumarg_INT > one_sumarg_915 = (), 
      one_sum_INT > one_sum_916 = SUM[(A, B, C); 3; one_sumarg_915], 
      one_TYC > one_910 = CON_MU(/-\(vdt_one_920)one_sum_916)#0,
   at the expense of MORE and not LESS special casing in datatype.sml for
   the case when num_datatype = 1.  *)
datatype one = A | B | C

(* Monomorphic, multiple components.  Code is good. *)
     and foo = D | E of bar
     and bar = F | G of baz
     and baz = H | I of foo

(* Polymorphic, single component. *)
(* Same minor problem as in monomorphic case *)
(* I see a lot of related sums.  Probably the best
   we can do is to name the sumarg type early
   so that every occurrence of
	(CON_PATH(mpoly_var_1071.'a_TYV),
	 {CON_PATH(mpoly_var_1071.'a_TYV) * CON_PATH(mpoly_var_1071.'b_TYV)},
	 {CON_PATH(mpoly_var_1071.'a_TYV) * CON_PATH(mpoly_var_1071.'b_TYV) * CON_PATH(mpoly_var_1071.'c_TYV)})
   could be eliminated.
*)
(* Consider this code:
 
	LET name_1124 = {CON_PATH(mpoly_var_1071.'a_TYV) * CON_PATH(mpoly_var_1071.'b_TYV)}
        IN  /TOTALNONRECUR\anonfun_1131 (_1125 : {name_1124 * name_1124}) : bool_1105 =
            LET _1126 = (_1125)#1_~48
                _1127 = (_1125)#2_~48
            IN  CASE(arg = UNROLL(CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0,
                                  SUM[(false, true); 2; ()],
                                  APP(mpoly_var_1071.+E'a_INT,
                                      ((_1126)#1_~48, (_1127)#1_~48))) : SUM[(false, true); 2; ()],
                     resultType = bool_1105,
                     boundVar = unused_1130,
                     0: false_1107,
                     1: APP(mpoly_var_1071.+E'b_INT,
                            ((_1126)#2_~48, (_1127)#2_~48)),
                     Default: ---)
            END
        END
	
 (1a) This is an unnamed function for equality at 'a * 'b.   The question to
     answer is: is the equality compiler smart enough to name such
     functions when appropriate (to avoid re-generatign them)?

 (1b) Equality at 'a * 'b could be viewed as a factor of equality at
      'a * 'b * 'c.  Can this observation be made precise and useful?
     
 (2) The application of equality at 'a is ugly compared to the application at 'b.
     Can the code be made shorter.  Since our bool_con is the same as
     the result type of eq'a, we can replace UNROLL(MESS, ...) with
     UNROLL(bool_1005, ...).
*)
(* The module for this datatype certainly looks much larger than
   those for the monomorphic datatypes.  Each polymorphic expose
   and constructor uses around 50 lines of pretty-print.  Most of
   this space is due to the necessity of applying constructors to
   a particular set of polymorphic arguments.  The situation
   will slightly improve when we move to opaque datatypes since
   presumably we will no longer inline the values inside their
   functor signatures. *)
     and ('a,'b,'c) polyone = J
                            | K of 'a
                            | L of 'a * 'b
                            | M of 'a * 'b * 'c

(* Polymorphic, multiple components *)
(*
   We can use simpler polymorphic argument labels
   	'a_TYV 		a_TYV
	+E'a_INT	+Ea_INT
*)
(*
  When processing CON_SUM {..., carrier, ...}
  We could name carrier and process
  CON_SUM {..., carrier=name, ...}
*)
(*
  Should we eliminate irrelevant type variables?
	datatype ('a,'b,'c) should_be_mono = A | B | C
*)
(*	 
  Is there a better way to write the result type:
            [it_INT > top_eq_1266 : 
                {({CON_MU(/-\(vdt_plogh_1189, vdt_xyzzy_1190)
                          (SUM[(Q, R); 1; vdt_xyzzy_1190],
                           SUM[(N, O, P); 1; (CON_PATH(mpoly_var_1166.'a_TYV), vdt_plogh_1189)]))#0 *
                   CON_MU(/-\(vdt_plogh_1189, vdt_xyzzy_1190)
                          (SUM[(Q, R); 1; vdt_xyzzy_1190],
                           SUM[(N, O, P); 1; (CON_PATH(mpoly_var_1166.'a_TYV), vdt_plogh_1189)]))#0} ->
                 CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0) *
                 ({CON_MU(/-\(vdt_plogh_1189, vdt_xyzzy_1190)
                          (SUM[(Q, R); 1; vdt_xyzzy_1190],
                           SUM[(N, O, P); 1; (CON_PATH(mpoly_var_1166.'a_TYV), vdt_plogh_1189)]))#1 *
                   CON_MU(/-\(vdt_plogh_1189, vdt_xyzzy_1190)
                          (SUM[(Q, R); 1; vdt_xyzzy_1190],
                           SUM[(N, O, P); 1; (CON_PATH(mpoly_var_1166.'a_TYV), vdt_plogh_1189)]))#1} ->
                 CON_MU(/-\(_259)(SUM[(false, true); 2; ()]))#0)}]), 
 *)
     and ('a,'b,'c) xyzzy = N | O of 'a | P of ('a,'b,'c) plogh
     and ('a,'b,'c) plogh = Q | R of ('a,'b,'c) xyzzy
