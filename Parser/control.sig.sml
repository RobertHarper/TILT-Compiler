(* control.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)

signature PRINTCONTROL =
  sig
   val printDepth : int ref
   val printLength : int ref
   val stringDepth : int ref
   val printLoop : bool ref
   val signatures : int ref
   val out : {say : string -> unit, flush : unit -> unit} ref
   val linewidth : int ref
   val say : string -> unit
   val flush: unit -> unit
 end

signature MCCONTROL =
sig
  val printArgs : bool ref
  val printRet : bool ref
  val bindContainsVar : bool ref
  val bindExhaustive : bool ref
  val matchExhaustive : bool ref
  val matchRedundant : bool ref
  val expandResult : bool ref
end

signature CGCONTROL =
sig
  val tailrecur : bool ref
  val recordopt : bool ref
  val tail : bool ref
  val allocprof : bool ref
  val closureprint : bool ref
  val closureStrategy : int ref
  val lambdaopt : bool ref
  val cpsopt : bool ref
  val rounds : int ref
  val path : bool ref
  val betacontract : bool ref
  val eta : bool ref
  val selectopt : bool ref
  val dropargs : bool ref
  val deadvars : bool ref
  val flattenargs : bool ref
  val extraflatten : bool ref
  val switchopt : bool ref
  val handlerfold : bool ref
  val branchfold : bool ref
  val arithopt : bool ref
  val betaexpand : bool ref
  val unroll : bool ref
  val knownfiddle : bool ref
  val invariant: bool ref
  val targeting: int ref
  val lambdaprop: bool ref
  val newconreps : bool ref
  val boxedconstconreps : bool ref
  val sharepath : bool ref
  val staticprof : bool ref
  val unroll_recur : bool ref
  val hoistup : bool ref
  val hoistdown : bool ref
  val recordcopy : bool ref
  val recordpath : bool ref
  val debugcps : bool ref
  val misc4 : int ref
  val argrep : bool ref
  val bodysize : int ref
  val reducemore : int ref
  val alphac : bool ref
  val comment : bool ref
  val knownGen : int ref
  val knownClGen : int ref
  val escapeGen : int ref
  val calleeGen : int ref
  val spillGen : int ref
  val foldconst : bool ref
  val etasplit : bool ref
  val printLambda : bool ref
  val printit : bool ref
  val printsize : bool ref
  val scheduling : bool ref
  val cse : bool ref
  val optafterclosure : bool ref
  val uncurry : bool ref
  val ifidiom : bool ref
  val comparefold : bool ref
  val csehoist : bool ref
  val rangeopt : bool ref
  val icount : bool ref
  val mtderiv : bool ref
  val debugRep : bool ref
  val checklty1 : bool ref
  val checklty2 : bool ref
  val checklty3 : bool ref
  val checkcps1 : bool ref
  val checkcps2 : bool ref
  val checkcps3 : bool ref
  val checkcps  : bool ref
  val liftLiterals : bool ref
  val flatfblock : bool ref
  val deadup : bool ref
  val pollChecks : bool ref
  val pollRatioAtoI : real ref

  val mudebugging : bool ref
  val eedebugging : bool ref
  val insdebugging : bool ref
  val smdebugging : bool ref
  val emdebugging : bool ref
  val esdebugging : bool ref
  val etdebugging : bool ref
  val ecdebugging : bool ref
end

signature CONTROL =
   sig structure MC : MCCONTROL
       structure CG : CGCONTROL
       structure Print : PRINTCONTROL
       val debugging : bool ref
       val primaryPrompt : string ref
       val secondaryPrompt : string ref
       val internals : bool ref
       val weakUnderscore : bool ref
       val copyToplevelOpen : bool ref
           (* if true, top level open decls implemented by rebinding
	      dynamic components *)
       val interp : bool ref
       val debugLook : bool ref
       val debugCollect : bool ref
       val debugBind : bool ref
       val saveLambda : bool ref
       val saveLvarNames : bool ref
       val preserveLvarNames : bool ref
       val markabsyn : bool ref
       val trackExn : bool ref
       val indexing : bool ref
       val instSigs : bool ref
       val quotation : bool ref

       val saveit : bool ref
       val saveAbsyn : bool ref
       val saveConvert : bool ref
       val saveCPSopt : bool ref
       val saveClosure : bool ref

       val lambdaSplitEnable: bool ref
       val crossInlineEnable: bool ref
   end

