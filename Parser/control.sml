(*$import TopLevel TextIO *)

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

structure Control : CONTROL =
  struct
    structure Print : PRINTCONTROL =
      struct
        val printDepth = ref 5
        val printLength = ref 12
        val stringDepth = ref 70
        val printLoop = ref true;
        val signatures = ref 2
        val out = ref{
		say = fn s => TextIO.output(TextIO.stdOut,s),
		flush = fn () => TextIO.flushOut TextIO.stdOut
	      }
        val linewidth = ref 79
        fun say s = #say (!out) s
        fun flush() = #flush (!out) ()
      end

    structure MC : MCCONTROL =
    struct
      val printArgs = ref false
      val printRet = ref false
      val bindContainsVar = ref true
      val bindExhaustive = ref true
      val matchExhaustive = ref true
      val matchRedundant = ref true
      val expandResult = ref false
    end

    structure CG : CGCONTROL =
    struct
      val tailrecur = ref true
      val recordopt = ref true
      val tail = ref true
      val allocprof = ref false
      val closureprint = ref false
      val closureStrategy = ref 0
      val lambdaopt = ref true
      val cpsopt = ref true
      val rounds = ref 10
      val path = ref false
      val betacontract = ref true
      val eta = ref true
      val selectopt = ref true
      val dropargs = ref true
      val deadvars = ref true
      val flattenargs = ref false
      val extraflatten = ref false
      val switchopt = ref true
      val handlerfold = ref true
      val branchfold = ref false
      val arithopt = ref true
      val betaexpand = ref true
      val unroll = ref true
      val knownfiddle = ref false
      val invariant = ref true
      val targeting = ref 0
      val lambdaprop = ref false
      val newconreps = ref true
      val boxedconstconreps = ref false
      val unroll_recur = ref true
      val sharepath = ref true
      val staticprof = ref false
      val hoistup = ref false
      val hoistdown = ref false
      val recordcopy = ref true
      val recordpath = ref true
      val verbose = ref false
      val debugcps = ref false
      val misc4 = ref 0
      val argrep = ref true
      val bodysize = ref 20
      val reducemore = ref 15
      val alphac = ref true
      val comment = ref false
      val knownGen = ref 0
      val knownClGen = ref 0
      val escapeGen = ref 0
      val calleeGen = ref 0
      val spillGen = ref 0
      val foldconst = ref true
      val etasplit = ref true
      val printLambda = ref false
      val printit = ref false
      val printsize = ref false
      val scheduling = ref true
      val cse = ref false
      val optafterclosure = ref false
      val uncurry = ref true
      val ifidiom = ref true
      val comparefold = ref true
      val csehoist = ref false
      val rangeopt = ref false
      val icount = ref false
      val mtderiv = ref true
      val debugRep = ref false
      val checklty1 = ref false
      val checklty2 = ref false
      val checklty3 = ref false
      val checkcps1 = ref false
      val checkcps2 = ref false
      val checkcps3 = ref false
      val checkcps = ref false
      val liftLiterals = ref false
      val flatfblock = ref true
      val deadup = ref true
      val pollChecks = ref false
      val pollRatioAtoI = ref 1.0

      val mudebugging = ref false
      val eedebugging = ref false
      val insdebugging = ref false
      val smdebugging = ref false
      val emdebugging = ref false
      val esdebugging = ref false
      val etdebugging = ref false
      val ecdebugging = ref false
    end
    val primaryPrompt = ref "- "
    val secondaryPrompt = ref "= "
    val weakUnderscore = ref false
    val copyToplevelOpen = ref true
    val debugging = ref false
    val internals = ref false
    val interp = ref false
    val debugLook = ref false
    val debugCollect = ref false
    val debugBind = ref false
    val markabsyn = ref true
    val trackExn = ref true
    val indexing = ref false
    val instSigs = ref true
    val quotation = ref false  (* controls backquote quotation *)

    val preserveLvarNames : bool ref = ref false
    val saveit = ref false 
    val saveLvarNames : bool ref = saveit
    val saveAbsyn : bool ref = saveit
    val saveLambda : bool ref = saveit
    val saveConvert : bool ref = saveit
    val saveCPSopt : bool ref = saveit
    val saveClosure : bool ref = saveit

    val lambdaSplitEnable = ref false
    val crossInlineEnable  = ref false
end

(*
 * $Log$
# Revision 1.2  98/01/21  20:40:11  pscheng
# moved the .sig files to .sig.sml file
# 
# Revision 1.1  97/03/26  14:12:24  pscheng
# added copy of SMLNJ parser files
# 
 *)
