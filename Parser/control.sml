(*$import Prelude TextIO CONTROL *)

(* control.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *)


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
# Revision 1.5  2001/12/13  16:32:35  swasey
# *** empty log message ***
# 
# Revision 1.4  2000/09/12  18:56:48  swasey
# Changes for cutoff compilation
# 
# Revision 1.3  98/02/01  01:27:56  pscheng
# Changes to facilitate bootstrapping:
#   Added ascription in various places
#   Split up files into signature and code
# 
# Revision 1.2  1998/01/21  20:40:11  pscheng
# moved the .sig files to .sig.sml file
#
# Revision 1.1  97/03/26  14:12:24  pscheng
# added copy of SMLNJ parser files
# 
 *)
