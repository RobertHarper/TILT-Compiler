#include "s.h"
#include "r.h"
#include "sparc.h"

int checkAtGC = 0;
int LEAST_GC_TO_CHECK = 0;

void
init_int(int *x, int y)
{
	if (*x == 0)
		*x = y;
}

void
init_double(double *x, double y)
{
	if (*x == 0.0)
		*x = y;
}

static int
process_bool(int *var, char *item, char *option)
{
	int match = !strcasecmp(item,option);
	if (match) 
		*var = true;
	return match;
}

static int
process_string(char **var, char *item, char *option)
{
	 int len = strlen(item);
	int prefix_match = !strncmp(item,option,len);
	int optlen = strlen(option); 
	if (!prefix_match || (optlen<=len) || (option[len] != '='))
		return false;
	*var = emalloc(strlen(option)-len);
	strcpy(*var,option+len+1);
	return true;
}

static int
process_long(long *var, char *item, char *option)
{
	int itemLen = strlen(item);
	int optLen = strlen(option); 
	int prefixMatch = !strncmp(item,option,itemLen);
	if (prefixMatch) {
		if (optLen == itemLen) {
			*var = 1;    /* Special case to act like bool too */
			return true;
		}
		else if (optLen>itemLen && (option[itemLen] == '=')) {
			*var = atol(&option[itemLen+1]);
			return true;
		}
	}
	return 0;
}

static int
process_int(int *var, char *item, char *option)
{
	long temp = *var;
	int status = process_long(&temp,item,option);
	*var = (int) temp;
	return status;
}

static int
process_byte(int *var, char *item, char *option)
{
	char optionByte[100];
	long temp = *var;
	int status = process_long(&temp,item,option);
	*var = (int) (1024 * temp);
	if (status)
		return status;
	sprintf(optionByte,"%sbyte",option);
	status = process_long(&temp,item,optionByte);
	*var = (int) temp;
	return status;
}

static int
process_double(double *var, char *item, char *option)
{
	int len = strlen(item);
	int prefix_match = !strncmp(item,option,len);
	int optlen = strlen(option); 
	double possval = 0.0;
	if (!prefix_match || (optlen<=len) || (option[len] != '='))
		return 0;
	/*  For some bizarre reason, atof will NOT work here, it returns 1.0 or 0.0 */
	sscanf(option+len+1,"%lf",&possval);
	*var = possval;
	return 1;
}

enum { Bool, Int, Long, Double, Byte, String };

struct option_entry {
	int	type;
	char*	name; 
	void*	item;
	char*	description;
};

static int stackOrder=0, queueOrder=0, hybridOrder=0, doSpaceCheck=0;
static int help=0, semi=0, gen=0, semipara=0, genpara=0, semiconc=0, genconc=0;
static int FixheapByte=0, RelaxFixheapByte=0;
static struct option_entry table[] = {
	{Bool, "help", &help, "Print help info but do not execute program"},
	{Bool, "semi", &semi, "Use the semispace garbage collector"},
	{Bool, "gen", &gen,   "Use the generational garbage collector"},
	{Bool, "semipara", &semipara, "Use the semispace, parallel garbage collector"},
	{Bool, "genpara", &genpara, "Use the generational, parallel garbage collector"},
	{Bool, "semiconc", &semiconc, "Use the semispace, concurrent garbage collector"},
	{Bool, "genconc", &genconc, "Use the generational, concurrent garbage collector"},
	{Bool, "forceMirrorArray", &forceMirrorArray, "Force collector to use mirrored pointer arrays"},
	{Bool, "useGenStack", &useGenStack, "Use generational stack tracing"},
	{Int, "paranoid", &paranoid, "Run in paranoid mode every nth GC"},
	{Bool, "verbose", &verbose, "Be verbose when paranoid"},
	{Bool, "diag", &diag, "Run in diagnostic mode"},
	{Int, "collectDiag", &collectDiag, "Run in collector diagnostic mode"},
	{Bool, "timeDiag", &timeDiag, "Run in time-diagnostic mode"},
	{Bool, "threadDiag", &threadDiag, "Show thread-related diagnostic messages"},
	{Bool, "warnThreshold", &warnThreshold, "Show information if pause exceeds threshold"},
	{Double, "warnUtil", &warnUtil, "Show information if utilization falls below given fraction at 10 ms level"},
	{Bool, "debugStack", &debugStack, "Show scanning of stack frames"},
	{Bool, "gcstats", &SHOW_GCSTATS, "Show GC statistics during execution"},
	{Bool, "gcdebug", &SHOW_GCDEBUG, "Show GC debugging information during execution"},
	{Bool, "gcforward", &SHOW_GCFORWARD, "Show object forwarding infomation during GC"},
	{Bool, "gcerror", &SHOW_GCERROR, "Show GC errors"},
	{Bool, "showheaps", &SHOW_HEAPS, "Show heaps before and after each GC"},
	{Bool, "showglobals", &SHOW_GLOBALS, "Show globals before and after each GC"},
	{Int, "showatgc", &LEAST_GC_TO_CHECK, "Show heaps starting at this GC"},
	{Int, "checkatgc", &checkAtGC, "Check heaps starting at this GC"},
	{Int, "stackletSize", &MLStackletSize, "Stack size of thread stacklets measured in Kbytes"},
	{Int, "proc", &NumProc, "Use this many processors"},
	{Int, "rotateProc", &RotateProc, "Skip this many processors before beginning assignment"},
	{Int, "usageCount", &usageCount, "Update usage whenever this many gray objects processed"},
	{Byte, "largeheap", &LargeHeapByte, "Set large object heap size in bytes"},
	{Byte, "minheap", &MinHeapByte, "Set minimum size of heap in bytes"},
	{Byte, "maxheap", &MaxHeapByte, "Set maximum size of heap in bytes"},
	{Byte, "nursery", &NurseryByte, "Set size of nursery in bytes"},
	{Byte, "fixheap", &FixheapByte, "Set the size of heap in kbytes"},
	{Byte, "relaxFixheap", &RelaxFixheapByte, "Set the size of heap in Kbytes when concurrent collector off"},
	{Int, "minratio", &MinRatio, "Set the minimum ratio of of live objects to all objects"},
	{Int, "maxratio", &MaxRatio, "Set the maximum ratio of of live objects to all objects"},
	{Int, "minOffRequest", &minOffRequest, "Minimum size of mutator request when collector is off"},
	{Int, "maxOffRequest", &maxOffRequest, "Maximum size of mutator request when collector is off"},
	{Int, "minOnRequest", &minOnRequest, "Minimum size of mutator request when collector is on"},
	{Int, "copyPageSize", &copyPageSize, "Minimum size of copying area for the multi-collector case"},
	{Int, "copyChunkSize", &copyChunkSize, "Minimum size of objects that are copied into area directly allocated from shared pool"},
	{Int, "objFetchSize", &objFetchSize, "Number of items to fetch from the shared work stack"},
	{Int, "localWorkSize", &localWorkSize, "Number of items to work on from local shared stack before accessing shared work stack"},
	{Int, "doCopyCopySync", &doCopyCopySync, "Perform copy-copy synchronization for parallel/concurrent collectiors"},
	{Int, "noSharing", &noSharing, "Do not use the shared stack to distribute work"},
	{Int, "noWorkTrack", &noWorkTrack, "Do not update or test for work done.  Should be used only in addition to noSharing"},
	{Int, "doAgressive", &doAgressive, "Use 2 phase concurrent collection"},
	{Int, "doMinorAgressive", &doMinorAgressive, "Use 2 phase concurrent collection for minor collections"},
	{Int, "accounting", &accountingLevel, "Amount of accounting to perform"},
	{Double, "targetUtil", &targetUtil, "Do work to maintain target utilization"},
	{Double, "collectionRate", &CollectionRate, "Rate of concurrent collector"},
	{Double, "objCopyWeight", &objCopyWeight, "Weight given to cost of copying an object"},
	{Double, "objScanWeight", &objScanWeight, "Weight given to cost of scanning an object"},
	{Double, "fieldCopyWeight", &fieldCopyWeight, "Weight given to cost of copying a field"},
	{Double, "fieldScanWeight", &fieldScanWeight, "Weight given to cost of scanning a field"},
	{Int, "stackOrder", &stackOrder, "Use stack ordering (use pop) on local work set"},
	{Int, "queueOrder", &queueOrder, "Use queue ordering (use dequeue) on local work set"},
	{Int, "hybridOrder", &hybridOrder, "Use a set of gray regions"},
	{Int, "forceSpaceCheck", &doSpaceCheck, "Perform space check on allocating space for copying"},
	{Int, "grayAsReplica", &grayAsReplica, "In concurrent collectors, store gray set as replica (with backptrs)"},

	/* For debugging */
	{Int, "fetchSize", &fetchSize, ""},
	{Int, "cacheSize", &cacheSize, ""},
	{Int, "cacheSize2", &cacheSize2, ""},

	{String, "historyFile", &historyFile, "Write state history into given file"},
	{Int, "info", &information, "Level of information to print"},
};

static void
process_option(int argc, char **argv)
{
	char **cur;
	int i;
	for (cur=argv+1; *cur != NULL; cur++) {
		int matched = 0;
		char *poss_option = *cur;
		char *option = poss_option+1;
		if (*poss_option != '@')
			/* First non-option signals start of user-program options.*/
			break;
		for (i=0; i<arraysize(table); i++) {
			switch (table[i].type) {
			case Bool:
				matched = process_bool(table[i].item, table[i].name, option);
				break;
			case Int:
				matched = process_int(table[i].item, table[i].name, option);
				break;
			case Long:
				matched = process_long(table[i].item, table[i].name, option);
				break;
			case Double:
				matched = process_double(table[i].item, table[i].name, option);
				break;
			case Byte:
				matched = process_byte(table[i].item, table[i].name, option);
				break;
			case String:
				matched = process_string(table[i].item, table[i].name, option);
				break;
			default:
				DIE("unknown type for option entry");
			}
			if (matched) break;
      		}
      		if (!matched) {
			fprintf(stderr,"unknown option (use @help for a list): %s\n",option);
			exit(1);
		}
	}
	setCommandLine(argv[0], cur);
	if (semi) collector_type = Semispace;
	if (gen) collector_type = Generational;
	if (semipara) collector_type = SemispaceParallel;
	if (genpara) collector_type = GenerationalParallel;
	if (semiconc) collector_type = SemispaceConcurrent;
	if (genconc) collector_type = GenerationalConcurrent;
	if (stackOrder) ordering = StackOrder;
	if (queueOrder) ordering = QueueOrder;
	if (hybridOrder) ordering = HybridOrder;
	if (doSpaceCheck) forceSpaceCheck = 1;
	if (RelaxFixheapByte) {
		assert(collector_type == SemispaceConcurrent ||
			collector_type == GenerationalConcurrent);
		relaxed = 1;
		MinHeapByte = MaxHeapByte = RelaxFixheapByte;
	}
	if (FixheapByte) MinHeapByte = MaxHeapByte = FixheapByte;
	if (help) {
		printf("Boolean options are activated like this: @diag\n");
		printf("Int, long, and double options are activated like this: @nursery=512\n");
		printf("The following options are available.\n");
		for (i=0; i<arraysize(table); i++) {
			printf("%12s : ", table[i].name);
			switch (table[i].type) {
			case Bool: printf("bool = %s", *(int *)table[i].item ? "true" : "false"); break;
			case Int: printf("int = %d", *(int *)(table[i].item)); break;
			case Long: printf("long = %ld", *(long *)(table[i].item)); break;
			case Double: printf("double = %lf", *(double *)(table[i].item)); break;
			case Byte: printf("byte = %d", *(int *)(table[i].item)); break;
			case String: printf("string option = %s", *(char**)(table[i].item) ? *(char**)(table[i].item) : "NONE"); break;
			default: DIE("unknown type");
			}
			printf(" -- %s\n", table[i].description);
		}
		exit(1);
	}
}

int
main(int argc, char **argv)
{
	process_option(argc,argv);
	stats_init();
	memobj_init();
	signal_init();
	thread_init();
	global_init(); 
	exn_init();
	stack_init();  /* must follow thread_init */
	GCInit();
	thread_go((ptr_t) GetGlobal(&link_LINKUNIT_DOTmain));
	stats_finish();
	return 0;
}
