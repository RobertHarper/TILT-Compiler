#include "sparcasm.h"
#include "asm.h"
#include <ucontext.h>
#include <pthread.h>

#undef PARANOID	/* XXX eliminate in favor of constant */
#undef GCTABLE_HASENTRYID
#undef FLATTEN_MODULES	/* Must agree with Tonil.flatten_modules */

/*
	mem_t
		An ML value that has a pointer type.  It might be a
		pointer into the stack or heap.  There is not
		necessarily an object at the location.
	loc_t
		A location which contains an ML value.  External information
		is needed to determine whether the location contains a pointer
		value or not.  Possible locations are stack slots, memory
		locations reflecting register contents, and locations inside
		ML objects in the data segment or in the heap.
	ploc_t
		A location which contains an ML pointer.
	tag_t
		An ML tag.
*/	
typedef volatile uint32*	vptr_t;
typedef uint32*	mem_t;
typedef val_t*	loc_t;
typedef ptr_t*	ploc_t;
typedef volatile ptr_t*	vploc_t;
typedef uint32	tag_t;

/*
	Not generally thread-safe.  However, HashLookup's can be
	safely interleaved.
*/
typedef struct HashEntry HashEntry_t;
typedef struct HashTable HashTable_t;
struct HashEntry {
	unsigned long key;
	void* data;
};
struct HashTable {
	int size;
	int logsize;
	int logmask;
	HashEntry_t* table;
};

/*
	A set is a collection of non-NULL values with two different
	sets of operations: stack-like or queue-like.  Thread-safety
	is controlled by instance creation.

	The data elements are from first to last - 1 with no
	wraparound.  If first = last, then the set is empty.

	If size = last + 1, then the set is full.  The slot of the
	data array at last is always NULL.

	When access is always stack-like, then first will be zero.
	Stack operations may be used only if no queue-like operations
	are ever used since stack operations rely on first being zero.
	The stack-like operations are Push and Pop.  When last + 1
	equals size on a Push, the stack is increased in size.

	The queue-like operations are Push and Dequeue.  When last + 1
	equals size on a Push, the queue is normalized.  If the queue
	is mostly full (75%), normalization will increase the size.
	In any case the elements are shifted downwards so that first
	becomes 0.
*/
typedef struct Set__t Set_t;
struct Set__t {
	long size;	/* size of data array */
	ptr_t* data;	/* array of data elements */
	ptr_t* first;
	ptr_t* last;
	ptr_t* limit;	/* = data + size */
};

/*
	There must be at least 3 strong barriers in the cycle for
	resetting to work.
*/
typedef struct Barriers__t Barriers_t;
struct Barriers__t {
	int numProcessor;	/* Number of processors to synchronize */
	int maxBarrier;	/* Total number of barrier */
	volatile long* barriers;	/* Number of processors at each barrier */
	/*
		index[i][j] is the number of the j-th processor at
		barrier i.
	*/
	volatile long** index;
	/*
		Number of barriers we are using.  Must be >= 3.  Reset
		with lastStrongBarrier.
	*/
	volatile int phase;
	/*
		Last barrier to be used.  We must keep this count in
		order to reset barriers correctly.  Whenever we pass a
		strong barrier, we clear all barriers from lastUsed up
		to but excluding the strong barrier and update
		lastUsed to the strong barrier.  No clearing is done
		on a weak barrier.
	*/
	volatile int lastUsed;
};

typedef int (*finalizer_t)(void*);
typedef int (*logger_t)(int, int, void*);
typedef volatile struct Rooms__t Rooms_t;
struct Rooms__t {
	long* cur;
	long* wait;
	long* prev;
	finalizer_t* finalizer;
	void** finalizerData;
	logger_t logger;
	void* loggerData;
	int size;
	int which;
	int lock;
};

typedef volatile struct Bitmap_st Bitmap_t;
struct Bitmap_st {
	long lock;
	int size;
	int pos;
	int used;
	unsigned long *data;
};

typedef struct Timer__t Timer_t;
struct Timer__t {
	int on;	/* Is timer on */
	double last;	/* Last time in milliseconds */
	struct timespec tp;
};

typedef struct Statistic__t Statistic_t;
struct Statistic__t {
	/* Running statistics without subsampling */
	double last, min, max, sum;
	int count;
};

typedef struct Histogram__t Histogram_t;
struct Histogram__t {
	Statistic_t stat;	/* The number of data points is stored in stat */
	/*
		38 buckets:
		1 for <.001; 
		6 for .001 to .01 in this pattern
			.001 < .002 < .003 < .004 < .006 < .008 < .010;
		6 for .01 to .1; 
		6 for .1 to 1; 
		6 for 1 to 10; 
		6 for 10 to 100; 
		6 for 100 to 1000; 
		1 for > 1000
	*/
	double bucketStart[38];
	double bucketEnd[38];
	int bucket[38];
};

/*
	A sequence of numbers (times) s1, s2, ...  , each associated with
	a bit-value on/off, is fed to this data structure and it computes
	the smallest fraction of on to off values in any given window of
	size w.  That is, it computes values

	s_x1 + s_x2 + ... + s_xm    with x1 < x2 < ...
	------------------------ 
	s_y1 + s_y2 + ... + s_ym    with y1 < y2 < ...

	where the x and y values are distinct and form a contiguous
	sequence and so that

	w - s_max(xm,yn) > s_x1 + s_x2 + ... + s_xm + s_y1 + s_y2 + ... + s_ym > w
*/
typedef struct WindowQuotient__t WindowQuotient_t;
struct WindowQuotient__t {	/* quotient = on / off */
	int	granularity;	/* number of parts millisecond is divided into */
	int	size;
	double	onRemain;
	double	offRemain;
	char*	data;	/* Each char indicates state of 1 ms / granularity */
	int	first;	/* first occupied slot */
	int	last;	/* first unused slot */
	int	numWindows;
	int	windowSize[20];	/* in units of granularity * ms */
	int	onSum[20];	/* sum of active data values that are on */
	int	offSum[20];	/* sum of active data values that are off */
	int	start[20];	/* first slot for this window size */
	Statistic_t stat[20];
};

typedef struct StackChain__t StackChain_t;
typedef volatile struct Stacklet__t Stacklet_t;
typedef struct range__t range_t;
typedef struct Heap__t Heap_t;
typedef enum {
	Inconsistent,
	/*
		no information on primary or replica
	*/
	Pending,
	/*
		primary not used; replica uninitialized but needs to
		be copied before primary modified
	*/
	Copying,
	/*
		primary not used; replica snapshot being made, copying
		from primary snapshot
	*/
	InactiveCopied,
	/*
		primary not used; replica snapshot made (and scanned
		by end of GC)
	*/
	ActiveCopied
	/*
		primary used; replica snapshot made (and scanned by
		end of GC)
	*/
} StackletState_t;
/*
	Each stacklet is actually a pair of stacklets.  The variable
	stackletOffset indicates which one we use.
*/
struct Stacklet__t {
	int count;	/* Reference count of how many stack chains I belong to */
	int mapped;	/* Has memory been mapped to this stacklet? */
	StackletState_t state;
	/*
		Base region of mapped memory with cursor.  Other region
		obtained by adding constant offset.  baseExtendedBottom is the
		true bottom of the stack; there is an extra area for C.
		baseBottom is the normal bottom of the stack.
	*/
	mem_t baseExtendedBottom;
	mem_t baseBottom;
	mem_t baseCursor;
	mem_t baseTop;
	mem_t retadd;	/* retadd necessary for resumption */
	/*
		Recorded when the replica is copied from the primary so that
		the replica stacklet can be later scanned.
	*/
	mem_t replicaCursor;
	mem_t replicaRetadd;
	/*
		The callee save registers at the bottom frame are restored
		upon return.  There are two sets of 32 GP registers accessed
		depending on if StackletPrimaryOffset is 0.
	*/
	reg_t  bottomBaseRegs[64];
	/* Corresponds to stack frames of this stacklet. */
	Set_t* callinfoStack;
	StackChain_t* parent;	/* Chain this stacklet belongs to. */
};
struct StackChain__t {
	volatile int used;
	volatile int cursor;	/* Index of first uninitialized stacklet. */
	volatile int avail;
	Stacklet_t** stacklets;	/* Dynamic -- expands but doesn't contract. */
	volatile void* thread;	/* Really of type Thread_t*  */
};
struct range__t {
	mem_t low;	/* pointers */
	mem_t high;
	unsigned int diff;	/* difference in bytes = 4 * (high - low) */
};
struct Heap__t {
	/*
		Invariant:
			bottom <= cursor <= top <= writeableTop <= mappedTop
			size = top - bottom (in bytes)
		valid
			Indicates whether this heap is in current use.
		bottom
			The physical and logical bottom of the memory region and
			heap.
		top
			The logical top of the heap.
		mappedTop
			The top of the memory region that is mapped.
		writeableTop
			The top of the memory region that is unprotected.
		cursor
			The next allocation point in the logical heap.
		range
			The physical range bottom to physicalTop.
		lock
			Used to synchronize multiple access to heap object.
		freshPages
			Pages not accessed by collector since start of GC.
		bitmap
			Stores starts of objects for debugging.
	*/
	int id;
	volatile int valid;
	mem_t bottom;
	volatile mem_t top;
	volatile mem_t mappedTop;
	volatile mem_t writeableTop;
	volatile mem_t cursor;
	volatile int size;
	range_t range;
	pthread_mutex_t* lock;
	volatile int* freshPages;
	Bitmap_t* bitmap;
};

typedef enum {
	/*
		SelfReplica is relevant for mirrored ptr arrays in tenured
		space during a minor collection.
	*/
	NoReplica, OtherReplica, SelfReplica,
} ShowType_t;

typedef struct Callinfo__t Callinfo_t;
typedef struct CallinfoCursor__t CallinfoCursor_t;
typedef struct CopyRange__t CopyRange_t;
typedef struct Usage__t Usage_t;
typedef struct Thread__t Thread_t;
typedef struct LocalWork__t LocalWork_t;
typedef struct SharedStack__t SharedStack_t;
typedef struct Summary__t Summary_t;
typedef struct Proc__t Proc_t;
/*
	This structure must match the GC table entry format generated by
	the compiler.
*/
struct Callinfo__t {
	val_t	retadd;
#ifdef GCTABLE_HASENTRYID
	int	entryid;
#endif
	/*
		low 16 bits = entry size in words
		next 16 bits = frame size in words
	*/
	int	size0;
	/*
		low 16 bits = byte section size in words
		next 5 bits = word offset of return address
		upper 11 bits = zero
	*/
	int	size1;
	int	regtrace_a;	/* ab=10: YES	ab=00:NO  */
	int	regtrace_b;	/* ab=11:CALLEE	ab=01:SPEC */
	/*
		Must be word aligned.  Stack status: 00=TRACE_NO,
		01=TRACE_YES, 02=TRACE_CALLEE (?), and 03=TRACE_?.  Then byte
		data then special data.  Note that ONLY the BYTE DATA follows
		the natural endian-ness.  The other fields use ints/4 bytes
		when laid out.  If the the other fields, like the regtrace or
		stacktrace are read fom memory in smaller sizes for
		optimization purposes, the runtime must dispatch at
		compile-time on endian-ness.  The pairs of bits in
		stacktrace/regtrace are laid out starting from the lsb to the
		most significant bit.
	*/
	char	 __rawdata[4];
};

struct CallinfoCursor__t {
	Callinfo_t* callinfo;
	int entrySize;
	int frameSize;
	int RAQuadOffset;
	int byteOffset;	/* cursors into the special sections */
	int wordOffset;
	unsigned int yesBits;
	unsigned int calleeMask;
	unsigned int specialBits;
	char* byteData;
	int* wordData;
	/* Cached information on stack slots. */
	int slotCount;	/* -(n+1) cache called n times; >0 cache is active */
	int slot[10];
	int trace[10];
};

/* Not concurrently accessed. */
struct CopyRange__t {
	mem_t start;
	mem_t cursor;
	mem_t stop;
	mem_t reserve;
	Heap_t* heap;
	Proc_t* proc;
};

/* Not concurrently accessed. */
struct Usage__t {
	long numWrites;
	long bytesAllocated;
	long bytesReplicated;
	long fieldsCopied;
	long fieldsScanned;
	long ptrFieldsScanned;
	long objsCopied;
	long objsScanned;
	long pagesTouched;
	long rootsProcessed;
	long globalsProcessed;
	long stackSlotsProcessed;
	/*
		Weighted average of bytesCopied, bytesScanned, and
		rootProcessed - not always up-to-date.
	*/
	long workDone;
	/*
		Used by recentWorkDone; workDone (in the past) +
		localWorkSize.
	*/
	long checkWork;
	long maxWork;	/* maximum work to do in this segment */
	/*
		Cycles down from localWorksize to zero repeatedly.  When zero,
		workDone is updated.
	*/
	long counter;
};

/*
	The first several fields must agree with asm.h and
	../../Sparc/sparc.sml.
*/
struct Thread__t {
	volatile reg_t saveregs[32];
	volatile double fregs[32];
	volatile Proc_t* proc;
	volatile long notInML;
	volatile long safeToGC;
	volatile long padding;
	volatile double scratch;
	/* Why were we stoppped and how do we resume? */
	volatile long request;
	/*
		If positive, how many bytes needed for allocation.
		If negative, how many bytes of write buffer needed.
	*/
	volatile long requestInfo;
	volatile reg_t Csaveregs[32];
	volatile double Cfregs[32];
	volatile ploc_t writelistAlloc;
	volatile ploc_t writelistLimit;
	volatile mem_t stackLimit;	/* Bottom of current stacklet */
	volatile mem_t stackTop;	/* Top of current stacklet */
	volatile int globalOffset;	/* zero or four */
	volatile int stackletOffset;	/* zero or stackletSize*1024 */
	volatile int arrayOffset;	/* zero or four */

	/*
		The following fields are not accessed by assembly code or the
		mutator.
	*/
	/* Thunk of this thread.  NULL after thunk has started. */
	volatile ptr_t thunk;
	/*
		rootVals contains root values and are needed only for the
		concurrent collector.  rootLocs contains root locations (when
		non-NULL) from which the corresponding rootVals obtained their
		values.
	*/
	volatile ptr_t rootVals[10];
	volatile ploc_t rootLocs[10];
	StackChain_t* stack;
	long tid;	/* thread ID */
	long id;	/* ID */
	volatile int used;
	volatile long status;	/* long for FetchAndAdd */
	volatile int pinned;
	Thread_t* parent;
};

typedef enum {
	/*
		The states Scheduler, Mutator, GC, and Done are disjoint.  The
		remaining GC* states are substates of GC.
	*/
	Scheduler, Mutator, GC, Done, Idle, 
	GCStack, GCGlobal, GCReplicate, GCWork, GCWrite, GCIdle,
} ProcessorState_t;

struct LocalWork__t {
	Set_t objs;	/* Gray objects */
	Set_t grayRegion;	/* Regions of gray objects */
	Set_t globals;	/* Global variables */
	Set_t roots;	/* Stack and global locations containing root values */
	/* Used by incremental collector for breaking up scanning stacks. */
	Set_t segments;
	Set_t stacklets;
	/* Used by generational collectors for arrays allocated in tenured area. */
	Set_t backObjs;
	/* Used by generational, concurrent collector for modified array fields. */
	Set_t backLocs;
	Set_t nextBackObjs;	/* Used when double processing */
	/*
		Used when double processing.  Note that the location is the
		mirror of the ones in backLocs.
	*/
	Set_t nextBackLocs;
	/* has local stack grabbed items from shared stack? */
	volatile int hasShared;
};

/*
	Shared stack of root values, objects, large object segments,
	stacklets.
*/
struct SharedStack__t {
	LocalWork_t work;
	int doubleProcess;	/* do nextBackObj and nextBackLocs? */
	/* Number of local stacks that might be non-empty */
	volatile long numLocalStack;
	Rooms_t* threeRoom;	/* Pops, Pushes, Pushes from GCRelease */
	volatile long numPush;
	volatile long numPop;
};

typedef enum {
	/*
		Each segment might be no collection, minor, or major.
		Independently, it might involve flipping the collector on or
		off or both.
	*/
	MinorWork	= 1,
	MajorWork	= 2,
	FlipOn	= 4,
	FlipOff	= 8,
	FlipTransition	= 16,
} Summary_Type_t;

/*
	Summarizes information of a state.  Not concurrently accessed.
	Fields that are frequently and directly accessed should be placed
	first so their displacement is small.  Statistics, histories,
	histograms, or indirectly accessed fields come last.
*/
struct Summary__t {
	double time;	/* time in ms */
	int segment;	/* segment number */
	int state;	/* primary state type */
	Summary_Type_t type;	/* additional segment type info */
	/* value of segUsage.workdone for GC and bytes allocaetd for Mutator */
	int data1;
	int data2;	/* numWrites for Mutator */
	int data3;
	double util1;
	double util2;
	double util3;
};

struct Proc__t {
	/*
		Address of system thread stack that can be used to enter
		scheduler.
	*/
	int stack;
	int procid;	/* sys thread id */
	mem_t allocStart;	/* allocation range */
	mem_t allocCursor;
	mem_t allocLimit;
	ploc_t writelistStart;	/* write list range */
	ploc_t writelistCursor;
	ploc_t writelistEnd;
	int processor;	/* processor id that this pthread is bound to */
	/* Current user thread mapped to this system thread. */
	Thread_t* userThread;
	LocalWork_t work;
	/* Info for current segment which will be added to a cycle. */
	Usage_t segUsage;
	Usage_t cycleUsage;	/* Info for current GC cycle */
	/* Possibly used by a generational concurrent collector. */
	Set_t majorRegionStack;
	int barrierPhase;
	Timer_t totalTimer;	/* Time spent in entire processor */
	Timer_t currentTimer;	/* Time spent running any subtask */
	int segmentNumber;	/* Current segment number */
	/* Was there minor work, major work, flip off, or flip on? */
	int segmentType;
	/* Time last spent in mutator - used for computing utilization level. */
	double mutatorTime;
	double nonMutatorTime;	/* Total time since mutator suspended */
	int nonMutatorCount;
	int nonMutatorSegmentType;
	ProcessorState_t state;	/* What the processor is working on */
	/*
		Number of bytes just copied (0 if not copied).  Modified by
		call to alloc/copy.
	*/
	int bytesCopied;
	/*
		Non-zero if object just copied (check bytesCopied) might have
		pointer field.
	*/
	int needScan;
	CopyRange_t copyRange;	/* Only one active per processor */
	/* Last hash entry key/data for optimizing LookupCallinfo. */
	unsigned long lastHashKey;
	void* lastHashData;
	CallinfoCursor_t lastCallinfoCursor;

	/* Less frequently accessed fields */
	pthread_t pthread;	/* pthread that this system thread is implemented as */
	ptr_t writelist[3 * 4096];

	/* Rotating history of last $n$ segments */
	Summary_t history[1000000];
	int lastSegWorkDone;
	int firstHistory;	/* index of first slot */
	int lastHistory;	/* Index of first unused slot */

	Statistic_t numWritesStatistic;
	/* just minor - won't this exclude large objects? XXXX */
	Statistic_t bytesAllocatedStatistic;
	Statistic_t bytesReplicatedStatistic;	/* only for concurrent collectors */
	Statistic_t bytesCopiedStatistic;	/* both minor and major */
	/* XXX Should the next 3 be program-wide? */
	Statistic_t workStatistic;
	Statistic_t schedulerStatistic;
	Statistic_t accountingStatistic;
	Statistic_t idleStatistic;
	Histogram_t mutatorHistogram;
	Statistic_t gcStatistic;
	Statistic_t gcIdleStatistic;
	Statistic_t gcWorkStatistic;
	Statistic_t gcStackStatistic;
	Statistic_t gcGlobalStatistic;
	Statistic_t gcWriteStatistic;
	Statistic_t gcReplicateStatistic;
	Statistic_t gcOtherStatistic;
	Histogram_t timeDivWorkHistogram;
	Histogram_t gcPauseHistogram;
	Histogram_t gcFlipOffHistogram;
	Histogram_t gcFlipOnHistogram;
	Histogram_t gcFlipBothHistogram;
	Histogram_t gcFlipTransitionHistogram;
	WindowQuotient_t utilizationQuotient1;
	WindowQuotient_t utilizationQuotient2;

	long numCopied;	/* Number of objects copied */
	/*
		Number of times an object is reached after it's already been
		forwarded.
	*/
	long numShared;
	long numContention;
	/* Number of failed (simultaneous) attempts to copy an object. */
	long numRoot;
	long numLocative;

	char* buf;	/* buf, bufsize for mlstring2cstring_static */
	int bufsize;

	char* tab;
	char delayMsg[1024];
};

typedef volatile struct Object_Profile__t Object_Profile_t;
struct Object_Profile__t {
	int RecordWord;
	int Record;
	int Pair;
	int IArray;
	int RArray;
	int PArray;
	int IArrayWord;
	int RArrayWord;
	int PArrayWord;
};

typedef enum {
	GCOff, GCPendingAgressive, GCAgressive, GCPendingOn, GCOn,
	GCPendingOff,
} GCStatus_t;

typedef enum {
	Minor, Major,
} GCType_t;

typedef enum {
	NoWordAlign, OddWordAlign, EvenWordAlign,
} Align_t;

typedef enum {
	PointerField, IntField, DoubleField, MirrorPointerField,
	OldPointerField,
} Field_t; 

typedef struct ArraySpec__t ArraySpec_t;
struct ArraySpec__t {
	Field_t type;
	/*
		ElemLen is bytes for intField, words for pointerField, and
		doublewords for doubleField.  ByteLen is (elemLen+3)/4*4 for
		intField, elemLen*4 for pointerField, and elemLen*8 for
		doubleField.
	*/
	int elemLen;
	int byteLen;
	val_t intVal;
	volatile ptr_t pointerVal;
	double doubleVal;
};

typedef enum {
	Copy, LocCopy, LocAlloc,
} LocAllocCopy_t;

typedef enum {
	OneSpace, OneSpaceLarge, TwoSpaceLarge,
} SourceSpaceCheck_t;

typedef enum {
	DoSpaceCheck, NoSpaceCheck,
} SpaceCheck_t;

typedef enum {
	NoTransfer, Transfer, SelfTransfer, BackTransfer,
} Transfer_t;

typedef enum {
	NoSet, PrimarySet, ReplicaSet,
} StackType_t;

typedef enum {
	NoCopyWrite, DoCopyWrite,
} CopyWrite_t;

typedef enum {
	NoCopyCopy, DoCopyCopy,
} CopyCopy_t;

enum {
	TILT_PAGESIZE=8192,

	/*
		Tags and related constants.
		Must agree with ../../Rtl/tags.sml
	*/
	FORWARD1_TYPE	= 0x0,
	FORWARD2_TYPE	= 0x4,
	/*
		Note that 0x0 and 0x4 cannot be used as those correspond to
		possible pointer values.
	*/
	RECORD_TYPE	= 0x1,
	WORD_ARRAY_TYPE	= 0x2,
	QUAD_ARRAY_TYPE	= 0x3, /* Quad aligned and quad accessed */
	PTR_ARRAY_TYPE	= 0x5,
	MIRROR_PTR_ARRAY_TYPE	= 0x6,
	OTHER_TYPE	= 0x7,
	NONSKIP_OTHER_TYPE	= 0xf,

	NONSKIP_OTHER_TY_OFFSET	= 5,

#	define TAG(ty)\
		(NONSKIP_OTHER_TYPE\
		| ((ty)<<NONSKIP_OTHER_TY_OFFSET))
	/*
		SEGSTALL_TAG is different from STALL_TAG so object is parsable
		starting at first tag.
	*/
	STALL_TAG	= TAG(0),
	SEGSTALL_TAG	= TAG(1),
	SEGPROCEED_TAG	= TAG(2),
	MIRROR_GLOBAL_PTR_TAG	= TAG(3),
#	undef TAG

	/* Records are not allowed to be empty */
	RECLEN_OFFSET	= 3,
	RECLEN_MAX	= 24,
	RECMASK_OFFSET	= 8,

#	define TAG(len,mask)\
		(RECORD_TYPE\
		| ((len)<<RECLEN_OFFSET)\
		| ((mask) << RECMASK_OFFSET))
	TAG_REC_INT	= TAG(1,0),
	TAG_REC_TRACE	= TAG(1,1),
	TAG_REC_INTINT	= TAG(2,0),
	TAG_REC_TRACEINT	= TAG(2,1),
	TAG_REC_INTTRACE	= TAG(2,2),
	TAG_REC_TRACETRACE	= TAG(2,3),
#	undef TAG

	ARRLEN_OFFSET	= 3,

	/* Trace table values.  Must match tracetable.sml. */
	TRACE_NO=0, TRACE_YES, TRACE_CALLEE, TRACE_SPECIAL,
	SpTyShift	= 30,
	SpTyMask	= 3,
	SpTyStackRec=0, SpTyLabelRec, SpTyGlobalRec, SpTyUnset,
};

#ifdef DEBUG
#define fastAssert(x)  assert(x)
#else
#define fastAssert(x) 
#endif

#define DivideUp(a,div) (((int)(a) + (div) - 1) / (div))
#define DivideDown(a,div) ((int)(a)  / (div))
#define RoundUp(x,mult) (((int)(x) + (mult) - 1) / (mult)*(mult))
#define RoundDown(x,mult) (((int)(x) / (mult))*(mult))
#define Max(a,b) ((a) > (b) ? (a) : (b))
#define Min(a,b) ((a) < (b) ? (a) : (b))
#define typed_swap(t,a,b) { t swap_temp = a; a = b; b = swap_temp; }

#define uninit_val	((val_t)258)

/* Of the other tags, fourth bit zero is reserved for skip tags */
#define IS_SKIP_TAG(t)	((((tag_t)t) & 15) == 0x7)
#define IS_NONSKIP_OTHER_TAG(t)	((((tag_t)t) & 15) == 0xf)
#define MAKE_SKIP(w)	(((w) << 4) | 0x7)
#define GET_SKIPWORD(t)	(((tag_t)t) >> 4)
#define GET_TYPE(t)	(((tag_t)t) & 0x7)
#define TYPE_IS_FORWARD(t)\
	((t) == FORWARD1_TYPE || (t) == FORWARD2_TYPE)
#define TYPE_IS_ARRAY(t)\
	((t) == WORD_ARRAY_TYPE ||\
	 (t) == QUAD_ARRAY_TYPE ||\
	 (t) == PTR_ARRAY_TYPE ||\
	 (t) == MIRROR_PTR_ARRAY_TYPE)
#define TAG_IS_FORWARD(t)	((((tag_t)t) & 0x3) == 0)
#define TAG_IS_OTHER(t)	((((tag_t)t) & 0x7) == OTHER_TYPE)

/* Records are not allowed to be empty */
#define MAKE_REC(len,mask)\
	((len << RECLEN_OFFSET) |\
	 (mask << RECMASK_OFFSET) |\
	 RECORD_TYPE)
/*
	GET_RECLEN
		record length in words
	GET_WORD_ARRAY_LEN
		array length in bytes - includes char array
	GET_QUAD_ARRAY_LEN
		array length in bytes - mult of 8
	GET_PTR_ARRAY_LEN
		array length in bytes - mult of 4
	GET_MIRROR_PTR_ARRAY_LEN
		array length in bytes - mult of 8
	GET_ANY_ARRAY_LEN
		array length in bytes
*/
#define GET_RECLEN(t)	((((tag_t)t) >> RECLEN_OFFSET) & 31)
#define GET_RECMASK(t)	(((tag_t)t) >> RECMASK_OFFSET)
#define GET_WORD_ARRAY_LEN(t)	(((tag_t)t) >> ARRLEN_OFFSET)
#define GET_QUAD_ARRAY_LEN(t)	(((tag_t)t) >> ARRLEN_OFFSET)
#define GET_PTR_ARRAY_LEN(t)	(((tag_t)t) >> ARRLEN_OFFSET)
#define GET_MIRROR_PTR_ARRAY_LEN(t)\
	(((tag_t)t) >> ARRLEN_OFFSET)
#define GET_ANY_ARRAY_LEN(t)	(((tag_t)t) >> ARRLEN_OFFSET)

#define IS_TRACE_NO(trace)	((trace)==TRACE_NO)
#define IS_TRACE_YES(trace)	((trace) == TRACE_YES)
#define IS_TRACE_CALLEE(trace)	((trace) == TRACE_CALLEE)
#define IS_TRACE_SPECIAL(trace)	((trace) == TRACE_SPECIAL)

#define GET_SPECIAL_TYPE(x)	(((x)>>SpTyShift) & SpTyMask)
#define IS_SPECIAL_STACK_REC(type)	((type)==SpTyStackRec)
#define IS_SPECIAL_LABEL_REC(type)	((type)==SpTyLabelRec)
#define IS_SPECIAL_GLOBAL_REC(type) ((type)==SpTyGlobalRec)
#define IS_SPECIAL_UNSET(type)	((type)==SpTyUnset)

/* ML link unit */
extern int	link_modulecount;
extern val_t	link_gctable;
extern val_t	link_globalstart;
extern val_t	link_globalend;
extern val_t	link_traceglobalstart;
extern val_t	link_traceglobalend;
extern val_t	link_LINKUNIT_DOTmain;

/* ML TiltExn unit */
/*
	Locations of important exceptions.  Changes to name.sml,
	toil.sml, or tonil.sml may affect these.
*/
#ifdef FLATTEN_MODULES
	extern val_t	ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTDiv__0_DOTmk__i__INT;
	extern val_t	ml__PLUSF_PLUSUTiltExn__INT__r__i_DOTOverflow__0_DOTmk__i__INT;
#else
	extern val_t	ml__PLUSUTiltExn__INT__r__INT;
#endif

/* barriers.c */
/*
	StrongBarrier returns index of arrival after all processors
	have reached barrier.

	WeakBarrier returns index of arrival immediately; that is, it
	does not have barrier semantics.

	CheckBarrier returns next index of arrival immediately.
*/
Barriers_t*	createBarriers(int size, int phase);
int	strongBarrier(Barriers_t*, Proc_t*);
int	weakBarrier(Barriers_t*, Proc_t*);
int	checkBarrier(Barriers_t*, Proc_t*);

/* bitmap.c */
/*
	Not thread-safe.
*/
Bitmap_t*	CreateBitmap(int size);
void	DestroyBitmap(Bitmap_t*);
int	BitmapSize(Bitmap_t*);
int	ClearBitmap(Bitmap_t*);
int	AllocBitmapRange(Bitmap_t*, int size);	/* multiple callers for allocation */
int	SetBitmapRange(Bitmap_t*, int start, int size);	/* multiple callers for marking */
int	IsSet(Bitmap_t*, unsigned int);

/* commandline.c */
void setCommandLine(char* cmd, char** argv);

/* create.c */
void	init_iarray(ptr_t obj, int byteLen, int v);
void	init_parray(ptr_t obj, int len, ptr_t v);
/* wordLen is double logical length. */
void	init_double_ptr_array(ptr_t obj, int logLen, ptr_t v);
void	init_farray(ptr_t obj, int len, double v);

/* exn.c */
extern uint32 exncounter;
void	exn_init(void);
void raise_exception(ucontext_t*, ptr_t);
void raise_exn(ptr_t);
void toplevel_exnhandler(Thread_t*);
/*
	The portable routines mkOverflowExn and mkDivExn construct new
	exception records each time they are invoked.  These project
	global records out of the basis library.
*/
exn	getOverflowExn(void);
exn	getDivExn(void);

/* firstdata.c */
extern unsigned long	firstdata;
extern unsigned long	firsttext;

/* forward.c */
/*
	Gives the object length including tag words.  A skip tag is also
	legal input.
*/
unsigned long	objectLength(ptr_t obj, mem_t* start);
int	empty_writelist(Proc_t*);
void	process_writelist(Proc_t*, Heap_t*, Heap_t*);
int	try_process_writelist(Proc_t*, Thread_t*, Heap_t*, Heap_t*);
/* Leftover area is padded with PadHeapArea */
void 	DischargeCopyRange(CopyRange_t*);
/* Leftover area is padded with PadHeapArea */
mem_t	AllocFromCopyRangeSlow(Proc_t*, int size, Align_t align);
void	InitCopyRange(CopyRange_t*, Proc_t*, Heap_t*);
void	SetCopyRange(CopyRange_t*, Proc_t*, Heap_t*, Set_t* region);
/* Current unused area is padded with PadHeapArea */
void	PadCopyRange(CopyRange_t*);
void	ClearCopyRange(CopyRange_t*);
int	IsNullCopyRange(CopyRange_t*);
/*
	Allocate entire heap to copy range - for uniprocessors so space
	check is avoided.
*/
void	AllocEntireCopyRange(CopyRange_t*);
/*
	Return remainder of copy range to heap - copy range must reside
	contiguous to heap.
*/
void	ReturnCopyRange(CopyRange_t*);
void	AddGrayCopyRange(CopyRange_t*);
/*
	The following definitions apply to these prototypes and to the
	inlined forwarding and scanning code in gcinline.h.

	Note that "copy" = "alloc" + "transfer"

	transferScanObj_*
		copy the fields of the given primary object to the replica
		object indicated by the forwarding pointer; apply a function *
		to each pointer field of the object; returns nothing
	scanObj_*
		apply a function * to each pointer field of the object;
		returns nothing
	scanTag_*
		apply a function * to each pointer field of the object just
		after the tag; returning address just past obj
	scanUntil_*
		apply a function * to each pointer field of the objects until
		the allocation pointer equals the scan pointer
	alloc_*
		alloc space for a copy of the object and install a forwarding
		pointer in the original object but do not copy fields
	copy_*
		makes a copy of an object and install a forwarding pointer in
		the original object
	locCopy_*
		update a location containing a pointer to an object with a
		pointer to the copy; this usually involves calling the copy_*
		version of the function
	locAlloc_*
		update a location containing a pointer to an object with a
		pointer to the copy; this usually involves calling the alloc_*
		version of the function
	noSpaceCheck_
		assume there is enough room in the CopyRange; this is possible
		in uniprocessor stop-and-copy collection
	copyCopySync_*
		a modifier on copy/alloc; the object is locked by atomically
		installing STALL_TAG; to synchronize multiple copiers
	scanWriteSync_*
		a modifier on scan; the object is reserved by writing
		STALL_TAG; to synchronize a copier and a writer
	primarySet_*
		a copy/alloc routine that inserts the primary into the set if
		the primary was actually copied
	replicaSet_*
		a copy/alloc routine that inserts the replica into the set if
		the primary was actually copied
	1_, 1L_
		qualifying the copy/alloc by checking if the pointer is in the
		given source space; 1L also adds large space objects to the
		large root set.
	2_, 2L_
		qualifying the copy/alloc by checking if the pointer is in one
		of 2 source spaces; 2L also adds large space objects to the
		large root set.
	unique_
		assumes object has not not been copied yet and that there are
		no other copiers

	copy_* and alloc_*
		(1) Assumes obj is a heap pointer
		(2) Takes a primary object and (if it is not already copied)
			(a) allocate space for a new copy of the object,
			(b) copy the tag and the fields (if copy and not alloc)
			(c) install a forwarding pointer from the primary to the
			copy.
		(3) The allocation pointer and limit pointer are updated in
		copyRange.
		(4) The number of bytes copied is stored in proc->bytesCopied
		(0 if not copied).  proc->segUsage will also be updated.
		(5) The (possibly previously) copied object is returned.
		(6) When _copyCopySync_ is present, the object being copied is
		locked first to prevent multiple copies from being made
		because of multiple copiers.  For copy (not alloc), the object
		is copied coarsely, all fields at once, even if the object is
		large.
		(7) When _noSpaceCheck_ is present, the limit check on the
		copyRange is omitted.
		(8) When _primarySet is present, the original object, if just
		copied, is added to the set When _replicaSet is present, the
		new object, if a copy was just made, is added to the set
*/
ptr_t	copy(Proc_t*, ptr_t obj);
ptr_t	alloc(Proc_t*, ptr_t obj);
ptr_t	copy_replicaSet(Proc_t*, ptr_t obj);
ptr_t	copy_noSpaceCheck(Proc_t*, ptr_t obj);
ptr_t	copy_noSpaceCheck_replicaSet(Proc_t*, ptr_t obj);
ptr_t	copy_copyCopySync(Proc_t*, ptr_t obj);
ptr_t	copy_copyCopySync_primarySet(Proc_t*, ptr_t obj);
ptr_t	copy_copyCopySync_replicaSet(Proc_t*, ptr_t obj);
ptr_t	alloc_copyCopySync(Proc_t*, ptr_t obj);
ptr_t	alloc_copyCopySync_primarySet(Proc_t*, ptr_t obj);
ptr_t	alloc_copyCopySync_replicaSet(Proc_t*, ptr_t obj);
ptr_t	copy_noSpaceCheck_copyCopySync(Proc_t*, ptr_t obj);
ptr_t	copy_noSpaceCheck_copyCopySync_replicaSet(Proc_t*, ptr_t obj);

/* gc.c */
typedef enum {
	Semispace, Generational, SemispaceParallel,
	GenerationalParallel, SemispaceConcurrent, GenerationalConcurrent,
	SemispaceExplicit,
} Collector_Type_t;

typedef enum {
	DefaultOrder, ImplicitOrder, QueueOrder, StackOrder,
	HybridOrder,
} Order_t;

extern int	paranoid;
extern int	verbose;
extern int	diag;
extern int	collectDiag;
extern int	timeDiag;
extern int	debug;
extern Collector_Type_t	collector_type;
extern int	SHOW_GCERROR;
extern int	SHOW_GCSTATS;
extern int	SHOW_GCDEBUG;
extern int	SHOW_HEAPS;
extern int	SHOW_GLOBALS;
extern int	SHOW_GCFORWARD;
extern double	pauseWarningThreshold;
extern double	warnUtil;
extern int	warnThreshold;
extern int	doCopyCopySync;
extern int	noSharing;
extern int	noWorkTrack;
extern int	relaxed;
extern int	addOldStackletOnUnderflow;
extern int	cacheSize;	/* in pages */
extern int	fetchSize;
extern int	cacheSize2;
/*
	GCType is used by Gen, GenPara, and GenConc.  GCStatus is used by
	SemiConc and GenConc.  Semi and SemiPara have no state variables.
*/
extern volatile GCType_t	GCType;
extern volatile GCStatus_t	GCStatus;
extern Statistic_t	heapSizeStatistic;
extern Statistic_t	minorSurvivalStatistic;
extern Statistic_t	majorSurvivalStatistic;
extern Heap_t*	fromSpace;	/* The two semispaces for the ... */
extern Heap_t*	toSpace;	/* tenured area of a generational collector. */
extern Heap_t*	nursery;	/* Used by the generational collector. */
extern SharedStack_t*	workStack;	/* Used by all ...*/
extern Barriers_t*	barriers;	/* parallel/concurrent collectors. */
extern int	NumGC;
extern int	NumMajorGC;
extern int	forceMirrorArray;	/* Are we flipping globals and arrays? */
extern int	mirrorGlobal;
extern int	mirrorArray;
extern int	primaryGlobalOffset;	/* Used by concurrent collector ... */
extern int	replicaGlobalOffset;	/* to support global root redirection. */
extern int	primaryArrayOffset;	/* Used by generational/concurrent ... */
extern int	replicaArrayOffset;	/* collector to support atomic redirection. */
extern int	NurseryByte;
extern int	MinHeapByte;
extern int	MaxHeapByte;
extern int	accountingLevel;
extern double	MinRatio;
extern double	MaxRatio;
extern int	MinRatioSize;
extern int	MaxRatioSize;
extern long	NumRoots;
extern long	NumContentions;
extern long	NumWrites;
extern long	NumLocatives;
extern int	GenKBytesCollected;
extern int	minOffRequest;
extern int	maxOffRequest;
extern int	minOnRequest; 
extern int	copyPageSize;	/* Size of area allocated for a collector */
/* Amount limit is increment for creation of gray regions */
extern int	copyCheckSize;
/*
	Minimum size of object that causes direct allocation from heap if
	local pool empty.
*/
extern int	copyChunkSize;
extern int	threadFetchSize;	/* Number of thread/stackchains to fetch */
/* Number of globals to fetch from global pool */
extern int	globalLocFetchSize;
/* Number of root locs to fetch from global pool */
extern int	rootLocFetchSize;
extern int	objFetchSize;	/* Number of objects to fetch from global pool */
/* Number of gray regions to fetch from global pool */
extern int	grayRegionFetchSize;
/* Number of (large object) segments to fetch from global pool */
extern int	segFetchSize;
/* Number of back objects to fetch from global pool */
extern int	backObjFetchSize;
/* Number of back locations to fetch from global pool */
extern int	backLocFetchSize;
extern int	doAgressive;
extern int	doMinorAgressive;
extern double	useLastUtil;
extern double	targetUtil;;
extern int	arraySegmentSize;
/*
	If zero, not splitting large arrays.  An array of more than
	arraySegmentSize bytes is considered large and broken into
	segments for incremental copying.  Each segment (except possibly
	the last) is of size arraySegmentSize.
*/
extern Order_t	ordering;	/* Implicit queue, explicit stack, explicit queue */
/* Do space check even when not necessary */
extern int	forceSpaceCheck;
extern double	CollectionRate;	/* Ratio of coll rate to alloc rate */
/* In concurrent collector, store gray set as replicas (with backpointers) */
extern int	grayAsReplica;
extern double	objCopyWeight;
extern double	objScanWeight;
extern double	fieldCopyWeight;
extern double	fieldScanWeight;
extern double	ptrFieldScanWeight;
extern double	pageWeight;
extern double	rootWeight;
extern double	globalWeight;
extern double	stackSlotWeight;
/*
	AllocFromThread and AllocFromHeap bytesToAlloc does
	not include alignment.
*/
mem_t	AllocFromThread(Thread_t*, int bytesToAlloc, Align_t align);
mem_t	AllocFromHeap(Heap_t*, Thread_t*, int bytesToAlloc, Align_t align);
void	GCFromC(Thread_t*, int RequestSizeBytes, int isMajor);
/*
	Is there enough allocation and write buffer space in processor to
	map thread?
*/
int	GCSatisfiable(Proc_t*, Thread_t*);
/*
	Assign heap to allocation area of a processor - if heap is null,
	fields set to StartHeapLimit - must be null for multiple
	processors
*/
void	ResetAllocation(Proc_t*, Heap_t*);
/*
	These are initialization and finalization routines.
*/
void	GCInit(void);
void	GCInit_Help(double defaultMinRatio, double defaultMaxRatio, 
	int defaultMinRatioSize, int defaultMaxRatioSize);
void	GCInit_Semi(void);
void	GCInit_Gen(void);
void	GCInit_SemiPara(void);
void	GCInit_GenPara(void);
void	GCInit_SemiConc(void);
void	GCInit_GenConc(void);
/*
	Idle (unmapped) processors call the poll function periodically in
	case there is GC work.  It may return immediately or do some work.
*/
void	GCPoll(Proc_t*);
void	GCPoll_SemiPara(Proc_t*);
void	GCPoll_GenPara(Proc_t*);
void	GCPoll_SemiConc(Proc_t*);
void	GCPoll_GenConc(Proc_t*);
/*
	Actual collection routines.
*/
void	GCFromScheduler(Proc_t*, Thread_t*);  
/* Does not return; goes to scheduler; argument may be NULL. */
void	GCFromMutator(Thread_t*);
/* Called by scheduler when a thread is unmapped. */
void	GCReleaseThread(Proc_t*);
/*
	The collector functions.
*/
void	GC_Semi(Proc_t*, Thread_t*);
void	GC_SemiPara(Proc_t*, Thread_t*);
void	GC_SemiConc(Proc_t*, Thread_t*);
void	GC_Gen(Proc_t*, Thread_t*);
void	GC_GenPara(Proc_t*, Thread_t*);
void	GC_GenConc(Proc_t*, Thread_t*);
/*
	Must be called each time a thread is released.
*/
void	GCRelease_Semi(Proc_t*);
void	GCRelease_Gen(Proc_t*);
void	GCRelease_SemiPara(Proc_t*);
void	GCRelease_GenPara(Proc_t*);
void	GCRelease_SemiConc(Proc_t*);
void	GCRelease_GenConc(Proc_t*);
int	returnToML(Thread_t*, mem_t linkValue);
int	returnFromGCFromC(Thread_t*);
int	returnFromGCFromML(Thread_t*);
int	returnFromYield(Thread_t*);
/*
	Allocating large arrays.  These routines call may call GCFromC.
*/
ptr_t	AllocBigArray_Semi(Proc_t*, Thread_t*, ArraySpec_t*);
ptr_t	AllocBigArray_Gen (Proc_t*, Thread_t*, ArraySpec_t*);
ptr_t	AllocBigArray_SemiPara(Proc_t*, Thread_t*, ArraySpec_t*);
ptr_t	AllocBigArray_GenPara(Proc_t*, Thread_t*, ArraySpec_t*);
ptr_t	AllocBigArray_SemiConc(Proc_t*, Thread_t*, ArraySpec_t*);
ptr_t	AllocBigArray_GenConc(Proc_t*, Thread_t*, ArraySpec_t*);
/*
	The amount "request" is added to what is considered live.  The
	amount "withhold" is subtracted from both spaces for computation
	of liveness ratio.  The fraction "reserve" is reserved for
	concurrent collector.
*/
void	HeapAdjust1(int request, int unused, int withhold,
	double rate, int phases, Heap_t* from1, Heap_t* to);
void	HeapAdjust2(int request, int unused, int withhold,
	double rate, int phases, Heap_t* from1, Heap_t* from2,
	Heap_t* to);
int	expandedToReduced(int size, double rate, int phases);
int	reducedToExpanded(int size, double rate, int phases);
/*
	Make sure all the pointer values in the stack/globals are in the
	legal heaps.
*/
void	paranoid_check_all(Heap_t* firstPrimary, Heap_t* secondPrimary,
	Heap_t* firstReplica, Heap_t* secondReplica, Heap_t* largeSpace);
void	measure_semantic_garbage_after(void);

/* gc_gen.c */

/* gc_gen_conc.c */

/* gc_gen_para.c */

/* gc_large.c */
extern int	LargeHeapByte;
Heap_t*	largeSpace;

void	gc_large_init(void);
mem_t	gc_large_alloc(Proc_t*, int byteLen, Align_t);   
void	gc_large_startCollect(void);	/* one caller only */
void	gc_large_addRoot(ptr_t);	/* multiple callers */
void	gc_large_endCollect(void);	/* one caller only */

/* gc_para.c */
/*
	isEmptySharedStack
		Was is (possibly) empty at some point?  Conservative.
	resetSharedStack
		Make local stack seem non-empty.
	popSharedStack(ss,lw)
		Increments ss->numStack and lw->hasShared if number of items
		fetched > 0.
	pushSharedStack(conditional,ss,lw)
		Use conditional = 1 for pushes that don't effect termination.
		Decrements ss->numStack and resets lw->hasShared to 0 if
		lw->hasShared was 1.  Returns 1 if global stack empty and
		numStack is 0.
*/
SharedStack_t*	SharedStack_Alloc(int doubleProcess, int stackletSize,
	int globalLocSize, int rootLocSize, int objSize, int segmentSize, 
	int backObjSize, int backLocSize);
int	isEmptySharedStack(SharedStack_t*);
void	discardNextSharedStack(SharedStack_t*);
void	resetSharedStack(SharedStack_t*, LocalWork_t*, int getNext);
void	popSharedStack(SharedStack_t*, LocalWork_t*);
int	pushSharedStack(int conditional, SharedStack_t*, LocalWork_t*);

/* gc_semi.c */

/* gc_semi_conc.c */

/* gc_semi_para.c */

/* gcstat.c */
void	object_profile_init(Object_Profile_t*);
void	show_gcstats(Object_Profile_t* alloc, Object_Profile_t* collect);
void	gc_heapstat(mem_t bottom, mem_t alloc_ptr);
void	gc_sanity_stackreg_check(reg_t* saveregs, Heap_t* from,
	int* stackbot, int* stacktop);
void	gcstat_heapprofile_beforecollect(mem_t bot, mem_t top);
void	gcstat_heapprofile_aftercollect(mem_t bot, mem_t top);
void	gcstat_show_heapprofile(void);

/* global.c */
extern mem_t	RuntimeGlobalData_Start;
extern mem_t	RuntimeGlobalData_Cur;
extern mem_t	RuntimeGlobalData_End; 
extern mem_t	datastart;
extern mem_t	dataend;
extern mem_t	textstart;
extern mem_t	textend;

void	global_init(void);
val_t	GetGlobal(ptr_t);

/* hash.c */
HashTable_t*	CreateHashTable(int);
void	DestroyHashTable(HashTable_t*);
HashEntry_t*	HashTableLookup(HashTable_t*, unsigned long key, int insert);
void	HashTableInsert(HashTable_t*, HashEntry_t*);

/* main.c */
extern int checkAtGC;
extern int LEAST_GC_TO_CHECK;
void	init_int(int*, int);
void	init_double(double*, double);

/* malloc.c */
void*	ecalloc(size_t, size_t);

/* memobj.c */
/*
	When we don't have a real initial heap limit, we use
	StartHeapLimit.  When heap limit is being used to interrupt a
	thread, we use StopHeapLimit.
*/
extern mem_t	StartHeapLimit;
extern mem_t	StopHeapLimit;
extern int	NumHeap;
extern int	NumStackChain;
extern int	MLStackletSize;
extern int	CStackletSize;
extern int	primaryStackletOffset;
extern int	replicaStackletOffset;
/*
	StackChain_Alloc
		Obtains an initial stacklet.
	StackChain_Copy
		Duplicates stacklets and links to replicas.
	StackChain_Size
		Total size of active area of stacklets.
*/
StackChain_t*	StackChain_Alloc(void*);
void	StackChain_Dealloc(StackChain_t*);
StackChain_t*	StackChain_Copy(void*, StackChain_t*);
int	StackChain_Size(StackChain_t*);
/*
	Stacklet_Dealloc
		Decrease reference count; if freed, call dealloc on replica.
	GetStacklet
		Get stacklet for given stack pointer; stack chain can be
		obtained through parent field.
	CurrentStacklet
		Get bottom stacklet of chain.
	NewStacklet
		Allocate new stacklet to chain.
	EstablishStacklet
		Fix stackchain cursor (possible exceptions) and
		return active stacklet.
	PopStacklet
		Pop the most recent stacklet; at least one must remain.
	Stacklet_Copy
		Replica area copied from primary area of stacklet; returns
		whether caller was copier.
	Stacklet_KillReplica
		Mark replica area inconsistent with primary area.
*/
int	StackletId(Stacklet_t*);
mem_t	StackletPrimaryTop(Stacklet_t*);
mem_t	StackletPrimaryCursor(Stacklet_t*);
mem_t	StackletPrimaryBottom(Stacklet_t*);
void	Stacklet_Dealloc(Stacklet_t*);
Stacklet_t*	GetStacklet(mem_t sp);
Stacklet_t*	CurrentStacklet(StackChain_t*);
Stacklet_t*	NewStacklet(StackChain_t*);
Stacklet_t*	EstablishStacklet(StackChain_t*, mem_t sp);
void	PopStacklet(StackChain_t*);
mem_t	StackError(ucontext_t*, mem_t);
int	Stacklet_Copy(Stacklet_t*);
void	Stacklet_KillReplica(Stacklet_t*);
void	DequeueStacklet(StackChain_t*);

void	memobj_init(void);

void	SetRange(range_t*, mem_t low, mem_t high);

Heap_t*	Heap_Alloc(int MinSize, int MaxSize);
Heap_t*	GetHeap(ptr_t);
int	inSomeHeap(ptr_t);
void	Heap_Check(Heap_t*);
void	Heap_Reset(Heap_t *);
int	Heap_ResetFreshPages(Proc_t*, Heap_t*);
/*
	Resizes the heap, making mprotect calls if paranoid.  The cursor
	is set to bottom if reset is true.  If the heap is being shrunk,
	then reset must be true.
*/
void	Heap_Resize(Heap_t*, long newSize, int reset);
int	Heap_GetSize(Heap_t*);
int	Heap_GetMaximumSize(Heap_t*);
int	Heap_GetAvail(Heap_t*);
int	Heap_GetUsed(Heap_t*);
void	PadHeapArea(mem_t bottom, mem_t top);

/* platform.c */
int	GetBcacheSize(void); /* secondary cache size */
int	GetIcacheSize(void); /* primary icache size */
int	GetDcacheSize(void); /* primary dcache size */
int	GetPhysicalPages(void); /* main memory size */ 

/* queue.c */
/*
	Normalize ensures that the set has at least three slots free.

	SetCopy(from,to) copies the contents of from into to without
	changing from.

	SetTransfer(from,to) transfers the contents of from into to,
	leaving from empty.

	Dequeue and the SetPop functions return NULL if the set is
	empty.
*/
Set_t*	SetCreate(long initialSize);
void	SetInit(Set_t*, long initialSize);
void	SetDestroy(Set_t*);
void	SetNormalize(volatile Set_t*); 
void	SetNormalizeExpand(volatile Set_t*, int addSize);
void	SetCopy(Set_t* from, Set_t* to); 
void	SetTransfer(Set_t* from, Set_t* to);

/* rooms.c */
/*
	exitRoom returns
		0 if not last to leave
		1 if last but no finalizer
		r if last and finalizer returns r
	changeRoom(r,i) is equivalent to
		exitRoom(r);
		enterRoom(r,i);
	except that it guarantees entry into room i immediately if room i
	is the next room to be active.
*/
Rooms_t*	createRooms(int n);
void	destroyRooms(Rooms_t*);
void	assignExitCode(Rooms_t*, int, finalizer_t, void*);
void	assignLogCode(Rooms_t*, int, logger_t, void*);
void	enterRoom(Rooms_t*, int);
int	exitRoom(Rooms_t*);
int	changeRoom(Rooms_t*, int);

/* show.c */
extern int	numErrors;
extern int	errorsToShow;
int	inHeaps(ptr_t, Heap_t** legalHeaps, Bitmap_t** legalStarts);
void	scan_heap(char* label, mem_t start, mem_t finish, mem_t top,
	Heap_t** legalHeaps, Bitmap_t** legalStarts, int show,
	ShowType_t replicaType, Bitmap_t* makeStart);
void	memdump(char* title, unsigned int* start, int len, unsigned int* target);

/* sigaction.c */

/* signal.c */
void	signal_init(void);
void	install_signal_handlers(int isMain);
mem_t	GetSp(ucontext_t*);
mem_t	GetPc(ucontext_t*);
unsigned long	GetIReg(ucontext_t*, int);
void	GetIRegs(ucontext_t*, unsigned long*);
void	SetIReg(ucontext_t*, int, unsigned long);

/* stack.c */
extern int	debugStack;
extern int	useGenStack;
extern long	GlobalTableSize;
extern long	MutableTableSize;
extern long	CodeSize;
extern long	GCTableSize;
extern long	SMLGlobalSize;
extern long	TotalStackDepth;
extern long	MaxStackDepth;
extern long	TotalStackSize;
extern long	TotalNewStackDepth;

void	show_stack(mem_t sp, mem_t cur_retadd, mem_t top);
void	stack_init(void);
void	add_global_root(Proc_t*, volatile mem_t);
/* Return all initialized global locs not in the tenured list. */
void	minor_global_scan(Proc_t*);
/* Move initialized global locs into the tenured list. */
void	minor_global_promote(Proc_t*);
void	major_global_scan(Proc_t*);
void	stub_error(void);
/* For debugging - pass in primary or replica globalOffset. */
void	NullGlobals(int globalOffset);
/* Scan all root locations in primaryStack */
void	thread_root_scan(Proc_t*, Thread_t*);
/* Initialize snaphosts - returns 1 if thread as started. */
void	initial_root_scan(Proc_t*, Thread_t*);
/* Obtain roots from startStack; returns 1 when stacklet complete. */
int	work_root_scan(Proc_t*, Stacklet_t*);
/* Clean up thread but don't do flip - for CollectorTransition */
void	discard_root_scan(Proc_t*, Thread_t*);
void	complete_root_scan(Proc_t*, Thread_t*);	/* Perform flip */
void	installThreadRoot(Thread_t*, vploc_t);
void	uninstallThreadRoot(Thread_t*, vploc_t);

/* stats.c */
extern int	information;
extern char*	historyFile;
void	reset_timer(Timer_t*);
void	start_timer(Timer_t*);
double	lap_timer(Timer_t*);
void	restart_timer(Timer_t*);
void	stop_timer(Timer_t*);
void	reset_statistic(Statistic_t*);
void	add_statistic(Statistic_t*, double);
void	reset_histogram(Histogram_t*);
void	add_histogram(Histogram_t*, double);
void	reset_windowQuotient(WindowQuotient_t*, int fineness);
void	add_windowQuotient(WindowQuotient_t*, double, int on);
double	get_prewindow(WindowQuotient_t*, int which, double neededOnTime);
void	add_statString(char *);
void	stats_init(void);
void	stats_finish(void);
void	resetTimeList(void);
double	addTimeList(void* proc, int which, int data);
int	showTimeList(double min);

/* support.s */
long	FetchAndAdd(volatile long*, int);
long	TestAndSet(volatile long*);
int	CompareAndSwap(volatile int*, int testValue, int swapValue);
void	memOrder(void);
void	memBarrier(void);

/* thread.c */
extern int	usageCount;
extern int	localWorkSize;
/* locks (de)scheduling of sys threads */
extern pthread_mutex_t ScheduleLock;
extern Thread_t*	Threads;
extern Thread_t*	mainThread;
extern int	NumThread;
extern int	NumProc;
extern int	RotateProc;
extern int	threadDiag;
void	init_localWork(LocalWork_t*, int objSize, int segSize, int globalSize, 
	int rootSize, int stackletSize, int backObjSize, int backLocSize);
int	isLocalWorkAlmostEmpty(LocalWork_t*);	/* exclude backObjs and backLocs */
int	isLocalWorkEmpty(LocalWork_t*);
void	procChangeState(Proc_t*, ProcessorState_t, int which);
long	bytesCopied(Usage_t*);
/*
	Updates workDone as a weighted average of other fields.
*/
void	updateWorkDone(Proc_t*);
Thread_t*	getThread(void);
Proc_t*	getProc(void);
Proc_t*	getNthProc(int);
void	showHistory(Proc_t*, int howMany, char*);  /* filename; NULL for stdout */
void	ResetJob(void);	/* For iterating over all jobs in work list */
Thread_t*	NextJob(void);
/* Change all user thread's limit to StopHeapLimit. */
void	StopAllThreads(void);
double nonMutatorTime(Proc_t*);	/* Time of current GC segment */
double segmentTime(Proc_t*);	/* Time of current GC segment */
void	thread_init(void);
void	thread_go(ptr_t thunk);
void	scheduler(Proc_t*);	/* Unmap user thread of Proc if mapped */
void	Finish(void);
Thread_t*	YieldRest(void);
void	UpdateJob(Proc_t*);	/* GCRelease thread; update processor's info */
void	ReleaseJob(Proc_t*);	/* UpdateJob; release/unmap thread */
void	Thread_Pin(Thread_t*);
void	Thread_Unpin(Thread_t*);
int	thread_total(void);
int	thread_max(void);

static inline tag_t
getTag(vptr_t obj)
{
	tag_t tag = (tag_t) obj[-1];
	while (tag == STALL_TAG)
		tag = (tag_t) obj[-1];
	while (TAG_IS_FORWARD(tag)) {
		ptr_t replica = (ptr_t) tag;
		fastAssert(replica != obj);
		tag = (tag_t) replica[-1];
	}
	return tag;
}

static inline mem_t
AlignMemoryPointer(mem_t alloc, Align_t align)
{
	int curEven;
	if (align == NoWordAlign)
		return alloc;
	curEven = (((val_t)(alloc)) & 7) == 0;
	if ((align == OddWordAlign && curEven) ||
	    (align == EvenWordAlign && !curEven))
		*(alloc++) = MAKE_SKIP(1);
	return alloc;
}

static inline int
UnusedProcAlloc(Proc_t *proc)
{
	return sizeof(val_t) * (proc->allocLimit - proc->allocCursor);
}

static inline int
IsText(vptr_t addr)
{
	return (textstart <= addr && addr <= textend);
}

static inline int
IsGlobalData(vptr_t addr)
{
	return (datastart <= addr && addr <= dataend);
}

static inline int
IsTagData(ptr_t addr)
{
	unsigned long a = (unsigned long)addr;
	return a <= 256;
}

static inline int
InRange(mem_t addr, range_t* range)
{
	return ((unsigned int)addr - (unsigned int)range->low <= range->diff);
}

static inline int
NotInRange(mem_t addr, range_t* range) 
{
	return ((unsigned int)addr - (unsigned int)range->low > range->diff);
}

static inline int
inHeap(vptr_t v, Heap_t *heap)
{
	return (((val_t)v - (val_t)heap->bottom) < heap->size);
}

static inline int
Heap_TouchPage(Heap_t* h, mem_t addr)
{
	int offset = sizeof(val_t) * (addr - h->bottom);
	int page = DivideDown(offset, TILT_PAGESIZE);
	int word = page >> 5;
	int bit = page & 31;
	int mask = 1 << bit;
	int info = h->freshPages[word];
	assert(sizeof(int) == 4);
	assert(addr >= h->bottom && addr < h->top);
	h->freshPages[word] = info | mask;
	return !(mask & info);
}

static inline void
GetHeapArea(Heap_t* heap, int size,
	mem_t* bottom, mem_t* cursor, mem_t* top)
{
	mem_t region = (mem_t) FetchAndAdd((long *)(&heap->cursor), size);
	mem_t newHeapCursor = region + size / sizeof(val_t);
	if (newHeapCursor > heap->top) {
		FetchAndAdd((long *)(&heap->cursor), -size);
		*bottom = *cursor = *top = 0;
	}
	else {
		/* Do most machines have non-blockig read? */
		/* val_t forceRead = *newHeapCursor;     */
		*bottom = region;
		*cursor = region;
		*top = newHeapCursor;
		/* PadHeapArea(*bottom,*top); */
	}
}

static inline long
SetLength(Set_t* s)
{
	return s->last - s->first;
}

static inline long
SetFullSize(Set_t* s)
{
	return s->size;
}

static inline void
SetReset(Set_t* s)
{
	s->first = s->data;
	s->last = s->data;
}

static inline int
SetIsEmpty(Set_t* s)
{
	return (s->first == s->last);
}

static inline void
SetPush(volatile Set_t* set, ptr_t item)
{
	fastAssert(item != NULL);
	if (set->last + 1 >= set->limit)
		SetNormalize(set);
	*(set->last++) = item;
}

static inline void
SetPush2(Set_t* set, ptr_t item1, ptr_t item2)
{
	fastAssert(item1 != NULL);
	fastAssert(item2 != NULL);
	if (set->last + 2 >= set->limit)
		SetNormalize(set);
	*(set->last++) = item1;
	*(set->last++) = item2;
}

static inline void
SetPush3(Set_t* set, ptr_t item1, ptr_t item2, ptr_t item3)
{
	fastAssert(item1 != NULL);
	fastAssert(item2 != NULL);
	fastAssert(item3 != NULL);
	if (set->last + 3 >= set->limit)
		SetNormalize(set);
	*(set->last++) = item1;
	*(set->last++) = item2;
	*(set->last++) = item3;
}

static inline ptr_t
SetDequeue(Set_t* s)
{
	if (s->first == s->last)
		return NULL;
	return *(s->first++);
}

static inline ptr_t
SetPop(Set_t* set)
{
	/*  fastAssert(set->first == set->data); */
	if (set->first == set->last)
		return NULL;
	return *(--set->last);
}

static inline ptr_t
SetPop2(Set_t* set, ptr_t* item2Ref)
{
	fastAssert(set->first == set->data);
	if (set->last > set->first + 1) {
		*item2Ref = *(--set->last);  /* In reverse order of push */
		return *(--set->last);
	}
	return NULL;
}

static inline ptr_t
SetPop3(Set_t* set, ptr_t* item2Ref, ptr_t* item3Ref)
{
	fastAssert(set->first == set->data);
	if (set->last > set->first + 2) {
		*item3Ref = set->last[-1];  /* In reverse order of push */
		*item2Ref = set->last[-2];  /* In reverse order of push */
		set->last -= 3;
		return *set->last;
	}
	return NULL;
}

static inline int
updateGetWorkDone(Proc_t* proc)
{
	updateWorkDone(proc);
	return proc->segUsage.workDone;
}

static inline void
addMaxWork(Proc_t* proc, int additionalWork)
{
	if (additionalWork <= 0)
		return;
	proc->segUsage.maxWork += additionalWork;
	if (proc->segUsage.maxWork < 0)    /* overflow */
		proc->segUsage.maxWork = INT_MAX;
}

static inline int
reachMaxWork(Proc_t* proc)
{
	updateWorkDone(proc);
	proc->segUsage.checkWork =
		Min(proc->segUsage.maxWork,
			proc->segUsage.workDone + localWorkSize);
	return proc->segUsage.workDone >= proc->segUsage.maxWork;
}

static inline int
updateReachCheckWork(Proc_t* proc)
{
	int workDone;
	updateWorkDone(proc);
	workDone = proc->segUsage.workDone;
	if (workDone > proc->segUsage.checkWork) {
		proc->segUsage.checkWork =
			Min(proc->segUsage.maxWork,
				workDone + localWorkSize);
		return 1;
	}
	return 0;
}

static inline int
reachCheckWork(Proc_t* proc)
{
	int workDone;
	if (--proc->segUsage.counter == 0) {
		proc->segUsage.counter = usageCount;
		updateWorkDone(proc);
	}
	workDone = proc->segUsage.workDone;
	if (workDone > proc->segUsage.checkWork) {
		proc->segUsage.checkWork =
			Min(proc->segUsage.maxWork,
				workDone + localWorkSize);
		return 1;
	}
	return 0;
}

static inline ploc_t
DupGlobal(vptr_t global)
{
	ploc_t primaryLoc = (ploc_t) &global[primaryGlobalOffset / sizeof(val_t)];
	ploc_t replicaLoc = (ploc_t) &global[replicaGlobalOffset / sizeof(val_t)];
	*replicaLoc = *primaryLoc;
	return replicaLoc;
}
