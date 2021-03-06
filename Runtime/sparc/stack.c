#include "s.h"
#include "r.h"
#include "sparc.h"

int GCTableEntryIDFlag = 0;  /* let the user code set it if it thinks it's on */
int save_rate = 70;
int useGenStack = 0;


static mem_t gctable = &link_gctable;
static mem_t globalstart = &link_globalstart;
static mem_t globalend = &link_globalend;
static mem_t traceglobalstart = &link_traceglobalstart;
static mem_t traceglobalend = &link_traceglobalend;

int debugStack = 0;
long MaxStackDepth = 0;
long TotalStackDepth = 0;
long TotalNewStackDepth = 0;
long TotalStackSize  = 0;


#define GET_ENTRYSIZE(x)      ((x)         & 65535)
#define GET_FRAMESIZE(x)      (((x) >> 16) & 65535)
#define GET_BYTESTUFFSIZE(x)  ((x)         & 65535)
#define GET_QUAD_RA_OFFSET(x) (((x) >> 16) & 31)


typedef unsigned int bot;

extern val_t global_exnrec; /* C/asm convention has asm label be the Lvalue in C */


void stub_error(void)
{
  DIE("stub_error: should be a dead ra");
}

static inline bot
LookupStackBot(CallinfoCursor_t *cursor, int pos)
{
  unsigned int v = ((int*)(cursor->callinfo->__rawdata))[pos >> 4];
  int whichbot = pos & 15;
  return (v >> (2 * whichbot)) & 3;
}

static inline int
LookupSpecialByte(CallinfoCursor_t *cursor)
{
  return cursor->byteData[cursor->byteOffset++];
}

static inline void
LookupSpecialWordPair(CallinfoCursor_t *cursor, int *a, int *b)
{
  *a = cursor->wordData[cursor->wordOffset++];
  *b = cursor->wordData[cursor->wordOffset++];
  fastAssert (&(cursor->wordData[cursor->wordOffset]) <=
	      (int *) cursor->callinfo + cursor->entrySize);
}

static inline void
LookupSpecialWord(CallinfoCursor_t *cursor, int *a)
{
  *a = cursor->wordData[cursor->wordOffset++];
  fastAssert (&(cursor->wordData[cursor->wordOffset]) <=
	      (int *) cursor->callinfo + cursor->entrySize);
}
HashTable_t *CallinfoHashTable = NULL;
long GCTableSize = 0;
long SMLGlobalSize = 0;
long GlobalTableSize = 0;
long MutableTableSize = 0;

void global_root_init(void);

void stack_init(void)
{
  struct HashEntry e;
  unsigned int mi = 0, count=0;

#ifdef GCTABLE_HASENTRYID
  assert(GCTableEntryIDFlag == 1);
#else
  assert(GCTableEntryIDFlag == 0);
#endif

  for (mi=0; mi<link_modulecount; mi++) {
    int *startpos = (int *)(gctable[mi]);
    int *curpos = startpos; 
    if (debugStack) 
      printf("Scanning GC tables of module %d at %lx\n", mi, (long)startpos);
    while (*curpos) {
      int entrySize = GET_ENTRYSIZE(((Callinfo_t *)curpos)->size0);
      count++;
      assert(entrySize != 0);
      curpos += entrySize;
    }
    GCTableSize += (unsigned long)curpos - (unsigned long)startpos;
    SMLGlobalSize += (long)(globalend[mi]) - 
      (long)(globalstart[mi]);
    MutableTableSize += (long)(traceglobalend[mi]) - 
      (long)(traceglobalstart[mi]);
    }
  CallinfoHashTable = CreateHashTable(2*count);
  for (mi=0; mi<link_modulecount; mi++) {
    int *startpos = (int *)(gctable[mi]);
    int *curpos = startpos; 
    while (*curpos) {
      e.key = (unsigned long)(*curpos);
      e.data = (void *)curpos;
      assert(IsText((ptr_t) e.key));
      HashTableInsert(CallinfoHashTable,&e);
      curpos += GET_ENTRYSIZE(((Callinfo_t *)curpos)->size0);
    }
  }
  global_root_init();
}

static inline Callinfo_t*
LookupCallinfo(Proc_t* proc, val_t retAdd)
{
  struct HashEntry *e;
#if defined(sparc)
  val_t key = retAdd + 8;  /* We add 2 words because 
			      (1) the link value is the address of the calling instruction
			      (2) there is a delay slot. */
#else
    error platform not defined
#endif

   if ((val_t) proc->lastHashKey == key) {
     return proc->lastHashData;
   }
  e = HashTableLookup(CallinfoHashTable,(unsigned long) key,0);
  if (e == NULL)
    DIE("unknown return address during stack trace");
  proc->lastHashKey = e->key;
  proc->lastHashData = e->data;
  return (Callinfo_t *) e->data;
}


/* If client is uptrace, then only some fields are initlialized */
static inline CallinfoCursor_t*
getCursor(Proc_t* proc, Callinfo_t* callinfo)
{
  CallinfoCursor_t *cursor = &proc->lastCallinfoCursor;
  cursor->byteOffset = 0;
  cursor->wordOffset = 0;
  if (cursor->callinfo != callinfo) {
    unsigned int ra = callinfo->regtrace_a;
    unsigned int rb = callinfo->regtrace_b;
    int size0 = callinfo->size0;
    int size1 = callinfo->size1;
    int offset = GET_QUAD_RA_OFFSET(size1);
    cursor->callinfo = callinfo;
    cursor->frameSize = GET_FRAMESIZE(size0);
    cursor->entrySize = GET_ENTRYSIZE(size0);
    if (offset == 31) 
      offset = LookupSpecialByte(cursor);
    cursor->RAQuadOffset = offset;
    cursor->slotCount = -1;
    cursor->yesBits    = ra & ~rb;  /* unconditionally contain pointers */
    cursor->calleeMask = ra & rb;   /* inherit register values from previous frame */
    cursor->specialBits = ~ra & rb; /* contains pointers depending on additional information */
    {
      int stacktrace_rawbytesize = cursor->frameSize >> 2;
      int stacktrace_bytesize = (stacktrace_rawbytesize + 3) & (~3);
      int bytedata_bytesize = GET_BYTESTUFFSIZE(cursor->callinfo->size1) << 2;
      cursor->byteData = callinfo->__rawdata + stacktrace_bytesize;
      cursor->wordData = (int *) (cursor->byteData + bytedata_bytesize);
    }
  }
  return cursor;
}

/* Returns whether cursor was cached */
int CacheCursor(CallinfoCursor_t *cursor)
{
  if (cursor->slotCount >= 0)
    return 1;
  cursor->slotCount--;
  if (cursor->slotCount == -3) {
    int i;
    cursor->slotCount = 0;
    for (i=0; i<cursor->frameSize; i++) {
      unsigned int word = ((int *)cursor->callinfo->__rawdata)[i >> 4];
      int trace = (word >> (2 * (i & 15))) & 3;
      if (!IS_TRACE_NO(trace)) {
	cursor->slot[cursor->slotCount] = i;
	cursor->trace[cursor->slotCount] = trace;
	cursor->slotCount++;
	if (cursor->slotCount >= arraysize(cursor->slot)) {
	  cursor->slotCount = -4;
	  return 0;
	}
      }
    }
    return 1;
  }
  return 0;
}

static inline ptr_t
PathProjectStep(ptr_t base, int indices, int* stop)
{
  int index;
  /* These numbers must match up with tracetable.sml. */
  #define BITS_PER_INDEX 5
  #define INDEX_MASK 31
  #define STEP                           \
    index = indices & INDEX_MASK;        \
    if (index == 0) {                    \
      *stop = 1;                         \
      return base;                       \
    } else {                             \
      indices >>= BITS_PER_INDEX;        \
      base = (ptr_t) base[index - 1];    \
    }
  /* The number of STEPs must match up with tracetable.sml. */
  STEP; STEP; STEP; STEP; STEP; STEP;
  return base;
}

static inline ptr_t
PathProject(CallinfoCursor_t* cursor, ptr_t base, int indices)
{
  int stop = 0;
  for (;;) {
    base = PathProjectStep(base,indices,&stop);
    if (stop) return base;
    LookupSpecialWord(cursor,&indices);
  }
}

int should_trace_special(CallinfoCursor_t *cursor, mem_t cur_sp, int regstate,
			 loc_t data_add, int i)
{
  ptr_t res = 0;  /* res is the type of the value */
  int special_type, special_data, type;
  int shouldTrace = 0;
  ptr_t base;

  LookupSpecialWordPair(cursor, &special_type, &special_data);
  type = GET_SPECIAL_TYPE(special_type);

  if (IS_SPECIAL_STACK_REC(type)) {
    base = (ptr_t)(cur_sp[special_data/4]);
  } else if (IS_SPECIAL_LABEL_REC(type)) {
    base = (ptr_t) special_data; 
  } else if (IS_SPECIAL_GLOBAL_REC(type)) {
    ptr_t global = (ptr_t) special_data;  /* just global address */
    base = (ptr_t) global[primaryGlobalOffset / sizeof(val_t)];
  } else if (IS_SPECIAL_UNSET(type))
    DIE("Registers/Stack locations of type UNSET are not supported");
  else
    DIE("malformed trace table");
  res = PathProject(cursor, base, special_type);
  shouldTrace = (((val_t) res) > 3);   /* Types 0 to 3 represent integral/non-pointer types */
  return shouldTrace;
}

static inline int
should_trace(unsigned long trace, CallinfoCursor_t* cursor,
	mem_t cur_sp, int regstate, val_t* data_add, int i)
{
  if (IS_TRACE_NO(trace)) {
    return 0;
  }
  else if (IS_TRACE_YES(trace)) {
    return 1;
  }
  else if (IS_TRACE_CALLEE(trace)) {
    int pos = LookupSpecialByte(cursor);
    int shouldTrace = regstate & (1 << pos);
    return shouldTrace;
  }
  else 
    return should_trace_special(cursor, cur_sp, regstate, data_add, i);
}

static void uptrace_stacklet(Proc_t *proc, Stacklet_t *stacklet, int stackletOffset)
{
  Set_t *callinfoStack = stacklet->callinfoStack;
  int count = 0;
  mem_t curSP = stacklet->replicaCursor + stackletOffset / sizeof(val_t);
  mem_t curRA = stacklet->replicaRetadd;
  mem_t top = stacklet->baseTop + stackletOffset / sizeof(val_t);

  assert(SetIsEmpty(callinfoStack));
  while (curSP < top) {
    Callinfo_t *callinfo = LookupCallinfo(proc,(val_t)curRA);
    CallinfoCursor_t *cursor = getCursor(proc, callinfo);
    SetPush(callinfoStack, (ptr_t) callinfo);
    if (debugStack) {
      mem_t nextRA = (mem_t)curSP[cursor->RAQuadOffset];
      mem_t nextSP = curSP + cursor->frameSize;
      printf("%d: top = %lx   curSP = %lx  curRA = %lx     nextSP = %lx nextRA = %lx\n",
	     count++,(long)top,(long)curSP,(long)curRA,(long)nextSP,(long)nextRA);
    }
    curRA = (mem_t) curSP[cursor->RAQuadOffset];
    curSP = curSP + cursor->frameSize;
    count++;
  }
  proc->segUsage.stackSlotsProcessed += SetLength(callinfoStack) * 4;
  assert(curSP == top);
}


/* 
Scan stacklet for root locations.
New register state is recorded in stacklet.
The return address in the queue identifies the frame
which is pointed to by bot_sp at the moment.  Note that
bot_sp points to the top of this frame.  Thus, when we are
done, bot_sp will point to the bottom of the last processed frame.  

*/

static unsigned int downtrace_stacklet(Proc_t *proc, Stacklet_t *stacklet,
				       int stackletOffset,
				       volatile unsigned long *saveregs)
{
  int i, j, k;
  mem_t cur_sp = stacklet->baseTop + stackletOffset / sizeof(val_t);
  unsigned int regstate = 0;  /* Initial regstate is empty since callee-save registers are empty */
  Set_t *callinfoStack = stacklet->callinfoStack;

  while (!SetIsEmpty(callinfoStack)) {
    Callinfo_t *callinfo = (Callinfo_t *) SetPop(callinfoStack);
    CallinfoCursor_t *cursor = getCursor(proc, callinfo);
    int numChunks = (cursor->frameSize+15)>>4; /* A chunk is the number of slots for one word of info */

    cur_sp -= cursor->frameSize;
    proc->segUsage.stackSlotsProcessed += cursor->frameSize;
    if (debugStack) {
      printf("==========================================================\n");
      printf("SP = %lx   RA = %d   FrameSize = %d words\n", (long)cur_sp,cursor->callinfo->retadd,cursor->frameSize);
    }
    
    /* Process the stack slots - 16 slots per word of information.
       The stack may not have a multiple of 16 slots but that's fine
       as long as we don't access those slots since the information
       for non-existent slots will be TRACE_NO.  
       (1) Short-circuit if info is zero (23 bits at a time).
       (2) Otheriwse, break iterations into a double loop of 4 by 4 iterations
           so that we can short-circuit on blocks of 4 TRACE_NO's (8 bits at a time).
    */
    if (CacheCursor(cursor)) {
      for (i=0; i<cursor->slotCount; i++) {
	int slot = cursor->slot[i];
	int trace = cursor->trace[i];
	if (should_trace(trace, cursor, cur_sp, regstate, cur_sp + slot, slot)) {
	  ptr_t data = (ptr_t) cur_sp[slot];
	  if (IsTagData(data) || IsGlobalData(data))
	    continue;
	  if (debugStack) 
	    printf("   pushed location %lx\n", (long) &cur_sp[slot]);
	  SetPush(&proc->work.roots, (ptr_t) &cur_sp[slot]); 
	}
      }
    }
    else {
      for (i=0; i < numChunks; i++) {
	unsigned long info = ((int *)cursor->callinfo->__rawdata)[i];
	if (info == 0)
	continue;
	for (j=0; j<4; j++) {
	  if ((info & 0xff) == 0) {
	    info >>= 8;
	    continue;
	  }
	  for (k=0; k<4; k++, info >>= 2) {
	    unsigned long trace = info & 3;
	    int curSlot = 16 * i + 4 * j + k;  /* Faster to recompute this */
	    if (should_trace(trace,cursor,cur_sp,regstate,cur_sp+curSlot,curSlot)) {
	      ptr_t data = (ptr_t) cur_sp[curSlot];
	      if (IsTagData(data) || IsGlobalData(data))
		continue;
	      if (debugStack) 
		printf("   pushed location %lx\n", (long) &cur_sp[curSlot]);
	      SetPush(&proc->work.roots, (ptr_t) &cur_sp[curSlot]);
	    }
	  }
	}
      }
    }

    /* Compute the state of the registers */
    {
      unsigned int tempRegstate = cursor->yesBits | (cursor->calleeMask & regstate);
      unsigned int specialBits = cursor->specialBits;
      if (specialBits != 0) {
	for (i=0; i<8; i++) {
	  if (((specialBits >> (4*i)) & 0xf) == 0)
	    continue;
	  for (j=0; j<4; j++) {
	    int pos = 4*i + j;
	    mem_t data_add = (mem_t) &saveregs[pos];
	    if ((specialBits & (1U << pos)) &&
		(should_trace_special(cursor, cur_sp, regstate, data_add, -1)))
	      tempRegstate |= 1 << pos;
	  }
	}
      }
      regstate = tempRegstate;
    }
  }
  return regstate;
}


static void addRegRoots(Proc_t *proc, volatile unsigned long *saveregs, unsigned int regMask)
{
  int i;
  for (i=0; i<32; i++)
    if (regMask & (1 << i)) {
      ploc_t rootLoc = (ploc_t) &saveregs[i];
      ptr_t rootVal = *rootLoc;
      if (!(IsTagData(rootVal)) && !(IsGlobalData(rootVal))) 
	SetPush(&proc->work.roots, (ptr_t) rootLoc);
    }
}

/* Might be given rootLoc that was already installed */
void installThreadRoot(Thread_t *th, vploc_t rootLoc)
{
  int i;
  for (i=0; i<arraysize(th->rootLocs); i++) {
    if (th->rootLocs[i] == rootLoc)
      return;
    if (th->rootLocs[i] == NULL) {
      th->rootLocs[i] = (ploc_t) rootLoc;
      th->rootVals[i] = (ptr_t) *rootLoc;
      return;
    }
  }
  DIE("installThreadRoot out of rootLocs entries");
}

/* Might be given rootLoc that was never installed */
void uninstallThreadRoot(Thread_t *th, vploc_t rootLoc)
{
  int i;
  for (i=0; i<arraysize(th->rootLocs); i++) {
    if (th->rootLocs[i] == rootLoc) {
      th->rootLocs[i] = NULL;
      th->rootVals[i] = NULL;
      return;
    }
  }
}

void thread_root_scan(Proc_t *proc, Thread_t *th)
{
  unsigned long *saveregs = (unsigned long *)(th->saveregs);
  int i, numFrames = 0, numWords = 0;
  StackChain_t *stack = th->stack;
  ptr_t thunk = th->thunk;

  assert(primaryStackletOffset == 0);

  procChangeState(proc, GCStack, 500);
  if (collectDiag >= 2)
    printf("Proc %d: GC %d: thread_root_scan on thread %ld with thunk %lx\n", proc->procid, NumGC, th->tid, (long)thunk);

  for (i=0; i<arraysize(th->rootLocs); i++) {
    if (th->rootLocs[i] != NULL) {
      ptr_t rootVal = *th->rootLocs[i];
      if (!IsTagData(rootVal) && !IsGlobalData(rootVal)) 
	SetPush(&proc->work.roots, (ptr_t) th->rootLocs[i]);
    }
  }

  /* Alternatively, we can call installThreadRoot and uninstallThreadRoot */
  if (thunk != NULL) {
    if (!IsTagData(thunk) && !IsGlobalData(thunk)) 
      SetPush(&proc->work.roots, (ptr_t) &(th->thunk));
    return;   /* Thunk not yet started and so no more roots */
  }

  for (i=stack->cursor-1; i>=0; i--) {
    Stacklet_t *stacklet = stack->stacklets[i];
    stacklet->state = Pending;
    Stacklet_Copy(stacklet);
    uptrace_stacklet(proc, stacklet, primaryStackletOffset);
    numFrames += SetLength(stacklet->callinfoStack);
    numWords += stacklet->baseTop - stacklet->baseCursor;
  }
  TotalStackDepth += numFrames;
  MaxStackDepth = (numFrames < MaxStackDepth) ? MaxStackDepth : numFrames;
  TotalStackSize += numWords * sizeof(val_t);

  for (i=0; i<stack->cursor; i++) {
    Stacklet_t *stacklet = stack->stacklets[i];
    unsigned int bottomRegstate = downtrace_stacklet(proc, stacklet, primaryStackletOffset, saveregs);
    addRegRoots(proc, &stacklet->bottomBaseRegs[primaryStackletOffset == 0 ? 0 : 32], bottomRegstate | (1 << EXNPTR));
    if (i + 1 == stack->cursor)
      addRegRoots(proc, (unsigned long *)(th->saveregs), bottomRegstate | (1 << EXNPTR));
  }
}

void initial_root_scan(Proc_t *proc, Thread_t *th)
{
  int i;
  StackChain_t *stack = th->stack;
  ptr_t thunk = th->thunk;

  procChangeState(proc, GCStack, 510);
  Thread_Pin(th);

  installThreadRoot(th, (ploc_t) &th->thunk);
  for (i=0; i<sizeof(th->rootLocs)/sizeof(ploc_t); i++) {
    ptr_t rootVal = th->rootVals[i];
    if (rootVal != NULL) {
      if (!IsTagData(rootVal) && !IsGlobalData(rootVal)) 
	SetPush(&proc->work.roots, (ptr_t) &th->rootVals[i]);
    }
  }

  if (thunk != NULL) 
    return;   /* Thunk not yet started and so no more roots */

  assert(stack->cursor > 0);
  for (i=0; i<stack->cursor; i++) {
    Stacklet_t *stacklet = th->stack->stacklets[i];
    stacklet->state = Pending;  /* stacklets must be copied before resumption */
    SetPush(&proc->work.stacklets, (ptr_t) stacklet);
  }
}

/* This will fill up rootLocs */
int work_root_scan(Proc_t *proc, Stacklet_t *stacklet)
{
  volatile reg_t* replicaRegs = &stacklet->bottomBaseRegs[replicaStackletOffset == 0 ? 0 : 32];

  int numWords, numFrames;
  unsigned int bottomRegstate;

  procChangeState(proc, GCStack, 520);
  assert(!useGenStack);

  if (stacklet->state != ActiveCopied && stacklet->state != ActiveCopied) {
    int copied = Stacklet_Copy(stacklet);  /* copy stacklet from primary to replica area */
    if (copied)
      proc->segUsage.stackSlotsProcessed += (stacklet->baseTop - stacklet->baseCursor) / 40;
  }
  if (updateReachCheckWork(proc))
    return 0;

  /* Scan from bottom to get frame sizes and descriptors */
  if (SetIsEmpty(stacklet->callinfoStack)) {
    uptrace_stacklet(proc, stacklet, replicaStackletOffset);
    numFrames += SetLength(stacklet->callinfoStack);
    numWords += stacklet->baseTop - stacklet->baseCursor;
    TotalStackDepth += numFrames;
    TotalStackSize += numWords * sizeof(val_t);
    /*  MaxStackDepth = (numFrames < MaxStackDepth) ? MaxStackDepth : numFrames; */
  }
  if (updateReachCheckWork(proc))
    return 0;

  /* Trace from top to bottom for roots */
  bottomRegstate = downtrace_stacklet(proc, stacklet, replicaStackletOffset, replicaRegs);
  addRegRoots(proc, (unsigned long *) replicaRegs, bottomRegstate | (1 << EXNPTR));
  return 1;
}

int stkSize = 0;

/* Will fill up rootsLocs */
void complete_root_scan(Proc_t *proc, Thread_t *th)
{
  int i;
  StackChain_t *stack= th->stack;
  int firstActive = stack->cursor;
  /* double t1, t2; */
  /*  int upcount = 0, downcount = 0; */

  procChangeState(proc, GCStack, 530);

  /* Thread might not be really be live but was pinned to preserve liveness so 
     that snapshot and snapshotRegs can be used */
  Thread_Unpin(th);   /* Might not have been pinned if thread created after start of GC */
  if (th->used == 0)  /* Thread is actually dead now.  Was live only due to pinning */
    return;

  installThreadRoot(th, (ploc_t) &th->thunk);   /* Thread may not have existed at the beginning of the GC and
						   so initial_root_scan was not called on it */
  for (i=0; i<arraysize(th->rootLocs); i++) {
    if (th->rootLocs[i] != NULL) {
      ptr_t rootVal = *th->rootLocs[i];
      if (!IsTagData(rootVal) && !IsGlobalData(rootVal)) 
	SetPush(&proc->work.roots, (ptr_t) th->rootLocs[i]);
    }
  }
  uninstallThreadRoot(th, (ploc_t) &th->thunk);

  if (th->thunk != NULL) 
    return;   /* Thunk not yet started and so no more roots */

  for (i=0; i<stack->cursor; i++) {
    Stacklet_t *stacklet = stack->stacklets[i];
    if (stacklet->state != InactiveCopied) { /* Replica is not up-to-date if stacklet active */
      if (firstActive == stack->cursor)
	firstActive = i;
      stacklet->state = Pending;
      Stacklet_Copy(stacklet);
      uptrace_stacklet(proc, stacklet, replicaStackletOffset);
    }
  }
  assert(firstActive <= stack->cursor);  /* Could equal if collection finished in first segment */
  for (i=firstActive; i<stack->cursor; i++) {
    volatile reg_t* replicaRegs = &stack->stacklets[i]->bottomBaseRegs[replicaStackletOffset == 0 ? 0 : 32];
    int bottomRegstate = downtrace_stacklet(proc, stack->stacklets[i], replicaStackletOffset, th->saveregs);
    addRegRoots(proc, (unsigned long *) replicaRegs, bottomRegstate | (1 << EXNPTR));
    if (i + 1 == stack->cursor)
      addRegRoots(proc, (unsigned long *)(th->saveregs),  bottomRegstate | (1 << EXNPTR));
    /* downcount++;*/
  }
}


/* Cleans up like complete_root_scan but does not actually scan - used for CollectorTransition */
void discard_root_scan(Proc_t *proc, Thread_t *th)
{
  procChangeState(proc, GCStack, 540);
  /* Thread might not be really be live but was pinned to preserve liveness so 
     that snapshot and snapshotRegs can be used */
  Thread_Unpin(th);   /* Might not have been pinned if thread created after start of GC */
  if (th->used == 0)  /* Thread is actually dead now.  Was live only due to pinning */
    return;

  uninstallThreadRoot(th, (ploc_t) &th->thunk);
}

/* ----------------------------------------------------- 
   ---------------- GLOBAL STUFF ----------------------- */
/* Global variables are the only possible roots since they
   may contain heap values.  Before a global variable is initialized,
   it is not known whether is it of a pointer type (due to abstraction).
   Global variables of type float should not be considered.
*/
static Set_t *promotedGlobal;       /* An initialized global is put in this stack after being trapped by
					a write-barrier. */
static Set_t *tenuredGlobal;        /* Accumulates the contents of promotedGlobal
					across all previous calls to minor_global_promote */


void global_root_init(void)
{
  int mi, numGlobals = 0;  
  for (mi=0; mi<link_modulecount; mi++) {
    mem_t start = (mem_t)traceglobalstart[mi];
    mem_t stop = (mem_t)traceglobalend[mi];
    numGlobals += stop - start;
  }
  promotedGlobal = SetCreate(numGlobals);
  tenuredGlobal = SetCreate(numGlobals);
}

void add_global_root(Proc_t *proc, volatile mem_t global)
{
  tag_t tag = global[-1];
  if (tag == TAG_REC_TRACE) {
    ptr_t globalVal = (ptr_t) *global;
    if (!IsGlobalData(globalVal) && !IsTagData(globalVal))
      SetPush(promotedGlobal, (ptr_t) global);
  }
  else if (tag == MIRROR_GLOBAL_PTR_TAG) {
    ptr_t globalVal = (ptr_t) global[primaryGlobalOffset / sizeof(val_t)];
    assert(global[replicaGlobalOffset / sizeof(val_t)] == uninit_val);
    if (!IsGlobalData(globalVal) && !IsTagData(globalVal))
      SetPush(promotedGlobal, (ptr_t) global);
    else
      DupGlobal(global);
  }
  else 
    DIE("add_global_root bad tag");
  proc->segUsage.globalsProcessed++;
}

void minor_global_scan(Proc_t *proc)
{  
  procChangeState(proc, GCGlobal, 600);
  SetCopy(promotedGlobal, &proc->work.globals);
}

/* Transfers items from promotedGlobal to tenuredGlobal */
void minor_global_promote(Proc_t *proc)
{
  proc->segUsage.globalsProcessed += SetLength(promotedGlobal) / 10;
  SetTransfer(promotedGlobal, tenuredGlobal);
}

void major_global_scan(Proc_t *proc)
{
  procChangeState(proc, GCGlobal, 610);
  minor_global_promote(proc);
  assert(SetIsEmpty(promotedGlobal));
  proc->segUsage.globalsProcessed += SetLength(tenuredGlobal) / 10;
  SetCopy(tenuredGlobal, &proc->work.globals);
}

void NullGlobals(int globalOffset)
{
  int mi;
  /* Null out replica global vars */
  for (mi=0; mi<link_modulecount; mi++) {
    mem_t start = (mem_t)traceglobalstart[mi];
    mem_t stop = (mem_t)traceglobalend[mi];
    for ( ; start < stop; start++) {
      mem_t global = (mem_t) (*start);
      global[globalOffset / sizeof(val_t)] = uninit_val;
    }
  }
}
