#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <values.h>

#include "general.h"
#include "tag.h"
#include "queue.h"
#include "stack.h"
#include "thread.h"
#include "hash.h"
#include "stats.h"
#include "memobj.h"
#include "client.h"
#include "forward.h"
#include "global.h"

int GCTableEntryIDFlag = 0;  /* let the user code set it if it thinks it's on */
int save_rate = 70;
int useGenStack = 0;


static mem_t GCTABLE_BEGIN_ADDR = &GCTABLE_BEGIN_VAL;
static mem_t GCTABLE_END_ADDR = &GCTABLE_END_VAL;
static mem_t GLOBALS_BEGIN_ADDR = &GLOBALS_BEGIN_VAL;
static mem_t GLOBALS_END_ADDR = &GLOBALS_END_VAL;
static mem_t TRACE_GLOBALS_BEGIN_ADDR = &TRACE_GLOBALS_BEGIN_VAL;
static mem_t TRACE_GLOBALS_END_ADDR = &TRACE_GLOBALS_END_VAL;

int debugStack = 0;
long MaxStackDepth = 0;
long TotalStackDepth = 0;
long TotalNewStackDepth = 0;
long TotalStackSize  = 0;

extern mem_t start_client_retadd_val;

#define GET_ENTRYSIZE(x)      ( x        & 511)
#define GET_FRAMESIZE(x)      ((x >> 9)  & 511)
#define GET_BYTESTUFFSIZE(x)  ((x >> 18) & 511)
#define GET_QUAD_RA_OFFSET(x) ((x >> 27) & 31)


typedef unsigned int bot;

extern val_t global_exnrec; /* C/asm convention has asm label be the Lvalue in C */


void stub_error()
{
  printf("stub_error: should be a dead ra");
  assert(0);
}

INLINE(LookupStackBot)
bot LookupStackBot(CallinfoCursor_t *cursor, int pos)
{
  unsigned int v = ((int*)(cursor->callinfo->__rawdata))[pos >> 4];
  int whichbot = pos & 15;
  return (v >> (2 * whichbot)) & 3;
}

INLINE(LookupSpecialByte)
int LookupSpecialByte(CallinfoCursor_t *cursor)
{
  return cursor->byteData[cursor->byteOffset++];
}

INLINE(LookupSpecialWordPair)
void LookupSpecialWordPair(CallinfoCursor_t *cursor, int *a, int *b)
{
  *a = cursor->wordData[cursor->wordOffset++];
  *b = cursor->wordData[cursor->wordOffset++];
  fastAssert (&(cursor->wordData[cursor->wordOffset]) <=
	      (int *) cursor->callinfo + cursor->entrySize);
}

HashTable_t *CallinfoHashTable = NULL;
long GCTableSize = 0;
long SMLGlobalSize = 0;
long GlobalTableSize = 0;
long MutableTableSize = 0;

void global_root_init(void);

void stack_init()
{
  struct HashEntry e;
  unsigned int mi = 0, count=0, i,j;

#ifdef GCTABLE_HASENTRYID
  assert(GCTableEntryIDFlag == 1);
#else
  assert(GCTableEntryIDFlag == 0);
#endif

  for (mi=0; mi<module_count; mi++) {
    int *startpos = (int *)(GCTABLE_BEGIN_ADDR[mi]);
    int *endpos = (int *)(GCTABLE_END_ADDR[mi]);
    int *curpos = startpos; 
    if (debugStack) 
      printf("Scanning GC tables of module %d: %d to %d\n", mi, startpos, endpos);
    while (curpos < endpos) {
      int entrySize = GET_ENTRYSIZE(((Callinfo_t *)curpos)->sizes);
      count++;
      assert(entrySize != 0);
      curpos += entrySize;
    }
    GCTableSize += (long)endpos - (long)startpos;
    SMLGlobalSize += (long)(GLOBALS_END_ADDR[mi]) - 
      (long)(GLOBALS_BEGIN_ADDR[mi]);
    MutableTableSize += (long)(TRACE_GLOBALS_END_ADDR[mi]) - 
      (long)(TRACE_GLOBALS_BEGIN_ADDR[mi]);
/*
       GlobalTableSize += (long)(GLOBAL_TABLE_END_ADDR[mi]) - 
	           (long)(GLOBAL_TABLE_BEGIN_ADDR[mi]);
                   
*/
    }
  CallinfoHashTable = CreateHashTable(2*count);
  for (mi=0; mi<module_count; mi++) {
    int *startpos = (int *)(GCTABLE_BEGIN_ADDR[mi]);
    int *endpos = (int *)(GCTABLE_END_ADDR[mi]);
    int *curpos = startpos; 
    while (curpos < endpos) {
      e.key = (unsigned long)(*curpos);
      e.data = (void *)curpos;
      assert(IsText((ptr_t) e.key));
      HashTableInsert(CallinfoHashTable,&e);
      curpos += GET_ENTRYSIZE(((Callinfo_t *)curpos)->sizes);
    }
  }
  /*
  for (mi=0; mi<NUM_STACK_STUB; mi++) {
    e.key = (unsigned long)(GetStackStub(mi));
    e.data = (void *)mi;
    HashTableInsert(CallinfoHashTable,&e);
  }
  */
  global_root_init();
}

#ifdef CACHE_DEBUG
int XXXcount, XXXhit[100], XXXmiss[100];
int XXXmissValCursor = 0, XXXmissVal[64];
#endif CACHE_DEBUG

INLINE(LookupCallinfo)
Callinfo_t *LookupCallinfo(Proc_t *proc, val_t retAdd)
{
  struct HashEntry *e;
#if defined(alpha_osf)
  val_t key = retAdd;
#elif defined(solaris)
  val_t key = retAdd + 8;  /* We add 2 words because 
			      (1) the link value is the address of the calling instruction
			      (2) there is a delay slot. */
#else
    error platform not defined
#endif

   if ((val_t) proc->lastHashKey == key) {
#ifdef CACHE_DEBUG
     XXXhit[XXXcount]++;
#endif
     return proc->lastHashData;
   }
#ifdef CACHE_DEBUG
  XXXmiss[XXXcount]++;
  XXXmissVal[XXXmissValCursor++] = key;
  XXXmissValCursor &= 63;
#endif
  e = HashTableLookup(CallinfoHashTable,(unsigned long) key,0);
  if (e == NULL) {
    fprintf(stderr,"FATAL ERROR: key = %d not found in table during stack trace\n",key);
    assert(0);
  }
  proc->lastHashKey = e->key;
  proc->lastHashData = e->data;
  return (Callinfo_t *) e->data;
}


/* If client is uptrace, then only some fields are initlialized */
INLINE(getCursor)
CallinfoCursor_t *getCursor(Proc_t *proc, Callinfo_t *callinfo)
{
  CallinfoCursor_t *cursor = &proc->lastCallinfoCursor;
  cursor->byteOffset = 0;
  cursor->wordOffset = 0;
  if (cursor->callinfo != callinfo) {
    unsigned int ra = callinfo->regtrace_a;
    unsigned int rb = callinfo->regtrace_b;
    int sizes = callinfo->sizes;
    int offset = GET_QUAD_RA_OFFSET(sizes);
    cursor->callinfo = callinfo;
    cursor->frameSize = GET_FRAMESIZE(sizes);
    cursor->entrySize = GET_ENTRYSIZE(sizes);
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
      int bytedata_bytesize = GET_BYTESTUFFSIZE(cursor->callinfo->sizes) << 2;
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
	if (cursor->slotCount > (sizeof(cursor->slot) / sizeof(int))) {
	  cursor->slotCount = -4;
	  return 0;
	}
      }
    }
    return 1;
  }
  return 0;
}

INLINE(PathProject)
ptr_t PathProject(ptr_t base, int index)
{
  ptr_t res = base;
  int rec_pos1 = GET_SPECIAL_REC_POS(index);
  if (rec_pos1 > 0) {
    int rec_pos2 = GET_SPECIAL_REC_POS2(index);
    res = (ptr_t) res[rec_pos1-1];
    if (rec_pos2 > 0) {
      int rec_pos3 = GET_SPECIAL_REC_POS3(index);
      res = (ptr_t) res[rec_pos2-1];
      if (rec_pos3 > 0) {
	int rec_pos4 = GET_SPECIAL_REC_POS4(index);
	res = (ptr_t) res[rec_pos3-1];
	if (rec_pos4 > 0) 
	  res = (ptr_t) res[rec_pos4-1];
      }
    }
  }
  return res;
}

int should_trace_special(CallinfoCursor_t *cursor, mem_t cur_sp, int regstate,
			 loc_t data_add, int i)
{
  val_t data = *data_add;
  ptr_t res = 0;  /* res is the type of the value */
  int special_type, special_data;
  int shouldTrace = 0;

  LookupSpecialWordPair(cursor, &special_type,&special_data);

  if (IS_SPECIAL_STACK_REC(special_type)) {
    ptr_t base = (ptr_t)(cur_sp[special_data/4]);
    res = PathProject(base, special_type);
  }
  else if (IS_SPECIAL_LABEL_REC(special_type)) {
    ptr_t base = (ptr_t) special_data; 
    res = PathProject(base, special_type);
  }
  else if (IS_SPECIAL_GLOBAL_REC(special_type)) {
    ptr_t global = (ptr_t) special_data;  /* just global address */
    ptr_t base = (ptr_t) global[primaryGlobalOffset / sizeof(val_t)];
    res = PathProject(base, special_type);
  }
  else if (IS_SPECIAL_UNSET(special_type)) {
    printf("Registers/Stack locatiobs of type UNSET is not supported\n");
    assert(0);
  }
  else
    assert(0);
  shouldTrace = (((val_t) res) > 3);   /* Types 0 to 3 represent integral/non-pointer types */
  return shouldTrace;
}


INLINE(should_trace)
int should_trace(unsigned long trace, 
		 CallinfoCursor_t *cursor, mem_t cur_sp, int regstate,
		 val_t *data_add, int i)
{
  val_t data = *data_add;
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

#ifdef CACHE_DEBUG
static void uptrace_stackletXXX(Proc_t *proc, Stacklet_t *stacklet, int stackletOffset)
{
  Stack_t *callinfoStack = stacklet->callinfoStack;
  int count = 0;
  mem_t curSP = stacklet->replicaCursor + stackletOffset / sizeof(val_t), nextSP;
  mem_t curRA = stacklet->replicaRetadd, nextRA;
  mem_t top = stacklet->baseTop + stackletOffset / sizeof(val_t);
  mem_t guardSP = curSP;

  assert(isEmptyStack(callinfoStack));
  while (curSP < top) {
    Callinfo_t *callinfo = LookupCallinfo(proc,(val_t)curRA);
    CallinfoCursor_t *cursor = getCursor(proc, callinfo);
    curRA = (mem_t) curSP[cursor->RAQuadOffset];
    curSP = curSP + cursor->frameSize;
    count++;
    if ((count & 127) == 127) {
      XXXcount++;
    }
    if (count > 240)
      break;
  }
}
#endif

static void uptrace_stacklet(Proc_t *proc, Stacklet_t *stacklet, int stackletOffset)
{
  Stack_t *callinfoStack = stacklet->callinfoStack;
  int count = 0;
  mem_t curSP = stacklet->replicaCursor + stackletOffset / sizeof(val_t), nextSP;
  mem_t curRA = stacklet->replicaRetadd, nextRA;
  mem_t top = stacklet->baseTop + stackletOffset / sizeof(val_t);
  mem_t guardSP = curSP;

  assert(isEmptyStack(callinfoStack));
  while (curSP < top) {
    Callinfo_t *callinfo = LookupCallinfo(proc,(val_t)curRA);
    CallinfoCursor_t *cursor = getCursor(proc, callinfo);
    pushStack(callinfoStack, (ptr_t) callinfo);
    if (debugStack) {
      mem_t nextRA = (mem_t)curSP[cursor->RAQuadOffset];
      mem_t nextSP = curSP + cursor->frameSize;
      printf("%d: top = %d   curSP = %d  curRA = %d     nextSP = %d nextRA = %d\n",
	     count++,top,curSP,curRA,nextSP,nextRA);
    }
    curRA = (mem_t) curSP[cursor->RAQuadOffset];
    curSP = curSP + cursor->frameSize;
    count++;
  }
  proc->segUsage.stackSlotsProcessed += lengthStack(callinfoStack) * 4;
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
				       unsigned long *saveregs)
{
  int i, j, k;
  mem_t cur_sp = stacklet->baseTop + stackletOffset / sizeof(val_t);
  unsigned int regstate = stacklet->topRegstate;
  Stack_t *callinfoStack = stacklet->callinfoStack;

  while (!isEmptyStack(callinfoStack)) {
    Callinfo_t *callinfo = (Callinfo_t *) popStack(callinfoStack);
    CallinfoCursor_t *cursor = getCursor(proc, callinfo);
    int numChunks = (cursor->frameSize+15)>>4; /* A chunk is the number of slots for one word of info */

    cur_sp -= cursor->frameSize;
    proc->segUsage.stackSlotsProcessed += cursor->frameSize;
    if (debugStack) {
      printf("==========================================================\n");
      printf("SP = %d   RA = %d   FrameSize = %d words\n", cur_sp,cursor->callinfo->retadd,cursor->frameSize);
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
	  pushStack(proc->rootLocs, (ptr_t) &cur_sp[slot]); 
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
	      pushStack(proc->rootLocs, (ptr_t) &cur_sp[curSlot]);
	    }
	  }
	}
      }
    }

    /* Compute the state of the registers */
    {
      unsigned int tempRegstate = cursor->yesBits | (cursor->calleeMask & regstate);
      unsigned int specialBits = cursor->specialBits;
      if (specialBits != 0)
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
      regstate = tempRegstate;
    }
  }
  stacklet->bottomRegstate = regstate;
  return regstate;
}


static void addRegRoots(Proc_t *proc, unsigned long *saveregs, unsigned int regMask)
{
  int i;
  for (i=0; i<32; i++)
    if (regMask & (1 << i)) {
      ploc_t rootLoc = (ploc_t) &saveregs[i];
      ptr_t rootVal = *rootLoc;
      if (!(IsTagData(rootVal)) && !(IsGlobalData(rootVal))) 
	pushStack(proc->rootLocs, (ptr_t) rootLoc);
    }
}

void thread_root_scan(Proc_t *proc, Thread_t *th)
{
  unsigned long *saveregs = (unsigned long *)(th->saveregs);
  int i, numFrames = 0, numWords = 0;
  unsigned int regMask = 0;
  StackChain_t *stack = th->stack;
  ptr_t thunk = th->thunk;

  if (collectDiag >= 2)
    printf("Proc %d: GC %d: thread_root_scan on thread %d with thunk %d\n", proc->procid, NumGC, th->tid, thunk);

  if (thunk != NULL) {
    if (!IsTagData(thunk) && !IsGlobalData(thunk)) 
      pushStack(proc->rootLocs, (ptr_t) &(th->thunk));
    return;   /* Thunk not yet started and so no more roots */
  }

  for (i=stack->cursor-1; i>=0; i--) {
    Stacklet_t *stacklet = stack->stacklets[i];
    stacklet->state = Pending;
    Stacklet_Copy(stacklet);
    uptrace_stacklet(proc, stacklet, primaryStackletOffset);
    numFrames += lengthStack(stacklet->callinfoStack);
    numWords += stacklet->baseTop - stacklet->baseCursor;
  }
  TotalStackDepth += numFrames;
  MaxStackDepth = (numFrames < MaxStackDepth) ? MaxStackDepth : numFrames;
  TotalStackSize += numWords * sizeof(val_t);

  assert(stack->stacklets[0]->topRegstate == 0);
  for (i=0; i<stack->cursor; i++) {
    Stacklet_t *stacklet = stack->stacklets[i];
    unsigned int nextRegstate = downtrace_stacklet(proc, stacklet, primaryStackletOffset, saveregs);
    if (i+1 < stack->cursor)
      stack->stacklets[i+1]->topRegstate = nextRegstate;
  }
  regMask = (CurrentStacklet(stack)->bottomRegstate) | (1 << EXNPTR);
  addRegRoots(proc, (unsigned long *)(th->saveregs), regMask);
}

Thread_t *initial_root_scan(Proc_t *proc, Thread_t *th)
{
  int i, numFrames = 0, numWords = 0;
  StackChain_t *stack = th->stack;
  ptr_t thunk = th->thunk;

  Thread_Pin(th);
    
  if (thunk != NULL) {
    if (!IsTagData(thunk) && !IsGlobalData(thunk)) {
      th->snapshotThunk = thunk;
      pushStack(proc->rootLocs, (ptr_t) &th->snapshotThunk);
    }
    return NULL;   /* Thunk not yet started and so no more roots */
  }
  assert(stack->cursor > 0);
  assert(th->snapshot == NULL);
  th->snapshot = StackChain_Copy(stack);          /* New stack chain */
  for (i=0; i<stack->cursor; i++) 
    th->snapshot->stacklets[i]->state = Pending;  /* Snapshot copying delayed */

  for (i=0; i<32; i++)
    th->snapshotRegs[i] = th->saveregs[i];  

  assert(th->snapshot->cursor > 0);
  return th;
}

/* This will fill up rootLocs */
int work_root_scan(Proc_t *proc, Thread_t *th, int workToDo)
{
  int i, done, uptraceDone = 1;
  StackChain_t *snapshot = th->snapshot;

  assert(!useGenStack);
  assert(snapshot->cursor>0);

  while (snapshot->cursor>0) {
    Stacklet_t *stacklet = snapshot->stacklets[0];  /* Get the oldest stacklet */
    int numWords, numFrames;
    unsigned int nextRegstate;
    /* Copy stacklet first */
    if (updateWorkDone(proc) >= workToDo) 
      break;
    if (stacklet->state != ActiveCopied && stacklet->state != ActiveCopied) {
      int copied = Stacklet_Copy(stacklet);  /* copy stacklet from primary to replica area */
      if (copied)
	proc->segUsage.stackSlotsProcessed += (stacklet->baseTop - stacklet->baseCursor) / 40;
    }
    /* Scan from bottom to get frame sizes and descriptors */
    if (updateWorkDone(proc) >= workToDo) 
      break;
    if (isEmptyStack(stacklet->callinfoStack)) {
      uptrace_stacklet(proc, stacklet, replicaStackletOffset);
      numFrames += lengthStack(stacklet->callinfoStack);
      numWords += stacklet->baseTop - stacklet->baseCursor;
      TotalStackDepth += numFrames;
      TotalStackSize += numWords * sizeof(val_t);
      /*  MaxStackDepth = (numFrames < MaxStackDepth) ? MaxStackDepth : numFrames; */
    }
    /* Trace from top to bottom for roots */
    if (updateWorkDone(proc) >= workToDo) 
      break;
    nextRegstate = downtrace_stacklet(proc, stacklet, replicaStackletOffset, th->snapshotRegs);
    /* Propagate regstate to next stacklet if it exists */
    if (snapshot->cursor > 1)
      snapshot->stacklets[1]->topRegstate = nextRegstate;
    /* Otherwise, bottom stacklet includes registers */
    else {
      int regMask = nextRegstate | (1 << EXNPTR);
      assert(snapshot->cursor == 1);
      addRegRoots(proc, (unsigned long *)(th->snapshotRegs), regMask);
    }
    DequeueStacklet(snapshot);   /* Remove oldest stacklet */
  }
  return (snapshot->cursor == 0);
}

int stkSize = 0;

/* Will fill up rootsLocs */
void complete_root_scan(Proc_t *proc, Thread_t *th)
{
  int i, regMask;
  StackChain_t *stack= th->stack;
  int firstActive = stack->cursor;

#ifdef CACHE_DEBUG
  resetTimeList(); 
  resetPerfMon();
  addTimeList(proc, 0, 0);
  XXXcount = 0;
  XXXmissValCursor = 0;
  for (i=0; i<100; i++)
    XXXhit[i] = XXXmiss[i] = 0;
#endif

  /* Thread might not be really be live but was pinned to preserve liveness so 
     that snapshot, snapshotThunk, and snapshotRegs can be used */
  Thread_Unpin(th);   /* Might not have been pinned if thread created after start of GC */
  if (th->used == 0)  /* Thread is actually dead now.  Was live only due to pinning */
    return;

  if (th->snapshot != NULL) {
    StackChain_Dealloc(th->snapshot);
    th->snapshot = NULL;
  }
  if (th->thunk != NULL) {
    if (!IsTagData(th->thunk) && !IsGlobalData(th->thunk)) 
      pushStack(proc->rootLocs, (ptr_t) &(th->thunk));
    return;   /* Thunk not yet started and so no more roots */
  }

  for (i=0; i<stack->cursor; i++) {
    Stacklet_t *stacklet = stack->stacklets[i];
    if (stacklet->state != InactiveCopied) { /* Replica is not up-to-date if stacklet active */
      if (firstActive == stack->cursor)
	firstActive = i;
      stacklet->state = Pending;
#ifdef CACHE_DEBUG
      addTimeList(proc, 1, 0);
#endif
      Stacklet_Copy(stacklet);
#ifdef CACHE_DEBUG
      addTimeList(proc, 2, 0);
      lapPerfMon();
      /* Uptrace can be done independently of other stacklets */
      uptrace_stackletXXX(proc, stacklet, replicaStackletOffset);
      resetStack(stacklet->callinfoStack);
      XXXcount++;
      lapPerfMon();
      addTimeList(proc, 3, 0);
#endif
      uptrace_stacklet(proc, stacklet, replicaStackletOffset);
#ifdef CACHE_DEBUG
      lapPerfMon();
      addTimeList(proc, 4, 0);
#endif
    }
  }
  assert(firstActive <= stack->cursor);  /* Could equal if collection finished in first segment */

  for (i=firstActive; i<stack->cursor; i++) {
#ifdef CACHE_DEBUG
    addTimeList(proc, 5, 0);
#endif
    downtrace_stacklet(proc, stack->stacklets[i], replicaStackletOffset, th->saveregs);
#ifdef CACHE_DEBUG
    lapPerfMon();
    addTimeList(proc, 6, 0);
#endif
  }
  regMask = (CurrentStacklet(stack)->bottomRegstate) | (1 << EXNPTR);
  addRegRoots(proc, (unsigned long *)(th->saveregs), regMask);

#ifdef CACHE_DEBUG
  addTimeList(proc, 7, 0);
  if (showTimeList(3.0)) {
    printf("\n");
    for (i=0; i<=XXXcount; i++)
      printf("hit[%d] = %d   miss[%d] = %d\n", i, XXXhit[i], i, XXXmiss[i]);
    for (i=0; i<=64; i++)
      printf("missVal[%d] = %d\n", i, XXXmissVal[i]);
    printf("\n");
    showPerfMon(0);
  }
#endif
}

/* ----------------------------------------------------- 
   ---------------- GLOBAL STUFF ----------------------- */
/* Global variables are the only possible roots since they
   may contain heap values.  Before a global variable is initialized,
   it is not known whether is it of a pointer type (due to abstraction).
   Global variables of type float should not be considered.
*/
static Stack_t promotedGlobal;       /* An initialized global is put in this stack after being trapped by
					a write-barrier. */
static Stack_t tenuredGlobal;        /* Accumulates the contents of promotedGlobal
					across all previous calls to minor_global_promote */


void global_root_init(void)
{
  int mi, numGlobals = 0;  
  for (mi=0; mi<module_count; mi++) {
    mem_t start = (mem_t)((&TRACE_GLOBALS_BEGIN_VAL)[mi]);
    mem_t stop = (mem_t)((&TRACE_GLOBALS_END_VAL)[mi]);
    numGlobals += stop - start;
  }
  allocStack(&promotedGlobal, numGlobals);
  allocStack(&tenuredGlobal, numGlobals);
}

void add_global_root(Proc_t *proc, mem_t global)
{
  tag_t tag = global[-1];
  if (tag == TAG_REC_INT) 
    assert(0);
  else if (tag == TAG_REC_TRACE) {
    ptr_t globalVal = (ptr_t) *global;
    if (!IsGlobalData(globalVal) && !IsTagData(globalVal))
      pushStack(&promotedGlobal, global);
  }
  else if (tag == MIRROR_GLOBAL_PTR_TAG) {
    ptr_t globalVal = (ptr_t) global[primaryGlobalOffset / sizeof(val_t)];
    assert(global[replicaGlobalOffset / sizeof(val_t)] == uninit_val);
    if (!IsGlobalData(globalVal) && !IsTagData(globalVal))
      pushStack(&promotedGlobal, global);
    else
      DupGlobal(global);
  }
  else 
    assert(0);
  proc->segUsage.globalsProcessed++;
}

void minor_global_scan(Proc_t *proc)
{  
  copyStack(&promotedGlobal, proc->globalLocs);
}

/* Transfers items from promotedGlobal to tenuredGlobal */
void minor_global_promote(Proc_t *proc)
{
  proc->segUsage.globalsProcessed += lengthStack(&promotedGlobal) / 10;
  transferStack(&promotedGlobal, &tenuredGlobal);
}

void major_global_scan(Proc_t *proc)
{
  minor_global_promote(proc);
  assert(isEmptyStack(&promotedGlobal));
  proc->segUsage.globalsProcessed += lengthStack(&tenuredGlobal) / 10;
  copyStack(&tenuredGlobal, proc->globalLocs);
}

void NullGlobals(int globalOffset)
{
  int mi;
  /* Null out replica global vars */
  for (mi=0; mi<module_count; mi++) {
    mem_t start = (mem_t)((&TRACE_GLOBALS_BEGIN_VAL)[mi]);
    mem_t stop = (mem_t)((&TRACE_GLOBALS_END_VAL)[mi]);
    for ( ; start < stop; start++) {
      mem_t global = (mem_t) (*start);
      global[globalOffset / sizeof(val_t)] = uninit_val;
    }
  }
}

val_t GetGlobal(ptr_t globalLoc)
{
  return globalLoc[primaryGlobalOffset / sizeof(val_t)];
}
