#include "general.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <string.h>

#include "tag.h"
#include "queue.h"
#include "gc.h"
#include "memobj.h"
#include "thread.h"
#include "show.h"
#ifdef alpha_osf
#include <c_asm.h>
#endif

#include "global.h"
#include "stack.h"
#include "bitmap.h"
#include "stats.h"
#include "gcstat.h"
#include "general.h"
#include "forward.h"

int doCopyCopySync = 1;

/* Using switch statements to dispatch on the "type" of objects causes gcc to "optimize"
   the code into a jump table.  The jump table turns out to be slower than a sequence of if-then-else 
   statements because 
   (1) Some cases are more common and we can order the if-then-else
   (2) Some cases can be collased together so there are not that many tests.
   (3) The jump defeats instruction prefetching.
*/

int IsNullCopyRange(CopyRange_t *copyRange)
{
  return (copyRange->heap == NULL);
}

void SetCopyRange(CopyRange_t *copyRange, Proc_t *proc, Heap_t *heap, expand_t *expand, discharge_t *discharge, 
		  Stack_t *rStack, int initialSize)
{
  copyRange->proc = proc;
  copyRange->start = copyRange->cursor = copyRange->stop = 0;
  copyRange->heap = heap;
  copyRange->expand = expand;
  copyRange->discharge = discharge;
  copyRange->regionStack = rStack;
  if (initialSize)
    (*copyRange->expand)(copyRange,initialSize);
}

void ClearCopyRange(CopyRange_t *copyRange)
{
  assert(copyRange->discharge != NULL);
  copyRange->discharge(copyRange);
  copyRange->start = copyRange->cursor = copyRange->stop = 0;
  copyRange->heap = NULL;
  copyRange->expand = NULL;
  copyRange->discharge = NULL;
  copyRange->regionStack = NULL;
}

/* Since doubleOddAlign and noCheck are statically known, the compiler can greatly simplify the function body */
INLINE(allocFromCopyRange)
mem_t allocFromCopyRange(CopyRange_t *copyRange, int byteLen, Align_t align, int noCheck)
{
  mem_t oldCursor = copyRange->cursor;
  int paddedByteLen = (align == NoWordAlign) ? byteLen : 4 + byteLen;
  mem_t newCursor = oldCursor + (paddedByteLen / sizeof(val_t));
  if (!noCheck && newCursor >= copyRange->stop) {
    assert(*copyRange->expand != NULL);
    (*copyRange->expand)(copyRange, paddedByteLen);
    oldCursor = copyRange->cursor;
    newCursor = oldCursor + (paddedByteLen / sizeof(val_t));
    if (oldCursor == NULL) {
      printf("Collector failed to obtain %d bytes for copying.  Perhaps collection fell behind.\n", paddedByteLen);
      assert(0);
    }
    assert(newCursor < copyRange->stop);
  }
  AlignMemoryPointer(&oldCursor, align);
  newCursor = oldCursor + (byteLen / sizeof(val_t));
  copyRange->cursor = newCursor;
#ifdef sparc
  {
    int discard;
    int next = ((int) oldCursor) + 64; /* Fetch next block */
    asm("prefetch [%1], 2" : "=r" (discard) : "r" (next));
  }
#endif
  return oldCursor;
}

/* If size is zero, then area is discharged without obtaining a new area. */
void expandCopyRange(CopyRange_t *copyRange, int size)
{
  int saveRegion = 0;
  int roundSize = RoundUp(size, pagesize);
  mem_t oldStart = copyRange->start, oldCursor = copyRange->cursor, oldStop = copyRange->stop;
  assert(copyRange->start <= copyRange->cursor);
  assert(copyRange->cursor <= copyRange->stop);
  PadHeapArea(copyRange->cursor, copyRange->stop);
  if (copyRange->start < copyRange->cursor && copyRange->regionStack != NULL) {
    saveRegion = 1;
    pushStack2(copyRange->regionStack, copyRange->start, copyRange->cursor);
  }
  if (roundSize)
    GetHeapArea(copyRange->heap, roundSize, &copyRange->start, &copyRange->cursor, &copyRange->stop);
  else 
    copyRange->start = copyRange->cursor = copyRange->stop = 0;
  if (diag)
    printf("Proc %d: expand (%d - %d) to (%d - %d)   %s\n", 
	   copyRange->proc->procid, 
	   oldStart, oldStop,
	   copyRange->start, copyRange->stop,
	   saveRegion ? "SAVING" : "");
}

void dischargeCopyRange(CopyRange_t *copyRange)
{
  expandCopyRange(copyRange, 0);
}

void PadCopyRange(CopyRange_t *copyRange)
{
  PadHeapArea(copyRange->cursor, copyRange->stop);
}



INLINE(getTag)
tag_t getTag(ptr_t obj)
{
  tag_t tag = (tag_t) obj[-1];

  while (TAG_IS_FORWARD(tag)) {
    ptr_t replica = (ptr_t) tag;
    fastAssert(replica != obj);
    tag = (tag_t) replica[-1];
  }
  return tag;
}

/* Returns object length including tag word in bytes */
unsigned long objectLength(ptr_t obj, mem_t *start)
{
  tag_t tag = getTag(obj);
  int type = GET_TYPE(tag);

  switch (type) {
    case RECORD_TYPE: {
      int numFields = GET_RECLEN(tag);
      assert (numFields != 0);  /* There should be no empty records */
      *start = obj - 1;
      return 4 * (1 + numFields);
    }
    case WORD_ARRAY_TYPE: 
    case QUAD_ARRAY_TYPE: 
    case PTR_ARRAY_TYPE:  
    case MIRROR_PTR_ARRAY_TYPE: { 
      int byteLen = GET_ANY_ARRAY_LEN(tag); 
      int dataWordLen = RoundUp(byteLen, 4);
      int numTags = 1 + (arraySegmentSize ? (byteLen > arraySegmentSize ? DivideUp(byteLen,arraySegmentSize) : 0) : 0);
      if (type != WORD_ARRAY_TYPE)
	assert(byteLen % 4 == 0);
      *start = obj - numTags;
      return 4 * numTags + dataWordLen;
    }
    case OTHER_TYPE:
       if (IS_SKIP_TAG(tag)) {
	 *start = obj;
	 return 4 * (GET_SKIPWORD(tag));
       }
    /* Fall-through */
    case FORWARD1_TYPE:
    case FORWARD2_TYPE: {
      mem_t tagstart = (mem_t) (obj - 1);
      printf("bad tag %d at %d\n",tag,tagstart);
      memdump("",tagstart-10,30,tagstart);
      printf("\n\n\n");
      printf("NumGC is %d\n",NumGC);
      assert(0);
    }
  } /* case */
}


/* -------------------------------------------------------------------------- */
/* Tag is the the tag value when the caller checked.  It may have changed */
INLINE(acquireOwnership)
tag_t acquireOwnership(Proc_t *proc, ptr_t white, tag_t tag, CopyRange_t *copyRange)
{

#ifdef alpha_osf
  int done = 0;
  while (!done) {
    /*    asm("ldl_l %0,-4(%1)" : "=i" (tag) : "i" (white)); */
    tag = asm("ldl_l %v0,-4(%a0)",white); 
    if (tag == STALL_TAG)
      done = 1;
    else 
      done |= asm("stl_c %a0,-4(%a1) ; mov %a0,%v0",STALL_TAG,white);
  }
  while (tag == STALL_TAG)
    tag = white[-1];
  return tag;
#endif

#ifdef sparc
  mem_t tagloc = white - 1;
  if (tag == STALL_TAG) {         /* Somebody grabbed it but did not finish forwarding */
    while (tag == STALL_TAG) {
      tag = white[-1];
      flushStore();               /* Might need to refetch from memory */
    }
    assert(TAG_IS_FORWARD(tag));  /* Object forwarded by someone else now */
  }
  else {                          /* Try to be the copier */
    /* Example of a SPARC ld statement with gcc asm
       int *ptr;
       int val;
       asm("ld   [%1],%0" : "=r" (val) : "r" (ptr)); 
       
       The following tries to atomicaly swap in the stall tag by comparing with original tag.
       Note that registers that are input and output are specified twice with the input
       use referring to the output register.
    */
    val_t localStall = STALL_TAG;
    asm("cas [%2],%3,%0" : "=r" (localStall) : "0" (localStall), "r" (tagloc), "r" (tag)); 
    /* localStall == tag           : we are the copier
       localStall == STALL_TAG     : somebody else is the copier and was in the middle of its operation
       localStall == a forward ptr : somebody else is the copier and forwarded it already */
    if (localStall == tag)
      ;                             
    else if (localStall == STALL_TAG) {
      proc->numContention++;
      if (diag) 
	printf("Proc %d: contention copying object white = %d\n", copyRange->proc->procid, white);
      while ((tag = white[-1]) == STALL_TAG)
	flushStore();
      assert(TAG_IS_FORWARD(tag));
    }
    else if (TAG_IS_FORWARD(localStall))
      tag = localStall;
    else {
      printf("Proc %d: forward.c: Odd tag of %d from white obj %d with original tag = %d -----------\n", 
	     copyRange->proc->procid, localStall, white, tag);
      assert(0);
    }
  }
  return tag;
#endif
}



INLINE(genericAlloc)
int genericAlloc(Proc_t *proc, ptr_t white, CopyRange_t *copyRange, int doCopy, int doCopyCopy, int skipSpaceCheck, int splitLarge)
{
  ptr_t obj;                       /* forwarded object */
  tag_t tag = white[-1];           /* original tag */
  int type;

  /* assert(white < copyRange->start || white >= copyRange->stop); */

  /* If the objects has not been forwarded, atomically try commiting to be the copier.
     When we leave the block, we are the copier if "tag" is not a forwarding pointer. */
  if (TAG_IS_FORWARD(tag)) {
    /* ptr_t gray = (ptr_t) tag;
       assert(gray != (ptr_t) gray[-1]); Make sure object is not self-forwarded */
    proc->numShared++;
    return 0;
  }
  else {
    if (doCopyCopy && doCopyCopySync)  { /* We omit copy-copy sync for measuring the costs of the copy-copy sync */
      /*
	volatile int dummy;
      int *addr = (int *)RoundDown((int)white, pagesize);
      startAlternatePerfMon();
      dummy = *addr;
      *addr = dummy; 
      stopAlternatePerfMon();
      */
      tag = acquireOwnership(proc, white, tag, copyRange);
    }
  }

  proc->segUsage.objsCopied++;

  /* The tag must be restored only after the forwarding address is written */
  type = GET_TYPE(tag);
  if (type == RECORD_TYPE) {           /* As usual, the record case is the most common */
    int i, numFields = GET_RECLEN(tag);
    int objByteLen = 4 * (1 + numFields);
    mem_t region = allocFromCopyRange(copyRange, objByteLen, NoWordAlign, skipSpaceCheck);
    obj = region + 1;
    if (doCopy)
      for (i=0; i<numFields; i++)     /* Copy fields */
	obj[i] = white[i];
    obj[-1] = tag;                    /* Write tag last */
    white[-1] = (val_t) obj;        /* Store forwarding pointer last */
    /* Sparc TSO order guarantees forwarding pointer will be visible only after fields are visible */
    proc->numCopied++;
    proc->segUsage.fieldsCopied += numFields;
    return objByteLen;
  }
  else if (TYPE_IS_FORWARD(type)) {
    /* ptr_t gray = (ptr_t) tag;
       assert(gray != (ptr_t) gray[-1]); Make sure object is not self-forwarded */
    proc->numShared++;
    return 0;
  }
  else if (TYPE_IS_ARRAY(type)) {
    int i, arrayByteLen = GET_ANY_ARRAY_LEN(tag);
    int dataByteLen = RoundUp(arrayByteLen, 4);
    int numTags = 1 + (splitLarge ? (arrayByteLen > arraySegmentSize ? DivideUp(arrayByteLen,arraySegmentSize) : 0) : 0);
    int objByteLen = dataByteLen + 4 * numTags;
    Align_t align = (type == QUAD_ARRAY_TYPE) ? ((numTags & 1) ? OddWordAlign : EvenWordAlign) : NoWordAlign;
    mem_t region = allocFromCopyRange(copyRange, objByteLen, align, skipSpaceCheck);
    obj = region + numTags;
    if (type != WORD_ARRAY_TYPE)
      assert(arrayByteLen % 4 == 0);
    if (doCopy)
      memcpy((char *) obj, (const char *)white, objByteLen - 4);
    for (i=0; i<numTags-1; i++)
      obj[-2-i] = SEGPROCEED_TAG;
    obj[-1] = tag;	
    white[-1] = (val_t) obj;
    proc->numCopied++;
    proc->segUsage.fieldsCopied += objByteLen / 2;
    return objByteLen;
  }
  else if (IS_SKIP_TAG(tag)) {
    return 0;
  }
  else 
    printf("\n\ncopy_copyCopySync: BAD TAG: white = %d, tag = %d\n",white,tag);
  assert(0);
}

int copy(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return genericAlloc(proc, white, copyRange, 1, 0, 0, 0);
}

int alloc(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return genericAlloc(proc, white, copyRange, 0, 0, 0, 0);
}

int splitAlloc(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return genericAlloc(proc, white, copyRange, 0, 0, 0, 1);
}

void copy_noSpaceCheck(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  (void) genericAlloc(proc, white, copyRange, 1, 0, 1, 0);
}


int copy_copyCopySync(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return genericAlloc(proc, white, copyRange, 1, 1, 0, 0);
}

int alloc_copyCopySync(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return genericAlloc(proc, white, copyRange, 0, 1, 0, 0);
}

int splitAlloc_copyCopySync(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return genericAlloc(proc, white, copyRange, 0, 1, 0, 1);
}

/* ------------------------------------------------------- */
/* -------------- Exported functions --------------------- */
/* ------------------------------------------------------- */

/* (1) Add global roots.
   (2) If from and to are not NULL, add the locations of all pointer arrays containing back pointers 
*/
void process_writelist(Proc_t *proc, Heap_t *from, Heap_t *to)
{
  ploc_t curLoc = proc->writelistStart;
  ploc_t end = proc->writelistCursor;
  while (curLoc < end) {
    /* Each writelist entry has 3 values */
    ptr_t obj = (ptr_t) (*(curLoc++)), data;
    int byteOffset = (int) (*(curLoc++));  
    ptr_t possPrevPtrVal = (ptr_t) (*(curLoc++));  /* We ignore this value for non-concurrent collectors */
    tag_t tag = getTag(obj);
    int type = GET_TYPE(tag);
    ploc_t field;

    if (IsGlobalData(obj)) {
      add_global_root(proc,obj);
      continue;
    }
    if (from == NULL)
      continue;
    if (type == WORD_ARRAY_TYPE || type == QUAD_ARRAY_TYPE)
      continue;
    else if (type == PTR_ARRAY_TYPE || type == MIRROR_PTR_ARRAY_TYPE)
      ;
    else {
      printf("XXX obj = %d   tag = %d   type = %d\n", obj, tag, type);
      assert(0);
    }
    field  = (ploc_t) (obj + byteOffset / sizeof(val_t));
    data = *field;
    if (NotInRange(data,&to->range)) {
      pushStack(proc->rootLocs, (ptr_t) field);
    }
  }
  proc->writelistCursor = proc->writelistStart;
}


/* --------------- Scanning routines ----------------------- */

mem_t scanTag_locCopy1_noSpaceCheck(Proc_t *proc, mem_t start, CopyRange_t *copyRange, 
				    Heap_t *from)
{
  tag_t tag = start[0];
  int type = GET_TYPE(tag);
  ptr_t gray = start + 1;

  proc->segUsage.objsScanned++;
  if (type == RECORD_TYPE) {
    unsigned mask = GET_RECMASK(tag);
    int i, fieldLen = GET_RECLEN(tag);
    for (i=0; i<fieldLen; i++, mask >>= 1) 
      if (mask & 1) 
	locCopy1_noSpaceCheck(proc,(ploc_t)gray + i,copyRange,from);
    proc->segUsage.fieldsScanned += fieldLen;
    return gray + fieldLen;
  }
  else if (type == WORD_ARRAY_TYPE || type == QUAD_ARRAY_TYPE) {
    unsigned int fieldLen = (GET_ANY_ARRAY_LEN(tag) + 3) / 4; /* WORD_ARRAY len might not be mult of 4 */
    proc->segUsage.fieldsScanned += fieldLen;
    return gray + fieldLen;
  }
  else if (type == PTR_ARRAY_TYPE) {
    int i, fieldLen = GET_ANY_ARRAY_LEN(tag) >> 2;
    for (i=0; i<fieldLen; i++) 
      locCopy1_noSpaceCheck(proc,(ploc_t)gray + i,copyRange,from);
    proc->segUsage.ptrFieldsScanned += fieldLen;
    return gray + fieldLen;
  }
  else if (type == MIRROR_PTR_ARRAY_TYPE) {
    int i, fieldLen = GET_ANY_ARRAY_LEN(tag) >> 3;
    for (i=0; i<fieldLen; i++) {
      locCopy1_noSpaceCheck(proc,(ploc_t)gray + 2 * i + (primaryArrayOffset/sizeof(val_t)), copyRange, from);
      gray[2 * i + (replicaArrayOffset/sizeof(val_t))] = gray[2 * i + (primaryArrayOffset/sizeof(val_t))];
    }
    proc->segUsage.ptrFieldsScanned += 2 * fieldLen;
    return gray + 2 * fieldLen;
  }
  else if (IS_SKIP_TAG(tag))
    return start + (GET_SKIPWORD(tag));
  else {
    printf("\n\nscanTag_locCopy1_noSpaceCheck: found bad tag = %d at start %d\n",tag,start);
    assert(0);
  }
  assert(0);
}

mem_t scanTag_locCopy2L_noSpaceCheck(Proc_t *proc, mem_t start, CopyRange_t *copyRange,
				     Heap_t *from, Heap_t *from2, Heap_t *large)
{
  tag_t tag = start[0];
  int type = GET_TYPE(tag);
  ptr_t gray = start + 1;

  proc->segUsage.objsScanned++;
  if (type == RECORD_TYPE) {
    unsigned mask = GET_RECMASK(tag);
    int i, fieldLen = GET_RECLEN(tag);
    for (i=0; i<fieldLen; i++, mask >>= 1)
      if (mask & 1) 
	locCopy2L_noSpaceCheck(proc,(ploc_t)(gray+i),copyRange,from,from2,large);
    proc->segUsage.fieldsScanned += fieldLen;
    return gray + fieldLen;
  }
  else if (type == WORD_ARRAY_TYPE || type == QUAD_ARRAY_TYPE) {
    unsigned int fieldLen = (GET_ANY_ARRAY_LEN(tag) + 3) / 4; /* IARRAY len might not be mult of 4 */
    proc->segUsage.fieldsScanned += fieldLen;
    return gray + fieldLen;
  }
  else if (type == PTR_ARRAY_TYPE) {
    int i, fieldLen = GET_ANY_ARRAY_LEN(tag) >> 2;
    for (i=0; i<fieldLen; i++)
      locCopy2L_noSpaceCheck(proc,(ploc_t)gray + i,copyRange,from,from2,large);
    proc->segUsage.ptrFieldsScanned += fieldLen;
    return gray + fieldLen;
  }
  else if (type == MIRROR_PTR_ARRAY_TYPE) {
    int i, fieldLen = GET_ANY_ARRAY_LEN(tag) >> 3;
    for (i=0; i<fieldLen; i++) {
      locCopy2L_noSpaceCheck(proc,(ploc_t)gray + 2*i + (primaryArrayOffset/sizeof(val_t)), copyRange,from,from2,large);
      gray[2 * i + (replicaArrayOffset/sizeof(val_t))] = gray[2 * i + (primaryArrayOffset/sizeof(val_t))];
    }
    proc->segUsage.ptrFieldsScanned += fieldLen;
    return gray + fieldLen;
  }
  else if (IS_SKIP_TAG(tag))
    return start + GET_SKIPWORD(tag);
  else {
    printf("\n\nscanTag_locCopy2L_noSpaceCheck: bad tag = %d at start=%d\n",tag,start);
    assert(0);
  }
  assert(0);
}

void scanObj_locCopy1_noSpaceCheck(Proc_t *proc, ptr_t obj, CopyRange_t *copyRange, Heap_t *from_range)
{
  scanTag_locCopy1_noSpaceCheck(proc, obj - 1, copyRange, from_range);
}

void scanUntil_locCopy1_noSpaceCheck(Proc_t *proc, mem_t start_scan, CopyRange_t *copyRange,
				      Heap_t *from_range)
{
  mem_t cur = start_scan;
  while (cur < copyRange->cursor) {
    cur = scanTag_locCopy1_noSpaceCheck(proc, cur, copyRange, from_range);
  }
  assert(cur == copyRange->cursor);
}

void scanUntil_locCopy2L_noSpaceCheck(Proc_t *proc, mem_t start_scan, CopyRange_t *copyRange,
				      Heap_t *from_range, Heap_t *from2_range, Heap_t *large)
{
  mem_t cur = start_scan;
  while (cur < copyRange->cursor) {
    cur = scanTag_locCopy2L_noSpaceCheck(proc, cur, copyRange, from_range, from2_range, large);
  }
  assert(cur == copyRange->cursor);
}




typedef enum LocAllocCopy__t {Copy, LocCopy, LocAlloc} LocAllocCopy_t;
typedef enum SpaceCheck__t {OneSpace, OneSpaceLarge, TwoSpaceLarge} SpaceCheck_t;
typedef enum Transfer__t {NoTransfer, Transfer, SelfTransfer} Transfer_t;

/* The generic scanning function with compile-time parameters.
   primaryOrReplicaGray - The primary or replica gray object is passed in
   start, end - if start <> end, large object is indicated; in bytes
   splitLarge - if true, then large arrays are split
   Transfer - if NoTransfer, primary is unknown and object is replica
              if Transfer, object was primary and the replica is uninitialized
	      if SelfTransfer, object is primary and replica (MIRROR_PTR)
   localStack - stack for holding primary or replica gray objects
   localSegmentStack - stack for holding primary large object segments
   copyRange - where new objects are allocated rfom
   from_range, from2_range, large_range - determines if an object is primary
   spaceCheck - forward if from given space(s) and/or large space
   doCopyWrite - copy-write synchronization; if true, must also have mayTransfer
*/

INLINE(genericScan)
void genericScan(Proc_t *proc, 
		 ptr_t primaryOrReplicaGray, int start, int end,
		 Stack_t *localStack, Stack_t *localSegmentStack, CopyRange_t *copyRange,
		 Heap_t *from_range, Heap_t *from2_range, Heap_t *large_range,
		 int doCopyWrite, LocAllocCopy_t locAllocCopy, SpaceCheck_t spaceCheck, Transfer_t transfer, int splitLarge)
{
  /* If performing transfer, we are given primaryGray and must copy fields into replicaGray */
  ptr_t primaryGray = (transfer == NoTransfer) ? NULL : 
                      ((transfer == Transfer)  ?  primaryOrReplicaGray 
		                               : primaryOrReplicaGray);
  ptr_t replicaGray = (transfer == NoTransfer) ? primaryOrReplicaGray : 
                      ((transfer == Transfer)  ? (ptr_t) primaryGray[-1] 
		                               : primaryOrReplicaGray);
  tag_t tag = replicaGray[-1];
  int i, discard;

  if (doCopyWrite)
    assert(transfer != NoTransfer);
  proc->segUsage.objsScanned++;
  while (1) {
    int type = GET_TYPE(tag);
    if (type == RECORD_TYPE) {                      /* Records are the most common case - no copy-write sync needed */
      unsigned int mask = GET_RECMASK(tag);
      int fieldlen = GET_RECLEN(tag);
      int ptrFields = 0;
      for (i=0; i<fieldlen; i++, mask >>= 1) {
	if (transfer == Transfer)
	  replicaGray[i] = primaryGray[i];
	if (mask & 1) {
	  ptrFields++;
	  /* Since no copy-write sync is required, objects that point to themselves won't cause looping */
	  switch (locAllocCopy) {
	  case LocAlloc:
	    switch (spaceCheck) {
	      case OneSpace:
		if (splitLarge)
		  locSplitAlloc1_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range);
		else
		  locAlloc1_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range);
		break;
	      case OneSpaceLarge:
		assert(splitLarge == 1);
		locSplitAlloc1L_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range, large_range);
		break;
	      case TwoSpaceLarge:
		assert(splitLarge == 0);
		locAlloc2L_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range,from2_range,large_range);
		break;
	      default : assert(0);
	    }
	    break;
	  case LocCopy:
	    assert(splitLarge == 0);
	    switch (spaceCheck) {
	      case OneSpace: locCopy1_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range); break;
	      case OneSpaceLarge: locCopy1L_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range,large_range); break;
	      case TwoSpaceLarge: locCopy2L_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range,from2_range,large_range); break;
	      default: assert(0);
	    }
	    break;
	  case Copy:
	    assert(splitLarge == 0);
	    switch (spaceCheck) {
	      case OneSpace: copy1_copyCopySync_primaryStack(proc,(ptr_t) replicaGray[i],localStack,copyRange,from_range); break;
	      default: assert(0);
	    }
	    break;
	  } /* swtich locAllocCopy */
	}
      }
      proc->segUsage.fieldsScanned += fieldlen - ptrFields;
      proc->segUsage.ptrFieldsScanned += ptrFields;
      return;
    } 
    else if (TYPE_IS_ARRAY(type)) {
      int arrayByteLen = GET_ANY_ARRAY_LEN(tag);  
      int byteLen = RoundUp(arrayByteLen, 4);      /* Int array length might not be multiple of 4 */
      int fieldLen = DivideUp(byteLen, 4);         /* In words for all array types */
      int large =  splitLarge && (byteLen > arraySegmentSize);
      int syncIndex = large ? -(2+DivideUp(start,arraySegmentSize)) : -1; /* Use normal tag or segment tag */
      int firstField = 0, lastField = fieldLen;

      if (type != WORD_ARRAY_TYPE)
	assert(arrayByteLen % 4 == 0);

      /* If a large array object, convert into segments */
      if (large && start == end) {
	int segments = DivideUp(byteLen, arraySegmentSize);
	assert(primaryGray != NULL);
	for (i=0; i<segments; i++) {
	  int start = i * arraySegmentSize;
	  int end = start + arraySegmentSize;
	  if (end > byteLen)
	    end = byteLen;
	  pushStack3(localSegmentStack, (ptr_t) primaryGray, (ptr_t) start, (ptr_t) end);
	}
	return;
      }
      
      /* If segmented, fields we work on is derived from start and end */
      if (start != end) {
	firstField = start / sizeof(val_t);
	lastField =  end / sizeof(val_t);
      }

      if (doCopyWrite) 
	primaryGray[syncIndex] = large ? SEGSTALL_TAG : STALL_TAG;
      if (type == WORD_ARRAY_TYPE || type == QUAD_ARRAY_TYPE) {
	if (transfer == Transfer) {
	  memcpy((char *) &(replicaGray[firstField]), (const char *) &(primaryGray[firstField]), sizeof(val_t) * (lastField - firstField)); 
	}
	proc->segUsage.fieldsScanned += lastField - firstField;
      }
      else if (type == PTR_ARRAY_TYPE || type == MIRROR_PTR_ARRAY_TYPE) {
	int isPtrArray = type == PTR_ARRAY_TYPE;
	int primaryOffset = isPtrArray ? 0 : primaryArrayOffset;
	int replicaOffset = isPtrArray ? 0 : replicaArrayOffset;
	if (!isPtrArray) {
	  assert(firstField % 2 == 0);
	  assert(lastField % 2 == 0);
	}
	for (i=firstField; i<lastField; i += (isPtrArray ? 1 : 2)) {
	  ploc_t primaryField = (ploc_t) &primaryGray[i + primaryOffset / sizeof(val_t)];
	  ploc_t replicaField = (ploc_t) &replicaGray[i + replicaOffset / sizeof(val_t)];
	  if ((transfer == Transfer) ||
	      (transfer == SelfTransfer && (!isPtrArray))) {
	    *replicaField = *primaryField;
	  }
	  if (doCopyWrite && (ptr_t) *primaryField == primaryGray) { /* Check is needed for objects that point to themselves - otherwise, loop */
	    *replicaField = (ptr_t) replicaGray;
	    continue;
	  }
	  switch (locAllocCopy) {
	    case LocAlloc:
	      switch (spaceCheck) {
		case OneSpace:
		  if (splitLarge)
		    locSplitAlloc1_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range);
		  else 
		    locAlloc1_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range);
		  break;
	        case OneSpaceLarge:
		  assert(splitLarge == 1);
		  locSplitAlloc1L_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range,large_range);
		  break;
	        case TwoSpaceLarge:
		  assert(splitLarge == 0);
		  locAlloc2L_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range,from2_range,large_range);
		  break;
	        default: assert(0);
	      }
	      break;
	    case LocCopy:
	      if (splitLarge)  {
		switch (spaceCheck) {
		case OneSpace: locSplitCopy1_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range); break; 
		case OneSpaceLarge: assert(0);
		case TwoSpaceLarge: assert(0);
		default : assert(0);
		}
	      }
	      else {
		switch (spaceCheck) {
		case OneSpace: locCopy1_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range); break;
		case OneSpaceLarge: locCopy1L_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range,large_range); break;
		case TwoSpaceLarge: locCopy2L_copyCopySync_primaryStack(proc,replicaField,localStack,copyRange,from_range,from2_range,large_range); break;
		default : assert(0);
		}
	      }
	      break;
	    case Copy:
	      assert(splitLarge == 0);
	      switch (spaceCheck) {
	        case OneSpace: copy1_copyCopySync_primaryStack(proc,(ptr_t) replicaGray[i],localStack,copyRange,from_range); break;
	        default: assert(0);
	      }
	      break;
	  } /* switch locAllocCopy */
	  /* Both fields must be up-to-date */
	  if ((transfer == Transfer) && (!isPtrArray)) 
	    replicaGray[i + primaryOffset / sizeof(val_t)] = (val_t) (*replicaField);
	} /* for */
	proc->segUsage.ptrFieldsScanned += lastField - firstField;
      }  /* PTR_ARRAY or MIRROR_PTR_ARRAY */
      else 
	assert(0);
      if (doCopyWrite) 
	primaryGray[syncIndex] = large ? SEGPROCEED_TAG : (val_t) replicaGray;
      if (lastField - firstField > 32) 
	updateWorkDone(proc);
      return;
      }
    else if (TYPE_IS_FORWARD(type)) {
      tag = ((ptr_t)tag)[-1];
      continue;
    }
    else if (IS_SKIP_TAG(tag)) {
      proc->segUsage.fieldsScanned += 1;
      return;
    }
    else {
      while (tag == STALL_TAG) {
	flushStore();
	tag = replicaGray[-1];
      }
      continue;
    }
  }
  assert(0);
}

void scanObj_copy1_copyCopySync_primaryStack(Proc_t *proc, ptr_t replicaGray, Stack_t *localStack, CopyRange_t *copyRange,
					     Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, localStack, NULL,  copyRange,
	      from_range, NULL, NULL,
	      0, Copy, OneSpace, NoTransfer, 0);
}

void scanObj_locCopy1_copyCopySync_primaryStack(Proc_t *proc, ptr_t replicaGray, Stack_t *localStack, CopyRange_t *copyRange,
					     Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, localStack, NULL,  copyRange,
	      from_range, NULL, NULL,
	      0, LocCopy, OneSpace, NoTransfer, 0);
}

void scanObj_locCopy1L_copyCopySync_primaryStack(Proc_t *proc, ptr_t replicaGray, Stack_t *localStack, CopyRange_t *copyRange,
						 Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, replicaGray, 0, 0, localStack, NULL,  copyRange,
	      from_range, NULL, large_range,
	      0, LocCopy, OneSpaceLarge, NoTransfer, 0);
}

void transferScanObj_locCopy1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, Stack_t *localStack, CopyRange_t *copyRange,
							Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, localStack, NULL,  copyRange,
	      from_range, NULL, NULL,
	      0, LocCopy, OneSpace, Transfer, 0);
}

void selfTransferScanObj_locAlloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, 
								 Stack_t *localStack, Stack_t *segmentStack, CopyRange_t *copyRange,
							Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, localStack, segmentStack,  copyRange,
	      from_range, NULL, NULL,
	      0, LocAlloc, OneSpace, SelfTransfer, 0);
}

void selfTransferScanObj_locSplitAlloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, 
								 Stack_t *localStack, Stack_t *segmentStack, CopyRange_t *copyRange,
							Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, localStack, segmentStack,  copyRange,
	      from_range, NULL, NULL,
	      0, LocAlloc, OneSpace, SelfTransfer, 1);
}

void transferScanObj_locCopy2L_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, Stack_t *localStack, CopyRange_t *copyRange,
							Heap_t *from_range, Heap_t *from2_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, 0, 0, localStack, NULL,  copyRange,
	      from_range, from2_range, large_range,
	      0, LocCopy, TwoSpaceLarge, Transfer, 0);
}


void transferScanObj_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, 
								       Stack_t *objStack, Stack_t *segmentStack, CopyRange_t *copyRange,
								       Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, objStack, segmentStack,  copyRange,
	      from_range, NULL, NULL,
	      1, LocAlloc, OneSpace, Transfer, 1);
}




void transferScanObj_copyWriteSync_locSplitAlloc1L_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, 
								       Stack_t *objStack, Stack_t *segmentStack, CopyRange_t *copyRange,
								       Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, 0, 0, objStack, segmentStack, copyRange,
	      from_range, NULL, large_range,
	      1, LocAlloc, OneSpaceLarge, Transfer, 1);
}

void transferScanSegment_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, int start, int end,
									   Stack_t *objStack, Stack_t *segmentStack, CopyRange_t *copyRange,
									   Heap_t *from_range)
{
  genericScan(proc, primaryGray, start, end, objStack, segmentStack,  copyRange,
	      from_range, NULL, NULL,
	      1, LocAlloc, OneSpace, Transfer, 1);
}


void selfTransferScanSegment_copyWriteSync_locSplitAlloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, int start, int end,
									   Stack_t *objStack, Stack_t *segmentStack, CopyRange_t *copyRange,
									   Heap_t *from_range)
{
  genericScan(proc, primaryGray, start, end, objStack, segmentStack,  copyRange,
	      from_range, NULL, NULL,
	      1, LocAlloc, OneSpace, SelfTransfer, 1);
}





void transferScanSegment_copyWriteSync_locSplitAlloc1L_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, int start, int end,
										 Stack_t *objStack, Stack_t *segmentStack, CopyRange_t *copyRange,
										 Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, start, end, objStack, segmentStack,  copyRange,
	      from_range, NULL, large_range,
	      1, LocAlloc, OneSpace, Transfer, 1);
}



void selfTransferScanSegment_copyWriteSync_locSplitAlloc1L_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, int start, int end,
										 Stack_t *objStack, Stack_t *segmentStack, CopyRange_t *copyRange,
										 Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, start, end, objStack, segmentStack,  copyRange,
	      from_range, NULL, large_range,
	      1, LocAlloc, OneSpace, SelfTransfer, 1);
}


