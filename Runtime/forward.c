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

/* Since doubleOddAlign and noCheck are staically known, the compiler can greatly simplify the function body */
INLINE1(allocFromCopyRange)
INLINE2(allocFromCopyRange)
mem_t allocFromCopyRange(CopyRange_t *copyRange, int byteLen, int doubleOddAlign, int noCheck)
{
  mem_t oldCursor = copyRange->cursor;
  int paddedByteLen = doubleOddAlign ? byteLen + 4 : byteLen;
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
  if (doubleOddAlign) {
    if ((((int) oldCursor) & 7) == 0) 
      *(oldCursor++) = SKIP_TYPE | (1 << SKIPLEN_OFFSET);
    else 
      newCursor--;
  }
  copyRange->cursor = newCursor;
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


int getNontagNonglobalPointerLocations(ptr_t obj, Stack_t *oStack)
{
  tag_t tag = obj[-1];

  switch (GET_TYPE(tag)) {
    case RECORD_TYPE: {
      int i;
      int fieldLen = GET_RECLEN(tag);
      int mask = GET_RECMASK(tag);
      for (i=0; i<fieldLen; i++, mask >>= 1) {
	if (mask & 1) {   /* low bit set means current field is pointer */
	  loc_t field = (loc_t) (obj + i);
	  ptr_t value = (ptr_t) *field;
	  if (IsTagData(value) || IsGlobalData(value))
	    continue;
	  pushStack(oStack, field);
	}
      }
      assert(mask == 0);
      return fieldLen;
    }
    case PARRAY_TYPE: {
      int byteLen = GET_ARRLEN(tag);
      int wordLen = byteLen >> 2;
      int i;
      for (i=0; i<wordLen; i++) {
	loc_t field = (loc_t) (obj + i);
	ptr_t value = (ptr_t) *field;
	if (!(IsTagData(value)) && !(IsGlobalData(value))) 
	  pushStack(oStack, field);
      }
      return wordLen;
    }
    case IARRAY_TYPE: 
    case RARRAY_TYPE:       return 0 ;
    case SKIP_TYPE: 
    case FORWARD1_TYPE:
    case FORWARD2_TYPE:
    default:
      printf("Unexpeted tag %d found in getNontagPointerLocation\n",tag); 
      assert(0); 
  }
  assert(0);
}


/* Returns object length includnig tag word in bytes */
unsigned long objectLength(ptr_t obj)
{
  tag_t tag = (tag_t) obj[-1];

  switch (GET_TYPE(tag)) {
    case IARRAY_TYPE:
    case RARRAY_TYPE:
    case PARRAY_TYPE: {
      int bytelen = GET_ARRLEN(tag);
      return 4 + ((bytelen + 3) / 4) * 4;
    }
  case RECORD_TYPE: {
    int numFields = GET_RECLEN(tag);
    assert (numFields != 0);  /* There should be no empty records */
    return 4 * (1 + numFields);
  }
  case SKIP_TYPE:
    return 4 * (tag >> SKIPLEN_OFFSET);
  case FORWARD1_TYPE:
  case FORWARD2_TYPE: 
    return objectLength((ptr_t) tag);
  default: {
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
INLINE1(copyOrAllocAndSpaceCheck)
INLINE2(copyOrAllocAndSpaceCheck)
int copyOrAlloc(Proc_t *proc, ptr_t white, CopyRange_t *copyRange, int doCopy, int noSpaceCheck)
{
  ptr_t obj;
  int tag = white[-1];
  int type = GET_TYPE(tag);

  if (type == RECORD_TYPE) {               /* Record tag is the most common case */
    int i, numFields = GET_RECLEN(tag);
    int objByteLen = 4 * (1 + numFields);
    mem_t region = allocFromCopyRange(copyRange, objByteLen, 0, noSpaceCheck);
    obj = region + 1;
    if (doCopy)
      for (i=0; i<numFields; i++)         /* Copy tag and record fields */
	obj[i] = white[i];
    obj[-1] = tag;                       /* Write tag last */
    white[-1] = (val_t) obj;             /* Store forwarding pointer in old object */
    proc->numCopied++;
    proc->segUsage.bytesCopied += objByteLen;
    return objByteLen;
  }
  else if (TYPE_IS_FORWARD(type)) {
    proc->numShared++;
    return 0;
  }
  else if (TYPE_IS_ARRAY(type)) {
    int i, arrayByteLen = GET_ARRLEN(tag);
    int objByteLen = 4 + ((arrayByteLen + 3) >> 2 << 2);
    mem_t region = allocFromCopyRange(copyRange, objByteLen, type == RARRAY_TYPE, noSpaceCheck);
    obj = region + 1;
    if (doCopy)
      memcpy((char *) obj, (const char *)white, objByteLen - 4);
    obj[-1] = tag;                       /* Write tag last */
    white[-1] = (val_t) obj;             /* Store forwarding pointer in old object */
    proc->numCopied++;
    proc->segUsage.bytesCopied += objByteLen;
    return objByteLen;
  }
  else if (type == SKIP_TYPE)
    return 0;
  else 
    printf("\n\ncopyOrAlloc: Bad tag %d for object %d \n", tag, white);
  assert(0);
}

int copy(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return copyOrAlloc(proc, white, copyRange, 1, 0);
}

int alloc(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return copyOrAlloc(proc, white, copyRange, 0, 0);
}

void copy_noSpaceCheck(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  (void) copyOrAlloc(proc, white, copyRange, 1, 1);
}


/* Tag is the the tag value when the caller checked.  It may have changed */
INLINE1(acquireOwnership)
INLINE2(acquireOwnership)
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

INLINE1(copyOrAlloc_copyCopySync)
INLINE2(copyOrAlloc_copyCopySync)
int copyOrAlloc_copyCopySync(Proc_t *proc, ptr_t white, CopyRange_t *copyRange, int doCopy)
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
    if (doCopyCopySync)  /* For measuring the costs of the copy-copy sync */
      tag = acquireOwnership(proc, white, tag, copyRange);
  }

  /* The tag must be restored only after the forwarding address is written */
  type = GET_TYPE(tag);
  if (type == RECORD_TYPE) {           /* As usual, the record case is the most common */
    int i, numFields = GET_RECLEN(tag);
    int objByteLen = 4 * (1 + numFields);
    mem_t region = allocFromCopyRange(copyRange, objByteLen, 0, 0);
    obj = region + 1;
    if (doCopy)
      for (i=0; i<numFields; i++)     /* Copy fields */
	obj[i] = white[i];
    obj[-1] = tag;                  /* Write tag last */
    white[-1] = (val_t) obj;        /* Store forwarding pointer last */
    /* Sparc TSO order guarantees forwarding pointer will be visible only after fields are visible */
    proc->numCopied++;
    proc->segUsage.bytesCopied += objByteLen;
    return objByteLen;
  }
  else if (TYPE_IS_FORWARD(type)) {
    /* ptr_t gray = (ptr_t) tag;
       assert(gray != (ptr_t) gray[-1]); Make sure object is not self-forwarded */
    proc->numShared++;
    return 0;
  }
  else if (TYPE_IS_ARRAY(type)) {
    int i, arrayByteLen = GET_ARRLEN(tag);
    int objByteLen = 4 + ((arrayByteLen + 3) >> 2 << 2);
    mem_t region = allocFromCopyRange(copyRange, objByteLen, type == RARRAY_TYPE, 0);
    obj = region + 1;
    if (doCopy)
      memcpy((char *) obj, (const char *)white, objByteLen - 4);
    obj[-1] = tag;	
    white[-1] = (val_t) obj;
    proc->numCopied++;
    proc->segUsage.bytesCopied += objByteLen;
    return objByteLen;
  }
  else if (type == SKIP_TYPE)
    return 0;
  else 
    printf("\n\ncopy_copyCopySync: BAD TAG: white = %d, tag = %d\n",white,tag);
  assert(0);
}

int copy_copyCopySync(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return copyOrAlloc_copyCopySync(proc, white, copyRange, 1);
}

int alloc_copyCopySync(Proc_t *proc, ptr_t white, CopyRange_t *copyRange)
{
  return copyOrAlloc_copyCopySync(proc, white, copyRange, 0);
}

/* ------------------------------------------------------- */
/* -------------- Exported functions --------------------- */
/* ------------------------------------------------------- */

void discard_writelist(Proc_t *proc)
{
  proc->writelistCursor = proc->writelistStart;
}

/* Add the locations of all pointer arrays modified with back pointers */
void add_writelist_to_rootlist(Proc_t *proc, range_t *from, range_t *to)
{
  ploc_t curLoc = proc->writelistStart;
  ploc_t end = proc->writelistCursor;
  while (curLoc < end) {
    ptr_t obj = (ptr_t) (*(curLoc++)), data;
    int byteOffset = (int) (*(curLoc++));  
    ptr_t prevPtrVal = (ptr_t) (*(curLoc++));  /* We ignore this value for non-concurrent collectors */
    tag_t tag = (tag_t) obj[-1];
    ploc_t field;
    if (GET_TYPE(tag) != PARRAY_TYPE)
      continue;
    field  = (ploc_t) (obj + byteOffset / sizeof(val_t));
    data = *field;
    if (NotInRange(data,to)) {
      pushStack(proc->roots, (ptr_t) field);
    }
  }
  proc->writelistCursor = proc->writelistStart;
}


/* --------------- Scanning routines ----------------------- */

mem_t scanTag_locCopy1_noSpaceCheck(Proc_t *proc, mem_t start, CopyRange_t *copyRange, 
				    range_t *from)
{
  tag_t tag = start[0];
  int type = GET_TYPE(tag);
  ptr_t gray = start + 1;

  if (type == RECORD_TYPE) {
    unsigned mask = GET_RECMASK(tag);
    int i, fieldLen = GET_RECLEN(tag);
    for (i=0; i<fieldLen; i++, mask >>= 1) 
      if (mask & 1) 
	locCopy1_noSpaceCheck(proc,(ploc_t)gray + i,copyRange,from);
    proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    return gray + fieldLen;
  }
  else if (type == IARRAY_TYPE || type == RARRAY_TYPE) {
    unsigned int fieldLen = (GET_ARRLEN(tag) + 3) / 4; /* IARRAY len might not be mult of 4 */
    proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    return gray + fieldLen;
  }
  else if (type == PARRAY_TYPE) {
    int i, fieldLen = GET_ARRLEN(tag) >> 2;
    for (i=0; i<fieldLen; i++) 
      locCopy1_noSpaceCheck(proc,(ploc_t)gray + i,copyRange,from);
    proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    return gray + fieldLen;
  }
  else if (type == SKIP_TYPE)
    return start + (tag >> SKIPLEN_OFFSET);
  else {
    printf("\n\nscanTag_locCopy1_noSpaceCheck: found bad tag = %d at start %d\n",tag,start);
    assert(0);
  }
  assert(0);
}

mem_t scanTag_locCopy2_noSpaceCheck(Proc_t *proc, mem_t start, CopyRange_t *copyRange,
				   range_t *from, range_t *from2, range_t *large)
{
  tag_t tag = start[0];
  int type = GET_TYPE(tag);
  ptr_t gray = start + 1;

  if (type == RECORD_TYPE) {
    unsigned mask = GET_RECMASK(tag);
    int i, fieldLen = GET_RECLEN(tag);
    for (i=0; i<fieldLen; i++, mask >>= 1)
      if (mask & 1) 
	locCopy2_noSpaceCheck(proc,(ploc_t)(gray+i),copyRange,from,from2,large);
    proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    return gray + fieldLen;
  }
  else if (type == IARRAY_TYPE || type == RARRAY_TYPE) {
    unsigned int fieldLen = (GET_ARRLEN(tag) + 3) / 4; /* IARRAY len might not be mult of 4 */
    proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    return gray + fieldLen;
  }
  else if (type == PARRAY_TYPE) {
    int i, fieldLen = GET_ARRLEN(tag) >> 2;
    for (i=0; i<fieldLen; i++)
      locCopy2_noSpaceCheck(proc,(ploc_t)(gray+i),copyRange,from,from2,large);
    proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    return gray + fieldLen;
  }
  else if (type == SKIP_TYPE)
    return start + (tag >> SKIPLEN_OFFSET);
  else {
    printf("\n\nscanTag_locCopy2_noSpaceCheck: bad tag = %d at start=%d\n",tag,start);
    assert(0);
  }
  assert(0);
}

void scanObj_locCopy1_noSpaceCheck(Proc_t *proc, ptr_t obj, CopyRange_t *copyRange, range_t *from_range)
{
  scanTag_locCopy1_noSpaceCheck(proc, obj - 1, copyRange, from_range);
}

void scanUntil_locCopy1_noSpaceCheck(Proc_t *proc, mem_t start_scan, CopyRange_t *copyRange,
				      range_t *from_range)
{
  mem_t cur = start_scan;
  while (cur < copyRange->cursor) {
    cur = scanTag_locCopy1_noSpaceCheck(proc, cur, copyRange, from_range);
  }
  assert(cur == copyRange->cursor);
}

void scanUntil_locCopy2_noSpaceCheck(Proc_t *proc, mem_t start_scan, CopyRange_t *copyRange,
				      range_t *from_range, range_t *from2_range, range_t *large)
{
  mem_t cur = start_scan;
  while (cur < copyRange->cursor) {
    cur = scanTag_locCopy2_noSpaceCheck(proc, cur, copyRange, from_range, from2_range, large);
  }
  assert(cur == copyRange->cursor);
}

void transferScanObj_locCopy1_copyCopySync_replicaStack(Proc_t *proc, ptr_t gray,  Stack_t *localStack, CopyRange_t *copyRange,
						range_t *from_range)
{
  tag_t tag = gray[-1];
  while (1) {
    int type = GET_TYPE(tag);
    /* Records are the most common case */
    if (type == RECORD_TYPE) {
      unsigned mask = GET_RECMASK(tag);
      int i, fieldlen = GET_RECLEN(tag);
      for (i=0; i<fieldlen; i++, mask >>= 1) {
	if (mask & 1)
	  locCopy1_copyCopySync_replicaStack(proc,(ploc_t)gray+i,localStack,copyRange,from_range);
      }
      proc->segUsage.bytesScanned += 4 * (1 + fieldlen);
    } 
    /* Arrays */
    else if (TYPE_IS_ARRAY(type)) {
      int i, arrayByteLen = GET_ARRLEN(tag);  /* Int array length might not be multiple of 4 */
      int fieldLen = (arrayByteLen + 3) >> 2;   
      if (type == PARRAY_TYPE) 
	for (i=0; i<fieldLen; i++) 
	  locCopy1_copyCopySync_replicaStack(proc,(ploc_t)gray+i,localStack,copyRange,from_range);
      proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    }
    /* Forwarding pointers */
    else if (TYPE_IS_FORWARD(type)) {
      tag = ((ptr_t)tag)[-1];
      continue;
    }
    /* Skip */
    else if (type == SKIP_TYPE) 
      proc->segUsage.bytesScanned += (tag >> SKIPLEN_OFFSET);
    /* Tag not in place yet */
    else {
      while (tag == STALL_TAG) {
	flushStore();
	tag = gray[-1];
      }
      continue;
    }
    return;
  }
  assert(0);
}


void scanObj_locCopy2_copyCopySync_replicaStack(Proc_t *proc, ptr_t gray, Stack_t *localStack,  CopyRange_t *copyRange,
							 range_t *from_range,  range_t *from2_range, range_t *large_range)
{
  tag_t tag = gray[-1];
  while (1) {
    int type = GET_TYPE(tag);
    if (type == RECORD_TYPE) {
      int i, fieldLen = GET_RECLEN(tag);
      unsigned mask = GET_RECMASK(tag);
      for (i=0; i<fieldLen; i++, mask >>= 1) {
	if (mask & 1)
	  locCopy2_copyCopySync_replicaStack(proc,(ploc_t)gray+i,localStack,copyRange,
					       from_range,from2_range,large_range);
      }
      proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    }
    /* Arrays */
    else if (TYPE_IS_ARRAY(type)) {      
      int i, arrayByteLen = GET_ARRLEN(tag);  /* Int array length might not be multiple of 4 */
      int fieldLen = (arrayByteLen + 3) >> 2;  
      if (type == PARRAY_TYPE) 
	for (i=0; i<fieldLen; i++) 
	  locCopy2_copyCopySync_replicaStack(proc,(ploc_t)gray+i,localStack,copyRange,
					from_range,from2_range,large_range);
      proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
    }
    /* Skip */
    else if (type == SKIP_TYPE) 
      proc->segUsage.bytesScanned += (tag >> SKIPLEN_OFFSET);
    /* Forwarding pointers */
    else if (TYPE_IS_FORWARD(type)) {
      tag = ((ptr_t)tag)[-1];
      continue;
    }
    /* Tag not in place yet */
    else {
      while (tag == STALL_TAG) {
	flushStore();
	tag = gray[-1];
      }
      continue;
    }
    return;
  }
  assert(0);
}


typedef enum LocAllocCopy__t {Copy, LocCopy, LocAlloc} LocAllocCopy_t;

INLINE1(mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack)
INLINE2(mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack)
void mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack
           (Proc_t *proc, 
	    ptr_t primaryOrReplicaGray, 
	    Stack_t *localStack, CopyRange_t *copyRange,
	    range_t *from_range, range_t *from2_range, range_t *large_range,
	    int doCopyWrite, LocAllocCopy_t locAllocCopy, int howMany, int mayTransfer)
{
  /* If performing transfer, we are given primaryGray and must copy fields into replicaGray */
  ptr_t primaryGray = mayTransfer ? primaryOrReplicaGray : NULL;
  ptr_t replicaGray = mayTransfer ? (ptr_t) primaryGray[-1] : primaryOrReplicaGray;
  tag_t tag = replicaGray[-1];
  if (doCopyWrite)
    assert(mayTransfer);
  while (1) {
    int type = GET_TYPE(tag);
    if (type == RECORD_TYPE) {                      /* Records are the most common case - no copy-write sync needed */
      unsigned int mask = GET_RECMASK(tag);
      int i, fieldlen = GET_RECLEN(tag);
      for (i=0; i<fieldlen; i++, mask >>= 1) {
	if (mayTransfer)
	  replicaGray[i] = primaryGray[i];
	if (mask & 1) {
	  /* Since no copy-write sync is required, objects that point to themselves won't cause looping */
	  switch (locAllocCopy) {
	  case LocAlloc:
	    if (howMany == 1)
	      locAlloc1_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range);
	    else if (howMany == 2)
	      locAlloc2_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range,from2_range,large_range);
	    else 
	      assert(0);
	    break;
	  case LocCopy:
	    if (howMany == 1)
	      locCopy1_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range);
	    else if (howMany == 2)
	      locCopy2_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range,from2_range,large_range);
	    else
	      assert(0);
	    break;
	  case Copy:
	    if (howMany == 1)
	      copy1_copyCopySync_primaryStack(proc,(ptr_t) replicaGray[i],localStack,copyRange,from_range);
	    else
	      assert(0);
	  }
	}
      }
      proc->segUsage.bytesScanned += 4 * (1 + fieldlen);
      return;
    } 
    else if (TYPE_IS_ARRAY(type)) {
      int i, arrayByteLen = GET_ARRLEN(tag);      /* Int array length might not be multiple of 4 */
      int fieldLen = (arrayByteLen + 3) >> 2;     
      if (doCopyWrite)
	primaryGray[-1] = STALL_TAG;
      if (fieldLen > arraySegmentSize && largeArrayStack != NULL) {
	int i, segments = DivideUp(fieldLen, arraySegmentSize);
	static Stack_t stack;
	static int first = 1;
	if (first) {
	  first = 0;
	  allocStack(&stack, 1024);
	}
	assert(primaryGray != NULL);
	resetSharedObjStack(largeArrayStack, 1);
	for (i=0; i<segments; i++) 
	  pushStack2(&stack, (ptr_t) primaryGray, (ptr_t) i);
	pushSharedObjStack(largeArrayStack, &stack);
      }
      if (mayTransfer)
	memcpy((char *) replicaGray, (const char *)primaryGray, 4 * fieldLen);
      if (type == PARRAY_TYPE) 
	for (i=0; i<fieldLen; i++) {
	  if (doCopyWrite && 
	      (ptr_t) primaryGray[i] == primaryGray) { /* Check is needed for objects that point to themselves - otherwise, loop */
	    replicaGray[i] = (val_t) replicaGray;
	    continue;
	  }
	  switch (locAllocCopy) {
	    case LocAlloc:
	      if (howMany == 1)
		locAlloc1_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range);
	      else if (howMany == 2)
		locAlloc2_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range,from2_range,large_range);
	      else
		assert(0);
	      break;
	    case LocCopy:
	      if (howMany == 1) 
		locCopy1_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range);
	      else if (howMany == 2)
		locCopy2_copyCopySync_primaryStack(proc,(ploc_t)replicaGray+i,localStack,copyRange,from_range,from2_range,large_range);
	      else
		assert(0);
	      break;
	    case Copy:
	      if (howMany == 1)
		copy1_copyCopySync_primaryStack(proc,(ptr_t) replicaGray[i],localStack,copyRange,from_range);
	      else
		assert(0);
	  }
	}
      if (doCopyWrite)
	primaryGray[-1] = (val_t) replicaGray;
      proc->segUsage.bytesScanned += 4 * (1 + fieldLen);
      if (fieldLen > 32) 
	updateWorkDone(proc);
      return;
    }
    else if (TYPE_IS_FORWARD(type)) {
      tag = ((ptr_t)tag)[-1];
      continue;
    }
    else if (type == SKIP_TYPE) {
      proc->segUsage.bytesScanned += (tag >> SKIPLEN_OFFSET);
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
					     range_t *from_range)
{
  mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack(proc, replicaGray, localStack, copyRange,
										   from_range, NULL, NULL,
										   0, Copy, 1, 0);
}

void scanObj_locCopy1_copyCopySync_primaryStack(Proc_t *proc, ptr_t replicaGray, Stack_t *localStack, CopyRange_t *copyRange,
					     range_t *from_range)
{
  mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack(proc, replicaGray, localStack, copyRange,
										   from_range, NULL, NULL,
										   0, LocCopy, 1, 0);
}

void transferScanObj_locCopy1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, Stack_t *localStack, CopyRange_t *copyRange,
							range_t *from_range)
{
  mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack(proc, primaryGray, localStack, copyRange,
										from_range, NULL, NULL,
										0, LocCopy, 1, 1);
}

void transferScanObj_locCopy2_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, Stack_t *localStack, CopyRange_t *copyRange,
							range_t *from_range, range_t *from2_range, range_t *large_range)
{
  mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack(proc, primaryGray, localStack, copyRange,
										from_range, from2_range, large_range,
										0, LocCopy, 2, 1);
}


void transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, Stack_t *localStack, CopyRange_t *copyRange,
								       range_t *from_range)
{
  mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack(proc, primaryGray, localStack, copyRange,
										from_range, NULL, NULL,
										1, LocAlloc, 1, 1);
}

void transferScanObj_copyWriteSync_locAlloc2_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, Stack_t *localStack, CopyRange_t *copyRange,
							range_t *from_range, range_t *from2_range, range_t *large_range)
{
  mayTransferScanObj_mayCopyWriteSync_mayLocAllocCopyNum_copyCopySync_primaryStack(proc, primaryGray, localStack, copyRange,
										from_range, from2_range, large_range,
										1, LocAlloc, 2, 1);
}


