/* Unfortunately, the DEC cc compiler will not inline functions
   that use 32-bit pointers.  On the other hand, gcc
   on the Alpha does not support 32-bit pointers at all.
*/


#ifndef _forward_h
#define _forward_h

#include "queue.h"
#include "general.h"
#include "memobj.h"
#include "thread.h"
#include "gc_large.h"



void dischargeCopyRange(CopyRange_t *copyRange);           /* Leftover area is padded with PadHeapArea */
void expandCopyRange(CopyRange_t *copyRange, int size);    /* Leftover area is padded with PadHeapArea */

int IsNullCopyRange(CopyRange_t *copyRange);
void SetCopyRange(CopyRange_t *copyRange, Proc_t *, Heap_t *heap, expand_t *expand, discharge_t *discharge, Stack_t *region, int);
void PadCopyRange(CopyRange_t *copyRange);                 /* Current unused area is padded with PadHeapArea */
void ClearCopyRange(CopyRange_t *copyRange);

/* Given an object or a skip tag, returns the length.
   Also returns the start (including tags) of object */
unsigned long objectLength(ptr_t obj, mem_t *start);



/* A few definition
   -----------------
   Note that "copy" = "alloc" + "transfer"

   transferScanObj_* - copy the fields of the given primary object to the replica object indicated by the forwarding pointer;
                       apply a function * to each pointer field of the object; returns nothing
   scanObj_*         - apply a function * to each pointer field of the object; returns nothing
   scanTag_*         - apply a function * to each pointer field of the object just after the tag; returning address just past obj
   scanUntil_*       - apply a function * to each pointer field of the objects until the allocation pointer equals the scan pointer
   alloc_*           - alloc space for a copy of the object and install a forwarding pointer in the original object but do not copy fields
   copy_*            - makes a copy of an object and install a forwarding pointer in the original object
   locCopy_*         - update a location containing a pointer to an object with a pointer to the copy;
                       this usually involves calling the copy_* version of the function
   locAlloc_*        - update a location containing a pointer to an object with a pointer to the copy;
                       this usually involves calling the alloc_* version of the function
   noSpaceCheck_     - assume there is enough room in the CopyRange; this is possible in uniprocessor stop-and-copy  collection
   copyCopySync_*    - a modifier on copy/alloc; the object is locked by atomically installing STALL_TAG; to synchronize multiple copiers 
   scanWriteSync_*   - a modifier on scan; the object is reserved by writing STALL_TAG; to synchronize a copier and a writer
   primaryStack_*    - a copy/alloc routine that inserts the primary into the stack if the primary was actually copied
   replicaStack_*    - a copy/alloc routine that inserts the replica into the stack if the primary was actually copied
   1_                - qualifying the copy/alloc by checking if the pointer is in the given source space
   2L_               - qualifying the copy/alloc by checking if the pointer is in one of 2 source spaces;
                       if object is in large space then call gc_large_addRoot
   unique_           - assumes object has not not been copied yet and that there are no other copiers
*/

/* ------------------------------------------------------- */
/* -------------- Internal functions --------------------- */
/* ------------------------------------------------------- */

/* copy and alloc
   (0) Assumes obj is a heap pointer
   (1) Takes a primary object and (if it is not already copied)
       (a) allocate space for a new copy of the object,
       (b) copy the tag and the fields (if copy and not alloc)
       (c) install a forwarding pointer from the primary to the copy.
   (2) The allocation pointer and limit pointer are updated in copyRange.
   (3) The number of bytes copied is returned and added to proc->curBytesCopied.
   (4) The copied object can be retrieved with obj[-1].
*/
int copy(Proc_t *, ptr_t obj, CopyRange_t *copyRange);
int alloc(Proc_t *, ptr_t obj, CopyRange_t *copyRange);
int copy_noSpaceCheck(Proc_t *, ptr_t obj, CopyRange_t *copyRange);
int copy_noSpaceCheck_replicaStack(Proc_t *, ptr_t obj, CopyRange_t *copyRange, Stack_t *);

/* copy_copyCopySync and alloc_copyCopySync
   (1) Takes a primary object and (if it is not already copied)
       (a) allocate space for a new copy of the object,
       (b) copy the tag and the fields (if copy and not alloc)
       (c) install a forwarding pointer from the primary to the copy.
   (2) The allocation pointer and limit pointer are in copyRange.
   (3) The object being copied is locked first to prevent multiple copies
       from being made because of multiple copiers.  The object is 
       copied coarsely, all at once, even if the object is large.
   (4) The number of bytes copied is returned and added to proc->curBytesCopied.
   (5) The new copy of the object can be retrieved with obj[-1].
*/
int copy_copyCopySync(Proc_t *, ptr_t obj, CopyRange_t *copyRange);
int copy_copyCopySync_primaryStack(Proc_t *, ptr_t obj, CopyRange_t *copyRange, Stack_t *grayStack);
int copy_copyCopySync_replicaStack(Proc_t *, ptr_t obj, CopyRange_t *copyRange, Stack_t *grayStack);

int alloc_copyCopySync(Proc_t *, ptr_t obj, CopyRange_t *copyRange);
int alloc_copyCopySync_primaryStack(Proc_t *, ptr_t obj, CopyRange_t *copyRange, Stack_t *grayStack);
int alloc_copyCopySync_replicaStack(Proc_t *, ptr_t obj, CopyRange_t *copyRange, Stack_t *grayStack);

int copy_noSpaceCheck_copyCopySync(Proc_t *, ptr_t obj, CopyRange_t *copyRange);
int copy_noSpaceCheck_copyCopySync_replicaStack(Proc_t *, ptr_t obj, CopyRange_t *copyRange, Stack_t *grayStack);


/* ------------------------------------------------------- */
/* -------------- Exported functions --------------------- */
/* ------------------------------------------------------- */

/* copy1_noSpaceCheck */
INLINE(copy1_noSpaceCheck)
void copy1_noSpaceCheck(Proc_t *proc, ptr_t obj, CopyRange_t *copyRange, Heap_t *from)
{
  if (InRange((mem_t) obj, &from->range))
    copy_noSpaceCheck(proc,obj,copyRange);
}

/* locCopy1_noSpaceCheck */
INLINE(locCopy1_noSpaceCheck)
void locCopy1_noSpaceCheck(Proc_t *proc, ploc_t loc, CopyRange_t *copyRange, Heap_t *from)
{
  ptr_t obj = *loc;
  if (InRange((mem_t) obj, &from->range)) {
    copy_noSpaceCheck(proc,obj,copyRange);
    *loc = (ptr_t) obj[-1];
  }
}

/* locCopy2L_noSpaceCheck */
INLINE(locCopy2L_noSpaceCheck)
void locCopy2L_noSpaceCheck(Proc_t *proc, ploc_t loc, CopyRange_t *copyRange,
			    Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t obj = *loc;
  if (InRange((mem_t) obj, &from->range) ||
      InRange((mem_t) obj, &from2->range)) {
    copy_noSpaceCheck(proc,obj,copyRange);
    *loc = (ptr_t) obj[-1];
  }
  else if ((val_t) obj - (val_t) large->range.low < large->range.diff)
    gc_large_addRoot(obj);
}

/* copy2L_noSpaceCheck */
INLINE(copy2L_noSpaceCheck)
void copy2L_noSpaceCheck(Proc_t *proc, ptr_t obj, CopyRange_t *copyRange,
			 Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  if (InRange((mem_t) obj, &from->range) ||
      InRange((mem_t) obj, &from2->range)) {
    copy_noSpaceCheck(proc,obj,copyRange);
  }
  else if ((val_t) obj - (val_t) large->range.low < large->range.diff)
    gc_large_addRoot(obj);
}

INLINE(copy1_copyCopySync_replicaStack)
void copy1_copyCopySync_replicaStack(Proc_t *proc, ptr_t obj, Stack_t *localStack, 
				     CopyRange_t *copyRange, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    copy_copyCopySync_replicaStack(proc,obj,copyRange,localStack);
}

INLINE(alloc1_copyCopySync_primaryStack)
void alloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t obj, Stack_t *localStack, 
				     CopyRange_t *copyRange, Heap_t *from)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync_primaryStack(proc,obj,copyRange,localStack);
}

INLINE(alloc1L_copyCopySync_primaryStack)
void alloc1L_copyCopySync_primaryStack(Proc_t *proc, ptr_t obj, Stack_t *localStack, 
				     CopyRange_t *copyRange, Heap_t *from, Heap_t *large)
{ 
  if (InRange((mem_t) obj, &from->range))
    alloc_copyCopySync_primaryStack(proc,obj,copyRange,localStack);
  else if (InRange((mem_t) obj, &large->range)) 
    gc_large_addRoot(obj);
}

INLINE(locAlloc1_copyCopySync_primaryStack)
void locAlloc1_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, 
					CopyRange_t *copyRange, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    alloc_copyCopySync_primaryStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
}

INLINE(locAlloc1L_copyCopySync_primaryStack)
void locAlloc1L_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, 
					CopyRange_t *copyRange, Heap_t *from, Heap_t *large)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    alloc_copyCopySync_primaryStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
  else if (InRange((mem_t) white, &large->range)) 
    gc_large_addRoot(white);
}

/* locCopy1_copyCopySync_replicaStack */
INLINE(locCopy1_copyCopySync_replicaStack)
void locCopy1_copyCopySync_replicaStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, 
					CopyRange_t *copyRange, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    copy_copyCopySync_replicaStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
}

/* locCopy1_noSpaceCheck_copyCopySync_replicaStack */
INLINE(locCopy1_noSpaceCheck_copyCopySync_replicaStack)
void locCopy1_noSpaceCheck_copyCopySync_replicaStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, 
						     CopyRange_t *copyRange, Heap_t *from)
{ 
  ptr_t white = *loc;						
  if (InRange((mem_t) white, &from->range)) {	
    copy_noSpaceCheck_copyCopySync_replicaStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
}

/* locCopy1_noSpaceCheck_replicaStack */
INLINE(locCopy1_noSpaceCheck_replicaStack)
void locCopy1_noSpaceCheck_replicaStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, 
					CopyRange_t *copyRange, Heap_t *from)
{ 
  ptr_t white = *loc;				
  if (InRange((mem_t) white, &from->range)) {			
    copy_noSpaceCheck_replicaStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
}

/* locCopy1_noSpaceCheck_copyCopySync */
INLINE(locCopy1_noSpaceCheck_copyCopySync)
void locCopy1_noSpaceCheck_copyCopySync(Proc_t *proc, ploc_t loc, CopyRange_t *copyRange, Heap_t *from)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range)) {				
    copy_noSpaceCheck_copyCopySync(proc,white,copyRange);
    *loc = (ptr_t) white[-1];
  }
}

/* copy1_copyCopySync_primaryStack */
INLINE(copy1_copyCopySync_primaryStack)
void copy1_copyCopySync_primaryStack(Proc_t *proc, ptr_t white, Stack_t *localStack, 
				     CopyRange_t *copyRange, Heap_t *from)
{ 
  if (InRange((mem_t) white, &from->range)) 
    copy_copyCopySync_primaryStack(proc,white,copyRange,localStack);
}

/* locCopy1_copyCopySync_primaryStack */
INLINE(locCopy1_copyCopySync_primaryStack)
void locCopy1_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, 
					CopyRange_t *copyRange, Heap_t *from)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range)) {
    copy_copyCopySync_primaryStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
}

/* locCopy1L_copyCopySync_primaryStack */
INLINE(locCopy1L_copyCopySync_primaryStack)
void locCopy1L_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, 
					 CopyRange_t *copyRange, Heap_t *from, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range)) {			
    copy_copyCopySync_primaryStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
  else if (InRange((mem_t) white, &large->range)) 
    gc_large_addRoot(white);
}

/* locCopy2L_copyCopySync_replicaStack */
INLINE(locCopy2L_copyCopySync_replicaStack)
void locCopy2L_copyCopySync_replicaStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange,
					 Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    copy_copyCopySync_replicaStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}


/* copy2L_copyCopySync_primaryStack */
INLINE(copy2L_copyCopySync_primaryStack)
void copy2L_copyCopySync_primaryStack(Proc_t *proc, ptr_t white, Stack_t *localStack, CopyRange_t *copyRange,
				      Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) 
    copy_copyCopySync_primaryStack(proc,white,copyRange,localStack);
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

/* locCopy2L_copyCopySync_primaryStack */
INLINE(locCopy2L_copyCopySync_primaryStack)
void locCopy2L_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange,
					 Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    copy_copyCopySync_primaryStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

/* locCopy2L_copyCopySync_replicaStack */
INLINE(locCopy2L_copyCopySync_replicaStack)
void locCopy2L_copyCopySync_replica(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange,
				    Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    copy_copyCopySync_replicaStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}

/* locAlloc2L_copyCopySync_primaryStack */
INLINE(locAlloc2L_copyCopySync_primaryStack)
void locAlloc2L_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange, 
					  Heap_t *from, Heap_t *from2, Heap_t *large)
{ 
  ptr_t white = *loc;							
  if (InRange((mem_t) white, &from->range) ||
      InRange((mem_t) white, &from2->range)) {
    alloc_copyCopySync_primaryStack(proc,white,copyRange,localStack);
    *loc = (ptr_t) white[-1];
  }
  else if (InRange((mem_t) white, &large->range))
    gc_large_addRoot(white);
}


/* ------------------- Writelist Routines ---------------------- */
void process_writelist(Proc_t *proc, Heap_t *from, Heap_t *to);

/* ------------------- Scanning Routines ---------------------- */

/* scanUntil_locCopy?_noSpaceCheck - Standard cheney scan for stop and copy collectors
   scanObj_locCopy?_copyCopySync_replicaStack - Parallel but not concurrent collectors
   transferScanObj_locCopy?_copyCopySync_primaryStack - Parallel but not concurrent collectors
   transferScanObj_copyWriteSync_locCopy?_copyCopySync_primaryStack - Concurrent collectors
*/
void scanObj_locCopy1_noSpaceCheck(Proc_t *proc, ptr_t obj, 
				   CopyRange_t *copyRange, Heap_t *from_range);
void scanUntil_locCopy1_noSpaceCheck(Proc_t *, mem_t start_scan, 
				     CopyRange_t *, Heap_t *from_range);
void scanUntil_locCopy2L_noSpaceCheck(Proc_t *, mem_t start_scan, CopyRange_t *,
				     Heap_t *from_range, Heap_t *from2_range, Heap_t *large);

void scanObj_locCopy1_copyCopySync_replicaStack(Proc_t *, ptr_t gray_obj, Stack_t *, CopyRange_t *copyRange,
						Heap_t *from_range);

void scanObj_copy1_copyCopySync_primaryStack(Proc_t *, ptr_t replicaGray, Stack_t *, CopyRange_t *copyRange,
					     Heap_t *from_range);

void scanObj_locCopy1_copyCopySync_primaryStack(Proc_t *, ptr_t replicaGray, Stack_t *, CopyRange_t *copyRange,
						Heap_t *from_range);

void scanObj_locCopy1L_copyCopySync_primaryStack(Proc_t *, ptr_t replicaGray, Stack_t *, CopyRange_t *copyRange,
						 Heap_t *from_range, Heap_t *large_range);

void scanObj_locCopy2L_copyCopySync_replicaStack(Proc_t *, ptr_t replicaGray, Stack_t *, CopyRange_t *copyRange,
						 Heap_t *nursery_range, Heap_t *from_range, Heap_t *large_range);

void transferScanObj_locCopy1_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, Stack_t *, CopyRange_t *copyRange,
							Heap_t *from_range);
void scanObj_locCopy1_copyCopySync_replicaStack(Proc_t *, ptr_t repGray, Stack_t *, CopyRange_t *copyRange,
						Heap_t *from_range);

void scanObj_locCopy1_noSpaceCheck_copyCopySync_replicaStack(Proc_t *, ptr_t repGray, Stack_t *, CopyRange_t *copyRange,
						Heap_t *from_range);
void scanObj_locCopy1_noSpaceCheck_replicaStack(Proc_t *, ptr_t repGray, Stack_t *, CopyRange_t *copyRange,
						Heap_t *from_range);
void selfTransferScanObj_locAlloc1_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, Stack_t *, Stack_t *, 
							     CopyRange_t *copyRange, Heap_t *from_range);

void transferScanObj_locCopy2L_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, Stack_t *, CopyRange_t *copyRange,
							Heap_t *from_range, Heap_t *from2_range, Heap_t *large_range);


void transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, 
									    Stack_t *obj, Stack_t *seg, CopyRange_t *copyRange,
									    Heap_t *from_range);

void transferScanObj_copyWriteSync_locAlloc1L_copyCopySync_primaryStack(Proc_t *proc, ptr_t primaryGray, 
								       Stack_t *objStack, Stack_t *segmentStack, CopyRange_t *copyRange,
								       Heap_t *from_range, Heap_t *large_range);


void transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, int start, int end,
										Stack_t *obj, Stack_t *seg, CopyRange_t *copyRange,
										Heap_t *from_range);
void selfTransferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, int start, int end,
										Stack_t *obj, Stack_t *seg, CopyRange_t *copyRange,
										Heap_t *from_range);
void transferScanSegment_copyWriteSync_locAlloc1L_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, int start, int end,
										 Stack_t *obj, Stack_t *seg, CopyRange_t *copyRange,
										 Heap_t *from_range, Heap_t *large_range);
void selfTransferScanSegment_copyWriteSync_locAlloc1L_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, int start, int end,
										 Stack_t *obj, Stack_t *seg, CopyRange_t *copyRange,
										 Heap_t *from_range, Heap_t *large_range);



#endif



