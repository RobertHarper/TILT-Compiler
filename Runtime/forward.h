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

unsigned long objectLength(ptr_t obj);

/* getNontagNonglobalPointerLocations - used for processing globals
   (1) Takes an (initilaized) object and a queue of pointer locations
   (2) Decodes the object using its tag and place into the queue 
       all the pointer fields that do not contain
       tags (small constructor values) or addresses of globals
*/

int getNontagNonglobalPointerLocations(ptr_t obj, Stack_t *locs);


/* A few definitions
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
   2_                - qualifying the copy/alloc by checking if the pointer is in one of 2 source spaces;
                       if object is in large space then enqueue it into the large object queue
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

/* copy_noSpaceCheck
   a special version of copy which performs no space checks in the copy range and also returns nothing
*/
void copy_noSpaceCheck(Proc_t *, ptr_t obj, CopyRange_t *copyRange);

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
int alloc_copyCopySync(Proc_t *, ptr_t obj, CopyRange_t *copyRange);




/* ------------------------------------------------------- */
/* -------------- Exported functions --------------------- */
/* ------------------------------------------------------- */

/* locCopy1_noSpaceCheck */
INLINE1(locCopy1_noSpaceCheck)
INLINE2(locCopy1_noSpaceCheck)
void locCopy1_noSpaceCheck(Proc_t *proc, ploc_t loc, CopyRange_t *copyRange, range_t *from)
{
  ptr_t obj = *loc;
  if ((val_t) obj - (val_t)from->low < from->diff) {
    copy_noSpaceCheck(proc,obj,copyRange);
    *loc = (ptr_t) obj[-1];
  }
}

/* locCopy2_noSpaceCheck */
INLINE1(locCopy2_noSpaceCheck)
INLINE2(locCopy2_noSpaceCheck)
void locCopy2_noSpaceCheck(Proc_t *proc, ploc_t loc, CopyRange_t *copyRange,
			   range_t *from, range_t *from2, range_t *large)
{ 
  ptr_t obj = *loc;
  if (((val_t) obj - (val_t) from->low < from->diff) ||  
      ((val_t) obj - (val_t) from2->low < from2->diff)) {
    copy_noSpaceCheck(proc,obj,copyRange);
    *loc = (ptr_t) obj[-1];
  }
  else if ((val_t) obj - (val_t) large->low < large->diff)
    gc_large_addRoot(obj);
}

/* alloc_unique_primaryStack
   (0) Assumes the object has not been copied.
   (1) The object must have been copied. The (newly-made) gray forwarded object is pushed on the proc's local stack
   (2) Returns the size of the object (which equals bytesCopied if object was copied).
       Otherwise, object was a skip tag and we return the amount to skip.
*/
INLINE1(alloc_unique_primaryStack)
INLINE2(alloc_unique_primaryStack)
int alloc_unique_primaryStack(Proc_t *proc, ptr_t white, Stack_t *localStack, CopyRange_t *copyRange, range_t *from)
{ 
  tag_t tag;
  int bytesCopied = alloc(proc,white,copyRange);
  if (bytesCopied) {
    pushStack(localStack, white);
    return bytesCopied;
  } 
  /* If object is not then, then we encountered a skip tag or a forwarding pointer.
     Since the object must not already have been copied, we must have encountered a skip tag. */
  /* assert(GET_TYPE(tag) == SKIP_TAG); */
  tag = white[-1];
  return 4 * (tag >> SKIPLEN_OFFSET);
}



/* copy1_copyCopySync_replicaStack */
INLINE1(copy1_copyCopySync_replicaStack)
INLINE2(copy1_copyCopySync_replicaStack)
void copy1_copyCopySync_replicaStack(Proc_t *proc, ptr_t white, Stack_t *localStack, CopyRange_t *copyRange, range_t *from)
{ 
  if ((val_t) white - (val_t)from->low < from->diff) {
    int bytesCopied = copy_copyCopySync(proc,white,copyRange);
    if (bytesCopied) {
      ptr_t gray = (ptr_t) white[-1];
      pushStack(localStack, gray);
    } 
  }
}

/* alloc1_copyCopySync_primaryStack */
INLINE1(alloc1_copyCopySync_primaryStack)
INLINE2(alloc1_copyCopySync_primaryStack)
void alloc1_copyCopySync_primaryStack(Proc_t *proc, ptr_t white, Stack_t *localStack, CopyRange_t *copyRange, range_t *from)
{ 
  if ((val_t) white - (val_t)from->low < from->diff) {
    int bytesCopied = alloc_copyCopySync(proc,white,copyRange);
    if (bytesCopied) {
      pushStack(localStack, white);
    }
  }
}

/* locCopy1_copyCopySync_replicaStack */
INLINE1(locCopy1_copyCopySync_replicaStack)
INLINE2(locCopy1_copyCopySync_replicaStack)
void locCopy1_copyCopySync_replicaStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange, range_t *from)
{ 
  ptr_t white = *loc;							
  if ((val_t) white - (val_t)from->low < from->diff) {
    int bytesCopied = copy_copyCopySync(proc,white,copyRange);
    ptr_t gray = (ptr_t) white[-1];
    *loc = gray;
    if (bytesCopied) 
      pushStack(localStack, gray);
  }
}

/* copy1_copyCopySync_primaryStack */
INLINE1(copy1_copyCopySync_primaryStack)
INLINE2(copy1_copyCopySync_primaryStack)
void copy1_copyCopySync_primaryStack(Proc_t *proc, ptr_t white, Stack_t *localStack, CopyRange_t *copyRange, range_t *from)
{ 
  if ((val_t) white - (val_t)from->low < from->diff) {
    int bytesCopied = copy_copyCopySync(proc,white,copyRange);
    if (bytesCopied) 
      pushStack(localStack, white);
  }
}

/* locCopy1_copyCopySync_primaryStack */
INLINE1(locCopy1_copyCopySync_primaryStack)
INLINE2(locCopy1_copyCopySync_primaryStack)
void locCopy1_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange, range_t *from)
{ 
  ptr_t white = *loc;							
  if ((val_t) white - (val_t)from->low < from->diff) {
    int bytesCopied = copy_copyCopySync(proc,white,copyRange);
    ptr_t gray = (ptr_t) white[-1];
    *loc = gray;
    if (bytesCopied) 
      pushStack(localStack, white);
  }
}
/* locCopy2_copyCopySync_replicaStack */
INLINE1(locCopy2_copyCopySync_replicaStack)
INLINE2(locCopy2_copyCopySync_replicaStack)
void locCopy2_copyCopySync_replicaStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange,
					range_t *from, range_t *from2, range_t *large)
{ 
  ptr_t white = *loc;
  if (((val_t) white - (val_t)from->low < from->diff) ||
      ((val_t) white - (val_t)from2->low < from2->diff)) {
    int bytesCopied = copy_copyCopySync(proc,white,copyRange);
    ptr_t gray = (ptr_t) white[-1];
    if (bytesCopied) 
      pushStack(localStack, gray);
    *loc = gray;
  }
  else if ((val_t) white - (val_t) large->low < large->diff)
    gc_large_addRoot(white);
}

/* locCopy2_copyCopySync_primaryStack */
INLINE1(locCopy2_copyCopySync_primaryStack)
INLINE2(locCopy2_copyCopySync_primaryStack)
void locCopy2_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange,
					range_t *from, range_t *from2, range_t *large)
{ 
  ptr_t white = *loc;
  if (((val_t) white - (val_t)from->low < from->diff) ||
      ((val_t) white - (val_t)from2->low < from2->diff)) {
    int bytesCopied = copy_copyCopySync(proc,white,copyRange);
    ptr_t gray = (ptr_t) white[-1];
    *loc = gray;
    if (bytesCopied) 
      pushStack(localStack, white);
  }
  else if ((val_t) white - (val_t) large->low < large->diff)
    gc_large_addRoot(white);
}

/* locAlloc1_copyCopySync_primaryStack */
INLINE1(locAlloc1_copyCopySync_primaryStack)
INLINE2(locAlloc1_copyCopySync_primaryStack)
void locAlloc1_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange, range_t *from)
{ 
  ptr_t white = *loc;							
  if ((val_t) white - (val_t)from->low < from->diff) {
    int bytesCopied = alloc_copyCopySync(proc,white,copyRange);
    ptr_t gray = (ptr_t) white[-1];
    *loc = gray;
    if (bytesCopied) {
      pushStack(localStack, white);
    }
  }
}


/* locAlloc2_copyCopySync_primaryStack */
INLINE1(locAlloc2_copyCopySync_primaryStack)
INLINE2(locAlloc2_copyCopySync_primaryStack)
void locAlloc2_copyCopySync_primaryStack(Proc_t *proc, ploc_t loc, Stack_t *localStack, CopyRange_t *copyRange, 
					range_t *from, range_t *from2, range_t *large)
{ 
  ptr_t white = *loc;							
  if (((val_t) white - (val_t)from->low < from->diff) ||
      ((val_t) white - (val_t)from2->low < from2->diff)) {
    int bytesCopied = alloc_copyCopySync(proc,white,copyRange);
    ptr_t gray = (ptr_t) white[-1];
    *loc = gray;
    if (bytesCopied) 
      pushStack(localStack, white);
  }
  else if ((val_t) white - (val_t) large->low < large->diff)
    gc_large_addRoot(white);
}


/* ------------------- Writelist Routines ---------------------- */
void discard_writelist(Proc_t *proc);
void add_writelist_to_rootlist(Proc_t *proc, range_t *from, range_t *to);

/* ------------------- Scanning Routines ---------------------- */

/* scanUntil_locCopy?_noSpaceCheck - Standard cheney scan for stop and copy collectors
   scanObj_locCopy?_copyCopySync_replicaStack - Parallel but not concurrent collectors
   transferScanObj_locCopy?_copyCopySync_primaryStack - Parallel but not concurrent collectors
   transferScanObj_copyWriteSync_locCopy?_copyCopySync_primaryStack - Concurrent collectors
*/
void scanObj_locCopy1_noSpaceCheck(Proc_t *proc, ptr_t obj, 
				   CopyRange_t *copyRange, range_t *from_range);
void scanUntil_locCopy1_noSpaceCheck(Proc_t *, mem_t start_scan, 
				     CopyRange_t *, range_t *from_range);
void scanUntil_locCopy2_noSpaceCheck(Proc_t *, mem_t start_scan, CopyRange_t *,
				     range_t *from_range, range_t *from2_range, range_t *large);

void scanObj_locCopy1_copyCopySync_replicaStack(Proc_t *, ptr_t gray_obj, Stack_t *, CopyRange_t *copyRange,
						range_t *from_range);
void scanObj_locCopy2_copyCopySync_replicaStack(Proc_t *, ptr_t gray_obj, Stack_t *, CopyRange_t *copyRange,
						range_t *from_range, range_t *from2_range, range_t *large_range);

void scanObj_copy1_copyCopySync_primaryStack(Proc_t *, ptr_t replicaGray, Stack_t *, CopyRange_t *copyRange,
					     range_t *from_range);
void scanObj_locCopy1_copyCopySync_primaryStack(Proc_t *, ptr_t replicaGray, Stack_t *, CopyRange_t *copyRange,
						range_t *from_range);
void transferScanObj_locCopy1_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, Stack_t *, CopyRange_t *copyRange,
							range_t *from_range);
void transferScanObj_locCopy2_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, Stack_t *, CopyRange_t *copyRange,
							range_t *from_range, range_t *from2_range, range_t *large_range);

void transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, Stack_t *, CopyRange_t *copyRange,
								       range_t *from_range);
void transferScanObj_copyWriteSync_locAlloc2_copyCopySync_primaryStack(Proc_t *, ptr_t primaryGray, Stack_t *, CopyRange_t *copyRange,
								       range_t *from_range, range_t *from2_range, range_t *large_range);



#endif



