#ifndef _scan_h
#define _scan_h

/* ------------------- Scanning Routines ---------------------- */
typedef enum LocAllocCopy__t {Copy, LocCopy, LocAlloc} LocAllocCopy_t;
typedef enum SourceSpaceCheck__t {OneSpace, OneSpaceLarge, TwoSpaceLarge} SourceSpaceCheck_t;
typedef enum SpaceCheck__t {DoSpaceCheck, NoSpaceCheck} SpaceCheck_t;
typedef enum Transfer__t {NoTransfer, Transfer, SelfTransfer, BackTransfer} Transfer_t;
typedef enum StackType__t {NoSet, PrimarySet, ReplicaSet} StackType_t;
typedef enum CopyWrite__t {NoCopyWrite, DoCopyWrite} CopyWrite_t;
typedef enum CopyCopy__t {NoCopyCopy, DoCopyCopy} CopyCopy_t;


/* The generic scanning function with compile-time parameters.
   primaryOrReplicaGray - The primary or replica gray object is passed in
   start, end - if start <> end, large object is indicated; in bytes
   Transfer - if NoTransfer, object is replica.  Primary is unknown.
              if Transfer, object is primary.  Replica fields are uninitialized.
              if BackTransfer, object is replica.  Replica fields are uninitialized.  First field of replica is a backpointer to primary.
	      if SelfTransfer, object is both primary and replica (MIRROR_PTR).  Replica fields uninitialized.
   localStack - stack for holding primary or replica gray objects
   localSegmentStack - stack for holding primary large object segments
   copyRange - where new objects are allocated rfom
   from_range, from2_range, large_range - determines if an object is primary
   sourceSpaceCheck - forward if from given space(s) and/or large space
   doCopyWrite - copy-write synchronization; if true, must also have mayTransfer
*/



/* --------------- Scanning routines ----------------------- */
INLINE(doField)
void doField(Proc_t *proc, ploc_t field, 
	     Heap_t *from_range, Heap_t *from2_range, Heap_t *large_range,
	     CopyCopy_t copyCopy, CopyWrite_t copyWrite, 
	     LocAllocCopy_t locAllocCopy, SourceSpaceCheck_t sourceSpaceCheck, 
	     StackType_t stackType, SpaceCheck_t spaceCheck)
{
  switch (locAllocCopy) {
  case LocAlloc:
    assert(copyCopy == DoCopyCopy);
    assert(spaceCheck == DoSpaceCheck);
    switch (sourceSpaceCheck) {
    case OneSpace:
      if (stackType == PrimarySet)
	locAlloc1_copyCopySync_primarySet(proc,field,from_range);
      else if (stackType == ReplicaSet)
	locAlloc1_copyCopySync_replicaSet(proc,field,from_range);
      else if (stackType == NoSet)
	locAlloc1_copyCopySync(proc,field,from_range);
      else
	assert(0);
      break;
    case OneSpaceLarge:
      if (stackType == PrimarySet)
	locAlloc1L_copyCopySync_primarySet(proc,field,from_range, large_range);
      else 
	assert(0);
      break;
    case TwoSpaceLarge:
      if (stackType == PrimarySet)
	locAlloc2L_copyCopySync_primarySet(proc,field,from_range,from2_range,large_range);
      else 
	assert(0);
      break;
    default : 
      assert(0);
    }
    break;
  case LocCopy:
    switch (sourceSpaceCheck) {
    case OneSpace: 
      if (stackType == NoSet) {
	if (copyCopy == NoCopyCopy) {
	  if (spaceCheck == NoSpaceCheck)
	    locCopy1_noSpaceCheck(proc,field,from_range); 
	  else
	    locCopy1(proc,field,from_range); 
	}
	else {
	  assert(copyCopy == DoCopyCopy);
	  assert(spaceCheck == DoSpaceCheck);
	  locCopy1_copyCopySync(proc,field,from_range); 
	}
      }
      else if (stackType == PrimarySet) {
	assert(copyCopy == DoCopyCopy);
	assert(spaceCheck == DoSpaceCheck);
	locCopy1_copyCopySync_primarySet(proc,field,from_range); 
      }
      else if (stackType == ReplicaSet) {
	if (spaceCheck == DoSpaceCheck) {
	  if (copyCopy == DoCopyCopy)
	    locCopy1_copyCopySync_replicaSet(proc,field,from_range); 
	  else
	    locCopy1_replicaSet(proc,field,from_range); 
	}
	else {
	  assert(spaceCheck == NoSpaceCheck);
	  if (copyCopy == DoCopyCopy)
	    locCopy1_noSpaceCheck_copyCopySync_replicaSet(proc,field,from_range); 
	  else
	    locCopy1_noSpaceCheck_replicaSet(proc,field,from_range); 
	}
      }
      else
	assert(0);
      break;
    case OneSpaceLarge: 
      assert(copyCopy == DoCopyCopy);
      assert(stackType == PrimarySet);
      assert(spaceCheck == DoSpaceCheck);
      locCopy1L_copyCopySync_primarySet(proc,field,from_range,large_range); 
      break;
    case TwoSpaceLarge: 
      if (stackType == PrimarySet) {
	assert(spaceCheck == DoSpaceCheck);
	assert(copyCopy == DoCopyCopy);
	locCopy2L_copyCopySync_primarySet(proc,field,from_range,from2_range,large_range); 
      }
      else if (stackType == ReplicaSet) {
	assert(spaceCheck == DoSpaceCheck);
	assert(copyCopy == DoCopyCopy);
	locCopy2L_copyCopySync_replicaSet(proc,field,from_range,from2_range,large_range); 
      } else {
	assert(stackType == NoSet);
	assert(spaceCheck == NoSpaceCheck);
	assert(copyCopy == NoCopyCopy);
	locCopy2L_noSpaceCheck(proc,field,from_range,from2_range,large_range); 
      }
      break;
    default: assert(0);
    }
    break;
  case Copy:
    assert(copyCopy == DoCopyCopy);
    assert(stackType == PrimarySet);
    assert(spaceCheck == DoSpaceCheck);
    switch (sourceSpaceCheck) {
      case OneSpace: copy1_copyCopySync_primarySet(proc,*field,from_range); break;
      default: assert(0);
    }
    break;
  } /* switch locAllocCopy */
}



INLINE(genericScan)
ptr_t genericScan(Proc_t *proc, 
		 ptr_t primaryOrReplicaGray, int start, int end,
		 Heap_t *from_range, Heap_t *from2_range, Heap_t *large_range,
		 CopyCopy_t copyCopy, CopyWrite_t copyWrite, 
		 LocAllocCopy_t locAllocCopy, SourceSpaceCheck_t sourceSpaceCheck, 
		 Transfer_t transfer, StackType_t stackType, SpaceCheck_t spaceCheck)
{
  /* If performing transfer, we are given primaryGray and must copy fields into replicaGray */
  ptr_t primaryGray, replicaGray;
  tag_t tag;
  int type;
  int i, discard;
  int largeArrayByteLen; /* For breakUpArray */

  /* A backpointer is stored in the first field of the replica object whenever alloc (not copy) was previously
     used.  In that case, either Transfer or BackTransfer will now be used.  A backpointer is not present
     only if the replica object is not really there (skip tag) or is an empty array.  In any case,
     the backpointer must be deleted prior to any field is copied, thus ensuring that the write barrier
     properly performs updates of fields that have been processed.  Note that the write barrier omits
     objects which have not been processed to avoid deleting the backpointer.  

     For small objects, fields are scanned in order starting with the first field so no special action needs
     to be taken as the backpointer is deleted before the fields are processed.  However, for large objects
     the fields are scanned in parallel so the backpointer must be explicitly deleted.
  */
  switch (transfer) {
    case NoTransfer:
      primaryGray = NULL;
      replicaGray = primaryOrReplicaGray;
      tag = replicaGray[-1];
      type = GET_TYPE(tag);
      break;
    case Transfer:
      primaryGray = primaryOrReplicaGray;
      replicaGray = (ptr_t) primaryGray[-1];
      tag = replicaGray[-1];
      type = GET_TYPE(tag);
      break;
    case SelfTransfer:
      primaryGray = primaryOrReplicaGray;
      replicaGray = primaryOrReplicaGray;
      tag = replicaGray[-1];
      type = GET_TYPE(tag);
      break;
    case BackTransfer:
      replicaGray = primaryOrReplicaGray;
      tag = replicaGray[-1];
      primaryGray = (ptr_t) replicaGray[0];  /* Ill-defined when replica is skip tag or empty array */
      type = GET_TYPE(tag);

      fastAssert(IS_SKIP_TAG(tag) ||
		 (TYPE_IS_ARRAY(type) && (GET_ANY_ARRAY_LEN(tag) == 0)) ||
		 (primaryGray[-1] == replicaGray));   /* Does forwarding pointer agree? */
      break;
  }

  if (copyWrite == DoCopyWrite)
    assert(transfer != NoTransfer);
  proc->segUsage.objsScanned++;
  while (1) {
    if (type == RECORD_TYPE) {                      /* Records are the most common case - no copy-write sync needed */
      unsigned int mask = GET_RECMASK(tag);
      int fieldLen = GET_RECLEN(tag);
      int ptrFields = 0;

      for (i=0; i<fieldLen; i++, mask >>= 1) {
	if (transfer == Transfer || transfer == BackTransfer)
	  replicaGray[i] = primaryGray[i];
	if (mask & 1) {
	  ptrFields++;
	  doField(proc, (ploc_t) replicaGray + i, 
		  from_range, from2_range, large_range,
		  copyCopy, copyWrite, locAllocCopy, sourceSpaceCheck, 
		  stackType, spaceCheck);
	}
      }
      proc->segUsage.fieldsScanned += fieldLen - ptrFields;
      proc->segUsage.ptrFieldsScanned += ptrFields;
      return primaryOrReplicaGray + fieldLen;
    } 

    else if (type == WORD_ARRAY_TYPE || type == QUAD_ARRAY_TYPE) {
      int arrayByteLen = GET_ANY_ARRAY_LEN(tag);  
      int fieldLen = DivideUp(arrayByteLen, 4);         /* Length for word array might not be multiple of 4 */
      int byteLen = 4 * fieldLen;                  
      int large =  (arraySegmentSize > 0) && (byteLen > arraySegmentSize);
      int syncIndex = large ? -(2+DivideUp(start,arraySegmentSize)) : -1; /* Use normal tag or segment tag */
      int firstField = 0, lastField = fieldLen;

      if (large) {
	if (start == end) {
	  largeArrayByteLen = byteLen;
	  goto breakUpArray;
	}
	else {
	  firstField = start / sizeof(val_t);
	  lastField =  end / sizeof(val_t);
	  fieldLen = lastField - firstField;
	}
      }

      /* primaryGray is not well-defined if array is empty and transfer is BackTransfer */
      if (fieldLen > 0 && (transfer == Transfer || transfer == BackTransfer)) {
	if (copyWrite == DoCopyWrite) 
	  replicaGray[syncIndex] = large ? SEGSTALL_TAG : STALL_TAG;
	memcpy((char *) &(replicaGray[firstField]), (const char *) &(primaryGray[firstField]), sizeof(val_t) * (lastField - firstField)); 
	proc->segUsage.fieldsScanned += lastField - firstField;
	if (lastField - firstField > 32) 
	  updateWorkDone(proc);
	if (copyWrite == DoCopyWrite) 
	  replicaGray[syncIndex] = large ? SEGPROCEED_TAG : (val_t) tag;
      }

      return primaryOrReplicaGray + fieldLen;
    }

    else if (type == PTR_ARRAY_TYPE || type == MIRROR_PTR_ARRAY_TYPE) {
      int arrayByteLen = GET_ANY_ARRAY_LEN(tag);  
      int fieldLen = arrayByteLen / 4;             /* Ptr and MirrorPtr arrays are always in multiple of 4 */
      int byteLen = arrayByteLen;
      int large =  (arraySegmentSize > 0) && (byteLen > arraySegmentSize);
      int syncIndex = large ? -(2+DivideUp(start,arraySegmentSize)) : -1; /* Use normal tag or segment tag */
      int firstField = 0, lastField = fieldLen;

      if (large) {
	if (start == end) {
	  largeArrayByteLen = byteLen;
	  goto breakUpArray;
	}
	else {
	  firstField = start / sizeof(val_t);
	  lastField =  end / sizeof(val_t);
	  fieldLen = lastField - firstField;
	}
      }
      
      /* primaryGray is not well-defined if array is empty and transfer is BackTransfer */
      if (fieldLen > 0) {
	if (copyWrite == DoCopyWrite) 
	  replicaGray[syncIndex] = large ? SEGSTALL_TAG : STALL_TAG;
	if (type == PTR_ARRAY_TYPE) {
	  for (i=firstField; i<lastField; i ++) {
	    ploc_t primaryField = (ploc_t) &primaryGray[i];
	    ploc_t replicaField = (ploc_t) &replicaGray[i];
	    if (transfer == Transfer || transfer == BackTransfer)
	      *replicaField = *primaryField;
	    /* Check is needed for objects that point to themselves - otherwise, loop */
	    if (copyWrite == DoCopyWrite && (ptr_t) *primaryField == primaryGray) { 
	      *replicaField = (ptr_t) replicaGray;
	      continue;
	    }
	    doField(proc, (ploc_t) replicaField, 
		    from_range, from2_range, large_range,
		    copyCopy, copyWrite, locAllocCopy, sourceSpaceCheck, 
		    stackType, spaceCheck);
	  } /* for */
	}
	else /* type == MIRROR_PTR_ARRAY_TYPE */ {
	  int primaryOffset = primaryArrayOffset;
	  int replicaOffset = replicaArrayOffset;
	  
	  for (i=firstField; i<lastField; i += 2) {
	    ploc_t primaryField = (ploc_t) &primaryGray[i + primaryOffset / sizeof(val_t)];
	    ploc_t replicaField = (ploc_t) &replicaGray[i + replicaOffset / sizeof(val_t)];
	    
	    if ((transfer == Transfer) || (transfer == BackTransfer) || (transfer == SelfTransfer)) {
	      ploc_t field3 = (ploc_t) &primaryGray[i + replicaOffset / sizeof(val_t)];  /* these allow paranoid check to work */
	      ploc_t field4 = (ploc_t) &replicaGray[i + primaryOffset / sizeof(val_t)];
	      *replicaField = *primaryField;
	      *field3 = *primaryField;
	      *field4 = *primaryField;
	    }
	    /* Check is needed for objects that point to themselves - otherwise, loop */
	    if (copyWrite == DoCopyWrite && (ptr_t) *primaryField == primaryGray) { 
	      *replicaField = (ptr_t) replicaGray;
	      continue;
	    }
	    doField(proc, (ploc_t) replicaField, 
		    from_range, from2_range, large_range,
		    copyCopy, copyWrite, locAllocCopy, sourceSpaceCheck, 
		    stackType, spaceCheck);
	    /* Both fields must be up-to-date */
	    if (transfer == Transfer || transfer == BackTransfer)
	      replicaGray[i + primaryOffset / sizeof(val_t)] = (val_t) (*replicaField);
	  } /* for */
	} /* if */
      }
      proc->segUsage.ptrFieldsScanned += fieldLen;
      if (copyWrite == DoCopyWrite) 
	replicaGray[syncIndex] = large ? SEGPROCEED_TAG : tag;
      if (fieldLen > 32) 
	updateWorkDone(proc);
      return primaryOrReplicaGray + fieldLen;
    }
    else if (TYPE_IS_FORWARD(type)) {
      tag = ((ptr_t)tag)[-1];
      continue;
    }
    else if (IS_SKIP_TAG(tag)) {
      proc->segUsage.fieldsScanned += 1;
      return primaryOrReplicaGray + (GET_SKIPWORD(tag) - 1);
    }
    else {
      while (tag == STALL_TAG) {
	memBarrier();
	tag = replicaGray[-1];
      }
      continue;
    }
  }
  assert(0);
  breakUpArray:    /* If a large array object, convert into segments */
  {
    int segments = DivideUp(largeArrayByteLen, arraySegmentSize);
    assert(primaryGray != NULL);
    replicaGray[0] = NULL;    /* Clear out backpointer */
    for (i=0; i<segments; i++) {
      int start = i * arraySegmentSize;
      int end = start + arraySegmentSize;
      if (end > largeArrayByteLen)
	end = largeArrayByteLen;
      SetPush3(&proc->work.segments, (ptr_t) primaryGray, (ptr_t) start, (ptr_t) end);
    }
    return primaryOrReplicaGray + (largeArrayByteLen / sizeof(val_t));
  }
}


/* scanUntil_locCopy?_noSpaceCheck - Standard cheney scan for stop and copy collectors
   scanObj_locCopy?_copyCopySync_replicaSet - Parallel but not concurrent collectors
   transferScanObj_locCopy?_copyCopySync_primarySet - Parallel but not concurrent collectors
   transferScanObj_copyWriteSync_locCopy?_copyCopySync_primarySet - Concurrent collectors
*/

INLINE(scanTag_locCopy1_noSpaceCheck)
mem_t scanTag_locCopy1_noSpaceCheck(Proc_t *proc, mem_t start, Heap_t *fromRange)
{
  ptr_t replicaGray = start + 1;
  return genericScan(proc, replicaGray, 0, 0, 
		     fromRange, NULL, NULL,
		     NoCopyCopy, NoCopyWrite,
		     LocCopy, OneSpace,
		     NoTransfer, NoSet, NoSpaceCheck);
}

INLINE(scanTag_locCopy1)
mem_t scanTag_locCopy1(Proc_t *proc, mem_t start, Heap_t *fromRange)
{
  ptr_t replicaGray = start + 1;
  return genericScan(proc, replicaGray, 0, 0, 
		     fromRange, NULL, NULL,
		     NoCopyCopy, NoCopyWrite,
		     LocCopy, OneSpace,
		     NoTransfer, NoSet, DoSpaceCheck);
}


INLINE(scanTag_locCopy1_replicaSet)
mem_t scanTag_locCopy1_replicaSet(Proc_t *proc, mem_t start, Heap_t *fromRange)
{
  ptr_t replicaGray = start + 1;
  return genericScan(proc, replicaGray, 0, 0, 
		     fromRange, NULL, NULL,
		     NoCopyCopy, NoCopyWrite,
		     LocCopy, OneSpace,
		     NoTransfer, ReplicaSet, DoSpaceCheck);
}

INLINE(scanTag_locCopy1_copyCopySync)
mem_t scanTag_locCopy1_copyCopySync(Proc_t *proc, mem_t start, Heap_t *fromRange)
{
  ptr_t replicaGray = start + 1;
  return genericScan(proc, replicaGray, 0, 0, 
		     fromRange, NULL, NULL,
		     DoCopyCopy, NoCopyWrite,
		     LocCopy, OneSpace,
		     NoTransfer, NoSet, DoSpaceCheck);
}

INLINE(scanTag_locCopy2L_noSpaceCheck)
mem_t scanTag_locCopy2L_noSpaceCheck(Proc_t *proc, mem_t start, 
					  Heap_t *from, Heap_t *from2, Heap_t *large)
{
  ptr_t replicaGray = start + 1;
  return genericScan(proc, replicaGray, 0, 0, 
		     from, from2, large,
		     NoCopyCopy, NoCopyWrite,
		     LocCopy, TwoSpaceLarge,
		     NoTransfer, NoSet, NoSpaceCheck);
}




INLINE(scanObj_locCopy1_noSpaceCheck)
void scanObj_locCopy1_noSpaceCheck(Proc_t *proc, ptr_t obj, Heap_t *from_range)
{
  scanTag_locCopy1_noSpaceCheck(proc, obj - 1, from_range);
}


INLINE(scanObj_locCopy1)
void scanObj_locCopy1(Proc_t *proc, ptr_t obj, Heap_t *from_range)
{
  scanTag_locCopy1(proc, obj - 1, from_range);
}

INLINE(scanUntil_locCopy1_noSpaceCheck)
void scanUntil_locCopy1_noSpaceCheck(Proc_t *proc, mem_t start_scan, 
				      Heap_t *from_range)
{
  mem_t cur = start_scan;
  CopyRange_t *copyRange = &proc->copyRange;
  while (cur < copyRange->cursor) {
    cur = scanTag_locCopy1_noSpaceCheck(proc, cur, from_range);
  }
  assert(cur == copyRange->cursor);
}

INLINE(scanUntil_locCopy1)
void scanUntil_locCopy1(Proc_t *proc, mem_t start_scan, 
			Heap_t *from_range)
{
  mem_t cur = start_scan;
  CopyRange_t *copyRange = &proc->copyRange;
  while (cur < copyRange->cursor) {
    cur = scanTag_locCopy1(proc, cur, from_range);
  }
  assert(cur == copyRange->cursor);
}


INLINE(scanRegion_locCopy1)
void scanRegion_locCopy1(Proc_t *proc, mem_t start_scan, mem_t stop_scan, 
			 Heap_t *from_range)
{
  mem_t cur = start_scan;
  while (cur < stop_scan) {
    cur = scanTag_locCopy1(proc, cur, from_range);
  }
  assert(cur == stop_scan);
}

INLINE(scanRegion_locCopy1_replicaSet)
void scanRegion_locCopy1_replicaSet(Proc_t *proc, mem_t start_scan, mem_t stop_scan, 
				    Heap_t *from_range)
{
  mem_t cur = start_scan;
  while (cur < stop_scan) {
    cur = scanTag_locCopy1_replicaSet(proc, cur, from_range);
  }
  assert(cur == stop_scan);
}

INLINE(scanRegion_locCopy1_copyCopySync)
void scanRegion_locCopy1_copyCopySync(Proc_t *proc, mem_t start_scan, mem_t stop_scan, 
			 Heap_t *from_range)
{
  mem_t cur = start_scan;
  while (cur < stop_scan) {
    cur = scanTag_locCopy1_copyCopySync(proc, cur, from_range);
  }
  assert(cur == stop_scan);
}

INLINE(scanUntil_locCopy2L_noSpaceCheck)
void scanUntil_locCopy2L_noSpaceCheck(Proc_t *proc, mem_t start_scan, 
				      Heap_t *from_range, Heap_t *from2_range, Heap_t *large)
{
  mem_t cur = start_scan;
  CopyRange_t *copyRange = &proc->copyRange;
  while (cur < copyRange->cursor) {
    cur = scanTag_locCopy2L_noSpaceCheck(proc, cur, from_range, from2_range, large);
  }
  assert(cur == copyRange->cursor);
}

INLINE(scanObj_copy1_copyCopySync_primarySet)
void scanObj_copy1_copyCopySync_primarySet(Proc_t *proc, ptr_t replicaGray, 
					     Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, NoCopyWrite, Copy, OneSpace, NoTransfer, PrimarySet, DoSpaceCheck);
}

INLINE(scanObj_locCopy1_copyCopySync_primarySet)
void scanObj_locCopy1_copyCopySync_primarySet(Proc_t *proc, ptr_t replicaGray,
					     Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, NoCopyWrite, LocCopy, OneSpace, NoTransfer, PrimarySet, DoSpaceCheck);
}

INLINE(scanObj_locCopy1L_copyCopySync_primarySet)
void scanObj_locCopy1L_copyCopySync_primarySet(Proc_t *proc, ptr_t replicaGray,
						 Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, large_range,
	      DoCopyCopy, NoCopyWrite, LocCopy, OneSpaceLarge, NoTransfer, PrimarySet, DoSpaceCheck);
}

INLINE(transferScanObj_locCopy1_copyCopySync_primarySet)
void transferScanObj_locCopy1_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray,
							Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, NoCopyWrite, LocCopy, OneSpace, Transfer, PrimarySet, DoSpaceCheck);
}


INLINE(scanObj_locCopy1_copyCopySync_replicaSet)
void scanObj_locCopy1_copyCopySync_replicaSet(Proc_t *proc, ptr_t replicaGray, 
						Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, NoCopyWrite, LocCopy, OneSpace, NoTransfer, ReplicaSet, DoSpaceCheck);
}

INLINE(scanObj_locCopy1_replicaSet)
void scanObj_locCopy1_replicaSet(Proc_t *proc, ptr_t replicaGray, 
						Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, NULL,
	      NoCopyCopy, NoCopyWrite, LocCopy, OneSpace, NoTransfer, ReplicaSet, DoSpaceCheck);
}

INLINE(scanObj_locCopy2L_copyCopySync_replicaSet)
void scanObj_locCopy2L_copyCopySync_replicaSet(Proc_t *proc, ptr_t replicaGray, 
						 Heap_t *nursery_range, Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      nursery_range, from_range, large_range,
	      DoCopyCopy, NoCopyWrite, LocCopy, TwoSpaceLarge, NoTransfer, ReplicaSet, DoSpaceCheck);
}


INLINE(scanObj_locCopy1_noSpaceCheck_copyCopySync_replicaSet)
void scanObj_locCopy1_noSpaceCheck_copyCopySync_replicaSet(Proc_t *proc, ptr_t replicaGray,
							     Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, NoCopyWrite, LocCopy, OneSpace, NoTransfer, ReplicaSet, NoSpaceCheck);
}

INLINE(scanObj_locCopy1_noSpaceCheck_replicaSet)
void scanObj_locCopy1_noSpaceCheck_replicaSet(Proc_t *proc, ptr_t replicaGray, 
							     Heap_t *from_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, NULL,
	      NoCopyCopy, NoCopyWrite, LocCopy, OneSpace, NoTransfer, ReplicaSet, NoSpaceCheck);
}


INLINE(selfTransferScanObj_locAlloc1_copyCopySync_primarySet)
void selfTransferScanObj_locAlloc1_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, 
							   Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, NoCopyWrite, LocAlloc, OneSpace, SelfTransfer, PrimarySet, DoSpaceCheck);
}

INLINE(transferScanObj_locCopy2L_copyCopySync_primarySet)
void transferScanObj_locCopy2L_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, 
							Heap_t *from_range, Heap_t *from2_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, 0, 0, 
	      from_range, from2_range, large_range,
	      DoCopyCopy, NoCopyWrite, LocCopy, TwoSpaceLarge, Transfer, PrimarySet, DoSpaceCheck);
}


INLINE(transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primarySet)
void transferScanObj_copyWriteSync_locAlloc1_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, 
								     Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, Transfer, PrimarySet, DoSpaceCheck);
}

INLINE(backTransferScanObj_copyWriteSync_locAlloc1_copyCopySync_replicaSet)
void backTransferScanObj_copyWriteSync_locAlloc1_copyCopySync_replicaSet(Proc_t *proc, ptr_t primaryGray, 
									 Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, BackTransfer, ReplicaSet, DoSpaceCheck);
}

INLINE(backTransferScanObj_copyWriteSync_locAlloc1_copyCopySync)
void backTransferScanObj_copyWriteSync_locAlloc1_copyCopySync(Proc_t *proc, ptr_t primaryGray, 
									 Heap_t *from_range)
{
  genericScan(proc, primaryGray, 0, 0, 
	      from_range, NULL, NULL,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, BackTransfer, NoSet, DoSpaceCheck);
}

INLINE(backTransferScanTag_copyWriteSync_locAlloc1_copyCopySync)
ptr_t backTransferScanTag_copyWriteSync_locAlloc1_copyCopySync(Proc_t *proc, mem_t start,
							       Heap_t *from_range)
{
  ptr_t replicaGray = start + 1;  
  tag_t tag = replicaGray[-1];
  while (tag == SEGPROCEED_TAG || tag == SEGSTALL_TAG)
    tag = *(replicaGray++);
  return genericScan(proc, replicaGray, 0, 0, 
		     from_range, NULL, NULL,
		     DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, BackTransfer, NoSet, DoSpaceCheck);
}

INLINE(backTransferScanRegion_copyWriteSync_locAlloc1_copyCopySync)
void backTransferScanRegion_copyWriteSync_locAlloc1_copyCopySync(Proc_t *proc, mem_t start_scan, mem_t stop_scan, 
								 Heap_t *from_range)
{
  mem_t cur = start_scan;
  while (cur < stop_scan) {
    cur = backTransferScanTag_copyWriteSync_locAlloc1_copyCopySync(proc, cur, from_range);
  }
  assert(cur == stop_scan);
}

INLINE(transferScanObj_copyWriteSync_locAlloc1L_copyCopySync_primarySet)
void transferScanObj_copyWriteSync_locAlloc1L_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, 
								      Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, 0, 0, 
	      from_range, NULL, large_range,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpaceLarge, Transfer, PrimarySet, DoSpaceCheck);
}

INLINE(scanObj_copyWriteSync_locCopy1_copyCopySync_replicaSet)
void scanObj_copyWriteSync_locCopy1_copyCopySync_replicaSet(Proc_t *proc, ptr_t replicaGray, 
							    Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, replicaGray, 0, 0, 
	      from_range, NULL, large_range,
	      DoCopyCopy, DoCopyWrite, LocCopy, OneSpaceLarge, NoTransfer, ReplicaSet, DoSpaceCheck);
}

INLINE(transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primarySet)
void transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, int start, int end,
									 Heap_t *from_range)
{
  genericScan(proc, primaryGray, start, end, 
	      from_range, NULL, NULL,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, Transfer, PrimarySet, DoSpaceCheck);
}

INLINE(transferScanSegment_copyWriteSync_locAlloc1_copyCopySync)
void transferScanSegment_copyWriteSync_locAlloc1_copyCopySync(Proc_t *proc, ptr_t primaryGray, int start, int end,
							      Heap_t *from_range)
{
  genericScan(proc, primaryGray, start, end, 
	      from_range, NULL, NULL,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, Transfer, NoSet, DoSpaceCheck);
}

INLINE(transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_replicaSet)
void transferScanSegment_copyWriteSync_locAlloc1_copyCopySync_replicaSet(Proc_t *proc, ptr_t primaryGray, int start, int end,
									 Heap_t *from_range)
{
  genericScan(proc, primaryGray, start, end, 
	      from_range, NULL, NULL,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, Transfer, ReplicaSet, DoSpaceCheck);
}


INLINE(selfTransferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primarySet)
void selfTransferScanSegment_copyWriteSync_locAlloc1_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, int start, int end,
									     Heap_t *from_range)
{
  genericScan(proc, primaryGray, start, end, 
	      from_range, NULL, NULL,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, SelfTransfer, PrimarySet, DoSpaceCheck);
}



INLINE(transferScanSegment_copyWriteSync_locAlloc1L_copyCopySync_primarySet)
void transferScanSegment_copyWriteSync_locAlloc1L_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, int start, int end,
										 Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, start, end,
	      from_range, NULL, large_range,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, Transfer, PrimarySet, DoSpaceCheck);
}



INLINE(selfTransferScanSegment_copyWriteSync_locAlloc1L_copyCopySync_primarySet)
void selfTransferScanSegment_copyWriteSync_locAlloc1L_copyCopySync_primarySet(Proc_t *proc, ptr_t primaryGray, int start, int end,
									      Heap_t *from_range, Heap_t *large_range)
{
  genericScan(proc, primaryGray, start, end,
	      from_range, NULL, large_range,
	      DoCopyCopy, DoCopyWrite, LocAlloc, OneSpace, SelfTransfer, PrimarySet, DoSpaceCheck);
}



#endif



