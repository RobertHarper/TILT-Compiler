/*  This file must be consistent with the compiler's notion of tags.
    Compare with the file Rtl/tags.sml 
*/
#ifndef _tag_h
#define _tag_h

#include "general.h"

/* value_t is to be used for the type of a pointer */
#ifndef _asm_
#ifdef alpha_osf
#pragma pointer_size save
#pragma pointer_size 32
#endif
typedef int bool_t;
typedef unsigned int ui_t;
typedef unsigned long reg_t; /* The machine register's actual type */
typedef ui_t  val_t;        /* An ML value that may or may nor be a pointer. */
typedef ui_t *ptr_t;        /* An ML value that has a pointer type.
                                It might be a small-valued constructor (<= 256),
			        points to global data (in the data segment),
			        or is in the heap(s). */
typedef volatile ui_t *vptr_t;
typedef val_t *loc_t;        /* A location which contains an ML value.
				External information is needed to determine whether
				the location contains a pointer value or not.
				Possible locations are stack slots,
				memory locations reflecting register contents,
				locations inside ML objects in the data segment
				or in the heap.
			      */
typedef ptr_t *ploc_t;        /* A location which contains an ML pointer */
typedef unsigned int  tag_t;  /* An ML tag */
typedef unsigned int *mem_t;  /* A memory address into the stack and heap.
				 There is not necessarily a object at the location. */
#ifdef alpha_osf
#pragma pointer_size restore
#endif
#endif

#define uninit_val       258

/* Note that 0x0 and 0x4 cannot be used as those correspond to possible pointer values */
#define FORWARD1_TYPE          0x0
#define FORWARD2_TYPE          0x4
#define RECORD_TYPE            0x1
#define WORD_ARRAY_TYPE        0x2
#define QUAD_ARRAY_TYPE        0x3 /* Quad aligned and quad accessed */
#define PTR_ARRAY_TYPE         0x5
#define MIRROR_PTR_ARRAY_TYPE  0x6
#define OTHER_TYPE             0x7

/* Of the other tags, fourth bit zero is reserved for skip tags */
#define IS_SKIP_TAG(t)          ((((tag_t)t) & 15) == 0x7)
#define IS_NONSKIP_OTHER_TAG(t) ((((tag_t)t) & 15) == 0xf)
#define MAKE_SKIP(w)          ((w << 4) | 0x7)
#define GET_SKIPWORD(t)       (((tag_t)t) >> 4)
#define NONSKIP_OTHER_TYPE    0xf
#define STALL_TAG             (NONSKIP_OTHER_TYPE | (0x0 << 5))
#define SEGSTALL_TAG          (NONSKIP_OTHER_TYPE | (0x1 << 5))   /* Is different from STALL_TAG so object is parsable starting at first tag */
#define SEGPROCEED_TAG        (NONSKIP_OTHER_TYPE | (0x2 << 5))
#define MIRROR_GLOBAL_PTR_TAG (NONSKIP_OTHER_TYPE | (0x3 << 5))

#define GET_TYPE(t)        (((tag_t)t) & 0x7)
#define TYPE_IS_FORWARD(t) ((t) == FORWARD1_TYPE || (t) == FORWARD2_TYPE)
#define TYPE_IS_ARRAY(t)   ((t) == WORD_ARRAY_TYPE || (t) == QUAD_ARRAY_TYPE || (t) == PTR_ARRAY_TYPE || (t) == MIRROR_PTR_ARRAY_TYPE)
#define TAG_IS_FORWARD(t)  ((((tag_t)t) & 0x3) == 0)
#define TAG_IS_OTHER(t)    ((((tag_t)t) & 0x7) == OTHER_TYPE)

/* Records are not allowed to be empty */
#define RECLEN_OFFSET    3
#define RECLEN_MAX       24
#define RECMASK_OFFSET   8
#define MAKE_REC(len,mask) ((len << RECLEN_OFFSET) | (mask << RECMASK_OFFSET) | RECORD_TYPE)
#define GET_RECLEN(t)      ((((tag_t)t) >> RECLEN_OFFSET) & 31)    /* rec len in words */
#define GET_RECMASK(t)     (((tag_t)t) >> RECMASK_OFFSET)          /* get record mask */


#define ARRLEN_OFFSET    3
#define GET_WORD_ARRAY_LEN(t)       (((tag_t)t) >> ARRLEN_OFFSET)  /* array length in bytes - includes char array */
#define GET_QUAD_ARRAY_LEN(t)       (((tag_t)t) >> ARRLEN_OFFSET)  /* array length in bytes - mult of 8 */
#define GET_PTR_ARRAY_LEN(t)        (((tag_t)t) >> ARRLEN_OFFSET)  /* array length in bytes - mult of 4 */
#define GET_MIRROR_PTR_ARRAY_LEN(t) (((tag_t)t) >> ARRLEN_OFFSET)  /* array length in bytes - mult of 8 */
#define GET_ANY_ARRAY_LEN(t)        (((tag_t)t) >> ARRLEN_OFFSET)  /* array length in bytes */

/* Records are not allowed to be empty */
#define TAG_REC_INT        (RECORD_TYPE | (1 << RECLEN_OFFSET) | (0 << RECMASK_OFFSET))
#define TAG_REC_TRACE      (RECORD_TYPE | (1 << RECLEN_OFFSET) | (1 << RECMASK_OFFSET))
#define TAG_REC_INTINT     (RECORD_TYPE | (2 << RECLEN_OFFSET) | (0 << RECMASK_OFFSET))
#define TAG_REC_TRACEINT   (RECORD_TYPE | (2 << RECLEN_OFFSET) | (1 << RECMASK_OFFSET))
#define TAG_REC_INTTRACE   (RECORD_TYPE | (2 << RECLEN_OFFSET) | (2 << RECMASK_OFFSET))
#define TAG_REC_TRACETRACE (RECORD_TYPE | (2 << RECLEN_OFFSET) | (3 << RECMASK_OFFSET))

INLINE(getTag)
tag_t getTag(vptr_t obj)
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

#endif /* _tag_h */

