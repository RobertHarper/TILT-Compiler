/*  This file must be consistent with the compiler's notion of tags.
    Compare with the file Rtl/tags.sml 
*/
#ifndef _tag_h
#define _tag_h

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

/* Note that 0x0 and 0x4 cannot be used as those are possible pointer values 
   and that position of that tag might be occupied by a forwarding pointer */
#define FORWARD1_TYPE    0x0
#define FORWARD2_TYPE    0x4
#define RECORD_TYPE      0x1
#define IARRAY_TYPE      0x2
#define PARRAY_TYPE      0x3
#define RARRAY_TYPE      0x5
#define SKIP_TYPE        0x6
#define OTHER_TYPE       0x7

#define STALL_TAG (OTHER_TYPE | (0x0 << 3))
#define SEGSTALL_TAG (OTHER_TYPE | (0x1 << 3))   /* Is different from STALL_TAG so object is parsable starting at first tag */
#define SEGPROCEED_TAG (OTHER_TYPE | (0x2 << 3))

#define TAG_IS_FORWARD(t)  ((((tag_t)t) & 0x3) == 0)
#define TAG_IS_OTHER(t)    ((((tag_t)t) & 0x7) == OTHER_TYPE)

#define GET_TYPE(t)        (((tag_t)t) & 0x7)
#define TYPE_IS_ARRAY(t)   ((t) == IARRAY_TYPE) || ((t) == PARRAY_TYPE) || ((t) == RARRAY_TYPE)
#define TYPE_IS_FORWARD(t) ((t) == FORWARD1_TYPE || (t) == FORWARD2_TYPE)

#define GET_OTHER_INFO(t) (((tag_t)t) >> 3)

#define SKIPLEN_OFFSET   3 /* offset storing number of words to skip */
#define GET_SKIP(t)       (((tag_t)t) >> SKIPLEN_OFFSET)

#define ARRLEN_OFFSET    3
#define GET_ARRLEN(t)    (((tag_t)t) >> ARRLEN_OFFSET) /* array length in bytes */
#define GET_ARRAY_AGE(t) ((((tag_t)t) >> 3) & 3)       /* XXX not sure this is kept */

#define RECLEN_OFFSET    3
#define RECLEN_MAX       24
#define RECMASK_OFFSET   8
#define GET_RECLEN(t)    ((((tag_t)t) >> RECLEN_OFFSET) & 31)    /* rec len in words */
#define GET_RECMASK(t)   (((tag_t)t) >> RECMASK_OFFSET)          /* get record mask */

/* Records are not allowed to be empty */
#define TAG_REC_INT        (RECORD_TYPE | (1 << RECLEN_OFFSET) | (0 << RECMASK_OFFSET))
#define TAG_REC_TRACE      (RECORD_TYPE | (1 << RECLEN_OFFSET) | (1 << RECMASK_OFFSET))
#define TAG_REC_INTINT     (RECORD_TYPE | (2 << RECLEN_OFFSET) | (0 << RECMASK_OFFSET))
#define TAG_REC_TRACEINT   (RECORD_TYPE | (2 << RECLEN_OFFSET) | (1 << RECMASK_OFFSET))
#define TAG_REC_INTTRACE   (RECORD_TYPE | (2 << RECLEN_OFFSET) | (2 << RECMASK_OFFSET))
#define TAG_REC_TRACETRACE (RECORD_TYPE | (2 << RECLEN_OFFSET) | (3 << RECMASK_OFFSET))


#define BUG(x) {printf(x); exit(-1); }

#endif /* _tag_h */

