/*  This file must be consistent with the compiler's notion of tags.
    Compare with the file Rtl/tags.sml 
*/
#ifndef _tag_h
#define _tag_h

/* value_t is to be used for the type of a pointer */
#ifndef _asm_
typedef unsigned int value_t;
#endif

#define RECORD_TAG      0x0
#define IARRAY_TAG      0x2
#define PARRAY_TAG      0x3
#define RARRAY_TAG      0x4
#define SKIP_TAG        0x5
#define FORWARD_TAG     0x6

#define GET_TYPE(t)       (((value_t)t) & 0x7)
#define IS_RECORD(t)      (GET_TYPE(t) == RECORD_TAG)
#define IS_IARRAY(t)      (GET_TYPE(t) == IARRAY_TAG)
#define IS_PARRAY(t)      (GET_TYPE(t) == PARRAY_TAG)
#define IS_RARRAY(t)      (GET_TYPE(t) == RARRAY_TAG)
#define IS_SKIP(t)        (GET_TYPE(t) == SKIP_TAG)
#define IS_FORWARD(t)     (GET_TYPE(t) == FORWARD_TAG)

#define ARRLEN_OFFSET    3
#define GET_ARRLEN(t)    (((value_t)t) >> ARRLEN_OFFSET) /* array length in bytes */
#define GET_ARRAY_AGE(t) ((((value_t)t) >> 3) & 3)       /* XXX not sure this is kept */

#define RECLEN_OFFSET    3
#define RECLEN_MAX       24
#define RECMASK_OFFSET   8
#define GET_RECLEN(t)    ((((value_t)t) >> RECLEN_OFFSET) & 31)    /* rec len in words */
#define GET_RECMASK(t)   (((value_t)t) >> RECMASK_OFFSET)          /* get record mask */

/* Records are not allowed to be empty */
#define TAG_REC_INT        (RECORD_TAG | (1 << RECLEN_OFFSET) | (0 << RECMASK_OFFSET))
#define TAG_REC_TRACE      (RECORD_TAG | (1 << RECLEN_OFFSET) | (1 << RECMASK_OFFSET))
#define TAG_REC_INTINT     (RECORD_TAG | (2 << RECLEN_OFFSET) | (0 << RECMASK_OFFSET))
#define TAG_REC_TRACEINT   (RECORD_TAG | (2 << RECLEN_OFFSET) | (1 << RECMASK_OFFSET))
#define TAG_REC_INTTRACE   (RECORD_TAG | (2 << RECLEN_OFFSET) | (2 << RECMASK_OFFSET))
#define TAG_REC_TRACETRACE (RECORD_TAG | (2 << RECLEN_OFFSET) | (3 << RECMASK_OFFSET))


#define BUG(x) {printf(x); exit(-1); }

#endif /* _tag_h */

