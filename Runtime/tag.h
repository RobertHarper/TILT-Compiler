#ifndef _tag_h
#define _tag_h

#define MAX_RECORDLEN 24
#define RECORD_TAG  0x0
#define RECORD_SUB_TAG  0x1
#define SKIP_TAG  0x2
#define IARRAY_TAG  0x4
#define RARRAY_TAG  0x5
#define ARRAY_TAG   0x6
#define FORWARD_TAG 0x7

#define GET_TYPE(t)       (((unsigned int)t) & 0x7)
#define POSSLEN_SHIFT     3
#define GET_POSSLEN(t)    (((unsigned int)t) >> POSSLEN_SHIFT)        /* array length in bytes */
#define GET_ARRAY_AGE(t)  ((((unsigned int)t) >> 3) & 3)
#define GET_RECLEN(t)     (((unsigned int)t) >> 27)
#define GET_RECMASK(t)    ((((unsigned int)t) << 5) >> 8)

#define IS_RECORD(t)      (GET_TYPE(t) == RECORD_TAG)
#define IS_RECORD_SUB(t)  (GET_TYPE(t) == RECORD_SUB_TAG)
#define IS_SKIP(t)        (GET_TYPE(t) == SKIP_TAG)
#define IS_IARRAY(t)      (GET_TYPE(t) == IARRAY_TAG)
#define IS_RARRAY(t)      (GET_TYPE(t) == RARRAY_TAG)
#define IS_ARRAY(t)       (GET_TYPE(t) == ARRAY_TAG)
#define IS_FORWARD(t)     (GET_TYPE(t) == FORWARD_TAG)

#define TAG_REC_EMPTY      (RECORD_TAG | (0 << 27) | (0 << 3))
#define TAG_REC_INT        (RECORD_TAG | (1 << 27) | (0 << 3))
#define TAG_REC_TRACE      (RECORD_TAG | (1 << 27) | (1 << 3))
#define TAG_REC_INTINT     (RECORD_TAG | (2 << 27) | (0 << 3))
#define TAG_REC_TRACETRACE (RECORD_TAG | (2 << 27) | (3 << 3))
#define TAG_REC_INTTRACE   (RECORD_TAG | (2 << 27) | (2 << 3))
#define TAG_REC_TRACEINT   (RECORD_TAG | (2 << 27) | (1 << 3))


typedef unsigned int value_t;

/* careful with macro since pointers get turned into ints not longs */
static int POINTER_IN_RANGE(a,start,finish)
{
 return ((long)(start)<(long)(a) && (long)(a)<(long)(finish));
}

#define POINTER_ISNIL(a) (a == 0)
#define BUG(x) {printf(x); exit(-1); }


#endif /* _tag_h */

