#include "tag.h"
#include "queue.h"
#include "stack.h"
#include "hash.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <values.h>
#include "general.h"
#include "stats.h"

#define DEBUG
#ifdef DEBUG
#include "memobj.h"
extern HeapObj_t *fromheap, *toheap;
#endif


int GCTableEntryIDFlag = 0;  /* let the user code set it if it thinks it's on */


extern long SHOW_GCDEBUG;
extern int GCTABLE_BEGIN_VAL;
extern int GCTABLE_END_VAL;
extern int CODE_BEGIN_VAL;
extern int CODE_END_VAL;
extern int SML_GLOBALS_BEGIN_VAL;
extern int SML_GLOBALS_END_VAL;
extern int MUTABLE_TABLE_BEGIN_VAL;
extern int MUTABLE_TABLE_END_VAL;
extern int GLOBAL_TABLE_BEGIN_VAL;
extern int GLOBAL_TABLE_END_VAL;

static int *GCTABLE_BEGIN_ADDR = &GCTABLE_BEGIN_VAL;
static int *GCTABLE_END_ADDR = &GCTABLE_END_VAL;
static int *CODE_BEGIN_ADDR = &CODE_BEGIN_VAL;
static int *CODE_END_ADDR = &CODE_END_VAL;
static int *SML_GLOBALS_BEGIN_ADDR = &SML_GLOBALS_BEGIN_VAL;
static int *SML_GLOBALS_END_ADDR = &SML_GLOBALS_END_VAL;

static int *MUTABLE_TABLE_BEGIN_ADDR = &MUTABLE_TABLE_BEGIN_VAL;
static int *MUTABLE_TABLE_END_ADDR = &MUTABLE_TABLE_END_VAL;
static int *GLOBAL_TABLE_BEGIN_ADDR = &GLOBAL_TABLE_BEGIN_VAL;
static int *GLOBAL_TABLE_END_ADDR = &GLOBAL_TABLE_END_VAL;
unsigned long MaxStackDepth = 0;
unsigned long TotalStackDepth = 0;
unsigned long TotalNewStackDepth = 0;
unsigned long TotalStackSize  = 0;

extern value_t start_client_retadd_val;
Queue_t *ScanQueue = 0;


struct Callinfo
{
  value_t retadd;
#ifdef GCTABLE_HASENTRYID
  int     entryid;
#endif
  int     sizes;             /* low 10 bits = entry size in words; 
                                next 9 bits = frame size in words;
                                next 9 bits = byte section size in words 
				upper 4 bits = octa_offset of return address
			     */
  int     regtrace_a;       /* ab=10: YES      ab=00:NO  */
  int     regtrace_b;       /* ab=11 CALLEE    ab=01:SPEC */
  char    __rawdata[4];      /* must be word aligned;
				for the stack status: 00 -> TRACE_NO; 01-> TRACE_YES;
				                      02 -> TRACE_CALLEE ?; 03 -> TRACE_?
				then comes byte data and then special data */
  /* Note that ONLY the BYTE DATA follows the natural endian-ness.
    The other fields use ints/4 bytes when laid out.  If the the other fields,
    like the regtrace or stacktrace are read fom memory in smaller sizes for
    optimization purposes, the runtime must dispatch at compile-time on endian-ness. 
    The pairs of bits in stacktrace/regtrace are laid out starting from the lsb
    to the most significant bit.  Note that this is biased towards little-endian,
    the one true endian!!!*/
};


typedef struct Callinfo Callinfo_t;
typedef unsigned int bot;



typedef void (voidfun_t)();
#define NUM_STACK_STUB 200
#define stub_decl(m,n) \
m##n##0, m##n##1, m##n##2, m##n##3, m##n##4, m##n##5, m##n##6, m##n##7, m##n##8, m##n##9

voidfun_t stub_decl(stack_stub_,00);
voidfun_t stub_decl(stack_stub_,01);
voidfun_t stub_decl(stack_stub_,02);
voidfun_t stub_decl(stack_stub_,03);
voidfun_t stub_decl(stack_stub_,04);
voidfun_t stub_decl(stack_stub_,05);
voidfun_t stub_decl(stack_stub_,06);
voidfun_t stub_decl(stack_stub_,07);
voidfun_t stub_decl(stack_stub_,08);
voidfun_t stub_decl(stack_stub_,09);
voidfun_t stub_decl(stack_stub_,10);
voidfun_t stub_decl(stack_stub_,11);
voidfun_t stub_decl(stack_stub_,12);
voidfun_t stub_decl(stack_stub_,13);
voidfun_t stub_decl(stack_stub_,14);
voidfun_t stub_decl(stack_stub_,15);
voidfun_t stub_decl(stack_stub_,16);
voidfun_t stub_decl(stack_stub_,17);
voidfun_t stub_decl(stack_stub_,18);
voidfun_t stub_decl(stack_stub_,19);
voidfun_t stub_decl(exn_stub_,00);
voidfun_t stub_decl(exn_stub_,01);
voidfun_t stub_decl(exn_stub_,02);
voidfun_t stub_decl(exn_stub_,03);
voidfun_t stub_decl(exn_stub_,04);
voidfun_t stub_decl(exn_stub_,05);
voidfun_t stub_decl(exn_stub_,06);
voidfun_t stub_decl(exn_stub_,07);
voidfun_t stub_decl(exn_stub_,08);
voidfun_t stub_decl(exn_stub_,09);
voidfun_t stub_decl(exn_stub_,10);
voidfun_t stub_decl(exn_stub_,11);
voidfun_t stub_decl(exn_stub_,12);
voidfun_t stub_decl(exn_stub_,13);
voidfun_t stub_decl(exn_stub_,14);
voidfun_t stub_decl(exn_stub_,15);
voidfun_t stub_decl(exn_stub_,16);
voidfun_t stub_decl(exn_stub_,17);
voidfun_t stub_decl(exn_stub_,18);
voidfun_t stub_decl(exn_stub_,19);


void *stack_stubs[NUM_STACK_STUB] = { 
  stub_decl(stack_stub_,00),  stub_decl(stack_stub_,01),
  stub_decl(stack_stub_,02),  stub_decl(stack_stub_,03),
  stub_decl(stack_stub_,04),  stub_decl(stack_stub_,05),
  stub_decl(stack_stub_,06),  stub_decl(stack_stub_,07),
  stub_decl(stack_stub_,08),  stub_decl(stack_stub_,09),
  stub_decl(stack_stub_,10),  stub_decl(stack_stub_,11),
  stub_decl(stack_stub_,12),  stub_decl(stack_stub_,13),
  stub_decl(stack_stub_,14),  stub_decl(stack_stub_,15),
  stub_decl(stack_stub_,16),  stub_decl(stack_stub_,17),
  stub_decl(stack_stub_,18),  stub_decl(stack_stub_,19)
};

void *exn_stubs[NUM_STACK_STUB] = { 
  stub_decl(exn_stub_,00),  stub_decl(exn_stub_,01),
  stub_decl(exn_stub_,02),  stub_decl(exn_stub_,03),
  stub_decl(exn_stub_,04),  stub_decl(exn_stub_,05),
  stub_decl(exn_stub_,06),  stub_decl(exn_stub_,07),
  stub_decl(exn_stub_,08),  stub_decl(exn_stub_,09),
  stub_decl(exn_stub_,10),  stub_decl(exn_stub_,11),
  stub_decl(exn_stub_,12),  stub_decl(exn_stub_,13),
  stub_decl(exn_stub_,14),  stub_decl(exn_stub_,15),
  stub_decl(exn_stub_,16),  stub_decl(exn_stub_,17),
  stub_decl(exn_stub_,18),  stub_decl(exn_stub_,19)
};

extern value_t global_exnrec;

/* the size of this structure affect the code in stack_asm.s */
struct StackSnapshot
{
  value_t saved_ra;
  value_t saved_sp;
  unsigned int saved_regstate;
  Queue_t *roots;
};

typedef struct StackSnapshot StackSnapshot_t;
static StackSnapshot_t snapshots[NUM_STACK_STUB];
value_t *snapshots_saved_ra_add[NUM_STACK_STUB];
value_t exn_codeptr_table[NUM_STACK_STUB];

#ifdef DEBUG
void stub_error()
{
  printf("stub_error: should be a dead ra");
  assert(0);
}
#endif



#ifdef DEBUG
bot LookupStackBot(Callinfo_t *callinfo, int pos)
{
  unsigned int v = ((int*)(callinfo->__rawdata))[pos >> 4];
  int whichbot = pos & 15;
  return (v >> (2 * whichbot)) & 3;
}

#else

#define LookupStackBot(callinfo, pos) \
(((((unsigned int*)(callinfo->__rawdata))[pos >> 4]) >> (2 * (pos & 15))) & 3)

#endif

#define GET_ENTRYSIZE(x)      ( x        & 1023)
#define GET_FRAMESIZE(x)      ((x >> 10) & 511)
#define GET_BYTESTUFFSIZE(x)  ((x >> 19) & 511)
#define GET_OCTA_RA_OFFSET(x) ((x >> 28) & 15)

int LookupSpecialByte(Callinfo_t *callinfo, int pos)
{
  int framesize = GET_FRAMESIZE(callinfo->sizes);
  int stacktrace_rawbytesize = framesize >> 2;
  int stacktrace_bytesize = (stacktrace_rawbytesize + 3) & (~3);
  char *special_byte = callinfo->__rawdata + stacktrace_bytesize;
  return special_byte[pos];
}

void LookupSpecialWordPair(Callinfo_t *callinfo, int pos, int *a, int *b)
{
  int framesize = GET_FRAMESIZE(callinfo->sizes);
  int stacktrace_rawbytesize = framesize >> 2;
  int stacktrace_bytesize = (stacktrace_rawbytesize + 3) & (~3);
  char *special_byte = callinfo->__rawdata + stacktrace_bytesize;

  int bytedata_bytesize = GET_BYTESTUFFSIZE(callinfo->sizes) << 2;
  int *special_word_stuff = (int *) (special_byte + bytedata_bytesize);
  *a = special_word_stuff[2*pos];
  *b = special_word_stuff[2*pos+1];
  if (*b >= ((int)callinfo) + (GET_ENTRYSIZE(callinfo->sizes) << 2))
    {
      printf("ERROR in lookupspecialwordpair\n");
      printf("stacktrace_bytesize = %d\n",stacktrace_bytesize);
      printf("bytedata_bytesize = %d\n",bytedata_bytesize);
      printf("framesize (words) = %d\n",framesize);
      printf("pos = %d\n",pos);
    }
}

extern int module_count;

HashTable_t *CallinfoHashTable = NULL;
long GCTableSize = 0;
long CodeSize = 0;
long SMLGlobalSize = 0;
long GlobalTableSize = 0;
long MutableTableSize = 0;



extern value_t ml_input_gcentry;
extern value_t ml_lookahead_gcentry; 


value_t GetStackStub(unsigned int n)
{
  if (n < NUM_STACK_STUB)
    return (value_t) (stack_stubs[n]);
  assert(0);
}

void stack_init()
{
  struct HashEntry e;
  unsigned int mi = 0, count=NUM_STACK_STUB, i,j;

  ScanQueue = QueueCreate(100);

#ifdef DEBUG
#ifdef GCTABLE_HASENTRYID
  assert(GCTableEntryIDFlag == 1);
#else
  assert(GCTableEntryIDFlag == 0);
#endif
#endif

  for (mi=0; mi<module_count; mi++)
    {    
      int *startpos = (int *)(GCTABLE_BEGIN_ADDR[mi]);
      int *endpos = (int *)(GCTABLE_END_ADDR[mi]);
      int *curpos = startpos; 
#ifdef DEBUG
      printf("%d:  start,end  %d %d\n",mi,startpos,endpos);
#endif
      while (curpos <  endpos)
	{
	  count++;
#ifdef STACK_DEBUG
	  printf("curpos=%d "
#ifdef GCTABLE_HASENTRYID
		 "id=%d, "
#endif
		 "sizes=%d, entrysize=%d\n",
		 curpos, 		 
#ifdef GCTABLE_HASENTRYID
		 ((Callinfo_t *)curpos)->entryid,
#endif
		 ((Callinfo_t *)curpos)->sizes,
		 GET_ENTRYSIZE(((Callinfo_t *)curpos)->sizes));
#endif	  
	  curpos += GET_ENTRYSIZE(((Callinfo_t *)curpos)->sizes);
	}
       GCTableSize += (long)endpos - (long)startpos;
       CodeSize += (long)(CODE_END_ADDR[mi]) - 
	           (long)(CODE_BEGIN_ADDR[mi]);
       SMLGlobalSize += (long)(SML_GLOBALS_END_ADDR[mi]) - 
	           (long)(SML_GLOBALS_BEGIN_ADDR[mi]);
       MutableTableSize += (long)(MUTABLE_TABLE_END_ADDR[mi]) - 
	           (long)(MUTABLE_TABLE_BEGIN_ADDR[mi]);
       GlobalTableSize += (long)(GLOBAL_TABLE_END_ADDR[mi]) - 
	           (long)(GLOBAL_TABLE_BEGIN_ADDR[mi]);
                   
    }
  CallinfoHashTable = CreateHashTable(2*count);
  for (mi=0; mi<module_count; mi++)
    {    
      int *startpos = (int *)(GCTABLE_BEGIN_ADDR[mi]);
      int *endpos = (int *)(GCTABLE_END_ADDR[mi]);
      int *curpos = startpos; 
      while (curpos <  endpos)
	{
	  e.key = (unsigned long)(*curpos);
	  e.data = (void *)curpos;
	  /*	  printf("mi=%d, %d < %d, e.key = %d\n",mi, curpos, endpos, e.key); */
	  HashTableInsert(CallinfoHashTable,&e);
	  curpos += GET_ENTRYSIZE(((Callinfo_t *)curpos)->sizes);
	}
    }
#ifdef DEBUG
  printf("Done callinfo insertion for all modules\n");
#endif
  for (mi=0; mi<NUM_STACK_STUB; mi++)
    {
      e.key = (unsigned long)(GetStackStub(mi));
#ifdef DEBUG
      e.data = (void *)mi;
#else
      e.data = NULL;
#endif
      HashTableInsert(CallinfoHashTable,&e);
    }
  e.key = (unsigned long)ml_input_gcentry;
  e.data = (void *)&ml_input_gcentry;
  HashTableInsert(CallinfoHashTable,&e);
  
#ifdef DEBUG
  printf("&in.. and input_gcentry are  %d  %d\n",
	 &ml_input_gcentry, ml_input_gcentry);
  printf("&lo.. and lookahead_gcentry are  %d  %d\n",
	 &ml_lookahead_gcentry, ml_lookahead_gcentry);
#endif

  e.key = (unsigned long)ml_lookahead_gcentry;
  e.data = (void *)&ml_lookahead_gcentry;
  HashTableInsert(CallinfoHashTable,&e);

  for (i=0; i<NUM_STACK_STUB; i++)
    {
      snapshots[i].roots = NULL;
#ifdef DEBUG
      snapshots[i].saved_ra = (value_t) stub_error;
#else
      snapshots[i].saved_ra = 0;
#endif
      snapshots_saved_ra_add[i] = &(snapshots[i].saved_ra);
    }
}


Callinfo_t *LookupCallinfo(value_t ret_add)
{
  unsigned int i;
  struct HashEntry *e;
  e = HashTableLookup(CallinfoHashTable,(unsigned long)ret_add);
#ifdef DEBUG
  if (ret_add == (value_t) ml_input_gcentry)
    printf("input_dogc lookup in stack trace\n");
  if (ret_add == (value_t) ml_lookahead_gcentry)
    printf("lookahead_dogc lookup in stack trace\n");
  if (e && ((value_t)(e->data) < NUM_STACK_STUB))
    { printf("stack_stub_%d lookup in stack trace\n",e->data); return NULL; }
  if (e)
    return (Callinfo_t *)(e->data);
  printf("FATAL ERROR: ret_add = %d not found in table during stack trace\n",ret_add);
  assert(0);
#endif
  return (Callinfo_t *)(e->data);
}


/* *sp_ptr initially points to the bottom of a frame with *cur_retadd_ptr as the retadd
   we walk stack frames until we have scanned frame_to_trace frames
        or we have examined all frames which lie below top
        or we hit a stub routine return address
   the resulting return addresses are deposited in q
   *sp_ptr will return pointing to the top of the last processed frame
   *cur_retadd_ptr will correspond to the last process frame 
*/
int findretadd(value_t *sp_ptr, value_t *cur_retadd_ptr, value_t top, 
	       Queue_t *q, int frame_to_trace)
{
  int overflowed = 0;
  int count = 0, done = 0;
  value_t sp = *sp_ptr;
  value_t cur_retadd = *cur_retadd_ptr;
  QueueClear(q);
  while ((frame_to_trace--)>0)
    {
      Callinfo_t *callinfo = 0;
      int octa_offset = 0;
      callinfo = LookupCallinfo(cur_retadd);
      if (callinfo == NULL) /* hit a stub routine */
	break;
      if (sp > top)
	{
	  static int first = 1;
	  if (first)
	    printf("findretadd overflowed: sp=%d > top=%d CONTINUING\n",sp,top);
	  first = 0;
	  overflowed = 1;
	}
      Enqueue(q,(void *)cur_retadd);
      octa_offset = GET_OCTA_RA_OFFSET((value_t) callinfo->sizes);
      if (octa_offset == 15)
	octa_offset = LookupSpecialByte(callinfo,0);
#ifdef GCTABLE_HASENTRYID
      if (SHOW_GCDEBUG) 
	printf("%d: id=%d cur_retadd %12d      sp/top %d %d\n",
	       count++,callinfo->entryid,cur_retadd,sp,top);
#else
      if (SHOW_GCDEBUG) 
	printf("%d: cur_retadd %12d  sp/top %d %d\n",
	       count++,cur_retadd,sp,top);
#endif

#ifdef little_endian
      cur_retadd = *((int *)(sp+8*octa_offset));
#else
#ifdef big_endian
      cur_retadd = *((int *)(sp+ 4*GET_FRAMESIZE((value_t) callinfo->sizes) + 8 ));
#else
      error endian not defined
#endif
#endif

      sp += GET_FRAMESIZE(callinfo->sizes)<<2;
      if (cur_retadd == (value_t)(&start_client_retadd_val) || (sp == top))
	{
	  done = 1;
#ifdef DEBUG
	  if (sp == top) printf("******* sp==top\n");
#endif	
	  break;
	}
    }
  *sp_ptr = sp;
  *cur_retadd_ptr = cur_retadd;

  if (overflowed)
    {
      printf("findretadd overflowed: sp=%d > top=%d\n",sp,top);
      assert(0);
    }
  return done;
}

void show_stack(value_t sp, value_t cur_retadd, value_t top)
{
  static Queue_t *retadd_queue = 0;
  int i;
  if (retadd_queue == 0)
    retadd_queue = QueueCreate(200);
  (void) findretadd(&sp,&cur_retadd,top,retadd_queue,MAXINT);
  for (i=0; i<QueueLength(retadd_queue); i++)
    if (SHOW_GCDEBUG) printf("%d: %d\n",i,QueueAccess(retadd_queue,i));
  
}



int should_trace_big(unsigned long trace, 
		     Callinfo_t *callinfo, value_t cur_sp, int regstate,
		     unsigned int *byte_offset, unsigned int *word_offset,
		     value_t *data_add
#ifdef DEBUG
		     ,int i
#endif
		     )
{
  value_t data = *data_add;
#ifdef DEBUG
  if (IS_TRACE_YES(trace))
    {
      if (SHOW_GCDEBUG)
	printf("yes: stack trace value %d at loc %d val=%d\n",
	       data_add,i*4,data);
      return 1;
    }
  else if (IS_TRACE_NO(trace))
    {
      if (SHOW_GCDEBUG)
	printf("no: stack trace value %d at loc %d val=%d\n",
	       data_add,i*4,data);
      return 0;
    }
#endif
   
#ifdef DEBUG
  if (IS_TRACE_CALLEE(trace))
    {
      int pos = LookupSpecialByte(callinfo,*byte_offset);
      (*byte_offset)++;
      if (regstate & (1 << pos))
	{
	  if (SHOW_GCDEBUG)
	    printf("callee trace: add=%d stackpos=%d"
		   " val=%d trace/pos=%d/%d\n",
		   data_add,4*i,data,trace,pos);
	}
      return (regstate & (1 << pos));
    }
#endif
  if (IS_TRACE_SPECIAL(trace))
    {
      /* res is the type of the value */
      int res = 0;
      int special_type, special_data;
#ifdef DEBUG
      printf("TRACE_SPECIAL: data_add = %d\n",data_add);
#endif
      LookupSpecialWordPair(callinfo,*word_offset,
			    &special_type,&special_data);
      (*word_offset)++;
      if (IS_SPECIAL_STACK(special_type))
	res = ((int *)cur_sp)[special_data/4];
      else if (IS_SPECIAL_UNSET(special_type))
	{
#ifdef DEBUG
	  printf("\n\nENCOUNTERED UNSET @ data_add = %d\n\n",data_add);
#endif
	  Enqueue(ScanQueue,(void *)data_add);
	}
      else if (IS_SPECIAL_GLOBAL(special_type))
	res = *((int *)special_data);
      else if (IS_SPECIAL_STACK_REC(special_type))
	{
	  int rec_pos = GET_SPECIAL_STACK_REC_POS(special_type);
	  int rec_pos2 = GET_SPECIAL_STACK_REC_POS2(special_type);
	  int rec_pos3 = GET_SPECIAL_STACK_REC_POS3(special_type);
	  int rec_pos4 = GET_SPECIAL_STACK_REC_POS4(special_type);
	  res = ((int *)((int *)cur_sp)[special_data/4])[rec_pos];
	  if (rec_pos2 > 0)
	    res = ((int *)res)[rec_pos2-1];
	  if (rec_pos3 > 0)
	    res = ((int *)res)[rec_pos3-1];
	  if (rec_pos4 > 0)
	    res = ((int *)res)[rec_pos4-1];
	}
      else if (IS_SPECIAL_GLOBAL_REC(special_type))
	{
	  int rec_pos = GET_SPECIAL_STACK_GLOBAL_POS(special_type);
	  int rec_pos2 = GET_SPECIAL_STACK_GLOBAL_POS2(special_type);
	  int rec_pos3 = GET_SPECIAL_STACK_GLOBAL_POS3(special_type);
	  int rec_pos4 = GET_SPECIAL_STACK_GLOBAL_POS4(special_type);
	  res = ((int *)(*((int *)special_data)))[rec_pos];
	  if (rec_pos2 > 0)
	    res = ((int *)res)[rec_pos2-1];
	  if (rec_pos3 > 0)
	    res = ((int *)res)[rec_pos3-1];
	  if (rec_pos4 > 0)
	    res = ((int *)res)[rec_pos4-1];
	}
      else
	printf("impossible trace_special wordpair entry: %d %d\n",
	       special_type,special_data);
      /* values 0 to 3 represent integers */
      if (res >= 3)
	return 1;
    }
  else
    {
      if (SHOW_GCDEBUG) printf("impossible table entry: stackdata\n");
      assert(0);
    }
  return 0;
}


/* the return address in the queue identifies the frame
which is pointed to by bot_sp at the moment.  Note that
bot_sp points to the top of this frame.  Thus, when we are
done, bot_sp will point to the bottom of the last processed frame.  */
value_t trace_stack_step(unsigned long *saveregs,
			 value_t *bot_sp, Queue_t *retadd_queue,
			 value_t top,
			 Queue_t *roots, unsigned int *regstate_ptr, 
			 unsigned int frame_to_trace)
{
  value_t res = 0;
  unsigned int mi;
  value_t cur_sp;
  unsigned int regstate = *regstate_ptr;

  cur_sp = *bot_sp;

  while (!IsEmpty(retadd_queue) && ((frame_to_trace--)>0))
    {
      value_t     retadd    = (value_t)(QueuePop(retadd_queue));
      Callinfo_t *callinfo  = LookupCallinfo(retadd);
      unsigned int temp_regstate = 0;
      unsigned int byte_offset = 0, word_offset = 0;
      unsigned int framesize_word = GET_FRAMESIZE(callinfo->sizes);

      int stacktrace_rawbytesize = framesize_word >> 2;
      int stacktrace_bytesize = (stacktrace_rawbytesize + 3) & (~3);
      char *special_byte = callinfo->__rawdata + stacktrace_bytesize;

      unsigned int octa_offset = GET_OCTA_RA_OFFSET((value_t) callinfo->sizes);
      cur_sp -= framesize_word << 2;
      if (octa_offset == 15)
	{
	  octa_offset = LookupSpecialByte(callinfo,0);
	  byte_offset = 1;
	}
      res = (value_t)((int *)(cur_sp+8*octa_offset));
#ifdef DEBUG
      if (SHOW_GCDEBUG)
	{
#ifdef GCTABLE_HASENTRYID
	  printf("\nid=%d, ",callinfo->entryid);
#endif
	  printf("sp is %d cur_retadd is %d  regstate is %d  framesize = %d\n",
		cur_sp,retadd,regstate,framesize_word<<2);
	  printf("   regtrace_a is %x   regtrace_b is %x\n\n",
		 callinfo->regtrace_a, callinfo->regtrace_b);
	}
#endif

      /* read one byte at a time for short-circuiting to work 
        CAREFUL with endian-ness */
      /*      printf("\n*** framesize_word = %d\n",framesize_word+3); */
      for (mi=0; mi<(framesize_word+3)>>2; mi++)
	{
#ifdef alpha_osf
	  unsigned long v = (long)(((char*)(callinfo->__rawdata))[mi]);	
#endif
#ifdef rs_aix
	  unsigned mi_bigendian = (mi & (~3)) | (3 - (mi & 3));
	  unsigned long v = (long)(((char*)(callinfo->__rawdata))[mi_bigendian]);
#endif
	  int mj;
	  

	  if (!v) continue; /* short-circuit on TRACE_NO of 4 slots */
	  for (mj=3; mj>=0; mj--)
	    {
	      unsigned long trace =  v & 3;
#ifdef DEBUG
	      int i = mi * 4 + (3 - mj);
	      value_t *data_add = ((value_t *)cur_sp)+i;
	      value_t data = *data_add;
	      if (IS_TRACE_SPECIAL(trace))
		printf("mi = %d,  mj = %d, v = %d, cur_sp = %d, data_add = %d\n",
		       mi,mj,v,cur_sp,((value_t *)cur_sp)+(mi*4+(3-mj)));
#endif
	      v >>= 2;
#ifdef DEBUG

	      if (should_trace_big(trace,callinfo,cur_sp, regstate,
				   &byte_offset, &word_offset,
				   data_add,i))
#else

	      if (IS_TRACE_YES(trace) ||
		  ((!IS_TRACE_NO(trace)) &&
		   (IS_TRACE_CALLEE(trace) ?
		    (regstate & (1 << special_byte[byte_offset++])) :
		    should_trace_big(trace,callinfo,cur_sp, regstate,
				     &byte_offset, &word_offset, 
				     ((value_t *)cur_sp)+(mi*4+(3-mj))))))
#endif
		{
		  /* better to recompute stack_pos as this case is relatively infrequent */
		  int cur_stackpos = cur_sp + (((mi << 2) + (3 - mj)) << 2);
#ifdef STACK_DEBUG
		  printf("Enqueueing stackpos %d: cur_sp=%d\n",
			 cur_stackpos,cur_sp);
#endif
		  Enqueue(roots,(void *)cur_stackpos);
		}

	      /*	      printf("------\n"); */

	    }
	}


      temp_regstate = 0;
      {
	unsigned int ra = callinfo->regtrace_a;
	unsigned int rb = callinfo->regtrace_b;
	unsigned int yes_bits    = ra & ~rb;
	unsigned int callee_mask = ra & rb;
	unsigned int needspecial = ~ra & rb;
	temp_regstate = yes_bits | (callee_mask & regstate);
	/*	printf("initial temp_regstate is %d\n",temp_regstate); */
	if (needspecial)
	  {
	    for (mi=0; mi<32; mi++)
	      {
		value_t *data_add = (value_t *)&(saveregs[mi]);
#if 0
		printf("data_add is %d\n",data_add);
		printf("data is %d\n",*data_add);
#endif
		if (!(needspecial & (1U << mi)))
		  continue;
#ifdef DEBUG
		printf("doing special for reg %d = %d -> %d\n",
		       mi,data_add,*data_add);
/*		printf("doing special for reg %d = %d -> %d -> %d\n",
		       mi,data_add,*data_add,*((value_t *)*data_add));
*/
		if (should_trace_big(TRACE_SPECIAL,callinfo,cur_sp, regstate,
				     &byte_offset, &word_offset,
				    data_add,-1))
		    
#else
		if (should_trace_big(TRACE_SPECIAL,callinfo,cur_sp, regstate,
				     &byte_offset, &word_offset,
				     data_add))
#endif
		  temp_regstate |= 1 << mi;
	      }
	  }
      }
      /*      printf("final_temp regstate is %d\n",temp_regstate); */
      regstate = temp_regstate;

    }
    *bot_sp = cur_sp;
    *regstate_ptr = regstate;
    if (IsEmpty(retadd_queue))
      res = 0;

    return res;
}

unsigned int trace_stack_normal(unsigned long *saveregs,
				value_t bot_sp, value_t cur_retadd, value_t top,
				Queue_t *root_lists)
{
  
  static Queue_t *retadd_queue = 0;
  static Queue_t *roots = 0;
  value_t cur_sp;
  unsigned int regstate = 0;
  int not_done = 1;

  if (retadd_queue == 0)
    retadd_queue = QueueCreate(2000);
  if (roots == 0)
    roots = QueueCreate(200);
  
  { 
    value_t oldbot_sp = bot_sp;
    int junk = findretadd(&bot_sp,&cur_retadd,top,retadd_queue,MAXINT);
    TotalStackDepth += QueueLength(retadd_queue);
    if (MaxStackDepth < QueueLength(retadd_queue))
      MaxStackDepth = QueueLength(retadd_queue);
    TotalStackSize += top - oldbot_sp;
  }
#ifdef STACK_DEBUG
  printf("done findretadd with retadd_queue length =  %d\n",
	 QueueLength(retadd_queue));
#endif
  QueueClear(roots);




  while (not_done)
    {
      not_done = trace_stack_step(saveregs,&bot_sp, retadd_queue, top,
				  roots, &regstate, MAXINT);
    }
  Enqueue(root_lists,roots);
  return regstate;
}



long save_rate = 70;
long use_stack_gen = 0;

value_t exnstub_maxsp = 0;

void stubify_exnrecord(value_t _exnptr, int cur_snapshot)
{
  value_t max_sp = 0;
  value_t *exnptr = (value_t *)_exnptr;

  while (exnptr != &global_exnrec && cur_snapshot>=0)
    {
      value_t code = exnptr[0];
      value_t sp = exnptr[1];
#ifdef DEBUG
      if (code == global_exnrec) /* redundant check */
	break;
#endif
      if (code == (value_t)exn_stubs[cur_snapshot]) /* reached old stubified exn */
	break;
      if (sp >= snapshots[cur_snapshot].saved_sp)
	{
	  exn_codeptr_table[cur_snapshot] = code;
	  exnptr[0] = (value_t)exn_stubs[cur_snapshot];
	  cur_snapshot--;
	}
      exnptr = (value_t *)(exnptr[2]);
    }

  exnstub_maxsp = 0;
}



value_t walk_exnrecord(value_t _exnptr)
{
  value_t max_sp = 0;
  value_t *exnptr = (value_t *)_exnptr;

  while (exnptr != &global_exnrec)
    {
      value_t code = exnptr[0];
      value_t sp = exnptr[1];
      if (code == global_exnrec) /* redundant check */
	break;
      if (code == 0 && max_sp < sp)
	max_sp = sp;
      exnptr = (value_t *)(exnptr[2]);
    }
  return max_sp;
}

/* lastexptr is the value of exnptr after the last garbage collection */
unsigned int trace_stack_gen(unsigned long *saveregs,
			     value_t bot_sp, value_t cur_retadd, value_t top,
			     Queue_t *root_lists, value_t last_exnptr, value_t this_exnptr)
{
  static int last_snapshot = -1;
  static Queue_t *retadd_queue = 0;
  value_t cur_sp;
  value_t max_sp = 0; 
  unsigned int regstate = 0;
  int i, j, not_done = 1;

  TotalStackSize += top - bot_sp;
    
  /*  max_sp = walk_exnrecord(last_exnptr); */
  max_sp = exnstub_maxsp;

  if (retadd_queue == 0)
    retadd_queue = QueueCreate(2000);

  {
    int temp = -1;
    while (((temp+1) <= last_snapshot) &&
	   (snapshots[temp+1].saved_ra != 0) &&
#ifdef DEBUG
	   ((temp+1) < NUM_STACK_STUB) &&
	   (snapshots[temp+1].saved_ra != (value_t)stub_error) &&
#endif
	   (snapshots[temp+1].saved_sp > max_sp))
      temp++;
    last_snapshot = temp;
  }


#ifdef DEBUG  
  printf("last_snapshot=%d,  top=%d  max_sp = %d\n",last_snapshot,top,max_sp);
#endif


  if (last_snapshot >= 0)
    {
      for (i=0; i<=last_snapshot; i++)
	{
	  Queue_t *r = snapshots[i].roots;
	  Enqueue(root_lists,r);
	}
      regstate = snapshots[last_snapshot].saved_regstate;
      top = snapshots[last_snapshot].saved_sp;
    }


#ifdef DEBUG
  /* show root_lists and checksum */
#endif


  assert(top > bot_sp);
  { 
    int predone = save_rate * (last_snapshot+1);
    int retadd_qlen, curstack_depth;

    (void) findretadd(&bot_sp,&cur_retadd,top,retadd_queue,MAXINT);
    retadd_qlen = QueueLength(retadd_queue);
    TotalNewStackDepth += retadd_qlen;
    curstack_depth = retadd_qlen + predone;
    TotalStackDepth += curstack_depth;
    if (MaxStackDepth < curstack_depth) 
      MaxStackDepth = curstack_depth;
#ifdef DEBUG
    printf("predone/total  %d/%d/%d\n",predone,curstack_depth);
#endif
  }


  while (not_done)
    {
      unsigned int inner_save_rate = save_rate;
      Queue_t *r = snapshots[last_snapshot+1].roots;
      if (last_snapshot + 2 >= NUM_STACK_STUB)
	inner_save_rate = MAXINT;
      if (r == NULL)
	r = QueueCreate(50);

      QueueClear(r);
      not_done = trace_stack_step(saveregs, &bot_sp, retadd_queue, top,
				  r, &regstate, inner_save_rate);
      Enqueue(root_lists,r);

      if (not_done)
	{
	  value_t next_retaddid = (value_t)QueuePopPeek(retadd_queue);
	  Callinfo_t *callinfo  = LookupCallinfo(next_retaddid);
	  value_t next_ra_add = bot_sp - (GET_FRAMESIZE(callinfo->sizes)<<2);
	  unsigned int octa_offset = GET_OCTA_RA_OFFSET(callinfo->sizes);
	  if (octa_offset == 15)
	    octa_offset = LookupSpecialByte(callinfo,0);
	  next_ra_add += octa_offset << 3;

	  last_snapshot++; 
	  snapshots[last_snapshot].roots = r;
	  snapshots[last_snapshot].saved_regstate = regstate;
	  snapshots[last_snapshot].saved_sp = bot_sp;
	  snapshots[last_snapshot].saved_ra = *((value_t *)next_ra_add);
#ifdef DEBUG
	  printf("snapshots = %d, snapshots[last_snapshot]=%d"
		 "s[c]->saved_ra=%d, s[c]->saved_sp=%d\n",
		 snapshots,snapshots[last_snapshot],
		 snapshots[last_snapshot].saved_ra,
		 snapshots[last_snapshot].saved_sp);
	  printf("saving(%d/%d): sp,ra %d,%d regstate=%d\n",
		 last_snapshot,*((value_t *)not_done),
		 bot_sp,*((value_t *)next_ra_add),regstate);
#endif
	  *((value_t *)next_ra_add) = GetStackStub(last_snapshot);
	}

    }


  stubify_exnrecord(this_exnptr, last_snapshot);

#ifdef DEBUG
  printf("stack_trace_gen: last_snapshot=%d\n",last_snapshot);
#endif

  return regstate;
}


unsigned int trace_stack(unsigned long *saveregs,
			 value_t bot_sp, value_t cur_retadd, value_t top,
			 Queue_t *root_lists)
{
  unsigned int regstate = 0;
  static value_t last_exnptr = 0; 
  value_t this_exnptr = saveregs[EXNPTR_REG];
  extern value_t global_exnrec;

  if (!last_exnptr)
    last_exnptr = (value_t)(&global_exnrec);

  QueueClear(ScanQueue);
  
  if (use_stack_gen)
    regstate = trace_stack_gen( saveregs, bot_sp,  cur_retadd, top, 
				root_lists, last_exnptr, this_exnptr);
  else
    regstate = trace_stack_normal( saveregs, bot_sp,  cur_retadd, top, root_lists);

  last_exnptr = this_exnptr;

#ifdef DEBUG
  if (SHOW_GCDEBUG)
  { 
    int i, j, count=0;
    value_t sum = 0;
    for (j=0; j<QueueLength(root_lists); j++)
      {
	Queue_t *roots = QueueAccess(root_lists,j);
	for (i=0; i<QueueLength(roots); i++)
	  {
	    sum += (value_t)QueueAccess(roots,i); 
	    printf("  root %d: %d\n",count,QueueAccess(roots,i));
	    count++; 
	  }
      }
    printf("after # of roots = %d,  cksum=%d, regstate is %d\n",
	   count,sum,regstate);
    printf("\n\n\n");
  }
#endif

#ifdef STACK_DEBUG
  printf("returning regstate = %x\n",regstate);
#endif

  return regstate;
}
