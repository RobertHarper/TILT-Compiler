#include "tag.h"
#include "queue.h"
#include "stack.h"
#include "thread.h"
#include "hash.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <values.h>
#include "general.h"
#include "stats.h"
#include "memobj.h"
#include "client.h"

int GCTableEntryIDFlag = 0;  /* let the user code set it if it thinks it's on */
long save_rate = 70;
long use_stack_gen = 0;


static value_t *GCTABLE_BEGIN_ADDR = &GCTABLE_BEGIN_VAL;
static value_t *GCTABLE_END_ADDR = &GCTABLE_END_VAL;
static value_t *CODE_BEGIN_ADDR = &CODE_BEGIN_VAL;
static value_t *CODE_END_ADDR = &CODE_END_VAL;
static value_t *SML_GLOBALS_BEGIN_ADDR = &SML_GLOBALS_BEGIN_VAL;
static value_t *SML_GLOBALS_END_ADDR = &SML_GLOBALS_END_VAL;
static value_t *MUTABLE_TABLE_BEGIN_ADDR = &MUTABLE_TABLE_BEGIN_VAL;
static value_t *MUTABLE_TABLE_END_ADDR = &MUTABLE_TABLE_END_VAL;

long MaxStackDepth = 0;
long TotalStackDepth = 0;
long TotalNewStackDepth = 0;
long TotalStackSize  = 0;

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

voidfun_t *stack_stubs[NUM_STACK_STUB] = { 
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


extern value_t global_exnrec;


void stub_error()
{
  printf("stub_error: should be a dead ra");
  assert(0);
}


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
  if ((value_t)(&(special_word_stuff[2*pos+1])) >= 
      ((value_t)callinfo) + (GET_ENTRYSIZE(callinfo->sizes) << 2))
    {
      printf("ERROR in lookupspecialwordpair\n");
      printf("stacktrace_bytesize = %d\n",stacktrace_bytesize);
      printf("bytedata_bytesize = %d\n",bytedata_bytesize);
      printf("framesize (words) = %d\n",framesize);
      printf("pos = %d\n",pos);
    }
}



HashTable_t *CallinfoHashTable = NULL;
long GCTableSize = 0;
long CodeSize = 0;
long SMLGlobalSize = 0;
long GlobalTableSize = 0;
long MutableTableSize = 0;



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

  ScanQueue = QueueCreate(0,100);

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
/*
       GlobalTableSize += (long)(GLOBAL_TABLE_END_ADDR[mi]) - 
	           (long)(GLOBAL_TABLE_BEGIN_ADDR[mi]);
                   
*/
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
}


Callinfo_t *LookupCallinfo(value_t ret_add)
{
  unsigned int i;
  struct HashEntry *e;
  e = HashTableLookup(CallinfoHashTable,(unsigned long)ret_add,0);
#ifdef DEBUG
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
  int i;
  Queue_t *retadd_queue = QueueCreate(0,200);
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
	  if (collector_type == Parallel)
	    assert(0);
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
	  res = ((int *)(((int *)special_data)))[rec_pos];
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
value_t trace_stack_step(Thread_t *th, unsigned long *saveregs,
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

  while (!QueueIsEmpty(retadd_queue) && ((frame_to_trace--)>0))
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
#ifdef little_endian
	  unsigned long v = (long)(((char*)(callinfo->__rawdata))[mi]);	
#ifdef big_endian
#error big and little endian both defined
#endif
#endif
#ifdef big_endian
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
		  value_t *slot = (value_t *)(cur_sp + (((mi << 2) + (3 - mj)) << 2));
#ifdef STACK_DEBUG
		  printf("Enqueueing stack slot %d: cur_sp=%d\n",
			 slot,cur_sp);
#endif
		  if (*slot > 256)
		    Enqueue(roots,(void *)slot);
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
    if (QueueIsEmpty(retadd_queue))
      res = 0;

    return res;
}

unsigned int trace_stack_normal(Thread_t *th, unsigned long *saveregs,
				value_t bot_sp, value_t cur_retadd, value_t top,
				Queue_t *root_lists)
{
  Queue_t *retadd_queue = th->retadd_queue;
  Queue_t *roots = th->snapshots[0].roots;
  value_t cur_sp;
  unsigned int regstate = 0;
  int not_done = 1;

  
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
      not_done = trace_stack_step(th, saveregs,&bot_sp, retadd_queue, top,
				  roots, &regstate, MAXINT);
    }
  Enqueue(root_lists,roots);
  return regstate;
}





/* lastexptr is the value of exnptr after the last garbage collection */
unsigned int trace_stack_gen(Thread_t *th, unsigned long *saveregs,
			     value_t bot_sp, value_t cur_retadd, value_t top,
			     Queue_t *root_lists, value_t last_exnptr, value_t this_exnptr)
{
  Queue_t *retadd_queue = th->retadd_queue;
  value_t cur_sp;
  value_t max_sp = th->maxSP;
  unsigned int regstate = 0;
  int i, j, not_done = 1;

  TotalStackSize += top - bot_sp;

  {
    int temp = -1;
    while (((temp+1) <= th->last_snapshot) &&
	   (th->snapshots[temp+1].saved_ra != 0) &&
#ifdef DEBUG
	   ((temp+1) < NUM_STACK_STUB) &&
	   (th->snapshots[temp+1].saved_ra != (value_t)stub_error) &&
#endif
	   (th->snapshots[temp+1].saved_sp > max_sp))
      temp++;
    th->last_snapshot = temp;
  }


#ifdef DEBUG  
  printf("th->last_snapshot=%d,  top=%d  max_sp = %d\n",th->last_snapshot,top,max_sp);
#endif


  if (th->last_snapshot >= 0)
    {
      for (i=0; i<=th->last_snapshot; i++)
	{
	  Queue_t *r = th->snapshots[i].roots;
	  Enqueue(root_lists,r);
	}
      regstate = th->snapshots[th->last_snapshot].saved_regstate;
      top = th->snapshots[th->last_snapshot].saved_sp;
    }


#ifdef DEBUG
  /* show root_lists and checksum */
#endif


  assert(top > bot_sp);
  { 
    int predone = save_rate * (th->last_snapshot+1);
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
      Queue_t *r = th->snapshots[th->last_snapshot+1].roots;
      if (th->last_snapshot + 2 >= NUM_STACK_STUB)
	inner_save_rate = MAXINT;
      if (r == NULL)
	r = QueueCreate(0,50);

      QueueClear(r);
      not_done = trace_stack_step(th, saveregs, &bot_sp, retadd_queue, top,
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

	  th->last_snapshot++; 
	  th->snapshots[th->last_snapshot].roots = r;
	  th->snapshots[th->last_snapshot].saved_regstate = regstate;
	  th->snapshots[th->last_snapshot].saved_sp = bot_sp;
	  th->snapshots[th->last_snapshot].saved_ra = *((value_t *)next_ra_add);
#ifdef DEBUG
	  printf("snapshots = %d, snapshots[th->last_snapshot]=%d"
		 "s[c]->saved_ra=%d, s[c]->saved_sp=%d\n",
		 snapshots,snapshots[th->last_snapshot],
		 snapshots[th->last_snapshot].saved_ra,
		 snapshots[th->last_snapshot].saved_sp);
	  printf("saving(%d/%d): sp,ra %d,%d regstate=%d\n",
		 th->last_snapshot,*((value_t *)not_done),
		 bot_sp,*((value_t *)next_ra_add),regstate);
#endif
	  *((value_t *)next_ra_add) = GetStackStub(th->last_snapshot);
	}

    }


#ifdef DEBUG
  printf("stack_trace_gen: th->last_snapshot=%d\n",th->last_snapshot);
#endif

  return regstate;
}


unsigned int trace_stack(Thread_t *th, unsigned long *saveregs,
			 value_t top, Queue_t *root_lists)
{
  unsigned int regstate = 0;
  static value_t last_exnptr = 0; 
  value_t this_exnptr = saveregs[EXNPTR_REG];
  extern value_t global_exnrec;
  long sp = saveregs[SP_REG];
#ifdef solaris
  long ret_add = saveregs[LINK_REG] + 8;
#else
  long ret_add = saveregs[RA_REG];
#endif

  if (!last_exnptr)
    last_exnptr = (value_t)(&global_exnrec);

  QueueClear(ScanQueue);
  
  if (use_stack_gen)
    regstate = trace_stack_gen( th, saveregs, sp,  ret_add, top, 
				root_lists, last_exnptr, this_exnptr);
  else
    regstate = trace_stack_normal( th, saveregs, sp,  ret_add, top, root_lists);

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



void debug_after_rootscan(unsigned long *saveregs, int regmask, 
			  Queue_t *root_lists, Heap_t *fromheap)
{
#ifdef DEBUG
  int allocptr = saveregs[ALLOCPTR_REG];
    if (SHOW_GCDEBUG && NumGC > LEAST_GC_TO_CHECK)
      {
	long i,j;
	printf("\n--------------- ROOT INFORMATION ---------------\n");
	for (i=0; i<32; i++)
	  if (regmask & (1 << i))
	    printf("LIVE REGISTER VALUE %d: %d\n",i,saveregs[i]);
	
	for (j=0; j<QueueLength(root_lists); j++)
	  {
	    Queue_t *roots = QueueAccess(root_lists,j);
	    for (i=0; i<QueueLength(roots); i++)
	      printf("ROOT #%d: %d(%d)\n",
		     i,QueueAccess(roots,i),*((int *)QueueAccess(roots,i)));
	  }
      }
    if (SHOW_HEAPS)
      {
	if (SHOW_HEAPS)
	  memdump("From Heap Before collection:", (int *)fromheap->bottom,100,0);
	show_heap("ORIG FROM",fromheap->bottom,allocptr,fromheap->top); 
	memdump("OLD_FROMHEAP",(int *)old_fromheap->bottom,
		((value_t)old_alloc_ptr - (value_t)old_fromheap->bottom)/4,0);
	show_heap("OLD_FROMHEAP",old_fromheap->bottom,old_alloc_ptr,
		  old_fromheap->top); 
      }
    else
      {
	check_heap("ORIG FROM",fromheap->bottom,allocptr,fromheap->top); 
	check_heap("OLD_FROMHEAP",old_fromheap->bottom,old_alloc_ptr,
		   old_fromheap->top); 
      }
#endif
}

void local_root_scan(Thread_t *th)
{
  Queue_t *root_lists = th->root_lists;
  Queue_t *reg_roots = th->reg_roots;
  unsigned long *saveregs = (unsigned long *)(th->saveregs);
  unsigned long i;
  
  start_timer(&th->stacktime);
  QueueClear(root_lists);
  QueueClear(reg_roots);
  
  if (saveregs[ALLOCLIMIT_REG] == StopHeapLimit) {  /* thread has not started */
    if (th->num_add == 0)
      Enqueue(reg_roots,&(th->start_address));
    else 
      for (i=0; i<th->num_add; i++)
	Enqueue(reg_roots,&(((value_t *)th->start_address)[i]));
  }
  else {
    long sp = saveregs[SP_REG];
    Stack_t *stack = GetStack(sp);
    int regmask = trace_stack(th, saveregs, stack->top, root_lists);
    regmask |= 1 << EXNPTR_REG;
    for (i=0; i<32; i++)
      if ((regmask & (1 << i)) && (saveregs[i] > 256))
	Enqueue(reg_roots,(int *)(&(saveregs[i])));
  }
  Enqueue(root_lists,reg_roots);

#ifdef DEBUG
  debug_after_rootscan(saveregs,regmask,root_lists);
#endif
  stop_timer(&th->stacktime);

}

void global_root_scan(Queue_t *global_roots, Queue_t *promoted_global_roots)
{
  static Queue_t *uninit_global_roots;
  static Queue_t *temp;
  static int first_time = 1;
  unsigned long i,mi, stack_top, len;

  if (first_time)
    { 
      uninit_global_roots = QueueCreate(0,100); 
      temp = QueueCreate(0,100); 
      for (mi=0; mi<module_count; mi++) {
	value_t *start = (value_t *)((&MUTABLE_TABLE_BEGIN_VAL)[mi]);
	value_t *stop = (value_t *)((&MUTABLE_TABLE_END_VAL)[mi]);
	for ( ; start < stop; start += 4) {
	  Enqueue(uninit_global_roots,start); 
	}
      }
    }

  QueueClear(promoted_global_roots);
  QueueClear(temp);
  len = QueueLength(uninit_global_roots);
  for (i=0; i<len; i++)
    {
      value_t *e = QueueAccess(uninit_global_roots,i);
      value_t table_entry = ((value_t *)e)[0];
      int     trace       = ((value_t *)e)[1];
      value_t data = *((value_t *)table_entry);
      int should_trace = 0;
      int resolved = (data != 258);

      if (!resolved)
	{ Enqueue(temp,e); }
      else if (IS_TRACE_YES(trace))
	should_trace = 1;
      else if (IS_TRACE_NO(trace))
	;
      else if (IS_TRACE_CALLEE(trace))
	{ printf("cannot have trace_callee for globals\n"); exit(-1); }
      else if (IS_TRACE_SPECIAL(trace))
	{ 
	  int special_type = ((value_t *)e)[2];
	  int special_data = ((value_t *)e)[3];
	  int res;
	  
	  if (IS_SPECIAL_STACK(special_type))
	    { printf("cannot have trace_special_stack for globals\n"); exit(-1); }
	  else if (IS_SPECIAL_UNSET(special_type))
	    { printf("cannot have trace_special_unset for globals\n"); exit(-1); }
	  else if (IS_SPECIAL_STACK_REC(special_type))
	    { printf("cannot have trace_special_stackrec for globals\n"); exit(-1); }
	  else if (IS_SPECIAL_GLOBAL(special_type))
	    res = *((int *)special_data);
	  else if (IS_SPECIAL_GLOBAL_REC(special_type))
	    {
	      int rec_pos = GET_SPECIAL_STACK_GLOBAL_POS(special_type);
	      int rec_pos2 = GET_SPECIAL_STACK_GLOBAL_POS2(special_type);
	      int rec_pos3 = GET_SPECIAL_STACK_GLOBAL_POS3(special_type);
	      int rec_pos4 = GET_SPECIAL_STACK_GLOBAL_POS4(special_type);
	      res = ((int *)(((int *)special_data)))[rec_pos];
	      if (rec_pos2 > 0)
		res = ((int *)res)[rec_pos2-1];
	      if (rec_pos3 > 0)
		res = ((int *)res)[rec_pos3-1];
	      if (rec_pos4 > 0)
		res = ((int *)res)[rec_pos4-1];
	    }
	  else
	    { printf("impossible trace_special wordpair entry: %d %d\n",
		     special_type,special_data);
	    exit(-1);
	    }
	  should_trace = (res >= 3);
	} /* TRACE_SPECIAL */ 

      Enqueue(global_roots,(void *)table_entry); /* this is accumulated */
      Enqueue(promoted_global_roots,(void *)table_entry); /* this is reset each time */
    }


  typed_swap(Queue_t *, uninit_global_roots,temp);

}

