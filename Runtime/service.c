#include "tag.h"

#ifdef alpha_osf
#include "interface_osf.h"
#endif
#ifdef rs_aix
#include "interface_aix.h"
#endif

#include <stdio.h>

/* Registers to save are all registers from 0 to 31 except for:
   r31(zero)
   alloclimit
   allocptr
   asmtmp
   gp
   pv
   In addition, we must save the handler
*/

long Prelude_31_Overflow_1 = 1234567890;
long Prelude_57_Div_1 = 1234567890;

#define gprintf  fprintf
/*
void exn_save(long *saveregs, long sp, value_t handler)
{
  int i;  
  int regmask, allocptr, alloclimit;
  int count = 0;
  value_t rec;


#ifdef DEBUG_EXN
  gprintf(stderr,
	  "\n\n--------exn_save entered with handler = %d ------\n",handler);
  for (i=0; i<32; i++)
    gprintf(stderr,"save: saveregs[%d] is %ld\n",i,saveregs[i]);
#endif

  allocptr = saveregs[ALLOCPTR_REG];
  alloclimit = saveregs[ALLOCLIMIT_REG];
  regmask = saveregs[REGMASK_REG];
  {
    int wh = 0;
    value_t fields[32];
    int masks[1];
    masks[0] = 0;
    fields[count++] = saveregs[EXNPTR_REG];
    fields[count++] = handler;
    while (wh < 32)
      {
	if (wh != ALLOCLIMIT_REG &&
	    wh != EXNPTR_REG &&
	    wh != ALLOCPTR_REG &&
	    wh != ASMTMP_REG)
	  fields[count++] = saveregs[wh];

	if (regmask & (1 << wh))
	  masks[0] |= 1 << count;
	wh++;
      }
    rec = alloc_record(&allocptr, alloclimit, fields, masks, count);
#ifdef DEBUG_EXN
    gprintf(stderr,"save: rec is %d\n",rec);
    gprintf(stderr,"save: rec[-1] is %d\n",((int *)rec)[-1]);
    gprintf(stderr,"save: rec[0] is %d\n",((int *)rec)[0]);
    gprintf(stderr,"save: get_rec(rec,0) is %d\n",(int)(get_record(rec,0)));
    gprintf(stderr,"save: get_rec(rec,2) is %d\n",(int)(get_record(rec,2)));
    gprintf(stderr,"save: get_rec(rec,23) is %d\n",(int)(get_record(rec,23)));
    gprintf(stderr,"save: get_rec(rec,24) is %d\n",(int)(get_record(rec,24)));
#endif
  }

  saveregs[ALLOCPTR_REG] = allocptr;

#ifdef DEBUG_EXN
  gprintf(stderr,"save: saveregs[0] is %d\n",saveregs[0]);
  gprintf(stderr,"save: exnptr is %d\n",saveregs[EXNPTR_REG]);
#endif

  saveregs[EXNPTR_REG] = rec;
}



void exn_restore(long *saveregs, value_t exn_ptr, value_t exn_arg)
{  
  int i;  
  int regmask, allocptr, alloclimit;
  int count = 0;
  value_t rec;

#ifdef DEBUG_EXN
  gprintf(stderr,"\n\n--------exn_restore entered---------\n");

  gprintf(stderr,"restore: exn_ptr is %d\n",exn_ptr);
  gprintf(stderr,"restore: rec[-1] is %d\n",((int *)exn_ptr)[-1]);
  gprintf(stderr,"restore: rec[0] is %d\n",((int *)exn_ptr)[0]);
#endif

  regmask = saveregs[REGMASK_REG];
  {
    int wh = 0;
    value_t *fields = (value_t *) exn_ptr;
    saveregs[EXNPTR_REG] = get_record(fields,count++);
    saveregs[ASMTMP_REG] = get_record(fields,count++);
    while (wh < 32)
      {
	if (wh != ALLOCLIMIT_REG &&
	    wh != ALLOCPTR_REG &&
	    wh != ASMTMP_REG &&
	    wh != EXNPTR_REG)
	  saveregs[wh] = get_record(fields,count++);
	wh++;
      }
  }

#ifdef DEBUG_EXN
  for (i=0; i<32; i++)
    gprintf(stderr,"RESTORE: saveregs[%d] is %ld\n",i,saveregs[i]);

  gprintf(stderr,"restore: saveregs[0] is %d\n",saveregs[0]);
  gprintf(stderr,"restore: saveregs[at] is %d\n",saveregs[ASMTMP_REG]);
  gprintf(stderr,"returning from exn_restore to asm linkage\n");
#endif
}
*/



