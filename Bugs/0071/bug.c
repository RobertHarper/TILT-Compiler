#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "thread.h"
#include "tag.h"
#include "exn.h"
#include "create.h"

typedef ptr_t unit;
typedef int word;

extern void raise_exception_raw(Thread_t *th, ptr_t exn_arg);
unit raise_an_exception(ptr_t exn)
{
  Thread_t* th = getThread();
  volatile unsigned long* saveregs = th->saveregs;
  int i;
  fprintf(stderr,"\n\n--------raise_an_exception------\n");
  for (i=0; i<32; i++) {
    fprintf(stderr,"  saveregs[%d] is 0x%lx\n",i,saveregs[i]);
  }
  fprintf(stderr,"NumGC = %d\n", NumGC);

  /* The exception raised does not seem to matter.  We see the illegal
     instruction when we raise SysErr, Div, and Overflow.  */
/*
  runtime_error_msg("here's your exception");
  raise_exn(getDivExn());
  raise_exn(getOverflowExn());
*/
/* Avoids the error:  raise_exception_raw(th,exn); */
  raise_exn(exn);
  return empty_record;		/* not reached */
}
#if 0
unit my_openf(word oflag)
{
  int fd;
  (void)open("/dev/null",oflag,0);
  abort();
  return empty_record;
}
#endif
