#ifndef _thread_h
#define _thread_h

#include "tag.h"
#include "memobj.h"
#include <signal.h>

struct ThreadObj__t
{
  int                running;
  int                done;
  int                valid;
  int                tid;
  StackChainObj_t   *stackchain;
  value_t            start_address;
  int                num_add;
  long               saveregs[32];
  long               ret_add;
};

typedef struct ThreadObj__t ThreadObj_t;

ThreadObj_t *thread_create(value_t start_adds, int num_add);
void thread_insert(ThreadObj_t *th);
void thread_go();



#endif

