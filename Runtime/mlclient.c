#include "tag.h"
#include "create.h"
#include "show.h"
#include "queue.h"
#include "gc.h"
#include <sys/types.h>
#include <sys/mman.h>
#include <stdlib.h>

value_t MakeStackFrame(value_t start, value_t *live, int sz)
{
  int size = 2 * sz;
  int framesize = size + 1 + (size + 31) / 32;
  int *bottom;
  int i,j;

  start -= 4 * framesize;
  bottom = (int *)start;

  bottom[0] = size;
  for (i=0; i<(size+31)/32; i++)
    {
      unsigned int mask = 0;
      for (j=0; j<32; j++)
	if ((j % 2 == 1) && ((j+32*i) < size))
	  mask |= (1<<j);
      bottom[1+i] = mask;
    }
  for (i=0; i<size; i++)
    {
      j = i + 1 + (size+31)/32;
      bottom[j] = random() % 1000;
      if (i % 2 == 1)
	bottom[j] = live[i/2];
    }
  return start;
}

value_t MakeStack(value_t start, value_t *live, int max)
{
  int maxlive = (max > 3) ? 3 : max;
  int sz, left;
  value_t newstack;
  if (max <= 0)
    return start;

  sz = 1 + (random() % maxlive);
  left = max - sz;
  if (left < 0)
    BUG("MakeStack: impossible to have negative args here\n");
  newstack = MakeStackFrame(start,live,sz);
  return MakeStack(newstack,live+sz,left);
}


void client_code(value_t topstack, 
		 value_t alloc_ptr, value_t limit)
{
  value_t a,b,c,d,e;
  const int livecountmax = 20;
  value_t live[livecountmax];
  int livecount = 0;
  int i;
  int numthings = 3;
value_t hstart = alloc_ptr;


  a = alloc_manyint(2,99,&alloc_ptr,limit);
  for (i=0; i<numthings; i++)
    {
      value_t temp1, temp2;
/*      temp1 = alloc_iarray(i,5,&alloc_ptr,limit); */
      temp1 = alloc_rarray(i,56.7,&alloc_ptr,limit); 
      temp2 = alloc_manyint(i,4,&alloc_ptr,limit);
      if (i < numthings/3)
	a = alloc_manyintrec(numthings-i,100+i,a,&alloc_ptr,limit);
      else if (i < 2*(numthings/3))
	a = alloc_recrec(temp1,a,&alloc_ptr,limit);
      else
	a = alloc_recrec(temp2,a,&alloc_ptr,limit);
      live[livecount++] = a;
      if (livecount > livecountmax)
	{
	  printf("LIVECOUNT ERROR\n");
	  exit(-1);
	}
    }


  {
    value_t bot;
    
    bot = MakeStack(topstack,live,livecount);  
    printf("\ndone with make stack\n");
    show_stack(bot,topstack); 
    ml_client(bot,alloc_ptr,limit);
    printf("\ncall to ml_client returned\n");
  }

}


