#include <stdlib.h>
#define _STDIO_UNLOCK_CHAR_IO
#include <stdio.h>
#include <errno.h>
#include "general.h"
#include <string.h>

#include "tag.h"
#include "create.h"
#include "mllib.h"

int exncounter = 4200;



static FILE *des2ptr_table[100];
static int lookahead_char[100];

static int curdes = 0;
static int mllib_inited = 0;
int alloc_des(FILE *val)
{
  int des = curdes;
  des2ptr_table[des] = val;
  curdes++;
  return des;
}

FILE* des2ptr(int des)
{
  if (des>=curdes)
    {
      printf("Illegal descriptor %d\n",des);
      if (!mllib_inited)
	printf("mllib has not been initialized\n");
      return 0;
    }
  return des2ptr_table[des];
}

void mllib_init()
{
  int i=0;
  for (i=0; i<100; i++)
    lookahead_char[i] = -1;
  des2ptr_table[0] = stdin;
  des2ptr_table[1] = stdout;
  des2ptr_table[2] = stderr;
  curdes = 3;

  mllib_inited = 1;
}



int ml_output(value_t _des, value_t mlstring)
{
/* XXX this treatment of descriptor is wrong */
  FILE *F = des2ptr(_des);
  char *str = (char *)mlstring;
  unsigned int tag = ((int *)mlstring)[-1];
  int bytelen = tag >> ARRLEN_OFFSET;

#ifdef DEBUG
  if (_des <= 2)
    {
      printf("tag = %d, byte length=%d first char=%d *%s*\n",tag, bytelen,str[0],str);    
    }
#endif

  fwrite(str,1,bytelen,F);

  return 0;
}




static int ml_open(char *mode, value_t mlstring)
{
  int des = 0;
  FILE *f = 0;
  char buf[100];
  unsigned int tag = ((int *)mlstring)[-1];
  int bytelen = tag >> ARRLEN_OFFSET;
  char *raw = (char *)mlstring;
  bcopy(raw,buf,bytelen);
  buf[bytelen] = 0;

  f = fopen(buf,mode);
  if (f == NULL)
    {
      printf("Error in fopen(\"%s\",\"%s\")\n  errno is %d\n",buf,mode,errno);
      if (errno == 2)
	printf("File does not exist for opening\n");
    }
  des = alloc_des(f);
  return des;
}

int ml_open_in(value_t mlstring)
{
  return ml_open("r",mlstring);
}

int ml_open_out(value_t mlstring)
{
  return ml_open("w",mlstring);
}

void ml_close_in(value_t des)
{
  fclose(des2ptr(des));
}

int ml_close_out(value_t des)
{
  fclose(des2ptr(des));
}

int ml_flush_out(value_t des)
{
  return fflush(des2ptr(des));
}

/* calling this should have no visible effects */
void peep(value_t _des)
{
  if (lookahead_char[_des] < 0)
    {
      /* XXX this treatment of descriptor is wrong */
      FILE *F = des2ptr(_des);
      lookahead_char[_des] = getc(F);
    }
}

int ml_end_of_stream(value_t des)
{
  peep(des);
  return !!(feof(des2ptr(des)));
}

value_t ml_lookahead(value_t _des)
{
  int found = 0;
  char c;
  
  peep(_des); 
  c = lookahead_char[_des];
  found = (c >= 0) && (c != EOF);
  if (found)
    {return (value_t) c; }
  else 
    return (value_t) 255;
}

value_t ml_input1(value_t _des)
{
  /* XXX this treatment of descriptor is wrong */
  FILE *F = des2ptr(_des);
  char buf[4];
  int buflen = 0;
  if (lookahead_char[_des] >= 0)
    {
      value_t result = (value_t)(lookahead_char[_des]);
      lookahead_char[_des] = -1;
      return result;
    }
  else
    {
      buflen = fread(buf,1,1,F);
      if (buflen == 1)
	{ return (value_t)(buf[0]); }
      else
	{ return (value_t)255; }
    }

}

value_t ml_input(value_t _des, value_t numtoread)
{
  /* XXX this treatment of descriptor is wrong */
  FILE *F = des2ptr(_des);
  char *buf = NULL;
  value_t res = alloc_uninit_string(numtoread,&buf);
  int buflen = 0;
  if (lookahead_char[_des] >= 0)
    {
      buf[0] = (char)lookahead_char[_des];
      buflen = 1 + fread(buf+1,1,numtoread-1,F);
      lookahead_char[_des] = -1;
    }
  else
    buflen = fread(buf,1,numtoread,F);
  /*      buf[buflen] = 0; ML strings not null terminated */
  adjust_stringlen(res,buflen);
  return res;
}

value_t til_div(value_t n1, value_t n2)

{
  return (value_t)((int)n1/(int)n2);
}

value_t til_divt(value_t n1, value_t n2)

{
  return (value_t)((int)n1/(int)n2); /* doesn't trap ??? */
}

value_t til_mod(value_t n1, value_t n2)

{
  return (value_t)((int)n1%(int)n2); /* may be wrong for signed values ??? */
}

value_t til_modt(value_t n1, value_t n2)

{
  return (value_t)((int)n1%(int)n2); /* may be wrong for signed values
					doesn't trap ??? */
}

value_t cvt_real2int(double x)

{
  return (value_t)x;
}

double cvt_int2real(value_t n)

{
  return (double)n;
}




