#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h>
#include <math.h>	/* for float operations */
#ifdef __linux__
#include <errno.h> 	/* for errno */
#endif

extern void *GC_malloc(int size);
extern void *GC_malloc_atomic(int size);
/* #define DO_MALLOC_DEBUG */
#ifdef DO_MALLOC_DEBUG
extern void GC_generate_random_backtrace(void);
#endif

typedef struct arr_internal {int size; void *elts;} *array;
typedef struct dyntag_internal {} *dyntag;
typedef void *(*gen_fun)(void *,int);


/************************Arrays***********************/


array   new_ptrarray(int size, void *init) { 
  void **a=GC_malloc(size*4);
  array na = GC_malloc(sizeof(struct arr_internal));
  na->size=size;
  na->elts=a;
  void **a_end = a+size;
  for(;a<a_end;a++)
    *a=init;
  return na;
}

array   new_int8array(int size, char init) {
  array na=GC_malloc(sizeof(struct arr_internal));
  char *a= GC_malloc_atomic(size);
  na->size = size;
  na->elts = a;
  char *a_end = a+size;
  for(;a<a_end;a++)
    *a=init;
  return na;
}

array   new_int32array(int size,int init) {
  array na=GC_malloc(sizeof(struct arr_internal));
  int *a=GC_malloc_atomic(4*size);
  na->size = size;
  na->elts = a;
  int *a_end = a+size;
  for(;a<a_end;a++)
    *a=init;
  return na;
} 


array   new_floatarray(int size,double init) {
  array na=GC_malloc(sizeof(struct arr_internal));
  double *a=GC_malloc_atomic(8*size);
  na->size = size;
  na->elts = a;
  double *a_end = a+size;
  for(;a<a_end;a++)
    *a=init;
  return na;
} 

dyntag new_dyntag() {
  dyntag d = GC_malloc(sizeof(struct dyntag_internal));
  return d;
}


int l__unit__INT_LAB[0] = {};

int l__array__zero__INT_LAB[2] = {0,0};
int l__wordarray__zero__INT_LAB[2] = {0,0};
int l__floatarray__zero__INT_LAB[2] = {0,0};

void l__raise__subscript__INT_LAB(void)
{
  fprintf(stderr, "\nsubscript!\n");
  exit(255);
}

void l__overflow__INT_LAB(void)
{
  fprintf(stderr, "\noverflow!\n");
  exit(255);
}

void l__div__zero__INT_LAB(void)
{
  fprintf(stderr, "\ndivision by zero error!\n");
  exit(255);
}

void pop_never_null()
{
  fprintf(stderr, "Null pointer exception. \n\n");
  exit(255);
}

extern char *exnCNameRuntime(void* exn);
void pop_exn_handler_verbose(void* exn)
{
  /*  string msg; */

 fprintf(stderr, "Uncaught exception: %s\n",exnCNameRuntime(exn));

 fflush(stderr);

 exit(255);
}


void array_bounds_error(void)
{
  fprintf(stderr, "\narray bounds error!\n");
  exit(255);
}


void division_by_zero_error(void)
{
  fprintf(stderr, "\narray bounds error!\n");
  exit(255);
}

