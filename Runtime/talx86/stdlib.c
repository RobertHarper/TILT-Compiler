/* Must agree with stdlib.tali. */

#include "s.h"
#include "r.h"
#include "talx86.h"

typedef struct arr_internal {int size; void *elts;} *array;
typedef struct dyntag_internal {} *dyntag;

int l__unit__INT_LAB[0] = {};

int l__wordarray__zero__INT_LAB[2] = {0,0};
int l__array__zero__INT_LAB[2] = {0,0};
int l__floatarray__zero__INT_LAB[2] = {0,0};

array
new_ptrarray(int size, void *init)
{
	void **a=(void**)emalloc(size*4);
	array na = (array)emalloc(sizeof(struct arr_internal));
	void **a_end = a+size;
	na->size=size;
	na->elts=a;
	for(;a<a_end;a++)
		*a=init;
	return na;
}

array
new_int8array(int size, char init)
{
	int hsize = sizeof(struct arr_internal);
	int bufsize = hsize + size;
	void* buf = emalloc_atomic(bufsize);
	array na = (array)buf;
	char* a = (char*)buf + hsize;
	char* a_end = a+size;
	na->size = size;
	na->elts = a;
	for(;a<a_end;a++)
		*a=init;
	return na;
}

array
new_int32array(int size,int init)
{
	int hsize = sizeof(struct arr_internal);
	int bufsize = hsize + 4*size;
	void* buf = emalloc_atomic(bufsize);
	array na = (array)buf;
	int* a = (int*)((char*)buf + hsize);
	int* a_end = a+size;
	na->size = size;
	na->elts = a;
	for(;a<a_end;a++)
		*a=init;
	return na;
}

array
new_floatarray(int size,double init)
{
	int hsize = sizeof(struct arr_internal);
	int bufsize = hsize + 8*size;
	void* buf = emalloc_atomic(bufsize);
	array na = (array)buf;
	double* a = (double*)((char*)buf + hsize);
	double* a_end = a+size;
	na->size = size;
	na->elts = a;
	for(;a<a_end;a++)
		*a=init;
	return na;
}

dyntag
new_dyntag(void)
{
	int size = sizeof(struct dyntag_internal);
	dyntag d = (dyntag)emalloc(size);
	return d;
}

void
dead_exit(void)
{
	DIE("dead_exit; control should not reach here");
}

void
exn_handler_exit(exn exn)
{
	string msg = exnMessageRuntime(exn);
	int msg_len = stringlen(msg);
	char* msg_buf = stringbuf(msg);
	fprintf(stderr, "uncaught exception: %.*s\n", msg_len, msg_buf);
	fflush(stderr);
	exit(1);
}

int 
tal_print_ml_string(string s) 
{
  char * cs = mlstring2cstring_malloc(s);
  printf("%s",cs);
  efree(cs);
}
