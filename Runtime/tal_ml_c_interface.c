#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <errno.h> 	/* for errno */
#include "tal_ml_c_interface.h"

extern mem_t GC_malloc(int size);
extern void GC_free(mem_t dead);
extern mem_t GC_malloc_atomic(int size);
/* #define DO_MALLOC_DEBUG */
#ifdef DO_MALLOC_DEBUG
extern void GC_generate_random_backtrace(void);
#endif


void DIE(char* msg)
{
  fprintf(stderr,"tilt runtime error in tal_ml_c_interface.c: %s\n",msg);
  abort();
}


static mem_t alloc_space(int bytesNeeded) 
{
  return (mem_t) GC_malloc(bytesNeeded);
}


ptr_t alloc_record(val_t *fields, int count)
{
  int i;
  ptr_t rec;
  
  if (count == 0)
    return empty_record;

  rec = alloc_space(4 * count );

  /* Initialize record fields */
  for (i=0; i<count; i++)
    rec[i] = fields[i];
  return rec;
}

static val_t get_record(ptr_t rec, int which)
{
  return rec[which];
}

string alloc_string(int strlen, char *str)
{
  string na= (string) GC_malloc(sizeof(struct arr_internal));
  char *a= (char *) GC_malloc_atomic(strlen);
  na->size = strlen;
  na->elts = a;
  memcpy(a,str,strlen);
  return na;
}


string alloc_uninit_string(int strlen, char **raw)
{
  string na= (string) GC_malloc(sizeof(struct str_internal));
  char *a= (char *) GC_malloc_atomic(strlen);
  na->size = strlen;
  na->elts = a;
  *raw = a;
  return na;
}

/* Shorten a string */
void adjust_stringlen(string str, int newByteLen)
{
  assert(newByteLen <= str->size);
  str->size=newByteLen;
}

static ptr_t alloc_recrec(ptr_t rec1, ptr_t rec2)
{
  val_t fields[2];
  fields[0] = (val_t) rec1;
  fields[1] = (val_t) rec2;
  return alloc_record(fields, 2);
}

ptr_t alloc_manyint(int count, int v)
{
  val_t fields[100];
  int i;

  if (count > arraysize(fields))
    DIE("alloc_manyint not fully imped");

  for (i=0; i<count; i++)
    fields[i] = v;

  return alloc_record(fields, count);
}

ptr_t alloc_intint(int a, int b)
{
  ptr_t result = alloc_manyint(2,a);
  result[1] = b;
  return result;
}


static void* emalloc(size_t size)
{
  void* buf = GC_malloc(size);
  if (buf == NULL) {
    char* cmsg = strerror(errno);
    DIE(cmsg);
  }
  return buf;
}


int stringlen(string na)
{
  return na->size;
}

char *string_get_elts(string na) 
{
  return na->elts;
}


string cstring2mlstring_alloc(char *str)
{
  return alloc_string(strlen(str),str);
}


char* mlstring2cstring_buffer(string mlstring, int len, char *buf)
{
  int bytelen = stringlen(mlstring);
  char *raw = string_get_elts(mlstring);
  if ((bytelen+1) > len)
     DIE("fixed-length buffer too small");
  memcpy(buf,raw,bytelen);
  buf[bytelen] = 0;
  return (char *)buf;
}

#define buffersize 1024
static char buffer[buffersize];    

char* mlstring2cstring_static(string mlstring)
{
  return mlstring2cstring_buffer(mlstring, buffersize, buffer);
}

char* mlstring2cstring_malloc(string mlstring)
{
  int bytelen = stringlen(mlstring);
  char *buf = emalloc(bytelen+1);
  return mlstring2cstring_buffer(mlstring, bytelen+1, buf);
}


/* Exn stuff */
#define GET_RECORD(t, p, i) ((t)get_record((p),(i)))
#define GET_PTR(p, i) GET_RECORD(ptr_t,p,i)
#define GET_INT(p, i) GET_RECORD(int,p,i)

/* The components of an exception packet. */
#define PACKET_STAMP 0
#define PACKET_ARG   1
#define PACKET_NAME  2

#define GET_STAMP(exn) GET_PTR((exn), PACKET_STAMP)
#define GET_ARG(exn)   GET_PTR((exn), PACKET_ARG)
#define GET_NAME(exn)  GET_PTR((exn), PACKET_NAME) 

static ptr_t subStamp = NULL;
static ptr_t divStamp = NULL;
static ptr_t ovflStamp = NULL;
static ptr_t sysErrStamp = NULL;
static ptr_t libFailStamp = NULL;

/* The Basis initialization should call this code to 
 * tell the runtime about some key ML level exceptions.
 */
unit registerSubExnRuntime(ptr_t exn) 
{
  subStamp = GET_STAMP(exn);
  return empty_record;
}
unit registerDivExnRuntime(ptr_t exn) 
{
  divStamp = GET_STAMP(exn);
  return empty_record;
}
unit registerOvflExnRuntime(ptr_t exn) 
{
  ovflStamp = GET_STAMP(exn);
  return empty_record;
}
unit registerSysErrExnRuntime(ptr_t exn) 
{
  sysErrStamp = GET_STAMP(exn);
  return empty_record;
}
unit registerLibFailExnRuntime(ptr_t exn) 
{
  libFailStamp = GET_STAMP(exn);
  return empty_record;
}

ptr_t getSubStamp()
{
  if (subStamp == NULL) {
    DIE("SubStamp uninitialized");
  }
  return subStamp;
}

ptr_t getDivStamp()
{
  if (divStamp == NULL) {
    DIE("DivStamp uninitialized");
  }
  return divStamp;
}

ptr_t getOvflStamp()
{
  if (ovflStamp == NULL) {
    DIE("OverflowStamp uninitialized");
  }
  return ovflStamp;
}

ptr_t getSysErrStamp()
{
  if (sysErrStamp == NULL) {
    DIE("SysErrStamp uninitialized");
  }
  return sysErrStamp;
}

ptr_t getLibFailStamp()
{
  if (libFailStamp == NULL) {
    DIE("libFailStamp uninitialized");
  }
  return libFailStamp;
}

ptr_t mkExn(string exnname, ptr_t exnstamp, val_t exnarg)
{
  val_t fields[3];
  ptr_t exn;
  fields[PACKET_STAMP] = (val_t) exnstamp;
  fields[PACKET_ARG]   = exnarg;
  fields[PACKET_NAME]  = (val_t)exnname;
  exn = alloc_record(fields, 3);
  return exn;
}

ptr_t mkDivExn(void) 
{
  string exnname = cstring2mlstring_alloc("Div");
  ptr_t exnstamp = getDivStamp();
  ptr_t exnarg = empty_record;
  ptr_t exn = mkExn(exnname, exnstamp, (val_t)exnarg);
  return exn;
}

ptr_t mkOverflowExn(void) 
{
  string exnname = cstring2mlstring_alloc("Overflow");
  ptr_t exnstamp = getOvflStamp();
  ptr_t exnarg = empty_record;
  ptr_t exn = mkExn(exnname, exnstamp, (val_t)exnarg);
  return exn;
}

ptr_t mkSubscriptExn(void) 
{
  string exnname = cstring2mlstring_alloc("Subscript");
  ptr_t exnstamp = getSubStamp();
  ptr_t exnarg = empty_record;
  ptr_t exn = mkExn(exnname, exnstamp, (val_t)exnarg);
  return exn;
}

ptr_t mkSysErrExn(string msg, int isSome, int e)
{
  string exnname = cstring2mlstring_alloc("SysErr");
  ptr_t exnstamp = getSysErrStamp();
  ptr_t errno_option = isSome ? alloc_manyint(1, e) : 0;
  ptr_t exnarg = alloc_recrec((ptr_t) msg, errno_option);
  ptr_t exn = mkExn(exnname, exnstamp, (val_t)exnarg);
  return exn;
}

string exnNameRuntime(ptr_t exn)
{
  return (string) GET_NAME(exn);
}

char *exnCNameRuntime(ptr_t exn)
{
  return mlstring2cstring_malloc(exnNameRuntime(exn));
}

string exnMessageRuntime(ptr_t exn)
{
  char buf[1024];
  ptr_t exnstamp = GET_STAMP(exn);
  
  if (exnstamp == getDivStamp()) {
    strcpy(buf, "divide by zero");
  } else if (exnstamp == getOvflStamp()) {
    strcpy(buf, "overflow");
  } else if (exnstamp == getLibFailStamp()) {
    const char* prefix = "LibFail: ";
    string msg = (string) GET_ARG(exn);
    int msg_len = stringlen(msg);
    assert(sizeof(buf) > sizeof(prefix) + msg_len);
    sprintf(buf, "%s%.*s", prefix, msg_len, string_get_elts(msg));
  } else if (exnstamp == getSysErrStamp()) {
    ptr_t exnarg = GET_ARG(exn);
    const char* prefix = "SysErr: ";
    string msg = (string) GET_PTR(exnarg, 0);
    int msg_len = stringlen(msg);
    ptr_t err_option = GET_PTR(exnarg, 1);
    int isSome = err_option != 0;
    int err = isSome ? GET_INT(err_option, 0) : 0;
    char err_buf[40];
    if (isSome) {
      sprintf(err_buf, " (errno=%d)", err);
    } else {
      *err_buf = '\0';
    }
    assert(sizeof(buf) > sizeof(prefix) + msg_len + strlen(err_buf));
    sprintf(buf, "%s%.*s%s", prefix, msg_len, string_get_elts(msg), err_buf);
    } else {
    string name = (string) GET_NAME(exn);
    int name_len = stringlen(name);
    assert(sizeof(buf) > name_len);
    sprintf(buf, "%.*s", name_len, string_get_elts(name));
  }
  return cstring2mlstring_alloc(buf);
}

ptr_t cons_rec_alloc(ptr_t car, ptr_t list)
{
  return alloc_recrec(car, list);
}

ptr_t cons_val_alloc(val_t val, ptr_t list)
{
  val_t fields[2];
  fields[0] = val;
  fields[1] = (val_t) list;
  return alloc_record(fields, 2);
}

ptr_t stringpair_ctoml_alloc(char* a, char* b)
{
  string ml_a = cstring2mlstring_alloc(a);
  string ml_b = cstring2mlstring_alloc(b);
  return alloc_recrec((ptr_t) ml_a, (ptr_t) ml_b);
}


static int string_list_length(string_list_long list)
{
  int length = 0;
  while (list) {
    length++;
    list = (string_list_long) list->cdr;
  }
  return length;
}

/* Translate a list of ml strings to an array of c strings */
char** string_list_to_array_malloc(string_list_long list)
{
  int i;
  int length = string_list_length(list);
  char** v = (char**)emalloc((length + 1)*sizeof(char*));
  for (i=0; i<length; i++) {
    v[i] = mlstring2cstring_malloc(list->car);
    list = (string_list_long) list->cdr;
  }
  v[length] = 0;
  return v;
}

void free_string_array(char** arr)
{
  int i;
  for (i=0; arr[i]; i++) {
    GC_free((void *) arr[i]);
  }
  GC_free((void *) arr);
}

string_list array_to_string_list(char** arr)
{
  /* Convert a NULL-terminated array of c strings into a value of type string list.
   */
  char* string = *arr;
  char** next = arr+1;
  if (string == NULL) return (ptr_t) 0;
  else {
    ptr_t car = (ptr_t) cstring2mlstring_alloc(string);
    ptr_t cdr = array_to_string_list(next);
    return cons_rec_alloc(car,cdr);
  }
}
