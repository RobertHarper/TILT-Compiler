#include <string.h>
#include <stdarg.h>
#include "general.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/resource.h>
#include <math.h>
#include <sys/wait.h>
#include <stdio.h>
#include <dirent.h>
#include <stropts.h>
#include <poll.h>
#include <sys/utsname.h>
#include <sys/resource.h>
#if (defined alpha_osf)
#include <float.h>
int ftime(struct timeb *tp);   /* This should be in sys/timeb.h but isn't on the Alpha */
#elif (defined solaris)
#include <ieeefp.h>
#endif
#include <fcntl.h>
#include <libgen.h>

#include "tag.h"
#include "thread.h"
#include "create.h"
#include "posix.h"
#include "global.h"
#include "stack.h"
#include "exn.h"

#ifdef sparc
int getrusage(int who, struct rusage *rusage);  /* Not in header files for some reason */
int ftime(struct timeb *tp);
#endif

#define PAIR(t,t1,t2,tlong,tstruct) struct tstruct { t1 first; t2 second; }; \
                                    typedef struct tstruct *tlong; \
                                    typedef ptr_t t;
                           
#define TRIPLE(t,t1,t2,t3,tlong,tstruct) struct tstruct { t1 first; t2 second; t3 third; }; \
                                         typedef struct tstruct *tlong; \
                                         typedef ptr_t t;

#define LIST(t,tlist,tlong,tstruct) struct tstruct \
                        { t car; \
			  ptr_t cdr; \
			}; \
                        typedef struct tstruct *tlong; \
                        typedef ptr_t tlist;

typedef ptr_t unit;  /* The value 256 */
typedef int bool;
typedef int posint;
typedef int word;
typedef char word8;
typedef ptr_t word8array;
typedef const ptr_t word8vector;
typedef ptr_t string;
PAIR(intpair,int,int,intpair_long,intpair_struct)
typedef intpair intpair_option;
typedef string string_option;
TRIPLE(inttriple,int,int,int,inttriple_long,inttriple_struct)
PAIR(wordpair,word,word,wordpair_long,wordpair_struct)
PAIR(intword,int,word,intword_long,intword_struct)
LIST(intword,intword_list,intword_list_long,intword_list_struct)
LIST(word,word_list,word_list_long,word_list_struct)
LIST(string,string_list,string_list_long,string_list_struct)
PAIR(string_stringlist,string,string_list,string_stringlist_long,string_stringlist_struct)
TRIPLE(string_word_stringlist,string,word,string_list,string_word_stringlist_long,string_word_stringlist_struct)

struct swwss
{ string a; word b; word c; string d; string e; };
typedef struct swwss *string_word_word_string_string;

struct iiiii
{ int a; int b; int c; int d; int e; };
typedef struct iiiii *int_int_int_int_int;

/* 
type statrep = (int * word * word * word * word * word * 
		   word * int * int * int * int) */

struct statrep_struct 
{
  int ftype;
  word mode;
  word ino;
  word dev;
  word nlink;
  word uid;
  word gid;
  int size;
  int atime;
  int mtime;
  int ctime;
};
typedef struct statrep_struct *statrep;

/* type flock_rep = int * int * Position.int * Position.int * int */

struct flock_rep_struct
{
  int a;
  int b;
  posint c;
  posint d;
  int e;
};
typedef struct flock_rep_struct *flock_rep;

/* type tm = (int * int * int * int * int * int * int * int * int) */
struct mltm_struct
{
  int a;
  int b;
  int c;
  int d;
  int e;
  int f;
  int g;
  int h;
  int i;
};
typedef struct mltm_struct *mltm;

struct string_mltm_struct
{
  char *a;
  mltm b;
};
typedef struct string_mltm_struct *string_mltm;

/* type termio_rep = (word *       	(* iflags *)
		   word *       	(* oflags *)
		   word *       	(* cflags *)
		   word *       	(* lflags *)
		   Word8Vector.vector *	(* cc *)
		   word *		(* inspeed *)
		   word			(* outspeed *)
		   )
*/
struct termio_rep_struct
{
  word iflags;
  word oflags;
  word cflags;
  word lflags;
  word cc;
  word inspeed;
  word outspeed;
};
typedef struct termio_rep_struct *termio_rep;


static int stringlen(ptr_t string)
{
  tag_t tag = string[-1];
  int len;
  while (TYPE_IS_FORWARD(GET_TYPE(tag)))
    tag = ((ptr_t)tag) [-1];
  assert(GET_TYPE(tag) == WORD_ARRAY_TYPE);
  len = GET_WORD_ARRAY_LEN(tag);
  return len;
}

static void runtime_error(int e);
static void runtime_error_msg(char* msg);
static void runtime_error_fmt(const char* fmt, ...);

static void unimplemented(int lineno)
{
  runtime_error_fmt("function not implemented at %s:%d", __FILE__, lineno);
}
#define UNIMP() unimplemented(__LINE__); assert(0);

static void* emalloc(size_t size)
{
  void* buf = malloc(size);
  if (buf == NULL)
    runtime_error(errno);
  return buf;
}

string cstring2mlstring_alloc(char *str)
{
  return alloc_string(strlen(str),str);
}


char* mlstring2cstring_buffer(string mlstring, int len, char *buf)
{
  int bytelen = stringlen(mlstring);
  char *raw = (char *)mlstring;
  if ((bytelen+1) > len) {
     runtime_error_msg("fixed-length buffer too small");
     assert(0);
  }
  memcpy(buf,raw,bytelen);
  buf[bytelen] = 0;
  return (char *)buf;
}

static char* mlstring2cstring_static(string mlstring)
{
  static char buf[1024];
  return mlstring2cstring_buffer(mlstring, sizeof(buf), buf);
}

static char* mlstring2cstring_malloc(string mlstring)
{
  int bytelen = stringlen(mlstring);
  char *buf = emalloc(bytelen+1);
  return mlstring2cstring_buffer(mlstring, bytelen+1, buf);
}

static void runtime_error(int e)
{
   string exnname = cstring2mlstring_alloc("VAL$RuntimeError");
   int exnstamp = RuntimeStamp();
   raise_exn(exnname, exnstamp, e, 0);
}

static void runtime_error_msg(char* msg)
{
   string exnname = cstring2mlstring_alloc("VAL$RuntimeErrorPRIME");
   int exnstamp = RuntimePrimeStamp();
   string exnarg = cstring2mlstring_alloc(msg);
   raise_exn(exnname, exnstamp, (val_t) exnarg, 1);
}

static void runtime_error_fmt(const char* fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  (void) vfprintf(stderr, fmt, ap);
  va_end(ap);
   /*
   char buf[1024];
   va_list args;
   va_start(args, fmt);
   vsprintf(buf, fmt, args);
   va_end(args);
   runtime_error_msg(buf);
   */
}

double ln(double arg)
{
  return log(arg);
}


int getRoundingMode(unit ignored)
{
#ifdef alpha_osf
  unsigned int mode = read_rnd();
  switch (mode) {
    case FP_RND_RZ: return 1;
    case FP_RND_RN: return 0;
    case FP_RND_RP: return 2;
    case FP_RND_RM: return 3;
  }
  runtime_error_fmt("read_rnd retrurned unrecognized mode %d", mode);
#elif (defined solaris)
  fp_rnd mode = fpgetround();
  switch (mode) {
    case FP_RZ: return 1;
    case FP_RN: return 0;
    case FP_RP: return 2;
    case FP_RM: return 3;
  }
  runtime_error_fmt("fpgetround retrurned unrecognized mode %d", mode);
#endif
}


ptr_t setRoundingMode(int ml_mode)
{
#if (defined alpha_osf)
  int mode = 0;
  switch (ml_mode) {
    case 1: mode = FP_RND_RZ; break;
    case 0: mode = FP_RND_RN; break;
    case 2: mode = FP_RND_RP; break;
    case 3: mode = FP_RND_RM; break;
    default : runtime_error_fmt("setRoundingMode given unknown ML rounding mode %d", ml_mode);
    }
  write_rnd(mode);
#elif (defined solaris)
  fp_rnd mode = 0;
  switch (ml_mode) {
    case 1: mode = FP_RZ; break;
    case 0: mode = FP_RN; break;
    case 2: mode = FP_RP; break;
    case 3: mode = FP_RM; break;
    default : runtime_error_fmt("setRoundingMode given unknown ML rounding mode %d", ml_mode);
    }
  fpsetround(mode);
#endif  
  return empty_record;
}

ptr_t exnNameRuntime(ptr_t exn)
{
  return (ptr_t) get_record(exn,2);
}

ptr_t exnMessageRuntime(ptr_t exn)
{
  char buf[1024];
  char* msg = NULL;
  val_t exnstamp = get_record(exn,0);
  
  if (exnstamp == *getDivExn())
    msg = "divide by zero";
  else if (exnstamp == *getOverflowExn())
    msg = "overflow";
  else if (exnstamp == RuntimeStamp()) {
    int e = get_record(exn,1);
    sprintf(buf, "RuntimeError: %s (errno=%d)", strerror(e), e);
    msg = buf;
  } else if (exnstamp == RuntimePrimeStamp()) {
    char* prefix = "RuntimeError': ";
    ptr_t errmsg = (ptr_t) get_record(exn,1);
    int len = stringlen(errmsg);
    assert(len < sizeof(buf) - sizeof(prefix));
    sprintf(buf, "%s%.*s", prefix, len, (char*) errmsg);
    msg = buf;
  } else {
    ptr_t exnname = (ptr_t) get_record(exn,2);
    int len = stringlen(exnname);
    assert(len < sizeof(buf));
    sprintf(buf, "%.*s", len, (char*) exnname);
    msg = buf;
  }
  return cstring2mlstring_alloc(msg);
}

/* extract the exponent */
int real_logb(double arg)
{
  int biasedExp = 0;
#ifdef alpha_osf
  unsigned long temp;
  assert(sizeof(unsigned long) ==  sizeof(double));
  *((double *)(&temp)) = arg;
  biasedExp = (int)(temp >> 52);
#elif (defined solaris)
  int temp[2];
  assert((2 * (sizeof(unsigned int))) == sizeof(double));
  *((double *)temp) = arg;
  biasedExp = (int)(temp[1]);
#else
#error "unimplemented"
#endif
  biasedExp -= 1023;
  return biasedExp;
}


intpair ml_timeofday()
{
  ptr_t result;
  struct timeval tp;
  struct timezone tzp;
  if (gettimeofday(&tp, &tzp) == -1)
    runtime_error(errno);
  return alloc_intint(tp.tv_sec,tp.tv_usec);
}

mltm ctm2mltm_alloc(struct tm *tm)
{
  ptr_t result = alloc_manyint(9,0);
  result[0] = tm->tm_sec;
  result[1] = tm->tm_min;
  result[2] = tm->tm_hour;
  result[3] = tm->tm_mday;
  result[4] = tm->tm_mon;
  result[5] = tm->tm_year;
  result[6] = tm->tm_wday;
  result[7] = tm->tm_yday;
  result[8] = tm->tm_isdst;
  return ((mltm) result);
}

string posix_ascTime (mltm mltm)
{
  char *result = asctime((struct tm *) mltm);
  ptr_t res = alloc_string(strlen(result),result);
  return (word8vector) res;
}

mltm posix_localTime (intpair sec_musec)
{
  time_t t = *((int *)sec_musec);
  struct tm *tm;
  tm = localtime (&t);
  return ctm2mltm_alloc(tm);
}

mltm posix_gmTime (intpair sec_musec)
{
  time_t t = *((int *)sec_musec);
  struct tm *tm;
  tm = gmtime (&t);
  return ctm2mltm_alloc(tm);
}

intpair posix_mkTime(mltm unused)
{
  UNIMP();
}

string posix_strfTime(string_mltm unused)
{
  UNIMP();
}

string posix_error_msg(int unused)
{
  UNIMP();
}

string posix_error_name(int unused)
{
  UNIMP();
}

int posix_error_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<arraysize(tbl); i++) {
    char *name = tbl[i].name;
    if (!(strcmp(name,carg)))
      return tbl[i].id;
  }
  runtime_error_fmt("posix_error_num could not find entry '%s'",carg);
}

string posix_os_tmpname(unit unused)
{
  char *buf = NULL;
  ptr_t res = alloc_uninit_string(L_tmpnam,&buf);
  char *result = tmpnam(buf);
  assert(result == buf);
  adjust_stringlen(res,strlen(buf));
  return (word8vector) res;
}

intword_list posix_os_poll(intword_list fd_event_list, intpair_option sec_usec_option)
{
  struct pollfd fds[10];
  unsigned long count = 0;
  int i, timeout = INFTIM;
  ptr_t result = 0;        /* nil */

  if (sec_usec_option != 0)  /* is it NONE? */
    {
      int *sec_usec = (int *)sec_usec_option;
      int sec = sec_usec[0];
      int usec = sec_usec[1];
      timeout = 1000 * sec + usec / 1000;
    }
  while (fd_event_list != 0) /* is it nil? */
    {
      int *fd_event = (int *)(fd_event_list[0]);
      int fd = fd_event[0];
      int event = fd_event[1];
      fd_event_list = (intword_list)(fd_event_list[1]);
      fds[count].fd = fd;
      fds[count].events = event;
      count++;
      if (count == arraysize(fds))
	runtime_error_fmt("posix_os_poll has static limit of %d file descriptors", arraysize(fds));
    }
  if (poll(fds,count,timeout) == -1)
    runtime_error(errno);
  for (i=count-1; i>=0; i--) {
    ptr_t car = alloc_intint(fds[i].fd, fds[i].revents);
    result = alloc_recrec(car,result);
  }
  return result;
}

int posix_io_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<arraysize(io_values); i++)
    {
      char *name = io_values[i].name;
      if (!(strcmp(name,carg)))
	return io_values[i].id;
    }
  runtime_error_fmt("posix_io_num could not find entry '%s'",carg);
}

intpair posix_io_pipe(unit unused)
{
  UNIMP();
}

int posix_io_dup(int unused)
{
  UNIMP();
}

unit posix_io_dup2(int unused1, int unused2)
{
  UNIMP();
}

unit posix_io_close(int fd)
{
  if (close(fd) == -1)
    runtime_error(errno);
  return empty_record;
}


/* ML strings are not null-terminated */
word8vector posix_io_read(int fd, int size)
{
  ptr_t *alloc, *limit;
  char *buf = NULL;
  int bytes_read;
  ptr_t res;

  assert(size >= 0);
  res = alloc_uninit_string(size,&buf);
  bytes_read = read(fd,buf,size);
  if (bytes_read == -1)
    runtime_error(errno);
  assert(bytes_read <= size);
  adjust_stringlen(res,bytes_read);
  return (word8vector) res;
}


int posix_io_readbuf(int fd, word8array buf, int len, int start)
{ 
  int bytes_read = read(fd, ((char *)buf) + start, len);
  if (bytes_read == -1)
    runtime_error(errno);
  return bytes_read;
}

int posix_io_writebuf(int fd, word8array buf, int len, int start)
{
  int written = write(fd, ((char *)buf) + start, len);
  if (written == -1)
    runtime_error(errno);
  return written;
}

int posix_io_fcntl_d(int unused1, int unused2)
{
  UNIMP();
}

word posix_io_fcntl_gf(int unused)
{
  UNIMP();
}

unit posix_io_fcntl_sfd(int unused1, word unused2)
{
  UNIMP();
}

wordpair posix_io_fcntl_gfl(int unused)
{
  UNIMP();
}

unit posix_io_fcntl_sfl(int unusde1, word unused2)
{
  UNIMP();
}

flock_rep posix_io_fcntl_l(int unused1, int unused2, flock_rep unused3)
{
  UNIMP();
}

int posix_io_lseek(int filedes, int offset, int whence)
{
  int result_pos = lseek(filedes, offset, whence);
  if (result_pos == -1)
    runtime_error(errno);
  return result_pos;
}

unit posix_io_fsync(int fd)
{
  int status = fsync(fd);
  if (status == -1)
    runtime_error(errno);
  return empty_record;
}

int posix_procenv_getpid(unit unused)
{
  int pid = (int)getpid();
  if (pid == -1)
    runtime_error(errno);
  return pid;
}

int posix_procenv_getppid(unit unused)
{
  UNIMP();
}

word posix_procenv_getuid(unit unused)
{
  UNIMP();
}

word posix_procenv_geteuid(unit unused)
{
  UNIMP();
}

word posix_procenv_getgid(unit unused)
{
  UNIMP();
}

word posix_procenv_getegid(unit unused)
{
  UNIMP();
}

unit posix_procenv_setuid(word unused)
{
  UNIMP();
}

unit posix_procenv_setgid(word unused)
{
  UNIMP();
}

word_list posix_procenv_getgroups(unit unused)
{
  UNIMP();
}

string posix_procenv_getlogin(unit unused)
{
  UNIMP();
}

int posix_procenv_getpgrp(unit unused)
{
  UNIMP();
}

int posix_procenv_setsid(unit unused)
{
  UNIMP();
}

unit posix_procenv_setpgid(int unused1, int unused2)
{
  UNIMP();
}

static ptr_t stringpair_ctoml_alloc(char* a, char* b)
{
  string ml_a = cstring2mlstring_alloc(a);
  string ml_b = cstring2mlstring_alloc(b);
  return alloc_recrec(ml_a, ml_b);
}

static ptr_t consrec_alloc(val_t val, ptr_t list)
{
  return alloc_recrec((ptr_t) val, list);
}

string_stringlist posix_procenv_uname(unit unused)
{
  ptr_t acc = NULL;
  struct utsname name;
  if (uname(&name) == -1)
    runtime_error(errno);

  #define ACC(n,v) acc = consrec_alloc((val_t) stringpair_ctoml_alloc(n, v), acc);

  ACC("sysname", name.sysname);
  ACC("nodename", name.nodename);
  ACC("release", name.release);
  ACC("version", name.version);
  ACC("machine", name.machine);

  #undef ACC

  return acc;
}

int posix_tty_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<arraysize(tty_values); i++) {
    char *name = tty_values[i].name;
    if (!(strcmp(name,carg)))
      return tty_values[i].id;
  }
  runtime_error_fmt("posix_tty_num could not find entry '%s'",carg);
  return 0;
}

termio_rep  posix_tty_tcgetattr(int unused)
{
  UNIMP();
}

unit posix_tty_tcsetattr(int unused1, int unused2, termio_rep  unused3)
{
  UNIMP();
}

unit posix_tty_tcsendbreak(int unused1, int unused2)
{
  UNIMP();
}

unit posix_tty_tcdrain(int unused)
{
  UNIMP();
}

unit posix_tty_tcflush(int unused1, int unused2)
{
  UNIMP();
}

unit posix_tty_tcflow(int unused1, int unused2)
{
  UNIMP();
}

int posix_tty_tcgetpgrp(int unused)
{
  UNIMP();
}

unit posix_tty_tcsetpgrp(int unused1, int unused2)
{
  UNIMP();
}

string_word_stringlist posix_sysdb_getgrgid(word unused)
{
  UNIMP();
}

string_word_stringlist posix_sysdb_getgrnam(string unused)
{
  UNIMP();
}

string_word_word_string_string posix_sysdb_getpwuid(word unused)
{
  UNIMP();
}

string_word_word_string_string posix_sysdb_getpwnam(string unused)
{
  UNIMP();
}

int posix_procenv_time(unit unused)
{
  UNIMP();
}

int_int_int_int_int posix_procenv_times(unit unused)
{
  UNIMP();
}

/* This code relies on NONE being represented by 0. */
string_option posix_procenv_getenv(string mlname)
{
  char *cname = mlstring2cstring_static(mlname);  /* Don't need to free this */
  char *cvalue = getenv(cname);                   /* Don't need to free this and cannot modify it */
  ptr_t mlvalue = (cvalue == NULL) ? 0 : alloc_string(strlen(cvalue),cvalue);
  return mlvalue;
}

string_list posix_procenv_environ(unit unused)
{
  UNIMP();
}

string posix_procenv_ctermid(unit unused)
{
  UNIMP();
}

string posix_procenv_ttyname(int unused)
{
  UNIMP();
}

bool posix_procenv_isatty(int unused)
{
  UNIMP();
}

int posix_process_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<arraysize(process_values); i++)
    {
      char *name = process_values[i].name;
      if (!(strcmp(name,carg)))
	return process_values[i].id;
    }
  runtime_error_fmt("posix_process_num could not find entry '%s'",carg);
  return 0;
}

word posix_process_sysconf(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<arraysize(sysconf_keys); i++)
    {
      char *name = sysconf_keys[i].name;
      if (!(strcmp(name,carg)))
	return sysconf(sysconf_keys[i].id);
    }
  runtime_error_fmt("posix_process_sysconf could not find entry '%s'",carg);
  return 0;
}

int posix_process_fork(unit unused)
{
  int code = fork();
  if (code == -1)
    runtime_error(errno);
  return code;
}

unit posix_process_exec(string path, string_list args)
{
  int code, i, length = 0, errorsave;
  string_list cur = args;
  char **argv;
  while (cur) {
    length++;
    cur = ((string_list_long) cur)->cdr;
  }
  argv = emalloc((length + 1) * sizeof(char *));
  for (cur=args, i=0; i<length; i++) {
    argv[i] = mlstring2cstring_malloc(((string_list_long) cur)->car);
    cur = ((string_list_long) cur)->cdr;
  }
  argv[length] = NULL;
  code = execv(mlstring2cstring_malloc(path),argv);
  errorsave = errno;
  /* couldn't exec, might as well free things */
  for (i=0; i<length; i++)
    free(argv[i]);
  free(argv);
  runtime_error(errorsave);
  return 0;
}


unit posix_process_exece(string unused1, string_list unused2, string_list unused)
{
  UNIMP();
}

unit posix_process_execp(string unused1, string_list unused)
{
  UNIMP();
}

inttriple posix_process_waitpid(int argpid, word options)
{
  int status;
  ptr_t result;
  int pid = waitpid(argpid, &status, options);
  int how, val;
  if (pid < 0)
    runtime_error(errno);

  if (WIFEXITED(status)) {
    how = 0;
    val = WEXITSTATUS(status);
  }
  else if (WIFSIGNALED(status)) {
    how = 1;
    val = WTERMSIG(status);
  }
  else if (WIFSTOPPED(status)) {
    how = 2;
    val = WSTOPSIG(status);
  }
  else
    runtime_error_fmt("POSIX waitpid got back unknown child status = %d", status);

  result = alloc_manyint(3,0);
  result[0] = pid;
  result[1] = how;
  result[2] = val;
  return (inttriple) result;
}

unit posix_process_exit(word8 status)
{
  exit(status);
  return 0; /* NOTREACHED */
}

unit posix_process_kill(int unused1, int unused2)
{
  UNIMP();
}

int posix_process_alarm(int unused)
{
  UNIMP();
}

unit posix_process_pause(unit unused)
{
  UNIMP();
}

int posix_process_sleep(int unused)
{
  UNIMP();
}

int posix_signal_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<arraysize(signal_values); i++)
    {
      char *name = signal_values[i].name;
      if (!(strcmp(name,carg)))
	return signal_values[i].id;
    }
  runtime_error_fmt("posix_signal_num could not find entry '%s'",carg);
  return 0;
}

word posix_filesys_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<arraysize(filesys_values); i++)
    {
      char *name = filesys_values[i].name;
      if (!(strcmp(name,carg)))
	return filesys_values[i].id;
    }
  runtime_error_fmt("posix_filesys_num could not find entry '%s'",carg);
  return 0;
}

int posix_filesys_opendir(string dirname)
{
  char* cdir = mlstring2cstring_static(dirname);
  DIR *dir = opendir(cdir);
  return (int)dir;
}

string posix_filesys_readdir(int arg)
{
  DIR *dir = (DIR *)arg;
  struct dirent *entry = readdir(dir);
  if (entry == NULL)
    return cstring2mlstring_alloc("");
  else 
    return cstring2mlstring_alloc(entry->d_name);
}

unit posix_filesys_rewinddir(int arg)
{
  DIR *dir = (DIR *)arg;
  rewinddir(dir);
  return empty_record;
}

unit posix_filesys_closedir(int arg)
{
  DIR *dir = (DIR *)arg;
  closedir(dir);
  return empty_record;
}

unit posix_filesys_chdir(string dirname)
{
  const char *cdirname = mlstring2cstring_static(dirname);
  if (chdir (cdirname) == -1)
    runtime_error(errno);
  return empty_record;
}

string posix_filesys_getcwd(unit unused)
{
  char buffer[1024];
  if (getcwd(buffer, sizeof(buffer) - 1) == NULL) {
    if (errno == ERANGE) {
      printf("posix_filesys_getcwd: buffer too small\n");
      assert(0);
    }
    runtime_error(errno);
    assert(0);
  }
  return cstring2mlstring_alloc(buffer);
}

int posix_filesys_openf(string filename, word oflag, word mode)
{
  const char *cfilename = mlstring2cstring_static(filename);
  int fd = open(cfilename,oflag,mode);
  if (fd == -1) {
    runtime_error(errno);
    return 0;
  } else return fd;
}

int posix_filesys_umask(word unused)
{
  UNIMP();
}

unit posix_filesys_link(string unusd1, string unused2)
{
  UNIMP();
}

unit posix_filesys_rename(string from, string to)
{
  char buf[1024];
  char *cfrom = mlstring2cstring_static(from);
  char *cto = mlstring2cstring_buffer(to, sizeof(buf), buf);  /* can't use ..._static twice */
  rename(cfrom,cto);
  return empty_record;
}

unit posix_filesys_symlink(string unused1, string unused)
{
  UNIMP();
}

unit posix_filesys_mkdir(string mlDir, word mode)
{
  char *cDir = mlstring2cstring_static(mlDir);
  int status;
    printf("mlDir = %d\n", mlDir);
    printf("mlDir = *%20s*\n", mlDir);
    printf("trying mkdir (\"%s\", %x) failed\n", cDir, mode);
status = mkdirp(cDir,mode);
  if (status) {
    printf("mkdir (\"%s\", %x) failed with errno = %d\n", cDir, mode, status);
    UNIMP();
  }
  return empty_record;
}

unit posix_filesys_mkfifo(string unused1, word unused)
{
  UNIMP();
}

unit posix_filesys_unlink(string arg)
{
  char* path = mlstring2cstring_static(arg);
  int result = unlink(path);
  if (result == -1)
    runtime_error(errno);
  return empty_record;
}

unit posix_filesys_rmdir(string unused)
{
  UNIMP();
}

string posix_filesys_readlink(string link)
{
  char buf[1024];
  int status = readlink(mlstring2cstring_static(link), buf, sizeof(buf) - 1);
  if (status == -1) 
    runtime_error(errno);
  else {
    buf[status] = 0;
    return cstring2mlstring_alloc(buf);
  }
}

unit posix_filesys_ftruncate(int unused1, int unused)
{
  UNIMP();
}

#define MODE_BITS (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID)

static statrep cstat2mlstat_alloc(struct stat *buffer)
{
  ptr_t record = alloc_manyint(11,0);
  statrep s = (statrep)record;
  s->ftype = buffer->st_mode & S_IFMT;
  s->mode = buffer->st_mode & MODE_BITS;
  s->ino = buffer->st_ino;
  s->dev = buffer->st_dev;
  s->nlink = buffer->st_nlink;
  s->uid = buffer->st_uid;
  s->gid = buffer->st_gid;
  s->size = buffer->st_size;
  s->atime = buffer->st_atime;
  s->mtime = buffer->st_mtime;
  s->ctime = buffer->st_ctime;
  return s;
}



statrep posix_filesys_stat(string name)
{
  struct stat buffer;
  if (stat(mlstring2cstring_static(name),&buffer) == -1)
    runtime_error(errno);
  return cstat2mlstat_alloc(&buffer);
}

statrep posix_filesys_lstat(string name)
{
  struct stat buffer;
  if (lstat(mlstring2cstring_static(name),&buffer) == -1)
    runtime_error(errno);
  return cstat2mlstat_alloc(&buffer);
}

statrep posix_filesys_fstat(int filedesc)
{
  struct stat buffer;
  if (fstat(filedesc,&buffer) == -1)
    runtime_error(errno);
  return cstat2mlstat_alloc(&buffer);
}

bool posix_filesys_access(string name, word mode)
{
  char *cname = mlstring2cstring_static(name);
  int code = access(cname,mode);
  if (code == 0)
    return 1; /* true */
  else {
    /* Only allowed to raise an exception for errors unrelated
     * to resolving the pathname and permissions.
     */
    switch (errno) {
    default:
      return 0; /* false */

    case EINTR:
      runtime_error(errno);
      return 0;	/* notreached */
    }
  }
}

unit posix_filesys_chmod(string unused1, word unused2)
{
  UNIMP();
}

unit posix_filesys_fchmod(int unused1, word unused2)
{
  UNIMP();
}

unit posix_filesys_chown(string unused1, word unused2, word unused3)
{
  UNIMP();
}

unit posix_filesys_fchown(int unused1, word unused2, word unused)
{
  UNIMP();
}

unit posix_filesys_utime(string unused1, int unused2, int unused)
{
  UNIMP();
}


intpair posix_filesys_pathconf(string unusde1, string unused)
{
  UNIMP();
}

intpair posix_filesys_fpathconf (int unused1, string unused)
{ 
  UNIMP();
}


word posix_io_fcntl_gfd()
{ 
  UNIMP();
}

ptr_t til_selfusage()
{
  val_t fields[4];
  int masks[4];
  struct rusage rusage;
  if (getrusage(RUSAGE_SELF, &rusage) == -1)
    runtime_error(errno);
  fields[0] = rusage.ru_utime.tv_sec;
  fields[1] = rusage.ru_utime.tv_usec;
  fields[2] = rusage.ru_stime.tv_sec;
  fields[3] = rusage.ru_stime.tv_usec;
  masks[0] = masks[1] = masks[2] = masks[3] = 0;
  return alloc_record(fields, masks, 4);
}

ptr_t til_realtime()
{
  struct timeb tp;
  ftime(&tp);
  return alloc_intint(tp.time, tp.millitm);
}


ptr_t printString(ptr_t str)
{
  int len = stringlen(str);
  printf("%.*s", len, (char*) str);
  return empty_record;
}

/* Not really appropriate here, but I'm planning on reorganizing anyway. */

static char* commandline_cmd = NULL;
static char** commandline_argv = NULL;

void setCommandLine(char* cmd, char** argv)
{
  commandline_cmd = cmd;
  commandline_argv = argv;
}

string commandline_name(unit ignored)
{
  assert(commandline_name != NULL);
  return cstring2mlstring_alloc(commandline_cmd);
}

string_list array_to_string_list(char** arr)
{
  /* Convert a NULL-terminated array of strings into a value of type string list.
   */
  char* string = *arr;
  char** next = arr+1;
  if (string == NULL) return (ptr_t) 0;
  else {
    ptr_t car = cstring2mlstring_alloc(string);
    ptr_t cdr = array_to_string_list(next);
    return alloc_recrec(car, cdr);
  }
}

string_list commandline_arguments(unit ignored)
{
  assert(commandline_argv != NULL);
  return array_to_string_list(commandline_argv);
}

/* debugging */
/*
ptr_t printSize(int size)
{
  printf("YYYYYYYYY size = %d\n", size);
}


ptr_t printAddr(ptr_t addr1, ptr_t addr2)
{
  static int count = 0;
  int i, size = 20000, found = 0;
  printf("XXX printAddr #%d: %u   %u -> (%u, %u) ->  %lf   %lf\n", count++, addr1, addr2, addr2[0], addr2[1], 
	 *((double *)addr2[0]), *((double *)addr2[1]));
  for (i=0; i<size && found < 4; i++)
    if ((ptr_t)addr1[i] == addr2) {
      printf("     %d:  point found at index %d   arrayOffset = %d\n", found, i, primaryArrayOffset);
      found++;
    }
  return empty_record;
}



ptr_t printAllAddr(ptr_t addr1)
{
  int i, size = 20000;
  printf("     arrayOffset = %d\n", primaryArrayOffset);
  for (i=0; i<size; i++) {
    ploc_t elem = (ploc_t) addr1[i];
    if ((i % 2) == (primaryGlobalOffset / sizeof(val_t)))
      printf("     %u[%5d] = %u -> (%u, %u) -> %lf   %lf\n",
	     addr1, i, elem, 
	     elem[0], elem[1], 
	     *((double *)elem[0]), *((double *)elem[1]));
    else
      printf("     %u[%5d] = %u -> (%u, %u) ->  ***\n",
	     addr1, i, elem, 
	     elem[0], elem[1]);
  }
  return empty_record;
}


ptr_t printCheck(ptr_t array, int index)
{
  ptr_t p1 = array[2*(index-1) + primaryArrayOffset/sizeof(val_t)];
  ptr_t p2 = array[2*index     + primaryArrayOffset/sizeof(val_t)];
  ptr_t x1 = p1[0];
  ptr_t x2 = p2[0];
  printf("printCheck: %10d    i = %5d    x1 = %d -> %lf     x2 = %d -> %lf\n",
	 array, index, x1, *((double *)x1), x2, *((double *)x2));
  return empty_record;
}

*/
