#include <limits.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include "general.h"
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/times.h>
#include <utime.h>
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
#include "stack.h"
#include "thread.h"
#include "create.h"
#include "posix.h"
#include "global.h"
#include "exn.h"
#include "gc.h"

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
  string cc;
  word inspeed;
  word outspeed;
};
typedef struct termio_rep_struct *termio_rep;

int stringlen(ptr_t string)
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
  runtime_error_fmt("function not implemented at %s:%d\n", __FILE__, lineno);
}
#define UNIMP() unimplemented(__LINE__); return 0

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
  if ((bytelen+1) > len)
     runtime_error_msg("fixed-length buffer too small");
  memcpy(buf,raw,bytelen);
  buf[bytelen] = 0;
  return (char *)buf;
}

/* Uses the per-processor buffer */
static char* mlstring2cstring_static(string mlstring)
{
  Proc_t *proc = getProc();  /* XXXX too slow? */
  return mlstring2cstring_buffer(mlstring, sizeof(proc->buffer), proc->buffer);
}

static char* mlstring2cstring_malloc(string mlstring)
{
  int bytelen = stringlen(mlstring);
  char *buf = emalloc(bytelen+1);
  return mlstring2cstring_buffer(mlstring, bytelen+1, buf);
}

string posix_error_msg(int);
static void runtime_error(int e)
{
  string msg = posix_error_msg(e);
  ptr_t exn = mkSysErrExn(msg, 1, e);
  raise_exn(exn);
}

static void runtime_error_msg(char* msg)
{
  string mlmsg = cstring2mlstring_alloc(msg);
  ptr_t exn = mkSysErrExn(mlmsg, 0, 0);
  raise_exn(exn);
}

static void runtime_error_fmt(const char* fmt, ...)
{
  /* This must not return */
  /*
  va_list ap;
  va_start(ap, fmt);
  (void) vfprintf(stderr, fmt, ap);
  va_end(ap);
  */
  char buf[1024];
  va_list args;
  va_start(args, fmt);
  vsprintf(buf, fmt, args);
  va_end(args);
  runtime_error_msg(buf);
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
    default :
      runtime_error_fmt("setRoundingMode given unknown ML rounding mode %d", ml_mode);
    }
  write_rnd(mode);
#elif (defined solaris)
  fp_rnd mode = 0;
  switch (ml_mode) {
    case 1: mode = FP_RZ; break;
    case 0: mode = FP_RN; break;
    case 2: mode = FP_RP; break;
    case 3: mode = FP_RM; break;
    default :
      runtime_error_fmt("setRoundingMode given unknown ML rounding mode %d", ml_mode);
    }
  fpsetround(mode);
#endif  
  return empty_record;
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


intpair ml_timeofday(void)
{
  ptr_t result;
  struct timeval tp;
  struct timezone tzp;
  if (gettimeofday(&tp, &tzp) == -1) {
    runtime_error(errno);
  }
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

struct tm* mltm2ctm(mltm mltm, struct tm* tm)
{
  ptr_t source = (ptr_t) mltm;
  tm->tm_sec   = source[0];
  tm->tm_min   = source[1];
  tm->tm_hour  = source[2];
  tm->tm_mday  = source[3];
  tm->tm_mon   = source[4];
  tm->tm_year  = source[5];
  tm->tm_wday  = source[6];
  tm->tm_yday  = source[7];
  tm->tm_isdst = source[8];
  return tm;
}

string posix_ascTime (mltm mltm)
{
  char *result = asctime((struct tm *) mltm);
  ptr_t res = alloc_string(strlen(result),result);
  return (word8vector) res;
}

mltm posix_localTime (int time)
{
  time_t t = (time_t) time;
  struct tm *tm;
  tm = localtime (&t);
  return ctm2mltm_alloc(tm);
}

mltm posix_gmTime (int time)
{
  time_t t = (time_t) time;
  struct tm *tm;
  tm = gmtime (&t);
  return ctm2mltm_alloc(tm);
}

int posix_mkTime(mltm mltm)
{
  struct tm tm;
  time_t time = mktime (mltm2ctm(mltm, &tm));
  if (time == (time_t)-1) {
    runtime_error_msg("invalid date");
  }
  return (int) time;
}

string posix_strfTime(string_mltm arg)
{
  char buf[1024];		/* Have to use a fixed buffer size since strftime fails to */
				/* indicate when the buffer is too small. */
  struct tm tm;
  strftime (buf, sizeof buf, arg->a, mltm2ctm(arg->b, &tm));
  return cstring2mlstring_alloc(buf);
}

string posix_error_msg(int e)
{
  char* cmsg = strerror(e);
  return cstring2mlstring_alloc(cmsg);
}

string posix_error_name(int e)
{
  int i;
  for (i=0; i < arraysize(tbl); i++) {
    int e2 = tbl[i].id;
    if (e == tbl[i].id) {
      return cstring2mlstring_alloc(tbl[i].name);
    }
  }
  runtime_error_fmt("unknown error %d", e);
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

static ptr_t cons_rec_alloc(ptr_t car, ptr_t list)
{
  return alloc_recrec(car, list);
}

static ptr_t cons_val_alloc(val_t val, ptr_t list)
{
  val_t fields[2];
  int masks[1];
  masks[0] = 2;
  fields[0] = val;
  fields[1] = (val_t) list;
  return alloc_record(fields, masks, 2);
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
      if (count == arraysize(fds)) {
	runtime_error_fmt("posix_os_poll has static limit of %d file descriptors", arraysize(fds));
      }
    }
  if (poll(fds,count,timeout) == -1) {
    runtime_error(errno);
  }
  for (i=count-1; i>=0; i--) {
    ptr_t car = alloc_intint(fds[i].fd, fds[i].revents);
    result = cons_rec_alloc(car,result);
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
  int fds[2];
  if (pipe(fds) == -1) {
    runtime_error(errno);
  }
  return alloc_intint(fds[0], fds[1]);
}

int posix_io_dup(int fd)
{
  int newfd = dup(fd);
  if (newfd == -1) {
    runtime_error(errno);
  }
  return newfd;
}

unit posix_io_dup2(int oldfd, int newfd)
{
  if (dup2(oldfd, newfd) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_io_close(int fd)
{
  if (close(fd) == -1) {
    runtime_error(errno);
  }
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
  if (bytes_read == -1) {
    runtime_error(errno);
  }
  assert(bytes_read <= size);
  adjust_stringlen(res,bytes_read);
  return (word8vector) res;
}


int posix_io_readbuf(int fd, word8array buf, int len, int start)
{ 
  int bytes_read = read(fd, ((char *)buf) + start, len);
  if (bytes_read == -1) {
    runtime_error(errno);
  }
  return bytes_read;
}

int posix_io_writebuf(int fd, word8array buf, int len, int start)
{
  int written = write(fd, ((char *)buf) + start, len);
  if (written == -1) {
    runtime_error(errno);
  }
  return written;
}

int posix_io_fcntl_d(int fd, int basefd)
{
  int newfd = fcntl(fd, F_DUPFD, basefd);
  if (newfd == -1) {
    runtime_error(errno);
  }
  return newfd;
}

word posix_io_fcntl_gfd(int fd)
{
  int r = fcntl(fd, F_GETFD);
  if (r == -1) {
    runtime_error(errno);
  }
  return r;			/* mask with FD_CLOEXEC? */
}

unit posix_io_fcntl_sfd(int fd, word flag)
{				/* mask with FD_CLOEXEC? */
  if (fcntl(fd, F_SETFD, flag) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

wordpair posix_io_fcntl_gfl(int fd)
{
  int r = fcntl(fd, F_GETFL);
  word flags, mode;
  if (r == -1) {
    runtime_error(errno);
  }
  flags = r & (~O_ACCMODE);
  mode = r & O_ACCMODE;
  return alloc_intint(flags, mode);
}

unit posix_io_fcntl_sfl(int fd, word flags)
{
  if (fcntl(fd, F_SETFL, flags) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

static flock_rep cflock2mlflock(struct flock* flock)
{
  ptr_t result = alloc_manyint(5,0);
  result[0] = flock->l_type;
  result[1] = flock->l_whence;
  result[2] = flock->l_start;
  result[3] = flock->l_len;
  result[4] = flock->l_pid;
  return (flock_rep) result;
}

static struct flock* mlflock2cflock(struct flock* flock, flock_rep rep)
{
  flock->l_type = rep->a;
  flock->l_whence = rep->b;
  flock->l_start = rep->c;
  flock->l_len = rep->d;
  flock->l_pid = rep->e;
  return flock;
}
  
flock_rep posix_io_fcntl_l(int fd, int cmd, flock_rep rep)
{
  struct flock flock;
  if (fcntl(fd, cmd, mlflock2cflock(&flock, rep)) == -1) {
    runtime_error(errno);
  }
  return cflock2mlflock(&flock);
}

int posix_io_lseek(int filedes, int offset, int whence)
{
  int result_pos = lseek(filedes, offset, whence);
  if (result_pos == -1) {
    runtime_error(errno);
  }
  return result_pos;
}

unit posix_io_fsync(int fd)
{
  int status = fsync(fd);
  if (status == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

int posix_procenv_getpid(unit unused)
{
  return (int)getpid();
}

int posix_procenv_getppid(unit unused)
{
  return (int)getppid();
}

word posix_procenv_getuid(unit unused)
{
  return (word)getuid();
}

word posix_procenv_geteuid(unit unused)
{
  return (word)geteuid();
}

word posix_procenv_getgid(unit unused)
{
  return (word)getgid();
}

word posix_procenv_getegid(unit unused)
{
  return (word)getegid();
}

unit posix_procenv_setuid(word uid)
{
  if (setuid((uid_t) uid) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_procenv_setgid(word gid)
{
  if (setgid((gid_t) gid) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

word_list posix_procenv_getgroups(unit unused)
{
  int i;
  ptr_t result = 0;
  gid_t grouplist[NGROUPS_MAX];
  int ngroups = getgroups(NGROUPS_MAX, grouplist);
  if (ngroups == -1) {
    runtime_error(errno);
  }
  for (i=ngroups-1; i>=0; i--) {
    val_t car = (val_t)(word)grouplist[i];
    result = cons_val_alloc(car, result);
  }
  return result;
}

string posix_procenv_getlogin(unit unused)
{
  char* cname = getlogin();
  if (cname == NULL) {
    runtime_error_msg("no login name");
  }
  return cstring2mlstring_alloc(cname);
}

int posix_procenv_getpgrp(unit unused)
{
  return (int)getpgrp();
}

int posix_procenv_setsid(unit unused)
{
  pid_t r = setsid();
  if (r == (pid_t)-1) {
    runtime_error(errno);
  }
  return (int)r;
}

unit posix_procenv_setpgid(int pid, int pgid)
{
  if (setpgid((pid_t) pid, (pid_t) pgid) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

static ptr_t stringpair_ctoml_alloc(char* a, char* b)
{
  string ml_a = cstring2mlstring_alloc(a);
  string ml_b = cstring2mlstring_alloc(b);
  return alloc_recrec(ml_a, ml_b);
}

string_stringlist posix_procenv_uname(unit unused)
{
  ptr_t acc = NULL;
  struct utsname name;
  if (uname(&name) == -1) {
    runtime_error(errno);
  }

  #define ACC(n,v) acc = cons_rec_alloc(stringpair_ctoml_alloc(n, v), acc);

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

static termio_rep ctermios2mltermios(struct termios* termios)
{
  int masks[1];
  val_t fields[7];
  masks[0] = 16;
  fields[0] = (val_t) termios->c_iflag;
  fields[1] = (val_t) termios->c_oflag;
  fields[2] = (val_t) termios->c_cflag;
  fields[3] = (val_t) termios->c_lflag;
  fields[4] = (val_t) alloc_string(NCCS, (char*) termios->c_cc);
  fields[5] = (val_t) cfgetispeed(termios);
  fields[6] = (val_t) cfgetospeed(termios);
  return (termio_rep) alloc_record(fields, masks, arraysize(fields));
}

static struct termios* mltermios2ctermios(struct termios* termios, termio_rep rep)
{
  termios->c_iflag = rep->iflags;
  termios->c_oflag = rep->oflags;
  termios->c_cflag = rep->cflags;
  termios->c_lflag = rep->lflags;
  assert(stringlen(rep->cc) == NCCS);
  memcpy(termios->c_cc, rep->cc, NCCS);
  return termios;
}

termio_rep  posix_tty_tcgetattr(int fd)
{
  struct termios termios;
  if (tcgetattr(fd, &termios) == -1) {
    runtime_error(errno);
  }
  return ctermios2mltermios(&termios);
}

unit posix_tty_tcsetattr(int fd, int action, termio_rep rep)
{
  struct termios termios;
  if (tcsetattr(fd, action, mltermios2ctermios(&termios, rep)) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_tty_tcsendbreak(int fd, int duration)
{
  if (tcsendbreak(fd, duration) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_tty_tcdrain(int fd)
{
  if (tcdrain(fd) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_tty_tcflush(int fd, int queue_selector)
{
  if (tcflush(fd, queue_selector) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_tty_tcflow(int fd, int action)
{
  if (tcflow(fd, action) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

int posix_tty_tcgetpgrp(int fd)
{
  pid_t r = tcgetpgrp(fd);
  if (r == (pid_t) -1) {
    runtime_error(errno);
  }
  return (int)r;
}

unit posix_tty_tcsetpgrp(int fd, int pgid)
{
  if (tcsetpgrp(fd, (pid_t) pgid) == -1) {
    runtime_error(errno);
  }
  return empty_record;
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

static char** string_list_to_array_malloc(string_list_long list)
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

static void free_string_array(char** arr)
{
  int i;
  for (i=0; arr[i]; i++) {
    free(arr[i]);
  }
  free(arr);
}

static string_list array_to_string_list(char** arr)
{
  /* Convert a NULL-terminated array of strings into a value of type string list.
   */
  char* string = *arr;
  char** next = arr+1;
  if (string == NULL) return (ptr_t) 0;
  else {
    ptr_t car = cstring2mlstring_alloc(string);
    ptr_t cdr = array_to_string_list(next);
    return cons_rec_alloc(car,cdr);
  }
}

static string_word_stringlist cgroup2ml(struct group* group)
{
  val_t fields[3];
  int masks[1];
  int i;
  masks[0] = 5;
  fields[0] = (val_t) cstring2mlstring_alloc(group->gr_name);
  fields[1] = (val_t) group->gr_gid;
  fields[2] = (val_t) array_to_string_list(group->gr_mem);
  return (string_word_stringlist) alloc_record(fields, masks, arraysize(fields));
}

string_word_stringlist posix_sysdb_getgrgid(word gid)
{
  struct group* r = getgrgid((gid_t) gid);
  if (r == NULL) {
    runtime_error(errno);
  }
  return cgroup2ml(r);
}

string_word_stringlist posix_sysdb_getgrnam(string mlname)
{
  char* cname = mlstring2cstring_static(mlname);
  struct group* r = getgrnam(cname);
  if (r == NULL) {
    runtime_error(errno);
  }
  return cgroup2ml(r);
}

static string_word_word_string_string cpasswd2ml(struct passwd* passwd)
{
  val_t fields[5];
  int masks[1];
  masks[0] = 25;
  fields[0] = (val_t) cstring2mlstring_alloc(passwd->pw_name);
  fields[1] = (val_t) passwd->pw_uid;
  fields[2] = (val_t) passwd->pw_gid;
  fields[3] = (val_t) cstring2mlstring_alloc(passwd->pw_dir);
  fields[4] = (val_t) cstring2mlstring_alloc(passwd->pw_shell);
  return (string_word_word_string_string) alloc_record(fields, masks, arraysize(fields));
}

string_word_word_string_string posix_sysdb_getpwuid(word uid)
{
  struct passwd* r = getpwuid((uid_t) uid);
  if (r == NULL) {
    runtime_error(errno);
  }
  return cpasswd2ml(r);
}

string_word_word_string_string posix_sysdb_getpwnam(string mlname)
{
  char* cname = mlstring2cstring_static(mlname);
  struct passwd* r = getpwnam(cname);
  if (r == NULL) {
    runtime_error(errno);
  }
  return cpasswd2ml(r);
}

int posix_procenv_time(unit unused)
{
  return (int) time(NULL);
}

int_int_int_int_int posix_procenv_times(unit unused)
{
  val_t fields[5];
  int masks[1];
  struct tms buf;
  clock_t t = times(&buf);
  if (t == (clock_t)-1) {
    runtime_error(errno);
  }
  masks[0] = 0;
  fields[0] = (val_t) t;
  fields[1] = (val_t) buf.tms_utime;
  fields[2] = (val_t) buf.tms_stime;
  fields[3] = (val_t) buf.tms_cutime;
  fields[4] = (val_t) buf.tms_cstime;
  return (int_int_int_int_int) alloc_record(fields, masks, arraysize(fields));
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
  extern char** environ;
  return array_to_string_list(environ);
}

string posix_procenv_ctermid(unit unused)
{
  char name[L_ctermid];
  if (ctermid(name) == NULL) {
    runtime_error(errno);
  }
  return cstring2mlstring_alloc(name);
}

string posix_procenv_ttyname(int fd)
{
  char* r = ttyname(fd);
  if (r == NULL) {
    runtime_error_msg("not a terminal device");
  }
  return cstring2mlstring_alloc(r);
}

bool posix_procenv_isatty(int fd)
{
  return isatty(fd) ? 1 : 0;
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
  Proc_t *self = getProc();
  int code = fork();
  if (code == 0) {   /* Child needs to reset pthread values */
    self->pthread = pthread_self();
  }
  if (code == -1) {  /* fork failed for some reason */
    runtime_error(errno);
  }
  return code;
}

unit posix_process_exec(string mlpath, string_list args)
{
  char* cpath = mlstring2cstring_static(mlpath);
  char** argv = string_list_to_array_malloc((string_list_long)args);
  int ignored = execv (cpath, argv);
  int errorsave = errno;
  free_string_array(argv);
  runtime_error(errorsave);
  return empty_record;
}

unit posix_process_exece(string mlpath, string_list args, string_list envs)
{
  char* cpath = mlstring2cstring_static(mlpath);
  char** argv = string_list_to_array_malloc((string_list_long)args);
  char** envp = string_list_to_array_malloc((string_list_long)envs);
  int ignored = execve (cpath, argv, envp);
  int errorsave = errno;
  free_string_array(argv);
  free_string_array(envp);
  runtime_error(errorsave);
  return empty_record;
}

unit posix_process_execp(string mlpath, string_list args)
{
  char* cpath = mlstring2cstring_static(mlpath);
  char** argv = string_list_to_array_malloc((string_list_long)args);
  int ignored = execvp (cpath, argv);
  int errorsave = errno;
  free_string_array(argv);
  runtime_error(errorsave);
  return empty_record;
}

inttriple posix_process_waitpid(int argpid, word options)
{
  int status;
  ptr_t result;
  int pid = waitpid(argpid, &status, options);
  int how, val;
  if (pid < 0) {
    runtime_error(errno);
  }

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
  else {
    runtime_error_fmt("POSIX waitpid got back unknown child status = %d", status);
  }

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

unit posix_process_kill(int pid, int sig)
{
  if (kill ((pid_t) pid, sig) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

int posix_process_alarm(int sec)
{
  return (int) alarm((unsigned)sec);
}

unit posix_process_pause(unit unused)
{
  int ignored = pause();
  return empty_record;
}

int posix_process_sleep(int sec)
{
  return (int)sleep((unsigned) sec);
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
  if (dir == NULL)
    runtime_error(errno);
  return (int)dir;
}

static bool filter(const char* name) {
	return (name[0] == '.' &&
		(name[1] == '\0' ||
		 (name[1] == '.' && name[2] == '\0')));
}

string posix_filesys_readdir(int arg)
{
  DIR *dir = (DIR *)arg;
  struct dirent *entry;
  do {
    errno = 0;
    entry = readdir(dir);
  } while (entry != NULL && filter(entry->d_name));
  if (entry != NULL)
    return cstring2mlstring_alloc(entry->d_name);
  else {
    if (errno != 0)
      runtime_error(errno);
    return cstring2mlstring_alloc("");
  }
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
  if (chdir (cdirname) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

string posix_filesys_getcwd(unit unused)
{
  char buffer[1024];
  if (getcwd(buffer, sizeof(buffer) - 1) == NULL) {
    if (errno == ERANGE) {
      /* XXX */
    }
    runtime_error(errno);
  }
  return cstring2mlstring_alloc(buffer);
}

int posix_filesys_openf(string filename, word oflag, word mode)
{
  const char *cfilename = mlstring2cstring_static(filename);
  int fd = open(cfilename,oflag,mode);
  if (fd == -1) {
    runtime_error(errno);
  }
  return fd;
}

word posix_filesys_umask(word mask)
{
  return (word)umask((mode_t) mask);
}

unit posix_filesys_link(string from, string to)
{
  char buf[1024];
  char *cfrom = mlstring2cstring_static(from);
  char *cto = mlstring2cstring_buffer(to, sizeof(buf), buf);  /* can't use ..._static twice */
  if (link (cfrom, cto) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_rename(string from, string to)
{
  char buf[1024];
  char *cfrom = mlstring2cstring_static(from);
  char *cto = mlstring2cstring_buffer(to, sizeof(buf), buf);  /* can't use ..._static twice */
  if (rename (cfrom,cto) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_symlink(string from, string to)
{
  char buf[1024];
  char *cfrom = mlstring2cstring_static(from);
  char *cto = mlstring2cstring_buffer(to, sizeof(buf), buf);  /* can't use ..._static twice */
  if (symlink(cfrom, cto) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_mkdir(string mlDir, word mode)
{
  char *cDir = mlstring2cstring_static(mlDir);
/*status = mkdirp(cDir,mode);  If you want mkdirp, implement it for
  alpha */
  if (mkdir(cDir,mode) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_mkfifo(string path, word mode)
{
  char* cpath = mlstring2cstring_static(path);
  if (mkfifo(cpath, (mode_t) mode) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_unlink(string arg)
{
  char* path = mlstring2cstring_static(arg);
  int result = unlink(path);
  if (result == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_rmdir(string path)
{
  char* cpath = mlstring2cstring_static(path);
  if (rmdir(cpath) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

string posix_filesys_readlink(string link)
{
  char buf[1024];
  int status = readlink(mlstring2cstring_static(link), buf, sizeof(buf) - 1);
  if (status == -1) {
    runtime_error(errno);
  } else {
    buf[status] = 0;
    return cstring2mlstring_alloc(buf);
  }
}

unit posix_filesys_ftruncate(int fd, int length)
{
  if (ftruncate(fd, (off_t) length) == -1) {
    runtime_error(errno);
  }
  return empty_record;
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
  if (stat(mlstring2cstring_static(name),&buffer) == -1) {
    runtime_error(errno);
  }
  return cstat2mlstat_alloc(&buffer);
}

statrep posix_filesys_lstat(string name)
{
  struct stat buffer;
  if (lstat(mlstring2cstring_static(name),&buffer) == -1) {
    runtime_error(errno);
  }
  return cstat2mlstat_alloc(&buffer);
}

statrep posix_filesys_fstat(int filedesc)
{
  struct stat buffer;
  if (fstat(filedesc,&buffer) == -1) {
    runtime_error(errno);
  }
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

unit posix_filesys_chmod(string path, word mode)
{
  char* cpath = mlstring2cstring_static(path);
  if (chmod(cpath, (mode_t) mode) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_fchmod(int fd, word mode)
{
  if (fchmod(fd, (mode_t) mode) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_chown(string path, word owner, word group)
{
  char* cpath = mlstring2cstring_static(path);
  if (chown(cpath, (uid_t) owner, (gid_t) group) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_fchown(int fd, word owner, word group)
{
  if (fchown(fd, (uid_t)owner, (gid_t)group) == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

unit posix_filesys_utime(string filename, int atime, int modtime)
{
  char* cfilename = mlstring2cstring_static(filename);
  int r;
  if (atime == -1) {
    r = utime (cfilename, NULL);
  } else {
    struct utimbuf arg;
    arg.actime = atime;
    arg.modtime = modtime;
    r = utime (cfilename, &arg);
  }
  if (r == -1) {
    runtime_error(errno);
  }
  return empty_record;
}

intpair posix_filesys_pathconf(string unused1, string unused)
{
  UNIMP();
}

intpair posix_filesys_fpathconf (int unused1, string unused)
{ 
  UNIMP();
}

ptr_t til_selfusage(void)
{
  val_t fields[4];
  int masks[1];
  struct rusage rusage;
  if (getrusage(RUSAGE_SELF, &rusage) == -1) {
    runtime_error(errno);
  }
  masks[0] = 0;
  fields[0] = (val_t) rusage.ru_utime.tv_sec;
  fields[1] = (val_t) rusage.ru_utime.tv_usec;
  fields[2] = (val_t) rusage.ru_stime.tv_sec;
  fields[3] = (val_t) rusage.ru_stime.tv_usec;
  return alloc_record(fields, masks, 4);
}

ptr_t til_realtime(void)
{
  struct timeb tp;
  ftime(&tp);
  return alloc_intint(tp.time, tp.millitm);
}

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

string_list commandline_arguments(unit ignored)
{
  assert(commandline_argv != NULL);
  return array_to_string_list(commandline_argv);
}
