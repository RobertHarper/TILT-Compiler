#include <string.h>
#include "general.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/resource.h>
#include <math.h>
#include <sys/wait.h>
#include <stdio.h>
#if (defined alpha_osf)
#include <float.h>
#elif (defined solaris)
#include <ieeefp.h>
#endif
#include <dirent.h>
#include <stropts.h>
#include <poll.h>

#include "tag.h"
#include "thread.h"
#include "create.h"
#include "posix.h"


#define PAIR(t,t1,t2,tlong,tstruct) struct tstruct { t1 first; t2 second; }; \
                                    typedef struct tstruct *tlong; \
                                    typedef value_t t;
                           
#define TRIPLE(t,t1,t2,t3,tlong,tstruct) struct tstruct { t1 first; t2 second; t3 third; }; \
                                         typedef struct tstruct *tlong; \
                                         typedef value_t t;

#define LIST(t,tlist,tlong,tstruct) struct tstruct \
                        { t car; \
			  value_t cdr; \
			}; \
                        typedef struct tstruct *tlong; \
                        typedef value_t tlist;

typedef int unit;
typedef int bool;
typedef int posint;
typedef int word;
typedef char word8;
typedef value_t word8array;
typedef const value_t word8vector;
typedef value_t string;
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



string cstring2mlstring_alloc(char *str)
{
  return alloc_string(strlen(str),str);
}


char* mlstring2cstring_buffer(string mlstring, int len, char *buf)
{
  unsigned int tag = ((int *)mlstring)[-1];
  int bytelen = GET_ARRLEN(tag);
  char *raw = (char *)mlstring;
  assert((bytelen+1) <= len);
  bcopy(raw,buf,bytelen);
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
  unsigned int tag = ((int *)mlstring)[-1];
  int bytelen = GET_ARRLEN(tag);
  char *buf = malloc(bytelen+1);
  return mlstring2cstring_buffer(mlstring, bytelen+1, buf);
}
 
double ln(double arg)
{
  return log(arg);
}


int getRoundingMode(int unit)
{
#ifdef alpha_osf
  unsigned int mode = read_rnd();
  switch (mode) {
    case FP_RND_RZ: return 1;
    case FP_RND_RN: return 0;
    case FP_RND_RP: return 2;
    case FP_RND_RM: return 3;
  }
  printf("read_rnd retrurned unrecognized mode %d\n", mode);
  assert(0);
#elif (defined solaris)
  fp_rnd mode = fpgetround();
  switch (mode) {
    case FP_RZ: return 1;
    case FP_RN: return 0;
    case FP_RP: return 2;
    case FP_RM: return 3;
  }
  printf("read_rnd retrurned unrecognized mode %d\n", mode);
  assert(0);
#endif
}


value_t setRoundingMode(int ml_mode)
{
#if (defined alpha_osf)
  int mode = 0;
  switch (ml_mode) {
    case 1: ml_mode = FP_RND_RZ; break;
    case 0: ml_mode = FP_RND_RN; break;
    case 2: ml_mode = FP_RND_RP; break;
    case 3: ml_mode = FP_RND_RM; break;
    default : printf("setrounding given unknown mML rounding mode %d\n", mode); assert(0);
    }
  write_rnd(mode);
#elif (defined solaris)
  fp_rnd mode = 0;
  switch (ml_mode) {
    case 1: ml_mode = FP_RZ; break;
    case 0: ml_mode = FP_RN; break;
    case 2: ml_mode = FP_RP; break;
    case 3: ml_mode = FP_RM; break;
    default : printf("setrounding given unknown mML rounding mode %d\n", mode); assert(0);
    }
  fpsetround(mode);
#endif  
  return empty_record;
}

char* exnNameRuntime(void *unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

char* exnMessageRuntime(void *unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
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
  assert(0);
#endif
  biasedExp -= 1023;
  return biasedExp;
}


intpair ml_timeofday()
{
  value_t result;
  struct timeval tp;
  struct timezone tzp;
  int fail = gettimeofday (&tp, &tzp);
  if (fail)
    {
      printf("POSIX function gettimeofday failed with errno = %d\n", errno);
      assert(0);
    }
  return alloc_intint(tp.tv_sec,tp.tv_usec);
}

mltm ctm2mltm_alloc(struct tm *tm)
{
  value_t result = alloc_manyint(9,0);
  ((value_t *) result)[0] = tm->tm_sec;
  ((value_t *) result)[1] = tm->tm_min;
  ((value_t *) result)[2] = tm->tm_hour;
  ((value_t *) result)[3] = tm->tm_mday;
  ((value_t *) result)[4] = tm->tm_mon;
  ((value_t *) result)[5] = tm->tm_year;
  ((value_t *) result)[6] = tm->tm_wday;
  ((value_t *) result)[7] = tm->tm_yday;
  ((value_t *) result)[8] = tm->tm_isdst;
  return ((mltm) result);
}

string posix_ascTime (mltm mltm)
{
  char *result = asctime((struct tm *) mltm);
  value_t res = alloc_string(strlen(result),result);
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
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_strfTime(string_mltm unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_error_msg(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_error_name(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_error_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<((sizeof tbl)/(sizeof (sys_const_t))); i++)
    {
      char *name = tbl[i].name;
      if (!(strcmp(name,carg)))
	return tbl[i].id;
    }
  printf("posix_error_num could not find entry '%s'\n",carg);
  assert(0);
}

string posix_os_tmpname(unit unused)
{
  char *buf = NULL;
  value_t res = alloc_uninit_string(L_tmpnam,&buf);
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
  value_t result = 0;        /* nil */

  if (sec_usec_option != 0)  /* is it NONE? */
    {
      int *sec_usec = (int *)sec_usec_option;
      int sec = sec_usec[0];
      int usec = sec_usec[1];
      timeout = 1000 * sec + usec / 1000;
    }
  while (fd_event_list != 0) /* is it nil? */
    {
      int *fd_event = (int *)(((value_t *)fd_event_list)[0]);
      int fd = fd_event[0];
      int event = fd_event[1];
      fd_event_list = ((value_t *)fd_event_list)[0];
      fds[count].fd = fd;
      fds[count].events = event;
      count++;
      assert(count < (sizeof(fds))/(sizeof (struct pollfd)));
    }
  printf("count = %d, timeout = %d \n",count, timeout);
  poll(fds,count,timeout);
  for (i=count-1; i>=0; i--) {
    int car = alloc_intint(fds[i].fd, fds[i].revents);
    result = alloc_recrec(car,result);
  }
  return result;
}

int posix_io_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<((sizeof io_values)/(sizeof (name_val_t))); i++)
    {
      char *name = io_values[i].name;
      if (!(strcmp(name,carg)))
	return io_values[i].id;
    }
  printf("posix_io_num could not find entry '%s'\n",carg);
  assert(0);
}

intpair posix_io_pipe(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_io_dup(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_io_dup2(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_io_close(int fd)
{
  int fail = close(fd);
  if (fail == -1)
    {
      printf("POSIX function close returned with errno = %d\n", errno);
      assert(0);
    }
  return empty_record;
}


/* ML strings are not null-terminated */
word8vector posix_io_read(int fd, int size)
{
  value_t *alloc, *limit;
  char *buf = NULL;
  int bytes_read;
  value_t res;

  assert(size >= 0);
  res = alloc_uninit_string(size,&buf);
  bytes_read = read(fd,buf,size);
  if (bytes_read == -1)
    { 
      printf("POSIX function read failed with errno = %d\n", errno);
      assert(0);
    }
  else 
    assert(bytes_read <= size);
  adjust_stringlen(res,bytes_read);
  return (word8vector) res;
}


int posix_io_readbuf(int fd, word8array buf, int len, int start)
{ 
  int bytes_read = read(fd, ((char *)buf) + start, len);
  if (bytes_read == -1)
    { 
      printf("POSIX function read failed with errno = %d\n", errno);
      assert(0);
    }
  return bytes_read;
}

int posix_io_writebuf(int fd, word8array buf, int len, int start)
{
  int written = write(fd, ((char *)buf) + start, len);
  if (written == -1)
  {
    printf("POSIX function write returned with errno = %d\n", errno);
    assert(0);
  }
  return written;
}

int posix_io_fcntl_d(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

word posix_io_fcntl_gf(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_io_fcntl_sfd(int unused1, word unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

wordpair posix_io_fcntl_gfl(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_io_fcntl_sfl(int unusde1, word unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

flock_rep posix_io_fcntl_l(int unused1, int unused2, flock_rep unused3)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_io_lseek(int filedes, int offset, int whence)
{
  int result_pos = lseek(filedes, offset, whence);
  if (result_pos == -1)
    {  
      printf("POSIX function lseek returned with errno = %d\n", errno);
       assert(0);
     }
  return result_pos;
}

unit posix_io_fsync(int fd)
{
  int status = fsync(fd);
  if (status == -1)
    {  
      printf("POSIX function fsync returned with errno = %d\n", errno);
      assert(0);
    }
  return empty_record;
}

int posix_procenv_getpid(unit unused)
{
  return (int)getpid();
}

int posix_procenv_getppid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

word posix_procenv_getuid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

word posix_procenv_geteuid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

word posix_procenv_getgid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

word posix_procenv_getegid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_procenv_setuid(word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_procenv_setgid(word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

word_list posix_procenv_getgroups(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_procenv_getlogin(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_procenv_getpgrp(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_procenv_setsid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_procenv_setpgid(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string_stringlist posix_procenv_uname(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_tty_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<((sizeof tty_values)/(sizeof (name_val_t))); i++)
    {
      char *name = tty_values[i].name;
      if (!(strcmp(name,carg)))
	return tty_values[i].id;
    }
  printf("posix_tty_num could not find entry '%s'\n",carg);
  assert(0);
}

termio_rep  posix_tty_tcgetattr(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_tty_tcsetattr(int unused1, int unused2, termio_rep  unused3)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_tty_tcsendbreak(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_tty_tcdrain(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_tty_tcflush(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_tty_tcflow(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_tty_tcgetpgrp(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_tty_tcsetpgrp(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string_word_stringlist posix_sysdb_getgrgid(word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string_word_stringlist posix_sysdb_getgrnam(string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string_word_word_string_string posix_sysdb_getpwuid(word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string_word_word_string_string posix_sysdb_getpwnam(string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_procenv_time(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int_int_int_int_int posix_procenv_times(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

/* This code relies on NONE being represented by 0. */
string_option posix_procenv_getenv(string mlname)
{
  char *empty = "";
  char *cname = mlstring2cstring_static(mlname);  /* Don't need to free this */
  char *cvalue = getenv(cname);                   /* Don't need to free this and cannot modify it */
  value_t mlvalue = (cvalue == NULL) ? 0 : alloc_string(strlen(cvalue),cvalue);
  return mlvalue;
}

string_list posix_procenv_environ(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_procenv_ctermid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_procenv_ttyname(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

bool posix_procenv_isatty(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_process_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<((sizeof process_values)/(sizeof (name_val_t))); i++)
    {
      char *name = process_values[i].name;
      if (!(strcmp(name,carg)))
	return process_values[i].id;
    }
  printf("posix_process_num could not find entry '%s'\n",carg);
  assert(0);
}

word posix_process_sysconf(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<((sizeof sysconf_keys)/(sizeof (name_val_t))); i++)
    {
      char *name = sysconf_keys[i].name;
      if (!(strcmp(name,carg)))
	return sysconf(sysconf_keys[i].id);
    }
  printf("posix_process_sysconf could not find entry '%s'\n",carg);
  assert(0);
}

int posix_process_fork(unit unused)
{
  int code = fork();
  if (code != -1)
    {
      printf("Call to fork returned with %d.\n",code);
      return code; 
    }
  else
    {
      printf("POSIX function fork got -1; errno = %d\n", errno);
      assert(0);
    }
}

unit posix_process_exec(string path, string_list args)
{
  int code, i, length = 0;
  string_list cur = args;
  char **argv;
  while (cur)
    {
      length++;
      cur = ((string_list_long) cur)->cdr;
    }
  argv = malloc((length + 1) * sizeof(char *));
  if (argv == NULL)
    {
      printf("malloc in posix_process_exec failed with length = %d\n", length);
      assert(0);
    }
  for (cur=args, i=0; i<length; i++)
    {
      argv[i] = mlstring2cstring_malloc(((string_list_long) cur)->car);
      cur = ((string_list_long) cur)->cdr;
    }
  argv[length] = NULL;
  code = execv(mlstring2cstring_malloc(path),argv);
  printf("POSIX function exec returned with errno = %d\n", errno);
  assert(0);
}

unit posix_process_exece(string unused1, string_list unused2, string_list unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_process_execp(string unused1, string_list unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

inttriple posix_process_waitpid(int argpid, word options)
{
  int status;
  value_t result;
  int pid = waitpid(argpid, &status, options);
  int how, val;
  if (pid < 0)
       {
	 printf("POSIX waitpid got back negative result errno = %d\n", errno);
	 assert(0);
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
  else
     {
       printf("POSIX waitpid got back unknown child status = %d\n", status);
       assert(0);
     }

  result = alloc_manyint(3,0);
  ((value_t *) result)[0] = pid;
  ((value_t *) result)[1] = how;
  ((value_t *) result)[2] = val;
  return (inttriple) result;
}

unit posix_process_exit(word8 unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_process_kill(int unused1, int unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_process_alarm(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_process_pause(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_process_sleep(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_signal_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<((sizeof signal_values)/(sizeof (name_val_t))); i++)
    {
      char *name = signal_values[i].name;
      if (!(strcmp(name,carg)))
	return signal_values[i].id;
    }
  printf("posix_signal_num could not find entry '%s'\n",carg);
  assert(0);
}

word posix_filesys_num(string arg)
{
  int i;
  char* carg = mlstring2cstring_static(arg);
  for (i=0; i<((sizeof filesys_values)/(sizeof (name_val_t))); i++)
    {
      char *name = filesys_values[i].name;
      if (!(strcmp(name,carg)))
	return filesys_values[i].id;
    }
  printf("posix_filesys_num could not find entry '%s'\n",carg);
  assert(0);
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

unit posix_filesys_chdir(string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_filesys_getcwd(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_filesys_openf(string filename, word oflag, word mode)
{
  int fd = open(mlstring2cstring_static(filename),oflag,mode);
  if (fd == -1)
    {
      printf("POSIX function open returned with errno = %d\n", errno);
      assert(0);
    }
  return fd;
}

int posix_filesys_umask(word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_link(string unusd1, string unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
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
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_mkdir(string unused1, word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_mkfifo(string unused1, word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_unlink(string arg)
{
  char* path = mlstring2cstring_static(arg);
  int result = unlink(path);
  if (result) {
    printf("posix_filesys_unlink failed with errno = %d\n", errno);
    assert(0);
  }
  return empty_record;
}

unit posix_filesys_rmdir(string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_filesys_readlink(string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_ftruncate(int unused1, int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

#define MODE_BITS (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID)

statrep cstat2mlstat_alloc(struct stat *buffer)
{
  value_t record = alloc_manyint(11,0);
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
  int error = stat(mlstring2cstring_static(name),&buffer);
  if (error)
    {
      printf("POSIX function stat returned error: errno = %d should raise exn\n",
	     errno);
      assert(0);
    } 
  return cstat2mlstat_alloc(&buffer);
}

statrep posix_filesys_lstat(string name)
{
  struct stat buffer;
  int error = lstat(mlstring2cstring_static(name),&buffer);
  if (error)
    {
      printf("POSIX function lstat returned error: errno = %d should raise exn\n",
	     errno);
      assert(0);
    } 
  return cstat2mlstat_alloc(&buffer);
}

statrep posix_filesys_fstat(int filedesc)
{
  struct stat buffer;
  int error = fstat(filedesc,&buffer);
  if (error)
    {
      printf("POSIX function fstat called with %d returned error: errno = %d should raise exn\n",
	     filedesc, errno);
      assert(0);
    } 
  return cstat2mlstat_alloc(&buffer);
}

/* XXX what about errno's other than NOENT - shouldn't an exception be raised by posix-filesys.sml? */
bool posix_filesys_access(string name, word mode)
{
  char *cname = mlstring2cstring_static(name);
  int code = access(cname,mode);
  if (code == 0)
    return 1; /* true */
  else
    return 0; /* false */
}

unit posix_filesys_chmod(string unused1, word unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_fchmod(int unused1, word unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_chown(string unused1, word unused2, word unused3)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_fchown(int unused1, word unused2, word unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_utime(string unused1, int unused2, int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}






intpair posix_filesys_pathconf(string unusde1, string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

intpair posix_filesys_fpathconf (int unused1, string unused)
{ 
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}




void posix_io_fcntl_gfd()
{ 
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

value_t til_selfusage()
{
  value_t fields[4];
  int masks[4];
  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage);
  fields[0] = rusage.ru_utime.tv_sec;
  fields[1] = rusage.ru_utime.tv_usec;
  fields[2] = rusage.ru_stime.tv_sec;
  fields[3] = rusage.ru_stime.tv_usec;
  masks[0] = masks[1] = masks[2] = masks[3] = 0;
  return alloc_record(fields, masks, 4);
}

value_t til_realtime()
{
  struct timeb tp;
  ftime(&tp);
  return alloc_intint(tp.time, tp.millitm);
}
