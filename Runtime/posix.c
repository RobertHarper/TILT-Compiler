#include "tag.h"
#include "posix.h"
#include <sys/stat.h>
#include <math.h>
#include <sys/wait.h>
#include <stdio.h>

long cur_alloc_pointer;
long cur_alloc_limit;



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
PAIR(intpair,int,int,intpair_long,intpair_struct);
typedef intpair intpair_option;
typedef string string_option;
TRIPLE(inttriple,int,int,int,inttriple_long,inttriple_struct);
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
struct tm_struct
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
typedef struct tm_struct *tm;

struct string_tm_struct
{
  char *a;
  tm b;
};
typedef struct string_tm_struct *string_tm;

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



static char* mlstring2cstring(string mlstring)
{
  static char buf[1024];
  unsigned int tag = ((int *)mlstring)[-1];
  int len = tag >> POSSLEN_SHIFT;
  char *raw = (char *)mlstring;
  bcopy(raw,buf,len);
  buf[len] = 0;
  return (char *)buf;
}


static char* mlstring2cstring_malloc(string mlstring)
{
  unsigned int tag = ((int *)mlstring)[-1];
  int len = tag >> POSSLEN_SHIFT;
  char *buf = malloc(len+1);
  char *raw = (char *)mlstring;
  bcopy(raw,buf,len);
  buf[len] = 0;
  return (char *)buf;
}
 
double ln(double arg)
{
  return log(arg);
}


int getRoundingMode(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}


int setRoundingMode(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
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
#ifdef alpha_osf
  unsigned long temp;
  *((double *)(&temp)) = arg;
  temp >>= 52;
  temp &= (1<<11 - 1);
  temp -= 1023;
  return temp;
#endif
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}


void ml_timeofday()
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_ascTime (tm unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

tm posix_localTime (intpair unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

tm posix_gmTime (intpair unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

intpair posix_mkTime(tm unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_strfTime(string_tm unused)
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
  char* carg = mlstring2cstring(arg);
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
  value_t res = alloc_uninit_string(L_tmpnam,&buf,&cur_alloc_pointer,cur_alloc_limit);
  char *result = tmpnam(buf);
  assert(result == buf);
  adjust_stringlen(res,strlen(buf));
  return (word8vector) res;
}

intword_list posix_os_poll(intword_list unused1, intpair_option unused2)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

int posix_io_num(string arg)
{
  int i;
  char* carg = mlstring2cstring(arg);
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
  return 256; /* unit is 256 */
}


/* ML strings are not null-terminated */
word8vector posix_io_read(int fd, int size)
{
  char *buf = NULL;
  value_t res = alloc_uninit_string(size,&buf,&cur_alloc_pointer,cur_alloc_limit);
  int bytes_read = read(fd,buf,size);
  if (bytes_read == -1)
    { 
      printf("POSIX function read failed with errno = %d\n", errno);
      assert(0);
    }
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
  return 256; /* unit */
}

int posix_procenv_getpid(unit unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
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
  char* carg = mlstring2cstring(arg);
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

string_option posix_procenv_getenv(string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
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
  char* carg = mlstring2cstring(arg);
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
  char* carg = mlstring2cstring(arg);
  for (i=0; i<((sizeof sysconf_values)/(sizeof (name_val_t))); i++)
    {
      char *name = sysconf_values[i].name;
      if (!(strcmp(name,carg)))
	return sysconf_values[i].id;
    }
  printf("posix_process_sysconf could not find entry '%s'\n",carg);
  assert(0);
}

int posix_process_fork(unit unused)
{
  int junk = printf("calling fork now\n");
  int code = fork();
  if (code != -1)
    {
      printf("return from fork with %d\n",code);
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
  printf("exec called\n");
  while (cur)
    {
      length++;
      cur = ((string_list_long) cur)->cdr;
    }
  printf("exec 1\n");
  argv = malloc((length + 1) * sizeof(char *));
  printf("exec 2\n");
  if (argv == NULL)
    {
      printf("malloc in posix_process_exec failed with length = %d\n", length);
      assert(0);
    }
  printf("exec 3\n");
  for (cur=args, i=0; i<length; i++)
    {
      argv[i] = mlstring2cstring_malloc(((string_list_long) cur)->car);
      cur = ((string_list_long) cur)->cdr;
    }
  printf("exec 4\n");
  argv[length] = NULL;
  printf("exec 5\n");
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

  result = alloc_manyint(3,0,&cur_alloc_pointer,cur_alloc_limit);
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
  char* carg = mlstring2cstring(arg);
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
  char* carg = mlstring2cstring(arg);
  for (i=0; i<((sizeof filesys_values)/(sizeof (name_val_t))); i++)
    {
      char *name = filesys_values[i].name;
      if (!(strcmp(name,carg)))
	return filesys_values[i].id;
    }
  printf("posix_filesys_num could not find entry '%s'\n",carg);
  assert(0);
}

int posix_filesys_opendir(string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

string posix_filesys_readdir(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_rewinddir(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
}

unit posix_filesys_closedir(int unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
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
  int fd = open(mlstring2cstring(filename),oflag,mode);
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

unit posix_filesys_rename(string unused1, string unused)
{
  printf("POSIX function not defined at line %d\n", __LINE__);
  assert(0);
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
  char* path = mlstring2cstring(arg);
  int result = unlink(path);
  if (result) {
    printf("posix_filesys_unlink failed with errno = %d\n", errno);
    assert(0);
  }
  return 256; /* unit */
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

statrep cstat2mlstat(struct stat *buffer)
{
  value_t record = alloc_manyint(11,0,&cur_alloc_pointer,cur_alloc_limit);
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
  int error = stat(mlstring2cstring(name),&buffer);
  if (error)
    {
      printf("POSIX function stat returned error: errno = %d should raise exn\n",
	     errno);
      assert(0);
    } 
  return cstat2mlstat(&buffer);
}

statrep posix_filesys_lstat(string name)
{
  struct stat buffer;
  int error = lstat(mlstring2cstring(name),&buffer);
  if (error)
    {
      printf("POSIX function lstat returned error: errno = %d should raise exn\n",
	     errno);
      assert(0);
    } 
  return cstat2mlstat(&buffer);
}

statrep posix_filesys_fstat(int filedesc)
{
  struct stat buffer;
  int error = fstat(filedesc,&buffer);
printf("fstat called with %d\n",filedesc);
  if (error)
    {
      printf("POSIX function fstat returned error: errno = %d should raise exn\n",
	     errno);
      assert(0);
    } 
  return cstat2mlstat(&buffer);
}

/* XXX what about errno's other than NOENT - shouldn't an exception be raised by posix-filesys.sml? */
bool posix_filesys_access(string name, word mode)
{
  char *cname = mlstring2cstring(name);
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
