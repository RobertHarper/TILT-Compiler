#include <errno.h>
#include <assert.h>
#include <sys/fcntl.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <termios.h>
#include <sys/wait.h>

typedef struct {        /* The representation of system constants */
    int         id;
    char        *name;
} sys_const_t;

typedef struct {        
    char        *name;
    int         id;
} name_val_t;


static sys_const_t tbl[] = {
        {EACCES,        "acces"},
        {EAGAIN,        "again"},
#if (defined(EWOULDBLOCK) && (EWOULDBLOCK != EAGAIN))
        {EWOULDBLOCK,   "wouldblock"},
#endif
        {EBADF,         "badf"},
#ifdef EBADMSG
        {EBADMSG,       "badmsg"},
#else
        {0,             "badmsg"},
#endif
        {EBUSY,         "busy"},
#ifdef ECANCELED
        {ECANCELED,     "canceled"},
#else
        {0,             "canceled"},
#endif
        {ECHILD,        "child"},
        {EDEADLK,       "deadlk"},
        {EDOM,          "dom"},
        {EEXIST,        "exist"},
        {EFAULT,        "fault"},
        {EFBIG,         "fbig"},
        {EINPROGRESS,   "inprogress"},
        {EINTR,         "intr"},
        {EINVAL,        "inval"},
        {EIO,           "io"},
        {EISDIR,        "isdir"},
        {ELOOP,         "loop"},
        {EMFILE,        "mfile"},
        {EMLINK,        "mlink"},
        {EMSGSIZE,      "msgsize"},
        {ENAMETOOLONG,  "nametoolong"},
        {ENFILE,        "nfile"},
        {ENODEV,        "nodev"},
        {ENOENT,        "noent"},
        {ENOEXEC,       "noexec"},
        {ENOLCK,        "nolck"},
        {ENOMEM,        "nomem"},
        {ENOSPC,        "nospc"},
        {ENOSYS,        "nosys"},
        {ENOTDIR,       "notdir"},
        {ENOTEMPTY,     "notempty"},
#ifdef ENOTSUP
        {ENOTSUP,       "notsup"},
#else
        {0,             "notsup"},
#endif
        {ENOTTY,        "notty"},
        {ENXIO,         "nxio"},
        {EPERM,         "perm"},
        {EPIPE,         "pipe"},
        {ERANGE,        "range"},
        {EROFS,         "rofs"},
        {ESPIPE,        "spipe"},
        {ESRCH,         "srch"},
        {E2BIG,         "toobig"},
        {EXDEV,         "xdev"},
    };


static name_val_t filesys_values [] = {
  {"A_EXEC",	   X_OK},
  {"A_FILE",       F_OK},
  {"A_READ",       R_OK},
  {"A_WRITE",      W_OK},
  {"O_APPEND",     O_APPEND},
  {"O_CREAT",      O_CREAT},
#ifdef O_DSYNC
  {"O_DSYNC",      O_DSYNC},
#else
  {"O_DSYNC",      0},
#endif
  {"O_EXCL",       O_EXCL},
  {"O_NOCTTY",     O_NOCTTY},
  {"O_NONBLOCK",   O_NONBLOCK},
  {"O_RDONLY",     O_RDONLY},
  {"O_RDWR",       O_RDWR},
#ifdef O_RSYNC
  {"O_RSYNC",      O_RSYNC},
#else
  {"O_RSYNC",      0},
#endif
#ifdef O_SYNC
  {"O_SYNC",       O_SYNC},
#else
  {"O_SYNC",       0},
#endif
  {"O_TRUNC",      O_TRUNC},
  {"O_WRONLY",     O_WRONLY},
  {"irgrp",        S_IRGRP},
  {"iroth",        S_IROTH},
  {"irusr",        S_IRUSR},
  {"irwxg",        S_IRWXG},
  {"irwxo",        S_IRWXO},
  {"irwxu",        S_IRWXU},
  {"isgid",        S_ISGID},
  {"isuid",        S_ISUID},
  {"iwgrp",        S_IWGRP},
  {"iwoth",        S_IWOTH},
  {"iwusr",        S_IWUSR},
  {"ixgrp",        S_IXGRP},
  {"ixoth",        S_IXOTH},
  {"ixusr",        S_IXUSR},
};


static name_val_t signal_values [] = {
  {"abrt", SIGABRT},
  {"alrm", SIGALRM},
  {"bus",  SIGBUS},
  {"chld", SIGCHLD},
  {"cont", SIGCONT},
  {"fpe",  SIGFPE},
  {"hup",  SIGHUP},
  {"ill",  SIGILL},
  {"int",  SIGINT},
  {"kill", SIGKILL},
  {"pipe", SIGPIPE},
  {"quit", SIGQUIT},
  {"segv", SIGSEGV},
  {"stop", SIGSTOP},
  {"term", SIGTERM},
  {"tstp", SIGTSTP},
  {"ttin", SIGTTIN},
  {"ttou", SIGTTOU},
  {"usr1", SIGUSR1},
  {"usr2", SIGUSR2},
};


static name_val_t io_values [] = {
  {"F_GETLK",  F_GETLK},
  {"F_RDLCK",  F_RDLCK},
  {"F_SETLK",  F_SETLK},
  {"F_SETLKW", F_SETLKW},
  {"F_UNLCK",  F_UNLCK},
  {"F_WRLCK",  F_WRLCK},
  {"SEEK_CUR", SEEK_CUR},
  {"SEEK_END", SEEK_END},
  {"SEEK_SET", SEEK_SET},
  {"append",   O_APPEND},
  {"cloexec",  FD_CLOEXEC},
#ifdef O_DSYNC
  {"dsync",    O_DSYNC},
#else
  {"dsync",    0},
#endif
  {"nonblock", O_NONBLOCK},
#ifdef O_RSYNC
  {"rsync",    O_RSYNC},
#else
  {"rsync",    0},
#endif
#ifdef O_SYNC
  {"sync",     O_SYNC},
#else
  {"sync",     0},
#endif
};

static name_val_t process_values [] = {
  {"WNOHANG",       WNOHANG},
#ifdef WUNTRACED
  {"WUNTRACED",     WUNTRACED},
#endif
};

static name_val_t tty_values [] = {
  {"B0", B0},
  {"B110", B110},
  {"B1200", B1200},
  {"B134", B134},
  {"B150", B150},
  {"B1800", B1800},
  {"B19200", B19200},
  {"B200", B200},
  {"B2400", B2400},
  {"B300", B300},
  {"B38400", B38400},
  {"B4800", B4800},
  {"B50", B50},
  {"B600", B600},
  {"B75", B75},
  {"B9600", B9600},
  {"BRKINT", BRKINT},
  {"CLOCAL", CLOCAL},
  {"CREAD", CREAD},
  {"CS5", CS5},
  {"CS6", CS6},
  {"CS7", CS7},
  {"CS8", CS8},
  {"CSIZE", CSIZE},
  {"CSTOPB", CSTOPB},
  {"ECHO", ECHO},
  {"ECHOE", ECHOE},
  {"ECHOK", ECHOK},
  {"ECHONL", ECHONL},
  {"EOF", VEOF},
  {"EOL", VEOL},
  {"ERASE", VERASE},
  {"HUPCL", HUPCL},
  {"ICANON", ICANON},
  {"ICRNL", ICRNL},
  {"IEXTEN", IEXTEN},
  {"IGNBRK", IGNBRK},
  {"IGNCR", IGNCR},
  {"IGNPAR", IGNPAR},
  {"INLCR", INLCR},
  {"INPCK", INPCK},
  {"INTR", VINTR},
  {"ISIG", ISIG},
  {"ISTRIP", ISTRIP},
  {"IXOFF", IXOFF},
  {"IXON", IXON},
  {"KILL", VKILL},
  {"MIN", VMIN},
  {"NCCS", NCCS},
  {"NOFLSH", NOFLSH},
  {"OPOST", OPOST},
  {"PARENB", PARENB}, 
  {"PARMRK", PARMRK},
  {"PARODD", PARODD},
  {"QUIT", VQUIT},
  {"START", VSTART},
  {"STOP", VSTOP},
  {"SUSP", VSUSP},
  {"TCIFLUSH", TCIFLUSH},
  {"TCIOFF", TCIOFF},
  {"TCIOFLUSH", TCIOFLUSH},
  {"TCION", TCION},
  {"TCOFLUSH", TCOFLUSH},
  {"TCOOFF", TCOOFF},
  {"TCOON", TCOON},
  {"TCSADRAIN", TCSADRAIN},
  {"TCSAFLUSH", TCSAFLUSH},
  {"TCSANOW", TCSANOW},
  {"TIME", VTIME},
  {"TOSTOP", TOSTOP},
};

static name_val_t sysconf_values [] = {
  {"JOB_CONTROL", _POSIX_JOB_CONTROL},
  {"CLK_TCK", CLK_TCK}
};
