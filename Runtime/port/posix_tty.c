/* ../../Basis/Posix/posix-tty.sml */

#include "s.h"
#include "r.h"
#include <termios.h>

static char Ettynum[] = "posix_tty_num: bad name";

static struct sysval tty_vec[] = {
	{"B0",	B0},
	{"B110",	B110},
	{"B1200",	B1200},
	{"B134",	B134},
	{"B150",	B150},
	{"B1800",	B1800},
	{"B19200",	B19200},
	{"B200",	B200},
	{"B2400",	B2400},
	{"B300",	B300},
	{"B38400",	B38400},
	{"B4800",	B4800},
	{"B50",	B50},
	{"B600",	B600},
	{"B75",	B75},
	{"B9600",	B9600},
	{"BRKINT",	BRKINT},
	{"CLOCAL",	CLOCAL},
	{"CREAD",	CREAD},
	{"CS5",	CS5},
	{"CS6",	CS6},
	{"CS7",	CS7},
	{"CS8",	CS8},
	{"CSIZE",	CSIZE},
	{"CSTOPB",	CSTOPB},
	{"ECHO",	ECHO},
	{"ECHOE",	ECHOE},
	{"ECHOK",	ECHOK},
	{"ECHONL",	ECHONL},
	{"EOF",	VEOF},
	{"EOL",	VEOL},
	{"ERASE",	VERASE},
	{"HUPCL",	HUPCL},
	{"ICANON",	ICANON},
	{"ICRNL",	ICRNL},
	{"IEXTEN",	IEXTEN},
	{"IGNBRK",	IGNBRK},
	{"IGNCR",	IGNCR},
	{"IGNPAR",	IGNPAR},
	{"INLCR",	INLCR},
	{"INPCK",	INPCK},
	{"INTR",	VINTR},
	{"ISIG",	ISIG},
	{"ISTRIP",	ISTRIP},
	{"IXOFF",	IXOFF},
	{"IXON",	IXON},
	{"KILL",	VKILL},
	{"MIN",	VMIN},
	{"NCCS",	NCCS},
	{"NOFLSH",	NOFLSH},
	{"OPOST",	OPOST},
	{"PARENB",	PARENB},	
	{"PARMRK",	PARMRK},
	{"PARODD",	PARODD},
	{"QUIT",	VQUIT},
	{"START",	VSTART},
	{"STOP",	VSTOP},
	{"SUSP",	VSUSP},
	{"TCIFLUSH",	TCIFLUSH},
	{"TCIOFF",	TCIOFF},
	{"TCIOFLUSH",	TCIOFLUSH},
	{"TCION",	TCION},
	{"TCOFLUSH",	TCOFLUSH},
	{"TCOOFF",	TCOOFF},
	{"TCOON",	TCOON},
	{"TCSADRAIN",	TCSADRAIN},
	{"TCSAFLUSH",	TCSAFLUSH},
	{"TCSANOW",	TCSANOW},
	{"TIME",	VTIME},
	{"TOSTOP",	TOSTOP},
};

static struct sysvals tty_values = {
	arraysize(tty_vec),
	tty_vec
};

int
posix_tty_num(cerr er, string key)
{
	return sysval_num(er, Ettynum, &tty_values, key);
}

/*
	Termiorep and termioset must agree
	with ../../Basis/externtys.sml:/termiorep
*/

static ptr_t
Termiorep(struct termios* termios)
{
	val_t fields[6];
	fields[0] = (val_t) termios->c_iflag;
	fields[1] = (val_t) termios->c_oflag;
	fields[2] = (val_t) termios->c_cflag;
	fields[3] = (val_t) termios->c_lflag;
	fields[4] = (val_t) cfgetispeed(termios);
	fields[5] = (val_t) cfgetospeed(termios);
	return alloc_record(fields, arraysize(fields));
}

/* Returns 0 if successful, error code on error. */
static int
termioset(struct termios* termios, ptr_t termiorep, string cc)
{
	memset(termios,0,sizeof(struct termios));
	termios->c_iflag = termiorep[0];
	termios->c_oflag = termiorep[1];
	termios->c_cflag = termiorep[2];
	termios->c_lflag = termiorep[3];
	assert(stringlen(cc) == NCCS);
	memcpy(termios->c_cc, stringbuf(cc), NCCS);
	if(cfsetispeed(termios, termiorep[5]) == -1)
		return errno;
	if(cfsetospeed(termios, termiorep[6]) == -1)
		return errno;
	return 0;
}

uct
posix_tty_tcgetattr(cerr er, int fd)
{
	struct termios* termios = enew_atomic(struct termios);
	if(tcgetattr(fd, termios) == -1)
		send_errno(er,errno);
	return (uct)termios;
}

ptr_t	/*termiosrep*/
posix_tty_tcgetattr_termiorep(uct p)
{
	struct termios* termios = (struct termios*)p;
	return Termiorep(termios);
}

string
posix_tty_tcgetattr_cc(uct p)
{
	struct termios* termios = (struct termios*)p;
	return alloc_string(NCCS, (char*) termios->c_cc);
}

unit
posix_tty_tcgetattr_free(uct p)
{
	struct termios* termios = (struct termios*)p;
	efree(termios);
	return empty_record;
}

unit
posix_tty_tcsetattr(cerr er, int fd, int action, ptr_t termiorep, string cc)
{
	struct termios termios;
	int e = termioset(&termios,termiorep,cc);
	if(e)
		send_errno(er,e);
	else if(tcsetattr(fd, action, &termios) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_tty_tcsendbreak(cerr er, int fd, int duration)
{
	if(tcsendbreak(fd, duration) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_tty_tcdrain(cerr er, int fd)
{
	if(tcdrain(fd) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_tty_tcflush(cerr er, int fd, int queue_selector)
{
	if(tcflush(fd, queue_selector) == -1)
		send_errno(er,errno);
	return empty_record;
}

unit
posix_tty_tcflow(cerr er, int fd, int action)
{
	if(tcflow(fd, action) == -1)
		send_errno(er,errno);
	return empty_record;
}

int
posix_tty_tcgetpgrp(cerr er, int fd)
{
	pid_t pid = tcgetpgrp(fd);
	if(pid == (pid_t)-1)
		send_errno(er,errno);
	return (int)pid;
}

unit
posix_tty_tcsetpgrp(cerr er, int fd, int pgid)
{
	if(tcsetpgrp(fd, (pid_t) pgid) == -1)
		send_errno(er,errno);
	return empty_record;
}
