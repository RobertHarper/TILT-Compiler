/* ../../Basis/Posix/posix-tty.sml */

#include "s.h"
#include "r.h"
#include <termios.h>

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

/*int*/cresult
posix_tty_num(string key)
{
	return sysval_num("posix_tty_num", &tty_values, key);
}

/*
	Termiorep and termioset must agree
	with type termiorep in ../../Basis/externtys.sml
*/

static ptr_t
Termiorep(struct termios* termios)
{
	val_t fields[7];
	fields[0] = (val_t) termios->c_iflag;
	fields[1] = (val_t) termios->c_oflag;
	fields[2] = (val_t) termios->c_cflag;
	fields[3] = (val_t) termios->c_lflag;
	fields[4] = (val_t) alloc_string(NCCS, (char*) termios->c_cc);
	fields[5] = (val_t) cfgetispeed(termios);
	fields[6] = (val_t) cfgetospeed(termios);
	return alloc_record(fields, 1<<4, arraysize(fields));
}

/* Returns 0 if successful, error code on error. */
static int
termioset(struct termios* termios, ptr_t termiorep)
{
	memset(termios,0,sizeof(struct termios));
	termios->c_iflag = termiorep[0];
	termios->c_oflag = termiorep[1];
	termios->c_cflag = termiorep[2];
	termios->c_lflag = termiorep[3];
	{
		string cc = (string)termiorep[4];
		assert(stringlen(cc) == NCCS);
		memcpy(termios->c_cc, stringbuf(cc), NCCS);
	}
	if(cfsetispeed(termios, termiorep[5]) == -1)
		return errno;
	if(cfsetospeed(termios, termiorep[6]) == -1)
		return errno;
	return 0;
}

/*termiorep*/cresult
posix_tty_tcgetattr(int fd)
{
	struct termios termios;
	if(tcgetattr(fd, &termios) == -1)
		return Error(SysErr(errno));
	else {
		return NormalPtr(Termiorep(&termios));
	}
}

/*unit*/cresult
posix_tty_tcsetattr(int fd, int action, ptr_t termiorep)
{
	struct termios termios;
	int e = termioset(&termios,termiorep);
	if(e)
		return Error(SysErr(e));
	else
		return unit_cresult(tcsetattr(fd, action, &termios));
}

/*unit*/cresult
posix_tty_tcsendbreak(int fd, int duration)
{
	return unit_cresult(tcsendbreak(fd, duration));
}

/*unit*/cresult
posix_tty_tcdrain(int fd)
{
	return unit_cresult(tcdrain(fd));
}

/*unit*/cresult
posix_tty_tcflush(int fd, int queue_selector)
{
	return unit_cresult(tcflush(fd, queue_selector));
}

/*unit*/cresult
posix_tty_tcflow(int fd, int action)
{
	return unit_cresult(tcflow(fd, action));
}

/*int*/cresult
posix_tty_tcgetpgrp(int fd)
{
	pid_t pid = tcgetpgrp(fd);
	if(pid == (pid_t)-1)
		return Error(SysErr(errno));
	else
		return Normal(pid);
}

/*unit*/cresult
posix_tty_tcsetpgrp(int fd, int pgid)
{
	return unit_cresult(tcsetpgrp(fd, (pid_t) pgid));
}
