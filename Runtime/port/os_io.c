/* ../../Basis/Unix/os-io.sml */

#include "s.h"
#include "r.h"
#include <poll.h>

static int
timeout(intpair_option torep)
{
	if(torep != 0){ 	/* is it SOME? */
		int *sec_usec = (int *)torep;
		int sec = sec_usec[0];
		int usec = sec_usec[1];
		return 1000 * sec + usec / 1000;
	}
	else
		return -1;	/* infinite timeout */
}

uct
os_io_poll(cerr er, int n, intword_list evlist, intpair_option torep)
{
	struct pollfd* fds = (struct pollfd*)emalloc(sizeof(struct pollfd) * n);
	int to = timeout(torep);
	int i;

	for(i=0; i<n; i++) {
		int* fd_event = (int*)(evlist[0]);
		int fd = fd_event[0];
		word event = fd_event[1];
		evlist = (intword_list)(evlist[1]);
		fds[i].fd = fd;
		fds[i].events = event;
	}
	assert(evlist == 0);
	if(poll(fds,n,to) == -1){
		send_errno(er,errno);
		efree(fds);
		return 0;
	}
	else
		return (uct)fds;
}

intword
os_io_poll_nth(uct p, int n)
{
	struct pollfd* fds = (struct pollfd*)p;
	val_t fields[2];
	fields[0] = (val_t) fds[n].fd;
	fields[1] = (val_t) fds[n].revents;
	return alloc_record(fields, arraysize(fields));
}

unit
os_io_poll_free(uct p)
{
	struct pollfd* fds = (struct pollfd*)p;
	efree(fds);
	return empty_record;
}
