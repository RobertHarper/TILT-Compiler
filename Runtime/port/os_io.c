/* ../../Basis/Unix/os-io.sml */

#include "s.h"
#include "r.h"
#include <poll.h>

/*intword_list*/cresult
posix_os_poll(intword_list fd_event_list, intpair_option sec_usec_option)
{
	struct pollfd fds[10];	/* XXX: Easy limit to eliminate */
	unsigned long count = 0;
	int i, timeout;
	ptr_t result = 0;        /* nil */

	if(sec_usec_option != 0){ 	/* is it NONE? */
		int *sec_usec = (int *)sec_usec_option;
		int sec = sec_usec[0];
		int usec = sec_usec[1];
		timeout = 1000 * sec + usec / 1000;
	} else
		timeout = -1;	/* infinite timeout */

	while(fd_event_list != 0){	/* is it nil? */
		int *fd_event = (int *)(fd_event_list[0]);
		int fd = fd_event[0];
		int event = fd_event[1];
		fd_event_list = (intword_list)(fd_event_list[1]);
		fds[count].fd = fd;
		fds[count].events = event;
		count++;
		if(count == arraysize(fds))
			return Error(SysErr_msg("posix_os_poll: static limit exceeded"));
	}
	if(poll(fds,count,timeout) == -1){
		return Error(SysErr(errno));
	}
	for (i=count-1; i>=0; i--) {
		intpair car = alloc_intint(fds[i].fd, fds[i].revents);
		result = cons_ptr_alloc(car,result);
	}
	return NormalPtr(result);
}
