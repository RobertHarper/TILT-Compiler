/* On Digital Unix, some care is needed to get the right semantics for sigaction().
   Without _XOPEN_SOURCE_EXTENDED, the third (void*) parameter to a SIGINFO-type
   signal handling function will not have type ucontext_t*.
*/


#if defined(alpha_osf) && !defined(_XOPEN_SOURCE_EXTENDED)
#define _XOPEN_SOURCE_EXTENDED
#endif

#include <signal.h>

int my_sigaction(int signal, const struct sigaction* action, struct sigaction* o_action)
{
  return sigaction(signal, action, o_action);
}
