#include "s.h"
#include "r.h"
#include "sparc.h"

static void
dummyFirstFunction(void)
{
}

unsigned long firsttext = (unsigned long) &dummyFirstFunction;
unsigned long firstdata = (unsigned long) &firsttext;
