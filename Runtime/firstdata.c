/* Used only for solaris */

void dummyFirstFunction(void)
{
}

unsigned long firsttext = (unsigned long) &dummyFirstFunction;
unsigned long firstdata = (unsigned long) &firsttext;


