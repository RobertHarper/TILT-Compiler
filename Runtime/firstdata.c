/* Used only for sparc */

void dummyFirstFunction(void)
{
}

unsigned long firsttext = (unsigned long) &dummyFirstFunction;
unsigned long firstdata = (unsigned long) &firsttext;


