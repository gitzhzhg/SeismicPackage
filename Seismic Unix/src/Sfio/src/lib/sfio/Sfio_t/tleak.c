#include	"sftest.h"

MAIN()
{
	Sfoff_t	m;

	if(sfopen(sfstdout, tstfile(0), "w") != sfstdout)
		terror("Can't open %s to write\n", tstfile(0));
	if(sfputr(sfstdout,"012345678\n",-1) != 10)
		terror("Can't write to %s\n", tstfile(0));

	if(sfopen(sfstdout, tstfile(1),"w") != sfstdout)
		terror("Can't open %s to write\n", tstfile(1));

	if(sfopen(sfstdin, tstfile(0), "r") != sfstdin)
		terror("Can't open %s to read\n", tstfile(0));

	if((m = sfmove(sfstdin,sfstdout, (Sfoff_t)SF_UNBOUND, -1)) != 10)
		terror("Moving data from %s to %s m=%lld\n",
			tstfile(0), tstfile(1), (Sflong_t)m);

	TSTEXIT(0);
}
