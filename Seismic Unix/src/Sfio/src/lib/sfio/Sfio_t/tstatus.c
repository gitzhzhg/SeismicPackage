#include	"sftest.h"

MAIN()
{
	Sfio_t*	ip;
	Sfio_t*	op;
	int	n;

	if(!(ip = sfopen((Sfio_t*)0, "/dev/null", "r")))
		terror("/dev/null read open\n");
	if(!(op = sfopen((Sfio_t*)0, tstfile(0), "w")))
		terror("Write open\n");

	n = (int)sfmove(ip, op, SF_UNBOUND, -1);

	if(n)
		terror("move count %d != 0\n", n);
	if(!sfeof(ip))
		terror("sfeof(ip) expected\n");
	if(sfeof(op))
		terror("sfeof(op) not expected\n");

	if(sferror(ip))
		terror("sferror(ip) not expected\n");
	if(sferror(op))
		terror("sferror(op) not expected\n");

	if(sfclose(ip))
		terror("sfclose(ip)\n");
	if(sfclose(op))
		terror("sfclose(op)\n");

	TSTEXIT(0);
}
