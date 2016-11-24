#include	"stdtest.h"

MAIN()
{
	FILE	*fp;

	if(!(fp = freopen("/dev/null", "w", stdout)) )
		terror("Can't reopen stdout\n");
	if(fp != stdout)
		terror("Didn't get the right file pointer\n");

	TSTEXIT(0);
}
