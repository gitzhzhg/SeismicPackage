#include	"sftest.h"

MAIN()
{
	unsigned int	i, r;
	Sfio_t	*fp;

	if(!(fp = sftmp(8)))
		terror("Can't open temp file\n");

	for(i = 10000; i <= 100000; i += 9)
		if(sfputu(fp,i) < 0)
			terror("Writing %u\n",i);

	sfseek(fp,(Sfoff_t)0,0);

	for(i = 10000; i <= 100000; i += 9)
		if((r = (unsigned int)sfgetu(fp)) != i)
			terror("Input=%u, Expect=%u\n",r,i);

	TSTEXIT(0);
}
