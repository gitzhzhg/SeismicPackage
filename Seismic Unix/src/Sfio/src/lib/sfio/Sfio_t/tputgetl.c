#include	"sftest.h"

MAIN()
{
	int	i, r;
	Sfio_t	*fp;

	if(!(fp = sftmp(8)))
		terror("Can't open temp file\n");

	for(i = -5448; i <= 5448; i += 101)
		if(sfputl(fp,(long)i) < 0)
			terror("Writing %d\n",i);

	sfseek(fp,(Sfoff_t)0,0);

	for(i = -5448; i <= 5448; i += 101)
		if((r = (int)sfgetl(fp)) != i)
			terror("Input=%d, Expect=%d\n",r,i);

	TSTEXIT(0);
}
