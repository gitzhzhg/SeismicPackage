#include	"sftest.h"

MAIN()
{
	Sfdouble_t	f, v;
	int	i, flag;
	Sfio_t	*fp;

	if(!(fp = sfopen(NIL(Sfio_t*), tstfile(0), "w+")) )
		terror("Can't open temp file\n");

#define BEGV	(Sfdouble_t)(1e-10)
#define ENDV	(Sfdouble_t)(1e-10 + 1)
#define INCR	(Sfdouble_t)(1e-3)

	for(f = BEGV; f < ENDV; f += INCR)
		if(sfputd(fp,f) < 0)
			terror("Writing %.12Lf\n",f);

	sfseek(fp,(Sfoff_t)0,0);
	for(flag = 0, f = BEGV, i = 0; f < ENDV; ++i, f += INCR)
	{	if((v = sfgetd(fp)) == f)
			continue;
		if(v <= (f - 1e-10) || v >= (f + 1e-10) )
			terror("Element=%d Input=%.12Lf, Expect=%.12Lf\n",i,v,f);
		else if(!flag)
		{	twarn("Element=%d Input=%.12Lf, Expect=%.12Lf\n",i,v,f);
			flag = 1;
		}
	}

	TSTEXIT(0);
}
