#include	"sftest.h"

/* see if multiple writers do create a consistent set of records. */

#define N_WRITER	3
#define N_RECORD	10000

static long vrandom()
{
#define 			LOWBITS 	((~((unsigned long)0)) >> 1)
	static unsigned long	hash =		0xdeadbeef;

	return (hash = hash*0xbadbeef + 0xdeadbeef)&LOWBITS;
}

MAIN()
{
	ssize_t	size[N_WRITER][N_RECORD];
	int	count[N_WRITER];
	char	record[N_WRITER][128], *s;
	Sfio_t*	fw[N_WRITER];
	Sfio_t*	fr;
	int	i, r, done;

	/* create random record sizes */
	for(i = 0; i < N_WRITER; ++i)
	for(r = 0; r < N_RECORD; ++r)
		size[i][r] = (ssize_t)(vrandom()%64) + 2;

	/* records for different processes */
	for(i = 0; i < N_WRITER; ++i)
	for(r = 0; r < 128; ++r)
		record[i][r] = '0'+i;

	/* create file */
	fr = sfopen(NIL(Sfio_t*),tstfile(0),"w+");

	/* create records */
	for(i = 0; i < N_WRITER; ++i)
	{	fw[i] = sfopen(NIL(Sfio_t*),tstfile(0),"a");
		count[i] = 0;
	}

	for(done = 0; done < N_WRITER; )
	{	i = (int)(vrandom()%N_WRITER);
		if(count[i] < N_RECORD)
		{	r = size[i][count[i]];
			if(!(s = sfreserve(fw[i],r,1)) || sfvalue(fw[i]) < r )
				terror("sfreserve fails in process %d\n", i);
			memcpy(s,record[i],r-1);
			s[r-1] = '\n';
			sfwrite(fw[i],s,r);

			if((count[i] += 1) == N_RECORD)
			{	done += 1;
				sfclose(fw[i]);
			}
		}
	}

	for(i = 0; i < N_WRITER; ++i)
		count[i] = 0;

	while((s = sfgetr(fr,'\n',0)) )
	{	if((i = s[0] - '0') < 0 || i >= N_WRITER)
			terror("Wrong record type\n");

		for(r = sfvalue(fr)-2; r > 0; --r)
			if(s[r] != s[0])
				terror("Bad record%d, count=%d\n", i, count[i]);

		if(sfvalue(fr) != size[i][count[i]])
			terror("Record%d count=%d size=%d sfvalue=%d\n",
				i, count[i], size[i][count[i]], sfvalue(fr));

		count[i] += 1;
	}

	for(i = 0; i < N_WRITER; ++i)
		if(count[i] != N_RECORD)
			terror("Bad count%d %d\n", i, count[i]);

	TSTEXIT(0);
}
