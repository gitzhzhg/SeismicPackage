#include	"sftest.h"


MAIN()
{
	int	i;
	char	wbuf[1023];
	char	rbuf[1023];
	Sfio_t	*fp;

	for(i = 0; i < sizeof(wbuf); ++i)
		wbuf[i] = (i%26)+'a';
	wbuf[sizeof(wbuf)-1] = '\0';

	if(!(fp = sftmp(0)))
		terror("Opening temp file\n");

	for(i = 0; i < 256; ++i)
		if(sfwrite(fp,wbuf,sizeof(wbuf)) != sizeof(wbuf))
			terror("Writing\n");

	sfseek(fp,(Sfoff_t)0,0);
	sfset(fp,SF_WRITE,0);
	sfsetbuf(fp,NIL(char*),0);
	sfsetbuf(fp,NIL(char*),(size_t)SF_UNBOUND);

	for(i = 0; i < 256; ++i)
	{	if(sfread(fp,rbuf,sizeof(rbuf)) != sizeof(rbuf))
			terror("Reading\n");

		if(strcmp(rbuf,wbuf) != 0)
			terror("Unmatched record\n");
	}

	TSTEXIT(0);
}
