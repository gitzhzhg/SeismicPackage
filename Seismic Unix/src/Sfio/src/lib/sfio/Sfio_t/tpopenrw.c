#include	"sftest.h"

MAIN()
{
	Sfio_t*	f;
	char	buf[1024], *s;
	int	n;
#ifdef DEBUG
	Sfio_t*	logf = sfopen(0,"LOG","a"); sfsetbuf(logf,NIL(Void_t*),0);
#endif

	alarm(10);
	if(argc > 1)
	{	/* coprocess only */
		while((s = sfreserve(sfstdin,-1,0)) )
		{
#ifdef DEBUG
			sfwrite(logf, s, sfvalue(sfstdin));
#endif
			sfwrite(sfstdout, s, sfvalue(sfstdin));
		}
		return 0;
	}

	/* make coprocess */
	if(!(f = sfpopen(NIL(Sfio_t*), sfprints("%s -p",argv[0]), "r+")))
		terror("Opening for read/write\n");
	for(n = 0; n < 10; ++n)
	{	sfsprintf(buf,sizeof(buf),"Line %d",n);
		sfputr(f,buf,'\n');
		if(!(s = sfgetr(f,'\n',1)))
			terror("Did not read back line\n");
		if(strcmp(s,buf) != 0)
			terror("Input=%s, Expect=%s\n",s,buf);
	}

	if(sfputr(f,"123456789",'\n') != 10)
		terror("Bad write");

	if(sfread(f,buf,3) != 3)
		terror("Did not get data back\n");
	if(strncmp(s,"123",3) != 0)
		terror("Wrong data\n");

	if(sfwrite(f,"aaa",3) != 3 || sfputc(f,'\n') != '\n')
		terror("Fail on write\n");

	if(!(s = sfgetr(f,'\n',1)) )
		terror("Should have gotten 456789\n"); 
	if(strcmp(s,"456789") != 0)
		terror("Wrong data2\n");

	if(!(s = sfgetr(f,'\n',1)) )
		terror("Should have gotten aaa\n"); 
	if(strcmp(s,"aaa") != 0)
		terror("Wrong data3\n");

	sfclose(f);
	
	TSTEXIT(0);
}
