#include	"sftest.h"

MAIN()
{
	Sfio_t	*f;

	if(argc > 1)
	{	if(sfopen(sfstdin,argv[1],"r") != sfstdin)
			terror("Can't reopen stdin");
		sfmove(sfstdin,sfstdout,(Sfoff_t)(-1),-1);
		return 0;
	}

	if(!(f = sfopen((Sfio_t*)0,tstfile(0),"w")))
		terror("Opening to write\n");
	if(sfputc(f,'a') != 'a')
		terror("sfputc\n");
	if(sfgetc(f) >= 0)
		terror("sfgetc\n");
	
	if(!(f = sfopen(f,tstfile(0),"r")))
		terror("Opening to read\n");
	if(sfgetc(f) != 'a')
		terror("sfgetc2\n");
	if(sfputc(f,'b') >= 0)
		terror("sfputc2\n");

	if(!(f = sfopen(f,tstfile(0),"r+")))
		terror("Opening to read/write\n");

	if(sfgetc(f) != 'a')
		terror("sfgetc3\n");
	if(sfputc(f,'b') != 'b')
		terror("sfputc3\n");
	if(sfclose(f) < 0)
		terror("sfclose\n");

	if(!(f = sfpopen(NIL(Sfio_t*),sfprints("%s %s", argv[0], tstfile(0)),"r")))
		terror("sfpopen\n");
	if(sfgetc(f) != 'a')
		terror("sfgetc4\n");
	if(sfgetc(f) != 'b')
		terror("sfgetc5\n");
	if(sfgetc(f) >= 0)
		terror("sfgetc6\n");

	if(!(f = sfopen(f,tstfile(0),"w")) )
		terror("sfopen\n");
	if(sfputc(f,'a') != 'a')
		terror("sfputc1\n");
	sfsetfd(f,-1);
	if(sfputc(f,'b') >= 0)
		terror("sfputc2\n");
	if(sfclose(f) < 0)
		terror("sfclose\n");

	if(!(f = sfopen(NIL(Sfio_t*),tstfile(0),"a+")) )
		terror("sfopen2\n");
	sfset(f,SF_READ,0);
	if(!sfreserve(f,0,-1) )
		terror("Failed on buffer getting\n");
	if(sfvalue(f) <= 0)
		terror("There is no buffer?\n");

	TSTEXIT(0);
}
