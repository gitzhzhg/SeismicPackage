#include	"sftest.h"

MAIN()
{
	Sfio_t*	f;

	if(!(f = sfopen(NIL(Sfio_t*),"ab","sr")) )
		terror("Can't open stream\n");
	if(sfeof(f) || sferror(f))
		terror("Can't be eof or error yet\n");
	if(sfgetc(f) != 'a')
		terror("Got wrong data\n");
	if(sfeof(f) || sferror(f))
		terror("Can't be eof or error yet2\n");
	if(sfgetc(f) != 'b')
		terror("Got wrong data2\n");
	if(sfeof(f) || sferror(f))
		terror("Can't be eof or error yet3\n");
	if(sfgetc(f) >= 0)
		terror("Got wrong data2\n");
	if(!sfeof(f))
		terror("Should be eof now\n");
	if(sfseek(f,(Sfoff_t)(-1),2) != 1)
		terror("Seek error\n");
	if(sfeof(f))
		terror("Shouldn't be eof any more\n");

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "w+")) )
		terror("Can't open stream2\n");
	if(sfeof(f) || sferror(f))
		terror("Can't be eof or error yet2\n");
	if(sfwrite(f,"ab",2) != 2)
		terror("Can't write data\n");
	if(sfseek(f,(Sfoff_t)0,0) != 0)
		terror("Can't seek back\n");
	if(sfgetc(f) != 'a')
		terror("Got wrong data3\n");
	if(sfeof(f) || sferror(f))
		terror("Can't be eof or error yet4\n");
	if(sfgetc(f) != 'b')
		terror("Got wrong data4\n");
	if(sfeof(f) || sferror(f))
		terror("Can't be eof or error yet5\n");
	if(sfgetc(f) >= 0)
		terror("Got wrong data5\n");
	if(!sfeof(f))
		terror("Should be eof now2\n");
	if(sfseek(f,(Sfoff_t)(-1),2) != 1)
		terror("Seek error2\n");
	if(sfeof(f))
		terror("Shouldn't be eof any more2\n");

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0),"w+")) )
		terror("Reopening %s\n", tstfile(0));
	sfwrite(f,"1234567890",10);
	sfseek(f,(Sfoff_t)0,0);

	if(sfopen(sfstdout, tstfile(1), "w") != sfstdout)
		terror("Opening %s\n", tstfile(1));

	if(sfmove(f,sfstdout,(Sfoff_t)(-1),-1) != 10)
		terror("sfmove failed\n");
	if(!sfeof(f))
		terror("f should be eof\n");
	if(sferror(sfstdout))
		terror("sfstdout should not be in error\n");

	TSTEXIT(0);
}
