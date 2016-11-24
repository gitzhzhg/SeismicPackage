#include	"sftest.h"

MAIN()
{
	Sfio_t*	f;
	char		buf[1024];
	char*		s;
	int		fd[2];

	close(0);
	if(pipe(fd) < 0 || fd[0] != 0)
		terror("Making pipe\n");

	strcpy(buf,"1234567890");
	if(!(f = sfopen(NIL(Sfio_t*),buf,"s")))
		terror("Opening string stream\n");

	if(!sfstack(f,sfstdin))
		terror("Stacking\n");

	if(write(fd[1],"ab",2) != 2)
		terror("Writing ab to pipe\n");
	if(!(s = sfreserve(f,SF_UNBOUND,1)) || sfvalue(f) != 2)
		terror("Peeking size1 = %d but should be 2\n", sfvalue(f));
	sfread(f,s,0);
	if(strncmp(s,"ab",2) != 0)
		terror("Wrong data1\n");

	if(write(fd[1],"cd",2) != 2)
		terror("Writing cd to pipe\n");
	close(fd[1]);
	if(!(s = sfreserve(f,4,0)) )
		terror("Peeking size2 = %d but should be 4\n", sfvalue(f));
	if(strncmp(s,"abcd",4) != 0)
		terror("Wrong data2\n");

	if(!(s = sfreserve(f,10,0)) )
		terror("Peeking size3 = %d but should be 10\n", sfvalue(f));
	if(strncmp(s,"1234567890",10) != 0)
		terror("Wrong data3\n");

	TSTEXIT(0);
}
