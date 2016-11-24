#include	"sftest.h"
#include	<signal.h>

#if __STD_C
void alrmf(int sig)
#else
void alrmf(sig)
int	sig;
#endif
{
	terror("Blocking somewhere until alarm went off\n");
}

MAIN()
{
	int	fd[2];
	Sfio_t	*fr, *fw;
	char	*s;
	int	i, j, n;
	char	buf[1024];

	if(sfnew(sfstdout,buf,sizeof(buf),SF_UNBOUND,SF_STRING|SF_WRITE) != sfstdout)
		terror("Reopen sfstdout\n");

	if(pipe(fd) < 0)
		terror("Can't open pipe\n");
	if(!(fr = sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,fd[0],SF_READ)) ||
	   !(fw = sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,fd[1],SF_WRITE)) )
		terror("Can't open stream\n");
	signal(SIGALRM,alrmf);
	sfwrite(fw,"0123456789",10);
	alarm(4);
	if(sfread(fr,buf,10) != 10)
		terror("Can't read data from pipe\n");
	sfwrite(fw,"0123456789",10);
	if(sfmove(fr,fw,(Sfoff_t)10,-1) != 10)
		terror("sfmove failed\n");
	alarm(0);
	sfpurge(fw);
	sfclose(fw);
	sfpurge(fr);
	sfclose(fr);

	if(pipe(fd) < 0)
		terror("Can't open pipe2\n");
	if(!(fr = sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,fd[0],SF_READ)) ||
	   !(fw = sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,fd[1],SF_WRITE)) )
		terror("Can't open stream\n");
	sfset(fr,SF_SHARE|SF_LINE,1);
	sfset(fw,SF_SHARE,1);

	if(sfwrite(fw,"1\n2\n3\n",6) != 6)
		terror("sfwrite failed0\n");
	i = j = -1;
	if(sfscanf(fr,"%d%d\n%n",&i,&j,&n) != 2 || i != 1 || j != 2 || n != 4)
		terror("sfscanf failed0\n");
	
	if(sfscanf(fr,"%d\n%n",&i,&n) != 1 || i != 3 || n != 2)
		terror("sfscanf failed1\n");

	if(sfwrite(fw,"123\n",4) != 4)
		terror("sfwrite failed\n");
	if(!(s = sfreserve(fr,4,0)) )
		terror("sfreserve failed\n");

	sfputr(fw,"abc",'\n');
	if(sfmove(fr,fw,(Sfoff_t)1,'\n') != 1)
		terror("sfmove failed\n");
	if(!(s = sfgetr(fr,'\n',1)) || strcmp(s,"abc") != 0)
		terror("sfgetr failed\n");

	if(sfwrite(fw,"111\n222\n333\n444\n",16) != 16)
		terror("Bad write to pipe\n");

	if(!(s = sfgetr(fr,'\n',1)) )
		terror("sfgetr failed\n");
	if(strcmp(s,"111") != 0)
		terror("sfgetr got wrong string\n");

	if(sfmove(fr,sfstdout,(Sfoff_t)2,'\n') != 2)
		terror("sfmove failed2\n");
	sfputc(sfstdout,0);
	if(strcmp("222\n333\n",buf) != 0)
		terror("sfmove got wrong data\n");
	if(sfmove(fr,NIL(Sfio_t*),(Sfoff_t)1,'\n') != 1)
		terror("sfmove failed\n");

	if(sfwrite(fw,"0123456789",11) != 11)
		terror("Bad write to pipe2\n");
	if(!(s = sfreserve(fr,11,0)) )
		terror("Bad peek size %d, expect 11\n",sfvalue(fr));
	if(strncmp(s,"0123456789",10) != 0)
		terror("Bad peek str %s\n",s);

	/* test for handling pipe error */
	if(pipe(fd) < 0)
		terror("Can't create pipe");
	close(fd[0]);
	if(!(fw = sfnew(NIL(Sfio_t*),NIL(Void_t*),sizeof(buf),fd[1],SF_WRITE)) )
		terror("Can't open stream");
	signal(SIGPIPE,SIG_IGN); /* avoid dying by sigpipe */

	for(i = 0; i < sizeof(buf); ++i)
		buf[i] = 'a';
	buf[sizeof(buf)-1] = 0;
	for(i = 0; i < 3; ++i)
	{	signal(SIGALRM,alrmf); /* do this to avoid infinite loop */
		alarm(4);
		sfprintf(fw, "%s\n", buf); /* this should not block */
		alarm(0);
	}

	TSTEXIT(0);
}
