#include	"sftest.h"
#include	<signal.h>

#define HANDLER	"Handler"
char	Buf[16];
int	Except;

#if __STD_C
void alrmhandler(int sig)
#else
void alrmhandler(sig)
int	sig;
#endif
{
	strcpy(Buf,HANDLER);

	if(Except == 0)
		signal(sig,alrmhandler);
	else if(Except == 1) /* testing return on interrupt */
	{	Except = 2;
		signal(sig,alrmhandler);
		alarm(2);
	}
	else if(Except == 2)
	{	twarn("System call was automatically resumed by the OS");
		exit(0);
	}
	else	terror("Unexpected Except(%d) state\n", Except);
}

#if __STD_C
int exceptf(Sfio_t* f, int type, Void_t* data, Sfdisc_t* disc)
#else
int exceptf(f, type, data, disc)
Sfio_t* 	f;
int		type;
Void_t*		data;
Sfdisc_t*	disc;
#endif
{
	if(type == SF_ATEXIT || type == SF_DPOP)
		return 0;

	if(type != SF_READ)
		terror("Bad Io type %0o\n", type);
	if(errno != EINTR)
		terror("Bad exception %d\n", errno);
	Except = -1;

	return -1;
}

Sfdisc_t Disc = {NIL(Sfread_f), NIL(Sfwrite_f), NIL(Sfseek_f), exceptf};

MAIN()
{
	int	fd[2];
	ssize_t	n;
	char	buf[128];

	if(pipe(fd) < 0)
		terror("Can't make pipe\n");
	if(sfnew(sfstdin,NIL(Void_t*),(size_t)SF_UNBOUND,fd[0],SF_READ) != sfstdin)
		terror("Can't renew stdin\n");
	sfdisc(sfstdin,&Disc);
	sfset(sfstdin,SF_SHARE,1);

	Except = 0;
	signal(SIGALRM,alrmhandler);
	alarm(2);
	if(sfreserve(sfstdin,1,1))
		terror("Unexpected data\n");
	if(strcmp(Buf,HANDLER) != 0)
		terror("Handler wasn't called\n");
	if(Except >= 0)
		terror("Exception handler wasn't called1\n");

	Buf[0] = 0;
	Except = 0;
	signal(SIGALRM,alrmhandler);
	alarm(2);
	if(sfgetr(sfstdin,'\n',0))
		terror("Unexpected data2\n");
	if(strcmp(Buf,HANDLER) != 0)
		terror("Handler wasn't called2\n");
	if(Except >= 0)
		terror("Exception handler wasn't called2\n");

	Buf[0] = 0;
	Except = 1; /* testing return-on-interrupt feature */
	sfdisc(sfstdin, NIL(Sfdisc_t*)); /* pop discipline		*/
	sfset(sfstdin, SF_IOINTR, 1);	/* set to return on interrupt	*/
	signal(SIGALRM,alrmhandler);
	if(write(fd[1],"0123456789",10) != 10)
		terror("Writing to pipe");
	alarm(2);
	if((n = sfread(sfstdin,buf,sizeof(buf))) != 10)
		twarn("Wrong read size(%d) after an interrupt\n", n);

	TSTEXIT(0);
}
