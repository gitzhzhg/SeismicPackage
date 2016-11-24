#include	"sftest.h"
#include	<signal.h>

static int	Write_error = 0;

#if __STD_C
static int except(Sfio_t* f, int type, Void_t* obj, Sfdisc_t* disc)
#else
static int except(f, type, obj, disc)
Sfio_t*	f;
int	type;
Void_t* obj;
Sfdisc_t* disc;
#endif
{
	if(type == SF_WRITE)
		Write_error = 1;
	return 0;
}

static Sfdisc_t Wdisc = {NIL(Sfread_f), NIL(Sfwrite_f), NIL(Sfseek_f), except};

MAIN()
{
	int	fd[2];
	Sfio_t	*r, *w;
	char*	s;
	void(*	handler)_ARG_((int));

#define N_STR	10
	alarm(10);
	if(argc > 1) /* to act as a coprocess that read/write ten lines */
	{	int	i, n, rv;

		n = atoi(argv[1]);
		for(i = 0; i < n; ++i)
		{	if(!(s = sfgetr(sfstdin,'\n',1)) )
				terror("Failed to read from stdin");
			if((rv = sfputr(sfstdout, s, '\n')) != sfvalue(sfstdin))
				terror("Failed to write rv=%d stdin=%d",
					rv, sfvalue(sfstdin));
		}
		sfsync(sfstdout);
		TSTEXIT(0);
	}

	signal(SIGPIPE,SIG_IGN);

	if(pipe(fd) < 0)
		terror("Opening pipes\n");

	if(!(w = sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,
			fd[1],SF_WRITE)) )
		terror("Opening write stream\n");
	if(!(r = sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,
			fd[0],SF_READ)) )
		terror("Opening read stream\n");

	sfdisc(w,&Wdisc);

	if(sfputr(w,"abc",'\n') != 4)
		terror("sfputr failed\n");
	if(!(s = sfgetr(r,'\n',1)) || strcmp(s,"abc") != 0)
		terror("sfgetr failed\n");

	if(sfclose(r) < 0)
		terror("sfclose failed closing read stream\n");

	if(sfputr(w,"def",'\n') != 4)
		terror("sfputr failed2\n");

	if(Write_error)
		terror("Write exception should not have been raised\n");

	if(sfclose(w) >= 0)
		terror("sfclose should have failed closing write stream\n");

	if(!Write_error)
		terror("Write exception did not get raised\n");

	signal(SIGPIPE,SIG_DFL);
	if((w = sfpopen(NIL(Sfio_t*), sfprints("%s %d", argv[0], N_STR), "w+")) )
	{	int	i;

		if((handler = signal(SIGPIPE,SIG_IGN)) == SIG_DFL ||
		   handler == SIG_IGN)
			terror("Bad signal handler for SIGPIPE\n");
		signal(SIGPIPE,handler);

		Write_error = 0;
		sfdisc(w,&Wdisc);

		for(i = 0; i < N_STR*10; ++i)
			if(sfputr(w, "abc",'\n') != 4)
				terror("Writing to coprocess1\n");

		sfsync(w);
		sfset(w,SF_READ,1);
		i = sffileno(w);
		sfset(w,SF_WRITE,1);
		if (i != sffileno(w))
			close(sffileno(w));

		for(i = 0; i < N_STR; ++i)
			if(!(s = sfgetr(w,'\n',1)) || strcmp(s,"abc") != 0)
				terror("Reading coprocess [%s]\n", s);
		if((s = sfgetr(w,'\n',1)) )
			terror("sfgetr should have failed\n");

		if(sfputr(w, "abc",'\n') != 4)
			terror("Writing to coprocess2\n");

		if(Write_error)
			terror("Write exception should not have been raised yet\n");

		if(sfclose(w) < 0)
			terror("sfclose should have returned an exit status\n");

		if(!Write_error)
			terror("Write exception should have been raised\n");
	}

	if(signal(SIGPIPE,SIG_DFL) != SIG_DFL)
		terror("SIGPIPE handler should have been SIG_DFL\n");

	/* test for stdio signal handling behavior */
	w = sfpopen((Sfio_t*)(-1), sfprints("%s %d 2>/dev/null",argv[0],N_STR), "w+");
	if(w && (handler = signal(SIGPIPE,SIG_DFL)) != SIG_DFL)
		terror("SIGPIPE handler should have been SIG_DFL\n");

	TSTEXIT(0);
}
