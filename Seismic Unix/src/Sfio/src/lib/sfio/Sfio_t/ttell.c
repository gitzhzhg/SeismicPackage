#include	"sftest.h"

#if __STD_C
static ssize_t discread(Sfio_t* f, void* buf, size_t n, Sfdisc_t* disc)
#else
static ssize_t discread(f, buf, n, disc)
Sfio_t*		f;
Void_t*		buf;
size_t		n;
Sfdisc_t*	disc;
#endif
{
	return sfrd(f, buf, n, disc);
}

#if __STD_C
static ssize_t discwrite(Sfio_t* f, const void* buf, size_t n, Sfdisc_t* disc)
#else
static ssize_t discwrite(f, buf, n, disc)
Sfio_t*		f;
Void_t*		buf;
size_t		n;
Sfdisc_t*	disc;
#endif
{
	return sfwr(f, buf, n, disc);
}

#if __STD_C
static Sfoff_t discseek(Sfio_t* f, Sfoff_t offset, int type, Sfdisc_t* disc)
#else
static Sfoff_t discseek(f, offset, type, disc)
Sfio_t*		f;
Sfoff_t		offset;
int		type;
Sfdisc_t*	disc;
#endif
{
	return (Sfoff_t)(-1);	/* pretend that stream is unseekable */
}

Sfdisc_t	Disc1 = { discread, discwrite };
Sfdisc_t	Disc2 = { discread, discwrite };
Sfdisc_t	Disc3 = { discread, discwrite };

MAIN()
{
	Sfio_t*	f;
	char*	s;
	int	i;

	alarm(10);
	if(argc > 1)
        {       /* coprocess only */
                while((s = sfgetr(sfstdin,'\n',0)) )
                        sfwrite(sfstdout,s,sfvalue(sfstdin));
		TSTEXIT(0);
        }

	if(!(f = sfpopen(NIL(Sfio_t*),sfprints("%s -p",argv[0]),"r+")) )
		terror("Open coprocess\n");

	if(sfwrite(f,"123\n",4) != 4)
		terror("Write coprocess\n");
	if(sftell(f) != 4)
		terror("sftell1\n");

	if(!(s = sfreserve(f,4,0)) || strncmp(s,"123\n",4) != 0 )
		terror("Read coprocess\n");
	if(sftell(f) != 8)
		terror("sftell2\n");

	sfset(f,SF_SHARE,1);

	if(sfwrite(f,"456\n",4) != 4)
		terror("Write coprocess2\n");
	if(sftell(f) != 12)
		terror("sftell 3\n");

	if(!(s = sfreserve(f,4,0)) || strncmp(s,"456\n",4) != 0 )
		terror("Read coprocess2\n");
	if(sftell(f) != 16)
		terror("sftell 4\n");

	sfclose(f);

	/* the below tests to see if stream position is correct when
	   multiple disciplines are put on a stream.
	*/
	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "w")))
		terror("Opening file to write");
	sfdisc(f,&Disc1);
	sfdisc(f,&Disc2);
	sfdisc(f,&Disc3);

	for(i = 0; i < 100; ++i)
	{	if(sfputr(f, "123456789", '\n') != 10)
			terror("Can't write out strings");
		sfsync(f);
		if(sftell(f) != (Sfoff_t)((i+1)*10) )
			terror("Wrong position");
	}
	sfclose(f);

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "r")))
		terror("Opening file to read");
	sfdisc(f,&Disc1);
	sfdisc(f,&Disc2);
	sfdisc(f,&Disc3);

	for(i = 0; i < 100; ++i)
	{	if(!(s = sfgetr(f, '\n', 1)) )
			terror("Can't read string");
		if(strcmp(s,"123456789") != 0)
			terror("Wrong string");
		if(sftell(f) != (Sfoff_t)((i+1)*10) )
			terror("Wrong position");
	}
	sfclose(f);

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "r")))
		terror("Opening file to read");
	Disc1.seekf = discseek; sfdisc(f,&Disc1);
	Disc2.seekf = discseek; sfdisc(f,&Disc2);
	Disc3.seekf = discseek; sfdisc(f,&Disc3);

	for(i = 0; i < 100; ++i)
	{	if(!(s = sfgetr(f, '\n', 1)) )
			terror("Can't read string");
		if(strcmp(s,"123456789") != 0)
			terror("Wrong string");
		if(sftell(f) != (Sfoff_t)((i+1)*10) )
			terror("Wrong position");
	}
	sfclose(f);

	TSTEXIT(0);
}
