#include	"sftest.h"


static int	Count;
static int	Size;

#if __STD_C
ssize_t writef(Sfio_t* f, const Void_t* buf, size_t n, Sfdisc_t* disc)
#else
ssize_t writef(f,buf,n,disc)
Sfio_t*		f;
Void_t*		buf;
size_t		n;
Sfdisc_t*	disc;
#endif
{
	Count += 1;
	if((n % Size) != 0)
		terror("Wrong record size\n");
	return write(sffileno(f),buf,n);
}

Sfdisc_t	Disc = {(Sfread_f)0, writef, (Sfseek_f)0, (Sfexcept_f)0, (Sfdisc_t*)0};

MAIN()
{
	Sfio_t*	f;
	char	buf[550];
	int	i;
	char*	s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

	Count = 0;
	Size = 52;

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "w")) )
		terror("Opening to write\n");
	sfsetbuf(f,buf,sizeof(buf));
	sfset(f,SF_WHOLE,1);
	sfdisc(f,&Disc);

	for(i = 0; i < 100; ++i)
		if(sfwrite(f,s,52) != 52)
			terror("sfwrite failed\n");
	sfclose(f);
	if(Count != 10)
		terror("Wrong number of writes1\n");

	Count = 0;
	Size = 53;

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0),"w")) )
		terror("Opening to write\n");
	sfsetbuf(f,buf,sizeof(buf));
	sfset(f,SF_WHOLE,1);
	sfdisc(f,&Disc);

	for(i = 0; i < 100; ++i)
		if(sfputr(f,s,'\n') != 53)
			terror("sfputr failed\n");
	sfclose(f);
	if(Count != 10)
		terror("Wrong number of writes2\n");

	TSTEXIT(0);
}
