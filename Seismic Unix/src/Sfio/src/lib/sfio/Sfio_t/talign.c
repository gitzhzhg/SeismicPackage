#include	"sftest.h"


static ssize_t	Read;
static int	Count;

#if __STD_C
ssize_t readf(Sfio_t* f, Void_t* buf, size_t n, Sfdisc_t* disc)
#else
ssize_t readf(f,buf,n,disc)
Sfio_t*		f;
Void_t*		buf;
size_t		n;
Sfdisc_t*	disc;
#endif
{
	Count += 1;
	Read += (n = sfrd(f,buf,n,disc));
	return n;
}

Sfdisc_t	Disc = {readf, (Sfwrite_f)0, (Sfseek_f)0, (Sfexcept_f)0, (Sfdisc_t*)0};

MAIN()
{
	Sfio_t*	f;
	int	i;
	char*	s;
	char	buf[1024], rbuf[128*1024];

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "w")) )
		terror("Opening to write\n");

	for(i = 0; i < sizeof(buf); ++i)
		buf[i] = 'a' + (i%26);

	for(i = 0; i < 1024; ++i)
		if(sfwrite(f,buf,sizeof(buf)) != sizeof(buf) )
			terror("Write error\n");
	sfclose(f);

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "r")) )
		terror("Opening to read\n");
	sfsetbuf(f,rbuf,sizeof(rbuf));

	sfdisc(f,&Disc);

	for(i = 0;; i += 64)
	{	sfseek(f,(Sfoff_t)i,0);
		if(!(s = sfreserve(f,619,1)) )
			break;
		sfread(f,s,64);
	}

	if(Read != 1024*sizeof(buf) )
		terror("Count=%d Read=%d\n", Count, Read);

	TSTEXIT(0);
}
