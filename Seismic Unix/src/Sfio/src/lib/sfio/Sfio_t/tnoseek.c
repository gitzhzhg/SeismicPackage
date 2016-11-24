#include	"sftest.h"

char*	buffer;
size_t	size;
size_t	count;

#if __STD_C
Sfoff_t discseek(Sfio_t* f, Sfoff_t offset, int type, Sfdisc_t* disc)
#else
Sfoff_t discseek(f,offset,type,disc)
Sfio_t*		f;
Sfoff_t		offset;
int		type;
Sfdisc_t*	disc;
#endif
{
	return 0;
}

#if __STD_C
ssize_t discwrite(Sfio_t* f, const Void_t* s, size_t n, Sfdisc_t* disc)
#else
ssize_t discwrite(f,s,n,disc)
Sfio_t*		f;
Void_t*		s;
size_t		n;
Sfdisc_t*	disc;
#endif
{
	buffer = (char*)s;
	size = n;
	count += 1;
	return n;
}

Sfdisc_t seekable = { (Sfread_f)0, discwrite, discseek, (Sfexcept_f)0 };

MAIN()
{
	char	buf[1024];

	sfsetbuf(sfstdout,buf,sizeof(buf));
	sfset(sfstdout,SF_LINE,0);

	if(sfdisc(sfstdout,&seekable) != &seekable)
		terror("Can't set discipline\n");
	if(sfseek(sfstdout,(Sfoff_t)0,0) < 0)
		terror("Sfstdout should be seekable\n");
	if(sfwrite(sfstdout,"123\n",4) != 4)
		terror("Can't write\n");
	if(sfwrite(sfstdout,"123\n",4) != 4)
		terror("Can't write\n");
	if(sfdisc(sfstdout,NIL(Sfdisc_t*)) != &seekable)
		terror("Can't pop discipline\n");

	if(buffer != buf || size != 8 || count != 1)
		terror("Wrong calls to write\n");

	TSTEXIT(0);
}
