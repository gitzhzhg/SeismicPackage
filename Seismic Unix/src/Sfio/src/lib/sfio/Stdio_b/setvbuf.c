#include	"sfstdio.h"

/*	Change buffer and set/unset line buffering.
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int setvbuf(reg FILE* f, char* buf, int flags, size_t size)
#else
int setvbuf(f, buf, flags, size)
reg FILE*	f;
char*		buf;
int		flags;
size_t		size;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	if(flags == _IOLBF)
		sfset(sf,SF_LINE,1);
	else if(flags == _IONBF)
	{	sfsync(sf);
		sfsetbuf(sf,NIL(Void_t*),0);
		sfset(sf,SF_LINE,0);
	}
	else if(flags == _IOFBF)
	{	if(size == 0)
			size = BUFSIZ;
		sfsync(sf);
		sfsetbuf(sf,buf,size);
		sfset(sf,SF_LINE,0);
	}
	return 0;
}
