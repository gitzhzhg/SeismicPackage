#include	"sfstdio.h"

/*	Opening a stream given a file descriptor.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
FILE* fdopen(int fd, const char* mode)
#else
FILE* fdopen(fd,mode)
int	fd;
char*	mode;
#endif
{
	reg Sfio_t*	sf;
	reg FILE*	f;
	int		flags, uflag;

	if((flags = _sftype(mode,NIL(int*),&uflag)) == 0)
		return NIL(FILE*);
	if(!uflag)
		flags |= SF_MTSAFE;

	if(!(sf = sfnew(NIL(Sfio_t*), NIL(Void_t*), (size_t)SF_UNBOUND, fd, flags)))
		return NIL(FILE*);
	if(!(f = _stdstream(sf, NIL(FILE*))))
	{	sfclose(sf);
		return NIL(FILE*);
	}

	return(f);
}
