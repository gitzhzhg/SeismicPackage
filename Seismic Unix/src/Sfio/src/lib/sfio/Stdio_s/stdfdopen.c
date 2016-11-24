#include	"stdio_s.h"

/*	Open a stream given a file descriptor.
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
Sfio_t* _stdfdopen(reg int fd, reg const char* mode)
#else
Sfio_t* _stdfdopen(fd,mode)
reg int		fd;
reg char*	mode;
#endif
{
	int	sflags, uflag;

	if(fd < 0 || !mode || (sflags = _sftype(mode,NIL(int*),&uflag)) == 0)
		return NIL(Sfio_t*);
	if(!uflag)
		sflags |= SF_MTSAFE;
	return sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,fd,sflags);
}
