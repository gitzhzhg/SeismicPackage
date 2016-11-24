#include	"stdio_s.h"

/*	Open a stream given a file descriptor.
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
Sfio_t* _stdfreopen(const char* file, const char* mode, Sfio_t* f)
#else
Sfio_t* _stdfreopen(file,mode,f)
char*	file;
char*	mode;
Sfio_t*	f;
#endif
{
	if((f = sfopen(f, file, mode)) )
	{	int	uflag;
		_sftype(mode, NIL(int*), &uflag);
		if(!uflag)
			f->flags |= SF_MTSAFE;
	}

	return f;
}
