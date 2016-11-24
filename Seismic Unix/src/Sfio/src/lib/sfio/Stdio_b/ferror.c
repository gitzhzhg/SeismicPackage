#include	"sfstdio.h"

/*	Return the error condition if any.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int ferror(FILE* f)
#else
int ferror(f)
FILE*	f;
#endif
{
	reg Sfio_t*	sf;
	
	if(!(sf = _sfstream(f)))
		return -1;

	_stdseterr(f,sf);
	return sferror(sf);
}

#if _lib_ferror_unlocked && _done_ferror_unlocked && !defined(ferror)
#define _done_ferror_unlocked	1
#define ferror	ferror_unlocked
#include	"ferror.c"
#undef ferror
#endif
