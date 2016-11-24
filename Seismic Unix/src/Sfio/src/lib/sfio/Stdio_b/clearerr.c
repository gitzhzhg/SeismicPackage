#include	"sfstdio.h"

/*	Clear error status from a stream
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
void clearerr(FILE* f)
#else
void clearerr(f)
FILE*	f;
#endif
{
	reg Sfio_t*	sf;
	
	if(f && (sf = _sfstream(f)) )
	{	sfclrlock(sf);
		_stdclrerr(f);
	}
}

#if _lib_clearerr_unlocked && !_done_clearerr_unlocked && !defined(clearerr)
#define _done_clearerr_unlocked	1
#define clearerr	clearerr_unlocked
#include		"clearerr.c"
#undef clearerr
#endif
