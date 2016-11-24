#include	"sfstdio.h"

/*	Return file number.
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int fileno(FILE* f)
#else
int fileno(f)
FILE*	f;
#endif
{
	reg Sfio_t*	sf;
	
	if(!(sf = _sfstream(f)))
		return -1;
	return sffileno(sf);
}

#if _lib_fileno_unlocked && !_done_fileno_unlocked && !defined(fileno)
#define _done_fileno_unlocked	1
#define fileno	fileno_unlocked
#include	"fileno.c"
#undef fileno
#endif
