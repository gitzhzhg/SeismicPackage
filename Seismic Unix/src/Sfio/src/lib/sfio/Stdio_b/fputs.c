#include	"sfstdio.h"

/*	Output a string.
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int fputs(const char* s, FILE* f)
#else
int fputs(s, f)
reg char*	s;
reg FILE*	f;
#endif
{
	reg int		rv;
	reg Sfio_t*	sf;

	if(!s || !(sf = _sfstream(f)))
		return -1;

	if((rv = sfputr(sf,s,-1)) < 0)
		_stdseterr(f,sf);
	return rv;
}

#if _lib_fputs_unlocked && !_done_fputs_unlocked && !defined(fputs)
#define _done_fputs_unlocked	1
#define fputs	fputs_unlocked
#include	"fputs.c"
#undef fputs
#endif
