#include	"sfstdio.h"

/*	Return the eof condition if any.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int feof(FILE* f)
#else
int feof(f)
FILE*	f;
#endif
{
	reg Sfio_t*	sf;
	
	if(!(sf = _sfstream(f)))
		return -1;

	_stdseterr(f,sf);
	return sfeof(sf);
}

#if _lib_feof_unlocked && !_done_feof_unlocked && !defined(feof)
#define _done_feof_unlocked	1
#define feof	feof_unlocked
#include	"feof.c"
#undef feof
#endif
