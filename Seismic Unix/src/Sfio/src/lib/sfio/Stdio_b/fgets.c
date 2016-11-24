#include	"sfstdio.h"

/*	Get a line.
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
char* fgets(char* buf, int n, FILE* f)
#else
char* fgets(buf,n,f)
char*	buf;
int	n;
FILE*	f;
#endif
{
	reg Sfio_t*	sf;
	reg char*	rv;

	if(!buf || !(sf = _sfstream(f)) )
		return NIL(char*);
	if(!(rv = _stdgets(sf,buf,n,0)))
		_stdseterr(f,sf);
	return rv;
}

#if _lib_fgets_unlocked && !_done_fgets_unlocked && !defined(fgets)
#define _done_fgets_unlocked	1
#define fgets	fgets_unlocked
#include	"fgets.c"
#undef fgets
#endif
