#include	"sfstdio.h"

/*	Write out a set of data
**	Written by Kiem-Phong Vo
*/

#if __STD_C
size_t fwrite(const Void_t* buf, size_t esize, size_t nelts, reg FILE* f)
#else
size_t fwrite(buf,esize,nelts,f)
reg Void_t*	buf;
reg size_t	esize;
reg size_t	nelts;
reg FILE*	f;
#endif
{
	reg ssize_t	rv;
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return 0;

	if((rv = sfwrite(sf,buf,esize*nelts)) >= 0)
		return (esize == 0 ? 0 : rv/esize);
	else
	{	_stdseterr(f,sf);
		return 0;
	}
}

#if _lib_fwrite_unlocked && !_done_fwrite_unlocked && !defined(fwrite)
#define _done_fwrite_unlocked	1
#define fwrite	fwrite_unlocked
#include	"fwrite.c"
#undef fwrite
#endif
