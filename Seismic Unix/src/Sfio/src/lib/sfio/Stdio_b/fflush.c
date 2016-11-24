#include	"sfstdio.h"

/*	Flushing buffered output data.
**	This has been modified to work with both input&output streams.
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int fflush(reg FILE* f)
#else
int fflush(f)
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;

	if(!f)
		return sfsync(NIL(Sfio_t*));
	if(!(sf = _sfstream(f)))
		return -1;

	if(sf->extent >= 0 && !(sf->mode&SF_INIT) )
		(void)sfseek(sf, (Sfoff_t)0, SEEK_CUR|SF_PUBLIC);

	if((sf->mode&SF_WRITE) && sfsync(sf) < 0)
		return -1;
	if((sf->mode&SF_READ) && sfpurge(sf) < 0)
		return -1;
	return 0;
}

#if _lib_fflush_unlocked && !_done_fflush_unlocked && !defined(fflush)
#define _done_fflush_unlocked	1
#define fflush	fflush_unlocked
#include	"fflush.c"
#undef fflush
#endif
