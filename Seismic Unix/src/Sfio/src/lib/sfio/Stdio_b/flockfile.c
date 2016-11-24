#include	"sfstdio.h"

/*	Lock a file stream.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
void flockfile(FILE* f)
#else
void flockfile(f)
reg FILE*	f;
#endif
{
	Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return;

	(void)sfmutex(sf, SFMTX_LOCK);
}
