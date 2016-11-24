#include	"sfstdio.h"

/*	Set IO position pointer
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int fsetpos(reg FILE* f, reg stdfpos_t* pos)
#else
int fsetpos(f, pos)
reg FILE*	f;
reg stdfpos_t*	pos;
#endif
{
	reg Sfio_t*	sf;

	if(!pos || *pos < 0 || !(sf = _sfstream(f)))
		return -1;

	return (stdfpos_t)sfseek(sf, (Sfoff_t)(*pos), SEEK_SET|SF_SHARE) == *pos ? 0 : -1;
}

#if _lib___fsetpos64 && !_done___fsetpos64 && !defined(fsetpos)
#define _done___fsetpos64	1
#define fsetpos		__fsetpos64
#include		"fsetpos.c"
#undef fsetpos
#endif

#if _lib_fsetpos64 && !_done_fsetpos64 && !defined(fsetpos)
#define _done_fsetpos64	1
#define fsetpos		fsetpos64
#include		"fsetpos.c"
#undef fsetpos
#endif
