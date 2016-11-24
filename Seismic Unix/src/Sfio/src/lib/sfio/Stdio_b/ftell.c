#include	"sfstdio.h"

/*	Tell current IO position pointer
**	Written by Kiem-Phong Vo
*/

#ifndef lcloff_t
#define lcloff_t	long
#endif

#if __STD_C
lcloff_t ftell(reg FILE* f)
#else
lcloff_t ftell(f)
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return (lcloff_t)(-1);

	return (lcloff_t)sfseek(sf, (Sfoff_t)0, SEEK_CUR|SF_SHARE);
}


#if _lib_ftello && !_done_ftello && !defined(ftell)
#define _done_ftello	1
#undef lcloff_t
#define lcloff_t	stdoff_t
#define ftell		ftello
#include		"ftell.c"
#undef ftell
#endif

#if _lib___ftello64 && !_done___ftello64 && !defined(ftell)
#define _done___ftello64	1
#undef lcloff_t
#define lcloff_t	stdoff_t
#define ftell			__ftello64
#include			"ftell.c"
#undef ftell
#endif

#if _lib___ftello64 && !_done_ftello64 && !defined(ftell)
#define _done_ftello64	1
#undef lcloff_t
#define lcloff_t	stdoff_t
#define ftell		ftello64
#include		"ftell.c"
#undef ftell
#endif
