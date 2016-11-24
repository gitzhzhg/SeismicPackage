/* OBSOLETE 19961031 -- for shared library compatibility */

#include	"sfhdr.h"

#undef	_sfgetl2

_BEGIN_EXTERNS_
#if _BLD_sfio && defined(__EXPORT__)
#define extern	__EXPORT__
#endif

extern long	_sfgetl2 _ARG_((Sfio_t*, long));

#undef	extern
_END_EXTERNS_

#if __STD_C
long _sfgetl2(reg Sfio_t* f, long v)
#else
long _sfgetl2(f, v)
reg Sfio_t*	f;
long		v;
#endif
{
	if (v < 0)
		return -1;
	sfungetc(f, v);
	return sfgetl(f);
}
