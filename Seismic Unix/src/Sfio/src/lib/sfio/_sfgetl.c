#include	"sfhdr.h"

/*
 * for backwards compatibility with pre-threaded sfgetl() inline
 */

#ifdef __EXPORT__
#define extern	__EXPORT__
#endif

extern
#if __STD_C
Sflong_t _sfgetl(reg Sfio_t* f)
#else
Sflong_t _sfgetl(f)
reg Sfio_t*	f;
#endif
{
	sfungetc(f, (unsigned char)_SF_(f)->val);
	return sfgetl(f);
}
