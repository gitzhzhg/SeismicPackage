#include	"sfhdr.h"

/*
 * for backwards compatibility with pre-threaded sfgetl() inline
 */

#ifdef __EXPORT__
#define extern	__EXPORT__
#endif

extern
#if __STD_C
Sfulong_t _sfgetu(reg Sfio_t* f)
#else
Sfulong_t _sfgetu(f)
reg Sfio_t*	f;
#endif
{
	sfungetc(f, (unsigned char)_SF_(f)->val);
	return sfgetu(f);
}
