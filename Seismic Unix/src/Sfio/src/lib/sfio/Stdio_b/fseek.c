#include	"sfstdio.h"

/*	Reposition stream IO pointer
**	Written by Kiem-Phong Vo
*/

#ifndef lcloff_t
#define lcloff_t	long
#endif

#if __STD_C
int fseek(FILE* f, lcloff_t offset, int whence)
#else
int fseek(f,offset,whence)
reg FILE*	f;
reg lcloff_t	offset;
reg int		whence;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	/* ready for either read or write */
#if _FILE_cnt
	f->std_cnt = 0;
#endif
#if _FILE_r
	f->std_r = 0;
#endif
#if _FILE_w
	f->std_w = 0;
#endif
#if _FILE_readptr
	f->std_readptr = f->std_readend = NIL(uchar*);
#endif
#if _FILE_writeptr
	f->std_writeptr = f->std_writeend = NIL(uchar*);
#endif

	return sfseek(sf, (Sfoff_t)offset, whence|SF_SHARE) < (Sfoff_t)0 ? -1 : 0;
}


#if _lib_fseeko && !_done_fseeko && !defined(fseek)
#define _done_fseeko	1
#undef lcloff_t
#define lcloff_t	stdoff_t
#define fseek		fseeko
#include		"fseek.c"
#undef fseek
#endif

#if _lib___fseeko64 && !_done___fseeko64 && !defined(fseek)
#define _done___fseeko64	1
#undef lcloff_t
#define lcloff_t		stdoff_t
#define fseek			__fseeko64
#include			"fseek.c"
#undef fseek
#endif

#if _lib_fseeko64 && !_done_fseeko64 && !defined(fseek)
#define _done_fseeko64	1
#undef lcloff_t
#define lcloff_t	stdoff_t
#define fseek		fseeko64
#include		"fseek.c"
#undef fseek
#endif
