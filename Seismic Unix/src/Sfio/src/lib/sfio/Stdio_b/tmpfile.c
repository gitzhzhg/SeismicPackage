#include	"sfstdio.h"

/*	Creating a temporary stream.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
FILE* tmpfile(void)
#else
FILE* tmpfile()
#endif
{
	reg Sfio_t*	sf;
	reg FILE*	f;

	if(!(sf = sftmp(0)))
		f = NIL(FILE*);
	else if(!(f = _stdstream(sf, NIL(FILE*))))
		sfclose(sf);
	else	sf->flags |= SF_MTSAFE;

	return f;
}

#if _lib___tmpfile64 && !_done___tmpfile64 && !defined(tmpfile)
#define _done___tmpfile64	1
#define tmpfile			__tmpfile64
#include			"tmpfile.c"
#undef tmpfile
#endif

#if _lib_tmpfile64 && !_done_tmpfile64 && !defined(tmpfile)
#define _done_tmpfile64	1
#define tmpfile		tmpfile64
#include		"tmpfile.c"
#undef tmpfile
#endif
