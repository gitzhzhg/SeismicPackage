#include	"sfstdio.h"

/*	Write out a byte
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int putc(int c, FILE* f)
#else
int putc(c, f)
reg int		c;
reg FILE*	f;
#endif
{
	return fputc(c,f);
}

#if _lib_putc_unlocked && !_done_putc_unlocked && !defined(putc)
#define _done_putc_unlocked	1
#define putc	putc_unlocked
#include	"putc.c"
#undef putc
#endif
