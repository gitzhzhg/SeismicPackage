#include	"sfstdio.h"

/*	Write out a byte to stdout.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int putchar(int c)
#else
int putchar(c)
reg int	c;
#endif
{
	return fputc(c,stdout);
}

#if _lib_putchar_unlocked && !_done_putchar_unlocked && !defined(putchar)
#define _done_putchar_unlocked	1
#define putchar	putchar_unlocked
#include	"putchar.c"
#undef putchar
#endif
