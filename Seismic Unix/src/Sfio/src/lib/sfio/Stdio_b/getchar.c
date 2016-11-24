#include	"sfstdio.h"


/*	Get a byte from stdin.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int getchar(void)
#else
int getchar()
#endif
{
	return fgetc(stdin);
}

#if _lib_getchar_unlocked && !_done_getchar_unlocked && !defined(getchar)
#define _done_getchar_unlocked	1
#define getchar	getchar_unlocked
#include	"getchar.c"
#undef getchar
#endif
