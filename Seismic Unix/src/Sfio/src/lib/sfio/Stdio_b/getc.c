#include	"sfstdio.h"

/*	Get a byte
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int getc(reg FILE* f)
#else
int getc(f)
reg FILE* f;
#endif
{
	return fgetc(f);
}

#if _lib_getc_unlocked && !_done_getc_unlocked && !defined(getc)
#define _done_getc_unlocked	1
#define getc	getc_unlocked
#include	"getc.c"
#undef getc
#endif
