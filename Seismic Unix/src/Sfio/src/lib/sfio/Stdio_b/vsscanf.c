#include	"sfstdio.h"

/*	Internal scanf engine to read from a string buffer
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vsscanf(char* s, const char* form, va_list args)
#else
int vsscanf(s,form,args)
reg char*	s;
reg char*	form;
va_list		args;
#endif
{
	return (s && form) ? (int)sfvsscanf(s,form,args) : -1;
}
