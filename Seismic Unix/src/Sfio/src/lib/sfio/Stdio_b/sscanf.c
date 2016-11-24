#include	"sfstdio.h"

/*	Scan data from a string
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int sscanf(const char* s, const char* form, ...)
#else
int sscanf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;
#if __STD_C
	va_start(args,form);
#else
	reg char*	s;
	reg char*	form;
	va_start(args);
	s = va_arg(args,char*);
	form = va_arg(args,char*);
#endif

	rv = (s && form) ? (int)sfvsscanf(s,form,args) : -1;
	va_end(args);

	return rv;
}
