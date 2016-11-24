#include	"sfstdio.h"

/*	Format a string with given buffer size.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int snprintf(char* s, size_t n,  const char* form, ...)
#else
int snprintf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;
#if __STD_C
	va_start(args,form);
#else
	reg char*	s;
	reg size_t	n;
	reg char*	form;
	va_start(args);
	s = va_arg(args,char*);
	n = va_arg(args,size_t);
	form = va_arg(args,char*);
#endif

	rv = (s && form) ? (int)sfvsprintf(s,n,form,args) : -1;
	va_end(args);

	return rv;
}

#if _lib___snprintf && !done_lib___snprintf && !defined(snprintf)
#define done_lib___snprintf 1
#define snprintf __snprintf
#include	"snprintf.c"
#endif
