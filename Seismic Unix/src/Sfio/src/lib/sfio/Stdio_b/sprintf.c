#include	"sfstdio.h"

/*	Format a string
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int sprintf(char* s,  const char* form, ...)
#else
int sprintf(va_alist)
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

	rv = (s && form) ? (int)sfvsprintf(s,SF_MAXINT,form,args) : -1;
	va_end(args);

	return rv;
}

#if __STD_C
int asprintf(char** as,  const char* form, ...)
#else
int asprintf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;
#if __STD_C
	va_start(args,form);
#else
	reg char**	as;
	reg char*	form;
	va_start(args);
	as = va_arg(args,char**);
	form = va_arg(args,char*);
#endif

	rv = (as && form) ? (int)sfvaprints(as,form,args) : -1;
	va_end(args);

	return rv;
}
