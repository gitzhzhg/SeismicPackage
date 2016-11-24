#include	"stdio_s.h"

/*	Read formatted data from a stream
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int _stdfscanf(Sfio_t* f, const char* form, ...)
#else
int _stdfscanf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;

#if __STD_C
	va_start(args,form);
#else
	reg Sfio_t*	f;
	reg char*	form;	/* scanning format */
	va_start(args);
	f = va_arg(args,Sfio_t*);
	form = va_arg(args,char*);
#endif

	rv = (f && form) ? (int)sfvscanf(f,form,args) : -1;

	va_end(args);
	return rv;
}
