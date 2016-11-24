#include	"stdio_s.h"


/*	fprintf function
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int _stdfprintf(Sfio_t* f, const char *form, ...)
#else
int _stdfprintf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;

#if __STD_C
	va_start(args,form);
#else
	reg Sfio_t*	f;
	reg char*	form;
	va_start(args);
	f = va_arg(args,Sfio_t*);
	form = va_arg(args,char*);
#endif

	rv = (f && form) ? (int)sfvprintf(f,form,args) : -1;

	va_end(args);

	return rv;
}
