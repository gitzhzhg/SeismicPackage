#include	"stdio_s.h"


/*	printf function
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int _stdprintf(const char *form, ...)
#else
int _stdprintf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;

#if __STD_C
	va_start(args,form);
#else
	reg char	*form;
	va_start(args);
	form = va_arg(args,char*);
#endif

	rv = form ? (int)sfvprintf(sfstdout,form,args) : -1;

	va_end(args);
	return rv;
}
