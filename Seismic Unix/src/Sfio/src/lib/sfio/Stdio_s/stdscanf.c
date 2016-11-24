#include	"stdio_s.h"

/*	Read formatted data from a stream
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int _stdscanf(const char* form, ...)
#else
int _stdscanf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;

#if __STD_C
	va_start(args,form);
#else
	reg char	*form;	/* scanning format */
	va_start(args);
	form = va_arg(args,char*);
#endif

	rv = form ? (int)sfvscanf(sfstdin,form,args) : -1;
	va_end(args);
	return rv;
}
