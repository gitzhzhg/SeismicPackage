#include	"sfstdio.h"

/*	Write out data to stdout using a given format.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int printf(const char* form, ...)
#else
int printf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;
	reg Sfio_t*	sf;
#if __STD_C
	va_start(args,form);
#else
	reg char*	form;	/* print format */
	va_start(args);
	form = va_arg(args,char*);
#endif

	if(!form || !(sf = _sfstream(stdout)))
		return -1;

	if((rv = (int)sfvprintf(sf,form,args)) < 0)
		_stdseterr(stdout,sf);

	va_end(args);

	return rv;
}
