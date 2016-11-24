#include	"sfstdio.h"

/*	Read input from stdin using a given format
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int scanf(const char* form, ...)
#else
int scanf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;
	reg Sfio_t*	sf;
#if __STD_C
	va_start(args,form);
#else
	reg char*	form;	/* scanning format */
	va_start(args);
	form = va_arg(args,char*);
#endif

	if(!form || !(sf = _sfstream(stdin)))
		return -1;

	if((rv = (int)sfvscanf(sf,form,args)) <= 0)
		_stdseterr(stdin,sf);

	va_end(args);

	return rv;
}
