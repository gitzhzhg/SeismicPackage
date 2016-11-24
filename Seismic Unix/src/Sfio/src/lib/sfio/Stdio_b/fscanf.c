#include	"sfstdio.h"

/*	Scan input data using a given format
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int fscanf(FILE* f, const char* form, ...)
#else
int fscanf(va_alist)
va_dcl
#endif
{
	va_list		args;
	reg int		rv;
	reg Sfio_t*	sf;

#if __STD_C
	va_start(args,form);
#else
	reg FILE*	f;	/* file to be scanned */
	reg char*	form;	/* scanning format */
	va_start(args);
	f = va_arg(args,FILE*);
	form = va_arg(args,char*);
#endif

	if(!form || !(sf = _sfstream(f)))
		return -1;

	if((rv = (int)sfvscanf(sf,form,args)) <= 0)
		_stdseterr(f,sf);

	va_end(args);

	return rv;
}
