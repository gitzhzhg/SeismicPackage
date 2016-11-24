#include	"sfstdio.h"

/*	Internal scanf engine to read from stdin
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vscanf(const char* form, va_list args)
#else
int vscanf(form,args)
char*	form;          /* format to use */
va_list args;           /* arg list if argf == 0 */
#endif
{
	reg int		rv;
	reg Sfio_t*	sf;

	if(!form || !(sf = _sfstream(stdin)))
		return -1;

	if((rv = (int)sfvscanf(sf,form,args)) <= 0)
		_stdseterr(stdin,sf);

	return rv;
}
