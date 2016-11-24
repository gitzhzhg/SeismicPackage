#include	"sfstdio.h"

/*	Internal printf engine to write to stdout
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vprintf(const char* form, va_list args)
#else
int vprintf(form,args)
char*	form;          /* format to use */
va_list args;           /* arg list if argf == 0 */
#endif
{
	reg int		rv;
	reg Sfio_t*	sf;

	if(!form || !(sf = _sfstream(stdout)))
		return -1;

	if((rv = (int)sfvprintf(sf,form,args)) < 0)
		_stdseterr(stdout,sf);

	return rv;
}
