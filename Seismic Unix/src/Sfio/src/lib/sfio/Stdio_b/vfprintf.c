#include	"sfstdio.h"

/*	Internal printf engine.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int vfprintf(FILE* f, const char* form, va_list args)
#else
int vfprintf(f,form,args)
FILE*	f;
char*	form;          /* format to use */
va_list args;           /* arg list if argf == 0 */
#endif
{
	reg int		rv;
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	if((rv = (int)sfvprintf(sf,form,args)) < 0)
		_stdseterr(f,sf);

	return rv;
}
