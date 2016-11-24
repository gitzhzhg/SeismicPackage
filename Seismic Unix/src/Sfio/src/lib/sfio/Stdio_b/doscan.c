#define _in_doscan	1
#include	"sfstdio.h"

/*	The internal scanf engine in older stdios.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int _doscan(FILE *f, const char* form, va_list args)
#else
int _doscan(f,form,args)
FILE*	f;
char*	form;	/* format to use */
va_list args;	/* arg list if argf == 0 */
#endif
{
	reg int		rv;
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	if((rv = (int)sfvscanf(sf,form,args)) <= 0)
		_stdseterr(f,sf);
	return rv;
}
