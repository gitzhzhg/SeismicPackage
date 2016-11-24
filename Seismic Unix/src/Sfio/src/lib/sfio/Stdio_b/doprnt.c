#define _in_doprnt	1
#include	"sfstdio.h"

/*	The internal printf engine in older stdios.
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int _doprnt(const char* form, va_list args, FILE* f)
#else
int _doprnt(form,args,f)
char*	form;          /* format to use */
va_list args;           /* arg list if argf == 0 */
FILE*	f;
#endif
{
	reg int		rv;
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)) )
		return -1;

	if((rv = sfvprintf(sf,form,args)) < 0)
		_stdseterr(f,sf);
	return rv;
}
