#include	"sfstdio.h"

/*	Get a string from stdin.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
char* gets(char* buf)
#else
char* gets(buf)
char*	buf;
#endif
{
	reg Sfio_t*	sf;
	reg char*	rv;

	if(!buf || !(sf = _sfstream(stdin)))
		return NIL(char*);

	if(!(rv = _stdgets(sf,buf,BUFSIZ,1)))
		_stdseterr(stdin,sf);
	return rv;
}
