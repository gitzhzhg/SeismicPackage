#include	"sfstdio.h"

/*	Put back an input byte.
**	Written by Kiem-Phong Vo
*/

#if __STD_C
int ungetc(int c, FILE* f)
#else
int ungetc(c,f)
reg int		c;
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	return sfungetc(sf,c);
}
