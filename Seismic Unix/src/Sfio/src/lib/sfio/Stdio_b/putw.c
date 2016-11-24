#include	"sfstdio.h"

/*	Write out a word
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int putw( int c, FILE* f)
#else
int putw(c, f)
int		c;
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	if(sfwrite(sf,(char*)(&c),sizeof(int)) <= 0)
		_stdseterr(f,sf);

	return sferror(sf);
}
