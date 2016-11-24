#include	"sfstdio.h"

/*	Get a word.
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int getw(FILE* f)
#else
int getw(f)
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;
	int		w;

	if(!(sf = _sfstream(f)))
		return -1;

	if(sfread(sf,(char*)(&w),sizeof(int)) != sizeof(int))
	{	_stdseterr(f,sf);
		return -1;
	}
	else	return w;
}
