#include	"sfstdio.h"

/*	Junk all buffered data
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int fpurge(reg FILE* f)
#else
int fpurge(f)
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	return sfpurge(sf);
}
