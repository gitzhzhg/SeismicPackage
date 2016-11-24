#include	"sfstdio.h"

/*	Set line mode
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int setlinebuf(reg FILE* f)
#else
int setlinebuf(f)
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	sfset(sf,SF_LINE,1);
	return(0);
}
