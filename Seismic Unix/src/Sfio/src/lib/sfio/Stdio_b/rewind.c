#include	"sfstdio.h"

/*	Reposition IO pointer to origin
**	Written by Kiem-Phong Vo
*/


#if __STD_C
void rewind(reg FILE* f)
#else
void rewind(f)
reg FILE*	f;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return;

	(void)sfseek(sf, (Sfoff_t)0, 0|SF_SHARE);

	clearerr(f);
}
