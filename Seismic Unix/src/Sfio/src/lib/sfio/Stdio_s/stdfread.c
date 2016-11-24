#include	"stdio_s.h"

/*	Read in a block of data
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
ssize_t _stdfread(Void_t* buf, size_t esize, size_t nelts, Sfio_t* f)
#else
ssize_t _stdfread(buf, esize, nelts, f)
Void_t*	buf;
size_t	esize;
size_t	nelts;
Sfio_t*	f;
#endif
{
	ssize_t	rv;

	if(!f || !buf)
		return -1;

	if((rv = sfread(f, buf, esize*nelts)) >= 0)
		return esize == 0 ? 0 : rv/esize;
	else	return 0;
}
