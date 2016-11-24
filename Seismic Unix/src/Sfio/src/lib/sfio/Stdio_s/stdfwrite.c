#include	"stdio_s.h"

/*	Write out a block of data
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
ssize_t _stdfwrite(const Void_t* buf, size_t esize, size_t nelts, Sfio_t* f)
#else
ssize_t _stdfwrite(buf, esize, nelts, f)
Void_t*	buf;
size_t	esize;
size_t	nelts;
Sfio_t*	f;
#endif
{
	ssize_t	rv;

	if(!f || !buf)
		return (ssize_t)(-1);

	if((rv = sfwrite(f, buf, esize*nelts)) > 0)
		return (esize == 0 ? 0 : rv/esize);
	else	return 0;
}
