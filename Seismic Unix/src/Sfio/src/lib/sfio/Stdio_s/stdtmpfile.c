#include	"stdio_s.h"

/*	Open a temp stream.
**
**	Written by Kiem-Phong Vo.
*/

Sfio_t* _stdtmpfile()
{
	Sfio_t* f;

	if((f = sftmp(0)) )
		f->flags |= SF_MTSAFE;

	return f;
}
