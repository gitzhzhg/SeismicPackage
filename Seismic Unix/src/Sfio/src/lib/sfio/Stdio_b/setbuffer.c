#include	"sfstdio.h"

/*	Change stream buffer
**	Written by Kiem-Phong Vo
*/


#if __STD_C
int setbuffer(reg FILE* f, char* buf, size_t size)
#else
int setbuffer(f,buf, size)
reg FILE*	f;
char*		buf;
size_t		size;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return -1;

	sfsetbuf(sf,(Void_t*)buf, buf ? size : 0);
	return 0;
}
