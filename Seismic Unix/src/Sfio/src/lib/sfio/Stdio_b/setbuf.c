#include	"sfstdio.h"

/*	Change stream buffer
**	Written by Kiem-Phong Vo
*/


#if __STD_C
void setbuf(reg FILE* f, char* buf)
#else
void setbuf(f,buf)
reg FILE*	f;
char*		buf;
#endif
{
	reg Sfio_t*	sf;

	if(!(sf = _sfstream(f)))
		return;

	(void)sfsetbuf(sf,(Void_t*)buf,BUFSIZ);
}
