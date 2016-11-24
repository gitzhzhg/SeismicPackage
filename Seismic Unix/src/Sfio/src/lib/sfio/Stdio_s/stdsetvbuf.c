#include	"stdio_s.h"

/*	Stdio function setvbuf()
**
**	Written by Kiem-Phong Vo.
*/

#if __STD_C
int _stdsetvbuf(Sfio_t* f, char* buf, int type, size_t size)
#else
int _stdsetvbuf(f,buf,type,size)
Sfio_t*	f;
char*	buf;
int	type;
size_t	size;
#endif
{
	SFMTXSTART(f,-1);

	if(type == _IOLBF)
	{	sfset(f,SF_LINE,1);
	}
	else if((f->flags&SF_STRING))
	{	SFMTXRETURN(f, -1);
	}
	else if(type == _IONBF)
	{	sfsync(f);
		sfsetbuf(f,NIL(Void_t*),0);
		sfset(f,SF_LINE,0);
	}
	else if(type == _IOFBF)
	{	if(size == 0)
			size = SF_BUFSIZE;
		sfsync(f);
		sfsetbuf(f,(Void_t*)buf,size);
		sfset(f,SF_LINE,0);
	}

	SFMTXRETURN(f, 0);
}
