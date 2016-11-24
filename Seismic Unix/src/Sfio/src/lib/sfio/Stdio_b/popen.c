#include	"sfstdio.h"

/*	Opening a process as a stream
**	Written by Kiem-Phong Vo
*/


#if __STD_C
FILE* popen(const char* string,const char* mode)
#else
FILE* popen(string,mode)
char*	string;
char*	mode;
#endif
{
	reg Sfio_t*	sf;
	reg FILE*	f;

	if(!(sf = sfpopen((Sfio_t*)(-1), string, mode)))
		f = NIL(FILE*);
	else if(!(f = _stdstream(sf, NIL(FILE*))))
		sfclose(sf);
	else
	{	int	uflag;
		_sftype(mode, NIL(int*), &uflag);
		if(!uflag)
			sf->flags |= SF_MTSAFE;
	}

	return(f);
}
