#include	"stdio_s.h"

#if __STD_C
int _stdfflush(FILE* f)
#else
int _stdfflush(f)
FILE*	f;
#endif
{
	if(!f)
		return -1;
	if(f->extent >= 0 && !(f->mode&SF_INIT))
		(void)sfseek(f, (Sfoff_t)0, SEEK_CUR|SF_PUBLIC);
	if((f->mode&SF_WRITE) && sfsync(f) < 0)
		return -1;
	if((f->mode&SF_READ) && sfpurge(f) < 0)
		return -1;
	return 0;
}
