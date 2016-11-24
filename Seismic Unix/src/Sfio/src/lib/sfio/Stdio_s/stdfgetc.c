#include	"stdio_s.h"

#if __STD_C
int _stdfgetc(FILE* f)
#else
int _stdfgetc(f)
FILE*	f;
#endif
{
	return f ? sfgetc(f) : -1;
}
