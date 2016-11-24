#include	"stdio_s.h"

#if __STD_C
int _stdgetc(FILE* f)
#else
int _stdgetc(f)
FILE*	f;
#endif
{
	return f ? _std_getc(f) : -1;
}
