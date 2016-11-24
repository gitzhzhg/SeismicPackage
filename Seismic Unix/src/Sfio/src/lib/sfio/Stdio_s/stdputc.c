#include	"stdio_s.h"

#if __STD_C
int _stdputc(int c, FILE* f)
#else
int _stdputc(c, f)
int	c;
FILE*	f;
#endif
{
	return f ? _std_putc(c,f) : -1;
}
