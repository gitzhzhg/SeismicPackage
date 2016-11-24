#include	"stdio_s.h"

#if __STD_C
int _stdgetw(FILE* f)
#else
int _stdgetw(f)
FILE*	f;
#endif
{
	int	w;

	return (f && sfread(f, &w, sizeof(int)) == sizeof(int)) ? w : -1;
}
