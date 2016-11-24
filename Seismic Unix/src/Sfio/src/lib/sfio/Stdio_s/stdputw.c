#include	"stdio_s.h"

#if __STD_C
int _stdputw(int w, FILE* f)
#else
int _stdputw(w, f)
int	w;
FILE*	f;
#endif
{
	if(!f)
		return -1;

	(void)sfwrite(f, &w, sizeof(int));
	return sferror(f);
}
