#include	"sfstdio.h"
#include	"string.h"

#if __STD_C
void perror(const char* s)
#else
void perror(s)
char*	s;
#endif
{
	char*		err;
	extern int	errno;

	if((err = strerror(errno)) )
		sfprintf(sfstderr,"%s: %s\n", s ? s : "", err);
}
