#include	"sftest.h"

static int	Type;

#if __STD_C
static void notify(Sfio_t* f, int type, int fd)
#else
static void notify(f, type, fd)
Sfio_t*	f;
int		type;
int		fd;
#endif
{
	switch(Type = type)
	{
	case SF_NEW:
	case SF_CLOSING:
	case SF_SETFD:
	case SF_READ:
	case SF_WRITE:
		return;
	default:
		terror("Unexpected nofity-type: %d\n",type);
	}
}

MAIN()
{
	Sfio_t*	f;
	int	fd;

	sfnotify(notify);

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "w")) && Type != SF_NEW)
		terror("Notify did not announce SF_NEW event\n");
	fd = sffileno(f);
	close(fd+5);
	if(sfsetfd(f,fd+5) != fd+5 || Type != SF_SETFD)
		terror("Notify did not announce SF_SETFD event\n");
	if(sfclose(f) < 0 || Type != SF_CLOSING)
		terror("Notify did not announce SF_CLOSING event\n");
	
	if(sfputc(sfstdin,'a') >= 0 || Type != SF_WRITE)
		terror("Notify did not announce SF_WRITE event\n");

	if(sfgetc(sfstdout) >= 0 || Type != SF_READ)
		terror("Notify did not announce SF_READ event\n");

	TSTEXIT(0);
}
