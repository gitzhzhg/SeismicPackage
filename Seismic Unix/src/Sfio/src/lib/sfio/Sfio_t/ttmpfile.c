#include	"sftest.h"

MAIN()
{
	Sfio_t*	f;

	if(!(f = sftmp(1025)))
		terror("Can't open temp file\n");
	if(sffileno(f) >= 0)
		terror("Attempt to create file detected\n");
	if(sfputc(f,0) < 0)
		terror("Can't write to temp file\n");
	if(sffileno(f) >= 0)
		terror("Attempt to create file detected\n");
	if(sfclose(f) < 0)
		terror("Can't close temp file\n");
	if(sffileno(f) >= 0)
		terror("Attempt to create file detected\n");

	if(!(f = sftmp(8)))
		terror("Can't open temp file\n");
	if(sffileno(f) >= 0)
		terror("Attempt to create file detected\n");
	sfdisc(f,NIL(Sfdisc_t*));
	if(sffileno(f) < 0)
		terror("Real file wasn't created\n");
	if(sfclose(f) < 0)
		terror("Can't close temp file\n");

	if(!(f = sftmp(8)))
		terror("Can't open temp file\n");
	if(sffileno(f) >= 0)
		terror("Attempt to create file detected\n");
	if(sfseek(f, (Sfoff_t)8, SEEK_SET) < 0)
		terror("Can't seek on temp file\n");
	if(sffileno(f) >= 0)
		terror("Attempt to create file detected\n");
	if(sfputc(f,0) < 0)
		terror("Can't write to temp file\n");
	if(sffileno(f) < 0)
		terror("Real file wasn't created\n");
	if(sfclose(f) < 0)
		terror("Can't close temp file\n");

	TSTEXIT(0);
}
