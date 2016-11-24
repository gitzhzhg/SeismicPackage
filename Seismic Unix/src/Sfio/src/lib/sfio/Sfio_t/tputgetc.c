#include	"sftest.h"


MAIN()
{	Sfio_t	*f;
	int	i, c;

	if(!(f = sftmp(8)))
		terror("Can't open temp file\n");

	for(i = 0; i < 10000; ++i)
		if(sfputc(f,(i%26)+'a') < 0)
			terror("Writing %c\n",(i%26)+'a');

	sfseek(f,(Sfoff_t)0,0);

	for(i = 0; i < 10000; ++i)
		if((c = sfgetc(f)) != ((i%26)+'a'))
			terror("Input=%#o, Expect=%c\n",c,(i%26)+'a');

	TSTEXIT(0);
}
