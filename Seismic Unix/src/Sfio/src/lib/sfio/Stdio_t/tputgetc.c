#include	"stdtest.h"


MAIN()
{	FILE	*f;
	int	i, c;

	if(!(f = fopen(tstfile(0), "w+")) )
		terror("Can't open temp file\n");

	for(i = 0; i < 10000; ++i)
		if(fputc((i%26)+'a', f) < 0)
			terror("Writing %c\n",(i%26)+'a');

	fseek(f, 0L, 0);

	for(i = 0; i < 10000; ++i)
		if((c = fgetc(f)) != ((i%26)+'a'))
			terror("Input=%#o, Expect=%c\n",c,(i%26)+'a');

	TSTEXIT(0);
}
