#include	"stdtest.h"

MAIN()
{
	FILE	*fp;
	int	i, c, e;

	if(!(fp = fopen(tstfile(0), "w+")) )
		terror("Can't open temp file\n");

	for(i = 0; i < 256; ++i)
		if(putc(i, fp) < 0)
			terror("Bad putc\n");

	for(i = 1; i < 255; ++i)
	{	if(fseek(fp, (long)(-i), SEEK_END) < 0)
			terror("fseek seek_end failed\n");
		if((c = getc(fp)) != (e = 256-i) )
			terror("Bad getc: expect %d, get %d\n", e, c);

		if(fseek(fp, (long)(i), SEEK_SET) < 0)
			terror("fseek seek_set failed\n");
		if((c = getc(fp)) != i)
			terror("Bad getc: expect %d, get %d\n", i, c);

		if(fseek(fp, (long)(-1), SEEK_CUR) < 0)
			terror("fseek seek_cur failed\n");

		if((c = getc(fp)) != i )
			terror("Bad getc: expect %d, get %d\n", i, c);
	}

	TSTEXIT(0);
}
