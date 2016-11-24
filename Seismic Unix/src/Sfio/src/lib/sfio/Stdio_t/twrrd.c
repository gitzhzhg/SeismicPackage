#include	"stdtest.h"


MAIN()
{
	int	i;
	char	wbuf[1023];
	char	rbuf[1023];
	FILE	*fp;

	for(i = 0; i < sizeof(wbuf); ++i)
		wbuf[i] = (i%26)+'a';
	wbuf[sizeof(wbuf)-1] = '\0';

	if(!(fp = fopen(tstfile(0), "w+")) )
		terror("Opening temp file\n");

	for(i = 0; i < 256; ++i)
		if(fwrite(wbuf,sizeof(wbuf),1,fp) != 1)
			terror("Writing\n");

	fseek(fp,(long)0,0);

	for(i = 0; i < 256; ++i)
	{	if(fread(rbuf,sizeof(rbuf),1,fp) != 1)
			terror("Reading\n");

		if(strcmp(rbuf,wbuf) != 0)
			terror("Unmatched record\n");
	}

	TSTEXIT(0);
}
