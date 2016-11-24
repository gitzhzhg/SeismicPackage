#include "stdtest.h"
#include <time.h>

MAIN()
{
	struct tm*	tmbufp;
	time_t		l1, l2;
	FILE*		f;
	char		buf[1024];

	if(!(f = fopen(tstfile(0),"w+")) )
		terror("Open file to write");

	/* these sometimes call rare Stdio functions such as fread_unlocked.
	   Hopefully, iffe did catch these and built them.
	*/
	l2 = time(&l1);
	tmbufp = localtime(&l2);

	fprintf(f, "%ld %ld %p", l1, l2, tmbufp);
	fseek(f, 0L, 0);
	fread(buf, 1, sizeof(buf), f);

	TSTEXIT(0);
}
