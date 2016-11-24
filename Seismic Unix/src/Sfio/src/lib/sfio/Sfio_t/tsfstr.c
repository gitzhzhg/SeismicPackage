#include	"sftest.h"


MAIN()
{
	Sfio_t	*fp;
	char	*s, buf[1024];
	ssize_t	n;

	if(!(fp = sfstropen()) )
		terror("Can't open a string stream");

	if(sfwrite(fp,"0123456789abcd",15) != 15)
		terror("sfwrite failed");

	if(!(s = sfstrseek(fp, -5, SEEK_CUR)) )
		terror("sfstrseek failed");
	if(strcmp(s,"abcd") != 0)
		terror("Got wrong data");

	if(!(s = sfstruse(fp)) )
		terror("sfstruse failed");
	if(strcmp(s,"0123456789") != 0)
		terror("Got bad data");

	if(!(s = sfstrseek(fp, 0, SEEK_END)) )
		terror("Bad sfstrseek");

	if(!(s = sfstrrsrv(fp, 64*1024)) )
		terror("Can't reserve space");
	if(strcmp(sfstrbase(fp),"0123456789") != 0)
		terror("Lost data");

	if(sfstrbuf(fp, buf, sizeof(buf), 0) < 0)
		terror("sfstrtmp failed");
	if(sfstrbase(fp) != buf)
		terror("Wrong base");
	if(sfstrsize(fp) != sizeof(buf))
		terror("Wrong buffer size");

	sfstrseek(fp,sizeof(buf),SEEK_SET);
	if(sfstruse(fp))
		terror("sfstruse should have failed");

	if(!(fp = sfstropen()) )
		terror("Bad open");
	if(!(s = (char*)sfreserve(fp, -1, SF_WRITE|SF_LOCKR)) )
		terror("sfreserve failed");
	if((n = sfvalue(fp)) < 1)
		terror("sfreserved failed size=%d", n);

	TSTEXIT(0);
}
