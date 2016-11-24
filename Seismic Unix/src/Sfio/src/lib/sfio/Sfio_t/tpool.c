#include	"sftest.h"

static char	Serial[128], *S = Serial;
#if __STD_C
ssize_t writef(Sfio_t* f, const Void_t* buf, size_t n, Sfdisc_t* disc)
#else
ssize_t writef(f, buf, n, disc)
Sfio_t*		f;
Void_t*		buf;
size_t		n;
Sfdisc_t*	disc;
#endif
{
	memcpy((Void_t*)S,buf,n);
	S += n;
	return n;
}
Sfdisc_t	Serialdc = {NIL(Sfread_f), writef, NIL(Sfseek_f), NIL(Sfexcept_f) };

MAIN()
{
	int	i, n, on;
	char	*s, *os, *s1, *s2, *s3;
	char	poolbuf[1024];
	Sfio_t	*f1, *f2, *f3, *f4;

	if(!(f1 = sfopen((Sfio_t*)0,tstfile(0),"w+")) ||
	   !(f2 = sfopen((Sfio_t*)0,tstfile(1),"w"))  ||
	   !(f3 = sfopen((Sfio_t*)0,tstfile(2),"w")))
		terror("Opening files\n");

	if(!(f4 = sfopen((Sfio_t*)0,tstfile(0),"r+")) )
		terror("Opening file to read\n");
	sfungetc(f1,'a');
	sfungetc(f4,'b');
	sfpool(f1,f4,0);
	sfungetc(f1,'a');
	sfpool(f1,NIL(Sfio_t*),0);

	sfsetbuf(f2,poolbuf,sizeof(poolbuf));
	sfsetbuf(f3,poolbuf,sizeof(poolbuf));
	if(!sfpool(f2,f3,0) )
		terror("Setting pool\n");

	os = "1234567890\n";
	on = strlen(os);
	for(i = 0; i < 100; ++i)
		if(sfputr(f1,os,-1) < 0)
			terror("Writing data\n");
	sfseek(f1,(Sfoff_t)0,0);
	for(i = 0; i < 100; ++i)
	{	if(!(s = sfgetr(f1,'\n',0)) || (n = sfvalue(f1)) != on)
			terror("Reading data\n");
		if(sfwrite(f2,s,n) != n)
			terror("Writing1\n");
		if(sfwrite(f3,s,n) != n)
			terror("Writing2\n");
	}

	/* see if data matches */
	if(!(f1 = sfopen(f1, tstfile(0), "r")) ||
	   !(f2 = sfopen(f2, tstfile(1), "r")) ||
	   !(f3 = sfopen(f3, tstfile(2), "r")) )
		terror("sfopen for file comparison\n");

	if(sfsize(f1) != sfsize(f2) || sfsize(f2) != sfsize(f3))
		terror("Files don't match sizes");

	n = (int)sfsize(f1);
	if(!(s1 = sfreserve(f1,n,0)) ||
	   !(s2 = sfreserve(f2,n,0)) ||
	   !(s3 = sfreserve(f3,n,0)) )
		terror("Fail reserving data");

	if(memcmp(s1,s2,n) != 0 || memcmp(s2,s3,n) != 0)
		terror("Data do not match");
	sfclose(f1);
	sfclose(f2);
	sfclose(f3);

	f1 = sfstdout; f2 = sfstderr;
	sfdisc(sfstdout,&Serialdc);
	sfdisc(sfstderr,&Serialdc);
	sfset(sfstdout,SF_LINE,0);
	sfset(sfstderr,SF_LINE,0);
	if(sfpool(sfstdout,sfstderr,0) != sfstderr )
		terror("sfpool1\n");
	sfputc(sfstdout,'1');
	sfputc(sfstderr,'2');
	sfputc(sfstdout,'3');
	sfputc(sfstderr,'4');
	sfsync(sfstderr);
	if(strcmp(Serial,"1234") != 0)
		terror("Pool not serializing output\n");
	sfdisc(sfstdout,NIL(Sfdisc_t*));
	sfdisc(sfstderr,NIL(Sfdisc_t*));

	sfclose(sfstdout);
	if(!(f1 = sfopen((Sfio_t*)0,tstfile(0),"r")))
		terror("sfopen\n");
	if(!sfpool(f1,sfstderr,0) )
		terror("sfpool2\n");

	if(!(f1 = sfopen(NIL(Sfio_t*), tstfile(0), "w+")) ||
	   !(f2 = sfopen(NIL(Sfio_t*), tstfile(1), "w+")) ||
	   !(f3 = sfopen(NIL(Sfio_t*), tstfile(2), "w+")) )
		terror("sfopen3\n");
	if(sfpool(f1,f2,SF_SHARE) != f2 || sfpool(f3,f2,SF_SHARE) != f2 )
		terror("sfpool3\n");
	if(sfputc(f3,'x') < 0)
		terror("sfputc to f3\n");
	if(sfputc(f2,'y') < 0)
		terror("sfputc to f2\n");
	if(sfputc(f1,'z') < 0)
		terror("sfputc to f1\n");
	if(sfseek(f1,(Sfoff_t)0,0) != 0)
		terror("sfseek failed on f1\n");
	if(!(s = sfreserve(f1,3,1)) || sfvalue(f1) != 3)
		terror("sfreserve failed on f1\n");
	if(memcmp(s,"xyz",3) != 0)
		terror("Wrong data\n");

	if((os = sfreserve(f2,-1,0)) )
		terror("sfreserve should have failed on f2\n");

	if(sfpool(NIL(Sfio_t*),f2,0) != f1)
		terror("Didn't get right pool head for f2\n");

	if(sfread(f1,s,3) != 3)
		terror("Wrong read on f1\n");

	if(!sfpool(f3,NIL(Sfio_t*),0) )
		terror("sfpool to delete f3\n");

	if(sfpool(f1,NIL(Sfio_t*),0) != f1 )
		terror("sfpool to delete f1\n");

	if(!(f1 = sfopen(NIL(Sfio_t*), tstfile(0), "w+")) ||
	   !(f2 = sfopen(NIL(Sfio_t*), tstfile(1), "w")) )
		terror("sfopen4\n");
	sfputc(f1,'a');
	sfputc(f1,'b');
	sfputc(f1,'c');
	sfset(f1,SF_WRITE,0);	/* off write mode */
	if(sfpool(f1,f2,SF_SHARE) )
		terror("sfpool should fail pooling read+write streams\n");
	if(sfseek(f1,(Sfoff_t)0,0) != (Sfoff_t)0)
		terror("sfseek failed\n");
	if(!(s = sfreserve(f1,3,1)) || memcmp(s,"abc",3) != 0)
		terror("Can't get data from f1\n");

	TSTEXIT(0);
}
