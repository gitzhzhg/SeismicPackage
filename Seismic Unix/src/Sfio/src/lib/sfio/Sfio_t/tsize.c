#include	"sftest.h"

MAIN()
{
	Sfio_t	*f, *f2;
	char*	s;
	int	i, n;
	char	buf[16*1024];

	if(!(f = sfopen(NIL(Sfio_t*), tstfile(0), "w+") ) )
		terror("Can't open file\n");

	if(sfnputc(f,'a',1000) != 1000)
		terror("Writing\n");

	if(sfseek(f,(Sfoff_t)0,0) != 0)
		terror("Seeking\n");

	if((n = (int)sfsize(f)) != 1000)
		terror("Wrong size %d\n", n);

	if(!(f2 = sfnew(NIL(Sfio_t*),NIL(Void_t*),(size_t)SF_UNBOUND,
			sffileno(f),SF_WRITE)) )
		terror("Can't open stream\n");

	if(sfseek(f2,(Sfoff_t)1999,0) != (Sfoff_t)1999)
		terror("Seeking2\n");
	sfputc(f2,'b');
	sfsync(f2);
	if((n = (int)sfsize(f2)) != 2000)
		terror("Wrong size2 %d\n", n);

	if((n = (int)sfsize(f)) != 1000)
		terror("Wrong size3 %d\n", n);

	sfputc(f,'a');
	sfset(f,SF_SHARE,1);
	if((n = (int)sfsize(f)) != 2000)
		terror("Wrong size4 %d\n", n);

	if(!(f = sfopen(f,NIL(char*),"srw")) )
		terror("Can't open string stream\n");

	sfwrite(f,"0123456789",10);
	if(sfsize(f) != 10)
		terror("String size is wrong1\n");
	sfseek(f,(Sfoff_t)19,0);
	if(sfsize(f) != 10)
		terror("String size is wrong2\n");
	sfputc(f,'a');
	if(sfsize(f) != 20)
		terror("String size is wrong3\n");
	sfseek(f,(Sfoff_t)0,0);
	if(sfsize(f) != 20)
		terror("String size is wrong4\n");
	sfseek(f,(Sfoff_t)0,0);
	if(!(s = sfreserve(f,SF_UNBOUND,1)) && sfvalue(f) != 20)
		terror("String size is wrong5\n");
	sfread(f,s,5);
	if(sfsize(f) != 20)
		terror("String size is wrong6\n");
	sfwrite(f,"01234567890123456789",20);
	if(sfsize(f) != 25)
		terror("String size is wrong7\n");

	strcpy(buf,"0123456789");
	if(!(f = sfopen(f,buf,"s+")) )
		terror("Can't open string stream2\n");
	if(sfset(f,0,0)&SF_MALLOC)
		terror("SF_MALLOC should not have been set\n");
	if(sfsize(f) != 10)
		terror("String size is wrong8\n");
	sfread(f,buf,5);
	if((n = (int)sfwrite(f,"0123456789",10)) != 5)
		terror("Write wrong amount %d\n", n);
	if(sfsize(f) != 10)
		terror("String size is wrong9\n");

	if(!(f = sfopen(f, tstfile(0),"w") ) )
		terror("Reopening file1\n");
	for(i = 0; i < 10000; ++i)
		if(sfputc(f,'0'+(i%10)) != '0'+(i%10) )
			terror("sfputc failed\n");

	if(!(f = sfopen(f, tstfile(0),"r+") ) )
		terror("Reopening file2\n");
	if(sfsize(f) != 10000)
		terror("Bad size of file1\n");

	sfsetbuf(f,buf,1024);
	for(i = 0; i < 20; ++i)
		if(!sfreserve(f,100,0))
			terror("Reserve failed\n");

	s = buf+1024;
	for(i = 0; i < 20; ++i)
		s[i] = '0' + i%10;
	sfseek(f,(Sfoff_t)(10000-10),0);
	if(sfwrite(f,s,20) != 20)
		terror("Write failed\n");
	if(sfsize(f) != 10010)
		terror("Bad size of file2\n");
	sfseek(f,(Sfoff_t)0,0);
	for(i = 0; i < 10; ++i)
	{	if(!(s = sfreserve(f,1001,0)) )
			terror("Reserve failed2\n");
		if(s[0] != '0'+i)
			terror("Bad data1\n");
	}
	for(n = 0; n < 1001; ++n)
		if(s[n] != ((n+i-1)%10 + '0') )
			terror("Bad data\n");

	/* test to see if a string stream extends ok during writes */
	s = malloc(5);
	f = sfnew((Sfio_t*)0, (void*)s, 5, -1, SF_STRING|SF_READ|SF_WRITE|SF_MALLOC);
	if(!f)
		terror("Can't create string stream\n");
	if(sfwrite(f,"01",2) != 2)
		terror("Bad write to string stream\n");
	if(sfwrite(f,"2345678",7) != 7)
		terror("Bad write to string stream2\n");
	if(sfputc(f,0) != 0)
		terror("sfputc failed\n");
	if(sfseek(f, (Sfoff_t)0, 0) != 0)
		terror("sfseek failed\n");
	if((n = (int)sfread(f, buf, 100)) != 10)
		terror("sfread gets wrong amount of data %d\n", n);
	if(strcmp(buf, "012345678") != 0)
		terror("Get wrong data\n");

	TSTEXIT(0);
}
