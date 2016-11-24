#include	"sftest.h"

MAIN()
{
	Sfio_t	*f;
	int	n;
	char	*s, *os, *endos;
	char	buf[8192];

	os = "123\n456\n789\n";
	if(!(f = sfopen((Sfio_t*)0,os,"s")))
		terror("Opening string\n");

	endos = os + strlen(os);
	while((s = sfgetr(f,'\n',0)) )
	{	
		if(s != os)
			terror("Did not get string\n");
		os += sfvalue(f);
	}

	if(os != endos)
		terror("Did not match everything\n");

	if(sfgetc(f) >= 0 || !sfeof(f))
		terror("Stream should have exhausted\n");

	if(!(f = sfopen(f,(char*)0,"s+")))
		terror("Opening string for r/w\n");
	for(n = 0; n < 26; ++n)
		if((sfputc(f,'a'+n)) != 'a'+n)
			terror("Output\n");
	if(sfgetc(f) >= 0)
		terror("Read a non-existent byte\n");
	sfseek(f,(Sfoff_t)0,0);
	if(!(s = sfreserve(f,26,0)) )
		terror("Didnot get the right amount of data\n");
	for(n = 0; n < 26; ++n)
		if((sfputc(f,'a'+n)) != 'a'+n)
			terror("Output2\n");
	sfseek(f,(Sfoff_t)2,0);
	if(!(s = sfreserve(f,50,0)) )
		terror("Didnot get the right amount of data2\n");

	if(!(f = sfopen(f,(char*)0,"s+")))
		terror("Opening string for r/w\n");
	sfset(f,SF_READ,0);
	sfseek(f,(Sfoff_t)0,0);
	if(!(s = sfreserve(f,SF_UNBOUND,1)) || (n = sfvalue(f)) <= 0 ||
	   sfwrite(f,s,0) != 0)
		terror("Buffer size should be positive\n");
	sfseek(f,(Sfoff_t)(n+8192),0);
	sfseek(f,(Sfoff_t)0,0);
	if(!(s = sfreserve(f,SF_UNBOUND,1)) || sfvalue(f) != (n+8192) ||
	   sfwrite(f,s,0) != 0)
		terror("Bad buffer size\n");

	if(!(f = sfopen(f,(char*)0,"s+")))
		terror("Opening string for r/w\n");
	if(sfwrite(f,buf,sizeof(buf)) != sizeof(buf))
		terror("Can't write large buffer\n");

	if(!(f = sfopen((Sfio_t*)0,(char*)0,"s+")))
		terror("Opening string for r/w\n");
	sfset(f,SF_READ,0);
	for(n = 0; n < 16*1024; ++n)
	{
         	if((n%1024) == 0)
		{	Sfoff_t a = sfseek(f,(Sfoff_t)1024,1);
			sfputc(f,'a');
			sfseek(f,(Sfoff_t)(-1025),1);
		}
                sfputc(f,'a');
	}
	sfseek(f,(Sfoff_t)0,0);
	if(!(s = sfreserve(f,SF_UNBOUND,1)) || sfvalue(f) != n+1024 ||
	   sfwrite(f,s,0) != 0)
		terror("Wrong buffer size\n");
	while(n-- > 0)
		if(*s++ != 'a')
			terror("Wrong data\n");

	if(!(f = sfopen((Sfio_t*)0,(char*)0,"s+")))
		terror("Opening r/w string\n");
	for(n = 0; n < 10; n++)
		sfputc(f,'a'+n);
	sfputc(f,'\n');
	sfseek(f,(Sfoff_t)0,0);
	for(n = 0; n <= 11 ; ++n)
		if(sfgetc(f) != 'a'+n)
			break;
	if(n != 10)
		terror("Get too many\n");
	if(sfgetc(f) >= 0)
		terror("Reading non-existent data\n");
	if(!sfeof(f))
		terror("Didn't get eof\n");

	TSTEXIT(0);
}
