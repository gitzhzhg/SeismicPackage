#include	"sftest.h"
#ifdef SF_APPEND
#undef SF_APPEND
#endif

#if _hdr_stat
#include	<stat.h>
#endif
#if _sys_stat
#include	<sys/stat.h>
#endif


Sfdisc_t Disc;

MAIN()
{
	int	n, fd;
	Sfio_t	*f;
	char	*s, buf[1024];
	int	fdv[100];

	buf[0] = 0;
	sfsetbuf(sfstdout,buf,sizeof(buf));
	if(!sfstdout->pool)
		terror("No pool\n");
	sfdisc(sfstdout,&Disc);
	sfset(sfstdout,SF_SHARE,0);
	sfputr(sfstdout,"123456789",0);
	if(strcmp(buf,"123456789") != 0)
		terror("Setting own buffer for stdout\n");
	if(sfpurge(sfstdout) < 0)
		terror("Purging sfstdout\n");

	if((fd = creat(tstfile(0),0666)) < 0)
		terror("Creating file\n");

	if(write(fd,buf,sizeof(buf)) != sizeof(buf))
		terror("Writing to file\n");
	if(lseek(fd, (off_t)0, 0) < 0)
		terror("Seeking back to origin\n");

	if(!(f = sfnew((Sfio_t*)0,buf,sizeof(buf),fd,SF_WRITE)))
		terror("Making stream\n");

	if(!(s = sfreserve(f,SF_UNBOUND,1)) || s != buf)
		terror("sfreserve1 returns the wrong pointer\n");
	sfwrite(f,s,0);

#define NEXTFD	12
	if((fd+NEXTFD) < (sizeof(fdv)/sizeof(fdv[0])) )
	{	struct stat	st;
		int		i;
		for(i = 0; i < fd+NEXTFD; ++i)
			fdv[i] = fstat(i,&st);
	}
	if((n = sfsetfd(f,fd+NEXTFD)) != fd+NEXTFD)
		terror("Try to set file descriptor to %d but get %d\n",fd+NEXTFD,n);
	if((fd+NEXTFD) < (sizeof(fdv)/sizeof(fdv[0])) )
	{	struct stat	st;
		int		i;
		for(i = 0; i < fd+NEXTFD; ++i)
			if(i != fd && fdv[i] != fstat(i,&st))
				terror("Fd %d changes status after sfsetfd %d->%d\n",
					i, fd, fd+NEXTFD);
	}

	if(!(s = sfreserve(f,SF_UNBOUND,1)) || s != buf)
		terror("sfreserve2 returns the wrong pointer\n");
	sfwrite(f,s,0);

	if(sfsetbuf(f,NIL(Void_t*),(size_t)SF_UNBOUND) != buf)
		terror("sfsetbuf didnot returns last buffer\n");

	sfsetbuf(f,buf,sizeof(buf));

	if(sfreserve(f,SF_UNBOUND,1) != buf || sfvalue(f) != sizeof(buf) )
		terror("sfreserve3 returns the wrong value\n");
	sfwrite(f,s,0);

	TSTEXIT(0);
}
