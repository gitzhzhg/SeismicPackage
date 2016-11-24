#include	"sftest.h"

#if __STD_C
ssize_t myread(Sfio_t* f, Void_t* buf, size_t n, Sfdisc_t* disc)
#else
ssize_t myread(f, buf, n, disc)
Sfio_t*	f;
Void_t*	buf;
size_t	n;
Sfdisc_t* disc;
#endif
{
	return sfrd(f,buf,n,disc);
}

Sfdisc_t Disc = {myread, NIL(Sfwrite_f), NIL(Sfseek_f), NIL(Sfexcept_f)};

MAIN()
{
	int	fd[2];

	if(pipe(fd) < 0)
		terror("Can't open pipe\n");

	if(sfnew(sfstdin,NIL(Void_t*),(size_t)SF_UNBOUND,fd[0],SF_READ) != sfstdin)
		terror("Can't initialize sfstdin\n");
	sfset(sfstdin,SF_SHARE,1);
	sfdisc(sfstdin,&Disc);

	if(sfnew(sfstdout,NIL(Void_t*),0,fd[1],SF_WRITE) != sfstdout)
		terror("Can't initialize sfstdout\n");
	sfputr(sfstdout,"111\n222\n333\n",-1);
	sfsync(sfstdout);

	if(strcmp(sfgetr(sfstdin,'\n',1),"111") != 0)
		terror("sfgetr failed1\n");
	if(sfstdin->_endb > sfstdin->_next)
		terror("sfgetr reads too much1\n");

	if(strcmp(sfgetr(sfstdin,'\n',1),"222") != 0)
		terror("sfgetr failed2\n");
	if(sfstdin->_endb > sfstdin->_next)
		terror("sfgetr reads too much2\n");

	TSTEXIT(0);
}
