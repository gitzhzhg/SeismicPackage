#include	"sftest.h"

MAIN()
{
	Sfio_t	*f1, *f2;
	char*	s;
	Sfoff_t	p;
	char	buf[1024];
	int	r, w;

	if(!(f1 = sfopen(NIL(Sfio_t*), tstfile(0), "w")) )
		terror("Can't open f1\n");
	if(!(f1 = sfopen(f1, tstfile(0), "a+")) )
		terror("Can't open f1\n");

	if(!(f2 = sfopen(NIL(Sfio_t*), tstfile(0), "a+")) )
		terror("Can't open f2\n");

	if(sfwrite(f1,"012345678\n",10) != 10 || sfsync(f1) < 0)
		terror("Writing to f1\n");
	if((p = sftell(f1)) != 10)
		terror("Bad sftell1 %ld\n",p);

	if(sfwrite(f2,"abcdefghi\n",10) != 10 || sfsync(f2) < 0)
		terror("Writing to f2\n");
	if((p = sftell(f2)) != 20)
		terror("Bad sftell2\n");

	if((p = sfseek(f1,(Sfoff_t)0,0)) != 0)
		terror("Bad seek\n");
	if(!(s = sfgetr(f1,'\n',1)) )
		terror("Bad getr1\n");
	if(strcmp(s,"012345678") != 0)
		terror("Bad input1\n");

	if((p = sftell(f1)) != 10)
		terror("Bad sftell3\n");

	if(sfwrite(f1,"012345678\n",10) != 10 || sfsync(f1) < 0)
		terror("Writing to f1\n");
	if((p = sftell(f1)) != 30)
		terror("Bad sftell4\n");

	if((p = sfseek(f2,(Sfoff_t)10,0)) != 10)
		terror("Bad seek\n");
	if(!(s = sfgetr(f2,'\n',1)) )
		terror("Bad getr2\n");
	if(strcmp(s,"abcdefghi") != 0)
		terror("Bad input2\n");

	if(!(s = sfgetr(f2,'\n',1)) )
		terror("Bad getr3\n");
	if(strcmp(s,"012345678") != 0)
		terror("Bad input3\n");

	if(!(f1 = sfopen(f1, tstfile(0), "w")) )
		terror("Can't open file to write\n");
	for(r = 0; r < 1024; ++r)
		buf[r] = 'a';
	if((w = sfwrite(f1,buf,1024)) != 1024)
		terror("writing w=%d\n", w);
	if(!(f1 = sfopen(f1, tstfile(0), "a")) )
		terror("Can't open file to append\n");
	sfseek(f1,(Sfoff_t)0,0);
	if((w = sfwrite(f1,buf,64)) != 64)
		terror("writing w=%d\n", w);
	if((r = (int)sftell(f1)) != (1024+64) )
		terror("seek position wrong s=%d\n", r);

	TSTEXIT(0);
}
