// test complex arithmetic via C++

#include "cwp.h"

int
main()
{
	int n=8,i;
	float x=5;
	complex a(3,4),b(1,2),c,*v;
	
	// test new C++ functions
	c = a+b-b; printf("a+b-b = (%g,%g)\n",c.r,c.i);
	c = a-b+b; printf("a-b+b = (%g,%g)\n",c.r,c.i);
	c = a*b/b; printf("a*b/b = (%g,%g)\n",c.r,c.i);
	c = a/b*b; printf("a/b*b = (%g,%g)\n",c.r,c.i);
	c = x+a-x; printf("x+a-x = (%g,%g)\n",c.r,c.i);
	c = x*a/x; printf("x*a/x = (%g,%g)\n",c.r,c.i);
	c = a*x/x; printf("a*x/x = (%g,%g)\n",c.r,c.i);
	c = a/x*x; printf("a/x*x = (%g,%g)\n",c.r,c.i);
	c = x+a-a; printf("x+a-a = (%g,%g)\n",c.r,c.i);
	c = a+x-a; printf("a+x-a = (%g,%g)\n",c.r,c.i);
	c = x-a+a; printf("x-a+a = (%g,%g)\n",c.r,c.i);
	c = x/a*a; printf("x/a*a = (%g,%g)\n",c.r,c.i);
	c = sqrt(pow(a,2)); printf("sqrt(pow(a,2)) = (%g,%g)\n",c.r,c.i);
	c = sqrt(pow(a,2.)); printf("sqrt(pow(a,2.)) = (%g,%g)\n",c.r,c.i);
	c = sqrt(pow(a,complex(2.0,0.0)));  
	printf("sqrt(pow(a,complex(2.0,0.0))) = (%g,%g)\n",c.r,c.i);
	c = exp(log(sqrt(a*a)));
	printf("exp(log(sqrt(a*a))) = (%g,%g)\n",c.r,c.i);

	// test old C functions (except cadd, cmul, etc., which don't work!)
	//c = cadd(a,b); printf("cadd(a,b) = (%g,%g)\n",c.r,c.i);
	v = alloc1complex(n);
	for (i=0; i<n; ++i)
		v[i] = complex(0.0,0.0);
	v[1] = complex(1.0,0.0);
	for (i=0; i<n; ++i)
		printf("v[%d] = (%g,%g)\n",i,v[i].r,v[i].i);
	pfacc(1,n,v);
	for (i=0; i<n; ++i)
		printf("v[%d] = (%g,%g)\n",i,v[i].r,v[i].i);
	pfacc(-1,n,v);
	for (i=0; i<n; ++i) 
		v[i] /= n;
	for (i=0; i<n; ++i)
		printf("v[%d] = (%g,%g)\n",i,v[i].r,v[i].i);
	free1complex(v);
	return EXIT_SUCCESS;
}
