/*****************************************************************************
C++ functions for complex numbers
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 08/06/90
*****************************************************************************/

#include "Complex.h"
#include "Dcomplex.h"

complex sqrt(complex a)
{
	float x,y,w,r,ar=a.r,ai=a.i;
	if (ar==0.0 && ai==0.0) {
		return complex(0.0,0.0);
	} else {
		x = (ar>=0.0?ar:-ar);
		y = (ai>=0.0?ai:-ai);
		if (x>=y) {
			r = y/x;
			w = sqrt(x)*sqrt(0.5*(1.0+sqrt(1.0+r*r)));
		} else {
			r = x/y;
			w = sqrt(y)*sqrt(0.5*(r+sqrt(1.0+r*r)));
		}
		if (ar>=0.0) {
			return complex(w,ai/(2.0*w));
		} else {
			if (ai<0.0) w = -w;
			return complex(ai/(2.0*w),w);
		}
	}
}

complex cos(complex a)
{
	return complex(cos(a.r)*cosh(a.i),-sin(a.r)*sinh(a.i));
}

complex sin(complex a)
{
	return complex(sin(a.r)*cosh(a.i),cos(a.r)*sinh(a.i));
}

complex cosh(complex a)
{
	return complex(cos(a.i)*cosh(a.r),sin(a.i)*sinh(a.r));
}

complex sinh(complex a)
{
	return complex(cos(a.i)*sinh(a.r),sin(a.i)*cosh(a.r));
}

complex exp(complex a)
{
	float r=exp(a.r);
	return complex(r*cos(a.i),r*sin(a.i));
}

complex log(complex a)
{
	float ar=a.r,ai=a.i,h=sqrt(ar*ar+ai*ai);
	return complex(log(h),atan2(ai,ar));
}

complex pow(complex a, int p)
{
	if (p==0) {
		return complex(1.0,0.0);
	} else if (a.r==0.0 && a.i==0.0) {
		return complex(0.0,0.0);
	} else {
		complex res(1.0,0.0);
		complex b=a;
		if (p<0) {
			p = -p;
			b = 1.0/b;
		}
		for(;;) {
			if (p&1) res *= b;
			if ((p>>=1)==0)
				return res;
			else
				b *= b;
		}
	}
}			

complex pow(complex a, double p)
{
	float ar,ai,amp,phs;
	if (p==0.0) return complex(1.0,0.0);
	if (a.r==0.0 && a.i==0.0) return complex(0.0,0.0);
	ar = a.r; ai = a.i;
	amp = exp(0.5*p*log(ar*ar+ai*ai));
	phs = p*atan2(ai,ar);
	return complex(amp*cos(phs),amp*sin(phs));	
}

complex pow(double a, complex p)
{
	float pr,pi,loga,amp,phs;
	if (p.r==0.0 && p.i==0.0) return complex(1.0,0.0);
	if (a==0.0) return complex(0.0,0.0);
	pr = p.r; pi = p.i;
	loga = 0.5*log(a*a);
	amp = exp(pr*loga);
	phs = pi*loga;
	return complex(amp*cos(phs),amp*sin(phs));	
}

complex pow(complex a, complex p)
{
	float ar,ai,pr,pi,loga,arga,amp,phs;
	if (p.r==0.0 && p.i==0.0) return complex(1.0,0.0);
	if (a.r==0.0 && a.i==0.0) return complex(0.0,0.0);
	pr = p.r; pi = p.i; ar = a.r; ai = a.i;
	loga = 0.5*log(ar*ar+ai*ai);
	arga = atan2(ai,ar);
	amp = exp(pr*loga-pi*arga);
	phs = pr*arga+pi*loga;
	return complex(amp*cos(phs),amp*sin(phs));
}

#ifdef DEBUG
// test
#include <stdio.h>
main()
{
	float x=5;
	complex a(3,4),b(1,2),c;
	
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
	c = sqrt(pow(a,2));  printf("sqrt(pow(a,2)) = (%g,%g)\n",c.r,c.i);
	c = sqrt(pow(a,2.));  printf("sqrt(pow(a,2.)) = (%g,%g)\n",c.r,c.i);
	c = sqrt(pow(a,complex(2.0,0.0)));  
	printf("sqrt(pow(a,complex(2.0,0.0))) = (%g,%g)\n",c.r,c.i);
	c = exp(log(sqrt(a*a)));
	printf("exp(log(sqrt(a*a))) = (%g,%g)\n",c.r,c.i);
}
#endif
