/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/************************************************************************
COMPLEXF  - Subroutines to perform operations on complex numbers.
		This set of functions complement the one in complex.c
		of the CWP library

cipow		raise a complex number to an integer power
crpow		raise a complex number to a real power
rcpow		raise a real number to a complex power
ccpow		raise a complex number to a complex power
cwp_ccos		compute the complex cosine of a complex angle
cwp_csin		compute the complex sine of a complex angle
cwp_ccosh		compute the complex hyperbolic cosine of a complex angle
cwp_csinh		compute the complex hyperbolic sine of a complex angle
cwp_cexp1		compute the complex exponential of a complex number
cwp_clog		compute the complex logarithm of a complex number
************************************************************************
Function Prototypes:
complex cipow(complex a, int p);
complex crpow(complex a, float p);
complex rcpow(float a, complex p);
complex ccpow (complex a, complex p)
complex cwp_ccos(complex a);
complex cwp_csin(complex a);
complex cwp_ccosh(complex a);
complex cwp_csinh(complex a);
complex cwp_cexp1(complex a);
complex cwp_clog(complex a);
************************************************************************
Credits:
	Dave Hale, original version in C++
	Gabriel Alvarez, translation to C
***********************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

complex cipow(complex a, int p)
{
	complex res;
	complex b;

	if (p==0) {
		return cmplx(1.0,0.0);
	} else if (a.r==0.0 && a.i==0.0) {
		return cmplx(0.0,0.0);
	} else {
		res=cmplx(1.0,0.0);
		b=a;
		if (p<0) {
			p = -p;
			b = cinv(b);
		}
		for(;;) {
			if (p&1) res = cmul(res,b);
			if ((p>>=1)==0)
				return res;
			else
				b = cmul(b,b);
		}
	}
}			

complex crpow(complex a, float p)
{
	float ar,ai,amp,phs;

	if (p==0.0) return cmplx(1.0,0.0);
	if (a.r==0.0 && a.i==0.0) return cmplx(0.0,0.0);

	ar = a.r; ai = a.i;
	amp = exp(0.5*p*log(ar*ar+ai*ai));
	phs = p*atan2(ai,ar);

	return cmplx(amp*cos(phs),amp*sin(phs));	
}

complex rcpow(float a, complex p)
{
	float pr,pi,loga,amp,phs;

	if (p.r==0.0 && p.i==0.0) return cmplx(1.0,0.0);
	if (a==0.0) return cmplx(0.0,0.0);
	pr = p.r; pi = p.i;
	loga = 0.5*log(a*a);
	amp = exp(pr*loga);
	phs = pi*loga;

	return cmplx(amp*cos(phs),amp*sin(phs));	
}

complex ccpow (complex a, complex p)
{
	float ar,ai,pr,pi,loga,arga,amp,phs;

	if (p.r==0.0 && p.i==0.0) return cmplx(1.0,0.0);
	if (a.r==0.0 && a.i==0.0) return cmplx(0.0,0.0);

	pr = p.r; pi = p.i; ar = a.r; ai = a.i;
	loga = 0.5*log(ar*ar+ai*ai);
	arga = atan2(ai,ar);
	amp = exp(pr*loga-pi*arga);
	phs = pr*arga+pi*loga;

	return cmplx(amp*cos(phs),amp*sin(phs));
}

complex cwp_ccos(complex a)
{
	return cmplx(cos(a.r)*cosh(a.i),-sin(a.r)*sinh(a.i));
}

complex cwp_csin(complex a)
{
	return cmplx(sin(a.r)*cosh(a.i),cos(a.r)*sinh(a.i));
}

complex cwp_ccosh(complex a)
{
	return cmplx(cos(a.i)*cosh(a.r),sin(a.i)*sinh(a.r));
}

complex cwp_csinh(complex a)
{
	return cmplx(cos(a.i)*sinh(a.r),sin(a.i)*cosh(a.r));
}

complex cwp_cexp1(complex a)
{
	float r=exp(a.r);
	return cmplx(r*cos(a.i),r*sin(a.i));
}

complex cwp_clog(complex a)
{
	float ar=a.r,ai=a.i,h=sqrt(ar*ar+ai*ai);
	return cmplx(log(h),atan2(ai,ar));
}

