/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/************************************************************************
COMPLEXFD  - Subroutines to perform operations on double complex numbers.
		This set of functions complement the one in complexd.c
		of the CWP library

dcipow		raise a double complex number to an integer power
dcrpow		raise a double complex number to a real power
rdcpow		raise a real number to a double complex power
dcdcpow		raise a double complex number to a double complex power
dccos		compute the double complex cosine of a double complex angle
dcsin		compute the double complex sine of a double complex angle
dccosh		compute the double complex hyperbolic cosine of a double complex angle
dcsinh		compute the double complex hyperbolic sine of a double complex angle
dcexp1		compute the double complex exponential of a double complex number
dclog		compute the double complex logarithm of a double complex number
************************************************************************
Function Prototypes:
dcomplex dcipow(dcomplex a, int p);
dcomplex dcrpow(dcomplex a, float p);
dcomplex rdcpow(float a, dcomplex p);
dcomplex dcdcpow (dcomplex a, dcomplex p)
dcomplex dccos(dcomplex a);
dcomplex dcsin(dcomplex a);
dcomplex dccosh(dcomplex a);
dcomplex dcsinh(dcomplex a);
dcomplex dcexp1(dcomplex a);
dcomplex dclog(dcomplex a);
************************************************************************
Credits:
	Dave Hale, original version in C++
	Gabriel Alvarez, translation to C
***********************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

dcomplex dcipow(dcomplex a, int p)
{
	dcomplex res;
	dcomplex b;

	if (p==0) {
		return dcmplx(1.0,0.0);
	} else if (a.r==0.0 && a.i==0.0) {
		return dcmplx(0.0,0.0);
	} else {
		res=dcmplx(1.0,0.0);
		b=a;
		if (p<0) {
			p = -p;
			b = dcinv(b);
		}
		for(;;) {
			if (p&1) res = dcmul(res,b);
			if ((p>>=1)==0)
				return res;
			else
				b = dcmul(b,b);
		}
	}
}			

dcomplex dcrpow(dcomplex a, float p)
{
	float ar,ai,amp,phs;

	if (p==0.0) return dcmplx(1.0,0.0);
	if (a.r==0.0 && a.i==0.0) return dcmplx(0.0,0.0);

	ar = a.r; ai = a.i;
	amp = exp(0.5*p*log(ar*ar+ai*ai));
	phs = p*atan2(ai,ar);

	return dcmplx(amp*cos(phs),amp*sin(phs));	
}

dcomplex rdcpow(float a, dcomplex p)
{
	float pr,pi,loga,amp,phs;

	if (p.r==0.0 && p.i==0.0) return dcmplx(1.0,0.0);
	if (a==0.0) return dcmplx(0.0,0.0);
	pr = p.r; pi = p.i;
	loga = 0.5*log(a*a);
	amp = exp(pr*loga);
	phs = pi*loga;

	return dcmplx(amp*cos(phs),amp*sin(phs));	
}

dcomplex dcdcpow (dcomplex a, dcomplex p)
{
	float ar,ai,pr,pi,loga,arga,amp,phs;

	if (p.r==0.0 && p.i==0.0) return dcmplx(1.0,0.0);
	if (a.r==0.0 && a.i==0.0) return dcmplx(0.0,0.0);

	pr = p.r; pi = p.i; ar = a.r; ai = a.i;
	loga = 0.5*log(ar*ar+ai*ai);
	arga = atan2(ai,ar);
	amp = exp(pr*loga-pi*arga);
	phs = pr*arga+pi*loga;

	return dcmplx(amp*cos(phs),amp*sin(phs));
}

dcomplex dccos(dcomplex a)
{
	return dcmplx(cos(a.r)*cosh(a.i),-sin(a.r)*sinh(a.i));
}

dcomplex dcsin(dcomplex a)
{
	return dcmplx(sin(a.r)*cosh(a.i),cos(a.r)*sinh(a.i));
}

dcomplex dccosh(dcomplex a)
{
	return dcmplx(cos(a.i)*cosh(a.r),sin(a.i)*sinh(a.r));
}

dcomplex dcsinh(dcomplex a)
{
	return dcmplx(cos(a.i)*sinh(a.r),sin(a.i)*cosh(a.r));
}

dcomplex dcexp1(dcomplex a)
{
	float r=exp(a.r);
	return dcmplx(r*cos(a.i),r*sin(a.i));
}

dcomplex dclog(dcomplex a)
{
	float ar=a.r,ai=a.i,h=sqrt(ar*ar+ai*ai);
	return dcmplx(log(h),atan2(ai,ar));
}

