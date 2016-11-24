/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
COMPLEXD - Functions to manipulate double-precision complex numbers

dcadd	add two dcomplex numbers
dcsub	subtract two dcomplex numbers
dcmul	multiply two dcomplex numbers
dcdiv	divide two dcomplex numbers
dcmplx	make a dcomplex number from two real numbers
dconjg	dcomplex conjugate of a dcomplex number 
dcneg	negate a dcomplex number
dcinv	invert a dcomplex number
dcsqrt	dcomplex square root of a dcomplex number
dcexp	dcomplex exponential of a dcomplex number
dcrmul	multiply a dcomplex number by a real number 
drcabs	real magnitude of a dcomplex number

******************************************************************************
Structure:
typedef struct _dcomplexStruct {  dcomplex number
	double r,i;
} dcomplex;

******************************************************************************
Function Prototypes:
dcomplex dcadd (dcomplex a, dcomplex b);
dcomplex dcsub (dcomplex a, dcomplex b);
dcomplex dcmul (dcomplex a, dcomplex b);
dcomplex dcdiv (dcomplex a, dcomplex b);
double drcabs (dcomplex z);
dcomplex dcmplx (double re, double im);
dcomplex dconjg (dcomplex z);
dcomplex dcneg (dcomplex z);
dcomplex dcinv (dcomplex z);
dcomplex dcsqrt (dcomplex z);
dcomplex dcexp (dcomplex z);
dcomplex dcrmul (dcomplex a, double x);

******************************************************************************
Notes:
The function "drcabs" was originally called "fcabs". This produced
a collision on some systems so a new name was chosen.

******************************************************************************
Reference:
Adapted from Press et al, 1988, Numerical Recipes in C (Appendix E).

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
Modified:  Dave Hale, Colorado School of Mines, 04/26/90
	Added function dcinv().
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

dcomplex dcadd(dcomplex a, dcomplex b)
{
	dcomplex c;
	c.r = a.r+b.r;
	c.i = a.i+b.i;
	return c;
}

dcomplex dcsub(dcomplex a, dcomplex b)
{
	dcomplex c;
	c.r = a.r-b.r;
	c.i = a.i-b.i;
	return c;
}

dcomplex dcmul(dcomplex a, dcomplex b)
{
	dcomplex c;
	c.r = a.r*b.r-a.i*b.i;
	c.i = a.i*b.r+a.r*b.i;
	return c;
}

dcomplex dcdiv(dcomplex a, dcomplex b)
{
	dcomplex c;
	double r,den;
	if (fabs(b.r)>=fabs(b.i)) {
		r = b.i/b.r;
		den = b.r+r*b.i;
		c.r = (a.r+r*a.i)/den;
		c.i = (a.i-r*a.r)/den;
	} else {
		r = b.r/b.i;
		den = b.i+r*b.r;
		c.r = (a.r*r+a.i)/den;
		c.i = (a.i*r-a.r)/den;
	}
	return c;
}

dcomplex dcmplx(double re, double im)
{
	dcomplex c;
	c.r = re;
	c.i = im;
	return c;
}

dcomplex dconjg(dcomplex z)
{
	dcomplex c;
	c.r = z.r;
	c.i = -z.i;
	return c;
}

dcomplex dcneg(dcomplex z)
{
	dcomplex c;
	c.r = -z.r;
	c.i = -z.i;
	return c;
}

dcomplex dcinv(dcomplex z)
{
	dcomplex c;
	double s;
	s = 1.0/(z.r*z.r+z.i*z.i);
	c.r = z.r*s;
	c.i = -z.i*s;
	return c;
}

dcomplex dcsqrt(dcomplex z)
{
	dcomplex c;
	double x,y,w,r;
	if (z.r==0.0 && z.i==0.0) {
		c.r = c.i = 0.0;
		return c;
	} else {
		x = fabs(z.r);
		y = fabs(z.i);
		if (x>=y) {
			r = y/x;
			w = sqrt(x)*sqrt(0.5*(1.0+sqrt(1.0+r*r)));
		} else {
			r = x/y;
			w = sqrt(y)*sqrt(0.5*(r+sqrt(1.0+r*r)));
		}
		if (z.r>=0.0) {
			c.r = w;
			c.i = z.i/(2.0*w);
		} else {
			c.i = (z.i>=0.0) ? w : -w;
			c.r = z.i/(2.0*c.i);
		}
		return c;
	}
}

dcomplex dcexp(dcomplex z)
{
	double a;
	dcomplex c;
	a = exp(z.r);
	c.r = a*cos(z.i);
	c.i = a*sin(z.i);
	return c;
}

dcomplex dcrmul(dcomplex a, double x)
{
	dcomplex c;
	c.r = x*a.r;
	c.i = x*a.i;
	return c;
}

double drcabs(dcomplex z)
{
	double x,y,ans,temp;
	x = fabs(z.r);
	y = fabs(z.i);
	if (x==0.0)
		ans = y;
	else if (y==0.0)
		ans = x;
	else if (x>y) {
		temp = y/x;
		ans = x*sqrt(1.0+temp*temp);
	} else {
		temp =x/y;
		ans = y*sqrt(1.0+temp*temp);
	}
	return ans;
}

