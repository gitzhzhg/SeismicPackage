/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
MKDIFF - Make an n-th order DIFFerentiator via Taylor's series method.

mkdiff		make discrete Taylor series approximation to n'th derivative.

******************************************************************************
Function Prototype:
void mkdiff (int n, float a, float h, int l, int m, float d[]);

******************************************************************************
Input:
n		order of desired derivative (n>=0 && n<=m-l is required)
a		fractional distance from integer sampling index (see notes)
h		sampling interval
l		sampling index of first coefficient (see notes below)
m		sampling index of last coefficient (see notes below)

Output:
d		array[m-l+1] of coefficients for n'th order differentiator

******************************************************************************
Notes:
The abscissae x of a sampled function f(x) can always be expressed as
x = (j+a)*h, where j is an integer, a is a fraction, and h is the
sampling interval.  To approximate the n'th order derivative fn(x)
of the sampled function f(x) at x = (j+a)*h, use the m-l+1 coefficients
in the output array d[] as follows:

	fn(x) = d[0]*f(j-l) + d[1]*f(j-l-1) +...+ d[m-l]*f(j-m)

i.e., convolve the coefficients in d with the samples in f.

m-l+1 (the number of coefficients) must not be greater than the
NCMAX parameter specified below.

For best approximations,
when n is even, use a = 0.0, l = -m
when n is odd, use a = 0.5, l = -m-1

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

#define NCMAX 100

void mkdiff (int n, float a, float h, int l, int m, float d[])
/*****************************************************************************
Make discrete Taylor series approximation to n'th derivative.
******************************************************************************
Input:
n		order of desired derivative (n>=0 && n<=m-l is required)
a		fractional distance from integer sampling index (see notes)
h		sampling interval
l		sampling index of first coefficient (see notes below)
m		sampling index of last coefficient (see notes below)

Output:
d		array[m-l+1] of coefficients for n'th order differentiator
******************************************************************************
Notes:
The abscissae x of a sampled function f(x) can always be expressed as
x = (j+a)*h, where j is an integer, a is a fraction, and h is the
sampling interval.  To approximate the n'th order derivative fn(x)
of the sampled function f(x) at x = (j+a)*h, use the m-l+1 coefficients
in the output array d[] as follows:

	fn(x) = d[0]*f(j-l) + d[1]*f(j-l-1) +...+ d[m-l]*f(j-m)

i.e., convolve the coefficients in d with the samples in f.

m-l+1 (the number of coefficients) must not be greater than the
NCMAX parameter specified below.

For best approximations,
when n is even, use a = 0.0, l = -m
when n is odd, use a = 0.5, l = -m-1
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	int nc,i;
	double fn,hn,v[NCMAX],b[NCMAX];

	/* determine number of coefficients */
	nc = m-l+1;

	/* build Vandermonde system of equations */
	for (i=0; i<nc; i++) {
		v[i] = -(l+i)-a;
		b[i] = 0.0;
	}
	for (i=1,fn=hn=1.0; i<=n; i++) {
		fn *= i;
		hn *= h;
	}
	b[n] = fn/hn;

	/* compute coefficients in double precision */
	vanded(nc,v,b,b);

	/* copy coefficients to output array */
	for (i=0; i<nc; i++)
		d[i] = b[i];
}
