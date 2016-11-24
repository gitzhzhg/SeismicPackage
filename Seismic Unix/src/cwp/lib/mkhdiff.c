/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
MKHDIFF - Compute filter approximating the bandlimited HalF-DIFFerentiator.

mkhdiff - Compute filter approximating the bandlimited half-differentiator.

******************************************************************************
Function Prototype:
void mkhdiff (float h, int l, float d[]);

******************************************************************************
Input:
h		sampling interval
l		half-length of half-differentiator (length = 1+2*l is odd)

Output:
d		array[1+2*l] of coefficients for half-differentiator

******************************************************************************
Notes:
The half-differentiator is defined by

				  pi
    d[l+j] = sqrt(1/h)/(2pi) * integral dw sqrt(-iw)*exp(-iwj)
				 -pi

				  pi
           = sqrt(2/h)/(2pi) * integral dw sqrt(w)*(cos(wj)-sin(wj))
				  0 

    for j = -l, -l+1, ... , l.

An alternative definition is that f'(j) = d(j)*d(j)*f(j), where
f'(j) denotes the derivative of a sampled function f(j) and *
denotes a convolution sum.

The half-derivative g(j) of f(j) may be computed by the following sum:

	g(j) = d[0]*f(j+l) + d[1]*f(j+l-1) + ... + d[2*l]*f(j-l)

The integral over frequency is evaluated numerically using Simpson's
method.  Although the Filon method of numerical integration is more
appropriate for this integral, the truncation of d[l+j] for |j| > l
is probably the greatest source of error.  In any case, d[l+j] is 
cosine-tapered to reduce these truncation errors.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

#define SUMAND(w,t) (sqrt((w))*(cos((w)*(t))-sin((w)*(t))))

void mkhdiff (float h, int l, float d[])
/*****************************************************************************
Compute filter approximating the bandlimited half-differentiator.
******************************************************************************
Input:
h		sampling interval
l		half-length of half-differentiator (length = 1+2*l is odd)

Output:
d		array[1+2*l] of coefficients for half-differentiator
******************************************************************************
Notes:
The half-differentiator is defined by

				  pi
    d[l+j] = sqrt(1/h)/(2pi) * integral dw sqrt(-iw)*exp(-iwj)
				 -pi

				  pi
           = sqrt(2/h)/(2pi) * integral dw sqrt(w)*(cos(wj)-sin(wj))
				  0 

    for j = -l, -l+1, ... , l.

An alternative definition is that f'(j) = d(j)*d(j)*f(j), where
f'(j) denotes the derivative of a sampled function f(j) and *
denotes a convolution sum.

The half-derivative g(j) of f(j) may be computed by the following sum:

	g(j) = d[0]*f(j+l) + d[1]*f(j+l-1) + ... + d[2*l]*f(j-l)

The integral over frequency is evaluated numerically using Simpson's
method.  Although the Filon method of numerical integration is more
appropriate for this integral, the truncation of d[l+j] for |j| > l
is probably the greatest source of error.  In any case, d[l+j] is 
cosine-tapered to reduce these truncation errors.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	int nw,j,iw;
	float dw,t,sum,w;

	/* compute number and width of frequency intervals for integration */
	nw = 8*l;
	dw = PI/nw;

	/* integrate by simpson's rule for j = -l to l */
	for (j= -l; j<=l; j++) {
		t = j;
		sum = SUMAND(0.0,t)-SUMAND(PI,t);
		for (iw=1,w=dw; iw<=nw-1; iw+=2,w+=2.0*dw)
			sum += 4.0*SUMAND(w,t)+2.0*SUMAND(w+dw,t);
		d[l+j] = sqrt(2.0/h)/(2.0*PI)*PI/(3.0*nw)*sum;
	}

	/* cosine taper to reduce truncation artifacts */
	for (j= -l; j<=l; j++)
		d[l+j] *= (0.54+0.46*cos(PI*j/l));
}
