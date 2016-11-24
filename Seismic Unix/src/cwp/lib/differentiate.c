/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "cwp.h"

/*********************** self documentation **********************/
/**********************************************************************

DIFFERENTIATE - simple DIFFERENTIATOR codes

differentiate - 1D two point centered difference based derivative

************************************************************************
Function Prototype:
void differentiate(int n, float h, float *f, float *fprime);
void ddifferentiate(int n, double h, double *f, double *fprime);

***********************************************************************
differentiate:
Input:
n		number of samples
h		sample rate
f		array[n] of input values
Output:
fprime		array[n], the derivative of f

************************************************************************
fprime:
Notes:
This is a simple 2 point centered-difference differentiator.
The derivatives at the endpoints are computed via 2 point leading and
lagging differences. 

************************************************************************
Author: John Stockwell, CWP, 1994
************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

void differentiate(int n, float h, float *f, float *fprime)
/************************************************************************
differentiate - compute the 1st derivative of a function f[]
************************************************************************
Input:
n		number of samples
h		sample rate
f		array[n] of input values

Output:
fprime		array[n], the derivative of f
***********************************************************************
ddifferentiate:
Input:
n		number of samples
h		sample rate
f		array[n] of input values
Output:
fprime		array[n], the derivative of f

************************************************************************
Notes:
This is a simple 2 point centered-difference differentiator. 
The derivatives at the endpoints are computed via 2 point leading and
lagging differences. 
************************************************************************
Author: John Stockwell, CWP, 1994
************************************************************************/
{
	int i;	
	float *temp;
	float h2=2*h;

	/* allocate space in temporary vector */
	temp = alloc1float(n);

	/* do first as a leading difference */
	fprime[0] = (f[1] - f[0])/h;

	/* do the middle values as a centered difference */
	for (i=1; i<n-1; ++i) fprime[i] = (f[i+1] - f[i-1])/h2;

	/* do last value as a lagging difference */
	fprime[n-1] = (f[n-1] - f[n-2])/h;

}


void ddifferentiate(int n, double h, double *f, double *fprime)
/************************************************************************
ddifferentiate - compute the 1st derivative of a function f[] (double precision)
************************************************************************
Input:
n		number of samples
h		sample rate
f		array[n] of input values

Output:
fprime		array[n], the derivative of f
************************************************************************
Notes: (Double precision form of differentiate)
This is a simple 2 point centered-difference differentiator.
The derivatives at the endpoints are computed via 2 point leading and
lagging differences. 
************************************************************************
Author: John Stockwell, CWP, 1994
************************************************************************/
{
	int i;	
	double *temp;
	double h2=2*h;

	/* allocate space in temporary vector */
	temp = alloc1double(n);

	/* do first as a leading difference */
	fprime[0] = (f[1] - f[0])/h;

	/* do the middle values as a centered difference */
	for (i=1; i<n-1; ++i) fprime[i] = (f[i+1] - f[i-1])/h2;

	/* do last value as a lagging difference */
	fprime[n-1] = (f[n-1] - f[n-2])/h;

}
