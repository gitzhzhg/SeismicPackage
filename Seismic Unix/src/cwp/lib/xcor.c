/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
XCOR - Compute z = x cross-correlated with y

xcor	compute z= x cross-correlated with y

******************************************************************************
Function Prototype:
void xcor (int lx, int ifx, float *x, int ly, int ify, float *y ,
	int lz, int ifz, float *z);

******************************************************************************
Input:
lx		length of x array
ifx		sample index of first x
x		array[lx] to be cross-correlated with y
ly		length of y array
ify		sample index of first y
y		array[ly] with which x is to be cross-correlated
lz		length of z array
ifz		sample index of first z

Output:
z		array[lz] containing x cross-correlated with y

******************************************************************************
Notes:
See notes for convolution function convolve_cwp().

The operation "x cross correlated with y"  is defined to be:

           ifx+lx-1
    z[i] =   sum    x[j]*y[i+j]  ;  i = ifz,...,ifz+lz-1
            j=ifx

This function performs cross-correlation by
(1) reversing the samples in the x array while copying
    them to a temporary array, and
(2) calling function convolve_cwp() with ifx set to 1-ifx-lx.
Assuming that the overhead of reversing the samples in x is negligible,
this method enables cross-correlation to be performed as efficiently as
convolution, while reducing the amount of code that must be optimized
and maintained.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 11/23/91
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

void xcor (int lx, int ifx, float *x,
	   int ly, int ify, float *y, 
	   int lz, int ifz, float *z)
/*****************************************************************************
Compute z = x cross-correlated with y; i.e.,

           ifx+lx-1
    z[i] =   sum    x[j]*y[i+j]  ;  i = ifz,...,ifz+lz-1
            j=ifx
******************************************************************************
Input:
lx		length of x array
ifx		sample index of first x
x		array[lx] to be cross-correlated with y
ly		length of y array
ify		sample index of first y
y		array[ly] with which x is to be cross-correlated
lz		length of z array
ifz		sample index of first z

Output:
z		array[lz] containing x cross-correlated with y
******************************************************************************
Notes:
See notes for convolution function convolve_cwp().
This function performs cross-correlation by
(1) reversing the samples in the x array while copying
    them to a temporary array, and
(2) calling function convolve_cwp() with ifx set to 1-ifx-lx.
Assuming that the overhead of reversing the samples in x is negligible,
this method enables cross-correlation to be performed as efficiently as
convolution, while reducing the amount of code that must be optimized
and maintained.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 11/23/91
*****************************************************************************/
{
	int i,j;
	float *xr;
	
	xr = alloc1float(lx);
	for (i=0,j=lx-1; i<lx; ++i,--j)
		xr[i] = x[j];
	convolve_cwp(lx,1-ifx-lx,xr,ly,ify,y,lz,ifz,z);
	free1float(xr);
}
