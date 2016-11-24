/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
HILBERT - Compute Hilbert transform y of x

hilbert		compute the Hilbert transform

******************************************************************************
Function Prototype:
void hilbert (int n, float x[], float y[]);

******************************************************************************
Input:
n		length of x and y
x		array[n] to be Hilbert transformed

Output:
y		array[n] containing Hilbert transform of x

******************************************************************************
Notes:
The Hilbert transform is computed by convolving x with a
windowed (approximate) version of the ideal Hilbert transformer.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

#define LHHALF 30	/* half-length of Hilbert transform filter*/
#define LH 2*LHHALF+1	/* filter length must be odd */

void hilbert (int n, float x[], float y[])
/*****************************************************************************
Compute Hilbert transform y of x
******************************************************************************
Input:
n		length of x and y
x		array[n] to be Hilbert transformed

Output:
y		array[n] containing Hilbert transform of x
******************************************************************************
Notes:
The Hilbert transform is computed by convolving x with a
windowed (approximate) version of the ideal Hilbert transformer.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	static int madeh=0;
	static float h[LH];
	int i;
	float taper;

	/* if not made, make Hilbert transform filter; use Hamming window */
	if (!madeh) {
		h[LHHALF] = 0.0;
		for (i=1; i<=LHHALF; i++) {
			taper = 0.54+0.46*cos(PI*(float)i/(float)(LHHALF));
			h[LHHALF+i] = taper*(-(float)(i%2)*2.0/
				(PI*(float)(i)));
			h[LHHALF-i] = -h[LHHALF+i];
		}
		madeh = 1;
	}

	/* convolve Hilbert transform with input array */
	convolve_cwp(LH,-LHHALF,h,n,0,x,n,0,y);
}
