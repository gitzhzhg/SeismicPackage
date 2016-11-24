/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
INTCUB - evaluate y(x), y'(x), y''(x), ... via piecewise cubic interpolation

intcub		evaluate y(x), y'(x), y''(x), ... via piecewise spline
			interpolation

******************************************************************************
Function Prototype:
void intcub (int ideriv, int nin, float xin[], float ydin[][4],

******************************************************************************
Input:
ideriv		=0 if y(x) desired; =1 if y'(x) desired, ...
nin		length of xin and ydin arrays
xin		array[nin] of monotonically increasing or decreasing x values
ydin		array[nin][4] of y(x), y'(x), y''(x), and y'''(x)
nout		length of xout and yout arrays
xout		array[nout] of x values at which to evaluate y(x), y'(x), ...

Output:
yout		array[nout] of y(x), y'(x), ... values

******************************************************************************
Notes:
xin values must be monotonically increasing or decreasing.

Extrapolation of the function y(x) for xout values outside the range
spanned by the xin values is performed using the derivatives in 
ydin[0][0:3] or ydin[nin-1][0:3], depending on whether xout is closest
to xin[0] or xin[nin-1], respectively.

******************************************************************************
Reference:
See: Greville, T. N., Theory and Application of Spline Functions, Academic Press
for a general discussion of spline functions.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

#define O2 0.5000000
#define O6 0.1666667

void intcub (int ideriv, int nin, float xin[], float ydin[][4], 
	int nout, float xout[], float yout[])
/*****************************************************************************
evaluate y(x), y'(x), y''(x), ... via piecewise cubic interpolation
******************************************************************************
Input:
ideriv		=0 if y(x) desired; =1 if y'(x) desired, ...
nin		length of xin and ydin arrays
xin		array[nin] of monotonically increasing or decreasing x values
ydin		array[nin][4] of y(x), y'(x), y''(x), and y'''(x)
nout		length of xout and yout arrays
xout		array[nout] of x values at which to evaluate y(x), y'(x), ...

Output:
yout		array[nout] of y(x), y'(x), ... values
******************************************************************************
Notes:
xin values must be monotonically increasing or decreasing.

Extrapolation of the function y(x) for xout values outside the range
spanned by the xin values is performed using the derivatives in 
ydin[0][0:3] or ydin[nin-1][0:3], depending on whether xout is closest
to xin[0] or xin[nin-1], respectively.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	static int idx;
	int iout;
	float delx;

	/* y(x) is desired, then */
	if (ideriv==0) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&idx);
			delx = xout[iout]-xin[idx];
			yout[iout] = (ydin[idx][0]+delx*
				(ydin[idx][1]+delx*
				(ydin[idx][2]*O2+delx*
				(ydin[idx][3]*O6))));
		}
		
	/* else, if y'(x) is desired, then */
	} else if (ideriv==1) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&idx);
			delx = xout[iout]-xin[idx];
			yout[iout] = (ydin[idx][1]+delx*
				(ydin[idx][2]+delx*
				(ydin[idx][3]*O2)));
		}
		
	/* else, if y''(x) is desired, then */
	} else if (ideriv==2) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&idx);
			delx = xout[iout]-xin[idx];
			yout[iout] = (ydin[idx][2]+delx*
				(ydin[idx][3]));
		}
		
	/* else, if y'''(x) is desired, then */
	} else if (ideriv==3) {
		for (iout=0; iout<nout; iout++) {
			xindex(nin,xin,xout[iout],&idx);
			delx = xout[iout]-xin[idx];
			yout[iout] = (ydin[idx][3]);
		}
		
	/* else, if any other derivative is desired, then */
	} else {
		for (iout=0; iout<nout; iout++)
			yout[iout] = 0.0;
	}
}
