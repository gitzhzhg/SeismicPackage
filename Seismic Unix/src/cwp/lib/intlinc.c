/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
INTLINC - evaluate complex y(x) via linear interpolation of y(x[0]), y(x[1]), ...

******************************************************************************
Function Prototype:
void intlinc (int nin, float xin[], complex yin[], complex yinl, complex yinr,
	int nout, float xout[], complex yout[]);

******************************************************************************
Input:
nin		length of xin and yin arrays
xin		array[nin] of monotonically increasing or decreasing x values
yin		array[nin] of input y(x) values
yinl		value used to extraplate y(x) to left of input yin values
yinr		value used to extraplate y(x) to right of input yin values
nout		length of xout and yout arrays
xout		array[nout] of x values at which to evaluate y(x)

Output:
yout		array[nout] of linearly interpolated y(x) values

******************************************************************************
Notes:
xin values must be monotonically increasing or decreasing.

Extrapolation of the function y(x) for xout values outside the range
spanned by the xin values in performed as follows:

	For monotonically increasing xin values,
		yout=yinl if xout<xin[0], and yout=yinr if xout>xin[nin-1].

	For monotonically decreasing xin values, 
		yout=yinl if xout>xin[0], and yout=yinr if xout<xin[nin-1].

If nin==1, then the monotonically increasing case is used.
*/

/**************** end self doc ********************************/

#include "cwp.h"

void
intlinc (int nin, float xin[], complex yin[], complex yinl, complex yinr, 
	int nout, float xout[], complex yout[])
/*****************************************************************************
evaluate complex y(x) via linear interpolation of y(x[0]), y(x[1]), ...
******************************************************************************
Input:
nin		length of xin and yin arrays
xin		array[nin] of monotonically increasing or decreasing x values
yin		array[nin] of input y(x) values
yinl		value used to extraplate y(x) to left of input yin values
yinr		value used to extraplate y(x) to right of input yin values
nout		length of xout and yout arrays
xout		array[nout] of x values at which to evaluate y(x)

Output:
yout		array[nout] of linearly interpolated y(x) values
******************************************************************************
Notes:
xin values must be monotonically increasing or decreasing.

Extrapolation of the function y(x) for xout values outside the range
spanned by the xin values in performed as follows:

	For monotonically increasing xin values,
		yout=yinl if xout<xin[0], and yout=yinr if xout>xin[nin-1].

	For monotonically decreasing xin values, 
		yout=yinl if xout>xin[0], and yout=yinr if xout<xin[nin-1].

If nin==1, then the monotonically increasing case is used.
******************************************************************************
Author:  intlin(): Dave Hale, Colorado School of Mines, 06/02/89
         adapted for complex numbers: Hans Ecke 2002
*****************************************************************************/
{
	static int idx;
	int jout;
	float x;

	/* if input x values are monotonically increasing, then */
	if (xin[0]<=xin[nin-1]) {
		for (jout=0; jout<nout; jout++) {
			x = xout[jout];
			if (x<xin[0])
				yout[jout] = yinl;
			else if (x>xin[nin-1])
				yout[jout] = yinr;
			else if (x==xin[nin-1] || nin==1)
				yout[jout] = yin[nin-1];
			else {
				xindex(nin,xin,x,&idx);
				yout[jout] = cadd(yin[idx],
                                                  crmul(csub(yin[idx+1],
                                                             yin[idx]
                                                             ),
                                                        (x-xin[idx]) / (xin[idx+1]-xin[idx])
                                                        )
                                                 );
			}
		}
	
	/* else, if input x values are monotonically decreasing, then */
	} else {
		for (jout=0; jout<nout; jout++) {
			x = xout[jout];
			if (x>xin[0])
				yout[jout] = yinl;
			else if (x<xin[nin-1])
				yout[jout] = yinr;
			else if (x==xin[nin-1] || nin==1)
				yout[jout] = yin[nin-1];
			else {
				xindex(nin,xin,x,&idx);
				yout[jout] = cadd(yin[idx],
  					          crmul(csub(yin[idx+1],
                                                             yin[idx]
                                                             ),
                                                        (x-xin[idx])/(xin[idx+1]-xin[idx])
                                                        )
                                                  );
			}
		}
	}
}
