/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
YCLIP - Clip a function y(x) defined by linear interpolation of the
uniformly sampled values:  y(fx), y(fx+dx), ..., y(fx+(nx-1)*dx).
Returns the number of samples in the clipped function.

yclip		clip a function y(x) defined by linear interplolation of
		uniformly sampled values

******************************************************************************
Function Prototype:
int yclip (int nx, float dx, float fx, float y[], float ymin, float ymax,
	float xc[], float yc[]);

******************************************************************************
Input:
nx		number of x (and y) values
dx		x sampling interval
fx		first x
y		array[nx] of uniformly sampled y(x) values
ymin		minimum y value; must not be greater than ymax
ymax		maximum y value; must not be less than ymin

Output:
xc		array[?] of x values for clipped y(x)
yc		array[?] of y values for clipped y(x)

Returned:	number of samples in output arrays xc and yc

******************************************************************************
Notes:
The output arrays xc and yc should contain space 2*nx values, which
is the maximum possible number (nc) of xc and yc returned.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/03/89
*****************************************************************************/
/**************** end self doc ********************************/


int yclip (int nx, float dx, float fx, float y[], float ymin, float ymax,
	float xc[], float yc[])
/*****************************************************************************
Clip a function y(x) defined by linear interpolation of the
uniformly sampled values:  y(fx), y(fx+dx), ..., y(fx+(nx-1)*dx).
Returns the number of samples in the clipped function.
******************************************************************************
Input:
nx		number of x (and y) values
dx		x sampling interval
fx		first x
y		array[nx] of uniformly sampled y(x) values
ymin		minimum y value; must not be greater than ymax
ymax		maximum y value; must not be less than ymin

Output:
xc		array[?] of x values for clipped y(x)
yc		array[?] of y values for clipped y(x)

Returned:	number of samples in output arrays xc and yc
******************************************************************************
Notes:
The output arrays xc and yc should contain space 2*nx values, which
is the maximum possible number (nc) of xc and yc returned.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 07/03/89
*****************************************************************************/
{
	int ix,nc;
	float xix,yix,yixm1;

	xix = fx;
	yix = y[0];
	nc = 0;
	xc[nc] = xix;
	if (yix<ymin)
		yc[nc++] = ymin;
	else if (yix>ymax)
		yc[nc++] = ymax;
	else 
		yc[nc++] = yix;
	for (ix=1; ix<nx; ix++) {
		yixm1 = yix;
		xix += dx;
		yix = y[ix];
		if (yixm1<ymin) {
			if (yix>=ymin) {
				xc[nc] = xix-dx*(yix-ymin)/(yix-yixm1);
				yc[nc++] = ymin;
				if (yix<=ymax) {
					xc[nc] = xix;
					yc[nc++] = yix;
				} else {
					xc[nc] = xix-dx*(yix-ymax)/(yix-yixm1);
					yc[nc++] = ymax;
				}
			}
		} else if (yixm1>ymax) {
			if (yix<=ymax) {
				xc[nc] = xix-dx*(yix-ymax)/(yix-yixm1);
				yc[nc++] = ymax;
				if (yix>=ymin) {
					xc[nc] = xix;
					yc[nc++] = yix;
				} else {
					xc[nc] = xix-dx*(yix-ymin)/(yix-yixm1);
					yc[nc++] = ymin;
				}
			}
		} else {
			if (yix<ymin) {
				xc[nc] = xix-dx*(yix-ymin)/(yix-yixm1);
				yc[nc++] = ymin;
			} else if (yix>ymax) {
				xc[nc] = xix-dx*(yix-ymax)/(yix-yixm1);
				yc[nc++] = ymax;
			} else {
				xc[nc] = xix;
				yc[nc++] = yix;
			}
		}
	}
	if (yix<ymin) {
		xc[nc] = xix;
		yc[nc++] = ymin;
	} else if (yix>ymax) {
		xc[nc] = xix;
		yc[nc++] = ymax;
	}
	return nc;
}
