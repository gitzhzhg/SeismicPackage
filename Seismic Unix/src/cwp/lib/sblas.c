/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
SBLAS - Single precision Basic Linear Algebra Subroutines
	(adapted from LINPACK FORTRAN):

isamax	return index of element with maximum absolute value
sasum	return sum of absolute values
saxpy	compute y[i] = a*x[i]+y[i]
scopy	copy x[i] to y[i] (i.e., set y[i] = x[i])
sdot	return sum of x[i]*y[i] (i.e., return the dot product of x and y)
snrm2	return square root of sum of squares of x[i]
sscal	compute x[i] = a*x[i]
sswap	swap x[i] and y[i]

******************************************************************************
Function Prototypes:
int isamax (int n, float *sx, int incx);
float sasum (int n, float *sx, int incx);
void saxpy (int n, float sa, float *sx, int incx, float *sy, int incy);
void scopy (int n, float *sx, int incx, float *sy, int incy);
float sdot (int n, float *sx, int incx, float *sy, int incy);
float snrm2 (int n, float *sx, int incx);
void sscal (int n, float sa, float *sx, int incx);
void sswap (int n, float *sx, int incx, float *sy, int incy);

******************************************************************************
isamax:
Input:
n		number of elements in array
sx		array[n] of elements
incx		increment between elements 

Returned:	index of element with maximum absolute value

******************************************************************************
sasum:
Input:
n		number of elements in array
sx		array[n] of elements
incx		increment between elements 

Returned:	sum of absolute values

******************************************************************************
saxpy:
Input:
n		number of elements in arrays
sa		the scalar multiplier
sx		array[n] of elements to be scaled and added
incx		increment between elements of sx
sy		array[n] of elements to be added
incy		increment between elements of sy

Output:
sy		array[n] of accumulated elements

******************************************************************************
scopy:
Input:
n		number of elements in arrays
sx		array[n] of elements to be copied
incx		increment between elements of sx
incy		increment between elements of sy

Output:
sy		array[n] of copied elements

******************************************************************************
sdot:
Input:
n		number of elements in arrays
sx		array[n] of elements
incx		increment between elements of sx
sy		array[n] of elements
incy		increment between elements of sy

Returned:	dot product of the two arrays

******************************************************************************
snrm2
Input:
n		number of elements in array
sx		array[n] of elements
incx		increment between elements 

Returned:	square root of sum of squares of x[i]

******************************************************************************
sscal:
Input:
n		number of elements in array
sa		the scalar multiplier
sx		array[n] of elements
incx		increment between elements 

Output:
sx		array[n] of scaled elements

******************************************************************************
sswap:
Input:
n		number of elements in arrays
sx		array[n] of elements
incx		increment between elements of sx
sy		array[n] of elements
incy		increment between elements of sy

Output:
sx		array[n] of swapped elements
sy		array[n] of swapped elements

******************************************************************************
Notes:
Adapted from Linpack Fortran.

snrm2:
This simple version may cause overflow or underflow! 

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 10/01/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

int isamax (int n, float *sx, int incx)
/*****************************************************************************
return index of element with maximum absolute value
******************************************************************************
Input:
n		number of elements in array
sx		array[n] of elements
incx		increment between elements 

Returned:	index of element with maximum absolute value
*****************************************************************************/
{
	int i,j,imax;
	float smax,xmag;

	/* if increment is not 1 */
	if (incx!=1) {
		imax = 0;
		smax = ABS(sx[0]);
		for (i=j=0; i<n; i++,j+=incx) {
			xmag = ABS(sx[j]);
			if (xmag>smax) {
				imax = i;
				smax = xmag;
			}
		}
	
	/* else, if increment is 1 */
	} else {
		imax = 0;
		smax = ABS(sx[0]);
		for (i=1; i<n; i++) {
			xmag = ABS(sx[i]);
			if (xmag>smax) {
				imax = i;
				smax = xmag;
			}
		}
	}
	return imax;
}

float sasum (int n, float *sx, int incx)
/*****************************************************************************
return sum of absolute values
******************************************************************************
Input:
n		number of elements in array
sx		array[n] of elements
incx		increment between elements 

Returned:	sum of absolute values
*****************************************************************************/
{
	int i,j,m;
	float sum=0.0;

	if (n<=0) return 0.0;

	/* if increment is not 1 */
	if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sum += ABS(sx[j]);

	/* else, if increment is 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 6 */
		m = n%6;
		for (i=0; i<m; i++)
			sum += ABS(sx[i]);
		
		/* finish in multiples of 6 */
		for (i=m; i<n; i+=6)
			sum += ABS(sx[i])+
				ABS(sx[i+1])+
				ABS(sx[i+2])+
				ABS(sx[i+3])+
				ABS(sx[i+4])+
				ABS(sx[i+5]);
	}
	return sum;
}

void saxpy (int n, float sa, float *sx, int incx, float *sy, int incy)
/*****************************************************************************
compute y[i] = y[i]+a*x[i]
******************************************************************************
Input:
n		number of elements in arrays
sa		the scalar multiplier
sx		array[n] of elements to be scaled and added
incx		increment between elements of sx
sy		array[n] of elements to be added
incy		increment between elements of sy

Output:
sy		array[n] of accumulated elements
*****************************************************************************/
{
	int i,j,ix,iy,m;

	if (n<=0 || sa==0.0) return;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy)
			sy[iy] += sa*sx[ix];
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sy[j] += sa*sx[j];
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 4 */
		m = n%4;
		for (i=0; i<m; i++)
			sy[i] += sa*sx[i];
		
		/* finish in multiples of 4 */
		for (i=m; i<n; i+=4) {
			sy[i] += sa*sx[i];
			sy[i+1] += sa*sx[i+1];
			sy[i+2] += sa*sx[i+2];
			sy[i+3] += sa*sx[i+3];
		}
	}
}

void scopy (int n, float *sx, int incx, float *sy, int incy)
/*****************************************************************************
copy x[i] to y[i] (i.e., set y[i] = x[i])
******************************************************************************
Input:
n		number of elements in arrays
sx		array[n] of elements to be copied
incx		increment between elements of sx
incy		increment between elements of sy

Output:
sy		array[n] of copied elements
*****************************************************************************/
{
	int i,j,ix,iy,m;

	if (n<=0) return;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy)
			sy[iy] = sx[ix];
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sy[j] = sx[j];
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 7 */
		m = n%7;
		for (i=0; i<m; i++)
			sy[i] = sx[i];
		
		/* finish in multiples of 7 */
		for (i=m; i<n; i+=7) {
			sy[i] = sx[i];
			sy[i+1] = sx[i+1];
			sy[i+2] = sx[i+2];
			sy[i+3] = sx[i+3];
			sy[i+4] = sx[i+4];
			sy[i+5] = sx[i+5];
			sy[i+6] = sx[i+6];
		}
	}
}

float sdot (int n, float *sx, int incx, float *sy, int incy)
/*****************************************************************************
return sum of x[i]*y[i] (i.e., return the dot product of x and y)
******************************************************************************
Input:
n		number of elements in arrays
sx		array[n] of elements
incx		increment between elements of sx
sy		array[n] of elements
incy		increment between elements of sy

Returned:	dot product of the two arrays
*****************************************************************************/
{
	int i,j,ix,iy,m;
	float sum=0.0;

	if (n<=0) return 0.0;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy)
			sum += sx[ix]*sy[iy];
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sum += sx[j]*sy[j];
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 5 */
		m = n%5;
		for (i=0; i<m; i++)
			sum += sx[i]*sy[i];
		
		/* finish in multiples of 5 */
		for (i=m; i<n; i+=5) {
			sum += sx[i]*sy[i]+
				sx[i+1]*sy[i+1]+
				sx[i+2]*sy[i+2]+
				sx[i+3]*sy[i+3]+
				sx[i+4]*sy[i+4];
		}
	}
	return sum;
}

float snrm2 (int n, float *sx, int incx)
/*****************************************************************************
return square root of sum of squares of x[i] (i.e. length of the vector)
******************************************************************************
Input:
n		number of elements in array
sx		array[n] of elements
incx		increment between elements 

Returned:	square root of sum of squares of x[i]
******************************************************************************
Note:  this simple version may cause overflow or underflow! 
*****************************************************************************/
{
	int i,j,m;
	float sum=0.0;

	if (n<=0) return 0.0;

	/* if increment is not 1 */
	if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sum += sx[j]*sx[j];

	/* else, if increment is 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 6 */
		m = n%6;
		for (i=0; i<m; i++)
			sum += sx[i]*sx[i];
		
		/* finish in multiples of 6 */
		for (i=m; i<n; i+=6)
			sum += sx[i]*sx[i]+
				sx[i+1]*sx[i+1]+
				sx[i+2]*sx[i+2]+
				sx[i+3]*sx[i+3]+
				sx[i+4]*sx[i+4]+
				sx[i+5]*sx[i+5];
	}
	return sqrt(sum);
}

void sscal (int n, float sa, float *sx, int incx)
/*****************************************************************************
compute x[i] = a*x[i]
******************************************************************************
Input:
n		number of elements in array
sa		the scalar multiplier
sx		array[n] of elements
incx		increment between elements 

Output:
sx		array[n] of scaled elements
*****************************************************************************/
{
	int i,j,m;

	if (n<=0) return;

	/* if increment not equal to 1 */
	if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx)
			sx[j] *= sa;
	
	/* else, if increment equal to 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 5 */
		m = n%5;
		for (i=0; i<m; i++)
			sx[i] *= sa;
		
		/* finish in multiples of 5 */
		for (i=m; i<n; i+=5) {
			sx[i] *= sa;
			sx[i+1] *= sa;
			sx[i+2] *= sa;
			sx[i+3] *= sa;
			sx[i+4] *= sa;
		}
	}
}

void sswap (int n, float *sx, int incx, float *sy, int incy)
/*****************************************************************************
swap x[i] and y[i]
******************************************************************************
Input:
n		number of elements in arrays
sx		array[n] of elements
incx		increment between elements of sx
sy		array[n] of elements
incy		increment between elements of sy

Output:
sx		array[n] of swapped elements
sy		array[n] of swapped elements
*****************************************************************************/
{
	int i,j,ix,iy,m;
	float stemp1,stemp2,stemp3;

	if (n<=0) return;

	/* if nonequal or nonpositive increments */
	if (incx!=incy || incx<0 || incy<0) {
		ix = (incx>=0)?0:(-n+1)*incx;
		iy = (incy>=0)?0:(-n+1)*incy;
		for (i=0; i<n; i++,ix+=incx,iy+=incy) {
			stemp1 = sx[ix];
			sx[ix] = sy[iy];
			sy[iy] = stemp1;
		}
	
	/* else, if equal, positive, nonunit increments */
	} else if (incx!=1) {
		for (i=j=0; i<n; i++,j+=incx) {
			stemp1 = sx[j];
			sx[j] = sy[j];
			sy[j] = stemp1;
		}
	
	/* else, if both increments equal 1 */
	} else {
		/* clean-up so remaining vector length is a multiple of 3 */
		m = n%3;
		for (i=0; i<m; i++) {
			stemp1 = sx[i];
			sx[i] = sy[i];
			sy[i] = stemp1;
		}
		
		/* finish in multiples of 3 */
		for (i=m; i<n; i+=3) {
			stemp1 = sx[i];
			stemp2 = sx[i+1];
			stemp3 = sx[i+2];
			sx[i] = sy[i];
			sx[i+1] = sy[i+1];
			sx[i+2] = sy[i+2];
			sy[i] = stemp1;
			sy[i+1] = stemp2;
			sy[i+2] = stemp3;
		}
	}
}
