/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
CUBICSPLINE - Functions to compute CUBIC SPLINE interpolation coefficients

cakima		compute cubic spline coefficients via Akima's method
		  (continuous 1st derivatives, only)
cmonot		compute cubic spline coefficients via the Fritsch-Carlson method
		  (continuous 1st derivatives, only)
csplin		compute cubic spline coefficients for interpolation 
		  (continuous 1st and 2nd derivatives)

chermite	compute cubic spline coefficients via Hermite Polynomial
		  (continuous 1st derivatives only)
******************************************************************************
Function Prototypes:
void cakima   (int n, float x[], float y[], float yd[][4]);
void cmonot   (int n, float x[], float y[], float yd[][4]);
void csplin   (int n, float x[], float y[], float yd[][4]);
void chermite (int n, float x[], float y[], float yd[][4]);

******************************************************************************
Input:
n		number of samples
x  		array[n] of monotonically increasing or decreasing abscissae
y		array[n] of ordinates

Output:
yd		array[n][4] of cubic interpolation coefficients (see notes)

******************************************************************************
Notes:
The computed cubic spline coefficients are as follows:
yd[i][0] = y(x[i])    (the value of y at x = x[i])
yd[i][1] = y'(x[i])   (the 1st derivative of y at x = x[i])
yd[i][2] = y''(x[i])  (the 2nd derivative of y at x = x[i])
yd[i][3] = y'''(x[i]) (the 3rd derivative of y at x = x[i])

To evaluate y(x) for x between x[i] and x[i+1] and h = x-x[i],
use the computed coefficients as follows:
y(x) = yd[i][0]+h*(yd[i][1]+h*(yd[i][2]/2.0+h*yd[i][3]/6.0))

Akima's method provides continuous 1st derivatives, but 2nd and
3rd derivatives are discontinuous.  Akima's method is not linear, 
in that the interpolation of the sum of two functions is not the 
same as the sum of the interpolations.

The Fritsch-Carlson method yields continuous 1st derivatives, but 2nd
and 3rd derivatives are discontinuous.  The method will yield a 
monotonic interpolant for monotonic data.  1st derivatives are set 
to zero wherever first divided differences change sign.

The method used by "csplin" yields continuous 1st and 2nd derivatives.

******************************************************************************
References:
See Akima, H., 1970, A new method for 
interpolation and smooth curve fitting based on local procedures,
Journal of the ACM, v. 17, n. 4, p. 589-602.

For more information, see Fritsch, F. N., and Carlson, R. E., 1980, 
Monotone piecewise cubic interpolation:  SIAM J. Numer. Anal., v. 17,
n. 2, p. 238-246.
Also, see the book by Kahaner, D., Moler, C., and Nash, S., 1989, 
Numerical Methods and Software, Prentice Hall.  This function was 
derived from SUBROUTINE PCHEZ contained on the diskette that comes 
with the book.

For more general information on spline functions of all types see the book by:
Greville, T.N.E, 1969, Theory and Applications of Spline Functions,
Academic Press.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines c. 1989, 1990, 1991
*****************************************************************************/
/**************** end self doc ********************************/

#include <cwp.h>

void cakima (int n, float x[], float y[], float yd[][4])
/*****************************************************************************
Compute cubic interpolation coefficients via Akima's method
******************************************************************************
Input:
n		number of samples
x  		array[n] of monotonically increasing or decreasing abscissae
y		array[n] of ordinates

Output:
yd		array[n][4] of cubic interpolation coefficients (see notes)
******************************************************************************
Notes:
The computed cubic spline coefficients are as follows:
yd[i][0] = y(x[i])    (the value of y at x = x[i])
yd[i][1] = y'(x[i])   (the 1st derivative of y at x = x[i])
yd[i][2] = y''(x[i])  (the 2nd derivative of y at x = x[i])
yd[i][3] = y'''(x[i]) (the 3rd derivative of y at x = x[i])

To evaluate y(x) for x between x[i] and x[i+1] and h = x-x[i],
use the computed coefficients as follows:
y(x) = yd[i][0]+h*(yd[i][1]+h*(yd[i][2]/2.0+h*yd[i][3]/6.0))

Akima's method provides continuous 1st derivatives, but 2nd and
3rd derivatives are discontinuous.  Akima's method is not linear, 
in that the interpolation of the sum of two functions is not the 
same as the sum of the interpolations.

For more information, see Akima, H., 1970, A new method for 
interpolation and smooth curve fitting based on local procedures,
Journal of the ACM, v. 17, n. 4, p. 589-602.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/30/89
Modified:  Dave Hale, Colorado School of Mines, 02/28/91
	changed to work for n=1.
*****************************************************************************/
{
	int i;
	float sumw,yd1fx,yd1lx,dx,divdf3;

	/* copy ordinates into output array */
	for (i=0; i<n; i++)
		yd[i][0] = y[i];

	/* if n=1, then use constant interpolation */
	if (n==1) {
		yd[0][1] = 0.0;
		yd[0][2] = 0.0;
		yd[0][3] = 0.0;
		return;

	/* else, if n=2, then use linear interpolation */
	} else if (n==2) {
		yd[0][1] = yd[1][1] = (y[1]-y[0])/(x[1]-x[0]);
		yd[0][2] = yd[1][2] = 0.0;
		yd[0][3] = yd[1][3] = 0.0;
		return;
	}

	/* compute 1st divided differences and store in yd[.][2] */
	for (i=1; i<n; i++)
		yd[i][2] = (y[i]-y[i-1])/(x[i]-x[i-1]);

	/* compute weights and store in yd[.][3] */
	for (i=1; i<n-1; i++)
		yd[i][3] = fabs(yd[i+1][2]-yd[i][2]);
	yd[0][3] = yd[1][3];
	yd[n-1][3] = yd[n-2][3];

	/* compute 1st derivative at first x */
	sumw = yd[1][3]+yd[0][3];
	yd1fx = 2.0*yd[1][2]-yd[2][2];
	if (sumw!=0.0)
		yd[0][1] = (yd[1][3]*yd1fx+yd[0][3]*yd[1][2])/sumw;
	else
		yd[0][1] = 0.5*(yd1fx+yd[1][2]);

	/* compute 1st derivatives in interior as weighted 1st differences */
	for (i=1; i<n-1; i++) {
		sumw = yd[i+1][3]+yd[i-1][3];
		if (sumw!=0.0)
			yd[i][1] = (yd[i+1][3]*yd[i][2]+yd[i-1][3]*yd[i+1][2])/sumw;
		else
			yd[i][1] = 0.5*(yd[i][2]+yd[i+1][2]);
	}

	/* compute 1st derivative at last x */
	sumw = yd[n-2][3]+yd[n-1][3];
	yd1lx = 2.0*yd[n-1][2]-yd[n-2][2];
	if (sumw!=0.0)
		yd[n-1][1] = (yd[n-2][3]*yd1lx+yd[n-1][3]*yd[n-1][2])/sumw;
	else
		yd[n-1][1] = 0.5*(yd[n-1][2]+yd1lx);

	/* compute 2nd and 3rd derivatives of cubic polynomials */
	for (i=1; i<n; i++) {
		dx = x[i]-x[i-1];
		divdf3 = yd[i-1][1]+yd[i][1]-2.0*yd[i][2];
		yd[i-1][2] = 2.0*(yd[i][2]-yd[i-1][1]-divdf3)/dx;
		yd[i-1][3] = (divdf3/dx)*(6.0/dx);
	}
	yd[n-1][2] = yd[n-2][2]+(x[n-1]-x[n-2])*yd[n-2][3];
	yd[n-1][3] = yd[n-2][3];
}

void cmonot (int n, float x[], float y[], float yd[][4])
/*****************************************************************************
compute cubic interpolation coefficients via the Fritsch-Carlson method,
which preserves monotonicity
******************************************************************************
Input:
n		number of samples
x  		array[n] of monotonically increasing or decreasing abscissae
y		array[n] of ordinates

Output:
yd		array[n][4] of cubic interpolation coefficients (see notes)
******************************************************************************
Notes:
The computed cubic spline coefficients are as follows:
yd[i][0] = y(x[i])    (the value of y at x = x[i])
yd[i][1] = y'(x[i])   (the 1st derivative of y at x = x[i])
yd[i][2] = y''(x[i])  (the 2nd derivative of y at x = x[i])
yd[i][3] = y'''(x[i]) (the 3rd derivative of y at x = x[i])

To evaluate y(x) for x between x[i] and x[i+1] and h = x-x[i],
use the computed coefficients as follows:
y(x) = yd[i][0]+h*(yd[i][1]+h*(yd[i][2]/2.0+h*yd[i][3]/6.0))

The Fritsch-Carlson method yields continuous 1st derivatives, but 2nd
and 3rd derivatives are discontinuous.  The method will yield a 
monotonic interpolant for monotonic data.  1st derivatives are set 
to zero wherever first divided differences change sign.

For more information, see Fritsch, F. N., and Carlson, R. E., 1980, 
Monotone piecewise cubic interpolation:  SIAM J. Numer. Anal., v. 17,
n. 2, p. 238-246.

Also, see the book by Kahaner, D., Moler, C., and Nash, S., 1989, 
Numerical Methods and Software, Prentice Hall.  This function was 
derived from SUBROUTINE PCHEZ contained on the diskette that comes 
with the book.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 09/30/89
Modified:  Dave Hale, Colorado School of Mines, 02/28/91
	changed to work for n=1.
Modified:  Dave Hale, Colorado School of Mines, 08/04/91
	fixed bug in computation of left end derivative
*****************************************************************************/
{
	int i;
	float h1,h2,del1,del2,dmin,dmax,hsum,hsum3,w1,w2,drat1,drat2,divdf3;

	/* copy ordinates into output array */
	for (i=0; i<n; i++)
		yd[i][0] = y[i];

	/* if n=1, then use constant interpolation */
	if (n==1) {
		yd[0][1] = 0.0;
		yd[0][2] = 0.0;
		yd[0][3] = 0.0;
		return;

	/* else, if n=2, then use linear interpolation */
	} else if (n==2) {
		yd[0][1] = yd[1][1] = (y[1]-y[0])/(x[1]-x[0]);
		yd[0][2] = yd[1][2] = 0.0;
		yd[0][3] = yd[1][3] = 0.0;
		return;
	}

	/* set left end derivative via shape-preserving 3-point formula */
	h1 = x[1]-x[0];
	h2 = x[2]-x[1];
	hsum = h1+h2;
	del1 = (y[1]-y[0])/h1;
	del2 = (y[2]-y[1])/h2;
	w1 = (h1+hsum)/hsum;
	w2 = -h1/hsum;
	yd[0][1] = w1*del1+w2*del2;
	if (yd[0][1]*del1<=0.0)
		yd[0][1] = 0.0;
	else if (del1*del2<0.0) {
		dmax = 3.0*del1;
		if (ABS(yd[0][1])>ABS(dmax)) yd[0][1] = dmax;
	}

	/* loop over interior points */
	for (i=1; i<n-1; i++) {

		/* compute intervals and slopes */
		h1 = x[i]-x[i-1];
		h2 = x[i+1]-x[i];
		hsum = h1+h2;
		del1 = (y[i]-y[i-1])/h1;
		del2 = (y[i+1]-y[i])/h2;

		/* if not strictly monotonic, zero derivative */
		if (del1*del2<=0.0) {
			yd[i][1] = 0.0;
		
		/*
		 * else, if strictly monotonic, use Butland's formula:
		 *      3*(h1+h2)*del1*del2 
		 * -------------------------------
		 * ((2*h1+h2)*del1+(h1+2*h2)*del2)
		 * computed as follows to avoid roundoff error
		 */
		} else {
			hsum3 = hsum+hsum+hsum;
			w1 = (hsum+h1)/hsum3;
			w2 = (hsum+h2)/hsum3;
			dmin = MIN(ABS(del1),ABS(del2));
			dmax = MAX(ABS(del1),ABS(del2));
			drat1 = del1/dmax;
			drat2 = del2/dmax;
			yd[i][1] = dmin/(w1*drat1+w2*drat2);
		}
	}

	/* set right end derivative via shape-preserving 3-point formula */
	w1 = -h2/hsum;
	w2 = (h2+hsum)/hsum;
	yd[n-1][1] = w1*del1+w2*del2;
	if (yd[n-1][1]*del2<=0.0)
		yd[n-1][1] = 0.0;
	else if (del1*del2<0.0) {
		dmax = 3.0*del2;
		if (ABS(yd[n-1][1])>ABS(dmax)) yd[n-1][1] = dmax;
	}

	/* compute 2nd and 3rd derivatives of cubic polynomials */
	for (i=0; i<n-1; i++) {
		h2 = x[i+1]-x[i];
		del2 = (y[i+1]-y[i])/h2;
		divdf3 = yd[i][1]+yd[i+1][1]-2.0*del2;
		yd[i][2] = 2.0*(del2-yd[i][1]-divdf3)/h2;
		yd[i][3] = (divdf3/h2)*(6.0/h2);
	}
	yd[n-1][2] = yd[n-2][2]+(x[n-1]-x[n-2])*yd[n-2][3];
	yd[n-1][3] = yd[n-2][3];
}

void csplin (int n, float x[], float y[], float yd[][4])
/*****************************************************************************
compute cubic spline interpolation coefficients for interpolation with
continuous second derivatives
******************************************************************************
Input:
n		number of samples
x  		array[n] of monotonically increasing or decreasing abscissae
y		array[n] of ordinates

Output:
yd		array[n][4] of cubic interpolation coefficients (see notes)
******************************************************************************
Notes:
The computed cubic spline coefficients are as follows:
yd[i][0] = y(x[i])    (the value of y at x = x[i])
yd[i][1] = y'(x[i])   (the 1st derivative of y at x = x[i])
yd[i][2] = y''(x[i])  (the 2nd derivative of y at x = x[i])
yd[i][3] = y'''(x[i]) (the 3rd derivative of y at x = x[i])

To evaluate y(x) for x between x[i] and x[i+1] and h = x-x[i],
use the computed coefficients as follows:
y(x) = yd[i][0]+h*(yd[i][1]+h*(yd[i][2]/2.0+h*yd[i][3]/6.0))
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 10/03/89
Modified:  Dave Hale, Colorado School of Mines, 02/28/91
	changed to work for n=1.
Modified:  Dave Hale, Colorado School of Mines, 08/04/91
	fixed bug in computation of left end derivative
*****************************************************************************/
{
	int i;
	float h1,h2,del1,del2,dmax,hsum,w1,w2,divdf3,sleft,sright,alpha,t;

	/* if n=1, then use constant interpolation */
	if (n==1) {
		yd[0][0] = y[0];
		yd[0][1] = 0.0;
		yd[0][2] = 0.0;
		yd[0][3] = 0.0;
		return;

	/* else, if n=2, then use linear interpolation */
	} else if (n==2) {
		yd[0][0] = y[0];  yd[1][0] = y[1];
		yd[0][1] = yd[1][1] = (y[1]-y[0])/(x[1]-x[0]);
		yd[0][2] = yd[1][2] = 0.0;
		yd[0][3] = yd[1][3] = 0.0;
		return;
	}
	
	/* set left end derivative via shape-preserving 3-point formula */
	h1 = x[1]-x[0];
	h2 = x[2]-x[1];
	hsum = h1+h2;
	del1 = (y[1]-y[0])/h1;
	del2 = (y[2]-y[1])/h2;
	w1 = (h1+hsum)/hsum;
	w2 = -h1/hsum;
	sleft = w1*del1+w2*del2;
	if (sleft*del1<=0.0)
		sleft = 0.0;
	else if (del1*del2<0.0) {
		dmax = 3.0*del1;
		if (ABS(sleft)>ABS(dmax)) sleft = dmax;
	}

	/* set right end derivative via shape-preserving 3-point formula */
	h1 = x[n-2]-x[n-3];
	h2 = x[n-1]-x[n-2];
	hsum = h1+h2;
	del1 = (y[n-2]-y[n-3])/h1;
	del2 = (y[n-1]-y[n-2])/h2;
	w1 = -h2/hsum;
	w2 = (h2+hsum)/hsum;
	sright = w1*del1+w2*del2;
	if (sright*del2<=0.0)
		sright = 0.0;
	else if (del1*del2<0.0) {
		dmax = 3.0*del2;
		if (ABS(sright)>ABS(dmax)) sright = dmax;
	}
	
	/* compute tridiagonal system coefficients and right-hand-side */	
	yd[0][0] = 1.0;
	yd[0][2] = 2.0*sleft;
	for (i=1; i<n-1; i++) {
		h1 = x[i]-x[i-1];
		h2 = x[i+1]-x[i];
		del1 = (y[i]-y[i-1])/h1;
		del2 = (y[i+1]-y[i])/h2;
		alpha = h2/(h1+h2);
		yd[i][0] = alpha;
		yd[i][2] = 3.0*(alpha*del1+(1.0-alpha)*del2);
	}
	yd[n-1][0] = 0.0;
	yd[n-1][2] = 2.0*sright;
	
	/* solve tridiagonal system for slopes */
	t = 2.0;
	yd[0][1] = yd[0][2]/t;
	for (i=1; i<n; i++) {
		yd[i][3] = (1.0-yd[i-1][0])/t;
		t = 2.0-yd[i][0]*yd[i][3];
		yd[i][1] = (yd[i][2]-yd[i][0]*yd[i-1][1])/t;
	}
	for (i=n-2; i>=0; i--)
		yd[i][1] -= yd[i+1][3]*yd[i+1][1];

	/* copy ordinates into output array */
	for (i=0; i<n; i++)
		yd[i][0] = y[i];

	/* compute 2nd and 3rd derivatives of cubic polynomials */
	for (i=0; i<n-1; i++) {
		h2 = x[i+1]-x[i];
		del2 = (y[i+1]-y[i])/h2;
		divdf3 = yd[i][1]+yd[i+1][1]-2.0*del2;
		yd[i][2] = 2.0*(del2-yd[i][1]-divdf3)/h2;
		yd[i][3] = (divdf3/h2)*(6.0/h2);
	}
	yd[n-1][2] = yd[n-2][2]+(x[n-1]-x[n-2])*yd[n-2][3];
	yd[n-1][3] = yd[n-2][3];
}
#include <cwp.h>

void chermite (int n, float x[], float y[], float yd[][4])
/*****************************************************************************
Compute cubic interpolation coefficients via Hermite polynomial
******************************************************************************
Input:
n		number of samples
x  		array[n] of monotonically increasing or decreasing abscissae
y		array[n] of ordinates

Output:
yd		array[n][4] of cubic interpolation coefficients (see notes)
******************************************************************************
Notes:
The computed cubic spline coefficients are as follows:
yd[i][0] = y(x[i])    (the value of y at x = x[i])
yd[i][1] = y'(x[i])   (the 1st derivative of y at x = x[i])
yd[i][2] = y''(x[i])  (the 2nd derivative of y at x = x[i])
yd[i][3] = y'''(x[i]) (the 3rd derivative of y at x = x[i])

To evaluate y(x) for x between x[i] and x[i+1] and h = x-x[i],
use the computed coefficients as follows:
y(x) = yd[i][0]+h*(yd[i][1]+h*(yd[i][2]/2.0+h*yd[i][3]/6.0))

*****************************************************************************/
{
	int i;
	float dx ,f1,f3;

	/* copy ordinates into output array */
	for (i=0; i<n; i++){
		yd[i][0] = y[i];
        }

	/* if n=1, then use constant interpolation */
	if (n==1) {
		yd[0][1] = 0.0;
		yd[0][2] = 0.0;
		yd[0][3] = 0.0;
		return;

	/* else, if n=2, then use linear interpolation */
	} else if (n==2) {
		yd[0][1] = yd[1][1] = (y[1]-y[0])/(x[1]-x[0]);
		yd[0][2] = yd[1][2] = 0.0;
		yd[0][3] = yd[1][3] = 0.0;
		return;
	}



        /* compute forward & backwards differences at the ends */

        yd[0][1]   = (y[2] - y[0])     / (x[2] - x[0]);
        yd[n-1][1] = (y[n-1] - y[n-2]) / (x[n-1] - x[n-2]);

        /* compute central differences in between */

        for( i=1; i<n-1; i++ ){

           yd[i][1] = 0.5 * (y[i] - y[i-1]) / (x[i] - x[i-1])
	            + 0.5 * (y[i+1] - y[i]) / (x[i+1] - x[i]);
        }

        /* calculate 2nd and 3rd derivatives */

        for( i=0; i<n-1; i++ ){

           dx       = x[i+1] - x[i];
           f1       = (y[i+1] - y[i]) / dx;
           f3       = yd[i][1] + yd[i+1][1] - 2.0 * f1;
           yd[i][2] = 2.0*(f1 - yd[i][1] - f3) / dx;
           yd[i][3] = 6.0*f3 / (dx*dx);

        }

}
