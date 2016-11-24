/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/

/********************************************************************
UNWRAP_PHASE - routines to UNWRAP phase of fourier transformed data

oppenheim_unwrap_phase - using the method of Oppenheim and Schafer (1975)
simple_unwrap_phase - by searching for phase jumps


********************************************************************
Function Prototype:
void oppenheim_unwrap_phase(int n, int trend, int zeromean, 
		float df, float *xr, float *xi, float *phase);
void simple_unwrap_phase(int n, int trend, int zeromean, 
		float w,  float *phase);
*************************************************************************
oppenheim_unwrap_phase:
Input:
n		number of samples
df		frequency sampling interval
trend		remove linear trend from unwrapped phase
xr		real part of signal
xi		imaginary part of signal

Output:
phase		array[n] of output unwrapped phase values

simple_unwrap_phase:
Input:
n               number of samples
trend           remove linear trend from phase
zeromean        =0 assume phase(0)=0.0, else assume zero mean
w               unwrapping parameter; returns an error if w=0
phase           array[n] of inpu

Output:
phase           array[n] of output phase values

*************************************************************************
oppenheim_unwrap_phase:
Notes:
1) The phase unwrapping method proposed by Oppenheim and Schaffer 
   1975 calculates the unwrapped phase by integrating the derivative 
   with respect to frequency of the phase of a signal F(w) . 

   Let u(w) = Re F(w) and v(w) = Im F(w)

   phase(w) = arctan[v(w)/u(w)] 

   Taking the derivative with respect to w of both sides

d/dw [ phase(w) ] = d/dw ( arctan (v/u) )
=   [ 1/ (1 +(v/u)^2) ] ( v'/u - vu'/u^2 ) = ( v'u - vu' )/(u^2 +v^2) 

2) Then, the d/dw phase   is integrated with respect to w 
   to produce the phase function

   phase(w) = integral phase'(w)  dw

3) the user has the option of removing the linear trend in the phase

   The approach allows us to avoid the principle branch behavior of
   the arctangent function.

References:
Oppenheim A.V. and R.W. Schafer, Digital Signal Processing,
Prentice-Hall, Englewood Cliffs, New Jersey, 1975.

Tria M., M. Van Der Baan 2, A. Larue, J. Mars 1, 2007,
Wavelet estimation in homomorphic domain by spectral 
averaging, for deconvolution of seismic data
For the BLISS Project, Université de Leeds, 
ITF Consorsium collaboration(s) (2007)

simple_unwrap_phase:
Notes:
The strategy is to look at the change in phase (dphase) with each
time step. If |dphase| >  PI/w, then use the previous value of
dphase. No attempt is made at smoothing the dphase curve.

*************************************************************************
oppenheim_unwrap_phase:
Author: John Stockwell, CWP, 2010

simple_unwrap_phase
Author: John Stockwell, CWP, 2010
************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

void oppenheim_unwrap_phase(int n, int trend, int zeromean, 
                        float df, float *xr, float *xi , float *phase)
/************************************************************************
oppenheim_unwrap_phase - unwrap the phase by the method of Oppenheim 1975
*************************************************************************
Input:
n		number of samples
trend		remove linear trend from unwrapped phase
df		frequency sampling interval
xr		real part of signal
xi		imaginary part of signal

Output:
phase		array[n] of output unwrapped phase values
*************************************************************************
Notes:
1) The phase unwrapping method proposed by Oppenheim 1975 makes use
of integrating the derivative with respect to frequency of the phase
of a signal F(w). Let u(w) = Re F(w) and v(w) = Im F(w)

phase(w) = arctan[v(w)/u(w)] 

d/dw [ phase(w) ] = d/dw ( arctan (v/u) )
=   [ 1/ (1 +(v/u)^2) ] ( v'/u - vu'/u^2 ) = ( v'u - vu' )/(u^2 +v^2) 

2) Then, the phase' is integrated
phase(w) = integral phase'(w)  dw

3) the user has the option of removing the linear trend in the phase

The approach allows us to avoid the principle branch behavior of
the arctangent function.

Reference:
Oppenheim A.V. and R.W. Schafer, Digital Signal Processing,
Prentice-Hall, Englewood Cliffs, New Jersey,
1975.

Tria M., M. Van Der Baan 2, A. Larue, J. Mars 1, 2007,
Wavelet estimation in homomorphic domain by spectral 
averaging, for deconvolution of seismic data
For the BLISS Project, Université de Leeds, 
ITF Consorsium collaboration(s) (2007)

*************************************************************************
Author: John Stockwell, CWP, 2010
************************************************************************/
{
	int i;
	float *phaseprime=NULL;
	float *xrprime=NULL;
	float *xiprime=NULL;
	float *freq=NULL;
	float  coeff[4];
	float temp;

	/* allocate space */
	phaseprime = alloc1float(n);
	xrprime = alloc1float(n);
	xiprime = alloc1float(n);
	freq = alloc1float(n);

	/* zero out the arrays */
	memset((void *) phase,  0, n*FSIZE);
	memset((void *) phaseprime,  0, n*FSIZE);
	memset((void *) xrprime,  0, n*FSIZE);
	memset((void *) xiprime,  0, n*FSIZE);


	/* initialize */
	phase[0]=xrprime[0]=xiprime[0]=phaseprime[0]=0.0;


	/* differentiate real and imaginary parts */
	differentiate(n, df, xr, xrprime);
	differentiate(n, df, xi, xiprime);

	/* compute unwrapped phase at each frequency step */
	temp=0.0;
	for (i = 0; i < n; ++i) {
		double denominator=(xr[i]*xr[i] + xi[i]*xi[i]);
		double numerator=(xr[i]*xiprime[i] - xrprime[i]*xi[i]);


		/* compute rate of change of phase */
		if (!CLOSETO(denominator,0.0)) {
			phaseprime[i] = numerator/denominator;
		} else { 
			phaseprime[i] = 0.0;
		}

		
		/* integrate the derivative of the phase */
		/* trapezoidal rule */
		if (i>0) {
			phase[i] =  phase[i-1] + df*(phaseprime[i] + phaseprime[i-1])/2.0;
		}

		freq[i] = i;
	}

	/* find the linear trend in the phase function */
	linear_regression(phase, freq, n, coeff);

	/* remove linear trend, assuming phase[0]=0.0  */
	if (trend) {
		for (i=0; i< n ; ++i ) {

			if (zeromean) {
				phase[i] = (phase[i] - ( coeff[0]*i + coeff[1]));
			} else {
				phase[i] = (phase[i] - ( coeff[0]*i ));
			}
		}
	}

	

	/* free space */
	free1float(phaseprime);
	free1float(xrprime);
	free1float(xiprime);

}

void simple_unwrap_phase(int n, int trend, int zeromean,
					float w,  float *phase)
/************************************************************************
unwrap_phase - unwrap the phase
*************************************************************************
Input:
n		number of samples
trend		remove linear trend from phase
zeromean	=0 assume phase(0)=0.0, else assume zero mean
w		unwrapping parameter; returns an error if w=0
phase		array[n] of input phase values

Output:
phase		array[n] of output phase values
*************************************************************************
Notes:
The strategy is to look at the change in phase (dphase) with each 
time step. If |dphase| >  PI/w, then use the previous value of 
dphase. No attempt is made at smoothing the dphase curve.
However, linear trend removal is provided.
*************************************************************************
Author: John Stockwell, CWP, 2010
************************************************************************/
{
	int i;
	float pibyw=0.0;
	float *dphase=NULL;
	float *temp=NULL;
	float *freq=NULL;
	float  coeff[4];



	/* prevent division by zero in PI/w */
	if (w==0) {
			fprintf(stderr,"wrapping parameter is zero");
			exit(EXIT_FAILURE);
	} else {
		
		pibyw = PI/w;
	}

	/* allocate space */
	dphase = alloc1float(n);
	temp = alloc1float(n);
        freq = alloc1float(n);

	/* zero out arrays */
	memset((void *) dphase,  0, n*FSIZE);
	memset((void *) temp,  0, n*FSIZE);
	memset((void *) freq,  0, n*FSIZE);

	/* initialize */
	temp[0]=phase[0];
	dphase[0]=0.0;
	freq[0] = 0;

	/* compute unwrapped phase at each frequency step */
	for (i = 1; i < n; ++i) {

		/* compute jump in phase */
		dphase[i] = phase[i] - phase[i-1];

		if ( i < n-1) {
			dphase[i+1] = phase[i+1] - phase[i-1];
		}


		/* if jump in dphase >= PI/w, use average of neighboring values */
		if (ABS(dphase[i] - dphase[i-1]) >= pibyw ) {
			if ( i < n -1 ) {	
				dphase[i] = (dphase[i-1] + dphase[i+1])/2.0;
			} else {
				dphase[i] = dphase[i-1] ;
			}
		}
		/* sum up values in temporary vector */
		temp[i] = temp[i-1] + dphase[i];

		/* capture array of frequency values */
		freq[i] = i;
	}


	/* remove linear trend, assuming phase[0]=0.0  */
	if (!trend) {
		for (i=0; i< n ; ++i ) phase[i] = temp[i];
	} else {
		/* find the linear trend in the phase function */
		linear_regression(temp, freq, n, coeff);

		for (i=0; i< n ; ++i ) {

			if (zeromean) {
				phase[i] = temp[i] - ( coeff[0]*i + coeff[1]);
			} else {
				phase[i] = temp[i] - ( coeff[0]*i );
			}
		}
	}

	
	/* free space */
	free1float(temp);
	free1float(dphase);

}
