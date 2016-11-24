/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/******************************************************************************
WAVEFORMS   Subroutines to define some wavelets for modeling of seismic
            data
    
ricker1_wavelet     Compute the time response of a source function as
            a Ricker wavelet with peak frequency "fpeak" Hz.    

ricker2_wavelet     Compute a Ricke wavelet with a given period, amplitude
            and distorsion factor

akb_wavelet         Compute the time response of a source function as
            a wavelet based on a wavelet used by Alford, Kelly, 
            and Boore.

spike_wavelet       Compute the time response of a source function as
            a spike.    

unit_wavelet        Compute the time response of a source function as
            a constant unit shift.  

zero_wavelet        Compute the time response of a source function as
            zero everywhere.    

berlage_wavelet     Compute the time response of a source function as a
            Berlage wavelet with peak frequency "fpeak" Hz, 
            exponential decay factor "decay", time exponent 
            "tn", and initial phase angle "ipa".

gaussian_wavelet    Compute the time response of a source function as a
            Gaussian wavelet with peak frequency "fpeak" in Hz.

gaussderiv_wavelet  Compute the time response of a source function as a
            Gaussian first derivative wavelet with peak frequency "fpeak" 
            in Hz.
deriv_n_gauss  Compute the n-th derivative of a gaussian in double precision

*******************************************************************************
Function Prototypes:
void ricker1_wavelet (int nt, float dt, float fpeak, float *wavelet);
void ricker2_wavelet (int hlw, float dt, float period, float ampl, 
    float distort, float *wavelet);
void akb_wavelet (int nt, float dt, float fpeak, float *wavelet);
void spike_wavelet (int nt, int tindex, float *wavelet);
void unit_wavelet (int nt, float *wavelet);
void zero_wavelet (int nt, float *wavelet);
void berlage_wavelet (int nt, float dt, float fpeak, float ampl, float tn,
                       float decay, float ipa, float *wavelet);
void gaussian_wavelet (int nt, float dt, float fpeak, float *wavelet);
void gaussderiv_wavelet (int nt, float dt, float fpeak, float *wavelet);

******************************************************************************
Authors: Tong Fei, Ken Larner 
Author: Nils Maercklin, February 2007
******************************************************************************/

/**************** end self doc ********************************/


#include "cwp.h"

void ricker1_wavelet (int nt, float dt, float fpeak, float *wavelet)
/******************************************************************************
ricker_wavelet -- Compute the   time response of a source function as
    a Ricker wavelet with peak frequency "fpeak" Hz.    
*******************************************************************************
Input: 
    int nt      number samples in output wavelet
    float dt    time step
    float fpeak peak frequency of the Ricker wavelet
    
*******************************************************************************
Output: 
    float wavelet   array[nt] of computed wavelet

*******************************************************************************
Author: Tong Fei, 1993, Colorado School of Mines.
******************************************************************************/
{
    int it;
    float   t1, t0; 

    t0=1.0/fpeak;

    for (it=0; it<nt; it++) {
        t1=it*dt;
        wavelet[it] = exp(-PI*PI*fpeak*fpeak*(t1-t0)*(t1-t0))*
            (1.0-2.*PI*PI*fpeak*fpeak*(t1-t0)*(t1-t0));
    }
}


/*****************************************************************************/

/* second ricker wavelet */

void ricker2_wavelet (int hlw, float dt, float period, float ampl, 
    float distort, float *wavelet)
/******************************************************************************
Compute Ricker wavelet
*******************************************************************************
Input:
hlw         half-length of the wavelet including center (samples)
dt          sampling interval (s)
period      wavelet period (s)
ampl        wavelet amplitude
distort     wavelet distortion factor

Output:
wavelet     Ricker wavelet
*******************************************************************************
Notes:
Creates a sampled version of the Ricker wavelet #2?? in (Ricker, N., 195?).
*******************************************************************************
Author:  Ken Larner, Colorado School of Mines, 02/26/90
Modified:
******************************************************************************/
{   
    float t=0., tnorm, xsq, wsym;
    int j;
        
    for (j=0; j<hlw; j++)   
    {   
        tnorm    = t/period;
        xsq      = (2.5*tnorm)*(2.5*tnorm);
        wsym     = ampl*(1.0 - 2.0*xsq)*exp(-xsq);
        wavelet[hlw+j-1] = wsym*(1.0 - 2.0*distort*tnorm);
        wavelet[hlw-j-1] = wsym*(1.0 + 2.0*distort*tnorm);
        t        = t + dt;  
    }
}


void akb_wavelet (int nt, float dt, float fpeak, float *wavelet)
/******************************************************************************
 akb_wavelet -- Compute the time response of a source function as
    a wavelet based on a wavelet used by Alford, Kelly, and Boore.
*******************************************************************************
 Input: 
    int nt      number of samples in output wavelet
    float dt    time step
    float fpeak peak frequency of the wavelet
    
*******************************************************************************
 Output: 
    float wavelet   array[nt] of computed wavelet

*******************************************************************************
 Reference:
    Alford, R., Kelly, K., and Boore, D., 1974,
    Accuracy of finite-difference modeling of the acoustic wave
    equation: Geophysics, vol. 39, p. 834-842.

 The waveform here differs from the one in Alford, Kelly, and Boore in
 that there is a time shift and an arbitrary amplitude scaling factor of 60.
*******************************************************************************
 Author: Tong Fei, 1993, Colorado School of Mines.
******************************************************************************/
{
    int it;
    float   t1; 
    float   t0=1.8/fpeak;

    for (it=0; it<nt; it++) {
        t1=it*dt;
        wavelet[it] = -60.0*(t1-t0)*exp(-2.0*fpeak*fpeak
            *(t1-t0)*(t1-t0));
    }
}


void spike_wavelet (int nt, int tindex, float *wavelet)
/******************************************************************************
spike_wavelet -- Compute the time response of a source function as
    a spike.    
*******************************************************************************
Input: 
    int nt          number of time step
    int tindex      time index to locate the spike
    
Output: 
    float wavelet   array[nt] of computed wavelet

*******************************************************************************
 Author: Tong Fei, 1993, Colorado School of Mines.
******************************************************************************/
{
    int it;

    for (it=0; it<nt; it++) {
        wavelet[it] = 0.0;
    }

    wavelet[tindex]=1.0;
}


void unit_wavelet (int nt, float *wavelet)
/******************************************************************************
unit_wavelet -- Compute the time response of a source function as
    a constant unit shift.  
*************I*****************************************************************
Input: 
    int nt          number of samples in output wavelet
    
*******************************************************************************
Output: 
    float wavelet   array[nt] of computed wavelet

*******************************************************************************
 Author: Tong Fei, 1993, Colorado School of Mines.
******************************************************************************/
{
    int it;

    for (it=0; it<nt; it++) {
        wavelet[it] = 1.0;
    }

}


void zero_wavelet (int nt, float *wavelet)
/******************************************************************************
zero_wavelet -- Compute the time response of a source function as
    zero everywhere.    
*******************************************************************************
Input: 
    int nt          number of samples in output wavelet
    
*******************************************************************************
Output: 
    float *wavelet  array[nt] of computed wavelet

*******************************************************************************
Author: Tong Fei, 1993, Colorado School of Mines.
******************************************************************************/
{
    int it;

    for (it=0; it<nt; it++) {
        wavelet[it] = 0.0;
    }

}


void berlage_wavelet (int nt, float dt, float fpeak, float ampl, float tn, \
    float decay, float ipa, float *wavelet)
/************************************************************************
berlage_wavelet -- Compute the time response of a source function as a
    Berlage wavelet with peak frequency "fpeak" Hz, exponential decay
    factor "decay", time exponent "tn", and initial phase angle "ipa".
*************************************************************************
Input:
nt         number samples in output wavelet
dt         time step
fpeak      peak frequency of the Berlage wavelet
ampl       wavelet amplitude
tn         non-negative time exponent (typically an integer number)
decay      non-negative exponential decay factor
ipa        initial phase angle in radians

Output:
wavelet    Berlage wavelet

*************************************************************************
References:
Aldridge, D. F. (1990). The Berlage wavelet.
    Geophysics, vol. 55(11), p. 1508-1511, doi:10.1190/1.1442799.
Berlage, A. J. (1932).
    Seismometer: Handbuch der Geophysik, vol. 4, p. 299-526.
*************************************************************************
Author: Nils Maercklin, July 2006
*************************************************************************/
{
    register int it;
    float t;

    for (it=0;it<nt;it++) {
        t = dt * (float)it;
        wavelet[it] = ampl * pow(t,tn) * exp(-decay*t) * \
            cos(2.0*PI*fpeak*t + ipa);
    }
}


void gaussian_wavelet (int nt, float dt, float fpeak, float *wavelet)
/************************************************************************
gaussian_wavelet -- Compute the time response of a source function as a
    Gaussian wavelet with peak frequency "fpeak" in Hz.
*************************************************************************
Input:
nt         number samples in output wavelet
dt         time step
fpeak      peak frequency of the Gaussian wavelet

Output:
wavelet    Gaussian wavelet

*************************************************************************
Author: Nils Maercklin, Oct. 2006
*************************************************************************/
{
    register int it;
    float t,t0,s;

    t0 = 1.0 / fpeak;
    s  = 1.0 / (sqrt(2.0)*PI*fpeak);
    
    for (it=0;it<nt;it++) {
        t = dt * (float)it;
        wavelet[it] = 1.0/(s*sqrt(2.0*PI)) * exp(-(t-t0)*(t-t0)/(2.0*s*s));
    }
}


void gaussderiv_wavelet (int nt, float dt, float fpeak, float *wavelet)
/************************************************************************
gaussderiv_wavelet -- Compute the time response of a source function as a
    Gaussian first derivative wavelet with peak frequency "fpeak" in Hz.
*************************************************************************
Input:
nt         number samples in output wavelet
dt         time step
fpeak      peak frequency of the Gaussian wavelet

Output:
wavelet    Gaussian wavelet, first derivative

*************************************************************************
Author: Nils Maercklin, Oct. 2006
Credit: adopted from a code by Dominique Rousset
*************************************************************************/
{
    register int it;
    float t,t0,s;

    t0 = 1.0 / fpeak;
    s  = 1.0 / (sqrt(2.0)*PI*fpeak);
    
    for (it=0;it<nt;it++) {
        t = dt * (float)it;
        wavelet[it] = -1.0 /(s*s*s*sqrt(2.0*PI)) * (t-t0) * \
                      exp(-(t-t0)*(t-t0)/(2.0*s*s));
    }
}

void
deriv_n_gauss(double dt, int nt, double t0, float fpeak, int n, double *w,
	    int sign, int verbose)
/****************************************************************************
  deriv_n_gauss:  Compute n-th order derivative of a Gaussian 
                    in double precision.
*****************************************************************************
Input:
dt		sampling interval
nt		length of waveform in samples
t0		time shift for (pseudo-) causality
fpeak		maximum frequency
n		order of derivative
sign		multiplier for polarity of waveform
verbose		flag for diagnostic messages
*****************************************************************************
Output:
w	array of size nt containing the waveform
*****************************************************************************
Return: none
*****************************************************************************
Notes:
Copyright (c) 2007 by the Society of Exploration Geophysicists.
For more information, go to http://software.seg.org/2007/0004 .
You must read and accept usage terms at:
http://software.seg.org/disclaimer.txt before use.
*****************************************************************************
Author: Werner M. Heigl, Apache Corporation, E&P Technology, November 2006
*****************************************************************************/
{
	int i;			/* loop variable			*/
	double sigma;		/* temporal variance of Gaussian	*/
	double C;		/* normalization constant		*/
	double *h  = NULL;	/* Hermite polynomial			*/
	double *h0 = NULL;	/* temp array for H_{n-1}		*/
	double *h1 = NULL;	/* temp array for H_{n}			*/
	double *t  = NULL;	/* time vector				*/

	
	/* allocate & initialize memory */
	t  = alloc1double(nt);
	h  = alloc1double(nt);
	h0 = alloc1double(nt);
	h1 = alloc1double(nt);

	memset((void *) t , 0, DSIZE * nt);
	memset((void *) h , 0, DSIZE * nt);
	memset((void *) h0, 0, DSIZE * nt);
	memset((void *) h1, 0, DSIZE * nt);
	if (verbose)
		fprintf(stderr,"memory allocated and initialized/n");

	/* initialize time vector */
	for (i = 0; i < nt; ++i)	t[i] = i * dt - t0;
	if (verbose)	fprintf(stderr,"t[] initialized/n");
	
	/* compute Gaussian */
	sigma = n / ( 4 * PI * PI * fpeak * fpeak );
	if (verbose)	fprintf(stderr,"sigma=%f",sigma);
	for (i = 0; i < nt; ++i)
		w[i] = exp( - t[i] * t[i] / (2 * sigma) );
	if (verbose)	fprintf(stderr,"Gaussian computed/n");
	
	/* compute Hermite polynomial */
	for (i = 0; i < nt; ++i) {
		h0[i] = 1.0;
		h1[i] = t[i] / sigma;
	}
	if (n==1)	memcpy((void *) h, (const void *) h1, DSIZE * nt);
	if (n>1)	hermite_n_polynomial(h, h0, h1, t, nt, n, sigma);
	if (verbose)	fprintf(stderr,"Hermite polynomial H_%d computed/n",n);
	
	/* compute waveform */
	for (i = 0; i < nt;++i)		w[i] = h[i] * w[i];
	if (verbose)	fprintf(stderr,"waveform computed/n");
	
	/* find normalization constant */
	C = fabs(w[0]);
	for (i = 1; i < nt; ++i)
		if (fabs(w[i]) > C)	C = fabs(w[i]);
	if (ISODD(n))	C = -C;	/* to account for (-1)^n */
	if (verbose)	fprintf(stderr,"C=%f/n",C);
	
	/* and finally normalize */
	for (i = 0; i < nt; ++i)	w[i] = sign * w[i] / C;
	if (verbose)	fprintf(stderr,"waveform normalized/n");

	/* check amplitude a t=0 */
	if (verbose)	fprintf(stderr,"w[o]=%.12f",w[0]);
	
	/* free memory */
	free1double(h); free1double(h0); free1double(h1);
	free1double(t);
	if (verbose)	fprintf(stderr,"memory freed/n");
}	
