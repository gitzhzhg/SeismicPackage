/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUSLOWIFT - Fourier Transforms by (SLOW) DFT algorithm (Not an FFT)",
"             complex frequency to real time domain traces 	",
" 								",
" suslowift <stdin >sdout sign=-1 				",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	sign=-1			sign in exponent of fft		",
" 	dt=from header		sampling interval		",
" 								",
" Trace header fields accessed: ns, dt				",
" Trace header fields modified: ns, dt, trid			",
" 								",
" Notes: To facilitate further processing, the sampling interval",
"       in frequency and first frequency (0) are set in the	",
"	output header.						",
" 								",
" Warning: This program is *not* fft based. Use only for demo 	",
" 	   purposes, *not* for large data processing.		",
" 								",
" 	No check is made that the data are real time traces!	",
" 								",
NULL};

/* Credits:
 *
 *	CWP: John Stockwell c. 1993
 *       based on suifft:   Shuki Ronen, Chris Liner, Jack K. Cohen
 *
 * Note: leave dt set for later inversion
 *
 */
/**************** end self doc ***********************************/

/* Prototype of function used internally */
void dftcr (int sign, int nsamp, float *re, complex *ct);

segy tr;

int
main(int argc, char **argv)
{
	register float *rt;	/* real trace				*/
	register complex *ct;	/* complex transformed trace		*/
	int nt;			/* number of points on input trace	*/
	int sign;		/* sign in exponent of transform	*/
	float nfft;		/* fft size				*/
	float dt;		/* sampling interval in secs		*/
	float d1;		/* output sample interval in Hz		*/
	float twonfft;		/* 2/nfft 				*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;
	nfft=nt;
	twonfft=2/nfft;

	/* dt is used only to set output header value d1 */
	if (!getparfloat("dt", &dt)) dt = (float) tr.dt/1000000.0;
	if (!dt) {
		dt = .004;
		warn("dt not set, assumed to be .004");
	}

	d1 = 1.0/(nt*dt);

	if (!getparint("sign", &sign)) sign = -1;
	if (sign != 1 && sign != -1)   err("sign = %d must be 1 or -1", sign);

        checkpars();


	/* allocate space */
	rt = ealloc1float(nfft);
	ct = ealloc1complex(nfft);

	/* Echo frequency step size (Hz) to par file */
	if (dt)  warn("d1=%f f1=0.0", 1.0/(nt*dt));

	/* Main loop over traces */
	do {
		register int i;

		/* Load values into ct */
		for (i = 0; i < nfft; ++i) {
                        ct[i].r = tr.data[2*i];
                        ct[i].i = tr.data[2*i + 1]; 
		}

		/* FFT */
		dftcr(sign, nfft, rt, ct);

		/* normalize */
		

		/* Load values into tr.data */
		for (i = 0; i < nfft; ++i) {
			tr.data[i] = rt[i]*twonfft;
		}

		/* Set header values */
		tr.trid = TREAL;
		tr.ns = nt;
		/*tr.d1 = d1;*/
		tr.d1 = 0; /* wmh: to get times correct on plots */
		tr.f1 = 0.0;

		puttr(&tr);

	} while (gettr(&tr));

	return EXIT_SUCCESS;
}

void dftrc (int sign, int nsamp, float *re, complex *out)
/*
 * Digital Fourier Transform --- (Not an FFT) don't use if speed is an issue.
 *
 * Author: John Stockwell, 26 Feb 1992, based on the well known algorithm
 *
 * 	See: Numerical Recipes in C, p.406 or other 
 */

{
	int ki,ni;				/* looping variables 	*/
	float twopibynsamp=0.0;			/* 2 * PI/nsamp */
	float *tmpre;
	float *tmpim;

	/* allocate space for temporary arrays */
	tmpre = ealloc1float(nsamp);
	tmpim = ealloc1float(nsamp);

	/* zero out arrays */
	for (ni=0; ni<nsamp; ++ni) {
		tmpim[ni] = 0.0;
		tmpre[ni] = 0.0;
		out[ni] = cmplx(0.0,0.0);
	}
	
	twopibynsamp=2*PI/nsamp;

	/* do the dft assuming that the input are real data only */
	for(ni=0; ni<nsamp; ++ni) {
		register float retemp = 0.0;
		register float imtemp = 0.0;

		for(ki=0; ki<nsamp; ++ki) {
			register float tmpcos=0.0;
			register float tmpsin=0.0;

			tmpcos = cos(sign*twopibynsamp*ki*ni);
			tmpsin = sin(sign*twopibynsamp*ki*ni);

			retemp = retemp + re[ki]*(tmpcos - tmpsin);
			imtemp = imtemp + (re[ki]*tmpsin);
		}
		tmpre[ni] = retemp;
		tmpim[ni] = imtemp;
	}

	/* prepare new values of re[] and im[] */
	for(ni=0; ni<nsamp; ni++) {
		out[ni].r=tmpre[ni];
		out[ni].i=tmpim[ni];
	}

}

void dftcr (int sign, int nsamp, float *re, complex *ct)
/*
 * Digital Fourier Transform --- (Not an FFT) don't use if speed is an issue.
 *
 * Author: John Stockwell, 26 Feb 1992, based on the well known algorithm
 *
 * 	See: Numerical Recipes in C, p.406 or other 
 */
{
	int ki,ni;		/* looping variables 	*/
	float freq,phase;	/* argument to cos and sin */
	float twopibynsamp=0.0;	/* 2 * PI/nsamp */
	float *tmpre;
	float *tmpim;

	/* allocate space for temporary arrays */
	tmpre = ealloc1float(nsamp);
	tmpim = ealloc1float(nsamp);

	/* zero out arrays */
	for (ni=0; ni<nsamp; ++ni) {
		tmpim[ni] = 0.0;
		tmpre[ni] = 0.0;
		re[ni] =0.0; 
	}
	
	twopibynsamp = sign*2*PI/nsamp;
	freq = phase = 0.0;

	/* do the dft assuming that the input are complex data only */
	for(ni=0; ni<nsamp; ++ni) {
		register float retemp = 0.0;
		register float imtemp = 0.0;

		for(ki=0; ki<nsamp; ++ki) {
			register float tmpcos=0.0;
			register float tmpsin=0.0;

			phase = freq*ki;

			tmpcos = cos(twopibynsamp*ki*ni);
			tmpsin = sin(twopibynsamp*ki*ni);

			retemp += ct[ki].r*tmpcos-ct[ki].i*tmpsin;
			imtemp += ct[ki].i*tmpcos+ct[ki].r*tmpsin;
		}
		tmpre[ni] = retemp;
		tmpim[ni] = imtemp;

		phase = 0.0;
		freq += twopibynsamp; /* saves a float mult */
	}

	/* prepare new values of re[] and im[] */
	for(ni=0; ni<nsamp; ni++) {
		re[ni]=tmpre[ni];
	}

}
