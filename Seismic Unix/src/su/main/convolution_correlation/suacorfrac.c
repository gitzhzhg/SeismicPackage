/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUACORFRAC: $Revision: 1.3 $ ; $Date: 2011/11/16 17:37:27 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUACORFRAC -- general FRACtional Auto-CORrelation/convolution		",
"									",
" suacorfrac power= [optional parameters] <indata >outdata 		",
"									",
" Optional parameters:							",
" a=0			exponent of complex amplitude	 		",
" b=0			multiplier of complex phase	 		",
" dt=(from header)	time sample interval (in seconds)		",
" verbose=0		=1 for advisory messages			",
" ntout=tr.ns		number of time samples output			",
" sym=0			if non-zero, produce a symmetric output from	",
"			lag -(ntout-1)/2 to lag +(ntout-1)/2		",
" Notes:								",
" The calculation is performed in the frequency domain.			",
" The fractional autocorrelation/convolution is obtained by raising	",
" Fourier coefficients to seperate real powers 				",
"		(a,b) for amp and phase:				",
"		     Aout exp[-i Pout] = Ain Ain^a exp[-i (1+b) Pin] 	",
"		where A=amplitude  P=phase.				",
" Some special cases:							",
"		(a,b)=(1,1)	-->	auto-correlation		",
"		(a,b)=(0.5,0.5)	-->	half-auto-correlation		",
"		(a,b)=(0,0)	-->	no change to data		",
"		(a,b)=(0.5,-0.5)-->	half-auto-convolution		",
"		(a,b)=(1,-1)	-->	auto-convolution		",
"									",
NULL};

/* Credits:
 *	UHouston: Chris Liner, Sept 2009
 *	CWP: Based on Hale's crpow
 *
 * Trace header fields accessed: ns, dt, trid, d1
*/
/**************** end self doc ***********************************/

/* function prototypes */
complex dopow(complex u, float a, float b);

#define	I		cmplx(0.0, 1.0)
#define	PIBY2		0.5 * PI
#define TWOPI		2.0 * PI
#define LOOKFAC		2	/* Look ahead factor for npfao	  */
#define PFA_MAX		720720	/* Largest allowed nfft		  */

segy tr;

int
main(int argc, char **argv)
{
	float a, b;		/* powers for amp and phase		*/
	register float *rt=NULL;/* real trace				*/
	register complex *ct=NULL;	/* complex transformed trace		*/
	complex filt;		/* pow'd input at one frequency	 	*/
	int nt;			/* number of points on input trace	*/
	size_t ntsize;		/* nt in bytes				*/
	float dt;		/* sample spacing (secs) on input trace	*/
	int nfft;		/* number of points in nfft		*/
	int nf;		 	/* number of frequencies (incl Nyq)     */
	float onfft;		/* 1 / nfft				*/
	int verbose;		/* flag to get advisory messages	*/
	size_t nzeros;		/* number of padded zeroes in bytes	*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	int ntout, sym;		/* output params			*/
	
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Set parameters */
	if (!getparint("verbose", &verbose))	  verbose  =  0;
	if (!getparfloat("a", &a))	  a = 0.0;
	if (!getparfloat("b", &b))	  b = 0.0;
	if (!getparint("sym",&sym)) 	  sym = 0;

	/* Get info from first trace */
	if (!gettr(&tr))	err("can't get first trace");
	seismic = ISSEISMIC(tr.trid);
	if (seismic) {
		if (verbose)	warn("input is seismic data, trid=%d",tr.trid);
		dt = ((double) tr.dt)/1000000.0;
	}
	else {
		if (verbose)	warn("input is not seismic data, trid=%d",tr.trid);
		dt = tr.d1;
	}
	if (!dt)	err("dt or d1 field is zero and not getparred");
	nt = tr.ns;
	ntsize = nt * FSIZE;

	if (!getparint("ntout",&ntout))   ntout=tr.ns;
        checkpars();

	/* Set up for fft 
	   extra 2 in nfft is to avoid wrap around */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
		err("Padded nt=%d -- too big", nfft);

	nf = nfft/2 + 1;
	onfft = 1.0 / nfft;
	nzeros = (nfft - nt) * FSIZE;

	/* Allocate fft arrays */
	rt   = ealloc1float(nfft);
	ct   = ealloc1complex(nf);

	
	/* Loop over traces */
	do {
		/* Load trace into rt (zero-padded) */
		memcpy( (void *) rt, (const void *) tr.data, ntsize);
		memset((void *) (rt + nt), 0, nzeros);

		/* FFT */
		pfarc(1, nfft, rt, ct);

		/* Apply filter */
		{ register int i;
			for (i = 0; i < nf; ++i) {

				filt = dopow(ct[i], a, b);
				ct[i] = cmul(ct[i], filt);

				/* symmetric output: flip sign of odd values */
				if (sym){
					if (ISODD(i)) {
						ct[i].r = -ct[i].r;
						ct[i].i = -ct[i].i;
					}
				}

			}
		}

		/* Invert */
		pfacr(-1, nfft, ct, rt);

		/* Load traces back in */
		{ register int i;
		for (i = 0; i < nt; ++i)  tr.data[i] = rt[i];
		}

		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}

/************************************************************************
dopow - raise a complex number to seperate real powers for amp and phase
*************************************************************************
Notes:

Aout exp[-i Pout] = Ain^a exp[-i b Pin] 
     where A=amplitude  P=phase
*************************************************************************
UHouston: Chris Liner: modified from D. Hale subroutine crpow
*************************************************************************/
complex dopow(complex u, float a, float b)
{
	float ur,ui,amp,phs;

	if (a==0.0) return cmplx(1.0,0.0);
	if (u.r==0.0 && u.i==0.0) return cmplx(0.0,0.0);

	ur = u.r; 
	ui = u.i;
	amp = exp(0.5*a*log(ur*ur+ui*ui));
	phs = -b*atan2(ui,ur);

	return cmplx(amp*cos(phs),amp*sin(phs));	
}


