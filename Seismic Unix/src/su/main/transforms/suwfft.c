/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUWFFT: $Revision: 1.4 $ ; $Date: 2011/11/16 23:35:04 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUWFFT - Weighted amplitude FFT with spectrum flattening 0->Nyquist	",
" 									",
" suwfft <stdin | suifft >sdout 					",
" 									",
" Required parameters:							",
" none									",
" 									",
" Optional parameters:							",
" w0=0.75		weight for AmpSpectrum[f-df]			",
" w1=1.00		weight for AmpSpectrum[f].. center value	",
" w2=0.75		weight for AmpSpectrum[f+df]			",
" 									",
" Notes: 								",
" 1. output format is same as sufft					",
" 2. suwfft | suifft is not quite a no-op since the trace		",
"    length will usually be longer due to fft padding.			",
" 3. using w0=0 w1=1 w2=0  gives truly flat spectrum, for other	        ",
"    weight choices the spectrum retains some of its original topograpy ",
" 									",
" Examples: 								",
" 1. boost data bandwidth to 10-90 Hz					",
"     suwfft < data.su | suifft | sufilter f=5,8,90,100 | suximage 	",
" 1. view amplitude spectrum after flattening				",
"     suwfft < data.su | suamp | suximage 				",
" 									",
" Caveat: The process of cascading the forward and inverse Fourier	",
"  transforms may result in the output trace length being greater than 	",
"  the input trace length owing to zero padding. The user may wish to	",
"  apply suwind to return the number of samples per trace to the original",
"  value:  Here NS is the number of samples per trace on the original data",
"  			... | suwind itmax=NS | ... 			",
NULL};

/* Credits:
 *
 *	UHouston: Chris Liner 
 *
 * Note: Concept from UTulsa PhD thesis of Bassel Al-Moughraby
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: ns, d1, f1, trid
 */
/**************** end self doc ***********************************/



#define LOOKFAC	2	/* Look ahead factor for npfaro	  */
#define PFA_MAX	720720	/* Largest allowed nfft	          */

segy tr;

int
main(int argc, char **argv)
{
	register float *rt;	/* real trace				*/
	register complex *ct;	/* complex transformed trace		*/
	int nt;			/* number of points on input trace	*/
	int nfft;		/* transform length			*/
	int nf;			/* number of frequencies		*/
	int sign;		/* sign in exponent of transform	*/
	int verbose;		/* flag to get advisory messages	*/
	float dt;		/* sampling interval in secs		*/
	float d1;		/* output sample interval in Hz		*/
	cwp_Bool seismic;	/* is this seismic data? */
	float c;		/* multiplier				*/
	float w0, w1, w2;	/* weights				*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	if (!getparint("verbose", &verbose))	verbose=1;

	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;

	/* check for seismic or well log data */
	seismic = ISSEISMIC(tr.trid);		
	if (seismic) {
		if (verbose)	warn("input is seismic data, trid=%d",tr.trid);
		dt = ((double) tr.dt)/1000000.0;
	}
	else {
		if (verbose)	warn("input is not seismic data, trid=%d",tr.trid);
		dt = tr.d1;
        }
	if (!dt) {
		dt = .004;
		if (verbose) warn("dt or d1 not set, assumed to be .004");
	}

	/* Set up pfa fft */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)  err("Padded nt=%d--too big", nfft);
	nf = nfft/2 + 1;
	d1 = 1.0/(nfft*dt);

	if (!getparint("sign", &sign)) sign = 1;
	if (sign != 1 && sign != -1)   err("sign = %d must be 1 or -1", sign);

	/* get weights */
	if (!getparfloat("w0",&w0)) w0 = 0.75;
	if (!getparfloat("w1",&w1)) w1 = 1.00;
	if (!getparfloat("w2",&w2)) w2 = 0.75;


        checkpars();

	rt = ealloc1float(nfft);
	ct = ealloc1complex(nf);

	/* If dt not set, issue advisory on frequency step d1 */
	if (dt && verbose)  warn("d1=%f", 1.0/(nfft*dt));

	/* Main loop over traces */
	do {
		register int i;

		/* Load trace into rt (zero-padded) */
		memcpy((void *) rt, (const void *) tr.data, nt*FSIZE);
		memset((void *) (rt + nt), (int) '\0', (nfft-nt)*FSIZE);

		/* FFT */
		pfarc(sign, nfft, rt, ct);

		/* Store values */
		for (i = 0; i < nf; ++i) {
			c =w0*rcabs(ct[i-1])+w1*rcabs(ct[i])+w2*rcabs(ct[i+1]);
			if (i==0 || i==nf) {
				tr.data[2*i]   = ct[i].r / rcabs(ct[i]);
				tr.data[2*i+1] = ct[i].i / rcabs(ct[i]);
			} else {
				tr.data[2*i]   = ct[i].r / c;
				tr.data[2*i+1] = ct[i].i / c;
			}
		}

		/* Set header values--npfaro makes nfft even */
		tr.ns = 2 * nf;
		tr.trid = FUNPACKNYQ;
		tr.d1 = d1;
		tr.f1 = 0.0;

		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
