/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUCLOGFFT: $Revision: 1.11 $ ; $Date: 2013/05/31 18:31:19 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUCLOGFFT - fft real time traces to complex log frequency domain traces",
" 									",
" suclogftt <stdin >sdout sign=1 					",
" 									",
" Required parameters:							",
" none									",
" 									",
" Optional parameters:							",
" sign=1			sign in exponent of fft			",
" dt=from header		sampling interval			",
" verbose=1		=0 to stop advisory messages			",
" 									",
" .... phase unwrapping options .....				   	",
" mode=suphase	simple jump detecting phase unwrapping			",
" 		=ouphase  Oppenheim's phase unwrapping			",
" unwrap=1       |dphase| > pi/unwrap constitutes a phase wrapping	",
" 	 	=0 no phase unwrapping	(in mode=suphase  only)		",
" trend=1	remove linear trend from the unwrapped phase	   	",
" zeromean=0     assume phase(0)=0.0, else assume phase is zero mean	",
" 									",
" Notes:								",
" clogfft(F(t)) = log(FFT(F(t)) = log|F(omega)| + iphi(omega)		",
" where phi(omega) is the unwrapped phase. Note that 0< unwrap<= 1.0 	",
" allows phase unwrapping to be tuned, if desired. 			",
" 									",
" To facilitate further processing, the sampling interval		",
" in frequency and first frequency (0) are set in the			",
" output header.							",
" 									",
" suclogfft unwrap=0 | suiclogfft is not quite a no-op since the trace	",
" length will usually be longer due to fft padding.			",
" 									",
" Caveats: 								",
" No check is made that the data ARE real time traces!			",
" 									",
" Output is type complex. To view amplitude, phase or real, imaginary	",
" parts, use    suamp 							",
" PI/unwrap = minimum dphase is assumed to constitute a wrap in phase	",
" for suphase unwrapping only 					",
" 									",
" Examples: 								",
" suclogfft < stdin | suamp mode=real | .... 				",
" suclogfft < stdin | suamp mode=imag | .... 				",
" 									",
" The real and imaginary parts of the complex log spectrum are the	",
" respective amplitude and phase (unwrapped) phase spectra of the input	",
" signal. 								",
NULL};

/* Credits:
 *      CWP: John Stockwell, Dec 2010 based on
 *	sufft by:
 *	CWP: Shuki Ronen, Chris Liner, Jack K. Cohen
 *	CENPET: Werner M. Heigl - added well log support
 *	U Montana: Bob Lankston - added m_unwrap_phase feature
 *
 * Note: leave dt set for later inversion
 *
 * Trace header fields accessed: ns, dt, d1, f1
 * Trace header fields modified: ns, d1, f1, trid
 */
/**************** end self doc ***********************************/


#define LOOKFAC	2	/* Look ahead factor for npfaro	  */
#define PFA_MAX	720720	/* Largest allowed nfft		   */

#define SIMPLE 0
#define OPPENHEIM 1

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
	cwp_Bool seismic;	/* is this seismic data? 		*/
	float unwrap;		/* PI/unwrap = min dphase assumed to wrap */
	float *phase;		/* phase of signal			*/
	float nfby2;		/* nf/2.0				*/
	
	int trend;		/* trend=1 remove linear treand in */
				/*        unwrapped phase */
	int zeromean;		/* =0 assume phase(0)=0.0, else zeromean */

	cwp_String mode="ouphase"; /* mode =suphase or =ouphase for phase unwrap */
	int imode=OPPENHEIM;	/* integer mode flag			*/

	float *real=NULL; 	/* real  part */
	float *imag=NULL;	/* imag part */

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	if (!getparint("verbose", &verbose))	verbose=1;
	if (!getparint("trend", &trend))	trend=1;
	if (!getparint("zeromean", &zeromean))	zeromean=0;


	/* note mode is intialized to "ouphase" and */
	/* imode is intialized to OPPENHEIM */
	getparstring("mode",&mode);
	if (STREQ(mode, "suphase")) imode = SIMPLE;
	else if (!STREQ(mode, "ouphase"))
			err("unknown mode=\"%s\", see self-doc", mode);


	if (verbose) warn("mode = %s",mode);

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
	nfby2 = nf/2.0;

	if (!getparint("sign", &sign)) sign = 1;
	if (!getparfloat("unwrap", &unwrap))	unwrap = 1.0;
	if (sign != 1 && sign != -1)   err("sign = %d must be 1 or -1", sign);


        checkpars();

	rt = ealloc1float(nfft);
	ct = ealloc1complex(nf);
	phase = ealloc1float(nf);
	real = ealloc1float(nf);
	imag = ealloc1float(nf);


	/* If dt not set, issue advisory on frequency step d1 */
	if (dt && verbose)  warn("d1=%f", 1.0/(nfft*dt));



	/* Main loop over traces */
	do {
		register int i;
	
		/* zero out arrays */
		memset((void *) phase, 0,nf*FSIZE);
		memset((void *) real, 0,nf*FSIZE);
		memset((void *) imag, 0,nf*FSIZE);
		

		/* Load trace into rt (zero-padded) */
		memcpy((void *) rt, (const void *) tr.data, nt*FSIZE);
		memset((void *) (rt + nt), 0, (nfft-nt)*FSIZE);


		/* FFT */
		pfarc(sign, nfft, rt, ct);


		/* calculate the amplitude and phase 		*/
		for (i = 0; i < nf; ++i) {
			float re=ct[i].r;
			float im=ct[i].i;
			float ampsqrd = re*re + im*im;
			
			/* calculate amplitude and phase */
			if (!CLOSETO(ampsqrd,0.0)) {
					tr.data[2*i] = (float) 0.5*log(ampsqrd);
					phase[i] = atan2(im, re);
				} else {
					phase[i] = 0.0;
					tr.data[2*i] = 0.0;
					
				}
				real[i] = ct[i].r;
				imag[i] = ct[i].i;
	
		}
		/* unwrap phase */
		if (unwrap) {

			switch(imode) {
			case SIMPLE:
				simple_unwrap_phase(nf, trend, zeromean, unwrap, phase);
			break;
			case OPPENHEIM:

				memset((void *) phase, 0,nf*FSIZE);
				oppenheim_unwrap_phase(nf, trend, zeromean, d1, real, imag, phase);
			break;
			default:
				err("mysterious mode=\"%s\"", mode);

			}

		}

		/* write unwrapped phase into imaginary part of output data */
		for (i = 0; i < nf; ++i) {
			tr.data[2*i+1] = phase[i];
		}


		/* Set header values--npfaro makes nfft even */
		tr.ns = 2*nf;
		tr.trid = FUNPACKNYQ;
		tr.d1 = d1;
		tr.f1 = 0.0;

		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
