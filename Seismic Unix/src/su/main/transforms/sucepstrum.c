/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUCEPSTRUM: $Revision: 1.10 $ ; $Date: 2015/08/07 22:34:05 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUCEPSTRUM - transform to the CEPSTRal domain				",
" 									",
"  sucepstrum <stdin >sdout sign1=1 					",
" 									",
" Required parameters:							",
" none									",
" 									",
" Optional parameters:							",
" sign1=1			sign in exponent of fft			",
" sign2=-1			sign in exponent of ifft		",
" dt=from header		sampling interval			",
" verbose=1			=0 to stop advisory messages		",
" 									",
" .... phase unwrapping options .....				   	",
" mode=ouphase	Oppenheim's phase unwrapping				",
"		=suphase simple jump detecting phase unwrapping		",
" unwrap=1       |dphase| > pi/unwrap constitutes a phase wrapping	",
" 	 	=0 no phase unwrapping	(in mode=suphase  only)		",
" trend=1	remove linear trend from the unwrapped phase	   	",
" zeromean=0     assume phase(0)=0.0, else assume phase is zero mean	",
" smooth=0      apply damped least squares smoothing to unwrapped phase ",
" r=10     ... damping coefficient, only active when smooth=1           ",
" 									",
" Notes:								",
" The complex log fft of a function F(t) is given by:			",
" clogfft(F(t)) = log(FFT(F(t)) = log|F(omega)| + iphi(omega)		",
" where phi(omega) is the unwrapped phase. Note that 0< unwrap<= 1.0 	",
" allows phase unwrapping to be tuned, if desired. 			",
" 									",
" The ceptrum is the inverse Fourier transform of the log fft of F(t) 	",
" F(t_c) =cepstrum(F(t)) = INVFFT[log(FFT(F(t))]			",
"                        =INVFFT[log|F(omega)| + iphi(omega)]		",
" Here t_c is the cepstral time domain. 				",
" 									",
" To facilitate further processing, the sampling interval		",
" in quefrency and first quefrency (0) are set in the			",
" output header.							",
" 									",
" Caveats: 								",
" No check is made that the data ARE real time traces!			",
" 									",
" Use suminphase to make minimum phase representations of signals 	",
NULL};

/* Credits:
 *      CWP: John Stockwell, June 2013 based on
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
	register float *rt=NULL;	/* real trace			*/
	register complex *ct=NULL;	/* complex transformed trace	*/
	register complex *clogfft=NULL;/* the clogfft domain form of a trace*/
	register float *cepstrum=NULL;/* the clogfft domain form of a trace*/
	int nt;			/* number of points on input trace	*/
	int nfft;		/* transform length			*/
	int nifft=0;		/* inverse transform length		*/
	float onifft=0.0;	/* one over inverse transform length	*/
	int nf;			/* number of frequencies		*/
	int sign1;		/* sign in exponent of 1st transform	*/
	int sign2;		/* sign in exponent of 2nd transform	*/
	int verbose;		/* flag to get advisory messages	*/
	float dt;		/* sampling interval in secs		*/
	float d1;		/* output sample interval in Hz		*/
	float newd1;		/* output sample interval in Hz		*/
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

        /* smoothing */
        int smooth=0;   /* =1 perform least squares damping */
        float r=0;      /* smoothing parameter, active only for smooth=1 */



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

         /* get smoothing */
        if(!getparint("smooth",&smooth))         smooth=0;
        if(!getparfloat("r",&r))    			r=10.0;           

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

	if (!getparint("sign1", &sign1)) sign1 = 1;
	if (!getparint("sign2", &sign1)) sign2 = -1;
	if (!getparfloat("unwrap", &unwrap))	unwrap = 1.0;
	if (sign1 != 1 && sign1 != -1)   err("sign1 = %d must be 1 or -1", sign1);
	if (sign2 != -1 && sign2 != 1)   err("sign2 = %d must be -1 or 1", sign2);


        checkpars();

	rt = ealloc1float(nfft);
	ct = ealloc1complex(nf);
	phase = ealloc1float(nf);
	real = ealloc1float(nf);
	imag = ealloc1float(nf);
	clogfft = ealloc1complex(nf);


	/* If dt not set, issue advisory on frequency step d1 */
	if (dt && verbose)  warn("d1=%f", 1.0/(nfft*dt));



	/* Main loop over traces */
	do {
		register int i;
	
		/* zero out arrays */
		memset((void *) phase, 0,nf*FSIZE);
		memset((void *) real, 0,nf*FSIZE);
		memset((void *) imag, 0,nf*FSIZE);
		memset((void *) clogfft, 0,nf*CSIZE);

		/* Load trace into rt (zero-padded) */
		memcpy((void *) rt, (const void *) tr.data, nt*FSIZE);
		memset((void *) (rt + nt), 0, (nfft-nt)*FSIZE);


		/* FFT */
		pfarc(sign1, nfft, rt, ct);


		/* calculate the amplitude and phase 		*/
		for (i = 0; i < nf; ++i) {
			float re=ct[i].r;
			float im=ct[i].i;
			float ampsqrd = re*re + im*im;
			
			/* calculate amplitude and phase */
			if (!CLOSETO(ampsqrd,0.0)) {
					clogfft[i].r = (float) 0.5*log(ampsqrd);
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
			float *xout=NULL;


			/* smoothing array of x values */
                        xout = alloc1float(nf);
                        if (smooth) for (i=0; i<nf; ++i) xout[i] = i;

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

                	/* smooth */
                	if (smooth) smooth_1(xout,phase,r,nf);
		}


		/* write unwrapped phase into imaginary part of output data */
		for (i = 0; i < nf; ++i) {
			clogfft[i].i = phase[i];
		}



		/* prepare for inverse fourier transform */
		nifft = 2*nf - 2;  /* see suifft */
		onifft= (float) 1.0/nifft;

	       if(tr.d1) {
       		     newd1 = onifft/tr.d1;
        	} else {
           		if(tr.dt) newd1 = (float) (((double) tr.dt)/1000000.0);
           	else newd1 = 0.0f;
        	}

		cepstrum = ealloc1float(nifft);
		memset((void *) cepstrum, 0,nifft*FSIZE);
		

		/* Inverse FFT to go from logfft to cepstral domain */
		pfacr(sign2, nifft, clogfft, cepstrum);

		/* load the cepstrum into the output trace */
		/* Load back and scale for inverse fft */
                for (i = 0; i < nifft; ++i)  tr.data[i] = cepstrum[i] * onifft;

		/* Set header values--npfaro makes nfft even */
		tr.ns = nifft;
		tr.trid = TREAL ;
		tr.d1 = newd1;
		tr.f1 = 0.0;

		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
