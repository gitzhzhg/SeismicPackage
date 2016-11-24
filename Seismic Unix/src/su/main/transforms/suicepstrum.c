/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUICEPSTRUM: $Revision: 1.2 $ ; $Date: 2013/06/26 23:10:21 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUICEPSTRUM - fft of complex log frequency traces to real time traces",
" 								",
"  suicepstrum <stdin >sdout sign2=-1				",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameter:						",
" 	sign1=1		sign in exponent of first fft		",
" 	sign2=-1	sign in exponent of inverse fft		",
"	sym=0		=1 center  output 			",
"	dt=tr.dt	time sampling interval (s) from header	",
"			if not set assumed to be .004s		",
" Output traces are normalized by 1/N where N is the fft size.	",
" 								",
" Note:								",
" The forward  cepstral transform is the			",
"   F(t_c) = InvFFT[ln[FFT(F(t))]] 				",
" The inverse  cepstral transform is the			",
"   F(t) = InvFFT[exp[FFT(F(t_c))]] 				",
" 								",
" Here t_c is the cepstral time (quefrency) domain 		",
NULL};

/* Credits:
 * 
 *   CWP: John Stockwell, Dec 2010 based on
 *     suifft.c by:
 *	CWP: Shuki Ronen, Chris Liner, Jack K. Cohen,  c. 1989
 *
 * Trace header fields accessed: ns, trid
 * Trace header fields modified: ns, trid
 */
/**************** end self doc ***********************************/


#define LOOKFAC	2	/* Look ahead factor for npfaro	  */
#define PFA_MAX	720720		/* Largest allowed fft	*/

segy tr;

int
main(int argc, char **argv)
{
	register complex *ct=NULL;	/* complex input trace		*/
	register float *rt=NULL;	/* real output trace		*/
	register float *newrt=NULL;	/* real output trace		*/
	int ns;			/* number of samples per trace		*/
	int newns;		/* number of samples per trace		*/
	int nfft;		/* fft size 				*/
	int newnfft;		/* fft size 				*/
	int nf;			/* number of frequencies		*/
	int newnf;		/* number of frequencies		*/
	int sign1;		/* sign in exponent of FFT		*/
	int sign2;		/* sign in exponent of INV FFTtransform	*/
	float onfft;		/* 1.0/nfft				*/
	float onewnfft;		/* 1.0/nfft				*/
	float dt;		/* time sampling interval sec		*/
	float d1;		/* reset time domain sampling		*/

	int sym=0;		/* =1 symmetric output			*/
	int verbose=0;		/* silent =1 chatty 			*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	if (!getparint("verbose", &verbose)) verbose = 0;


	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	ns = tr.ns;

	if (!getparfloat("dt", &dt)) 	dt = ((double) tr.dt/1000000.0);
	if (!dt){
		if (verbose) warn("dt header field not set!");
		dt = ((double) tr.d1);
	}

	/* if dt is not set in the header or getpar */
	if (!dt) dt = .004;


	/* Set up pfa fft */
	nfft = npfaro(ns, LOOKFAC * ns);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)  err("Padded ns=%d--too big", nfft);
	nf = nfft/2 + 1;
	d1 = 1.0/(nfft*dt);
	onfft=1.0/nfft;



	/* Set sign in exponent of transform */
	if (!getparint   ("sym", &sym)) sym = 0;
	if (!getparint   ("sign1", &sign1)) sign1 = 1;
	if (!getparint   ("sign2", &sign2)) sign2 = -1;

	if (verbose) warn("sign1 = %d sign2 = %d",sign1,sign2);

        checkpars();

	if (sign1 != 1 && sign1 != -1) err("sign1 = %d must be 1 or -1", sign1);
	if (sign2 != -1 && sign2 != 1) err("sign2 = %d must be -1 or 1", sign2);


	/* Allocate fft arrays */
	rt   = ealloc1float(nfft);
	ct   = ealloc1complex(nf);


	/* If dt not set, issue advisory on frequency step d1 */
        if (dt && verbose)  warn("d1=%f", 1.0/(nfft*dt));

	/* see  sufft */
	newns = 2*nf;
	newnfft = newns - 2;
	newnf = newnfft/2 + 1;
	onewnfft = 1.0/newnfft;

	newrt = ealloc1float(newnfft);

	/* Main loop over traces */
	do {
		register int i;

		/* zero out arrays */
		memset( (void *) ct, 0, nf*sizeof(complex));

		/* Load trace into rt (zero-padded) */
		memcpy((void *) rt, (const void *) tr.data, ns*FSIZE);
		memset((void *) (rt + ns), 0, (nfft-ns)*FSIZE);


		/* FFT */
		pfarc(sign1, nfft, rt, ct);

		
		/* Load traces into ct (pfa fills in negative freqs) */
		/* exponentiate prior to inverse transform */
		for (i = 0; i < nf; ++i) {
			register float temp=0.0;

			temp=ct[i].r;

			if (temp) {

				ct[i].r = (float) exp(temp) * cos(ct[i].i);
				ct[i].i = (float) exp(temp) * sin(ct[i].i);


				/* if sym=1 then flip the sign on every */
				/* other value to make output symmetric */
                        	if (sym) {
                                	if (ISODD(i)) {
                                        	ct[i].r = -ct[i].r;
                                        	ct[i].i = -ct[i].i;
                                	}
                        	}
     
					
			} else {
			
					ct[i].r = 0.0;
					ct[i].i = 0.0;
				
			}



		}

		memset( (void *) newrt, 0, newnfft*FSIZE);
		/* Inverse FFT */
		pfacr(sign2, newnf, ct, newrt);

		/* Load back into tr.data and prepare to write output trace */
		for (i = 0; i < newnfft; ++i)  {
			tr.data[i] = newrt[i] * onewnfft;
		}

		tr.trid = TREAL;
		tr.ns = newnfft;
		tr.f1 = 0.0f;
		tr.d1 = dt;
		tr.dt = (int)(dt*1000000.0);

		if (sym) tr.delrt=-(tr.delrt + (newnfft*tr.d1*1000.0)/2.0);

		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
