/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPHASE: $Revision: 1.4 $ ; $Date: 2011/11/16 18:03:07 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUPHASE - PHASE manipulation by linear transformation			",
" 									",
"  suphase  <stdin >sdout      						",
" 									",
" Required parameters:							",
" none									",
" Optional parameters:							",
" a=90			constant phase shift              		",
" b=180/PI              linear phase shift				",
" c=0.0			phase = a +b*(old_phase)+c*f;			",
" 									",
" Notes: 								",
" A program that allows the user to experiment with changes in the phase",
" spectrum of a signal.							",
NULL};

/**************** end self doc ********************************/

/* definitions used internally */
#define AMPSP(c) sqrt(c.r*c.r+c.i*c.i)
#define LOOKFAC	2	/* Look ahead factor for npfaro	  */
#define PFA_MAX	720720	/* Largest allowed nfft	          */

/* segy trace */
segy tr;

int
main(int argc, char **argv)
{
	float *rt=NULL;		/* real trace			*/
	float *amp=NULL;	/* amplitude spectra		*/
	float *ph=NULL;		/* phase			*/
	register complex *ct=NULL;	/* complex time trace	*/

	int nt;			/* number of points on input trace	*/
	int nfft;		/* transform length			*/
	int nf;			/* number of frequencies in transform	*/

	float dt;		/* sampling interval in secs		*/
	float d1;		/* output sample interval in Hz		*/
	int count=0;		/* counter				*/

	/* linear phase function */
	float a;		/* bias (intercept) of new phase	*/
	float b;		/* slope of linear phase function	*/
	float c;		/* new phase value			*/
	float onfft;		/* 1/nfft				*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;

	/* get parameters */
	/* dt is used only to set output header value d1 */
	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
	if (!dt) {
		dt = .004;
		warn("dt not set, assumed to be .002");
	}

	/* linear phase paramter values */
	if (!getparfloat("a", &a)) a = 0;
	if (!getparfloat("b", &b)) b = 180/PI;
	if (!getparfloat("c", &c)) c = 0.0;

	a *= PI/180.0;
	b *= PI/180.0;
	
	/* Set up pfa fft */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)  
		err("Padded nt=%d--too big", nfft);
	d1 = 1.0/(nfft*dt);
	nf = nfft/2 + 1;
        onfft = 1.0/nfft;
	 
	checkpars();

	/* Allocate space */
	rt = ealloc1float(nfft);
	ct = ealloc1complex(nf);
	amp = ealloc1float(nf);
	ph = ealloc1float(nf);

	/* Main loop over traces */
	count=0;
	do {
		register int i;
		
		/* Load trace into rt (zero-padded) */
		memcpy((void *) rt, (const void *) &tr.data, nt*FSIZE);
		memset((void *) (rt + nt), (int) '\0', (nfft-nt)*FSIZE);
		
		/* FFT */
		pfarc(1, nfft, rt, ct);
		for (i = 0; i < nf; ++i) {
			amp[i] = AMPSP(ct[i]);
			ph[i]  = a+b*atan2(ct[i].i,ct[i].r)+c*i;
		}
		for (i = 0; i < nf; ++i) {
			ct[i].r = amp[i]*cos(ph[i]);
			ct[i].i = amp[i]*sin(ph[i]);
		}
		pfacr(-1,nfft,ct,rt);
		for (i = 0; i < nt; ++i) rt[i]*=onfft;
		memcpy((void *) tr.data, (const void *) rt, nt*FSIZE);
		puttr(&tr);
		
	} while (gettr(&tr));

	return(CWP_Exit());
}
