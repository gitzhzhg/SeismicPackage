/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSPECFX: $Revision: 1.19 $ ; $Date: 2011/11/16 23:35:04 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUSPECFX - Fourier SPECtrum (T -> F) of traces 		",
" 								",
" suspecfx <infile >outfile 					",
" 								",
" Note: To facilitate further processing, the sampling interval	",
"       in frequency and first frequency (0) are set in the	",
"	output header.						",
" 								",
NULL};

/* Credits:
 *
 *	CWP: Dave (algorithm), Jack (reformatting for SU)
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: ns, dt, trid, d1, f1
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
	int nfft;		/* number of points on output trace	*/
	int nfby2p1;		/* nfft/2 + 1				*/
	float dt;		/* sample interval in secs		*/
	float d1;		/* output sample interval in Hz		*/
	int ntr=0;		/* number of traces			*/
	register int i;		/* counter				*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;

	/* dt is used only to set output header value d1 */
	if (!getparfloat("dt", &dt)) dt = ((double) tr.dt)/1000000.0;
	if (!dt) {
		dt = .004;
		warn("dt not set, assumed to be .004");
	}


        checkpars();


	/* Set up pfa fft */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
		 err("Padded nt=%d--too big", nfft);
	nfby2p1 = nfft/2 + 1;
	d1 = 1.0/(nfft*dt);

	rt = ealloc1float(nfft);
	ct = ealloc1complex(nfby2p1);


	/* Main loop over traces */
	do {
		++ntr;

		/* Load trace into rt (zero-padded) */
		memcpy( (void *) rt, (const void *) tr.data, nt*FSIZE);
		memset( (void *) (rt + nt), 0, (nfft - nt)*FSIZE);

		/* FFT */
		pfarc(1, nfft, rt, ct);

		/* Compute amplitude spectrum */
		tr.data[0] = rcabs(ct[0])/2.0;
		for (i = 1; i < nfby2p1; ++i)  tr.data[i] = rcabs(ct[i]);

		/* Set header values */
		tr.ns = nfby2p1;
		tr.dt = 0;	  /* d1=df is now the relevant step size */
		tr.trid = AMPLITUDE;
		tr.d1 = d1;
		tr.f1 = 0.0;

		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
