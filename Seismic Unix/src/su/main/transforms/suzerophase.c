/* Copyright (c) Colorado School of Mines, 2008.*/
/* All rights reserved.			*/

/* SUZEROPHASE: $Revision: 1.1 $ ; $Date: 2015/08/07 22:28:55 $	*/


#include "su.h"
#include "segy.h"
#include "cwp.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUZEROPHASE - convert input to zero phase equivalent			",
"									",
" suzerophase <stdin >stdout [optional parameters]	 		",
"									",
" Required parameters:					 		",
"	if dt is not set in header, then dt is mandatory		",
"									",
" Optional parameters:							",
"    	t0=1.0			time of peak value t0 			",
"	verbose=0		=1 for advisory messages		",
"									",
"									",
NULL};

/* Credits:
 *	c. 2011 Bruce VerWest as part of the SEAM project
 * 
 * Trace header fields accessed: ns, dt, d1
 */
/**************** end self doc ***********************************/

/* Prototype of function used internally */

#define PIBY2   1.57079632679490
#define TWOPI   6.2831853	
#define LOOKFAC 2	/* Look ahead factor for npfao    */
#define PFA_MAX 720720  /* Largest allowed nfft	   */

segy tr;

int
main(int argc, char **argv)
{
	register float *rt=NULL;	/* real trace			*/
	register complex *ct;   /* complex transformed trace		*/

	float t0;		/* t0 in ms				*/	
	float dom;		/* 2pi/nfft/dt			    	*/
	float dt;		/* time sampling interval		*/
	float scale;		/* scale factor = 1/nfft		*/

	int nt;		 	/* number of points on input trace	*/
	int nfft;		/* number of points for fft trace	*/
	int verbose;		/* flag to get advisory messages	*/
	cwp_Bool seismic;	/* is this seismic data?		*/

	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!getparint("verbose", &verbose))	verbose=0;
	if (!gettr(&tr))  err("can't get first trace");

	seismic = ISSEISMIC(tr.trid);
	if (seismic) {
		if (verbose)	warn("input is seismic data, trid=%d",tr.trid);
		dt = ((double) tr.dt)/1000000.0;
	} else {
		if (verbose) warn("input is not seismic data, trid=%d",tr.trid);

		dt = tr.d1;

	}

	/* error trapping so that the user can have a default value of dt */
	if (!(dt || getparfloat("dt", &dt))) {
		dt = .004;
		if (verbose) {
			warn("neither dt nor d1 are set, nor is dt getparred!");
			warn("assuming .004 sec sampling!");
		}
	}
    	if (!getparfloat("t0", &t0))	t0=1.0;

	nt = tr.ns;

	/* Set up FFT parameters */
	nfft = npfao(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
		err("Padded nt=%d -- too big", nfft);

	dom = TWOPI/nfft/dt;

	if (verbose) warn("nfft,nt,dt,dom is =%d %d %f %f",nfft,nt,dt);

	/* Allocate fft arrays */
	rt   = ealloc1float(nfft);
	ct   = ealloc1complex(nfft);

	/* Main loop over traces */
	do {
		register int i;

		/* Load trace into rt (zero-padded) */
		memcpy((void *) rt, (const void *) tr.data, nt*FSIZE);
		memset((void *) (rt + nt), 0 , (nfft-nt)*FSIZE);
		for (i=0; i<nfft; ++i) ct[i] = cmplx(rt[i],0.0);

		/* FFT, filter, inverse FFT */
		pfacc(1, nfft,  ct);

		for (i=0; i<nfft/2+1; ++i) {
			float r = rcabs(ct[i]);
			float om = i*dom;

			ct[i] = cmplx(r*cos(om*t0),r*sin(om*t0));
		}
		for (i=nfft/2+1; i<nfft; ++i) {
			ct[i] = conjg(ct[nfft-i]);
		}

		pfacc(-1, nfft, ct);
		scale = 1./nfft;
		for (i=0; i<nfft; ++i) ct[i] = crmul(ct[i], scale);

		/* Load traces back in, recall filter had nfft factor */
		for (i=0; i<nfft; ++i) rt[i] = ct[i].r;
		for (i = 0; i < nt; ++i)  tr.data[i] = rt[i];
		
		tr.delrt = -t0*1000;

		puttr(&tr);
	} while (gettr(&tr));

	return(CWP_Exit());
}
