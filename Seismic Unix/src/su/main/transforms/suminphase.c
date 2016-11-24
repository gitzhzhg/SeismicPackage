/* Copyright (c) Colorado School of Mines, 2008.*/
/* All rights reserved.	*/

/* SUMINPHASE: $Revision: 1.1 $ ; $Date: 2015/08/07 22:32:05 $	*/


#include "su.h"
#include "segy.h"
#include "cwp.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUMINPHASE - convert input to minimum phase				",
"									",
" suminphase <stdin >stdout [optional parameters]	 		",
"									",
" Required parameters:					 		",
"	if dt is not set in header, then dt is mandatory		",
"									",
" Optional parameters:							",
"	sign1=1		sign of first transform	(1 or -1)		",
"	sign2=-1	sign of second transform (-1 or 1)		",
"    	pnoise=1.e-9	   white noise in spectral routine		",
"	verbose=0		=1 for advisory messages		",
"									",
NULL};

/* Credits:
 *      SEAM Project: Bruce VerWest c. 2013
 * 
 * Trace header fields accessed: ns, dt, d1
 */
/**************** end self doc ***********************************/

/* Prototype of function used internally */
void kolmogoroff(int n, float pnoise, int sign1, int sign2, complex *cx);

#define PIBY2   1.57079632679490
#define LOOKFAC 2	/* Look ahead factor for npfao    */
#define PFA_MAX 720720  /* Largest allowed nfft	   */

segy tr;

int
main(int argc, char **argv)
{
	register float *rt=NULL;     /* real trace			*/
	register complex *ct=NULL;   /* complex transformed trace	*/
	int sign1;		/* sign of first transform		*/
	int sign2;		/* sign of second transform		*/
	float dt;		/* time sampling interval		*/
	float scale;		/* scale factor				*/
	float pnoise;		/* white noise factor			*/
	int nt;			/* number of points on input trace      */
	int nfft;		/* number of points for fft trace	*/
	int verbose;		/* flag to get advisory messages	*/
	cwp_Bool seismic;	/* is this seismic data?		*/

	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!getparint("verbose", &verbose))	verbose=0;
	if (!getparint("sign1", &sign1))	sign1=1;
	if (!getparint("sign2", &sign2))	sign1=-1;
	
	/* make sure that sign1 and sign2 are nonzero */
	if (!(sign1)) err("sign1=%d ",sign1);
	if (!(sign2)) err("sign2=%d ",sign2);

	/* make sure that sign1 and sign2 are either 1 or -1 */
	sign1 = sign1/ABS(sign1);
	sign2 = sign2/ABS(sign2);

	if (!gettr(&tr))  err("can't get first trace");
	seismic = ISSEISMIC(tr.trid);
	if (seismic) {
		if (verbose)	warn("input is seismic data, trid=%d",tr.trid);
		dt = ((double) tr.dt)/1000000.0;
	}
	else {
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
    	if (!getparfloat("pnoise", &pnoise)) pnoise=1.0e-9;

	nt = tr.ns;

	/* Set up FFT parameters */
	nfft = npfao(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
		err("Padded nt=%d -- too big", nfft);

	if (verbose) warn("nfft,nt,dt is =%d %d %f",nfft,nt,dt);

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
		pfacc(sign1, nfft,  ct);
		kolmogoroff(nfft, pnoise, sign1, sign2, ct);
		pfacc(sign2, nfft, ct);
		scale = 1./nfft;
		for (i=0; i<nfft; ++i) ct[i] = crmul(ct[i], scale);

		/* Load traces back in, recall filter had nfft factor */
		for (i=0; i<nfft; ++i) rt[i] = ct[i].r;
		for (i = 0; i < nt; ++i)  tr.data[i] = rt[i];

		puttr(&tr);
	} while (gettr(&tr));

	return(CWP_Exit());
}

void kolmogoroff(int n, float pnoise, int sign1, int sign2, complex *cx)
/*************************************************************************
Spectral factorization
**************************************************************************
Input:
n		length of input array
pnoise		white noise factor
sign1		=1 default (1 or -1) sign of first transform
sign2		=-1 default (-1 or 1) sign of second transform
cx		complex array[n] of amplitude values

Output:
cx		complex array[n] filter values
**************************************************************************
Author:   Bruce VerWest 2009
*************************************************************************/
{
	int i;		/* index		*/
	float r;	   /* temp real	 */
	float rmax;	   /* temp real	 */
	float scale;	   /* temp real	 */

	rmax=0.0;

	for (i=0; i<n; ++i) {
		r = rcabs(cx[i]);
		if (r > rmax) rmax = r;
	}

	for (i=0; i<n; ++i) {
		r = rcabs(cx[i])/rmax;
		if (r < pnoise) r = pnoise;
		cx[i] = cmplx(r*r,0.0);
		cx[i] = cwp_clog(cx[i]);
	}

	pfacc(sign2, n, cx);
	scale = 1./n;
	for (i=0; i<n; ++i) cx[i] = crmul(cx[i], scale);

	cx[0] = crmul(cx[0],0.5);
	cx[n/2] = crmul(cx[n/2],0.5);
	for (i=n/2+1; i<n; ++i) cx[i] = cmplx(0.,0.);

	pfacc(sign1, n, cx);

	for (i=0; i<n; ++i) {
		cx[i] = crmul(cwp_cexp(cx[i]),rmax);
	}
}


