/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFRAC: $Revision: 1.29 $ ; $Date: 2011/12/21 23:25:42 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUFRAC -- take general (fractional) time derivative or integral of	",
"	    data, plus a phase shift.  Input is CAUSAL time-indexed	",
"	    or depth-indexed data.					",
"									",
" sufrac power= [optional parameters] <indata >outdata 			",
"									",
" Optional parameters:							",
"	power=0		exponent of (-i*omega)	 			",
"			=0  ==> phase shift only			",
"			>0  ==> differentiation				",
"			<0  ==> integration				",
"									",
"	sign=-1			sign in front of i * omega		",
"	dt=(from header)	time sample interval (in seconds)	",
"	phasefac=0		phase shift by phase=phasefac*PI	",
"	verbose=0		=1 for advisory messages		",
"									",
" Examples:								",
"  preprocess to correct 3D data for 2.5D migration			",
"         sufrac < sudata power=.5 sign=1 | ...				",
"  preprocess to correct susynlv, susynvxz, etc. (2D data) for 2D migration",
"         sufrac < sudata phasefac=.25 | ...				",
" The filter is applied in frequency domain.				",
" if dt is not set in header, then dt is mandatory			",
"									",
" Algorithm:								",
"		g(t) = Re[INVFTT{ ( (sign) iw)^power FFT(f)}]		",
" Caveat:								",
" Large amplitude errors will result if the data set has too few points.",
"									",
" Good numerical integration routine needed!				",
" For example, see Gnu Scientific Library.				",
"									",
NULL};

/* Credits:
 *	CWP: Chris Liner, Jack K. Cohen, Dave Hale (pfas)
 *      CWP: Zhenyue Liu and John Stockwell added phase shift option
 *	CENPET: Werner M. Heigl - added well log support
 *
 * Trace header fields accessed: ns, dt, trid, d1
*/
/**************** end self doc ***********************************/


#define	I		cmplx(0.0, 1.0)
#define	PIBY2		0.5 * PI
#define TWOPI		2.0 * PI
#define LOOKFAC		2	/* Look ahead factor for npfao	  */
#define PFA_MAX		720720	/* Largest allowed nfft	          */

segy tr;

int
main(int argc, char **argv)
{
	float power;		/* power of i omega applied to data	*/
	float amp;		/* amplitude associated with the power	*/
	float arg;		/* argument of power 			*/
	float phasefac;		/* phase factor	 			*/
	float phase;		/* phase shift = phasefac*PI		*/
	complex exparg;		/* cwp_cexp(I arg)				*/
	register float *rt;	/* real trace				*/
	register complex *ct;	/* complex transformed trace		*/
	complex *filt;		/* complex power	 		*/
	int nt;			/* number of points on input trace	*/
	size_t ntsize;		/* nt in bytes				*/
	float dt;		/* sample spacing (secs) on input trace	*/
	float omega;		/* circular frequency			*/
	float domega;		/* circular frequency spacing (from dt)	*/
	float sign;		/* sign in front of i*omega default -1	*/
	int nfft;		/* number of points in nfft		*/
        int nf;                 /* number of frequencies (incl Nyq)     */
	float onfft;		/* 1 / nfft				*/
	int verbose;		/* flag to get advisory messages	*/
	size_t nzeros;		/* number of padded zeroes in bytes	*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Set parameters */
	if (!getparint("verbose", &verbose))	  verbose  =  0;
	if (!getparfloat("power", &power))	  power    =  0.0;
	if (!getparfloat("sign", &sign))	  sign     = -1.0;
	if (!getparfloat("phasefac", &phasefac))  phasefac =  0.0;
	phase = phasefac * PI;


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


	/* Set up for fft */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
		err("Padded nt=%d -- too big", nfft);

        nf = nfft/2 + 1;
        onfft = 1.0 / nfft;
	nzeros = (nfft - nt) * FSIZE;
	domega = TWOPI * onfft / dt;


	/* Allocate fft arrays */
	rt   = ealloc1float(nfft);
	ct   = ealloc1complex(nf);
	filt = ealloc1complex(nf);


	/* Set up args for complex power evaluation */
	arg = sign * PIBY2 * power + phase;
	exparg = cwp_cexp(crmul(I, arg));


	/* Evaluate complex power, put inverse fft scale in */
	{
		register int i;
		for (i = 0 ; i < nf; ++i) {

			omega = i * domega;

			/* kludge to handle omega=0 case for power < 0 */
			if (power < 0 && i == 0) omega = FLT_MAX;

			/* calculate filter */
			amp = pow(omega, power) * onfft;

			filt[i] = crmul(exparg, amp);
		}
	}
		

	/* Loop over traces */
	do {
		/* Load trace into rt (zero-padded) */
		memcpy( (void *) rt, (const void *) tr.data, ntsize);
		memset((void *) (rt + nt),0, nzeros);

		/* FFT */
		pfarc(1, nfft, rt, ct);


		/* Apply filter */
		{ register int i;
		for (i = 0; i < nf; ++i)  ct[i] = cmul(ct[i], filt[i]);
		}

		/* Invert */
		pfacr(-1, nfft, ct, rt);


		/* Load traces back in, recall filter had nfft factor */
		{ register int i;
		for (i = 0; i < nt; ++i)  tr.data[i] = rt[i];
		}


		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
