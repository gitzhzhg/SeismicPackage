/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUIMP2D: $Revision: 1.22 $ ; $Date: 2015/06/02 20:15:23 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **************/
char *sdoc[] = {
"							",
" SUIMP2D - generate shot records for a line scatterer	",
"           embedded in three dimensions using the Born	",
"	    integral equation				",							
"							",
" suimp2d [optional parameters] >stdout			",
"							",
" Optional parameters					",
"	nshot=1		number of shots			",
"	nrec=1		number of receivers		",
"	c=5000		speed				",
"	dt=.004		sampling rate			",
"	nt=256		number of samples		",
"	x0=1000		point scatterer location	",
"	z0=1000		point scatterer location	",
"	sxmin=0		first shot location		",
"	szmin=0		first shot location		",
"	gxmin=0		first receiver location		",
"	gzmin=0		first receiver location		",
"	dsx=100		x-step in shot location		",
"	dsz=0	 	z-step in shot location		",
"	dgx=100		x-step in receiver location	",
"	dgz=0		z-step in receiver location	",
"							",
" Example:						",
"	suimp2d nrec=32 | sufilter | supswigp | ...	",
"							",
NULL};

/* Credits:
 *	CWP: Norm Bleistein, Jack K. Cohen
 */

/* Theory: Use the 3D Born integral equation (e.g., Geophysics,
 * v51, n8, p1554(7)). Use 2-D delta function for alpha and do
 * remaining y-integral by stationary phase.
 *
 * Note: Setting a 2D offset in a single offset field beats the
 *       hell out of us.  We did _something_.
 *
 * Trace header fields set: ns, dt, tracl, tracr, fldr, sx, selev,
 *                          gx, gelev, offset
 */
/**************** end self doc ***************************/


#define LOOKFAC	2	/* Look ahead factor for npfao	  */
#define PFA_MAX	720720	/* Largest allowed nfft	          */

segy tr;

int
main(int argc, char **argv)
{
	float c;		/* speed			*/
	float dt;		/* sampling rate		*/
	int nt;			/* number of samples		*/
	size_t ntsize;		/* ... in bytes			*/
	int nshot;		/* number of shots		*/
	int nrec;		/* number of receivers		*/
	float x0, z0;		/* point scatterer location	*/
	float sxmin, szmin;	/* first shot location		*/
	float gxmin, gzmin;	/* first receiver location	*/
	float dsx;		/* x-step in shot location	*/
	float dsz;		/* z-step in shot location	*/
	float dgx;		/* x-step in receiver location	*/
	float dgz;		/* z-step in receiver location	*/

	float sx, sz;		/* shot location		*/
	float gx, gz;		/* receiver location		*/
	float rs;		/* distance to shot		*/
	float rg;		/* distance to receiver		*/
	float d;		/* rs + rg			*/
	float t;		/* total travel time		*/
	float k;		/* constant part of response	*/

	register float *rt;	/* real trace			*/
	register complex *ct;	/* complex transformed trace	*/
	int nfft;		/* size of fft 			*/
	int nfby2;		/* nfft/2			*/
	int nfby2p1;		/* nfft/2 + 1			*/
	size_t nzeros;		/* padded zeroes in bytes	*/
	float spread;		/* geometric spreading factor	*/
	float i32;		/* temp for omega to the 3/2	*/

	register int i;		/* counter			*/
	register int s;		/* shot counter			*/
	register int g;		/* receiver counter		*/
	register int tracl;	/* trace counter		*/

	float amplitude[1];	/* amplitude 			*/
	float *tout;		/* times[nt] for interpolation	*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(0);


	/* Get parameters */
	if (!getparint("nshot", &nshot))	nshot = 1;
	if (!getparint("nrec", &nrec))		nrec  = 1;
	if (!getparint("nt", &nt))		nt    = 256;
	if (!getparfloat("c", &c))		c     = 5000.0;
	if (!getparfloat("dt", &dt))		dt    = 0.004;
	if (!getparfloat("x0", &x0))		x0    = 1000.0;
	if (!getparfloat("z0", &z0))		z0    = 1000.0;
	if (!getparfloat("sxmin", &sxmin))	sxmin = 0.0;
	if (!getparfloat("szmin", &szmin))	szmin = 0.0;
	if (!getparfloat("gxmin", &gxmin))	gxmin = 0.0;
	if (!getparfloat("gzmin", &gzmin))	gzmin = 0.0;
	if (!getparfloat("dsx", &dsx))		dsx   = 100.0;
	if (!getparfloat("dsz", &dsz))		dsz   = 0.0;
	if (!getparfloat("dgx", &dgx))		dgx   = 100.0;
	if (!getparfloat("dgz", &dgz))		dgz   = 0.0;


	/* Set the constant header fields */
	tr.ns = nt;
	tr.dt = NINT(dt * 1000000.0);
	ntsize = nt * FSIZE;



	/* Set up for fft */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
		err("Padded nt=%d -- too big", nfft);

	nfby2 = nfft / 2;
	nfby2p1 = nfby2 + 1;
	nzeros = (nfft - nt) * FSIZE;


	/* Allocate fft arrays */
	rt   = ealloc1float(nfft);
	ct   = ealloc1complex(nfby2p1);


	/* Set the constant in the response amplitude
	   including scale for inverse fft below      */
	k = 1.0 / (4.0 * sqrt(2.0*c*dt*nfft) * c * dt * dt * nfft * nfft);

	/* Compute output times for interpolation */
	tout = ealloc1float(nt);
	for (i=0; i<nt; i++) tout[i]=i*dt;

	/* Create the traces */
	tracl = 0;
	for (s = 0; s < nshot; ++s) {	/* loop over shots */
		sx = sxmin + s * dsx;
		sz = szmin + s * dsz;
		rs = sqrt((sx - x0)*(sx - x0) + (sz - z0)*(sz - z0));

		for (g = 0; g < nrec; ++g) {	/* loop over receivers */
			memset((void *) tr.data, 0, ntsize);
			gx = gxmin + g * dgx;
			gz = gzmin + g * dgz;
			rg = sqrt((gx - x0)*(gx - x0) + (gz - z0)*(gz - z0));
			d = rs + rg;
			t = d/c;
			spread = sqrt(rs*rg*d);
			amplitude[0] = k/spread;

			/* Distribute response over full trace*/
			ints8r(1,dt,t,amplitude,0,0,nt,tout,tr.data);

			/* Load trace into rt (zero-padded) */
			memcpy((void *) rt, (const void *) tr.data, ntsize);
			memset((void *)(rt + nt), 0, nzeros);

			/* FFT */
			pfarc(1, nfft, rt, ct);

			/* Formula requires multiplication by
			   abs(omega) to 3/2 power times
			   exp(i pi / 4) sgn omega)    */
			for (i = 0; i < nfby2p1; ++i) {
				i32 = i * sqrt((double) i);
				ct[i] = crmul(ct[i], i32);
			}

			/* Invert and take real part */
			pfacr(-1, nfft, ct, rt);

			/* Load traces back in */
			memcpy( (void *) tr.data, (const void *) rt, ntsize);

			/* Set header fields */
			tr.tracl = tr.tracr = ++tracl;
			tr.fldr = 1 + s;
			tr.tracf = 1 + g;
			tr.sx = NINT(sx);
			tr.selev = -NINT(sz); /* above sea level > 0 */
			tr.gx = NINT(gx);
			tr.gelev = -NINT(gz); /* above sea level > 0 */
			
			/* If along a coordinate axis, use a signed offset */
			tr.offset = sqrt((sx - gx)*(sx - gx) +
			    		 (sz - gz)*(sz - gz));
			if (dgx == 0 && dgz != 0)
				tr.offset = NINT(dsz > 0 ? gz - sz : sz - gz);
			if (dgz == 0 && dgx != 0)
				tr.offset = NINT(dsx > 0 ? gx - sx : sx - gx);

			puttr(&tr);
		} /* end loop on receivers */
	} /* end loop on shots */

	return(CWP_Exit());
}
