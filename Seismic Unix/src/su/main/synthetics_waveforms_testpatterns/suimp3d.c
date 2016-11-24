/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUIMP3D: $Revision: 1.25 $ ; $Date: 2015/06/02 20:15:23 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **************/
char *sdoc[] = {
"							",
"SUIMP3D - generate inplane shot records for a point 	",
"          scatterer embedded in three dimensions using	",
"          the Born integral equation			",							
"							",
"suimp3d [optional parameters] >stdout 			",
"							",
"Optional parameters					",
"	nshot=1		number of shots			",
"	nrec=1		number of receivers		",
"	c=5000		speed				",
"	dt=.004		sampling rate			",
"	nt=256		number of samples		",
"	x0=1000		point scatterer location	",
"	y0=0		point scatterer location	",
"	z0=1000		point scatterer location	",
"   dir=0		do not include direct arrival	",
"	            =1 include direct arrival	",
"	sxmin=0		first shot location		",
"	symin=0		first shot location		",
"	szmin=0		first shot location		",
"	gxmin=0		first receiver location		",
"	gymin=0		first receiver location		",
"	gzmin=0		first receiver location		",
"	dsx=100		x-step in shot location		",
"	dsy=0	 	y-step in shot location		",
"	dsz=0	 	z-step in shot location		",
"	dgx=100		x-step in receiver location	",
"	dgy=0		y-step in receiver location	",
"	dgz=0		z-step in receiver location	",
"							",
" Example:                                              ",
"       suimp3d nrec=32 | sufilter | supswigp | ...     ",
"							",
NULL};

/* Credits:
 *	CWP: Norm Bleistein, Jack K. Cohen
 *  UHouston: Chris Liner 2010 (added direct arrival option)
 *
 */
 
/* Theory: Use the 3D Born integral equation (e.g., Geophysics,
 * v51, n8, p1554(7)). Use 3-D delta function for alpha.
 *
 * Note: Setting a 3D offset in a single offset field beats the
 *       hell out of us.  We did _something_.
 *
 * Trace header fields set: ns, dt, tracl, tracr, fldr, tracf,
 *                          sx, sy, selev, gx, gy, gelev, offset
 */
/**************** end self doc ***************************/


#define LOOKFAC	2	/* Look ahead factor for npfao	  */
#define PFA_MAX	720720	/* Largest allowed nfft	          */


segy tr;

int
main(int argc, char **argv)
{
	float c;			/* speed			*/
	float dt;			/* sampling rate		*/
	int nt;				/* number of samples		*/
	size_t ntsize;			/* ... in bytes			*/
	int nshot;			/* number of shots		*/
	int nrec;			/* number of receivers		*/
	float x0, y0, z0;		/* point scatterer location	*/
	float sxmin, symin, szmin;	/* first shot location		*/
	float gxmin, gymin, gzmin;	/* first receiver location	*/
	float dsx, dsy, dsz;		/* step in shot location	*/
	float dgx, dgy, dgz;		/* step in receiver location	*/
	int dir;			/* flag to include direct arrival */

	float sx, sy, sz;		/* shot location		*/
	float gx, gy, gz;		/* receiver location		*/
	float rs;			/* distance to shot		*/
	float rg;			/* distance to receiver		*/
	float d;			/* rs + rg			*/
	float t;			/* total travel time		*/
	float k;			/* constant part of response	*/
	float rd;			/* direct arrival travel path length */

	register float *rt;		/* real trace			*/
	register float *temp;	/* temporary vector of length nt */
	register complex *ct;		/* complex transformed trace	*/
	int nfft;			/* size of fft 			*/
	int nfby2;			/* nfft/2			*/
	int nfby2p1;			/* nfft/2 + 1			*/
	size_t nzeros;			/* padded zeroes in bytes	*/
	float spread;			/* 3-D spreading factor		*/

	register int i;			/* counter			*/
	register int s;			/* shot counter			*/
	register int g;			/* receiver counter		*/
	register int tracl;		/* trace counter		*/

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
	if (!getparfloat("y0", &y0))		y0    = 0.0;
	if (!getparfloat("z0", &z0))		z0    = 1000.0;
	if (!getparint("dir", &dir))		dir  = 0;
	if (!getparfloat("sxmin", &sxmin))	sxmin = 0.0;
	if (!getparfloat("symin", &symin))	symin = 0.0;
	if (!getparfloat("szmin", &szmin))	szmin = 0.0;
	if (!getparfloat("gxmin", &gxmin))	gxmin = 0.0;
	if (!getparfloat("gymin", &gymin))	gymin = 0.0;
	if (!getparfloat("gzmin", &gzmin))	gzmin = 0.0;
	if (!getparfloat("dsx", &dsx))		dsx   = 100.0;
	if (!getparfloat("dsy", &dsy))		dsy   = 0.0;
	if (!getparfloat("dsz", &dsz))		dsz   = 0.0;
	if (!getparfloat("dgx", &dgx))		dgx   = 100.0;
	if (!getparfloat("dgy", &dgy))		dgy   = 0.0;
	if (!getparfloat("dgz", &dgz))		dgz   = 0.0;


	/* Set the constant header fields */
	tr.ns = nt;
	tr.dt = NINT(dt * 1000000.0);
	ntsize = nt * FSIZE;
	
	/* alloc temp array */
	temp = ealloc1float(nt);


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
	k = 1.0 / (4.0 * c * c * dt * dt * dt * nfft * nfft * nfft);

	/* Compute output times for interpolation */
	tout = ealloc1float(nt);
	for (i=0; i<nt; i++) tout[i]=i*dt;

	/* Create the traces */
	tracl = 0;
	for (s = 0; s < nshot; ++s) {	/* loop over shots */
		sx = sxmin + s * dsx;
		sy = symin + s * dsy;
		sz = szmin + s * dsz;
		rs = sqrt((sx - x0)*(sx - x0) + (sy - y0)*(sy - y0) +
			(sz - z0)*(sz - z0));

		for (g = 0; g < nrec; ++g) {	/* loop over receivers */
			memset( (void *) tr.data, 0, ntsize);
			gx = gxmin + g * dgx;
			gy = gymin + g * dgy;
			gz = gzmin + g * dgz;
			rg = sqrt((gx - x0)*(gx - x0) + (gy - y0)*(gy - y0) +
				(gz - z0)*(gz - z0));
			d = rs + rg;
			t = d/c;
			spread = rs*rg;
			amplitude[0] = k/spread;

			/* Distribute diffraction response over full trace */
			ints8r(1,dt,t,amplitude,0,0,nt,tout,tr.data);
			
			if (dir == 1) {
				
				/* direct arrival distance, time, and spreading */
				rd = sqrt((gx - sx)*(gx - sx) + (gy - sy)*(gy - sy));
				t = rd/c;
				spread = rd;
				amplitude[0] = k/spread;

				/* Distribute direct response over temp trace */
				ints8r(1,dt,t,amplitude,0,0,nt,tout,temp);
				
				/* add in direct wave */
				for (i=0; i<nt; i++) tr.data[i] = tr.data[i] + temp[i];

			}
			
			/* Load trace into rt (zero-padded) */
			memcpy( (void *) rt, (const void *) tr.data, ntsize);
			memset( (void *) (rt + nt), 0, nzeros);

			/* FFT */
			pfarc(1, nfft, rt, ct);

			/* Multiply by omega^2 */
			for (i = 0; i < nfby2p1; ++i)
				ct[i] = crmul(ct[i], i*i);

			/* Invert and take real part */
			pfacr(-1, nfft, ct, rt);

			/* Load traces back in */
			memcpy( (void *) tr.data, (const void *) rt, ntsize);

			/* Set header fields---shot fields set above */
			tr.tracl = tr.tracr = ++tracl;
			tr.fldr = 1 + s;
			tr.tracf = 1 + g;
			tr.sx = NINT(sx);
			tr.sy = NINT(sy);
			tr.selev = -NINT(sz); /* above sea level > 0 */
			tr.gx = NINT(gx);
			tr.gy = NINT(gy);
			tr.gelev = -NINT(gz); /* above sea level > 0 */
			
			/* If along a coordinate axis, use a signed offset */
			tr.offset = sqrt((sx - gx)*(sx - gx) +
					 (sy - gy)*(sy - gy) +
			    		 (sz - gz)*(sz - gz));
			if (dgy == 0 && dgz == 0)
				tr.offset = NINT(dsx > 0 ? gx - sx : sx - gx);
			if (dgx == 0 && dgz == 0)
				tr.offset = NINT(dsy > 0 ? gy - sy : sy - gy);
			if (dgx == 0 && dgy == 0)
				tr.offset = NINT(dsz > 0 ? gz - sz : sz - gz);

			puttr(&tr);
		} /* end loop on receivers */
	} /* end loop on shots */

	return(CWP_Exit());
}
