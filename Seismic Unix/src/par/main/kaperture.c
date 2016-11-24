/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* KAPERTURE: $Revision: 1.10 $ ; $Date: 2011/11/16 16:42:16 $	*/

#include "par.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"	 								",
" KAPERTURE - generate the k domain of a line scatterer for a seismic array",
" 	      								",
" kaperture [optional parameters] >stdout 				",
" 									",
" Optional parameters							",
" 	x0=1000		point scatterer location			",
" 	z0=1000		point scatterer location			",
" 	nshot=1		number of shots					",
" 	sxmin=0		first shot location				",
" 	szmin=0		first shot location				",
" 	dsx=100		x-steps in shot location			",
" 	dsz=0		z-steps in shot location			",
" 	ngeo=1		number of receivers				",
" 	gxmin=0		first receiver location				",
" 	gzmin=0		first receiver location				",
" 	dgx=100		x-steps in receiver location			",
" 	dgz=0		z-steps in receiver location			",
" 	fnyq=125	Nyquist frequency  (Hz)				",
" 	fmax=125	maximum frequency  (Hz)				",
" 	fmin=5		minimum frequency  (Hz)				",
" 	nfreq=2		number of frequencies   			",
" 	both=0		= 1 gives negative freqs too			",
" 	nstep=60	points on Nyquist circle			",
" 	c=5000		speed						",
" 	outpar=/dev/tty output parameter file, contains:		",
" 				xmin, xmax, ymin, ymax 			",
" 				and npairs (needed for psgraph or xgraph)",
" 			other choices for outpar are: /dev/tty,		",
" 			/dev/stderr, or a name of a disk file		",
" Notes:								",
"       nfreq=1 produces fmin						",
"       nstep=0 suppresses the Nyquist circle				",
" 				and npairs				",
" Examples:								",
" 									",
" Default case: both=0 nfreq=2					",
" 									",
" 	kaperture nshot=NSHOT ngeo=NGEO nstep=NSTEP |			",
" 	psgraph	n=NPAIRS,NSTEP mark=1,0 marksize=1,0 linewidth=0,1 |...",
" 		WHERE: NPAIRS=NSHOT*NGEO				",
" 									",
" Other cases: 								",
" 									",
" both=0 nfreq=NFREQ > 2						",
" 	kaperture both=0 nfreq=NFREQ nshot=NSHOT ngeo=NGEO nstep=NSTEP |",
" 	psgraph	n=NPAIRS,NSTEP mark=1,0 marksize=1,0 linewidth=0,1 |...	",
" 		WHERE: NPAIRS=NFREQ*NSHOT*NGEO				",
" 									",
" both=1 nfreq=NFREQ > 2						",
" 	kaperture both=1 nfreq=NFREQ nshot=NSHOT ngeo=NGEO nstep=NSTEP |",
" 	psgraph	n=NPAIRS,NSTEP mark=1,0 marksize=1,0 linewidth=0,1 |...	",
" 		 WHERE: NPAIRS=NFREQ*NSHOT*NGEO*2			",
" 									",
" When in doubt to the size of NPAIRS, redirect output of kaperture to	",
" /dev/tty the first time to get npairs=:				",
"		 kaperture [optional parameters] > /dev/tty		",
NULL};
/**************** end self doc ***********************************/

/* Credits:
 *	CWP: Jack K. Cohen, Sebastien Geoltrain, Norm Bleistein
 */


#define twopi		6.28318530717959
#define fourpi		12.5663706143592
#define EPS		1.0e-20
#define RATIO		1.2     /* ratio of (invisible) frame to circle */

/* Default parameter values */
#define NSTEPS		60
#define NFREQ		1
#define NSHOT		1
#define NGEO 		1
#define C    		5000.0
#define X0   		1000.0
#define Z0   		1000.0
#define SXMIN		0.0
#define SZMIN		0.0
#define GXMIN		0.0
#define GZMIN		0.0
#define DSX  		100.0
#define DSZ   		0.0
#define DGX   		100.0
#define DGZ   		0.0
#define FNYQ  		125.0
#define FMAX  		125.0
#define FMIN  		125.0

int
main(int argc, char **argv)
{
	float x0, z0;		/* point scatterer location	*/
	int nshot;		/* number of shots		*/
	float sxmin, szmin;	/* first shot location		*/
	float dsx, dsz;		/* x,z-steps in shot 		*/
	int ngeo;		/* number of receivers		*/
	float gxmin, gzmin;	/* first receiver location	*/
	float dgx, dgz;		/* x,z-steps in receiver 	*/
	float sx, sz;		/* scatterer - shot 		*/
	float gx, gz;		/* scatterer - receiver 	*/
	float rs;		/* distance scatterer to shot	*/
	float rg;		/* ... scatterer to receiver	*/
	float *x=NULL, *y=NULL;	/* kx, kz coordinates		*/
	float tmpx;		/* temporary storage for kx, kz	*/
	float tmpy;		/* temporary storage for kx, kz	*/
	int npaths;		/* nshot*ngeo			*/
	float fnyq;		/* Nyquist frequency 		*/
	float fmax, fmin;	/* maximum, minimum frequency	*/
	int nfreq;		/* number of frequencies	*/
	float df;		/* step in frequency 		*/
	float freq;		/* frequency			*/
	int both;		/* boolean for doing neg freqs	*/
	float c;		/* speed			*/
	float kscale;		/* scale factor per frequency	*/
	float knyqscale;	/* ... for Nyquist frequency	*/
	float phi;		/* angle for Nyquist circle	*/
	register int iphi;	/* ... and counter 		*/
	int nstep;		/* ... and bound		*/
	int npoints;		/* number of kx-kz pairs	*/
	int npairs=0;		/* number of kx-kz pairs output	*/
	register int ipoint;	/* index for kx-kz pairs	*/
	float xmin, xmax;	/* x range for plotting		*/
	float ymin, ymax;	/* ... and z range		*/
	char *outpar=NULL;	/* file holding output parfile	*/
	FILE *outparfp=NULL;	/* ... its file pointer		*/
	int s;			/* shot index			*/
	int g;			/* receiver index		*/
	register int f;		/* frequency counter		*/



	/* Initialize */
	initargs(argc, argv);
	requestdoc(0);


	/* Get parameters */
	if (!getparint("nfreq",  &nfreq))	nfreq = NFREQ;
	if (!getparint("nshot",  &nshot))	nshot = NSHOT;
	if (!getparint("ngeo",   &ngeo))	ngeo  = NGEO;
	if (!getparfloat("c",    &c))		c     = C;
	if (!getparfloat("x0",   &x0))		x0    = X0;
	if (!getparfloat("z0",   &z0))		z0    = Z0;
	if (!getparfloat("sxmin",&sxmin))	sxmin = SXMIN;
	if (!getparfloat("szmin",&szmin))	szmin = SZMIN;
	if (!getparfloat("gxmin",&gxmin))	gxmin = GXMIN;
	if (!getparfloat("gzmin",&gzmin))	gzmin = GZMIN;
	if (!getparfloat("dsx",  &dsx))		dsx   = DSX;
	if (!getparfloat("dsz",  &dsz))		dsz   = DSZ;
	if (!getparfloat("dgx",  &dgx))		dgx   = DGX;
	if (!getparfloat("dgz",  &dgz))		dgz   = DGZ;
	if (!getparfloat("fnyq", &fnyq))	fnyq  = FNYQ;
	if (!getparfloat("fmax", &fmax))	fmax  = FMAX;
	if (!getparfloat("fmin", &fmin))	fmin  = FMIN;
	if (!getparint("nstep", &nstep))	nstep = NSTEPS;
	if (!getparint("both",   &both))	both   = 0;
	if (!getparstring("outpar", &outpar))	outpar = "/dev/tty";


        checkpars();

	/* Open file to save parameters */
	outparfp = efopen(outpar, "w");


	/* Allocate x, y arrays */
	npaths = nshot * ngeo;
	npoints = (both) ? 2 * npaths : npaths;
	x = ealloc1float(npoints);
	y = ealloc1float(npoints);
	memset( (void *) x, 0, npoints* FSIZE);
	memset( (void *) y, 0, npoints* FSIZE);

	/* Create the basic k-curve using ipoint = s*ngeo + g */
	for (ipoint = 0; ipoint < npaths; ++ipoint) {
		s = (float) ipoint / (float) ngeo;
		g = (float) ipoint - s * ngeo;
		sx = (float) x0 - (float) (sxmin + s * dsx);
		sz =(float)  z0 - (float) (szmin + s * dsz);
		gx =(float)  x0 - (float) (gxmin + g * dgx);
		gz =(float)  z0 - (float) (gzmin + g * dgz);
		rs = sqrt(sx * sx + sz * sz);
		if (rs<=EPS) rs = EPS; /* fudge to prevent divide by zero */
		rg = sqrt(gx * gx + gz * gz);
		if (rg<=EPS) rg = EPS; /* fudge to prevent divide by zero */

		/* Load values into x, y.  Reverse sign of kz to */
		/* agree with a positive downward z coordinate   */
		x[ipoint] = sx/rs + gx/rg;
		y[ipoint] = -(sz/rs + gz/rg);

		if (both) {  /* load negative values in back half */
			x[npaths + ipoint] = -x[ipoint];
			y[npaths + ipoint] = -y[ipoint];
		}
	}


	/* Scale and write in (x,y) binary format */
	df = (nfreq == 1) ? 0.0 : (fmax - fmin) / ((float) nfreq - 1.0);
	for (f = 0; f < nfreq; ++f) {
		freq = fmin + f * df;
		kscale = twopi * freq / c;
		for (ipoint = 0; ipoint < npoints; ++ipoint) {

			npairs++; /* count the number of pairs output */

			tmpx = kscale * x[ipoint];
			tmpy = kscale * y[ipoint];
			efwrite(&tmpx, FSIZE, 1, stdout);
			efwrite(&tmpy, FSIZE, 1, stdout);
		}
	}


	/* Largest value of the magnitude of the gradient sum is two */
	knyqscale = fourpi * fnyq / c;
	ymin = xmin = -RATIO * knyqscale; 
	ymax = xmax =  RATIO * knyqscale; 


	/* Draw a circle with the Nyquist radius as a boundary */
	if (nstep) {
		for (iphi = 0; iphi < nstep; ++iphi) {
			phi = iphi * twopi/nstep;
			tmpx = cos(phi) * knyqscale;
			tmpy = sin(phi) * knyqscale;
			efwrite(&tmpx, FSIZE, 1, stdout);
			efwrite(&tmpy, FSIZE, 1, stdout);
		}
	}
		

	/* Make par file */
	(void) fprintf(outparfp, "xmin=%f xmax=%f ymin=%f ymax=%f\n",
	                   xmin,   xmax,   ymin,   ymax);

	(void) fprintf(outparfp, "npairs=%d\n", npairs);

	/* Clean up */
	free1float(x);
	free1float(y);


	return(CWP_Exit());
}
