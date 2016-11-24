/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SULPRIME: $Revision: 1.4 $ ; $Date: 2011/11/16 17:24:58 $		*/

#include "su.h"
#include "segy.h"
#include <time.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SULPRIME - find appropriate Backus average length for  	",
" 		a given log suite, frequency, and purpose		",
" 								",
" sulprime < vp_vs_rho.su  [options]		or		",
" sulprime < vp_vs_rho_gr.su   [options]			",
" 								",
" Required parameters:						",
" none								",
"								",
" Optional parameter:						",
" b=2.0		target value of Backus number		 	",
"		b=2 is transmission limit (ok for proc, mig, etc.)",
"		b=0.3 is scattering limit (ok for modeling)	",
" dz=1		input depth sample interval (ft)		 ",
" f=60		frequency (Hz)... dominant or max (to be safe) 	",
" nmax=301	maximum averaging length (samples)		",
" verbose=1	print intermediate results			",
"		=0 print final result only			",
"								",
" Notes:							",
" 1. input is in sync with subackus, but vp and gr not used	",
"     (gr= gamma ray log)					",
" Related codes:  subackus, subackush				",
NULL};

/* 
 * Credits:
 *	UHouston: Chris Liner Sept 2008
 *              I gratefully acknowledge Saudi Aramco for permission
 *              to release this code developed while I worked for the
 *              EXPEC-ARC research division.
 * Reference:			
 *     The Backus Number (Liner and Fei, 2007, TLE)
 *
 */
/**************** end self doc ***********************************/

static void dobackus(int navg, int nz, float *p, float *avg);

segy tr;

int
main(int argc, char **argv) {
	float *vs;		/* s-wave velocity model 	*/
	float *vs0;		/* vertical s-wave speed 	*/
	int nz;			/* number of depth levels in log*/
	int i;			/* counters 			*/
	int navg=101;		/* length of averaging window	*/
	float *tmp;		/* temp array for avg subroutine */
	float *lo;		/* original stiffness params from logs 	*/
	float *lb;		/* backus-average stiffness params	*/
	float *rhoo, *rhob;	/* orig and backus avg density		*/	
	float minvs0=999;		/* minimum vertical shear speed after backus avg */
	float bb=0;		/* Backus number			*/
	float btarg;		/* target Backus number			*/
	float freq;		/* frequency				*/
	int verbose;		/* verbose output flag			*/
	float dz;		/* input depth sampling (ft)		*/
	float errf;		/* error in Backus number (float)	*/
	float errmin,bmin=0;	/* min err and associated B		*/
	int nmin=0;		/* navg associated with errmin		*/
	int nmax;		/* maximum averaging length (samples)	*/
	float vmin=0;		/* min vs0 at errmin			*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* get parameters */
	if (!getparfloat("b",&btarg)) 		btarg   = 2.0;
	if (!getparfloat("dz",&dz)) 		dz	= 1.0;
	if (!getparfloat("f",&freq)) 		freq	= 60.0;
	if (!getparint("nmax",&nmax))		nmax	= 301;
	if (!getparint("verbose",&verbose)) 	verbose = 0;
	
        checkpars();
	warn("Target value: B=%g  Frequency: %g Hz",btarg,freq);

	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nz = tr.ns;

	/* alloc input model vectors */
	vs = alloc1float(nz);
	rhoo = alloc1float(nz);

	/* skip vp trace */
	/* load input trace 2 into vs */
	gettr(&tr);
	for (i = 0; i < nz; ++i) {
		vs[i]  = tr.data[i];
	}

	/* load input trace 3 into rho */
	gettr(&tr);
	for (i = 0; i < nz; ++i) {
		rhoo[i]  = tr.data[i];
	}

	/* allocate other model vectors */
	/* orig quantities */
	lo 	= alloc1float(nz);
	/* averaged quantities */
	lb 	= alloc1float(nz);
	rhob 	= alloc1float(nz);
	/* misc		*/
	tmp 	= alloc1float(nz);
	vs0 	= alloc1float(nz);

	/* fill orig stiffness array */
	for (i = 0; i < nz; ++i) {
		lo[i] 	= rhoo[i] * vs[i] * vs[i];
	}

	/* initiallize errors */
	errf = 999.;
	errmin = 999.;
	
	for (navg=1; navg<=nmax; navg+=2) {
		

		/* calc L=lb */
		for (i = 0; i < nz; ++i) {
			tmp[i] = 1.0/lo[i];
		}
		dobackus(navg,nz,tmp,lb);
		for (i = 0; i < nz; ++i) {
			lb[i] = 1.0/lb[i];
		}

		/* calc rhob (averaged density)	*/
		dobackus(navg,nz,rhoo,rhob);

		/* calculate min Vs0 	*/
		minvs0 = 999999.0;
		for (i = 0; i < nz; ++i) {
			vs0[i] = sqrt( lb[i] / rhob[i] );
			if (vs0[i] < minvs0) minvs0 = vs0[i];
		}

		/* calc backus number B */
		bb = freq* (float) navg*dz*0.3048 / minvs0;
					  
		/* deviation of backus number from target value */
		errf = fabs(btarg - bb);
		if (errf < errmin) {
			errmin = errf;
			nmin = navg;
			bmin = bb;
			vmin = minvs0;
			/* warn("errmin=%g  nmin=%i  bmin=%g  vmin=%g",errmin,nmin,bmin,vmin); */
		}

		if ( verbose == 1 ) warn("err=%g; B = %g; L=%i (samples); minvs0=%g",
					  errf,bb,navg,minvs0);
		
	}
	
	warn("Results:	B=%g   navg=%i  min[vs0]=%g",bmin,nmin,vmin);

	return(CWP_Exit());
}

static void dobackus(int navg, int nz, float *p, float *avg)
/*****************************************************************************
dobackus - Do Backus averaging of parameter vector p[nz] using a 
centered window of navg samples.
*****************************************************************************

avg_j = (1/m) ( sum_{i=j-m/s}^{j+m/2} p_i )

where m=navg
*****************************************************************************/
{
	int i, j, m;
	float val;

	/* half width of rms window */
	m = (navg-1)/2;

	/* loop over output times */
	for (i = 0; i < nz; ++i) {
		val = 0.0;
		/* check we are not off the data ends */
		if (i-m > 0 && i+m < nz) {
			/* loop over window samples */
			for (j = i-m; j <= i+m; ++j) {
				val += p[j];
			}
			avg[i] = val/navg;
		}
		/* quick fix on edge effects */
		if (i <= m) avg[i] = p[i];
		if (i >= nz-m) avg[i] = p[i];
	}
		
}
