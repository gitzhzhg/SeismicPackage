/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUDIVCOR: $Revision: 1.16 $ ; $Date: 2012/11/29 18:05:28 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUDIVCOR - Divergence (spreading) correction				",
" 									",
" sudivcor <stdin >stdout  [optional parms]				",
" 									",
" Required Parameters:							",
" none									",
" 									",
" Optional Parameters:							",
" trms=0.0	times corresponding to rms velocities in vrms		",
" vrms=1500.0	rms velocities corresponding to times in trms	",
" vfile=	binary (non-ascii) file containing velocities vrms(t)	",
" 									",
" Notes:								",
" The trms, vrms arrays specify an rms velocity function of time.	",
" Linear interpolation and constant extrapolation is used to determine	",
" rms velocities at times not specified.  Values specified in trms	",
" must increase monotonically.						",
" 									",
" Alternatively, rms velocities may be stored in a binary file		",
" containing one velocity for every time sample.  If vfile is specified,",
" then the trms and vrms arrays are ignored.				",
" 									",
" The time of the first sample is assumed to be constant, and is taken	",
" as the value of the first trace header field delrt. 			",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Francesca Fazarri
 *
 * Trace header fields accessed:  ns, dt, delrt
 */
/**************** end self doc *******************************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;			/* number of points on input trace	*/
	float dt;		/* sample spacing			*/
	float tmin;		/* time of first sample			*/
	float *vt;		/* velocity function			*/
	float *vrms;		/* rms velocity picks			*/
	float *trms;		/* times corresponding to vrms picks	*/
	cwp_String vfile="";	/* binary file giving vrms(t)		*/
	float *divcor;		/* divergence correction function	*/
	float denom=1.0; 	/* vrms to divide by 			*/

	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);


	/* Get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	tmin = tr.delrt/1000.0;


	/* Determine velocity function v(t) */
	vt = ealloc1float(nt);
	if (!getparstring("vfile", &vfile)) {
		int ntrms = countparval("trms");
		int nvrms = countparval("vrms");
		int npar;
		register int it, itrms;

		if (nvrms != ntrms)
			err("number of trms and vrms must be equal");

		if (!ntrms)  ntrms = 2;  /* default case */
		trms = ealloc1float(ntrms);
		if (!(npar = getparfloat("trms", trms))) {
			trms[0] = 0.0;
			trms[1] = dt;
		}
		if (npar == 1) trms[1] = trms[0] + dt; /* const vel case */
		
		if (!nvrms)  nvrms = 2;  /* default case */
		vrms = ealloc1float(nvrms);
		if (!(npar = getparfloat("vrms", vrms))) {
			vrms[0] = 1500.0;
			vrms[1] = 1500.0;
		}
		if (npar == 1) vrms[1] = vrms[0];  /* const vel case */

		for (itrms = 1; itrms < ntrms; ++itrms)
			if (trms[itrms] <= trms[itrms-1])
				err("trms must increase monotonically");

		for (it = 0; it < nt; ++it) {
			float t = tmin + it*dt;
			intlin(ntrms,trms,vrms,vrms[0],vrms[ntrms-1],
				1,&t,&vt[it]);
		}
		denom = trms[1]*vrms[1]*vrms[1];
		
	} else {  /* user gave a vfile */
		FILE *fp = efopen(vfile, "r");
		
		if (nt != efread(vt, FSIZE, nt, fp)) {
			err("cannot read %d velocities from file %s",
				nt, vfile);
		} else denom = (tmin + dt)*vt[1]*vt[1];
	}
        checkpars();

	/* Form divergence correction vector */
	{ register int it;
	  
	  divcor = ealloc1float(nt);
	  for (it = 0; it < nt; ++it) {
	  	float t = tmin + it*dt;
		divcor[it] = t*vt[it]*vt[it] / denom;
	  }
	}
   
	  
	/* Main loop over traces */
	do {
		register int it;

	  	for (it = 0; it < nt; ++it)  tr.data[it] *=  divcor[it];
		puttr(&tr);
	} while (gettr(&tr));

	return(CWP_Exit());
}
