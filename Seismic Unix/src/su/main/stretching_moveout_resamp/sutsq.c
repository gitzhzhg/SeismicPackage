/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUTSQ: $Revision: 1.8 $ ; $Date: 2011/11/16 23:21:55 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUTSQ -- time axis time-squared stretch of seismic traces	",
"								",
" sutsq [optional parameters] <stdin >stdout 			",
"								",
" Required parameters:						",
"	none				 			",
"								",
" Optional parameters:						",
"       tmin= .1*nt*dt  minimum time sample of interest		",
"                       (only needed for forward transform)	",
"       dt= .004       output sample rate			",
"                       (only needed for inverse transform)	",
"       flag= 1        1=forward transform: time to time squared",
"                     -1=inverse transform: time squared to time",
"								",
" Note: The output of the forward transform always starts with	",
" time squared equal to zero.  'tmin' is used to avoid aliasing	",
" the early times.",
" ",	
NULL};

/*
 * Caveats:
 * 	Amplitudes are not well preserved.
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: ns, dt
 */
/**************** end self doc ***********************************/

segy tr;

int
main(int argc, char **argv)
{
	int j,nt,flag,ntout;
	float *buf,*ttn,dt,dtout=0.0,tmin,tmax;

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = (float) tr.dt/1000000.0;

	if (!getparfloat("tmin", &tmin)) tmin=0.1*nt*dt;
	if (!getparint("flag", &flag)) flag=1;
	if(flag==1) {
		dtout=tmin*2.*dt;
		tmax=nt*dt;
		ntout=1+tmax*tmax/dtout; CHECK_NT("ntout",ntout);
		ttn=ealloc1float(ntout);
		for(j=0;j<ntout;j++) ttn[j]=sqrt(j*dtout);
	}else{
		if (!getparfloat("dt", &dt)) dtout=0.004;
		ntout=1+sqrt(nt*dt)/dtout; CHECK_NT("ntout",ntout);
		ttn=ealloc1float(ntout);
		for(j=0;j<ntout;j++) ttn[j]=j*j*dtout*dtout;
	}
        checkpars();
	buf = ealloc1float(nt);

	fprintf(stderr,"sutsq: ntin=%d dtin=%f ntout=%d dtout=%f\n",
		nt,dt,ntout,dtout);

	/* Main loop over traces */
	do {
		for(j=0;j<nt;j++) buf[j]=tr.data[j];
		tr.ns = ntout;
		tr.dt = dtout*1000000.;			
		ints8r(nt,dt,0.,buf,0.0,0.0,
			ntout,ttn,tr.data);
		puttr(&tr);
	} while (gettr(&tr));
	
	return(CWP_Exit());
}

