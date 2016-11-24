/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUREDUCE: $Revision: 1.13 $ ; $Date: 2011/11/16 23:21:55 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 	   							",
" SUREDUCE - convert traces to display in reduced time		", 
" 	   							",
" sureduce <stdin >stdout rv=					",
" 								",
" Required parameters:						",
"	dt=tr.dt	if not set in header, dt is mandatory	",
"								",
" Optional parameters:						",
"	rv=8.0		reducing velocity in km/sec		",	
"								",
" Note: Useful for plotting refraction seismic data. 		",
" To remove reduction, do:					",
" suflip < reduceddata.su flip=3 | sureduce rv=RV > flip.su	",
" suflip < flip.su flip=3 > unreduceddata.su			",
"								",
" Trace header fields accessed: dt, ns, offset			",
" Trace header fields modified: none				",
NULL};

/*
 * Author: UC Davis: Mike Begnaud  March 1995
 *
 *
 * Trace header fields accessed: ns, dt, offset
 */
/**************** end self doc ***********************************/
segy tr;

int
main(int argc, char **argv)
{
	int nt;
	float dt, rv;

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get parameters */
	if (!getparfloat("rv", &rv))	 rv=8.0;

	/* Data validation */
	if (rv <= 0.0) err("rv=%f, must be >= 0.0", rv);

	/* Get info from first trace */
	if (!fgettr(stdin, &tr)) err("can't read first trace");
	if (!tr.dt) MUSTGETPARFLOAT("dt", &dt);
	nt = (int) tr.ns;
	dt = ((double) tr.dt)/1000000.0;	
        checkpars();

	/* Loop over traces */
	do {
		float off  = (float) ABS(tr.offset);
		float bt   = ((fabs) (off))/(rv*1000.0);
		int rnt    = NINT(bt/dt);
		register int i;

		for (i = 0; i < nt; ++i) {
			register int j = i + rnt;
			tr.data[i] = (i < (nt - rnt)) ?  tr.data[j] : 0.0;
		}
		puttr(&tr);
	} while (gettr(&tr));
	
	return(CWP_Exit());
}

