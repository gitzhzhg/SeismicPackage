/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUZERO: $Revision: 1.14 $ ; $Date: 2011/11/16 17:23:05 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUZERO -- zero-out (or set constant) data within a time window	",
" 								",
" suzero itmax= < indata > outdata				",
" 								",
" Required parameters						",
" 	itmax=		last time sample to zero out		",
" 								",
" Optional parameters						",
" 	itmin=0		first time sample to zero out		",
" 	value=0		value to set				",
" 								",
" See also: sukill, sumute					",
" 								",
NULL};

/* Credits:
 *	CWP: Chris
 *	Geocon: Garry Perratt (added value= option)
 *
 * Trace header fields accessed: ns
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int itmin;		/* first sample to zero out		*/
	int itmax;		/* last sample to zero out	 	*/
	float value;		/* value to set within window		*/
	int nt;			/* time samples per trace in input data	*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;

	/* Get params from user */
	MUSTGETPARINT("itmax", &itmax);
	if (!getparint("itmin", &itmin))	itmin = 0;
	if (!getparfloat("value", &value))	value = 0.0;
        checkpars();

	/* Error checking */
	if (itmax > nt)    err("itmax = %d, must be < nt", itmax);
	if (itmin < 0)     err("itmin = %d, must not be negative", itmin);
	if (itmax < itmin) err("itmax < itmin, not allowed");

	/* Main loop over traces */
	do { 
		register int i;
		for (i = itmin; i <= itmax; ++i)  tr.data[i] = value;
		
		puttr(&tr);
	} while(gettr(&tr));


	return(CWP_Exit());
}
