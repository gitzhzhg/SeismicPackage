/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUNULL: $Revision: 1.16 $ ; $Date: 2015/06/02 20:15:23 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUNULL - create null (all zeroes) traces	 		",
" 								",
" sunull nt=   [optional parameters] >outdata			",
" 								",
" Required parameter						",
" 	nt=		number of samples per trace		",
" 								",
" Optional parameters						",
" 	ntr=5		number of null traces to create		",
" 	dt=0.004	time sampling interval			",
" 								",
" Rationale: It is sometimes useful to insert null traces	",
"	 between \"panels\" in a shell loop.			",
" 								",
" See also: sukill, sumute, suzero				",
" 								",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen
 *
 * Trace header fields set: ns, dt, tracl
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;			/* number of time samples		*/
	int ntr;		/* number of traces			*/
	register int itr;	/* trace counter			*/
	float dt;		/* time sampling interval (seconds)	*/
	int idt;		/* 	...		(micro seconds)	*/
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(0);

	/* Get parameters */
	MUSTGETPARINT("nt", &nt);
	CHECK_NT("nt",nt);
        tr.ns = nt;

	if (!getparint("ntr", &ntr))	ntr = 5;
	if (!getparfloat("dt", &dt))	dt = .004;
	idt = NINT(1000000.0 * dt);

	/* Set tr.data to zeros */
	memset( (void *) tr.data, 0, nt*FSIZE);

	/* Set constant header fields */
	tr.dt = idt;
	tr.ns = nt;

	/* Main loop over traces */
	for (itr = 0; itr < ntr; ++itr) {

		tr.tracl = itr + 1;

		puttr(&tr);
	}

	return(CWP_Exit());
}
