/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURANDSTAT: $Revision: 1.5 $ ; $Date: 2011/11/16 23:16:23 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <time.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SURANDSTAT - Add RANDom time shifts STATIC errors to seismic traces	",
"									",
"     surandstat <stdin >stdout  [optional parameters]	 		",
"									",
" Required parameters:							",
"	none								",
" Optional Parameters:							",
" 	seed=from_clock    	random number seed (integer)            ",
"	max=tr.dt 		maximum random time shift (ms)		",
"	scale=1.0		scale factor for shifts			",
"									",
NULL};

/* Credits:
 *	U Houston: Chris Liner c. 2009
 */

/************************ end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;		/* number of samples on output trace	*/
	float dt;	/* sample rate on outpu trace		*/
	int max;		/* max time shift (ms) */
	unsigned int seed;      /* random number seed */
	int it;			/* time sample counter */
	int itr;		/* trace count */
	int its;		/* local shift in time samples */
	float scale;		/* scale factor for shifts */


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt   = tr.ns;
	dt   = ((double) tr.dt)/1000000.0;
	
	/* Get parameters */
	if (!getparint("max", &max))    max = (int) dt*1000.0;
	if (!getparfloat("scale", &scale))    scale = 1.0;


	/* Set seed */
	if (!getparuint("seed", &seed)) { /* if not supplied, use clock */
		if (-1 == (seed = (unsigned int) time((time_t *) NULL))) {
			err("time() failed to set seed");
		}
	}
        checkpars();

	/* Loop on traces */	
	itr = 0;
	do {
		sranuni(seed);
		/* random shift (ms) for this trace */
		its = scale*2*max*(0.5 - franuni()) + 1;

		if (its <= 0 ) {
			/* loop over output times */
			for (it=0; it<nt-its; ++it) {
				tr.data[it] = tr.data[it-its];	
			}
		} else {
			/* loop over output times */
			for (it=nt; it>its; --it) {
				tr.data[it] = tr.data[it-its];	
			}

		}
		puttr(&tr);

		++itr;

	} while (gettr(&tr));


	return(CWP_Exit());
}
