/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUJITTER: $Revision: 1.3 $ ; $Date: 2011/11/12 00:22:43 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <time.h>

/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUJITTER - Add random time shifts to seismic traces			",
"									",
"     sujitter <stdin >stdout  [optional parameters]	 		",
"									",
" Required parameters:							",
"	none								",
" Optional Parameters:							",
" 	seed=from_clock    	random number seed (integer)            ",
"	min=1 			minimum random time shift (samples)	",
"	max=1 			maximum random time shift (samples)	",
"	pon=1 			shift can be positive or negative	",
"				=0 shift is positive only		",
"	fldr=0 			each trace has new shift		",
"				=1 new shift when fldr header field changes",
" Notes:								",
" Useful for simulating random statics. See also:  suaddstatics		",
"									",
NULL};

/* Credits:
 *	U of Houston: Chris Liner 
 *	UH:  Chris added fldr, min, pon options 12/10/08
 */

/************************ end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;		/* number of samples on output trace	*/
	float dt;	/* sample rate on outpu trace		*/
	int min;		/* min time shift (samples) */
	int max;		/* max time shift (samples) */
	unsigned int seed;      /* random number seed */
	int it;			/* time sample counter */
	int itr;		/* trace counter */
	int its=0;		/* local shift in time samples */
	int sits;		/* sign of local shift */
	int fldr;		/* fldr use flag		*/
	int ishot;		/* shot counter (based on tr.fldr) */
	int ishotold;
	int pon;		/* flag for pos or neg shift */


	/* Hook up getpar */
	initargs(argc, argv);
	requestdoc(1);

	/* Get information from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt   = tr.ns;
	ishotold = tr.fldr;
	dt   = ((double) tr.dt)/1000000.0;
	
	/* Get parameters */
	if (!getparint("min", &min))    min = 1;
	if (!getparint("max", &max))    max = 1;
	if (!getparint("pon", &pon))    pon = 1;
	if (!getparint("fldr", &fldr))  fldr = 0;

	if (min>max) err("min>max... exit");

	/* Set seed */
	if (!getparuint("seed", &seed)) { /* if not supplied, use clock */
		if (-1 == (seed = (unsigned int) time((time_t *) NULL))) {
			err("time() failed to set seed");
		}
	}
	sranuni(seed);

	/* Loop on traces */	
	itr = 1;
	do {

		/* get fldr (shot) number */
		ishot = tr.fldr;

		if (itr==1) {
			/* initial shift for shot 1 
			   (used if fldr==1) */	
	       		its = min + (max-min)*franuni();
			if (pon==1) {
				/* include random sign to shift */
				sits = SGN(franuni()-0.5);
				its = its * sits;
			}
		}
		
		if (fldr==0) {
			/* each trace gets random shift */
	       		its = min + (max-min)*franuni();
			if (pon==1) {
				/* include random sign to shift */
				sits = SGN(franuni()-0.5);
				its = its * sits;
			}
		}

		if (fldr==1 && ishot!=ishotold) {
			/* new shot needs new shift */
	       		its = min + (max-min)*franuni();
			if (pon==1) {
				/* include random sign to shift */
				sits = SGN(franuni()-0.5);
				its = its * sits;
			}
			ishotold = ishot;
		}

		/* apply shift and output trace */
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

		itr += 1;

	} while (gettr(&tr));


	return(CWP_Exit());
}
