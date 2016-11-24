/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUMIX: $Revision: 1.11 $ ; $Date: 2011/11/16 23:09:52 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUMIX - compute weighted moving average (trace MIX) on a panel	",
"	  of seismic data						",
"									",
" sumix <stdin >sdout 							",
" mix=.6,1,1,1,.6	array of weights for weighted average		",
"									",
"									",
" Note: 								",
" The number of values defined by mix=val1,val2,... determines the number",
" of traces to be averaged, the values determine the weights.		",
" 									",
" Examples: 								",
" sumix <stdin mix=.6,1,1,1,.6 >sdout 	(default) mix over 5 traces weights",
" sumix <stdin mix=1,1,1 >sdout 	simple 3 trace moving average	",
" 									",
NULL};

/* Author:
 *	CWP: John Stockwell, Oct 1995
 *
 * Trace header fields accessed: ns
 */
/**************** end self doc ***********************************/


/* default weighting values */
#define VAL0	0.6
#define VAL1	1.0
#define VAL2	1.0
#define VAL3	1.0
#define VAL4	0.6


segy tr;

int
main(int argc, char **argv)
{
	int nmix;		/* number of traces to mix over		*/
	int imix;		/* mixing counter			*/
	int it;			/* sample counter			*/
	int nt;			/* number of time samples per trace	*/
	int itr=0;		/* trace counter			*/
	size_t databytes;	/* number of bytes (nt*FSIZE)		*/
	size_t mixbytes;	/* number of bytes (nt*FSIZE*nmix)	*/
	float *mix;		/* array of mix values			*/
	float *temp;		/* temp array for mixing 		*/
	float **data;		/* array for mixing 			*/
	
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */
	if(!gettr(&tr))
		err("can't get first trace");
	nt = tr.ns;

	/* Get mix weighting values values */
	if ((nmix = countparval("mix"))!=0) {
		mix = ealloc1float(nmix);
		getparfloat("mix",mix);
		
	} else {
		nmix = 5;
		mix = ealloc1float(nmix);

		mix[0] = VAL0;
		mix[1] = VAL1;
		mix[2] = VAL2;
		mix[3] = VAL3;
		mix[4] = VAL4;
	}

        checkpars();

	/* Divide mixing weight by number of traces to mix */
	for (imix = 0; imix < nmix; ++imix)
		mix[imix]=mix[imix]/((float) nmix);

	/* Compute databytes per trace and bytes in mixing panel */
	databytes = FSIZE*nt;
	mixbytes = databytes*nmix;

	/* Allocate temporary space for mixing  */
	data = ealloc2float(nt,nmix);
	temp = ealloc1float(nt);

	/* Zero out data array */
	memset((void *) data[0], 0, mixbytes);

	/* Loop over remaining traces */
	do {

		++itr;

		/* Zero out temp */
		memset((void *) temp, 0, databytes);

		/* Read data portion of trace into first column of data[][] */
		memcpy( (void *) data[0], (const void *) tr.data, databytes);
	
		/* Loop over time samples */
		for (it=0; it<nt; ++it) {

			/* Weighted moving average (mix) */
			for(imix=0; imix<nmix; ++imix)
				temp[it]+=data[imix][it]*mix[imix];

			/* put mixed data back in seismic trace */
			tr.data[it] = temp[it]; 
		}

		/* Bump columns of data[][] over by 1 */
		/* to make space for data from next trace */
		for (imix=nmix-1; 0<imix; --imix)
			for (it=0; it<nt; ++it) 
				data[imix][it] = data[imix-1][it];

				
		puttr(&tr);
	} while (gettr(&tr)); 

	return(CWP_Exit());

}
