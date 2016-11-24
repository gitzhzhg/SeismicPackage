/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUCENTSAMP - CENTRoid SAMPle seismic traces			",
" 								",
"  sucentsamp <stdin [optional parameters] >sdout  		",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	dt=from header		sampling interval		",
"	verbose=1		=0 to stop advisory messages	",
" 								",
" Notes: 							",
" This program takes seismic traces as input, and returns traces",
" consisting of spikes of height equal to the area of each lobe ",
" of each oscillation, located at the centroid of the lobe in	",
" question. The height of each spike equal to the area of the   ",
" corresponding lobe.						",
" 								",
" Caveat: No check is made that the data are real time traces!	",
" 								",
NULL};

/* Credits:
 *
 *	Providence Technologies: Tom Morgan 
 *
 * Trace header fields accessed: ns, dt
 */
/**************** end self doc ***********************************/

segy tr;

int
main(int argc, char **argv)
{
	register float *rt;	/* real trace				*/
	register float *mt;	/* magnitude trace			*/
	register float *ct;	/* resampled centroid trace 		*/

	int nt;			/* number of points on input trace	*/
	int verbose;		/* flag to get advisory messages	*/
	float dt;		/* sampling interval in secs		*/
	float invdt;		/* inverse dt				*/
	float hdt;		/* half dt				*/

	cwp_Bool inflect=cwp_false;	/* inflection point flag	  */
	cwp_Bool zero_cross=cwp_false; /* zero-crossing flag		*/
	cwp_Bool max_passed=cwp_false; /* maximum value passed flag	*/

	float *time;		/* array of trace sample time values	*/
	float sum_amp;	  /* sum of amplitudes in lobe		*/
	float t_cen;		/* centroid about amplitude axis	*/

	int isamp;		/* t_cen time sample number		*/

	float a_cen;		/* centroid about time axis		*/
	float a_mom;		/* moment about time axis		*/
	float t_mom;		/* moment about amplitude axis	  */
	float a_height;	 /* height of region for moment calc	*/
	float t_width;	  /* width of region for moment calc	*/

	int first;		/* number of first sample in lobe	*/
	int last;		/* number of last sample in lobe	*/
	int prev;		/* number of past sample in amp moment  */

	int small;		/* sample number of current smaller mag */
	int nvals;		/* number of samples in current lobe	*/
	int nvals_min;	  /* minimum samples in lobe for inclusion*/

	int i,k;		/* counter indices			*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	if (!getparint("verbose", &verbose))	verbose=1;

	if (!getparint("nvals_min", &nvals_min))	nvals_min = 1;

	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;


	/* dt is used only to set output header value d1 */
	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
        checkpars();

	if (!dt) {
		dt = .004;
		if (verbose) warn("dt not set, assumed to be .004");
	}
	invdt = 1.0 / dt;
	hdt = 0.5 * dt;


	/* Allocate space */
	rt = ealloc1float(nt);
	ct = ealloc1float(nt);
	mt = ealloc1float(nt);
	time = ealloc1float(nt);

	/* create an array of times */
	for (i = 0; i < nt; ++i) {
		time[i] = (float)(i + 1) * dt;
	}

	/* Main loop over traces */
	do {
	 	register int i;

		/* Load trace into rt and zero ct */
		memcpy((void *) rt, (const void *) tr.data, nt*FSIZE);
		memset((void *) ct, 0, nt*FSIZE);

		first = 0;
		mt[0] = fabs(rt[0]);
		sum_amp = rt[0];
		t_mom = time[0] * mt[0];

		for (i = 1; i < nt; ++i) {
			mt[i] = fabs(rt[i]);

			/* test for zero-crossing or inflection point */
			if(rt[i] * rt[i-1] > (float)0) {
				if(mt[i] > mt[i-1]) {
					if(max_passed) inflect = cwp_true;
			} else {
				max_passed = cwp_true;
			}
		} else {
			zero_cross = cwp_true;
		}

		/* if a zero-crossing or inflection point is not	*/
		/* encountered on the current trace sample,		*/
		/* accumulate the time moment				*/
		/* and sum of the lobe amplitude			*/
		if(!zero_cross && !inflect) {
			sum_amp = sum_amp + rt[i];
			t_mom = t_mom + (time[i] * mt[i]);
		} else {

			/* otherwise a zero-crossing or inflection has	*/
			/* occured, so stop and determine amplitude 	*/
			/* centroid and store results as a centroid	*/
			/* sample for the current lobe		   */

			/* determine the amplitude centroid */
			last = i - 1;

			/* if inflection point has been found divide it */
			/* between the calcs for this lobe and the next */
			if (inflect) {
				last = i;
				rt[last] = rt[last] * 0.5;
				mt[last] = fabs(rt[last]);
				sum_amp = sum_amp + rt[last];
				t_mom = t_mom + (time[last] * mt[last]);
			}

			nvals = last - first + 1;

			if(nvals == 1) {

				/* check to see if lobe is big enough	*/
				/* to be included			*/
				if(nvals >= nvals_min) {
		  			ct[i] = rt[i] * 0.5;
				}

				first = i;

				inflect = cwp_false;
				max_passed = cwp_false;
				zero_cross = cwp_false;

				sum_amp = rt[i];
				t_mom = time[i] * mt[i];
			} else {
				a_height = mt[first];

				if(mt[first] > mt[last]) {
		  			a_height = mt[last];
				}

				t_width = time[last] - time[first] + dt;
				a_mom = a_height * 0.5;
				a_cen = t_width * a_height * a_mom;
				small = first;

				if (mt[first] > mt[last]) small = last;
				for (k = 1; k < nvals; k ++) {
					prev = small;

		  			if (prev == first) first = first + 1;
		  			else last = last - 1; 

		  			small = first;
		  			if(mt[first] > mt[last]) small = last;

		  			a_height = rt[small] - rt[prev];
					a_mom = rt[prev] + (a_height * 0.5);
		  			t_width = t_width - dt;
		  			a_cen = a_cen + a_mom*t_width*a_height;
				}
				if(sum_amp == 0.0) {
					warn("i = %d, sum_amp = %f,divide by zero !",
						i,sum_amp);
				}
				a_cen = a_cen / (sum_amp * dt);

				/* determine the time centroid */
				t_cen = t_mom / fabs(sum_amp);

				/* start accumulating amplitude sum	*/
				/* and time moment for next lobe	*/
				sum_amp = rt[i];
				t_mom = time[i] * mt[i];
 
				/* set sample corresponding to t_cen	*/
				/* to amplitude a_cen 			*/
				isamp = (int) ((t_cen * invdt) + hdt);
		
				/* check to see if lobe is big enough	*/
				/* to be included			*/
				if(nvals >= nvals_min) {
					ct[isamp] = a_cen;
				}

				first = i;
				inflect = cwp_false;
				max_passed = cwp_false;
				zero_cross = cwp_false;
			}  /* end if block for case of nvals > 1 */
		}  /* end calc of new centroid */
	  }  /* end of loop down input trace */
		
	  /* Store values */

	  for (i = 0; i < nt; ++i) {
		tr.data[i] = ct[i];
	  }

	  /* Set header values  and write centroid trace */
	  tr.ns = nt;
	  puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
