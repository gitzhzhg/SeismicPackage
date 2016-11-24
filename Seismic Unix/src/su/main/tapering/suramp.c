/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SURAMP: $Revision: 1.13 $ ; $Date: 2011/11/16 23:33:10 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SURAMP - Linearly taper the start and/or end of traces to zero.	",
" 									",
" suramp <stdin >stdout [optional parameters]				",
" 									",
" Required parameters:							",
" 	if dt is not set in header, then dt is mandatory		",
" 							       		",
" Optional parameters							",
"	tmin=tr.delrt/1000	end of starting ramp (sec)		",
"	tmax=(nt-1)*dt		beginning of ending ramp (sec)		",
" 	dt = (from header)	sampling interval (sec)			",
" 									",
" The taper is a linear ramp from 0 to tmin and/or tmax to the		",
" end of the trace.  Default is a no-op!				",
" 									",
NULL};

/* Credits:
 *
 *	CWP: Jack K. Cohen, Ken Larner 
 *
 * Trace header fields accessed: ns, dt, delrt
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;			/* number of sample points on traces	*/
	float dt;		/* time sampling interval		*/
	float *taper1=NULL;	/* vector of taper weights (up ramp)	*/
	float *taper2=NULL;	/* vector of taper weights (down ramp)	*/
	int ntaper1;		/* number of taper weights (up ramp)	*/
	int ntaper2;		/* number of taper weights (down ramp)	*/
	float tmin;		/* end of up ramp			*/
	float tmax;		/* start of down ramp			*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
	nt = (int) tr.ns;
	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
	if (!dt) err("dt field is zero and not getparred");


	/* Get parameters */
	if (!getparfloat("tmin", &tmin))  tmin = tr.delrt/1000.0;
	if (!getparfloat("tmax", &tmax))	tmax = (nt - 1)*dt;
        checkpars();
	
	/* Make sure that ntaper1 and ntaper2 never < 0 */
	ntaper1 = NINT(MAX(0,(tmin - tr.delrt/1000.0)/dt));
	ntaper2 = NINT(MAX(0,(tr.delrt/1000.0 + (nt - 1)*dt - tmax )/dt));

	/* Set up taper weights */
	if (ntaper1) {
		register int i;
		taper1 = ealloc1float(ntaper1);
		for (i = 0; i < ntaper1; ++i)
			taper1[i] = (float) (i+1)/ntaper1;
	}
	if (ntaper2) {
		register int i;
		taper2 = ealloc1float(ntaper2);
		for (i = 0; i < ntaper2; ++i)
			taper2[i] = (float) (ntaper2 - i)/ntaper2;
	}
						
	

	/* Main loop over traces */
	do {
		register int i;
		if (ntaper1) {
			for (i = 0; i < ntaper1; ++i)
				tr.data[i] *= taper1[i];
		}

		if (ntaper2) {
			for (i = 0; i < ntaper2; ++i)
				tr.data[nt - ntaper2 + i] *= taper2[i];
		}

		puttr(&tr);
	} while (gettr(&tr));

	return(CWP_Exit());
}
