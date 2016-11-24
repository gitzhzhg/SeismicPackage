/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSHIFT: $Revision: 1.9 $ ; $Date: 2012/12/03 23:12:39 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUSHIFT - shifted/windowed traces in time				",
"									",
" sushift <stdin >stdout [tmin= ] [tmax= ]				",
"									",
" tmin=			min time to pass				",
" tmax=			max time to pass				",
" dt=                    sample rate in microseconds 			",
" fill=0.0               value to place in padded samples 		",
"									",
" (defaults for tmin and tmax are calculated from the first trace.	",
" verbose=		1 echos parameters to stdout			",
"									",
" Background :								",
" tmin and tmax must be given in seconds				",
"									",
" In the high resolution single channel seismic profiling the sample 	",
" interval is short, the shot rate and the number of samples are high.	",
" To reduce the file size the delrt time is changed during a profiling	",
" trip. To process and display a seismic section a constant delrt is	",
" needed. This program does this job.					",
"									",
" The SEG-Y header variable delrt (delay in ms) is a short integer.	",
" That's why in the example shown below delrt is rounded to 123 !	",
"									",
"   ... | sushift tmin=0.1234 tmax=0.2234 | ...				",
"									",
" The dt= and fill= options are intended for manipulating velocity	",
" volumes in trace format.  In particular models which were hung	",
" from the water bottom when created & which then need to have the	",
" water layer added.							",
"									",
NULL};

/*
 * Author:
 *
 * Toralf Foerster
 * Institut fuer Ostseeforschung Warnemuende
 * Sektion Marine Geologie
 * Seestrasse 15
 * D-18119 Rostock, Germany
 *
 * Trace header fields accessed: ns, delrt
 * Trace header fields modified: ns, delrt
 */
/**************** end self doc ********************************/

segy tr;

int
main (int argc, char **argv)
{
	short verbose;		/* if 1(yes) echo parameters to stderr  */
	float tmin;		/* minimum time to pass			*/
	float tmax;		/* maximum time to pass			*/
/*	unsigned short ns_out;*/	/* number of time samples on output */
	unsigned ns_out;	/* number of time samples on output     */
	int utmin,utmax;	/* tmin and tmax i usec			*/
	int ut1_in;		/* start time on input trace in usec	*/
	int ut2_in;		/* end time on input trace in usec	*/
        int dt;
        float fill;


	/* initialize arguments */
	initargs (argc, argv);
	requestdoc (1);


	/* get parameters from first trace */
	if (!gettr (&tr)) err ("can't get first trace");
        if( !getparint("dt" ,&dt ) ){
           dt=tr.dt;
        }
        if( !getparfloat( "fill" ,&fill ) ){
           fill=0.0;
        }

	if (getparfloat ("tmin", &tmin)) utmin =  NINT(1e06 * tmin);
	else	 utmin = 1e03 * tr.delrt;

	if (getparfloat ("tmax", &tmax)) utmax = NINT(1e06 * tmax);
	else utmax = utmin + tr.ns * dt;

	if (!getparshort ("verbose", &verbose)) verbose = 0;
        checkpars();

	/* check utmin, delrt */
	if (utmin >= utmax) err ("utmin (%i) >= utmax (%i) ?!", utmin, utmax);
	if ( NINT(1e-03 * utmin) > 32767 || NINT(1e-03 * utmin) < -32768)
		err (" delrt exceeds short");

	/* set output number of samples */
	ns_out = (utmax - utmin) / dt;
	if (verbose) warn ("IN %u samples  OUT %u samples", tr.ns, ns_out);
/* sushift.c:105: warning: comparison is always false due to limited range of data type */
	if (ns_out > SU_NFLTS)
		err ("ns_out=%u exceeds SU_NFLTS=%u", ns_out, SU_NFLTS);

	/* Main loop over traces */
	do {
		/* actual start/end time of input trace */
		ut1_in = 1000 * tr.delrt;
		ut2_in = ut1_in + dt * tr.ns;

		if (ut1_in != utmin || ut2_in != utmax) {
			int i;
			int it,it1,it2;	/* ..sample index */
			int ut1,ut2,ut;	/* times in usec  */
			float  *temp;

			ut = ut1 = ut2 = 0;

			/*
			 * trace fully/partially inside/outside of the time
			 * window ?
			 */
			if (ut1_in < utmin && ut2_in <= utmax) {
				ut1 = utmin - ut1_in;
				ut = ut2_in - utmin;
			} else if (ut1_in < utmin && ut2_in > utmax) {
				ut1 = utmin - ut1_in;
				ut = utmax - utmin;
			} else if (ut1_in >= utmin && ut2_in <= utmax) {
				ut2 = ut1_in - utmin;
				ut = ut2_in - ut1_in;
			} else if (ut1_in >= utmin && ut2_in > utmax) {
				ut2 = ut1_in - utmin;
				ut = utmax - ut1_in;
			} else {
				err (" internal error : ut1=%i ut2=%i ut=%i ut1_in=%i ut2_in=%i",
					ut1, ut2, ut, ut1_in, ut2_in);
			}
			it1 = ut1 / dt;
			it2 = ut2 / dt;
			it = ut / dt;

			temp = ealloc1float ( ns_out);
			/* make a filled temp trace	 */
			for (i = 0; i < ns_out; i++)
				temp[i] = fill;

			/*
			 * copy only the data inside of the time window to
			 * the temp trace
			 */
			for (i = 0; i < it; i++)
				temp[it2 + i] = tr.data[it1 + i];

			/* copy the full temp trace to the tr struct */
			for (i = 0; i < ns_out; i++)
				tr.data[i] = temp[i];

			free (temp);
		}
		tr.delrt = (short) (1e-03 * utmin);
		tr.ns = ns_out;

		puttr (&tr);

	} while (gettr (&tr));

	return(CWP_Exit());
}
