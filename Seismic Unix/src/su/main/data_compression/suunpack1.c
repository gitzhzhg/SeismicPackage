/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUUNPACK1: $Revision: 1.19 $ ; $Date: 2011/11/12 00:01:04 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
"SUUNPACK1 - unpack segy trace data from chars to floats	",
"								",
"    suunpack1 <packed_file >unpacked_file			",
"								",
"suunpack1 is the approximate inverse of supack1		",
"								",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Shuki Ronen, Brian Sumner
 *
 * Caveats:
 *	This program is for single site use with supack1.  See the
 *	supack1 header comments.
 *
 * Notes:
 *	ungpow and unscale are defined in segy.h
 *	trid = CHARPACK is defined in su.h and segy.h
 *
 *
 * Trace header fields accessed: ns, trid, ungpow, unscale
 * Trace header fields modified:     trid, ungpow, unscale
 */
/**************** end self doc ***********************************/


segy tr;	/* on input: SEGY hdr & (signed char) trace data */
		/* on output: data is floats */

int
main(int argc, char **argv)
{
	float ungpow;
	int nt;
	cwp_Bool isone, istwo;
	float f_one = 1.0;
	float f_two = 2.0;

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	if (tr.trid != CHARPACK) err("Not char packed traces");
	nt = tr.ns;
	ungpow = tr.ungpow;
	isone = CLOSETO(ungpow, f_one);
	istwo = CLOSETO(ungpow, f_two);


	/* Main loop over segy traces */
	do {
		/* Point input char trace at the trace data and unpack.
		   Since the floats take more room than the chars,
		   we load in from back end.
		
		   Note that the segy field tr.data is declared as
		   floats, so we need to invent a pointer for the
		   char array which is actually there.
		*/

		register int i;
		register float val;
		register signed char *itr = (signed char *) tr.data;

		if (istwo) {
			for (i = nt-1; i >= 0; --i) { 
				val = (float) itr[i];
				val *= tr.unscale;
				tr.data[i] = val * ABS(val);
			}
		} else if (isone) {
			for (i = nt-1; i >= 0; --i) { 
				val = (float) itr[i];
				val *= tr.unscale;
				tr.data[i] = val;
			}
		} else {
			for (i = nt-1; i >= 0; --i) { 
				val = (float) itr[i];
				val *= tr.unscale;
				tr.data[i] = (val >= 0.0) ?
					pow(val, ungpow) : -pow(-val, ungpow);
			}
		}


		/* Mark as seismic data and remove now unnecessary fields */
		tr.trid = 1;
		tr.ungpow = 0.0;
		tr.unscale = 0.0;


		/* Write out restored (unpacked) segy */
		puttr(&tr);


	} while (gettr(&tr));


	return(CWP_Exit());
}
