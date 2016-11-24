/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUUNPACK2: $Revision: 1.18 $ ; $Date: 2011/11/12 00:01:04 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUUNPACK2 - unpack segy trace data from shorts to floats	",
"								",
"    suunpack2 <packed_file >unpacked_file			",
"								",
" suunpack2 is the approximate inverse of supack2		",
"								",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Shuki Ronen, Brian Sumner
 *
 * Revised:  7/4/95 Stewart A. Levin  Mobil
 *          Changed decoding to parallel 2 byte encoding of supack2
 *
 * Caveats:
 *	This program is for single site use with supack2.  See the
 *	supack2 header comments.
 *
 * Notes:
 *	ungpow and unscale are defined in segy.h
 *	trid = SHORTPACK is defined in su.h and segy.h
 *
 * Trace header fields accessed: ns, trid, ungpow, unscale
 * Trace header fields modified:     trid, ungpow, unscale
 */
/**************** end self doc ***********************************/

segy tr;	/* on input: SEGY hdr & (short) trace data */
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
	if (tr.trid != SHORTPACK) err("Not short packed traces");
	nt = tr.ns;
	ungpow = tr.ungpow;
	isone = CLOSETO(ungpow, f_one);
	istwo = CLOSETO(ungpow, f_two);


	/* Main loop over segy traces */
	do {
		/* Point input short trace at the trace data and unpack.
		 * Since the floats take more room than the shorts,
		 * we load in from back end.
		 *
		 * Note that the segy field tr.data is declared as
		 * floats, so we need to invent a pointer for the
		 * 2 byte short array which is actually there.
		 * The essence of the code in the for loops below is:
		 * for (i = nt-1; i >= 0; --i) { 
                 *      val = (float) itr[i];
		 *      val *= tr.unscale;
		 *	tr.data[i] = ... ;
		 * }
		 * but, as in supack2, this isn't portable */
		register int i, j;
		register signed int si;
		register unsigned int ui;
		register float val;
		register unsigned char *itr = (unsigned char *) tr.data;

		if (istwo) {
			for (i = nt-1, j=2*nt-1; i >= 0; --i) { 
				ui = itr[j--];
				ui |= (itr[j--])<<8;
				si = (ui>32767)?ui-65536:ui;
				val = (float) si;
				val *= tr.unscale;
				tr.data[i] = val * ABS(val);
			}
		} else if (isone) {
			for (i = nt-1, j=2*nt-1; i >= 0; --i) { 
				ui = itr[j--];
				ui |= (itr[j--])<<8;
				si = (ui>32767)?ui-65536:ui;
				val = (float) si;
				val *= tr.unscale;
				tr.data[i] = val;
			}
		} else {
			for (i = nt-1, j=2*nt-1; i >= 0; --i) { 
				ui = itr[j--];
				ui |= (itr[j--])<<8;
				si = (ui>32767)?ui-65536:ui;
				val = (float) si;
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
