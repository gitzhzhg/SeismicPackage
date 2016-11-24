/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPACK2: $Revision: 1.22 $ ; $Date: 2011/11/16 17:38:58 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUPACK2 - pack segy trace data into 2 byte shorts		",
"								",
" supack2 <segy_file >packed_file	gpow=0.5 		",
"								",
" Required parameters:						",
"	none							",
"						        	",
" Optional parameter: 						",
"	gpow=0.5	exponent used to compress the dynamic	",
"			range of the traces			",
"								",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Shuki Ronen, Brian Sumner
 *
 * Revised: 7/4/95  Stewart A. Levin  Mobil
 *          Changed encoding to ensure 2 byte length (short is
 *	    8 bytes on Cray).
 *
 * Caveats:
 *	This program is for single site use.  Use segywrite to make
 *	a portable tape.
 *
 *	We are storing the local header words, ungpow and unscale,
 *	required by suunpack2 as floats.
 *	
 * Notes:
 *	ungpow and unscale are defined in segy.h
 *	trid = SHORTPACK is defined in su.h and segy.h
 *
 * Trace header fields accessed: ns
 * Trace header fields modified: ungpow, unscale, trid
 */
/**************** end self doc ***********************************/


#define GPOW	0.5	/* default power parameter */

segy tr;	/* on  input: SEGY hdr & (float) trace data */
		/* on output: data as 2-byte shorts          */

int
main(int argc, char **argv)
{
	float gpow;
	int nt;
	cwp_Bool isone, ishalf;
	float f_one = 1.0;
	float f_half = 0.5;


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters */
	if (!getparfloat("gpow", &gpow)) gpow = GPOW;
        checkpars();
	if (gpow <= 0.0) err("gpow = %g must be positive", gpow);
	isone  = CLOSETO(gpow, f_one);
	ishalf = CLOSETO(gpow, f_half);

	/* Get number of time samples from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;

	/* Main loop over segy traces */
	do {
		/* Point output trace at the trace data and pack.
		 * Since the shorts take less room than the floats,
		 * we don't overwrite.
		 *
		 * Note that the segy field tr.data is declared as
		 * floats, so we need to invent a pointer for the
		 * short array which is actually there. */

		register unsigned char *otr = (unsigned char *) tr.data;
		register int i,j;
		register signed   int si;
		register unsigned int ui;
		register float absmax;
		register float scale;

		/* Power transform to decrease dynamic range */
		if (!isone) {
			register float val;

			if (ishalf) {
				for (i = 0; i < nt; ++i) {
					val = tr.data[i];
					tr.data[i] = (val >= 0.0) ?
						sqrt(val) : -sqrt(-val);
				}
			} else {
				for (i = 0; i < nt; ++i) {
					val = tr.data[i];
					tr.data[i] = (val >= 0.0) ?
					    pow(val, gpow) : -pow(-val, gpow);
				}
			}
		}

		/* Store "ungpow" factor */
		tr.ungpow = 1.0/gpow;

		/* Read trace data and get absmax */
		absmax = ABS(tr.data[0]);
		for (i = 1; i < nt; ++i)
			absmax = MAX(absmax, ABS(tr.data[i]));

		/* Compute scale factor and store "unscale" factor */
		/* If max is zero, then put scale and unscale to zero too */
		scale = absmax ? SHRT_MAX/absmax : 0.0;
		tr.unscale = absmax ? 1.0/scale : 0.0;

		/* Apply the scale and load in short data
		 * Note: the essence of the code below is:
		 * for (i = 0; i < nt; ++i) { 
		 *	tr.data[i] *= scale;
		 *      otr[i] = (short) tr.data[i];
		 * }
		 * but this assumes shorts are 2 bytes, so isn't portable */
		for (i = 0, j=0; i < nt; ++i) { 
			tr.data[i] *= scale;
			si = (signed int) tr.data[i];
			ui = (si>>8)&255;
			otr[j++] = (unsigned char) ui;
			ui = si&255;
			otr[j++] = (unsigned char) ui;
		}

		/* Write trace ID as the packed short code number */
		tr.trid = SHORTPACK;

		/* Output the "segy" with shorts in the data array */
		puttr(&tr);

	} while (gettr(&tr));

	
	return(CWP_Exit());
}
