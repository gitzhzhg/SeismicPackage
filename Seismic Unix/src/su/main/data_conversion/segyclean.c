/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SEGYCLEAN: $Revision: 1.13 $ ; $Date: 2011/11/12 00:01:45 $		*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SEGYCLEAN - zero out unassigned portion of header		",
" 								",
" segyclean <stdin >stdout 					",
"								",
" Since \"foreign\" SEG-Y tapes may use the unassigned portion	",
" of the trace headers and since SU now uses it too, this	",
" program zeros out the fields meaningful to SU.		",
" 								",
"  Example:							",
"  	segyread trmax=200 | segyclean | suximage		",
"								",
"								",
NULL}; 

/* Credits:
 *	CWP: Jack Cohen
 *
 */
/**************** end self doc ********************************/


segy tr;

int
main(int argc, char **argv)
{

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	while (gettr(&tr)) {
		tr.f1 = 0.0;
		tr.d1 = 0.0;
		tr.f2 = 0.0;
		tr.d2 = 0.0;
		tr.ungpow = 0.0;
		tr.unscale = 0.0;
		tr.ntr = 0;
		tr.mark = 0;

		puttr(&tr);
	}


	return(CWP_Exit());
}
