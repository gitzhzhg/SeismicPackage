/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUKILL: $Revision: 1.18 $ ; $Date: 2011/11/17 00:03:38 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUKILL - zero out traces					",
" 								",
" sukill <stdin >stdout [optional parameters]			",
" 								",
" Optional parameters:						",
"	key=trid	header name to select traces to kill	",
"	a=2		header value identifying tracces to kill",
" or								",
" 	min= 		first trace to kill (one-based)		",
" 	count=1		number of traces to kill 		",
" 								",
" Notes:							",
"	If min= is set it overrides selecting traces by header.	",
" 								",
NULL};

/* Credits:
 *	CWP: Chris Liner, Jack K. Cohen
 *	header-based trace selection: Florian Bleibinhaus
 *
 * Trace header fields accessed: ns
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	cwp_String key;		/* trace header			*/
	cwp_String type;	/* type for trace header	*/
	int index;		/* index of trace header	*/
	Value val;		/* trace header value		*/
	double dval,a;		/* trace header value		*/
	register int itr;	/* trace counter		*/
	int min;		/* first trace to zero out	*/
	int count;		/* number of traces to zero out	*/
	int nt = 0;		/* number of time samples	*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters */
	if (!getparint("min", &min)) min = 0;
	if (min < 0) err("min = %d, must be >= 1", min);
	if (!getparint("count", &count))  count = 1;
	if (!getparstring("key", &key)) key = "trid";
	if (!getpardouble("a", &a)) a = 2.;
        checkpars();

	/* Get type and index value */
	type  = hdtype(key);
	index = getindex(key);


	if ( min>0 ) {

	/* Echo traces till min if they are there */
	for (itr = 1; itr < min; ++itr) {
		if (gettr(&tr)) puttr(&tr);
		else err("failed to get requested trace #%ld", itr);
	}

	/* Kill "count" traces if they are there
	 * Do first outside loop to get nt    */
	if (gettr(&tr)) {
		nt = tr.ns;
		memset( (void *) tr.data, 0, nt * FSIZE);
		puttr(&tr);
		++itr;
	} else err("failed to get requested trace #%ld", itr);

	for ( ; itr < min + count; ++itr) {
		if (gettr(&tr)) {
			memset( (void *) tr.data, 0, nt * FSIZE);
			puttr(&tr);
		} else err("failed to get requested trace #%ld", itr);
	}

	/* Echo the trailing traces if any */
	while (gettr(&tr)) {
		puttr(&tr);
	}

	} else {	/* select traces by header value */
		while (gettr(&tr)) {
			nt = tr.ns;
			gethval(&tr, index, &val);
			dval = vtod(type, val);
			if ( dval==a ) memset( (void *) tr.data, 0, nt*FSIZE);
			puttr(&tr);
		}
	}


	return(CWP_Exit());
}
