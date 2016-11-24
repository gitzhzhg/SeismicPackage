/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUHILB: $Revision: 1.19 $ ; $Date: 2011/11/12 00:42:19 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"								",
" SUHILB - Hilbert transform					",
"								",
" suhilb <stdin >sdout 						",
"							        ",
" Note: the transform is computed in the direct (time) domain   ",
"							        ",
NULL};

/* Credits:
 *	CWP: Jack Cohen   
 *      CWP: John Stockwell, modified to use Dave Hale's hilbert() subroutine
 *
 * Trace header fields accessed: ns, trid
 */
/**************** end self doc ***********************************/


segy tr;

int
main(int argc, char **argv)
{
	int nt;			/* number of points on input trace	*/
	float *data;		/* data values from each trace		*/
	float *hdata;		/* Hilbert transformed data values	*/
	register int i;		/* counter				*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */
	if (!gettr(&tr)) err ("can't get first trace");
	nt = tr.ns;

	/* Check that data is correct format */
	seismic = ISSEISMIC(tr.trid);
	if (!seismic)
		warn("input is not seismic data, trid=%d", tr.trid);

	/* allocate space for data and hdata */
	data = ealloc1float(nt);
	hdata = ealloc1float(nt);

	/* Loop over traces */
	do {

		/* read data from trace */
		for (i = 0; i < nt; i++) data[i] = tr.data[i];

		/* apply the Hilbert tranform */		
		hilbert(nt,data,hdata);

		/* put Hilbert tranformed data back in trace */
		for (i = 0; i < nt; i++) tr.data[i] = hdata[i];
		
		puttr(&tr);

	} while (gettr(&tr));


	return(CWP_Exit());
}
