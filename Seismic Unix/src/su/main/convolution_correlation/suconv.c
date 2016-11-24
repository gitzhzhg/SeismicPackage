/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUCONV: $Revision: 1.19 $ ; $Date: 2015/08/07 21:56:55 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUCONV - convolution with user-supplied filter			",
" 									",
" suconv <stdin >stdout  filter= [optional parameters]			",
" 									",
" Required parameters: ONE of						",
" sufile=		file containing SU trace to use as filter	",
" filter=		user-supplied convolution filter (ascii)	",
" 									",
" Optional parameters:							",
" panel=0		use only the first trace of sufile		",
" 			=1 convolve corresponding trace in sufile with	",
" 			trace in input data				",
" 									",
" Trace header fields accessed: ns					",
" Trace header fields modified: ns					",
" 									",
" Notes: It is quietly assumed that the time sampling interval on the	",
" single trace and the output traces is the same as that on the traces	",
" in the input file.  The sufile may actually have more than one trace,	",
" but only the first trace is used in panel=0. In panel=1 the corresponding",
" trace from the sufile are convolved with its counterpart in the data.	",
" Caveat, in panel=1 there have to be at least as many traces in sufile	",
" as in the input data. If not, a warning is returned, and later traces	",
" in the dataset are returned unchanged.				",
" 									",
" Examples:								",
"	suplane | suwind min=12 max=12 >TRACE				",
"	suconv<DATA sufile=TRACE | ...					",
" Here, the su data file, \"DATA\", is convolved trace by trace with the",
" the single su trace, \"TRACE\".					",
" 									",
"	suconv<DATA filter=1,2,1 | ...					",
" Here, the su data file, \"DATA\", is convolved trace by trace with the",
" the filter shown.							",
" 									",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Michel Dietrich
 *
 *  CAVEATS: no space-variable or time-variable capacity.
 *     The more than one trace allowed in sufile is the
 *     beginning of a hook to handle the spatially variant case.
 *
 * Trace header fields accessed: ns
 * Trace header fields modified: ns
 */
/**************** end self doc *******************************************/

segy intrace, outtrace, sutrace;

int
main(int argc, char **argv)
{
	int nt;			/* number of points on input traces	*/
	int ntout;		/* number of points on output traces	*/
	float *filter=NULL;	/* filter coefficients			*/
	int nfilter=0;		/* length of input wavelet in samples	*/
	cwp_String sufile="";	/* name of file containing one SU trace */
	FILE *fp=NULL;		/* ... its file pointer			*/
	int delrtf=0;		/* delrt from traces in sufile		*/

	int panel;		/* operational panel of the program	*/
	cwp_Bool issufile=cwp_false;	/* is sufile set?		*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	if (!getparint("panel",&panel))		panel=0;

	/* Get info from first trace */ 
	if (!gettr(&intrace) ) err("can't get first trace");
	nt = intrace.ns;

	/* Get parameters and set up filter array */
	if (!getparstring("sufile", &sufile) ) {
		if (!(nfilter = countparval("filter")))
			err("must specify filter= desired filter");
		filter = ealloc1float(nfilter);	getparfloat("filter", filter);
	} else if (panel==0) { /* if panel=0 only use the first trace */
		fp = efopen(sufile, "r");
		fgettr(fp, &sutrace);

                /* HD: get delrt from filter sufile */
                delrtf = sutrace.delrt;

		nfilter = sutrace.ns;
		filter = ealloc1float(nfilter);
		memcpy((void *) filter,
			(const void *) sutrace.data, nfilter*FSIZE);
		issufile=cwp_true; 
	} else { /* if panel=1 use each trace */
		fp = efopen(sufile, "r");
		issufile=cwp_true; 
	}
        checkpars();

	/* Set output trace length */
	ntout = nt + nfilter - 1;

	/* Main loop over traces */
	do {
		/* if panel=1 and sufile is defined */
		if ((panel==1) && (issufile==cwp_true)) {
			fgettr(fp, &sutrace);

                	/* HD: get delrt from filter sufile */
                	delrtf = sutrace.delrt;

			nfilter = sutrace.ns;
			filter = ealloc1float(nfilter);
			memcpy((void *) filter,
				(const void *) sutrace.data, nfilter*FSIZE);
			ntout = nt + nfilter - 1;
		}

		/* Convolve filter with trace */
		convolve_cwp(nfilter, 0, filter,
		     nt, 0, intrace.data, 
                     ntout, 0, outtrace.data);        


		/* Output filtered trace */
		memcpy((void *) &outtrace, (const void *) &intrace, HDRBYTES);
		outtrace.ns = ntout; 

		/* HD: update delrt */
		outtrace.delrt = intrace.delrt+delrtf;

		puttr(&outtrace);


	} while (gettr(&intrace));


	return(CWP_Exit());
}
