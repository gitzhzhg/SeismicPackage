/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUXCOR: $Revision: 1.19 $ ; $Date: 2011/11/16 17:37:27 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUXCOR - correlation with user-supplied filter			",
" 									",
" suxcor <stdin >stdout  filter= [optional parameters]			",
" 									",
" Required parameters: ONE of						",
" sufile=		file containing SU traces to use as filter	",
" filter=		user-supplied correlation filter (ascii)	",
" 									",
" Optional parameters:							",
" vibroseis=0		=nsout for correlating vibroseis data		",
" first=1		supplied trace is default first element of	",
" 			correlation.  =0 for it to be second.		",
" panel=0		use only the first trace of sufile as filter 	",
" 			=1 xcor trace by trace an entire gather		",
" ftwin=0		first sample on the first trace of the window 	",
" 				(only with panel=1)		 	",
" ltwin=0		first sample on the last trace of the window 	",
" 				(only with panel=1)		 	",
" ntwin=nt		number of samples in the correlation window	",
" 				(only with panel=1)		 	",
" ntrc=48		number of traces on a gather 			",
" 				(only with panel=1)		 	",
" 									",
" Trace header fields accessed: ns					",
" Trace header fields modified: ns					",
" 									",
" Notes: It is quietly assumed that the time sampling interval on the	",
" single trace and the output traces is the same as that on the traces	",
" in the input file.  The sufile may actually have more than one trace,	",
" but only the first trace is used when panel=0. When panel=1 the number",
" of traces in the sufile MUST be the same as the number of traces in 	",
" the input.								",
" 									",
" Examples:								",
"	suplane | suwind min=12 max=12 >TRACE				",
"	suxcor<DATA sufile=TRACE |...					",
" Here, the su data file, \"DATA\", is correlated trace by trace with the",
" the single su trace, \"TRACE\".					",
" 									",
"	suxcor<DATA filter=1,2,1 | ...					",
" Here, the su data file, \"DATA\", is correlated trace by trace with the",
" the filter shown.							",
" 									",
" Correlating vibroseis data with a vibroseis sweep:			",
" suxcor < data sufile=sweep vibroseis=nsout  |...			",
" 									",
" is equivalent to, but more efficient than:				",
" 									",
" suxcor < data sufile=sweep |						",
" suwind itmin=nsweep itmax=nsweep+nsout | sushw key=delrt a=0.0 |...   ",
" 									",
" sweep=vibroseis sweep in SU format, nsweep=number of samples on	",
" the vibroseis sweep, nsout=desired number of samples on output	",
" 									",
" or									",
" suxcor < data sufile=sweep |						",
" suwind itmin=nsweep itmax=nsweep+nsout | sushw key=delrt a=0.0 |...   ",
" 									",
" tsweep=sweep length in seconds, tout=desired output trace length in seconds",
" 									",
" In the spatially variant case (panel=1), a window with linear slope 	",
" can be defined:						 	",
" 	ftwin is the first sample of the first trace in the gather,  	",
" 	ltwin is the first sample of the last trace in the gather,	",
" 	ntwin is the lengthe of the window, 				",
" 	ntrc is the the number of traces in a gather. 			",
" 									",
" 	If the data consists of a number gathers which need to be 	",
"	correlated with the same number gathers in the sufile, ntrc	",
"	assures that the correlating window re-starts for each gather.	",
" 									",
"	The default window is non-sloping and takes the entire trace	",
"	into account (ftwin=ltwin=0, ntwin=nt).				",
" 									",
NULL};

/* Credits:
 *	CWP: Jack K. Cohen, Michel Dietrich
 *      CWP: modified by Ttjan to include cross correlation of panels
 *	   permitting spatially and temporally varying cross correlation.
 *      UTK: modified by Rick Williams for vibroseis correlation option.
 *
 *  CAVEATS: 
 *     In the option, panel=1 the number of traces in the sufile must be 
 *     the same as the number of traces on the input.
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
	int it,it2;		/* counter 				*/
	int ntout;		/* number of points on output traces	*/
	float *filter=NULL;	/* filter coefficients			*/
	float *trace;		/* trace coefficients			*/
	int nfilter=0;		/* length of input wavelet in samples	*/
	int ftwin;	      /* first sample in first windowed trace */
	int ltwin;	      /* first sample in last windowed trace */
	int ntwin;	      /* number of points on windowed input traces*/
	int dtwin=0;	      /* sample trace step for the gather	*/
	int ntrc;	      	/* number of traces on gather 		*/
	int fs;	      	/* first sample for cross-correlation 	*/
	int tracl;	     	/* trace number 			*/
	int tracr;	     	/* trace number in gather 	 	*/
	cwp_String sufile;	/* name of file containing one SU trace */
	FILE *fp=NULL;		/* ... its file pointer			*/
	int vibroseis;		/* vibroseis correlation flag		*/
	int first;		/* correlation order flag		*/
	int panel;		/* xcor with trace or panel 		*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&intrace)) err("can't get first trace");
	nt = intrace.ns;


	/* Get parameters and set up filter array */
	if (!getparint("vibroseis", &vibroseis))  vibroseis = 0;
	if (!getparint("first", &first))  first = 1;
	if (!getparint("panel", &panel))  panel = 0;
	if (!getparint("ntwin",&ntwin)) ntwin=nt;
	if (!getparint("ftwin",&ftwin)) ftwin=0;
	if (!getparint("ltwin",&ltwin)) ltwin=0;
	if (!getparint("ntrc",&ntrc)) ntrc=48;
	if (!getparstring("sufile", &sufile)) {
		if (!(nfilter = countparval("filter")))
			err("must specify filter= desired filter");
		filter = ealloc1float(nfilter);	getparfloat("filter", filter);
	} else {
		fp = efopen(sufile, "r");
		if (panel == 0){
			fgettr(fp, &sutrace);
			nfilter = sutrace.ns;
			filter = ealloc1float(nfilter);
			memcpy(( void *) filter,
				(const void *) sutrace.data, nfilter*FSIZE);
		}
	}

        checkpars();

	/* Set window and output-trace length */
	if (panel == 1) {
		nfilter = ntwin;
		nt = ntwin;
		dtwin = (ltwin - ftwin)/ntrc;
		filter = ealloc1float(nfilter);
	} else ntwin = nt;

	/* allocate space */
	trace = ealloc1float(nt);

	if (vibroseis <= 0) {
		ntout = nt + nfilter - 1;
	} else {
		ntout=vibroseis;
	}


	/* Main loop over traces */
	tracl = 1;
	if (first) {
		do {
			if (panel == 1){
				if ((!fgettr(fp, &sutrace))) 
					err("number of traces in sufile are not the same as the number of traces on the input");
				if ((tracr = (tracl % ntrc)) == 0) tracr=ntrc;
				fs = ftwin + tracr * dtwin;
				for (it=0; it<ntwin; it++) {
				      it2 = it + fs;
				      trace[it] = intrace.data[it2];
				      filter[it] = sutrace.data[it2];
	  			}

			} else {
				memcpy((void *) trace,
					(const void *) intrace.data, nt*FSIZE);
			}

			if (vibroseis <= 0) {
				xcor(nfilter, 0, filter,
				     nt, 0, trace, 
	       		      	     ntout, -nfilter + 1, outtrace.data);       
			} else {
				xcor(nfilter, 0, filter,
				     nt, 0, trace, 
	       		      	     ntout, 0, outtrace.data);       
			}
			
			memcpy((void *) &outtrace,
					(const void *) &intrace, HDRBYTES);

			if (vibroseis > 0) {
				outtrace.delrt = 0;
			}

			outtrace.ns = ntout; 
			puttr(&outtrace);
			tracl++;

		} while (gettr(&intrace));
	} else {
		do {
			if (panel == 1){
				if ((!fgettr(fp, &sutrace))) 
					err("number of traces in sufile are not the same as the number of traces on the input");
				if ((tracr = (tracl % ntrc)) == 0) tracr=ntrc;
				fs = ftwin + tracr * dtwin;
				for (it=0; it<ntwin; it++) {
				      it2 = it + fs;
				      trace[it] = intrace.data[it2];
				      filter[it] = sutrace.data[it2];
	  			}

			} else {
				memcpy((void *) trace,
					(const void *) intrace.data, nt*FSIZE);
			}

			if (vibroseis <= 0) {
				xcor(nt, 0, trace, 
				     nfilter, 0, filter,
		   	  	     ntout, -nt + 1, outtrace.data);	
			} else {
				xcor(nt, 0, trace,
				     nfilter, 0, filter, 
	       		      	     ntout, 0, outtrace.data);       
			}

			memcpy((void *) &outtrace,
					(const void *) &intrace, HDRBYTES);

			if (vibroseis > 0) {
				outtrace.delrt = 0;
			}

			outtrace.ns = ntout; 
			puttr(&outtrace);
			tracl++;

		} while (gettr(&intrace));
	}


	return(CWP_Exit());
}
