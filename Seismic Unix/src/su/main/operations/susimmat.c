/* Copyright (c) Colorado School of Mines, 2015.*/
/* All rights reserved.			*/

/* SUSIMMAT: $Revision: 1.3 $ ; $Date: 2015/08/11 21:38:33 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUSIMAT - Correlation similarity matrix for two traces.		",
" 		 Output is zero lag of cross-correlation of traces,	",
" 		 or linear regression correlation coefficient.		",
" 		 Horizontal axis is time of trace 1, vertical is trace 2.",
" 									",
" susimmat <data12 sufile=data2 >dataout 				",
" 									",
" Required parameters:							",
" sufile=		file containing SU traces to use as filter	",
" 									",
" Optional parameters:							",
" panel=0		use only the first trace of sufile as filter	",
" 		      	=1 compute sim matrix trace by trace an entire  ",
"                        gather						",
" mt=21			operator window length (odd integer)		",
" eps=1e-3		stability parameter				",
" taper=0		no taper to data fragments			",
" 			=1 apply exponential taper (1/e at ends)	",
" method=1		use xcorrelation as similarity meausure		",
" 			=2 same but normalized by (rms+eps)		",
" 			=3 use linear regression CC			",
" 			=4 use mt-dimensional vector angle		",
" 									",
" EXAMPLE PROCESSING SEQUENCES:						",
"   1. Look for all possible alignments of OBC P and Z data  		",
"   	susimmat < P_Z.su sufile=OBC_P.su  mt=71 > P_Zxcor.su		",
" 									",
" Note:  xcor window is collapsed as needed to compute edge values	",
" It is quietly assumed that the time sampling interval on the  single  ",
" trace									",
" and the output traces is the same as that on the traces in the input  ",
" file.  								",
" The sufile may actually have more than one trace, but only the first  ",
" trace is used when panel=0. When panel=1 the number of traces in the ",
" sufile MUST be  the same as the number of traces in the input.	",
NULL};

/* Credits:
 *	U Arkansas: Chris Liner (originally 11/2009 at U Houston)
 *      CWP: John Stockwell, some i/o modifications  Jul 2015
 *
 * References: 
 * Liner, Christopher L., and Robert G. Clapp. "Nonlinear pairwise 
 *	 alignment of seismic traces." The Leading Edge 23.11 (2004): 1146-1150.
 * Liner, C. L, and R. G. Clapp (2004), Nonlinear pairwise alignment of 
 *	 seismic traces GEOPHYSICS, VOL. 69, NO. 6 
 *	(NOVEMBER-DECEMBER 2004); P. 1552–1559, 7 FIGS.  10.1190/1.1836828 
 * 
 * Caveat:
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: ns, dt, tracl
 */
/**************** end self doc ***********************************/

/* Definitions */
#define XCOR       1
#define XCOR_NORM  2
#define LIN_REGR   3
#define VEC_ANGLE  4

segy intrace, outtrace, sutrace; 

int
main(int argc, char **argv)
{
	int nt;			/* samples per trace on input		*/
	int mt;			/* length of xcor window		*/
	int mtorig;		/* requested mt holder			*/
	int j;			/* time sample counter			*/

	int hw;			/* xcor window half length		*/
	int tracl;		/* trace header counter			*/
	int method;		/* operation method			*/
	int taper;		/* exponential taper	flag		*/

	float dt;	/* time sample rate in microsec		*/
	float x0;		/* zero lag value of xcor function	*/
	float rms1,rms2;	/* rms functions for x0 normalization	*/
	float eps;		/* stabilization parameter		*/
	float alpha;		/* var for exponential taper		*/
	float *x=NULL,*y=NULL;	/* arrays for linear fit		*/

	register int irow;	/* row counter			 	*/
	register int icol;	/* column counter		 	*/
	register float *trace=NULL;	/* input data vector 		*/
	register float *data=NULL;	/* input data vector 		*/
	register float **mat=NULL;	/* similarity matrix 		*/

	int verbose=0;		/* verbose =0 silent, !=0 chatty */

	int panel=0;		/* =0 use first trace only from sufile */
				/* =1 compute the sim matrix trace by trace */

	cwp_String sufile;	/* name of sufile */
	FILE *sufilefp=NULL;	/* pointer to sufile */		

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* get verbose flag */
	if (!getparint("verbose", &verbose))	 verbose = 0;

	/* Set parameters */
	if (!getparint("mt", &mt))	mt = 21;
		/* make sure it is odd*/
		if (mt != 21 && mt != 1) {
			mt = mt/2*2 + 1;
			if (verbose) warn("Using mt=%i",mt);
		}
	mtorig = mt;
	if (!getparint("method", &method))	method = 1;
	if (!(method == 1
		|| method == 2
		|| method == 3
		|| method == 4)) err("Bad method");

	if (verbose) warn("method = %i",method);

	if (mt < 5 && method==2) err("Need more than 5 points for linear regression");
	if (!getparint("taper", &taper)) taper = 0;
	if (!getparfloat("eps", &eps))	eps = 1e-3;
	
	/* Get info from first trace */
	if (!gettr(&intrace))  err("can't get first trace");
	nt = intrace.ns;
	dt = intrace.dt;



	/* allocate data vectors and similarity matrix */
	trace = ealloc1float(nt);
	data = ealloc1float(nt);
	mat = ealloc2float(nt,nt);
	x = ealloc1float(mt);
	y = ealloc1float(mt);

	/* open sufile */
	MUSTGETPARSTRING("sufile",&sufile);
	sufilefp = efopen(sufile, "r");
	fgettr(sufilefp, &sutrace);
	if (panel==0) {
		memcpy(( void *) data, (const void *) sutrace.data, nt*FSIZE);
	}

	/* zero out mat */

	/* Load input traces into data vectors trace and data */
	do {
		memset((void *) mat[0], 0, nt*nt*FSIZE);
		if (panel==1) { 
			if ((!fgettr(sufilefp, &sutrace)))
                                  err("number of traces in sufile are not the same as the number of traces on the input");

			memcpy( (void *) data, (const void *) sutrace.data, nt*FSIZE);

		} else { 
			memcpy( (void *) trace, (const void *) intrace.data, nt*FSIZE);
		}


	/* make sim matrix */
	for (icol = 0; icol < nt; ++icol) {

			for (irow = 0; irow < nt; irow++) {
	
				/* half-length of correlation window */
				hw = (mtorig-1)/2;

				x0 = 0;
				rms1 = 0;
				rms2 = 0;

				/* telescoping boundary conditions... low sides */
				if (irow <= hw || icol <= hw) {
					/* figure out how close we are to the edge */
					if (irow <= hw || icol <= hw ) {
						if (irow <= icol ) hw = irow;
						if (irow > icol )  hw = icol;
					}
				}

				/* telescoping boundary conditions... high sides */
				if (irow >= nt-hw || icol >= nt-hw) {
					/* figure out how close we are to the edge */
					if (irow <= hw || icol <= hw ) {
						if (irow <= icol ) hw = nt - icol;
						if (irow > icol )  hw = nt - irow;
					}
				}

				/* set mt consistent with boundary conditions */
				mt = 2 * hw + 1;

				/* zero lag of xcor of length mt */
				for (j = 0; j < mt; j++) {

					/* taper has value 1/e^2 at first and last xcor  */
					/* window points */
					alpha = 4*((hw-j+1)/(hw+1))*((hw-j+1)/(hw+1));

					/* similarity by zero lag of xcor */
					if (taper == 0) {
					x0 = x0 + trace[icol-hw+j]*data[irow-hw+j];
					}
					if (taper == 1) {
					x0 = x0 + trace[icol-hw+j]*data[irow-hw+j]*exp(-alpha);
					}
					rms1 = rms1 + trace[icol-hw+j]*trace[icol-hw+j];
					rms2 = rms2 + data[irow-hw+j]*data[irow-hw+j];

					/* load (x,y) for regression */
					x[j] = trace[icol-hw+j];
					y[j] = data[irow-hw+j];

				}

				switch(method) { /* beginning of cases */

					case XCOR:	
					{
						/* if xcor measure is requested */
						mat[icol][irow] = x0;

					}
					break;;
					case XCOR_NORM:	
					{
						/* if normalized xcor measure is requested */

						mat[icol][irow] = x0/(sqrt(rms1*rms2) + eps);
						mat[icol][irow] = acos(x0/(sqrt(rms1*rms2) + eps))*180./PI;
					}
					break;;
					case LIN_REGR:	
					{
						/* if similarity by linear regression is requested*/

						float coeff[4];
						linear_regression(y, x, mt, coeff);
						mat[icol][irow] = coeff[2];
					}
					break;;
					case VEC_ANGLE:
					{

						/* if vector angle measure is requested */
						mat[icol][irow] = acos(x0/(sqrt(rms1*rms2) + eps))*180./PI;
					}
					break;;
					default:
						err("mysterious method=\"%s\"", method);
					}

				}

			}


		/* pull off traces and output */
		tracl = 1;		
		outtrace.trid = 1;    /* su time traces */
		outtrace.ns = nt;
		outtrace.dt = dt;
 
		 for (icol = 0 ; icol < nt ; ++icol){
	 
			for (irow=0 ; irow < nt ; ++irow){
				outtrace.data[irow] = mat[icol][irow];
			}		

			outtrace.tracl = tracl;
			puttr(&outtrace);

			++tracl;
		}


	} while (gettr(&intrace));
	return(CWP_Exit());
}
