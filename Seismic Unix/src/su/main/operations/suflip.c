/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFLIP: $Revision: 1.31 $ ; $Date: 2011/11/16 23:09:52 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUFLIP - flip a data set in various ways			",
" 								",
" suflip <data1 >data2 flip=1 verbose=0				",
" 								",
" Required parameters:						",
" 	none							",
" 								",
" Optional parameters:						",
" 	flip=1 	rotational sense of flip			",
" 			+1  = flip 90 deg clockwise		",
" 			-1  = flip 90 deg counter-clockwise	",
" 			 0  = transpose data			",
" 			 2  = flip right-to-left		",
" 			 3  = flip top-to-bottom		",
" 	tmpdir=	 if non-empty, use the value as a directory path",
"		 prefix for storing temporary files; else if	",
"	         the CWP_TMPDIR environment variable is set use	",
"	         its value for the path; else use tmpfile()	",
" 								",
" 	verbose=0	verbose = 1 echoes flip info		",
" 								",
" NOTE:  tr.dt header field is lost if flip=-1,+1.  It can be	",
"        reset using sushw.					",
" 								",
" EXAMPLE PROCESSING SEQUENCES:					",
"   1.	suflip flip=-1 <data1 | sushw key=dt a=4000 >data2	",
" 								",
"   2.	suflip flip=2 <data1 | suflip flip=2 >data1_again	",
" 								",
"   3.	suflip tmpdir=/scratch <data1 | ...			",
" 								",
" Caveat:  may fail on large files.				",
NULL};

/* Credits:
 *	CWP: Chris Liner, Jack K. Cohen, John Stockwell
 *
 * Caveat:
 *	right-left flip (flip = 2) and top-bottom flip (flip = 3)
 *	don't require the matrix approach.  We sacrificed efficiency
 *	for uniform coding.
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: ns, dt, tracl
 */
/**************** end self doc ***********************************/

/* prototype */
void flipper(register float **indata, register float **flipdata,
		register int ncol, register int nrow, register int flip);
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr;

int
main(int argc, char **argv)
{
	int flip;		/* flag dictating sense of flip		*/
	int verbose;		/* flag for echoing info		*/
	int trdt;		/* time sample rate as integer		*/
	int nt;			/* samples per trace on input		*/
	int ntr;		/* traces in input data			*/
	register int i;		/* counter			 	*/
	register float **data;	/* matrix for input data		*/
	register
	    float **flipdata;	/* matrix for flipped data		*/

	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user given path		*/
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Set parameters */
	if (!getparint("verbose", &verbose))	verbose = 0;
	if (!getparint("flip",    &flip))    	flip = 1;
	if (flip != -1 && flip != 0 && flip != 1 && flip != 2 && flip != 3)
		err("flip = %d, flag must be -1, 0, 1, 2, or 3", flip);
	
	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

        checkpars();

	/* Get info from first trace */
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;
	trdt = tr.dt;

	/* Store traces and headers in tempfiles while getting a count */
	if (STREQ(tmpdir,"")) {
		tracefp = etmpfile();
		headerfp = etmpfile();
		if (verbose) warn("using tmpfile() call");
	} else { /* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(tracefile, temporary_filename(directory));
		strcpy(headerfile, temporary_filename(directory));
		/* Trap signals so can remove temp files */
		signal(SIGINT,  (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		tracefp = efopen(tracefile, "w+");
		headerfp = efopen(headerfile, "w+");
      		istmpdir=cwp_true;		
		if (verbose) warn("putting temporary files in %s", directory);
	}
	
	ntr = 0;
	do {
		++ntr;
		efwrite(&tr, HDRBYTES, 1, headerfp);
		efwrite(tr.data, FSIZE, nt, tracefp);
	} while (gettr(&tr));
	erewind(tracefp);
	erewind(headerfp);

	/* Allocate data matrices */
	data = ealloc2float(nt, ntr);
	if (flip == -1 || flip == 0 || flip == 1) {
		CHECK_NT("ntr",ntr);
		flipdata = ealloc2float(ntr, nt);
	} else {
		flipdata = ealloc2float(nt, ntr);
	}


	/* Load traces into the data matrix and close tmpfile */
	efread(*data, FSIZE, nt*ntr, tracefp);
	efclose(tracefp);
	if (istmpdir) eremove(tracefile);


	/* Verbose print */
	if (verbose) {
		fprintf(stderr, "\nSUFLIP:          flip = %d\n", flip);
		fprintf(stderr, "input:   samples/trace = %d", nt);
		fprintf(stderr, "    traces = %d \n", ntr);
		if (flip == 1 || flip == 0 || flip == -1) {
			fprintf(stderr, "output:  samples/trace = %d", ntr);
			fprintf(stderr, "    traces = %d \n\n", nt);
		} else {
			fprintf(stderr, "output:  samples/trace = %d", nt);
			fprintf(stderr, "    traces = %d \n\n", ntr);
		}
	}


	/* Sub does the flipping */
	flipper(data, flipdata, nt, ntr, flip); 


	/* Output the result by pulling traces off flipdata matrix */
	if (flip == -1 || flip == 0 || flip == 1) {
		for (i = 0; i < nt; i++) {
			memcpy((void *) tr.data,
			       (const void *) flipdata[i], ntr*FSIZE); 
			efread(&tr, 1, HDRBYTES, headerfp);
			tr.ns = ntr;
			tr.tracl = i + 1;
			puttr(&tr);
		}
	} else {
		for (i = 0; i < ntr; i++) {
			memcpy((void *) tr.data,
					(const void *) flipdata[i], nt*FSIZE); 
			efread(&tr, 1, HDRBYTES, headerfp);
			tr.ns = nt;
			tr.dt = trdt;
			tr.tracl = i + 1;
			puttr(&tr);
		}
	}

	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	return(CWP_Exit());
}


void flipper(register float **indata, register float **flipdata,
		register int ncol, register int nrow, int flip)
{
	register int icol, irow;

	switch (flip) {

	case -1: 	/*  flip 90 deg counter-clockwise  */

		for (irow = 0; irow < nrow; irow++) {
			for (icol = 0; icol < ncol; icol++) {
				flipdata[icol][nrow-1 - irow] =
							indata[irow][icol];
			}
		}
	break;
	case 0: 	/*  transpose  */

		for (irow = 0; irow < nrow; irow++) {
			for (icol = 0; icol < ncol; icol++) {
				flipdata[icol][irow] = indata[irow][icol];
			}
		}
	break;
	case 1: 	/*  flip 90 deg clockwise  */

		for (irow = 0; irow < nrow; irow++) {
			for (icol = 0; icol < ncol; icol++) {
				flipdata[ncol-1 - icol][irow] =
						indata[irow][icol];
			}
		}
	break;
	case 2:		/*  flip right-to-left  */

		for (irow = 0; irow < nrow; irow++) {
			for (icol = 0; icol < ncol; icol++) {
				flipdata[nrow-1 - irow][icol] =
						indata[irow][icol];
			}
		}
	break;
	case 3:		/*  flip top-to-bottom  */

		for (irow = 0; irow < nrow; irow++) {
			for (icol = 0; icol < ncol; icol++) {
				flipdata[irow][ncol-1 - icol] =
							indata[irow][icol];
			}
		}
	break;
	default:	/*  defensive programming  */

		err("%d: mysterious flip value: %d", __LINE__, flip);
	}
	return;
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(headerfp);
	efclose(tracefp);
	eremove(headerfile);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}
