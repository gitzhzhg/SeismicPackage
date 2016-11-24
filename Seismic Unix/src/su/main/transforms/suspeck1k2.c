/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSPECK1K2: $Revision: 1.13 $ ; $Date: 2011/11/16 23:35:04 $		*/

#include "su.h"
#include "segy.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUSPECK1K2 - 2D (K1,K2) Fourier SPECtrum of (x1,x2) data set		",
" 									",
" suspeck1k2 <infile >outfile [optional parameters]			",
"									",
" Optional parameters:							",
"									",
" d1=from header(d1) or 1.0	spatial sampling interval in first (fast)",
"				   dimension				",
" d2=from header(d2) or 1.0	spatial sampling interval in second	",
"				 (slow)  dimension			",
"									",
" verbose=0		verbose = 1 echoes information			",
"									",
" tmpdir= 	 	if non-empty, use the value as a directory path",
"		 	prefix for storing temporary files; else if the",
"	         	the CWP_TMPDIR environment variable is set use	",
"	         	its value for the path; else use tmpfile()	",
" 									",
" Notes:								",
" Because the data are assumed to be purely spatial (i.e. non-seismic), ",
" the data are assumed to have trace id (30), corresponding to (z,x) data",
"									",
" To facilitate further processing, the sampling intervals in wavenumber",
" as well as the first frequency (0) and the first wavenumber are set in",
" the output header (as respectively d1, d2, f1, f2).			",
" 									",
" The relation: w = 2 pi F is well known for frequency, but there	",
" doesn't seem to be a commonly used letter corresponding to F for the	",
" spatial conjugate transform variables.  We use K1 and K2 for this.	",
" More specifically we assume a phase:					",
"		-i(k1 x1 + k2 x2) = -2 pi i(K1 x1 + K2 x2).		",
" and K1, K2 define our respective wavenumbers.				",
" 									",
NULL};

/* Credits:
 *     CWP: John Stockwell, 26 April 1995, based on original code by
 *          Dave Hale and Jack Cohen	
 *
 * Trace header fields accessed: ns, d1, d2, trid
 * Trace header fields modified: tracl, ns, dt, trid, d1, f1, d2, f2
 */
/**************** end self doc ***********************************/


#define PFA_MAX	720720	/* Largest allowed nfft		  */

/* Prototype */
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
FILE *tracefp;		/* fp for trace storage file		*/

segy intrace, outtrace;

int main(int argc, char **argv)
{
	int nx1,nx2;		/* numbers of samples			*/
	float dx1,dx2;		/* sampling intervals			*/
	float d1,d2;		/* output intervals in K1, K2		*/
	float f1,f2;		/* output first samples in K1, K2	*/
	int ix1,ix2;		/* sample indices			*/
	int nx1fft,nx2fft;	/* dimensions after padding for FFT	*/
	int nK1,nK2;		/* transform (output) dimensions	*/
	int ik1,ik2;		/* transform sample indices		*/
	register complex **ct;	/* complex FFT workspace		*/
	register float **rt;	/* float FFT workspace			*/
	int verbose;		/* flag for echoing information		*/
	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/


	/* Hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);

	/* Get info from first trace */ 
	if (!gettr(&intrace))  err("can't get first trace");
	if (intrace.trid != TRID_DEPTH)
				warn("tr.trid = %d",intrace.trid);
	nx1 = intrace.ns;

	/* get sampling intervals */
	if (!getparfloat("d1", &dx1)) {
		if (intrace.d1) { /* is d1 field set? */
			dx1 = (float) intrace.d1;
		} else { /* d1 not set, assume 1.0 */
			dx1 = 1.0;
			warn("tr.d1 not set, assuming d1=1.0");
		}
	}
	if (!getparfloat("d2",&dx2)) {
		if (intrace.d2) { /* is d2 field set? */
			dx2 = intrace.d2;
		} else {
			dx2 = 1.0;
			warn("tr.d2 not set, assuming d2=1.0");
		}
	}

	if (!getparint("verbose", &verbose))	verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

	/* Store traces in tmpfile while getting a count */
	if (STREQ(tmpdir,"")) {
		tracefp = etmpfile();
		if (verbose) warn("using tmpfile() call");
	} else { /* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(tracefile, temporary_filename(directory));
		/* Trap signals so can remove temp files */
		signal(SIGINT,  (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		tracefp = efopen(tracefile, "w+"); 
     		istmpdir=cwp_true;		
		if (verbose) warn("putting temporary files in %s", directory);
	}


        checkpars();

	nx2 = 0;
	do { 
		++nx2;
		efwrite(intrace.data, FSIZE, nx1, tracefp);
	} while (gettr(&intrace));


	/* Determine lengths for prime-factor FFTs */
	nx1fft = npfar(nx1);
	nx2fft = npfa(nx2);
	if (nx1fft >= SU_NFLTS || nx1fft >= PFA_MAX)
		err("Padded nx1=%d--too big",nx1fft);
	if (nx2fft >= SU_NFLTS || nx2fft >= PFA_MAX)
		err("Padded nx2=%d--too big",nx2fft);

	/* Determine output header values */
	d1 = 2.0/(nx1fft*dx1);
	d2 = 2.0/(nx2fft*dx2);
	f1 = -1.0/dx1 - d1/2.;
	f2 = -1.0/dx2 + d2;

	/* Note: The choices for d1,d2,f1, and f2 were motivated by the
		 desire to have a plot that runs from -nyq to +nyq
		 in both directions.  The choice of the shift d1/2
		 is because the k1=0 information always falls on a
 		 sample, whether the number of k1 values is even or odd.
		 The choice of the shift d2 is because the ISODD trick
		 centers k2=0 information between the two center traces
 		 if the number of k1 values is even, or on the center trace
		 if the number is odd.
	*/

	/* Determine complex transform sizes */
	nK1 = nx1fft/2 + 1 ;
	nK2 = nx2fft;


	/* Allocate space */
	rt = alloc2float(nx1fft, nx2fft);
	ct = alloc2complex(nK1, nK2);

	/* zero rt[][] and ct[][] */
	memset( (void *) rt[0], 0, FSIZE*nx1fft*nx2fft);
	memset( (void *) ct[0], 0, sizeof(complex)*nK1*nK2);

	/* Load traces into fft arrays and close tmpfile */
	rewind(tracefp);
	for (ix2=0; ix2<nx2; ++ix2) {

		efread(rt[ix2], FSIZE, nx1, tracefp);

		/* if ix2 odd, negate to center transform of dimension 2 */
        	if (ISODD(ix2))
                        for (ix1=0; ix1<nx1; ++ix1)
					rt[ix2][ix1] = -rt[ix2][ix1];
	}
	efclose(tracefp);
	
	/* Fourier transform dimension 1 */
	pfa2rc(-1,1,nx1fft,nx2,rt[0],ct[0]);
	
	/* Fourier transform dimension 2 */
	pfa2cc(-1,2,nK1,nK2,ct[0]);
	
	/* Compute and output amplitude spectrum */
	for (ik2=0; ik2<nK2; ++ik2) {

		/* do upper half of K-plane */
		for (ik1=0; ik1<nK1; ++ik1)
			outtrace.data[ik1] = rcabs(ct[nK2-1-ik2][nK1-1-ik1]);

		/* build lower half plane from upper plane assuming symmetry */
		for (ik1=nK1; ik1<nx1fft; ++ik1)
			if (ik2<nK2-1)
				outtrace.data[ik1] = rcabs(ct[ik2+1][ik1-nK1]);
			else
				outtrace.data[ik1] = rcabs(ct[0][ik1-nK1]);
				

		/* set header values */
		outtrace.tracl = ik2+1;
		outtrace.ns = nx1fft;
		outtrace.dt = 0;  /* d1 is now the relevant step size */
		outtrace.trid = KOMEGA;
		outtrace.d1 = d1;
		outtrace.f1 = f1;
		outtrace.d2 = d2;
		outtrace.f2 = f2;

		puttr(&outtrace);
	}

	/* Clean up */
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(tracefp);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}
