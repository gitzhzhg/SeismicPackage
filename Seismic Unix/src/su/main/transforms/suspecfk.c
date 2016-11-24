/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSPECFK: $Revision: 1.21 $ ; $Date: 2011/11/16 23:35:04 $		*/

#include "su.h"
#include "segy.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUSPECFK - F-K Fourier SPECtrum of data set			",
" 								",
" suspecfk <infile >outfile [optional parameters]		",
"								",
" Optional parameters:						",
"								",
" dt=from header		time sampling interval		",
" dx=from header(d2) or 1.0	spatial sampling interval	",
"								",
" verbose=0	verbose = 1 echoes information			",
"								",
" tmpdir= 	 if non-empty, use the value as a directory path",
"		 prefix for storing temporary files; else if the",
"	         the CWP_TMPDIR environment variable is set use	",
"	         its value for the path; else use tmpfile()	",
" 								",
" Note: To facilitate further processing, the sampling intervals",
"       in frequency and wavenumber as well as the first	",
"	frequency (0) and the first wavenumber are set in the	",
"	output header (as respectively d1, d2, f1, f2).		",
" 								",
" Note: The relation: w = 2 pi F is well known, but there	",
"	doesn't	seem to be a commonly used letter corresponding	",
"	to F for the spatial conjugate transform variable.  We	",
"	use K for this.  More specifically we assume a phase:	",
"		i(w t - k x) = 2 pi i(F t - K x).		",
"	and F, K define our notion of frequency, wavenumber.	",
" 								",
NULL};

/* Credits:
 *
 *	CWP: Dave (algorithm), Jack (reformatting for SU)
 *
 * Trace header fields accessed: ns, dt, d2
 * Trace header fields modified: tracl, ns, dt, trid, d1, f1, d2, f2
 */
/**************** end self doc ***********************************/


#define PFA_MAX	720720	/* Largest allowed nfft	          */

/* Prototype */
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
FILE *tracefp;		/* fp for trace storage file		*/

segy intrace, outtrace;

int
main(int argc, char **argv)
{
	int nt,nx;		/* numbers of samples			*/
	float dt,dx;		/* sampling intervals			*/
	float d1,d2;		/* output intervals in F, K		*/
	float f1,f2;		/* output first samples in F, K		*/
	int it,ix;		/* sample indices			*/
	int ntfft,nxfft;	/* dimensions after padding for FFT	*/
	int nF,nK;		/* transform (output) dimensions	*/
	int iF,iK;		/* transform sample indices		*/
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
	nt = intrace.ns;


	/* dt is used only to set output header value d1 */
	if (!getparfloat("dt", &dt)) {
		if (intrace.dt) { /* is dt field set? */
			dt = ((double) intrace.dt)/ 1000000.0;
		} else { /* dt not set, assume 4 ms */
			dt = 0.004;
			warn("tr.dt not set, assuming dt=0.004");
		}
	}
	if (!getparfloat("dx",&dx)) {
		if (intrace.d2) { /* is d2 field set? */
			dx = intrace.d2;
		} else {
			dx = 1.0;
			warn("tr.d2 not set, assuming d2=1.0");
		}
	}

	if (!getparint("verbose", &verbose))	verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);


        checkpars();

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

	nx = 0;
	do { 
		++nx;
		efwrite(intrace.data, FSIZE, nt, tracefp);
	} while (gettr(&intrace));


	/* Determine lengths for prime-factor FFTs */
	ntfft = npfar(nt);
	nxfft = npfa(nx);
	if (ntfft >= SU_NFLTS || ntfft >= PFA_MAX)
			err("Padded nt=%d--too big",ntfft);
	if (nxfft >= SU_NFLTS || nxfft >= PFA_MAX)
			err("Padded nx=%d--too big",nxfft);

	/* Determine output header values */
	d1 = 1.0/(ntfft*dt);
	d2 = 1.0/(nxfft*dx);
	f1 = 0.0;
	f2 = -1.0/(2*dx);


	/* Determine complex transform sizes */
	nF = ntfft/2+1;
	nK = nxfft;


	/* Allocate space */
	ct = alloc2complex(nF, nK);
	rt = alloc2float(ntfft, nxfft);


	/* Load traces into fft arrays and close tmpfile */
	erewind(tracefp);
	for (ix=0; ix<nx; ++ix) {

		efread(rt[ix], FSIZE, nt, tracefp);

                /* if ix odd, negate to center transform of dimension 2 */
                if (ISODD(ix))
			for (it=0; it<nt; ++it)  rt[ix][it] = -rt[ix][it];

		/* pad dimension 1 with zeros */
		for (it=nt; it<ntfft; ++it)  rt[ix][it] = 0.0;
	}
	efclose(tracefp);


	/* Pad dimension 2 with zeros */
	for (ix=nx; ix<nxfft; ++ix)
		for (it=0; it<ntfft; ++it)  rt[ix][it] = 0.0;

	
	/* Fourier transform dimension 1 */
	pfa2rc(1,1,ntfft,nx,rt[0],ct[0]);
	

	/* Fourier transform dimension 2 */
	pfa2cc(-1,2,nF,nxfft,ct[0]);
	

	/* Compute and output amplitude spectrum */
	for (iK=0; iK<nK; ++iK) {
		for (iF=0; iF<nF; ++iF)  outtrace.data[iF] = rcabs(ct[iK][iF]);

		/* set header values */
		outtrace.tracl = iK + 1;
		outtrace.ns = nF;
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
