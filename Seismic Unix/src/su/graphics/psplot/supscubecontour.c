/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPSCUBECONTOUR: $Revision: 1.3 $ ; $Date: 2011/11/12 00:46:15 $		*/

#include "su.h"
#include "segy.h"
#include <signal.h>

/*********************** self documentation *****************************/
char *sdoc[] = {
" 									",
" SUPSCUBECONTOUR - PostScript CUBE plot of a segy data set		",
" 									",
" supscubecontour <stdin [optional parameters] | ...			",
" 							        	",
" Optional parameters: 							",
" 							        	",
" n2 is the number of traces per frame.  If not getparred then it	",
" is the total number of traces in the data set.  			",
" 							        	",
" n3 is the number of frames.  If not getparred then it			",
" is the total number of frames in the data set measured by ntr/n2	",
" 							        	",
" d1=tr.d1 or tr.dt/10^6	sampling interval in the fast dimension	",
"   =.004 for seismic 		(if not set)				",
"   =1.0 for nonseismic		(if not set)				",
" 							        	",
" d2=tr.d2			sampling interval in the slow dimension	",
"   =1.0 			(if not set)				",
" 							        	",
" f1=tr.f1 or tr.delrt/10^3 or 0.0  first sample in the fast dimension	",
" 							        	",
" f2=tr.f2 or tr.tracr or tr.tracl  first sample in the slow dimension	",
"   =1.0 for seismic		    (if not set)			",
"   =d2 for nonseismic		    (if not set)			",
" 							        	",
" verbose=0              =1 to print some useful information		",
"									",
" tmpdir=	 	if non-empty, use the value as a directory path	",
"		 	prefix for storing temporary files; else if the	",
"	         	the CWP_TMPDIR environment variable is set use	",
"	         	its value for the path; else use tmpfile()	",
" 									",
" Note that for seismic time domain data, the \"fast dimension\" is	",
" time and the \"slow dimension\" is usually trace number or range.	",
" Also note that \"foreign\" data tapes may have something unexpected	",
" in the d2,f2 fields, use segyclean to clear these if you can afford	",
" the processing time or use d2= f2= to over-ride the header values if	",
" not.									",
" 							        	",
" See the pscubecontour selfdoc for the remaining parameters.		",
" 							        	",
" example:   supscubecontour < infile [optional parameters]  | gv -	",
NULL};

/* Credits:
 *
 *	CWP: Dave Hale and Zhiming Li (pscube)
 *	     Jack K. Cohen  (suxmovie)
 *	     John Stockwell (supscubecontour)
 *
 * Notes:
 *	When n2 isn't getparred, we need to count the traces
 *	for pscube. Although we compute ntr, we don't allocate a 2-d array
 *	and content ourselves with copying trace by trace from
 *	the data "file" to the pipe into the plotting program.
 *	Although we could use tr.data, we allocate a trace buffer
 *	for code clarity.
 */
/**************** end self doc *******************************************/

static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files	*/
char tracefile[BUFSIZ];	/* filename for trace storage file	*/
FILE *tracefp;		/* fp for trace storage file		*/

segy tr;

int
main(int argc, char **argv)
{
	char plotcmd[BUFSIZ];	/* build pscube command for popen 	*/
	float *trbuf;		/* trace buffer			 	*/
	FILE *plotfp;		/* fp for plot data			*/
	int nt;			/* number of samples on trace		*/
	int n2;			/* number of traces per frame		*/
	int n3;			/* number of frames in data		*/
	int ntr;		/* number of traces			*/
	int verbose;		/* verbose flag				*/
	float d1;		/* time/depth sample rate 		*/
	float d2;		/* trace/dx sample rate 		*/
	float d3;		/* slowest dim sample rate 		*/
	float f1;		/* tmin/zmin				*/
	float f2;		/* tracemin/xmin	 		*/
	float f3=0.0;		/* first value in 3rd dimension		*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	cwp_Bool have_n2 = cwp_false;/* was n2 getparred?			*/
	cwp_Bool have_n3 = cwp_false;/* was n3 getparred?			*/
	cwp_Bool have_ntr = cwp_false;/* was ntr set in header?		*/
	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user given path		*/

	char *cwproot;		/* value of CWPROOT environment variable*/
	char *bindir;		/* directory path for tmp files		*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	

	/* Get info from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	seismic = ISSEISMIC(tr.trid); 
	nt = tr.ns;
	ntr = tr.ntr;
	if (ntr) have_ntr = cwp_true; 

	if (!getparint("verbose", &verbose))	verbose=0;
	if (!getparfloat("d1", &d1)) {
		if      (tr.d1)  d1 = tr.d1;
		else if (tr.dt)  d1 = ((double) tr.dt)/1000000.0;
		else {
			if (seismic) {
				d1 = 0.004;
				warn("tr.dt not set, assuming dt=0.004");
			} else { /* non-seismic data */
				d1 = 1.0;
				warn("tr.d1 not set, assuming d1=1.0");
			}
		}
	}

	if (!getparfloat("d2", &d2)) d2 = (tr.d2) ? tr.d2 : 1.0;
	if (!getparfloat("d3", &d3)) d3 = 1.0;

	if (!getparfloat("f1", &f1)) {
		if      (tr.f1)     f1 = tr.f1;
		else if (tr.delrt)  f1 = (float) tr.delrt/1000.0;
		else                f1 = 0.0;
	}

	if (!getparfloat("f2", &f2)) {
		if      (tr.f2)     f2 = tr.f2;
		else if (tr.tracr)  f2 = (float) tr.tracr;
		else if (tr.tracl)  f2 = (float) tr.tracl;
		else if (seismic)   f2 = 1.0;
		else 		    f2 = 0.0;
	}


	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

	/* See if CWPBIN environment variable is not set */
	if (!(bindir = getenv("CWPBIN"))) { /* construct bindir from CWPROOT */

		bindir = (char *) emalloc(BUFSIZ);

		/* Get value of CWPROOT environment variable */
		if (!(cwproot = getenv("CWPROOT"))) cwproot ="" ;
		if (STREQ(cwproot, "")) {
			warn("CWPROOT environment variable is not set! ");
			err("Set CWPROOT in shell environment as per instructions in CWP/SU Installation README files");
		}
		/* then bindir = $CWPROOT/bin */
		sprintf(bindir, "%s/bin", cwproot);
	}
	strcat(bindir,"/");   /* put / at the end of bindir */

	/* Allocate trace buffer */
	trbuf = ealloc1float(nt);

	/* Get or set n2 and n3 */
	if (getparint("n2", &n2)) have_n2 = cwp_true;
	if (getparint("n3", &n3)) have_n3 = cwp_true;
	if (have_n2 && have_n3) have_ntr = cwp_true;

	if (!have_ntr) { /* count traces */
		if (verbose) {
		    warn("ntr header field not set or n2 & n3 not getparred");
		    warn("... counting traces");
		}

		/* Create temporary "file" to hold data */
		/* Create temporary "file" to hold data */
		if (STREQ(tmpdir,"")) {
			tracefp = etmpfile();
			if (verbose) warn("using tmpfile() call");
		} else { /* user-supplied tmpdir */
			char directory[BUFSIZ];
			strcpy(directory, tmpdir);
			strcpy(tracefile, temporary_filename(directory));
			/* Handle user interrupts */
			signal(SIGINT, (void (*) (int)) closefiles);
			signal(SIGQUIT, (void (*) (int)) closefiles);
			signal(SIGHUP,  (void (*) (int)) closefiles);
			signal(SIGTERM, (void (*) (int)) closefiles);
			tracefp = efopen(tracefile, "w+");
			istmpdir=cwp_true;		
			if (verbose)
			      warn("putting temporary files in %s", directory);
		}


		/* Loop over input frames & put them into the data file */
		ntr = 0;
		do {
			++ntr;
			efwrite(tr.data, FSIZE, nt, tracefp);
		} while (gettr(&tr));

	} 

	/* Set dimensions of cube */
	if (!have_n2 && !have_n3) { n2 = ntr; n3=1; } 
	if (have_n2 && !have_n3) n3 = ntr/n2;
	if (!have_n2 && have_n3) n2 = ntr/n3;

	/* Set up pscube command line */
	sprintf(plotcmd,
		"%spscubecontour n1=%d n2=%d n3=%d d1=%f d2=%f d3=%f f1=%f f2=%f f3=%f",
			   bindir, nt, n2, n3, d1, d2, d3, f1, f2, f3);

	fprintf(stderr,"plotcmd = %s", plotcmd);

	for (--argc, ++argv; argc; --argc, ++argv) {
		if (strncmp(*argv, "d1=", 3) && /* skip those already set */
		    strncmp(*argv, "d2=", 3) &&
		    strncmp(*argv, "d3=", 3) &&
		    strncmp(*argv, "f1=", 3) &&
		    strncmp(*argv, "f2=", 3) &&
		    strncmp(*argv, "f3=", 3)) {
		    
			strcat(plotcmd, " ");   /* put a space between args */
			strcat(plotcmd, "\"");  /* user quotes are stripped */
			strcat(plotcmd, *argv); /* add the arg */
			strcat(plotcmd, "\"");  /* user quotes are stripped */
		}
	}


	/* Open pipe to pscubecontour and send the traces */
	plotfp = epopen(plotcmd, "w");
	
	if (!have_ntr) {
			/* send out stored traces one by one */
		rewind(tracefp);
		{ register int itr;
			for (itr = 0; itr < ntr; ++itr) {
				efread (trbuf, FSIZE, nt, tracefp);
				efwrite(trbuf, FSIZE, nt, plotfp);
			}
		}
	} else { /* just pump out traces and let pscubecontour do the work */
		do {
			efwrite(tr.data, FSIZE, nt, plotfp);
		} while (gettr(&tr));
	}


	/* Clean up */
	epclose(plotfp);
	if (!have_ntr) {
		efclose(tracefp);
		if (istmpdir) eremove(tracefile);
	}

	return EXIT_SUCCESS;
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(tracefp);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}
