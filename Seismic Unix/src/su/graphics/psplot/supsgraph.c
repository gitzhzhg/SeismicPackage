/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUPSGRAPH: $Revision: 1.21 $ ; $Date: 2011/11/12 00:46:15 $		*/

#include "su.h"
#include "segy.h"
#include <signal.h>

/*********************** self documentation *****************************/
char *sdoc[] = {
" 									",
" SUPSGRAPH - PostScript GRAPH plot of a segy data set			",
" 									",
" supsgraph <stdin [optional parameters] | ...				",
" 									",
" Optional parameters: 							",
" style=seismic		seismic is default here, =normal is alternate	",
"			(see psgraph selfdoc for style definitions)	",
" 									",
" nplot is the number of traces (ntr is an acceptable alias for nplot) 	",
" 									",
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
" 									",
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
" 									",
" See the psgraph selfdoc for the remaining parameters.			",
" 									",
" On NeXT:     supsgraph < infile [optional parameters]  | open      	",
NULL};

/* Credits:
 *
 *	CWP: Dave Hale and Zhiming Li (pswigp, etc.)
 *	   Jack Cohen and John Stockwell (supswigp, etc.)
 *
 * Notes:
 *	When the number of traces isn't known, we need to count
 *	the traces for pswigp.  You can make this value "known"
 *	either by getparring nplot or by having the ntr field set
 *	in the trace header.  A getparred value takes precedence
 *	over the value in the trace header.
 *
 *	When we must compute ntr, we don't allocate a 2-d array,
 *	but just content ourselves with copying trace by trace from
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
	char plotcmd[BUFSIZ];	/* build psgraph command for popen 	*/
	float *trbuf;		/* trace buffer			 	*/
	FILE *plotfp;		/* fp for plot data			*/
	int nt;			/* number of samples on trace		*/
	int ntr;		/* number of traces (plots)		*/
	int verbose;		/* verbose flag				*/
	float d1;		/* time/depth sample rate 		*/
	float d2;		/* trace/dx sample rate 		*/
	float f1;		/* tmin/zmin				*/
	float f2;		/* tracemin/xmin	 		*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	cwp_Bool have_ntr=cwp_false;/* is ntr known from header or user?	*/
	cwp_String style;	/* psgraph style parameter		*/
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
		if (seismic) {
			if (tr.dt) {
				d1 = (float) tr.dt / 1000000.0;
			} else {
				d1 = 0.004;
				warn("tr.dt not set, assuming dt=0.004");
			}
		} else { /* non-seismic data */
			if (tr.d1) {
				d1 = tr.d1;
			} else {
				d1 = 1.0;
				warn("tr.d1 not set, assuming d1=1.0");
			}
		}
	}

	if (!getparfloat("d2", &d2)) d2 = (tr.d2) ? tr.d2 : 1.0;

	if (!getparfloat("f1", &f1)) {
		if (seismic) {
			f1 = (tr.delrt) ? (float) tr.delrt/1000.0 : 0.0;
		} else {
			f1 = (tr.f1) ? tr.f1 : 0.0;
		}
	}

	if (!getparfloat("f2", &f2)) {
		if      (tr.f2)     f2 = tr.f2;
		else if (tr.tracr)  f2 = (float) tr.tracr;
		else if (tr.tracl)  f2 = (float) tr.tracl;
		else if (seismic)   f2 = 1.0;
		else 		    f2 = 0.0;
	}

	if (!getparfloat("f2", &f2)) f2 = 1.0;

	if (!getparstring("style", &style))	style="seismic";

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

	/* Get or set nplot */
	if (getparint("nplot", &ntr) || getparint("ntr", &ntr))
							have_ntr = cwp_true;


	if (!have_ntr) { /* count traces */
		if (verbose) {
 		   warn("nplot not getparred and ntr header field not set");
 		   warn("....  counting traces");
		}

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

		/* Loop over input data and read to temporary file */
		ntr = 0;
		do {
			++ntr;
			efwrite(tr.data, FSIZE, nt, tracefp);
		} while (gettr(&tr));


	} 

	/* Set up psgraph command line */
	sprintf(plotcmd, "%spsgraph n=%d nplot=%d d1=%f f1=%f style=%s", bindir,
			nt, ntr, d1, f1, style);

	for (--argc, ++argv; argc; --argc, ++argv) {
		if (strncmp(*argv, "n=", 2) && /* skip those already set */
		    strncmp(*argv, "nplot=", 6) &&
		    strncmp(*argv, "d1=", 3) &&
		    strncmp(*argv, "f1=", 3) &&
		    strncmp(*argv, "style=", 6)) {

			strcat(plotcmd, " ");   /* put a space between args */
			strcat(plotcmd, "\"");  /* user quotes are stripped */
			strcat(plotcmd, *argv); /* add the arg */
			strcat(plotcmd, "\"");  /* user quotes are stripped */
		}

	}


	/* Open pipe to psgraph and send the traces */
	plotfp = epopen(plotcmd, "w");
	
	if (!have_ntr) { /* send out stored traces one by one */
		rewind(tracefp);
		{ register int itr;
			for (itr = 0; itr < ntr; ++itr) {
				efread (trbuf, FSIZE, nt, tracefp);
				efwrite(trbuf, FSIZE, nt, plotfp);
			}
		}
	} else { /* just pump out traces and let psgraph do the work */
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
