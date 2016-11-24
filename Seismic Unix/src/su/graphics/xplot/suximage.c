/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUXIMAGE: $Revision: 1.40 $ ; $Date: 2011/11/12 00:45:18 $		*/

#include "su.h"
#include "segy.h"
#include <signal.h>

/*********************** self documentation *****************************/
char *sdoc[] = {
"									",
" SUXIMAGE - X-windows IMAGE plot of a segy data set	                ",
"									",
" suximage infile= [optional parameters] | ...  (direct I/O)            ",
"  or					                		",
" suximage <stdin [optional parameters] | ...	(sequential I/O)        ",
"									",
" Optional parameters:						 	",
"									",
" infile=NULL SU data to be ploted, default stdin with sequential access",
"             if 'infile' provided, su data read by (fast) direct access",
"									",
"	      with ftr,dtr and n2 suximage will pass a subset of data   ",
"	      to the plotting program-ximage:                           ",
" ftr=1       First trace to be plotted                                 ",
" dtr=1       Trace increment to be plotted                             ",
" n2=tr.ntr   (Max) number of traces to be plotted (ntr is an alias for n2)",
"	      Priority: first try to read from parameter line;		",
"		        if not provided, check trace header tr.ntr;     ",
"		        if still not provided, figure it out using ftello",
"									",
" d1=tr.d1 or tr.dt/10^6	sampling interval in the fast dimension	",
"   =.004 for seismic 		(if not set)				",
"   =1.0 for nonseismic		(if not set)				",
" 							        	",
" d2=tr.d2		sampling interval in the slow dimension	        ",
"   =1.0 		(if not set or was set to 0)		        ",
"									",
" key=			key for annotating d2 (slow dimension)		",
" 			If annotation is not at proper increment, try	",
" 			setting d2; only first trace's key value is read",
" 							        	",
" f1=tr.f1 or tr.delrt/10^3 or 0.0  first sample in the fast dimension	",
" 							        	",
" f2=tr.f2 or tr.tracr or tr.tracl  first sample in the slow dimension	",
"   =1.0 for seismic		    (if not set)			",
"   =d2 for nonseismic		    (if not set)			",
" 							        	",
" verbose=0             =1 to print some useful information		",
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
" the processing time or use d2= f2= to override the header values if	",
" not.									",
"									",
" See the ximage selfdoc for the remaining parameters.		        ",
"									",
NULL};

/* Credits:
 *
 *	CWP: Dave Hale and Zhiming Li (ximage, etc.)
 *	   Jack Cohen and John Stockwell (suximage, etc.)
 *	MTU: David Forel, June 2004, added key for annotating d2
 *      ConocoPhillips: Zhaobo Meng, Dec 2004, added direct I/O
 *
 * Notes:
 *
 *      When provide ftr and dtr and infile, suximage can be used to plot 
 *      multi-dimensional volumes efficiently.  For example, for a Offset-CDP
 *      dataset with 32 offsets, the command line
 *      suximage infile=volume3d.su ftr=1 dtr=32 ... &
 *      will display the zero-offset common offset data with ranrom access.  
 *      It is highly recommend to use infile= to view large datasets, since
 *      using stdin only allows sequential access, which is very slow for 
 *      large datasets.
 *
 *	When the number of traces isn't known, we need to count
 *	the traces for ximage.  You can make this value "known"
 *	either by getparring n2 or by having the ntr field set
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
	char plotcmd[BUFSIZ];   /* build ximage command for popen	*/
	float *trbuf;		/* trace buffer				*/
	FILE *plotfp;		/* fp for plot data			*/
	int nt;			/* number of samples on trace	  	*/
	int ntr;      	        /* number of traces			*/
	int verbose;		/* verbose flag				*/
	float d1;		/* time/depth sample rate		*/
	float d2;		/* trace/dx sample rate			*/
	float f1;		/* tmin/zmin			   	*/
	float f2;		/* tracemin/xmin			*/
	cwp_Bool seismic;	/* is this seismic data?		*/
	cwp_Bool have_ntr=cwp_false;/* is ntr known from header or user?*/
	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user given path		*/
	char *infile=NULL;      /* dataset file name */
        FILE *infp=stdin;       /* input SU dataset to be viewed */
        int dtr,ftr,ntraces,nseek,jtr;	/* work variables */

	char *cwproot=NULL;	/* value of CWPROOT environment variable*/
	char *bindir;		/* directory path for tmp files		*/

  	cwp_String skey=NULL, stype=NULL ; /* trace annotation		*/
	Value val1 ;		/* trace annotation			*/
	cwp_Bool akey = 0 ;	/* trace annotation			*/
      
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

        /* ZM: begin: get the line number, trace number type of control parameters for plotting */

        if (!getparint("ftr", &ftr)) ftr=1; /* using fortran convention */
        if (!getparint("dtr", &dtr)) dtr=1;
            
        if (ftr==0) ftr=1; /* for C-people like me */
        ftr--;             /* convert to C convention */
        if (ftr<0) err("First trace to be plotted ftr=%d must be positive",ftr);
        if (dtr<=0) err("Trace increment dtr=%d must be positive",dtr);
        if (ftr<0) err("ftr=%d must not be negative",ftr); 

	if (!getparstring("infile",&infile)) { /* default is stdin */
          infile=NULL;
          infp=stdin;
        } else               /* must provide a valid dataset name */
          if ((infp=fopen(infile, "r"))==NULL) 
            err("Can not open %s to plot",infile);
        /* ZM: end: get the line number, trace number type of control parameters for plotting */
	
	/* Get info from first trace */
	if (!fgettr(infp,&tr)) err("can't get first trace");
	seismic = ISSEISMIC(tr.trid); 
	nt = tr.ns;
	ntr = tr.ntr;
	if (ntr) have_ntr = cwp_true; 

	if (!getparint("verbose", &verbose))  verbose=0;
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
        if (fabs(d1)<=0.1E-20) d1=1.0;

      	if (!getparstring("key", &skey)) {
		akey = 0 ;
		skey = "" ;
	} else {
		akey  = 1 ;
		gethdval(&tr, skey, &val1) ;
		stype = hdtype(skey) ;
	}

	if (!getparfloat("d2", &d2)) d2 = (tr.d2) ? tr.d2 : 1.0;
        if (fabs(d2)<=0.1E-20) d2=1.0;

	if (!getparfloat("f1", &f1)) {
		if      (tr.f1)     f1 = tr.f1;
		else if (tr.delrt)  f1 = (float) tr.delrt/1000.0;
		else                f1 = 0.0;
	}

	if (!getparfloat("f2", &f2)) {
		if	(tr.f2)	f2 = tr.f2;
                else if (akey)      f2 = vtof(stype,val1) ;
		else if (tr.tracr)  f2 = (float) tr.tracr;
		else if (tr.tracl)  f2 = (float) tr.tracl;
		else if (seismic)   f2 = 1.0;
		else	            f2 = 0.0;
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
	/*	sprintf(bindir, "%s/bin", cwproot); */
	}
	/* strcat(bindir,"/"); */   /* put / at the end of bindir */

	/* Allocate trace buffer */
	trbuf = ealloc1float(nt);


        /* ZM: begin: get n2 by random access */
	/* Get or set ntr */
	if (getparint("n2", &ntr) || getparint("ntr", &ntr)) have_ntr = cwp_true;
        else if (infp!=stdin) {  /* ZM */

	    if (efseeko(infp,(off_t) 0,SEEK_END)==-1)
		err("input file size unknown; specify n2\n");

            if (verbose)
              warn("eftello(infp)=%ld",eftello(infp));

	    ntraces = (int) (eftello(infp)/((nt+60)*FSIZE));

            if (verbose)
              warn("There are ntraces=%d in the dataset",ntraces);

            if (ftr>=ntraces) err("First trace pass End of File: ftr=%d ntraces=%d",ftr,ntraces);
            ntr = (int) ((ntraces-1-ftr)/dtr);

            if (verbose)
              warn("Number of traces to be plotted",ntr);

            /* efseeko(infp,(off_t)0,SEEK_SET); */
            have_ntr = cwp_true;
        }
        /* ZM: end: get n2 by random access */


	if (!have_ntr) { /* count traces */
		if (verbose) {
			warn("n2 not getparred and ntr header field not set");
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
		} while (fgettr(infp,&tr));


	} 

	/* Set up ximage command line */
        if (verbose) {
    	    fprintf(stderr,
	        "%s/bin/ximage n1=%d n2=%d d1=%f d2=%f f1=%f f2=%f", cwproot,
		   nt, ntr, d1, d2, f1, f2);
        }

	sprintf(plotcmd,
	    "%s/bin/ximage n1=%d n2=%d d1=%f d2=%f f1=%f f2=%f verbose=%d ", cwproot,
		   nt, ntr, d1, d2, f1, f2, verbose);

	for (--argc, ++argv; argc; --argc, ++argv) {
		if (strncmp(*argv, "d1=", 3) && /* skip those already set */
		    strncmp(*argv, "d2=", 3) &&
		    strncmp(*argv, "f1=", 3) &&
		    strncmp(*argv, "verbose=", 8) &&
		    strncmp(*argv, "n1=", 3) &&
		    strncmp(*argv, "n2=", 3) &&
		    strncmp(*argv, "f2=", 3)) {
		    
			strcat(plotcmd, " ");   /* put a space between args */
			strcat(plotcmd, "\"");  /* user quotes are stripped */
			strcat(plotcmd, *argv); /* add the arg */
			strcat(plotcmd, "\"");  /* user quotes are stripped */
		}
	}


	/* Open pipe to ximage and send the traces */
	plotfp = epopen(plotcmd, "w");
	
	if (!have_ntr) { /* send out stored traces one by one  by sequential access*/
		rewind(tracefp);
		{ register int itr;
			for (itr = 0; itr < ntr; ++itr) {
				efread (trbuf, FSIZE, nt, tracefp);
                                if (itr>=ftr && (itr-ftr)/dtr*dtr==(itr-ftr)) efwrite(trbuf, FSIZE, nt, plotfp);
			}
		}
	} else {  /* ZM: just pump out traces and let ximage do the work by random access */
                for (jtr=0;jtr<ntr;jtr++) {
                    nseek = (off_t)(60+nt)*(ftr+jtr*dtr);
                    efseeko(infp,nseek*((off_t) FSIZE),SEEK_SET);

    	            if (!fgettr(infp,&tr)) break;
	    	        efwrite(tr.data, FSIZE, nt, plotfp);
                } /* ZM: end: reading by random access */
	}


	/* Clean up */
	epclose(plotfp);
	if (!have_ntr) {
		efclose(tracefp);
		if (istmpdir) eremove(tracefile);
	} else
		efclose(infp);

	return EXIT_SUCCESS;
}

/* for graceful interrupt termination */
static void closefiles(void)
{
	efclose(tracefp);
	eremove(tracefile);
	exit(EXIT_FAILURE);
}
