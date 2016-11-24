/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUSHAPE: $Revision: 1.16 $ ; $Date: 2015/08/07 22:00:45 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUSHAPE - Wiener shaping filter					",
" 									",
"  sushape <stdin >stdout  [optional parameters]			",
" 									",
" Required parameters:							",
" w=		vector of input wavelet to be shaped or ...		",
" ...or ... 								",
" wfile=        ... file containing input wavelet in SU (SEGY trace) format",
" d=		vector of desired output wavelet or ...			",
" ...or ... 								",
" dfile=        ... file containing desired output wavelet in SU format	",
" dt=tr.dt		if tr.dt is not set in header, then dt is mandatory",
" 									",
" Optional parameters:							",
" nshape=trace		length of shaping filter			",
" pnoise=0.001		relative additive noise level			",
" showshaper=0		=1 to show shaping filter 			",
" 									",
" verbose=0		silent; =1 chatty				",
" 									",
"Notes:									",
" 									",
" Example of commandline input wavelets: 				",
"sushape < indata  w=0,-.1,.1,... d=0,-.1,1,.1,... > shaped_data	",
" 									",
"sushape < indata  wfile=inputwavelet.su dfile=desire.su > shaped_data	",
" 									",
" To get the shaping filters into an ascii file:			",
" ... | sushape ... showwshaper=1 2>file | ...   (sh or ksh)		",
" (... | sushape ... showshaper=1 | ...) >&file  (csh)			",
" 									",
NULL};

/* Credits:
 *	CWP: Jack Cohen
 *	CWP: John Stockwell, added wfile and dfile  options
 *
 * Trace header fields accessed: ns, dt
 * Trace header fields modified: none
 *
 */
/**************** end self doc *******************************************/


#define PNOISE	0.001


segy intrace, outtrace;
segy dtr, wtr;

int
main(int argc, char **argv)
{
	int nt;			/* number of points on trace		*/

	float dt;		/* time sample interval (sec)		*/
	float *shaper;		/* shaping filter coefficients		*/
	float *spiker;		/* spiking decon filter (not used)	*/
	float *w;		/* input wavelet			*/

	int nw;			/* length of input wavelet in samples	*/
	float *d;		/* desired output wavelet		*/

	int nd;			/* length of desired wavelet in samples	*/
	int nshape;		/* length of shaping filter in samples	*/

	float pnoise;		/* pef additive noise level		*/
	float *crosscorr;	/* right hand side of Wiener eqs	*/
	float *autocorr;	/* vector of autocorrelations		*/
	int showshaper;		/* flag to display shaping filter	*/
        float f_zero=0.0;       /* zero valued item for comparison      */

	cwp_String wfile="";	/* input wavelet file name		*/
	cwp_String dfile="";	/* desired output wavelet file name	*/
	FILE *wfp;		/* input wavelet file pointer 		*/
	FILE *dfp;		/* desired wavelet file pointer		*/
	int verbose=0;		/* =0 silent; =1 chatty			*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&intrace)) err("can't get first trace");
	nt = intrace.ns;
	dt = intrace.dt/1000000.0;	if (!dt) MUSTGETPARFLOAT ("dt", &dt);


	/* Get parameters */
	if (!getparint("showshaper",  &showshaper))	showshaper = 0;
	if (!getparint("nshape",  &nshape))		nshape = nt;
	if (!getparfloat("pnoise",  &pnoise))		pnoise = PNOISE;
	if (!getparint("verbose", &verbose))		verbose = 0;

	/* Open dfile and wfile if they have been getparred */
	getparstring("dfile",&dfile);	
	getparstring("wfile",&wfile);	

	if ((*dfile=='\0')) { /* if no dfile, then get from command line */
		if (!(nd = countparval("d")))
			err("must specify d= desired wavelet");
		d = ealloc1float(nd);	getparfloat("d", d);

	} else { /* read from dfile  */

                if((dfp=fopen(dfile,"r"))==NULL)
                        err("cannot open dfile=%s\n",dfile);

        	if (!fgettr(dfp,&dtr))  err("can't get input wavelet");
        		nd = (int) dtr.ns;
		d = ealloc1float(nd);
		memcpy((void *) d, (const void *) dtr.data, nd*FSIZE);
	}
		
	if ((*wfile=='\0')) { /* then get w from command line */
		if (!(nw = countparval("w")))
			err("must specify w= desired wavelet");
		w = ealloc1float(nw);	getparfloat("w", w);

	} else { /* read from wfile  */

                if((wfp=fopen(wfile,"r"))==NULL)
                        err("cannot open wfile=%s\n",wfile);

        	if (!fgettr(wfp,&wtr))  err("can't get desired output wavelet");
        		nw = (int) wtr.ns;
		w = ealloc1float(nw);
		memcpy((void *) w, (const void *) wtr.data, nw*FSIZE);
	}

        checkpars();

	/* Get shaping filter by Wiener-Levinson */
	shaper	  = ealloc1float(nshape);
	spiker 	  = ealloc1float(nshape);	/* not used */
	crosscorr = ealloc1float(nshape);
	autocorr  = ealloc1float(nshape);
	xcor(nw, 0, w, nw, 0, w, nshape, 0, autocorr);  /* for matrix */
	xcor(nw, 0, w, nd, 0, d, nshape, 0, crosscorr); /* right hand side */
        if (CLOSETO(autocorr[0],f_zero))  err("can't shape with zero wavelet");
	autocorr[0] *= (1.0 + pnoise);			/* whiten */
	stoepf(nshape, autocorr, crosscorr, shaper, spiker);
		

	/* Show shaper on request */
	if (showshaper) {
		register int i;
		if (verbose) warn("Shaping filter:");
		for (i = 0; i < nshape; ++i)
			fprintf(stderr, "%10g%c", shaper[i],
				(i%6==5 || i==nshape-1) ? '\n' : ' ');
	}



	/* Main loop over traces */
	do {
		/* Center and convolve shaping filter with trace */
		convolve_cwp(nshape, (nw-nd)/2, shaper,
		     nt, 0, intrace.data, 
                     nt, 0, outtrace.data);        


		/* Output filtered trace */
		memcpy( (void *) &outtrace, (const void *) &intrace, HDRBYTES);
		puttr(&outtrace);

	} while (gettr(&intrace));


	return(CWP_Exit());
}
