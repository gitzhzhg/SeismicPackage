/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUGAIN: $Revision: 1.61 $ ; $Date: 2013/10/21 20:15:08 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>
#include <float.h>

/*********************** self documentation *****************************/
char *sdoc[] = {
"									",
" SUGAIN - apply various types of gain				  	",
"									",
" sugain <stdin >stdout [optional parameters]			   	",
"									",
" Required parameters:						  	",
"	none (no-op)						    	",
"									",
" Optional parameters:						  	",
"	panel=0	        =1  gain whole data set (vs. trace by trace)	",
"	tpow=0.0	multiply data by t^tpow			 	",
"	epow=0.0	multiply data by exp(epow*t)		    	",
"	etpow=1.0	multiply data by exp(epow*t^etpow)	    	",
"	gpow=1.0	take signed gpowth power of scaled data	 	",
"	agc=0	   flag; 1 = do automatic gain control	     		",
"	gagc=0	  flag; 1 = ... with gaussian taper			",
"	wagc=0.5	agc window in seconds (use if agc=1 or gagc=1)  ",
"	trap=none	zero any value whose magnitude exceeds trapval  ",
"	clip=none	clip any value whose magnitude exceeds clipval  ",
"	pclip=none	clip any value greater than clipval  		",
"	nclip=none	clip any value less than  clipval 		",
"	qclip=1.0	clip by quantile on absolute values on trace    ",
"	qbal=0	  flag; 1 = balance traces by qclip and scale     	",
"	pbal=0	  flag; 1 = bal traces by dividing by rms value   	",
"	mbal=0	  flag; 1 = bal traces by subtracting the mean    	",
"	maxbal=0	flag; 1 = balance traces by subtracting the max ",
"	scale=1.0	multiply data by overall scale factor	   	",
"	norm=0.0	divide data by overall scale factor	     	",
"	bias=0.0	bias data by adding an overall bias value	",
"	jon=0	   	flag; 1 means tpow=2, gpow=.5, qclip=.95	",
"	verbose=0	verbose = 1 echoes info				",
"	mark=0		apply gain only to traces with tr.mark=0	",
"			=1 apply gain only to traces with tr.mark!=0    ",
"	vred=0	  reducing velocity of data to use with tpow		",
"									",
" 	tmpdir=		if non-empty, use the value as a directory path	",
"			prefix for storing temporary files; else if the ",
"			the CWP_TMPDIR environment variable is set use  ",
"			its value for the path; else use tmpfile()	",
"									",
" Operation order:							",
" if (norm) scale/norm						  	",
"									",
" out(t) = scale * BAL{CLIP[AGC{[t^tpow * exp(epow * t^tpow) * ( in(t)-bias )]^gpow}]}",
"									",
" Notes:								",
"	The jon flag selects the parameter choices discussed in		",
"	Claerbout's Imaging the Earth, pp 233-236.			",
"									",
"	Extremely large/small values may be lost during agc. Windowing  ",
"	these off and applying a scale in a preliminary pass through	",
"	sugain may help.						",
"									",
"	Sugain only applies gain to traces with tr.mark=0. Use sushw,	",
"	suchw, suedit, or suxedit to mark traces you do not want gained.",
"	See the selfdocs of sushw, suchw, suedit, and suxedit for more	",
"	information about setting header fields. Use \"sukeyword mark\" ",
"	for more information about the mark header field.		",
"									",
"      debias data by using mbal=1					",
"									",
"      option etpow only becomes active if epow is nonzero		",
NULL};

/* Credits:
 *	SEP: Jon Claerbout
 *	CWP: Jack K. Cohen, Brian Sumner, Dave Hale
 *
 * Note: Have assumed tr.deltr >= 0 in tpow routine.
 *
 * Technical Reference:
 *	Jon's second book, pages 233-236.
 *
 * Trace header fields accessed: ns, dt, delrt, mark, offset
 */
/**************** end self doc *******************************************/

/* subroutine prototypes */
void gain(float *data, float tpow, float epow, float etpow, float gpow, float vred,
	  int agc, int gagc, int qbal, int pbal, int mbal, float scale, float bias,
	  register float trap, register float clip, float qclip, int iwagc,
	  register float tmin, register float dt, int nt,
	  int maxbal ,float pclip ,float nclip );
void do_tpow(float *data, float tpow, float vred, register float tmin,
	     register float dt, int nt);
void do_epow(float *data, float epow, float etpow, register float tmin, register float dt,
	     int nt);
void do_trap(float *data, register float trap, register int nt);
void do_clip(float *data, register float clip, register int nt);
void do_nclip(float *data, register float nclip, register int nt);
void do_pclip(float *data, register float pclip, register int nt);
void do_qclip(float *data, float qclip, int nt);
void do_qbal(float *data, float qclip, int nt);
void do_agc(float *data, int iwagc, int nt);
void do_gagc(float *data, int iwagc, int nt);
float quant(float *a, int k, int n);
static void closefiles(void);

#define TPOW     0.0
#define EPOW     0.0
#define ETPOW    1.0
#define GPOW     1.0
#define TRAP     0.0
#define CLIP     0.0
#define QCLIP    1.0
#define SCALE    1.0
#define BIAS     0.0
#define WAGC     0.5
#define VRED     0.0

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr;

int
main(int argc, char **argv)
{
	int verbose;	/* flag for echoing info			*/
	int jon;	/* flag to get Claerbout values		 	*/
	int agc;	/* agc flag				     	*/
	int gagc;	/* gaussian agc flag			    	*/
	int pbal;	/* power balance flag			   	*/
	int qbal;	/* quantile balance flag			*/
	int mbal=0;     /* mean balance flag			    	*/
	float tpow;     /* exponent of t				*/
	float epow;     /* deattenutation coefficient		   	*/
	float etpow;    /* deattenutation power of t			*/
	float gpow;     /* dynamic compression power		    	*/
	float vred;	/* data reducing velocity in meters per second	*/
	float trap;     /* zero any larger value magnitude than trapval */
	float clip;     /* clip any larger value magnitude than clipval */
	float pclip;    /* clip any value greater than clipval		*/
	float nclip;    /* clip any value less than clipval		*/
	float qclip;    /* clip at qth quantile (100qth percentile)     */
	float scale;    /* overall scale factor				*/
	float norm;     /* reciprocal of scale factor			*/
	float bias=0.0; /* overall bias  value				*/
	float wagc;     /* size of agc window in seconds		*/
	int iwagc=0;    /* ... half window in samples		   	*/
	int nt;	 /* number of samples on trace		   		*/
	float tmin;     /* delay recording time in secs		 	*/
	float dt;	/* sample rate in secs			  	*/
	float *data;	/* the data					*/
	int panel;	/* gain trace by trace or whole data set?	*/

	int maxbal=0;   /* max balance flag			     	*/

	int mark;	/* mark flag					*/


	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user given path		*/


	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);
	

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);


	/* Get nt from first trace */
	if (!gettr(&tr)) err("can't get first trace");
	nt   = (int) tr.ns;
	dt = ((double) tr.dt)/1000000.0;   /* microsecs to secs */
	tmin = tr.delrt/1000.0;		   /* millisecs to secs */
	if (!dt) getparfloat("dt", &dt);
	if (!dt) MUSTGETPARFLOAT("dt", &dt);


	/* Get parameters */
	if (!getparfloat ("tpow" , &tpow))	tpow     = TPOW;
	if (!getparfloat ("epow" , &epow))	epow     = EPOW;
	if (!getparfloat ("etpow" , &etpow))	etpow    = ETPOW;
	if (!getparfloat ("gpow" , &gpow))	gpow     = GPOW;
	if (!getparfloat ("vred" , &vred))	vred     = VRED;
	if (!getparfloat ("trap" , &trap))	trap     = TRAP;
	if (!getparfloat ("clip" , &clip))	clip     = CLIP;
	if (!getparfloat ("pclip" , &pclip))    pclip    = FLT_MAX;
	if (!getparfloat ("nclip" , &nclip))    nclip    = -FLT_MAX;
	if (!getparfloat ("qclip", &qclip))     qclip    = QCLIP;
	if (!getparfloat ("scale", &scale))     scale    = SCALE;
	if (!getparfloat ("norm",  &norm)) 	norm     = 0.0;
	if (!getparfloat ("bias",  &bias))	bias     = BIAS;
	if (!getparfloat ("wagc" , &wagc))	wagc     = WAGC;
	if (!getparint   ("agc"  , &agc))	agc	= 0;
	if (!getparint   ("gagc" , &gagc))	gagc     = 0;
	if (!getparint   ("pbal" , &pbal))	pbal     = 0;
	if (!getparint   ("qbal" , &qbal))	qbal     = 0;
	if (!getparint   ("mbal" , &mbal))	mbal     = 0;
	if (!getparint   ("panel", &panel))     panel    = 0;
	if (!getparint   ("jon"  , &jon))	jon	= 0;
	if (!getparint("verbose", &verbose))	verbose  = 0;
	if (!getparint   ("maxbal" , &maxbal))	maxbal   = 0;
	if (!getparint   ("mark" , &mark))	mark     = 0;
	
        checkpars();

	/* Data validation */
	if (vred < 0.0) err("vred = %f, must be positive", vred);
	if (trap < 0.0) err("trap = %f, must be positive", trap);
	if (clip < 0.0) err("clip = %f, must be positive", clip);
	if (qclip < 0.0 || qclip > 1.0) 
		err("qclip = %f, must be between 0 and 1", qclip);
	if (agc || gagc) {
		iwagc = NINT(wagc/dt);
		if (iwagc < 1) err("wagc=%g must be positive", wagc);
		if (iwagc > nt) err("wagc=%g too long for trace", wagc);
		iwagc >>= 1;  /* windows are symmetric, so work with half */
	}
	if (jon) { 
		tpow  = 2.0;
		gpow  = 0.5;
		qclip = 0.95;
	}

	if (norm) {
		scale /= norm;
	}

	/* Main loop over traces */
	if (!panel) { /* trace by trace */
		data = ealloc1float(nt);
		do {
			memcpy((void *)data, (const void *) tr.data, nt*FSIZE);

			if (!(tr.mark || mark) ) {
				gain(data, tpow, epow, etpow, gpow, vred, agc, gagc,
				     qbal, pbal, mbal, scale, bias, trap, clip,
				     qclip, iwagc, tmin, dt, nt, maxbal, pclip,
				     nclip );
			} else if ( (mark) && (tr.mark) ) {
				gain(data, tpow, epow, etpow, gpow, vred, agc, gagc,
				     qbal, pbal, mbal, scale, bias, trap, clip,
				     qclip, iwagc, tmin, dt, nt, maxbal, pclip,
				     nclip );
			}

			memcpy((void *)tr.data, (const void *) data, nt*FSIZE);
			puttr(&tr);

		} while(gettr(&tr));
	} else { /* do whole data set at once */
		int itr, ntr = 0;
		
		/* Store traces, headers in tempfiles while getting a count */
		if (STREQ(tmpdir,"")) {
			tracefp = etmpfile();
			headerfp = etmpfile();
			if (verbose) warn("using tmpfile() call");
		} else { /* user-supplied tmpdir */
		char directory[BUFSIZ];
		strcpy(directory, tmpdir);
		strcpy(tracefile, temporary_filename(directory));
		strcpy(headerfile, temporary_filename(directory));
		/* Handle user interrupts */
		signal(SIGINT, (void (*) (int)) closefiles);
		signal(SIGQUIT, (void (*) (int)) closefiles);
		signal(SIGHUP,  (void (*) (int)) closefiles);
		signal(SIGTERM, (void (*) (int)) closefiles);
		tracefp = efopen(tracefile, "w+");
		headerfp = efopen(headerfile, "w+");
			istmpdir=cwp_true;		
		if (verbose) warn("putting temporary files in %s", directory);
	}
		do {
			++ntr;
			efwrite(&tr, HDRBYTES, 1, headerfp);
			efwrite(tr.data, FSIZE, nt, tracefp);
		} while (gettr(&tr));
		erewind(tracefp);
		erewind(headerfp);
		data = ealloc1float(nt*ntr);
		
		/* Load traces into data and close tmpfile */
		efread(data, FSIZE, nt*ntr, tracefp);
		efclose(tracefp);
		if (istmpdir) eremove(tracefile);
	
		gain(data, tpow, epow, etpow, gpow, vred, agc, gagc, qbal,
		     pbal, mbal, scale, bias, trap, clip, qclip,
		     iwagc, tmin, dt, nt*ntr, maxbal, pclip, nclip );

		for (itr = 0; itr < ntr; itr++) {
			memcpy((void *) tr.data, (const void *) (data+itr*nt),
				nt*FSIZE);
			efread(&tr, 1, HDRBYTES, headerfp);
			puttr(&tr);
		}
		efclose(headerfp);
		if (istmpdir) eremove(headerfile);
	}
	
	free1(data);
	
	return(CWP_Exit());
}


/* Multiply by t^tpow */
void do_tpow(
	float *data,		/* the data			*/
	float tpow,	     /* multiply data by t^tpow	*/
	float vred,		/* reducing velocity		*/
	register float tmin,    /* first time on record	 */
	register float dt,	/* sampling rate in seconds     */
	int nt		  /* number of samples	    */
)
{
	static cwp_Bool first = cwp_true;   /* first entry flag     */
	static float *tpowfac;	  /* tpow values	  */
	register int i;		 /* counter		*/
	register float tred;	/* reduced time in seconds	*/

	if (first) { /* first entry, set up array of tpow factors */
		tpowfac = ealloc1float(nt);

		/* protect against negative tpow */
		tpowfac[0] = (tmin == 0.0) ? 0.0 : pow(tmin, tpow);
		for (i = 1; i < nt; ++i) 
			tpowfac[i] = pow(tmin + i*dt, tpow);

		first = cwp_false;
		/* for (i = 0; i < nt; ++i)
		   fprintf(stderr,"%d %f\n",i,tpowfac[i]); */
	} /* end first entry */

	if ( vred > 0.0 ) {	/* recompute array of tpowfac for each trace */
		tred = (float)tr.offset / vred;
		if ( tred < 0.0 ) tred *= -1.0;	/* remove sign */
		for (i = 1; i < nt; ++i)
			tpowfac[i] = pow(tmin + tred + i*dt, tpow);
	} /* fprintf(stderr,"%f %f %f\n",tred,tmin,tmin+(nt-1)*dt); */

	for (i = 0; i < nt; ++i)  data[i] *= tpowfac[i];
}


/* Exponential deattenuation  with deattenuation factor epow */
/* and with  with deattenuation  power etpow */
void do_epow(
	float *data,		/* the data			*/
	float epow,	     /* coefficient of t in exponent */
	float etpow,	     /* exponent of t in exponent */
	register float tmin,    /* first time on record	 */
	register float dt,	/* sampling rate in seconds     */
	int nt		  /* number of samples	    */
)
{
	register int i;		 /* counter		*/
	static cwp_Bool first = cwp_true;   /* first entry flag     */
	static float *epowfac;	  /* exponent stretchs    */
	static float *etpowfac;	  /* etpow values	  */

	if (first) {
		epowfac = ealloc1float(nt);
		etpowfac = ealloc1float(nt);


		/* protect against negative tpow */
		etpowfac[0] = (tmin == 0.0) ? 0.0 : pow(tmin, etpow);
		for (i = 1; i < nt; ++i) 
			etpowfac[i] = pow(tmin + i*dt, etpow);

		for (i = 0; i < nt; i++) 
			epowfac[i] = exp(epow * etpowfac[i]);

		first = cwp_false;
	}

	for (i = 0; i < nt; ++i)  data[i] *= epowfac[i];
}


/* Zero out outliers */
void do_trap(
	float *data,		/* the data			*/
	register float trap,    /* zero if magnitude > trap     */
	register int nt	 /* number of samples	    */
)
{
	register float *dataptr = data;

	while (nt--) {
		if (ABS(*dataptr) > trap) *dataptr = 0.0;
		dataptr++;
	}
}


/* Hard clip outliers */
void do_clip(
	float *data,		/* the data				*/
	register float clip,    /* hard clip if magnitude > clip	*/
	register int nt	 /* number of samples		    */
)
{
	register float *dataptr = data;
	register float mclip = -clip;

	while (nt--) {
		if (*dataptr > clip) {
			*dataptr = clip;
		} else if (*dataptr < mclip) {
			*dataptr = mclip;
		}
		dataptr++;
	}
}



/* Hard clip maxima */
void do_pclip(
	float *data,		/* the data				*/
	register float pclip,    /* hard clip if magnitude > clip	*/
	register int nt	 /* number of samples		    */
)
{
	register float *dataptr = data;

	while (nt--) {
		if (*dataptr > pclip) {
			*dataptr = pclip;
		}
		dataptr++;
	}
}


/* Hard clip minima */
void do_nclip(
	float *data,		/* the data				*/
	register float nclip,    /* hard clip if magnitude > clip	*/
	register int nt	 /* number of samples		    */
)
{
	register float *dataptr = data;

	while (nt--) {
		if (*dataptr < nclip) {
			*dataptr = nclip;
		}
		dataptr++;
	}
}


/* Quantile clip on magnitudes of trace values */
void do_qclip(
	float *data,	/* the data			*/
	float qclip,    /* quantile at which to clip    */
	int nt	  /* number of sample points	*/
)
{
	register int i;
	static cwp_Bool first = cwp_true;   /* first entry flag	     */
	static float *absdata;	  /* absolute value trace	 */
	static int iq;		  /* index of qclipth quantile    */
	float clip;		     /* ... value of rank[iq]	*/

	if (first) {
		absdata = ealloc1float(nt);
		iq = (int) (qclip * nt - 0.5); /* round, don't truncate */
		first = cwp_false;
	}

	/* Clip on value corresponding to qth quantile */
	for (i = 0; i < nt; ++i)  absdata[i] = ABS(data[i]);
	clip = quant(absdata, iq, nt);
	do_clip(data, clip, nt);
}


/* Quantile balance */
void do_qbal(
	float *data,	/* the data			*/
	float qclip,    /* quantile at which to clip    */
	int nt	  /* number of sample points	*/
)
{
	register int i;
	static cwp_Bool first = cwp_true;   /* first entry flag	     */
	static float *absdata;	  /* absolute value trace	 */
	static int iq;		  /* index of qclipth quantile    */
	float bal;			/* value used to balance trace  */

	if (qclip == 1.0) { /* balance by max magnitude on trace */
		bal = ABS(data[0]);
		for (i = 1; i < nt; ++i)  bal = MAX(bal, ABS(data[i]));

		if ((bal == 0.0)) {
			return;
		} else {
			for (i = 0; i < nt; ++i)  data[i] /= bal;
			return;
		}
	} else if (first) {
		absdata = ealloc1float(nt);
		iq = (int) (qclip * nt - 0.5); /* round, don't truncate */
		first = cwp_false;
	}

	/* Balance by quantile value (qclip < 1.0) */
	for (i = 0; i < nt; ++i)  absdata[i] = ABS(data[i]);
	bal = quant(absdata, iq, nt);

	if ((bal == 0.0)) {
		return;
	} else {
		for (i = 0; i < nt; ++i)  data[i] /= bal;
		do_clip(data, 1.0, nt);
		return;
	}
}


/* Automatic Gain Control--standard box */
void do_agc(float *data, int iwagc, int nt)
{
	static cwp_Bool first = cwp_true;
	static float *agcdata;
	register int i;
	register float val;
	register float sum;
	register int nwin;
	register float rms;


	/* allocate room for agc'd data */
	if (first) {
		first = cwp_false;
		agcdata = ealloc1float(nt);
	}


	/* compute initial window for first datum */
	sum = 0.0;
	for (i = 0; i < iwagc+1; ++i) {
		val = data[i];
		sum += val*val;
	}
	nwin = 2*iwagc+1;
	rms = sum/nwin;
	agcdata[0] = (rms <= 0.0) ? 0.0 : data[0]/sqrt(rms);

	/* ramping on */
	for (i = 1; i <= iwagc; ++i) {
		val = data[i+iwagc];
		sum += val*val;
		++nwin;
		rms = sum/nwin;
		agcdata[i] = (rms <= 0.0) ? 0.0 : data[i]/sqrt(rms);
	}

	/* middle range -- full rms window */
	for (i = iwagc + 1; i <= nt-1-iwagc; ++i) {
		val = data[i+iwagc];
		sum += val*val;
		val = data[i-iwagc];
		sum -= val*val; /* rounding could make sum negative! */
		rms = sum/nwin;
		agcdata[i] = (rms <= 0.0) ? 0.0 : data[i]/sqrt(rms);
	}

	/* ramping off */
	for (i = nt - iwagc; i <= nt-1; ++i) {
		val = data[i-iwagc];
		sum -= val*val; /* rounding could make sum negative! */
		--nwin;
		rms = sum/nwin;
		agcdata[i] = (rms <= 0.0) ? 0.0 : data[i]/sqrt(rms);
	}

	/* copy data back into trace */
	memcpy( (void *) data, (const void *) agcdata, nt*FSIZE);

	return;
}


#define EPS     3.8090232	/* exp(-EPS*EPS) = 5e-7, "noise" level  */

/* Automatic Gain Control--gaussian taper */
void do_gagc(float *data, int iwagc, int nt)
{
	static cwp_Bool first=cwp_true; /* first entry flag		 */
	static float *agcdata;  /* agc'd data			   */
	static float *w;	/* Gaussian window weights		*/
	static float *d2;	/* square of input data		 */
	static float *s;	/* weighted sum of squares of the data  */
	float u;		/* related to reciprocal of std dev     */
	float usq;		/* u*u				  */


	if (first) {
		first = cwp_false;

		/* Allocate room for agc'd data */
		agcdata = ealloc1float(nt);

		/* Allocate and compute Gaussian window weights */
		w = ealloc1float(iwagc);  /* recall iwagc is HALF window */
		u = EPS / ((float) iwagc);
		usq = u*u;
		{
			register int i;
			float floati;

			for (i = 1; i < iwagc; ++i) {
				floati = (float) i;
				w[i] = exp(-(usq*floati*floati));
			}
		}

		/* Allocate sum of squares and weighted sum of squares */
		d2 = ealloc1float(nt);
		s  = ealloc1float(nt);
	}


	/* Agc the trace */
	{
		register int i, j, k;
		register float val;
		register float wtmp;
		register float stmp;

		/* Put sum of squares of data in d2 and */
		/* initialize s to d2 to get center point set */
		for (i = 0; i < nt; ++i) {
			val = data[i];
			s[i] = d2[i] = val * val;
		}

		/* Compute weighted sum s; use symmetry of Gaussian */
		for (j = 1; j < iwagc; ++j) {
			wtmp = w[j];
			for (i = j; i < nt; ++i)  s[i] += wtmp*d2[i-j]; 
			k = nt - j;
			for (i = 0; i < k; ++i)   s[i] += wtmp*d2[i+j]; 
		}

		for (i = 0; i < nt; ++i) {
			stmp = s[i];
			agcdata[i] = (!stmp) ? 0.0 : data[i]/sqrt(stmp);
		}

		/* Copy data back into trace */
		memcpy( (void *) data, (const void *) agcdata, nt*FSIZE);
	}


	return;
}


/*
 * QUANT - find k/n th quantile of a[]
 *
 * Works by reordering a so a[j] < a[k] if j < k.
 *
 * Parameters:
 *    a	 - data
 *    k	 - indicates quantile
 *    n	 - number of points in data
 *
 * This is Hoare's algorithm worked over by SEP (#10, p100) and Brian.
 */

float quant(float *a, int k, int n)
{
	register int i, j;
	int low, hi;
	register float ak, aa;

	low = 0; hi = n-1;

	while (low < hi) {
		ak = a[k];
		i = low;
		j = hi;
		do {
			while (a[i] < ak) i++;
			while (a[j] > ak) j--;
			if (i <= j) {
				aa = a[i]; a[i] = a[j]; a[j] = aa;
				i++;
				j--;
			}
		} while (i <= j);

		if (j < k) low = i;

		if (k < i) hi = j;
	}

	return(a[k]);
}

/*
 * GAIN - apply all the various gains
 *
 */
void gain(float *data, float tpow, float epow, float etpow, float gpow, float vred,
	  int agc, int gagc, int qbal, int pbal, int mbal, float scale, float bias,
	  register float trap, register float clip, float qclip, int iwagc,
	  register float tmin, register float dt, int nt,
	  int maxbal ,float pclip ,float nclip )
{
    float f_two  = 2.0;
    float f_one  = 1.0;
    float f_half = 0.5;
    register int i;

	if (bias) {
		for (i = 0; i < nt; ++i)  data[i]+=bias ;
	}
	if (tpow) {
		do_tpow(data, tpow, vred, tmin, dt, nt);
	}
	if (epow) {
		do_epow(data, epow, etpow, tmin, dt, nt);
	}
	if (!CLOSETO(gpow, f_one)) {
		register float val;

		if (CLOSETO(gpow, f_half)) {
			for (i = 0; i < nt; ++i) {
				val = data[i];
				data[i] = (val >= 0.0) ?
					sqrt(val) :
				-sqrt(-val);
			}
		} else if (CLOSETO(gpow, f_two)) {
			for (i = 0; i < nt; ++i) {
				val = data[i];
				data[i] = val * ABS(val);
			}
		} else {
			for (i = 0; i < nt; ++i) {
				val = data[i];
				data[i] = (val >= 0.0) ?
					pow(val, gpow) :
				-pow(-val, gpow);
			}
		}
	}
	if (agc)		   do_agc(data, iwagc, nt);
	if (gagc)		  do_gagc(data, iwagc, nt);
	if (trap > 0.0)	    do_trap(data, trap, nt);
	if (clip > 0.0)	    do_clip(data, clip, nt);
	if (pclip < FLT_MAX )	do_pclip(data, pclip, nt);
	if (nclip > -FLT_MAX )     do_nclip(data, nclip, nt);
	if (qclip < 1.0 && !qbal)  do_qclip(data, qclip, nt);
	if (qbal)		  do_qbal(data, qclip, nt);
	if (pbal) {
		register int i;
		register float val;
		register float rmsq = 0.0;
		
		/* rmsq = sqrt (SUM( a()*a() ) / nt) */
		for (i = 0; i < nt; ++i) {
			val = data[i];
			rmsq += val * val;
		}
		rmsq = sqrt(rmsq / nt);

		if (rmsq) {
			for (i = 0; i < nt; ++i)
				data[i] /= rmsq;
		}
	}
	if (mbal) {
		register int i;
		register float mean = 0.0;
		
		/* mean = SUM (data[i] / nt) */
		for (i = 0; i < nt; ++i) {
			mean+=data[i];
		}
		/* compute the mean */
		mean/=nt;

		/* subtract the mean from each sample */
		if (mean) {
			for (i = 0; i < nt; ++i)
				data[i]-=mean;
		}
	}

	if (maxbal) {
		register int i;
		register float max = data[0];
		
		/* max */
		for (i = 0; i < nt; ++i) {
			if( data[i] > max ) max = data[i];
		}

		/* subtract max */
		for (i = 0; i < nt; ++i) data[i]-=max;
	}


	if (!CLOSETO(scale, f_one)) {
		register int i;

		for (i = 0; i < nt; ++i)  data[i] *= scale;
	}
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

