/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUHARLAN: $Revision: 1.11 $ ; $Date: 2015/08/07 22:21:43 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include "taup.h"
#include <time.h>
#include <signal.h>

/**************************** self documentation *****************************/
char *sdoc[] = {
"									",
" SUHARLAN - signal-noise separation by the invertible linear		",
"	    transformation method of Harlan, 1984			",
"									",
"   suharlan <infile >outfile  [optional parameters]			",
"									",
" Required Parameters:						 	",
" <none>								",
"									",
" Optional Parameters:							",
" FLAGS:								",
" niter=1	number of requested iterations				",
" anenv=1	=1 for positive analytic envelopes			",
"		=0 for no analytic envelopes (not recommended)		",
" scl=0		=1 to scale output traces (not recommended)		",
" plot=3	=0 for no plots. =1 for 1-D plots only			",
"		=2 for 2-D plots only. =3 for all plots			",
" norm=1	=0 not to normalize reliability values			",
" verbose=1	=0 not to print processing information			",
" rgt=2		=1 for uniform random generator				",
"		=2 for gaussian random generator			",
" sts=1		=0 for no smoothing (not recommended)			",
"									",
" tmpdir= 	 if non-empty, use the value as a directory path	",
"		 prefix for storing temporary files; else if the	",
"	         the CWP_TMPDIR environment variable is set use		",
"	         its value for the path; else use tmpfile()		",
"									",
" General Parameters:							",
" dx=20		offset sampling interval (m)				",
" fx=0	  	offset on first trace (m)				",
" dt=0.004	time sampling interval (s)				",
"									",
" Tau-P Transform Parameters:						",
" gopt=1	=1 for parabolic transform. =2 for Foster/Mosher	",
"		=3 for linear. =4 for absolute value of linear		",
" pmin1=-400	minimum moveout at farthest offset for fwd transf(ms)	",
" pmax1=400	maximum moveout at farthest offset for fwd transf(ms)	",
" pmin2=pmin1	minimum moveout at farthest offset for inv transf(ms)	",
" pmax2=pmax1	maximum moveout at farthest offset for inv transf(ms)	",
" np=100	number of p-values for taup transform			",
" prewhite=0.01	prewhitening value (suggested between 0.1 and 0,01)	",
" offref=2000	reference offset for p-values (m)			",
" depthref=500	reference depth for Foster/Mosher taup (if gopt=4)	",
" pmula=pmax1	maximum p-value preserved in the data (ms)		",
" pmulb=pmax1	minimum p-value muted on the data (ms)			",
" ninterp=0	number of traces to interpolate in input data		",
"									",
" Extraction Parameters:						",
" nintlh=50	number of intervals (bins) in histograms		",
" sditer=5	number of steepest descent iterations to compute ps	",
" c=0.04	maximum noise allowed in a sample of signal(%)		",
" rel1=0.5	reliability value for first pass of the extraction	",
" rel2=0.75	reliability value for second pass of the extraction	",
"								   	",
" Smoothing Parameters:							", 
" r1=10		number of points for damped lsq vertical smoothing	",
" r2=2		number of points for damped lsq horizontal smoothing	",
"								   	",
"								   	",
" Output Files:								",
" signal=out_signal 	name of output file for extracted signal	",
" noise=out_noise 	name of output file for extracted noise		",
"									",
" 									",
" Notes:								",
" The signal-noise separation algorithm was developed by Dr. Bill Harlan",
" in 1984. It can be used to separate events that can be focused by a	",
" linear transformation (signal) from events that can't (noise). The	",
" linear transform is whatever is well siuted for the application at	",
" hand. Here, only the discrete Radon transform is used, so the program	",
" is capable of separating events focused by that transform (linear,	",
" parabolic or time-invariantly hyperbolic). Should other transform be	",
" required, the changes to the program will be relatively		",
" straightforward.							",
"									",
" The reliability parameter is the most critical one to determine what	",
" to extract as signal and what to reject as noise. It should be tested	",
" for every dataset. The way to test it is to start with a small value,	",
" say 0.1 or 0.01. If too much noise is present in the extracted noise,	",
" it is too low. If too much signal was extracted, that is, part of the	",
" signal was lost, it is too big. All other parameters have good default",
" values and should perhaps not be changed in a first encounter with the",
" program. The transform parameters are also critical. They should be	",
" chosen such that no aliasing is present and such that the range of	",
" interesting slopes is spanned by the transform but not much more. The ",
" program suradon.c has more documentation on the transform paramters.	",
"									",
NULL};

/*
 * Credits:
 * 	Gabriel Alvarez CWP (1995) 
 *	Some subroutines are direct translations to C from Fortran versions
 * 	written by Dr. Bill Harlan (1984)
 *
 * References:
 *
 * 	Harlan, S., Claerbout, J., and Roca, F. (1984), Signal/noise
 *	separation and velocity estimation, Geophysics, v. 49, no. 11,
 *	p 1869-1880. 
 *
 * 	Harlan, S. (1988), Separation of signal and noise applied to
 *	vertical seismic profiles, Geophysics, v. 53, no. 7,
 *	p 932-946. 
 *
 *	Alvarez, G. (1995), Comparison of moveout-based approaches to
 *	ground roll and multiple suppression, MSc., Department of 
 *	Geophysics, Colorado School of Mines, (Chapter 3 deals
 *	exclusively with this method).
 *
 */
/************************** end self doc *************************************/

/* Prototypes of functions used internally */
void separate_signal_noise(int verbose, int rgt, int sts, int norm, int anenv,
	int scl, int plot, int seed, int sditer, int niter, int nintlh,
	float dt, int nt, int nx, int gopt, float prewhite, int ninterp,
	float offref, float depthref, float interoff, float pmin1, float pmax1,
	float pmin2, float pmax2, int np, float dx, float c, float rel1,
	float rel2, float r1, float r2,float fx, float pmula, float pmulb,
	float **in_traces, float **signal, float **noise);

void extract_signal (int verbose, int norm, int anenv, int sts, int plot,
	int sditer, int nintlh, float c, float r, int nx, int nt, float r1,
	float r2, float **traces, float **rand_traces);

void expected_value(int nintlh, int nx, int nt, int rindex, float *pspn,
	float *pn, float *ps, float *xamps, float **traces, float *Esd);

void compute_reliability(int norm, int nx, int nt, int nintlh, int rindex,
	float c, float *ps, float *pn, float *pspn, float **traces,
	float *reliability);

void zero_noisy_samples(int anenv, int sts, int nx, int nt, float r1,
	float r2, float r, int nintlh, float *amps, float **traces,
	float **anenv_traces, float *rel);

void compute_histogram_stuff(int verbose, int nx, int nt, int nintlh,
	float **traces, float **rand_traces, int *rindex, float *amps,
	float *xamps);

void make_histogram(int nintlh, int nt, int nx, float **traces, float *amps, 
	float *pdf);

void compute_analytic_envelopes(int sgn, int nx, int nt, float **traces, 
	float **analytic_envelopes);

void scale_traces(int scl, int nx, int nt, float **in_traces, 
	float **out_traces);

void scale_one_trace(int ns, float *in_trace, float *out_trace);

void matrix_transpose(int n1, int n2, float **matrix, float **tr_matrix);

float dot_product(int ns, float *vector1, float *vector2);

void compute_max_min_sum(int nx, int nt, float *min, float *max, float *sum,
	float **data);

void plot_one_d(int npoints, float *xamps, float *data, char *plotname);

void plot_two_d(int npoints, float *data, char *plotname);

void conv1(int nx, int fx, float *x, int ny, int fy, float *y, int nz,
	int fz, float *z, int flag, float perc);

void deconvolve_histograms(int nsamples, int mean_index, int niter,
	float *pd, float *pn, float *ps);

void gradient(int ns, int si, float rmax, float *pd, float *pn,
	float *ps, float *grad);

void cross_entropy(int ns, int si, float rmax, float *pd, float *pn,
	float *ps, float *fvalue);

float divide(float rmax, float a, float b);

void golden_search(float fvalue, int *iter, float *xvalue, float *alpha);

static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/


segy tro1,tro2,tro3;

int
main(int argc, char **argv)
{
	int ix, it;		/* loop counters */
	int anenv;		/* =1 for analytic envelopes */
	int scl;		/* =1 to apply trace scaling */
	int plot;		/* flag for producing plots */
	int seed;		/* seed for random number generator */
	int verbose;		/* flag to print processing information */
	int rgt;		/* random number generator type */
	int sts;		/* vertical and horizontal smoothing */
	int sditer;		/* # of steepest descent iterations for ps */
	int nintlh;		/* number of intervals per local histogram */
	int norm;		/* option to normalize reliability indicator */
	float c;		/* maximum error allowed for reliability */
	float rel1;		/* minimum allowed reliability (first pass) */
	float rel2;		/* minimum allowed reliability (second pass) */
	int nt;			/* number of time samples */
	int niter;		/* number of iterations */
	int ntr;		/* number of traces */
	float dx;		/* horizontal sampling interval */
	float fx;		/* offset of first trace */
	float dt;		/* time sampling interval (ms) */
	float r1;		/* vertical smoothing factor for dlsq method */
	float r2;		/*horizontal smoothing factor for dlsq method*/
	int ninterp;		/* traces to interpolate for tau-p transform*/
	int gopt;		/* options for offset function g(x) in taup */
	float offref;		/* offset reference for tau-p transform */
	float pmin1;		/* min moveout in ms at ref offset for fwd tr*/
	float pmax1;		/* max moveout in ms at ref offset for fwd tr*/
	float pmin2;		/* min moveout in ms at ref offset for inv tr*/
	float pmax2;		/* max moveout in ms at ref offset for inv tr*/
	float prewhite;		/* prewhitening factor for tau-p transform */
	float depthref;		/* reference depth if gopt=2 */
	float pmula;		/* maximum slope to keep in the data */
	float pmulb;		/* minimum slope rejected from the data */
	float interoff;		/* intercept offset for tau-p times */
	int np;			/* number of slopes (traces) in Tau-p domain */
	float **traces; 	/* Array for input traces */
	float **out_signal;	/* Array of extracted signal */
	float **out_noise; 	/* Array of extracted noise */
	char *signalfile="";	/* file name for output signal */
	char *noisefile="";	/* file name for output noise */
	FILE *signal_file;	/* File pointer to output signal */
	FILE *noise_file;	/* File pointer to output noise */
	char *tmpdir;		/* directory path for tmp files	*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path */
	
	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);

	/* get info from first trace */
	if (!gettr(&tro1))  err("can't get first trace");
	nt = tro1.ns;
	dt = (float) tro1.dt/1000000.0;

/* get general flags and set their defaults */
	if (!getparint("anenv",&anenv))			anenv	= 1;
	if (!getparint("scl",&scl))			scl	= 0;
	if (!getparint("plot",&plot))			plot	= 3;
	if (!getparint("norm",&norm))			norm	= 1;
	if (!getparint("verbose",&verbose))		verbose	= 1;
	if (!getparint("rgt",&rgt))			rgt	= 2;
	if (!getparint("sts",&sts))			sts	= 1;
	if (!getparint("gopt",&gopt))			gopt	= 1;

	/* get general parameters and set their defaults */
	if (!getparfloat("dx",&dx)) 			dx	= 20.0;
	if (!getparfloat("fx",&fx)) 			fx	= 0.0;
	if (!getparint("nintlh",&nintlh))		nintlh	= 50;
	if (!getparint("sditer",&sditer))		sditer	= 5;
	if (!getparfloat("c",&c)) 			c	= 0.05;
	if (!getparfloat("rel1",&rel1)) 		rel1	= 0.5;
	if (!getparfloat("rel2",&rel2)) 		rel2	= 0.75;
	if (!getparint("niter",&niter))			niter	= 1;
	if (!getparfloat("dt",&dt))			dt	= 0.004;

	/* get smoothing parameters and set their defaults */
	if (!getparfloat("r1",&r1))			r1	= 10.;
	if (!getparfloat("r2",&r2))			r2	= 2.;

	/* get suradon taup parameters and set their defaults */
	if (!getparfloat("pmin1",&pmin1))		pmin1	= -400.0;
	if (!getparfloat("pmax1",&pmax1))		pmax1	= 400.0;
	if (!getparfloat("pmin2",&pmin2))		pmin2	= pmin1;
	if (!getparfloat("pmax2",&pmax2))		pmax2	= pmax1;
	if (!getparint("np",&np)) 			np	= 100;
	if (!getparfloat("prewhite",&prewhite))		prewhite= 0.01;
	if (!getparfloat("offref",&offref))		offref	= 2000.;
	if (!getparfloat("interoff",&interoff))		interoff= 0.;
	if (!getparfloat("depthref",&depthref))		depthref= 0.;
	if (!getparfloat("pmula",&pmula))		pmula	= pmax1;
	if (!getparfloat("pmulb",&pmulb))		pmulb	= pmax1;
		if (!getparint("ninterp",&ninterp))	ninterp	= 0;

	/* get names of output files */
	if (!getparstring("signal",&signalfile))    signalfile="out_signal";
	if (!getparstring("noise",&noisefile))	    noisefile="out_noise";

	/* get random generator seed */
	if (!getparint("seed", &seed)) { /* if not supplied, use clock */
		if (-1 == (seed = (unsigned int) time((time_t *) NULL))) {
			seed=1;
			warn("time() failed to set seed, setting it to one");
		}
	}

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

	/* Store traces in tmpfile while getting a count */
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
		fwrite(&tro1, 1, HDRBYTES, headerfp);
		fwrite(tro1.data, FSIZE, nt, tracefp);
	} while (gettr(&tro1));

	/* allocate space */
	traces = alloc2float(nt,ntr);
	out_signal = alloc2float(nt,ntr);
	out_noise = alloc2float(nt,ntr);
	
	/* load traces into an array and close temp file */
	rewind(tracefp);
	for (ix=0; ix<ntr; ix++)
		fread (traces[ix], FSIZE, nt, tracefp);
	efclose (tracefp);

	/* separate bed reflections from diffractions and noise */
	separate_signal_noise (verbose, rgt, sts, norm, anenv, scl, plot, seed,
		sditer, niter, nintlh, dt, nt, ntr, gopt, prewhite, ninterp,
		offref, depthref, interoff, pmin1, pmax1, pmin2, pmax2, np,
		dx, c, rel1, rel2, r1, r2, fx, pmula, pmulb, traces,
		out_signal, out_noise);

	/* write extracted reflections */
	if (*signalfile !='\0') {
		if ((signal_file=fopen(signalfile,"w"))==NULL)
			err("cannot open signal file=%s\n",signalfile);
		erewind(headerfp);
		{	register int itr;
			for (itr=0; itr<ntr; itr++) {
				fread(&tro1, 1, HDRBYTES, headerfp);
				for (it=0; it<nt; it++)
					tro1.data[it]=out_signal[itr][it];
				fputtr(signal_file, &tro1);
			}
			efclose (signal_file);
		}
	}

	/* write extracted diffractions */
	if (*noisefile!='\0') {
		if ((noise_file=efopen(noisefile,"w"))==NULL)
			err("cannot open noise file=%s\n",noisefile);
		erewind(headerfp);
		{	register int itr;
			for (itr=0; itr<ntr; itr++) {
				efread(&tro2, 1, HDRBYTES, headerfp);
				for (it=0; it<nt; it++)
					tro2.data[it]=out_noise[itr][it];
				fputtr(noise_file, &tro2);
			}
			efclose (noise_file);
		}
	}

	/* free workspace */
	free2float(out_signal);
	free2float(out_noise);
	free2float(traces);
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());
}

/******************************************************************************

	Subroutine to separate (extract) focused energy (signal)
	from unfocused energy (noise), after applying an invertible
			linear transformation.

******************************************************************************/
void separate_signal_noise(int verbose, int rgt, int sts, int norm, int anenv,
	int scl, int plot, int seed, int sditer, int niter, int nintlh,
	float dt, int nt, int nx, int gopt, float prewhite, int ninterp,
	float offref, float depthref, float interoff, float pmin1, float pmax1,
	float pmin2, float pmax2, int np, float dx, float c, float rel1,
	float rel2, float r1, float r2,float fx, float pmula, float pmulb,
	float **in_traces, float **signal, float **noise)
/******************************************************************************
Input parameters:
Flags:
verbose		=1 to print processing information 
		=0 not to
rgt		=1 for uniform random generator
		=2 for gaussian random generator
anenv		=0 not to use analytic envelopes
		=1 to use signed analytic envelopes for extractions
		=2 to use positive analytic envelopes for extractions
		=3 to use negative analytic envelopes for extractions
scl		=0 do not apply trace scaling
		=1 apply trace by trace scaling to extracted signal
		=2 apply scaling by time slices
		=3 apply both scalings
plot		=0 not to produce any plots
		=1 to produce plots of histograms, Esd and reliability only
		=2 to also produce 2-D plots (in t-x and taup domains) 
		=3 to produce only 2-D plots (in t-x and taup domains) 
gopt		=1 for parabolic tau-p transform: g(x)=offset**2
		=2 for Foster/Mosher pseudo-hyperbolic tau-p transform
			g(x)=sqrt(ref_depth**2+offset**2)-ref_depth
		=3 linear tau-p: g(x)=offset
		=4 abs linear tau-p: g(x)=abs(offset)
		=5 for new pseudo-hyperbolic tau-p transform
			g(x)=1/ref_vel*sqrt(ref_vel**2+offset**2)


General Parameters:
seed		seed for random number generator
niter		number of iterations for extraction process
nintlh		number of intervals (bins) for making histograms
dt		time sampling interval
nt		number of time samples
nx		number of horizontal samples (traces)
fx		offset on first trace
c		maximum allowed error for reliability computation (in percent)
rel1		minimum reliability value to accept a sample as reliable 
		(first pass)
rel2		minimum reliability value to accept a sample as reliable 
		(second pass)

Taup transform parameters:
prewhite	prewhitening factor to stabilize inverse tau-p transform
		(in percent)
ninterp		number of traces to interpolate between each pair of input 
		traces prior to taup computation
offref		reference maximum offset to which tau-p times are associated 
pmin	    minimum moveout (slope) in ms on reference offset
pmin	    maximum moveout (slope) in ms on reference offset
np		number of slopes (traces) in tau-p domain
traces	  input traces

Smoothing parameters:
r1		vertical (time) soothing factor. Usually 1<r1<=20
r2		horizontal soothing factor. Usually 1<r2<=20

Input Data:
in_traces	2-D array[nx][nt] of input traces

Output Data:
signal		2-D array[nx][nt] of extracted signal
noise		2-D array[nx][nt] of noise
*******************************************************************************
Note:
In this subroutine, the different iterations are basically the same except that
an updated noise model is used. The original data array is used in every 
iteration to compute the data histogram.
******************************************************************************/
{
	int ix,ip,it,iter;		/* loop counters */
	float dp;			/* moveout sampling interva (ms) */
	float rand=0.;			/* random number */
	float rel=0.;			/* minimum reliability for extraction */
	float **traces_taup;		/* array to store tau_p traces */
	float **temp_traces_taup;
	float **noise_taup;
	char *plotname="";

	if (plot<0||plot>3) err ("plot flag has to be 0<=plot<=3");
	
	/* number of required dips to avoid aliasing in tau-p domain */
	dp=(pmax1-pmin1)/(np-1);

	/* if requested, print processing information */
	if (verbose==1) warn("nx=%d nt=%d\n",nx,nt);

	/* allocate working space */
	temp_traces_taup=alloc2float(nt,np);
	traces_taup = alloc2float(nt,np);
	noise_taup = alloc2float(nt,np);

	/* compute forward global slant stack of input traces */
		forward_p_transform (nx, nt, dt, pmax1, pmin1, dp, depthref,
		60., 80., 4., 20., 400, 5, 1, 0, ninterp, 1, prewhite,
		interoff, offref, gopt, dx, fx, pmula, pmulb, in_traces,
		traces_taup);

	/* if requested, plot input data and its taup transform */
	if (plot==2||plot==3) { 
		plotname="input";
		plot_two_d (nx*nt, in_traces[0], plotname);
		plotname="input_taup";
		plot_two_d (nx*nt, traces_taup[0], plotname);
	}

	/* save input traces */
	for (ix=0; ix<nx; ix++) 
		for (it=0; it<nt; it++) {
			noise[ix][it]=in_traces[ix][it];
	}

	/* save taup traces */
	for (ip=0; ip<np; ip++) 
		for (it=0; it<nt; it++) {
			temp_traces_taup[ip][it]=traces_taup[ip][it];
	}

	/* do bed reflections extraction (one or two iterations) */
	for (iter=0; iter<niter; iter++) {

		/* compute seed for random generator */
		if (rgt==1) sranuni(seed);	
		else if (rgt==2) srannor(seed);

			/* compute array of noise (randomly reversed) traces */
			for (ix=0; ix<nx; ix++) {
			
			if (rgt==1) rand=2.0*franuni()-1.0;
			else if (rgt==2) rand=frannor();
			
				if (rand < 0.0) {	
					for (it=0; it<nt; it++) 
						noise[ix][it] *=-1.0;
			}
		}


		/* compute global slant stack of noise traces */
		forward_p_transform (nx, nt, dt, pmax1, pmin1, dp, depthref,
			60., 80., 4., 20., 400, 5, 1, 0, ninterp, 1, prewhite,
			interoff, offref, gopt, dx, fx, pmula, pmulb,
			noise, noise_taup);

		/* if requested, plot RRT and their tau-p transform */
		if (plot==2||plot==3) {		
			if (iter==0) plotname="rand1";
			else if (iter==1) plotname="rand2";
			plot_two_d (nx*nt, noise[0], plotname);
			if (iter==0) plotname="rtaup1";
			else if (iter==1) plotname="rtaup2";
			plot_two_d (nx*nt, noise_taup[0], plotname);
		}

		/* extract signal from diffractions and noise in taup domain */
		if (iter==0) rel=rel1;
		else if (iter==1) rel=rel2;
		extract_signal (verbose, norm, anenv, sts, plot, sditer, 
			nintlh, c, rel, np, nt, r1, r2, traces_taup,
			noise_taup);

		/* inverse slant stack to get extracted reflections in t-x dom*/
		inverse_p_transform (nx, nt, dt, pmax2, pmin2, dp, depthref,
			60., 80., interoff, offref, gopt, dx, fx, traces_taup,
			signal);

		/* if requested, output plot of extracted signal ad its taup*/
		if (plot==2||plot==3) { 
			if (iter==0) plotname="signal1";
			else if (iter==1) plotname="signal2";
			plot_two_d (nx*nt, signal[0], plotname);
			if (iter==0) plotname="staup1";
			else if (iter==1) plotname="staup2";
			plot_two_d (nx*nt, traces_taup[0], plotname);
		}

		/* if requested, scale filtered traces with respect to input */
		if (scl !=0) {
			scale_traces (scl, nx, nt, in_traces, signal); 

			/* if requested, output plot of scaled signal */
			if (plot==2) {
				if (iter==0) plotname="s1scaled";
				else if (iter==1) plotname="s2scaled";
				plot_two_d (nx*nt, signal[0], plotname);
			}
		}
	
		/* subtract signal from data to estimate noise */
			for (ix=0; ix<nx; ix++) 
					for (it=0; it<nt; it++) {
				if (scl !=-1) {
					noise[ix][it]=in_traces[ix][it]-
							signal[ix][it];
				}
			}

		/* recover the taup-transformed data */
			for (ip=0; ip<np; ip++) 
					for (it=0; it<nt; it++) {
				traces_taup[ip][it]=temp_traces_taup[ip][it];
		}

		/* if requested, output plot of extracted noise */
		if (plot==2||plot==3) {  
			if (iter==0) plotname="noise1";
			else if (iter==1) plotname="noise2";
			plot_two_d (nx*nt, noise[0], plotname);
		}
	}

	/* clean up */
	free2float(temp_traces_taup);
	free2float(traces_taup);
	free2float(noise_taup);
}

/******************************************************************************

	Subroutine to extract linear reflections from behind diffractions
				and noise

******************************************************************************/
void extract_signal (int verbose, int norm, int anenv, int sts, int plot,
	int sditer, int nintlh, float c, float r, int nx, int nt, float r1,
	float r2, float **traces, float **rand_traces)
/******************************************************************************
Input:

Flags:
anenv		=0 not to use analytic envelopes
		=1 to use signed analytic envelopes
		=2 to use positive analytic envelopes	
		=3 to use negative analytic envelopes
sts		=0 not to smooth estimaded signal amplitudes
		=1 do damped least squares temporal and spatial smoothing
plot		=0 don't make any plots
		=1 make plots of histograms, reliability and Esd

General Parameters:
sditer		number of steepest descent iterations for ps computation
nintlh		number of intervals (bins) per histogram
nx		number of horizontal samples (traces)
nt		number of vertical samples

Smoothing Parameters:
r1		vertical smoothing factor (if (sts=1). 1<=r1<=20
r2		horizontal smoothing factor (if (sts=1). 1<=r2<=20

Data:
traces		2-D array[nx][nt] of tau-p transformed input traces
rand_traces	2-D array[nx][nt] of randomly reversed tau-p transformed traces

Output:
traces		2-D array[nx][nt] of tau-p transformed extracted signal traces
rand_traces	2-D array[nx][nt] of tau-p transformed extracted noise traces
******************************************************************************/
{
	int ix;			/* loop counters */
	int rindex=0;		/* reference index for histograms */
	float spd,sps,spn;	/* auxiliary variables to test unit area */
	float spspn;		/* auxilliary variable			*/
	float *pd;	   	/* array of data probability values */
	float *ps;		/* array of signal probability values */
	float *pn;		/* array of noise probability values */
	float **anenv_traces;	/* 2-D array of analytic envelopes */
	float *reliability;	/* 2-D array of computed reliability indicator*/
	float *Esd;		/* 2-D array of expected values */
	float *xamps;		/* 1-d array of reference amplitudes */
	float *amps;		/* 1-d array of amplitudes in histograms */
	float *pspn;		/* 1-d array of ps*pn */
	char *plotname="";	/* pointer to name of output plot files */
	static int count;

	/* if requested, print processing information */
	if (verbose==1) {
	    warn("PARAMETERS: c=%g rel=%g r1=%g r2=%g\n",c,r,r1,r2); 
	    warn("FLAGS: anenv=%d plot=%d sts=%d\n",anenv,plot,sts);
	}

	/* allocate space */
	pd = alloc1float(nintlh);
	ps = alloc1float(nintlh);
	pn = alloc1float(nintlh);
	amps=alloc1float(nintlh);
	xamps=alloc1float(nintlh);
	pspn=alloc1float(nintlh);
	Esd = alloc1float(nintlh);
	reliability = alloc1float(nintlh);
	anenv_traces=alloc2float(nt,nx);

	/* if requested, compute analytic envelopes */
	if (anenv !=0) 
		compute_analytic_envelopes (anenv,nx,nt,traces,anenv_traces); 

	/* compute histogram parameters */
	compute_histogram_stuff (verbose, nx, nt, nintlh, traces, rand_traces,
		&rindex, amps, xamps);

	/* compute pd(x) and pn(x) via local histograms */
	make_histogram (nintlh, nt, nx, traces, amps, pd);
	make_histogram (nintlh, nt, nx, rand_traces, amps, pn);

	/* smooth noise histogram */
	smooth_histogram (nintlh, pn);

	/* compute ps(x) via minimization of cross-entropy */
	deconvolve_histograms (nintlh, rindex, sditer, pd, pn, ps);

	/* compute ps(x) convolved with pn(x) */
	convolve_cwp (nintlh, -rindex, ps, nintlh, -rindex, pn, nintlh, -rindex, pspn);

	/* coumpute array of expected values */
		expected_value (nintlh, nx, nt, rindex, pspn, pn, ps, xamps, traces,
		Esd);

	/* compute reliability as a bounded convolution */
	compute_reliability (norm, nx, nt, nintlh, rindex, c, ps, pn, pspn,
		traces, reliability);

	/* zero out noisy samples depending on their reliability */
	zero_noisy_samples (anenv, sts, nx, nt, r1, r2, r, nintlh, amps,
		traces, anenv_traces, reliability);

	/* check constraint of unit area for histograms */
	if (verbose==1) {
		spd=spn=sps=spspn=0.0;
		for (ix=0;ix<nintlh;ix++) {
			spd +=pd[ix];
			spn +=pn[ix];
			sps +=ps[ix];
			spspn +=pspn[ix];
		}
	
		if (verbose) {
			warn("Area under the curve for pd=%g\n",spd);
			warn("Area under the curve for pn=%g\n",spn);
			warn("Area under the curve for ps=%g\n",sps);
			warn("Area under the curve for ps*pn=%g\n",spspn);
		}
	}	

	/* if requested, output plots of histograms, Esd and reliabilities */
	count++;
	if (plot==1||plot==3) {
		if (count==1) plotname="pd1";
		else if (count==2) plotname="pd2";
		plot_one_d (nintlh, xamps, pd, plotname);
		if (count==1) plotname="pn1";
		else if (count==2) plotname="pn2";
		plot_one_d (nintlh, xamps, pn, plotname);
		if (count==1) plotname="ps1";
		else if (count==2) plotname="ps2";
		plot_one_d (nintlh, xamps, ps, plotname);
		if (count==1) plotname="pspn1";
		else if (count==2) plotname="pspn2";
		plot_one_d (nintlh, xamps, pspn, plotname);
		if (count==1) plotname="Esd1";
		else if (count==2) plotname="Esd2";
		plot_one_d (nintlh, xamps, Esd, plotname);
		if (count==1) plotname="rel1";
		else if (count==2) plotname="rel2";
		plot_one_d (nintlh, xamps, reliability, plotname);
	}

	/* free allocated space */
	free2float(anenv_traces);
	free1float(pd);
	free1float(ps);
	free1float(pn);
	free1float(pspn);
	free1float(amps);
	free1float(xamps);
	free1float(Esd);
	free1float(reliability);
}

/******************************************************************************

	Subroutine to compute an array of expected values E(s|d)

******************************************************************************/
void expected_value (int nintlh, int nx, int nt, int rindex, float *pspn,
	float *pn, float *ps, float *xamps, float **traces, float *Esd)
/******************************************************************************
Input:
anenv		=0 do not use analytic envelopes
		!=0 use analytic envelopes
clipf		=1 use the maximum amplitude as the clip value for division
		=2 use the sample amplitude as the clip value for division
nintlh	  number of intervals (bins) per histogram
nx		number of horizontal samples (traces) (ignored)
nt		number of vertical (time) samples (ignored)
rindex		histogram index of mean data sample
pspn		1-D array[nintlh] of ps convolved with pn
ps		1-D array[nintlh] of signal probability density function
pn		1-D array[nintlh] of noise probability density function
pd		1-D array[nintlh] of data probability density function
traces    	2-D array[nx][nt] of input traces
anenv_traces    2-D array[nx][nt] of analytic envelopes traces

Output Parameters:
Esd		1-D array[nintlh] of E(s|d) values for reference amplitudes
*******************************************************************************
expected_value uses Harlan's description of the process. It only computes
the Esd indicator for the reference amplitudes, (bin centers) and applies
linear interpolation for all other amplitudes.
******************************************************************************/
{
	int ix;			/* loop counters */
	float *xps;		/* array of x*ps(x) */
	float max;		/* maxumum amplitude in input data */

	ix = 0*nt*nx; traces += ix;
	max=xamps[nintlh-1]+(xamps[1]-xamps[0]);

	/* allocate working space */
	xps = alloc1float(nintlh);

	/* compute the product x*ps(x) */
	for (ix=0; ix<nintlh; ix++) xps[ix] = xamps[ix]*ps[ix];

	/* compute the convolution of xps(x) with pn(x) */
	convolve_cwp (nintlh, -rindex, xps, nintlh, -rindex, pn, nintlh, -rindex, Esd);

	/* compute Esd for reference amplitudes (histogram bin centers) */
	for (ix=0; ix<nintlh; ix++) {
	
		/* do the division */
		Esd[ix]=divide(max,Esd[ix],pspn[ix]);
	}

	/* free allocated space */
	free1float(xps);
}

/******************************************************************************

	Compute reliability via bounded convolution

******************************************************************************/
void compute_reliability (int norm, int nx, int nt, int nintlh, int rindex,
	float c, float *ps, float *pn, float *pspn, float **traces,
	float *reliability)
/******************************************************************************
Input:
anenv		=0 not to use analytic envelopes
		!=0 use analytic envelopes
nx		number of hotizontal samples (traces) (ignored)
nt		number of vertical samples (samples per trace)
nintlh		number of bins per histogram
rindex		index of reference amplitude for histograms
c		maximum allowed error in percent
ps		1-D array[nintlh] of signal probability density histogram
pn		1-D array[nintlh] of noise probability density histogram
pspn		1-D array[nintlh] of ps convolved with pn
traces		2-D array[nx][nt] of input traces (ignored)
anenv_races	2-D array[nx][nt] of input analytic envelopes traces


Output:
reliability	1-D array[nintlh] of reliability values for bin centers
*******************************************************************************
Note:
This subroutine computes the reliability as a bounded convolution without 
explicitly computing the expected value of the signal given the data (Esd
indicator), assuming that for high amplitude samples the signal and the data
amplitudes are about the same, so in the limits of integration for the 
reliability indicator the s (signal) is implicitly replaced by d (data).
The reliability of a given sample is assumed to be that of the histogram bin 
to which it belongs)
******************************************************************************/
{
	int i;				/* loop counters */
	float max=-999999999;
	float *numerator;		/* numnerator of reliability */

	i = nt*nx*0;
	traces += i;
	/* allocate working space */
	numerator=alloc1float(nintlh);

	/* compute numerator of reliability as a bounded convolution */
		conv1 (nintlh, -rindex, ps, nintlh, -rindex, pn, nintlh, -rindex,			numerator, -1, c);

	/* compute reliability for bin centers */
	for (i=0; i<nintlh; i++) 
	  	reliability[i]=divide (1.0, numerator[i], pspn[i]);
	
	/* if requested normalize reliability */
	if (norm==1) {	
		for (i=0; i<nintlh; i++) {
			if (max<reliability[i]) max=reliability[i];
		}

		for (i=0; i<nintlh; i++) {
			reliability[i] /=max;
		}
	}

	/* free allocated space */
	free1float(numerator);
}

/******************************************************************************

	Subroutine to zero out noisy samples based on their Esd value

******************************************************************************/
void zero_noisy_samples (int anenv, int sts, int nx, int nt, float r1,
	float r2, float r, int nintlh, float *amps, float **traces, 
	float **anenv_traces, float *rel)
/******************************************************************************
Input parameters:
anenv		=1 use analytic envelopes
		=0 don't use them
sts		flag for smoothing
nx		number of horizontal samples (traces)
nt		number of vertical samples 
r		samples with reliability >r are kept 
nintlh		number of intervals in histograms	
Esd		1-D array[nintlh] of expected values for reference amplitudes
traces		2-D array[nx][nt] of input traces
anenv_traces	2-D array[nx][nt] of analytic envelope traces
rel	 	1-D array[nintlh] of reliability values for reference amplitudes

traces		2-D array[nx][nt] of extracted (focused) signal
******************************************************************************/
{
	int it,ix;		/* loop counters */
	int index=0;		/* sample index */
	float d;		/* sample amplitude */	
	float **oz;		/* array of ones and zeros */

	/* allocate working space */
	oz=alloc2float(nt,nx);

	/* create array of ones and zeros depending on reliability value */
	for (ix=0; ix<nx; ix++)
		for (it=0; it<nt; it++) {

			/* get sample amplitude */
			if (anenv==0) d=traces[ix][it];	 
			else d=anenv_traces[ix][it];

			/* find to which bin sample d belongs */
			xindex (nintlh, amps, d, &index);
			
			/* flag sample with zero or one */
			if (rel[index]<r) oz[ix][it]=0.0;
			else oz[ix][it]=1.0;
		}

	/* if requested, smooth array of zeros and ones horizontally and vert*/
	if (sts !=0) {
		dlsq_smoothing (nt, nx, 0, nt, 0, nx, r1, r2, 0, oz);
	}

	/* multiply data by smoothed array of ones and zeros */
	for (ix=0; ix<nx; ix++)
		for (it=0; it<nt; it++) 
			traces[ix][it] *=oz[ix][it];

	/* free allocated space */
	free2float(oz);
}

/******************************************************************************

	Subroutine to compute minimum, maximum, interval width and 
			amplitudes for histograms

******************************************************************************/
void compute_histogram_stuff (int verbose, int nx, int nt, int nintlh,
	float **traces, float **rand_traces, int *rindex, float *amps,
	float *xamps)
/******************************************************************************
Input:
nx		number of traces in input array
nt		number of samples per trace
nintlh		number of intervals (bins) in a histogram
rindex		pointer to reference index
traces		2-D array[nx][nt] of input traces
rand_traces	2-D array[nx][nt] of randomly reversed traces

Output:
amps		1-D array[nintlh] of left end bin amplitudes in histograms
xamps		1-D array[nintlh] of bin centers in histograms
		computed min, max and del_int values 
******************************************************************************/
{
	int ix;			/* loop counter */
	float del_int;		/* histogram bin width */
	float min1,max1;	/* minimum and maximum amplitudes of data */
	float min2,max2;	/* minimum and maximum amplitudes of noise */
	float fint;		/* auxiliary variable */
	float sum1=0.0;
	float sum2=0.0;
	float min;
	float max;

	/* compute min, max and sum of input array */
	compute_max_min_sum (nx, nt, &min1, &max1, &sum1, traces);  

	/* compute min, max and sum of randomly reversed array */
	compute_max_min_sum (nx, nt, &min2, &max2, &sum2, rand_traces);
	
	/* compute minimum and maximummbin amplitudes and bin interval */
	min = min1-0.5*(max2-min2);
	max = max1+0.5*(max2-min2);
	del_int = (max-min)/(nintlh-1);
	fint = min;
	min -= del_int/2.0;
	max += del_int/2.0;
		
	/* compute array of amplitudes for histograms */
	for (ix=0; ix<nintlh; ix++) {
		amps[ix]=min+del_int*ix;
		xamps[ix]=fint+del_int*ix;
	}

	/* find interval for the mean value of data histogram */
	xindex (nintlh, amps, sum1/(nt*nx), rindex);

	/* if requested, print processin information */
	if (verbose==1) {
		warn("for histograms: min=%g max=%g",min,max);
		warn("first bin=%g binwidth=%g\n",fint,del_int);
	}
}


/******************************************************************************

	Subroutine to compute one-dimensional histograms

******************************************************************************/
void make_histogram (int nintlh, int nt, int nx, float **traces, float *amps, 
	float *pdf)
/******************************************************************************
Input:
nintlh	  number of intervals per local histogram
nt		number of time samples
nx		number of horizontal samples
amps		1-D array[nintlh] of histogram amplitudes
traces		2-D array[nx][nt] of input traces

Output:
pdf		1-D array[nintlh] of computed  probabilty density function
******************************************************************************/
{
	int i,it,ix;   			/* loop counters */
	int index=0;			/* search index */
	int ns=nt*nx;			/* total number of samples */

	/* initialize pdf array */
	for (i=0; i<nintlh; i++) pdf[i]=0.0;

	/* compute histogram of data samples */
	for (ix=0; ix<nx; ix++) 
		for (it=0; it<nt; it++) {

			/* find interval to which sample belongs */
			xindex (nintlh, amps, traces[ix][it], &index);

			/* update interval frequencies */
			pdf[index] +=1.0;
		}

	/* normalize frequencies to get probability density distribution */
	for (i=0; i<nintlh; i++) pdf[i] /=ns;
}

/******************************************************************************

	Subroutine to compute the analytic envelopeis of an input 2-D
				array of traces

******************************************************************************/
void compute_analytic_envelopes (int sgn, int nx, int nt, float **traces, 
	float **analytic_envelopes)
/******************************************************************************
Input:
sgn			=1 compute signed analytic envelopes
			=2 compute positive analytic envelopes
			=3 compute negative analytic envelopes
			=4 compute signed square of analytic envelope
			=5 compute positive square analytic envelopes
			=6 compute negative analytic envelopes
nx			number of traces
nt			number of samples per trace
traces			2-D array[nx][nt] of input traces

Output:
analytic_envelopes	2-D array[nx][nt] of computed analytic envelops 	
******************************************************************************/
{
	int ix, it;			/* loop counters */
	float amp1,amp2,amp3;		/* auxiliary variables */
	float *hilbert_trace;		/* hilbert transform for 1 trace */

	/* allocate working space */
	hilbert_trace=alloc1float(nt);

	for (ix=0; ix<nx; ix++) {

		/* compute hilbert transform of current trace */
		hilbert (nt, traces[ix], hilbert_trace);

		for (it=0; it<nt; it++) {
			amp1=hilbert_trace[it];
			amp2=traces[ix][it];

			/* compute squared analytic envelopes */
			amp3=amp1*amp1+amp2*amp2;

			/* take care of square root */
			if (sgn==1||sgn==2||sgn==3) amp3=sqrt(amp3);

			/* take care of sign */
			if (sgn==3||sgn==6) analytic_envelopes[ix][it] =-amp3;
			else if (sgn==1||sgn==4) {
				if(amp2<0.0) analytic_envelopes[ix][it] =-amp3;
				else analytic_envelopes[ix][it] = amp3;
			} else {
				analytic_envelopes[ix][it]=amp3;
			}
		}
	}

	/* free allocated space */
	free1float(hilbert_trace);
}


/******************************************************************************

	Subroutine to scale output traces to the same amplitude level
			of the input traces

******************************************************************************/
void scale_traces (int scl, int nx, int nt, float **in_traces, 
	float **out_traces)
/******************************************************************************
Input parameters:
scl		=1 for trace scaling only
		=2 for time sice scaling only
		=3 for both 
nx		number of traces
nt		number of samples per trace
in_traces	reference set of traces to scale out_traces
out_traces	set of traces to be scaled according to amplitudes of
		in_traces

Output parameters:
out_traces	set of scaled traces
******************************************************************************/
{
	int ix,it;

	/* if requested, apply trace by trace scaling */
	if (scl==1||scl==3) {

		/* loop over traces */
		for (ix=0; ix<nx; ix++) { 
			scale_one_trace (nt,in_traces[ix],out_traces[ix]);
		}
	}  

	/* if requested, apply scaling by time sices */
	if (scl==2||scl==3) {
		float **temp_in;		/* transpose of in_traces */
		float **temp_out;		/* transpose of out_traces */

		/* allocate working space */
		temp_in=alloc2float(nx,nt);
		temp_out=alloc2float(nx,nt);

		/* transpose input and output arrays */
		matrix_transpose (nx, nt, in_traces, temp_in);
		matrix_transpose (nx, nt, out_traces, temp_out);

		/* loop over time slices */
		for (it=0; it<nt; it++) {
			scale_one_trace (nx, temp_in[it], temp_out[it]);
		}

		/* transpose output back */
		matrix_transpose (nt, nx, temp_out, out_traces);

		/* free allocated space */
		free2float(temp_in);
		free2float(temp_out);
	}
}

/******************************************************************************

	Subroutine to scale one trace to the same amplitude level of another

******************************************************************************/
void scale_one_trace (int ns, float *in_trace, float *out_trace)
/******************************************************************************
Input parameters:
ns		number of samples in trace to scale 
in_trace	reference trace to scale out_trace
out_trace 	trace to be scaled according to amplitudes of in_trace

Output parameters:
out_trace	scaled trace
******************************************************************************/	
{
	int is;
	float num,den;
	float scale;

	/* compute dot product of reference traces */
	num = dot_product (ns, in_trace, in_trace);

	/* compute dot product of output traces */	
	den = dot_product (ns, out_trace, out_trace);

	/* compute scale factor */
	if (den==0.0) {
		return;
	} else {
		scale = sqrt(num/den);	

		/* apply scale factor to each sample */
		for (is=0; is<ns; is++) {
			out_trace[is] *=scale;
		}
	}	
}

/******************************************************************************

		Subroutine to transpose a matrix

******************************************************************************/
void matrix_transpose (int n1, int n2, float **matrix, float **tr_matrix)
/******************************************************************************
Input:
n1		number of number of columns in input matrix
n2		number of rows in input matrix
matrix		2-D array[n1][n2] to be transposed

Output:
tr_matrix	2-D array[n2][n1] of transposed matrix
******************************************************************************/
{
	int i1,i2;			/* loop counters */

	for (i1=0;i1<n1;i1++)
		for (i2=0;i2<n2;i2++)
			tr_matrix[i2][i1]=matrix[i1][i2];
}

/******************************************************************************
	
		compute the dot product of two time series

******************************************************************************/
float dot_product (int ns, float *vector1, float *vector2)
/******************************************************************************
Input:
ns		number of samples in vectors
vector1		array[ns] of first vector
vector2		array[ns] of second vector

Output:
		dot product of vector1 and vector2
******************************************************************************/
{
	int is;
	float sum=0.0;

	/* compute the dot product */
	for (is=0; is<ns; is++) sum +=vector1[is]*vector2[is]; 

	/* output result */
	return(sum);
}

/******************************************************************************

	Subroutine to compute the maximum, minimum and sum of the
			samples in a 2-D array

******************************************************************************/
void compute_max_min_sum (int nx, int nt, float *min, float *max, float *sum,
	float **data)
/******************************************************************************
Input:
nx		number of horizontal samples (traces)
nt		number of vertical samples (samples per trace)
min		pointer to output minimum value
max		pointer to output maximum value
sum		pointer to output sum
data		2-D array[nx][nt] of traces

Output:
		max, min and sum values
******************************************************************************/
#define MAXVAL 9999999
{
	int ix,it;		/* loop counters */
	float d;		/* auxiliary variable for sample amplitude */

	/* initialize output variables */
	*min=MAXVAL;
	*max=-MAXVAL;
	*sum=0.0;
	
	for (ix=0; ix<nx; ix++)
		for (it=0; it<nt; it++) {

			/* get sample amplitude */
			d=data[ix][it];
	
			/* compute max, min and sum */
			if (d<*min) *min=d;
			if (d>*max) *max=d;
			*sum +=d;
		}
}

/******************************************************************************

		Output data for a one dimensional plot

******************************************************************************/
void plot_one_d (int npoints, float *xamps, float *data, char *plotname)
/******************************************************************************
Input Parameters:
npoints		number of opints to plot
xamps		1-D array [npoints] of abscissa values
data		1-D array [npoints] of ordinate values
plotname	character array of plot name

Output:
		Binary file called plotname
******************************************************************************/
{
	FILE *out;		/* file pointer to output data */

	out=fopen(plotname,"w");
	fwrite (xamps, sizeof(float), npoints, out);
	fclose(out);
	out=fopen(plotname,"a");
	fwrite (data, sizeof(float), npoints, out);
	fclose(out);
}

/******************************************************************************

		Subroutine to output a two-dimensional plot

******************************************************************************/
void plot_two_d (int npoints, float *data, char *plotname)
/******************************************************************************
Input Parameters:
npoints		number of points in output plot
data		1-D array [npoints] of data to plot
plotname	character array of plotname

Output:
		file called plotname ready to plot 
******************************************************************************/
{
	FILE *out;

	out=fopen(plotname,"w"); 
	fwrite (data, sizeof(float), npoints, out);
	fclose(out);
}

/******************************************************************************

	Subroutine to compute convolution, correlation or bounded
		convolution of two input 1-D arrays

******************************************************************************/
void conv1 (int nx, int fx, float *x, int ny, int fy, float *y, int nz,
	int fz, float *z, int flag, float perc)
/******************************************************************************
Input:
nx		number of samples in first input array
fx		index of first sample of first input array
x[nx]		first input array
ny		number of samples in second input array
fy		index of first sample of second input array
y[ny]		second input array
nz		number of samples in output array
fz		index of first sample in output array
flag		=0 for convolution, =1 for correlation and =-1 for bounded
		convolution
perc		Desired percentage for bounded convolution.
		=1 for regular convolution and correlation

Output:
z[nz]		convolution or correlation of arrays x and y

Note: z cannot be equal to x or y
*******************************************************************************
This subroutine is a direct translation to C of a Fortran version written
by Dr. Bill Harlan, 1984.
******************************************************************************/
{
	int ix,iy,iz;
	int jx,jy,jz;
	float rl,rr,r,s;

	/* initialize output array */
	for (iz=0; iz<nz; iz++) z[iz]=0.0;

	/* compute convolution or correlation */	
	for (iz=1; iz<=nz; iz++) {
		jz=iz+fz-1;

		for (ix=1; ix<=nx; ix++) {
			
			jx=ix+fx-1;
			jy=jz-jx;

			/* correlation, change the sign */
			if (flag==1) jy=jz+jx;
		
			iy=jy-fy+1;

			/* exit inner loop if out of bounds */
			if (iy<1||iy>ny) continue;
	
			s=1;	/* for normal convolution or correlation */
	
			/* if bounded convolution is desired */
			if (flag==-1) {
				r=jz;
				r=ABS(r)*perc;
				rl=jy-0.5;
				rr=jy+0.5;
				if (rl>r) rl=r;
				if (rl<-r) rl=-r;
				if (rr>r) rr=r;
				if (rr<-r) rr=-r;
				s=rr-rl;
			}

			/* update sum */
			z[iz-1] += s*x[ix-1]*y[iy-1];
		}
	}
}
	
/******************************************************************************

	Subroutine to compute ps(x) as a deconvolution of pd(x)
		and pn(x) with constraints of positivity and
				unit area

******************************************************************************/
void deconvolve_histograms (int nsamples, int mean_index, int niter,
	float *pd, float *pn, float *ps)
/******************************************************************************
Input:
nsamples	Number of samples in histograms
mean index	Index of the mean value sample in data histogram 
niter		Number of iterations for golden search (usually 10 will suffice)
pd		Array [nsamples] of data probability density function
pn		Array [nsamples] of noise probability density function

Output:
ps		Array [nsamples] of signal probability density function
*******************************************************************************
Note:
The deconvolved signal probability density funcion is computed by an
optimization algorithm in which the "best" ps(x) is computed to maximize the
probability (the fit) of the data histogram via minization of a suitable
cross-entropy indicator. The minization is performed for ps(x) with 
constraints of positivity adn unit area.
*******************************************************************************
Credits:
Translated to C by Gabriel Alvarez (1995) from a FORTRAN IV version
	written by Dr. Bill Harlan (1982)   
******************************************************************************/
{
	int j;			/* loop counter */
	int ndiv=20;		/* number of iteration for golden search */
	int iter;		/* index of iterations */
	int idiv;		/* index of golden search */
	float r1,r2;		/* proportions for ps updating */
	float rmax=1000.;	/* clip value for division of histograms */
	float alpha=0.5;	/* optimum golden search value */
	float xvalue=FLT_MIN;	/* optimum golden search x-value */
	float fvalue=0.0;       /* value of cross-entropy estimator */
	float scale=1.0;	/* some sort of scaling factor */
	float xgold;		/* x-value obtained from the golden search */
	float agold = 0;	/* alpha obtained from the golden search */
	float sum;
	float *psp=NULL;	/* array of trial perturbed ps */
	float *grad=NULL;	/* array of computed gradient */

	/* allocate working space */
	psp = alloc1float(nsamples);
	grad = alloc1float(nsamples);

	/* initialize ps array to a spike at the mean amplitude of histograms */
	for (j=0; j<nsamples; j++) ps[j]=0.0;
	ps[mean_index]=1.0;

	/* main loop */
	for (iter=0; iter<niter; iter++) {
		
		/* scale down alphas if last search was too large */ 
		/* xvalue should have the smallest non-zero value */
		/* previously used */
		scale = 2*alpha;
		if (alpha==0.0) scale = 2*xvalue;
		if (scale>1.0) scale = 1.0;

		/* compute normalized gradient of the cross-entropy estimator */
		gradient (nsamples, mean_index, rmax, pd, pn, ps, grad);

		/* do golden search to come up with optimum alpha value */
		idiv=1;
		while (idiv > 0 /*TRUE*/) {
			golden_search (fvalue, &idiv, &xgold, &agold);

			/* update xvalue and alpha */
			xvalue = xgold*scale;
			alpha = agold*scale;

			/* test to check loop exiting condition */
			if (idiv>=ndiv) break;

			/* compute trial ratios */
			r1 = 1.0 - xvalue;
			r2 = xvalue;

			/* update trial ps using trial ratios r1 and r2 */
			sum=0.0;
			for (j=0; j<nsamples; j++) {
				psp[j]=r1*ps[j]+r2*grad[j];
				sum +=psp[j];
			}

			/* normalize updated ps */
			for (j=0; j<nsamples; j++) psp[j] /=ABS(sum);

			/* compute cross-entropy estimator for updated ps */
			cross_entropy (nsamples, mean_index, rmax, pd, pn, 
				psp, &fvalue);
		}

		/* compute optimum ratios from optimum alpha */
		r1 = 1.0 - alpha;
		r2 = alpha;

		/* update ps using optimum ratios r1 and r2 */
		sum=0.0;
		for (j=0; j<nsamples; j++) {
			ps[j]=r1*ps[j]+r2*grad[j];
			sum +=ps[j];
		}

		/* normalize updated ps */
		for (j=0; j<nsamples; j++) ps[j] /=ABS(sum);
	}

	/* free allocated space */
	free1float(grad);
	free1float(psp);
}

/******************************************************************************

	Subroutine to compute the gradient of the cross-entropy estimator 
	  for constrained deconvolution of probability density
				functions

******************************************************************************/
void gradient (int ns, int si, float rmax, float *pd, float *pn,
	float *ps, float *grad)
/******************************************************************************
Input parameters:
ns		number of samples (intervals) in histograms
si		sample index of mean value in histograms
rmax		clip value for division of histograms
pd		data probability density function
pn		noise probability density function
ps		estimate of signal probability density function

Output value
grad		array of gradient of cross-entropy estimator
*******************************************************************************
Credits:
Translated to C by Gabriel Alvarez (1995) from a Fortran IV  version 
	written by Dr. Bill Harlan (1983)
******************************************************************************/
{
	int j;			/* loop variable */
	float sum;		/* variable to hold sum */
	float *pspn;		/* array for convolution of ps and pn */
	
	/* allocate space */
	pspn = alloc1float(ns);

	/* do convolution of pn with estimate of ps */
	convolve_cwp (ns, -si, pn, ns, -si, ps, ns, -si, pspn); 

	/* divide pd and pspn term by term */
	for (j=0; j<ns; j++)
		pspn[j] = divide (rmax, pd[j], pspn[j]);

	/* cross-correlate pn with  pd/(ps*pn) */
	xcor (ns, -si, pn, ns, -si, pspn, ns, -si, grad); 

	/* normalize gradient */
	sum=0.0;
	for (j=0; j<ns; j++) sum +=grad[j];
	for (j=0; j<ns; j++) grad[j] /=ABS(sum);

	/* free allocated space */
	free1float(pspn);
}

/******************************************************************************

	Subroutine to compute cross-entropy estimator 

******************************************************************************/
void cross_entropy (int ns, int si, float rmax, float *pd, float *pn,
	float *ps, float *fvalue)
/******************************************************************************
Input parameters:
ns		number of samples (intervals) in histograms
si		sample index of mean value in histograms
rmax		clip value for division of histograms
pd		data probability density function
pn		noise probability densityu function
ps		estimate of signal probability function

Output value
fvalue		pointer to computed cross-entropy value
*******************************************************************************
Credits:
Translated to C by Gabriel Alvarez (1995) from a Fortran IV  version 
	written by Dr. Bill Harlan (1983)
******************************************************************************/
{
	int j;			/* loop counter */
	float sum=0.0;		/* to store the sum */
	float *pspn;		/* variable for convolution of ps and pn */

	/* allocate working space */
	pspn = alloc1float(ns);

	/* convolve ps and pn */
	convolve_cwp (ns, -si, ps, ns, -si, pn, ns, -si, pspn);

	/* compute (ps*pn)/pd */
	for (j=0; j<ns; j++)
		pspn[j]=divide (rmax, pspn[j], pd[j]);
	
	/* compute the sum */
	for (j=0; j<ns; j++) {
		
		/* compute pd/(ps*pn) as the inverse of (ps*pn)/pd */
		pspn[j]=divide (rmax, 1.0, pspn[j]);

		/* multiply pd by log(pd/(ps*pn)) */
		sum +=pd[j]*log(pspn[j]);
	}

	/* output the result */
	*fvalue=sum;
} 

/******************************************************************************

	Subroutine to compute a=b/c  with a given clip value

******************************************************************************/
float divide(float rmax, float a, float b) 
/******************************************************************************
Input:
rmax		clip value
a		numerator
b		denominator

Output:	
		quotient=a/b
*******************************************************************************
Credits:
Translated to C by Gabriel Alvarez (1995) from a Fortran IV  version 
	written by Dr. Bill Harlan (1983)
******************************************************************************/
#define BIG 1000000000.0
{
	float quotient;			/* variable to hold the quotient a/b */

	/* check to see if both a and b are larger than BIG */ 	
	if (ABS(a)>BIG && ABS(b)>BIG) {
		a /=BIG;
		b /=BIG;
	} 
	
	/* check again */
	if (ABS(a)>BIG && ABS(b)>BIG) {
		a /=BIG;
		b /=BIG;
	} 

	rmax = ABS(rmax);		
	if (ABS(a)<(rmax*ABS(b))) {
		quotient  = a/b;
	
		/* this test will be true only if a and b are very small */
		if (ABS(quotient)>rmax) quotient=0.0;

	} else {
		quotient = rmax;

		/* check to see if quotient should be negative */
		if ((a>0.0 && b<0.0)||(a<0.0 && b>0.0)) quotient *= -1.0;
	}

	/* if numerator is zero, so is the quotient */	
	if (a==0.0) quotient=0.0;

	/* output quotient */
	return(quotient);
}

/******************************************************************************

	Subroutine to perform a one dimensional golden search
		for a functional value in the interval [0,1] 
	
******************************************************************************/
void golden_search (float fvalue, int *iter, float *xvalue, float *alpha)
/******************************************************************************
Input:
iter		index of iteration
fvalue		value of the function to find the minimum

Output:
xvalue		value of x that minimizes de function
alpha		optimum value
*******************************************************************************
Note:
The golden search algorithm used here is not optimum in the sense that it
does not make use of the ability of C to pass pointers to functions in a
subroutine. It was devised this way to cope with Fortran's lack of this
feature. A parabolic interpolation could perhaps be a better option.
*******************************************************************************
Credits:
Translated to C by Gabriel Alvarez (1995) from a Fortran IV  version 
	written by Dr. Bill Harlan (1983)
******************************************************************************/
{
	int j;				/* loop counter */
	static int ifill=0;		/* auxiliary function index */
	int im;				/* auxiliary index */
	float fm;			/* auxiliary variable */
	static float rmag=1.0;		/* golden ratio */
	static float x[4];		/* array to store searching points */
	static float f[4];		/* array to store function values */

	/* test condition for iteration index */
	if (*iter<1) *iter=1;			/* this should never happen */
	if (*iter==1) {
	
		/* define golden ratio */	
		*alpha = 0.0;
		rmag = 0.5*(sqrt(5.0)-1.0);

		/* initialize points for the search */
		x[0] = 0.0;
		x[1] = 1.0-rmag;
		x[2] = rmag;
		x[3] = 1.0;
		f[ifill] = fvalue;
		ifill = *iter-1;

	} else if (*iter<=4) {
	
		/* initialize function values */
		f[ifill] = fvalue;
		ifill = *iter-1;
	} else { 
	
		/* start the search */
		f[ifill] = fvalue;
		im = 0;
		fm = f[0];
		for (j=1; j<4; j++) {
			if (f[j]>=fm) continue;
			im = j;
			fm = f[j];
		}

		/* update bracketing interval */
		*alpha = x[im];
		if ((im==0)||(im==1)) {
			x[3] = x[2];
			f[3] = f[2];
			x[2] = x[1];
			f[2] = f[1];
			x[1] = (x[2] - x[0])*rmag +x[0];
			ifill = 1;
		} else {
			x[0] = x[1];
			f[0] = f[1];
			x[1] = x[2];
			f[1] = f[2];
			x[2] = x[3] - (x[3] - x[1])*rmag;
			ifill = 2;
		}
	}

	/* output values */
	*xvalue = x[ifill];
	*iter=*iter+1;
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
