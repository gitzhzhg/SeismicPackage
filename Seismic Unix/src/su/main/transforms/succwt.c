/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* succwt - Complex continuous wavelet transform of seismic traces */

#include "par.h"
#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUCCWT - Complex continuous wavelet transform of seismic traces	",
"									",
" succwt < tdata.su > tfdata.su	[optional parameters]			",
"									",
" Required Parameters:							",
" None									",
"									",
" Optional Parameters:							",
" noct=5	Number of octaves (int)					",
" nv=10		Number of voices per octave (int)			",
" fmax=Nyq	Highest frequency in transform				",
" p=-0.5	Power of scale value normalizing CWT			",
"		=0 for amp-preserved spec. decomp.			",
" c=1/(2*fmax)	Time-domain inverse gaussian damping parameter		",
"		(bigger c means more wavelet oscillations,		",
"		default gives minimal oscillations)			",
" k=1		Use complex Morlet as wavelet transform kernel		",
"		=2 use Fourier kernel ... Exp[i 2 pi f t]		",
" fs=1		Use dyadic freq sampling (CWT standard, honors		",
"		noct, nv)						",
"		=2 use linear freq sampling (Fourier standard)		",
" df=1		Frequency sample interval in Hz (used only for fs=2)	",
"			NOTE: not yet implimented (hardwired to df=1) 	",
" dt=(from tr.dt)	Sample interval override (in secs, if time data)",
" verbose=0	 Run silent, except echo c value. (=1 for more info)	",
"									",
" Examples:								",
" This generates amplitude spec of the CWT impulse response (IR).	",
"  suspike ntr=1 ix1=1 nt=125 | succwt | suamp | suximage & 		",
" Real part of Fourier IR with linear freq sampling:			",
" suspike ntr=1 ix1=1 nt=125 | succwt k=2 fs=2 | suamp mode=real | suximage &",
" Real part of Fourier IR with dyadic freq sampling: 			",
" suspike ntr=1 ix1=1 nt=125 | succwt k=2 | suamp mode=real | suximage &",
"									",
" Inverse CWT: (within a constant scale factor)				",
"	... | succwt p=-1 | suamp mode=real | sustack key=cdp > inv.su	",
"									",
" Notes:								",
" 1. Total number of scales: nscale = noct*nv				",
" 2. Each input trace spawns nscale complex output traces		",
" 3. Lowest frequency in the transform is fmax/( 2^(noct-1/nv) )	",
" 4. Header field (cdp) used as cwt spectrum counter			",
" 5. Header field (cdpt) used as scale counter within cwt spectrum	",
" 6. Header field (gut) holds number of cwt scales `na'			",
" 7. Header field (unscale) holds CWT scale `a'				",
"									",
" Header fields set: tracl, cdp, cdpt, unscale, gut			",
" 									",
NULL};

/*
 * Copyright (c) University of Tulsa, 2003-4.
 * All rights reserved.			
 * Author:	UTulsa: Chris Liner, SEP: Bob Clapp
 *
 * todo:
 *	fix fs=2 case to allow df not equal to 1
 * History:
 * 6/18/04
 *	major overhaul by Clapp, including fourier implementation.
 *	Speedup ~ 41 times	(4100 %)
 * 2/20/04
 *	made p=-0.5 default
 * 2/16/04
 *	added p option to experiment with CWT normalization
 * 2/12/04
 *	replace fb (bandwidth parameter) with c (t-domain gaussian damping const.)
 * 2/10/04 --- in sync with EAS paper in prep
 *	changed morlet scaling (c = 1) to preserve time-domain peak amplitude
 *	changed morlet exp sign to std CWT definition (conjugate) and 
 *	mathematica result that only gives positive freq gaussian with neg exp
 * 1/26/04
 *	added linear frequency sampling option
 * 1/23/04
 *	figured out fb and made it a getpar
 *	key: Look at real ccwt output and determine fb by number of 
 *		oscillations desired:	Default gives -+-+-+-
 * 1/20/04
 *	beefed up verbose output 
 *	dimension wavelet to length 2*nt and change correlation call
 *	... this is done to avoid conv edge effects
 * 1/19/04
 *	added fourier wavlet option for comparison with Fourier Transform action
 * 1/17/04
 *	complex morlet amp scaling now set to preserve first scale amp with IR 
 * 1/16/04
 *	added dt getpar to handle depth input properly
 *	preserves first tracl so tracl is ok after spice
 * 11/11/03
 *	initial version
 *
 * Trace header fields set: tracl, cdp, cdpt, unscale, gut
 */
/**************** end self doc ********************************/

/* Function Prototypes */
static void 
cmorlet (float a, float fmax, float c, float p,float dt, int nt, complex *w);
static void
cfourier (float a, float fmax, float c, float p,float dt, int nt, complex *w);

#define I	cmplx(0.0, 1.0)

segy tr;	/* input trace */
segy cwt;	/* wavelet transform trace for one scale value */

int
main(int argc, char **argv)
{
	int icdp;	/* output (t,f) spectrum counter 	*/
	int ia;		/* cwt scale index counter		*/
	int ioct;	/* octave index counter			*/
	int iv;		/* voice index counter			*/
	int it;		/* time index counter			*/
	int itracl;	/* output trace counter			*/
	int na;		/* number of cwt scales			*/
	int noct;	/* number of octaves			*/
	int nv;		/* number of voices per octave		*/
	int nt;		/* number of time samples		*/
	int out;	/* output option flag			*/
	
	float *a=NULL;		/* array of scale values	*/
	complex *w=NULL;	/* wavelet trace		*/
	complex *ctr=NULL;	/* complex trace whose real part*/
				/*   is the input trace		*/
	complex *ctmp=NULL;	/* temporary complex trace	*/

	float dt;	/* time sampling interval		*/
	float fmax;	/* maximum peak frequency of wavelet	*/
	float c;	/* bandwidth parameter for wavelet	*/
	float e;	/* temporary variable			*/
	int k,npad;	/* kernel flag				*/
	int ishift;

	int verbose;	/* =0 silent, =1 chatty			*/
	int fs,i;	/* frequency sampling flag: 1=dyadic, 2=linear */
	float df;	/* freq sample rate for fs=2 option	*/
	float p;	/* power of scale in CWT transform	*/
	complex	**basis=NULL;	/* basis of wavelets		*/
	float rms;	/* root mean square value		*/

	/* hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(0);
	
	/* Get parameters */
	if (!getparint	("noct", &noct))	noct = 5;
	if (!getparint	("nv", &nv))		nv = 10;
	if (!getparint	("verbose", &verbose))	verbose = 0;
	if (!getparint	("out", &out))	 	out = 1;
	if (!getparint	("fs", &fs))		fs = 1;
	if (!getparfloat ("p", &p))		p = -0.5;

	/* get information from the first header */
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	npad=npfa(nt);
	if (!getparfloat ("dt", &dt)) dt = ((double) tr.dt)/1000000.0;

	/* number of scales in cwt */
	na = noct * nv;
		
	/* get fmax from user or default to nyquist */
	if (!getparfloat ("fmax", &fmax)) fmax = 1/(2*dt);
	if (!getparfloat ("c", &c)) c = 1/(2*fmax);
		warn("c = %g",c);

	/* avoid nf=nt in linear sampling */
	if (!getparfloat ("df", &df)) df = 1.0;

	/* avoid nf=nt in linear sampling */
	if (fs==2) na = (int) fmax / df;
			
	/* get kernel wavelet preference from user */
	if (!getparint ("k", &k)) k = 1;
	if (k != 1 && k != 2) {
		k = 1;
		warn("Bad kernal wavelet choice ... use Morlet");
	}


        checkpars();

	/* allocate arrays */
	a = alloc1float(na);
	w = ealloc1complex(npad);
	ctr = ealloc1complex(npad);
	ctmp = ealloc1complex(npad);
	basis = ealloc2complex(npad,na);
	
	/* load cwt scale array 
		fs=1 is dyadic (cwt) freq sampling
		fs=2 is linear (fourier) freq sampling 
		NOTE: df=1 hz hardwired for fs=2
	*/
	if (fs == 2) {
		for (ia=0; ia<na; ++ia) {
			a[ia] = (float) na / (float) (ia + 1);
			if (verbose==1)
				warn("check a again %d %f",ia,a[ia]);
		}
	} else {
		ia = 0;
		for (ioct=0; ioct<=noct-1; ++ioct) {
			for (iv=0; iv<=nv-1; ++iv) {
				e = ioct + (float) iv/nv;
				a[ia] = pow(2.0,e);
				if (verbose==1)
					warn("aval %d %f %f", ia,a[ia],e);
				ia += 1;
			}
		}
	}
	
	/* echo extra info if verbose is requested */
	if (verbose==1) warn("fmax = %g, fmin = %g\n", fmax, fmax/a[na]);
	if (verbose==1) warn("scale_index / scale_value / frequency");
	if (verbose==1) {
		for (ia=0; ia<na; ++ia) {
			warn("	%i / %g / %g", ia, a[ia],fmax/a[ia]);
		}
	}

	/* construct basis wavelets */
	for (ia=0; ia<na; ++ia) {
		if (k == 2) {
			cfourier(a[ia],fmax,c,p,dt,npad,ctmp);
		} else {
			cmorlet(a[ia],fmax,c,p,dt,npad,ctmp);
		}
		ishift=npad/2 ;
		for(i=0; i < ishift+1 ; ++i){
			basis[ia][i]=ctmp[i+ishift];
			basis[ia][i+ishift]=ctmp[i];
		}
		pfacc(1, npad, basis[ia]);
	}
	
	/* initialize counters for output traces and cwt spectra */
	itracl = tr.tracl;	/* preserve tracl */
	icdp = 1;
	
	/* loop over traces */
	do {
		
		/* load input trace into real part of complex trace */
		for (it=0,rms=0;it<nt;it++) {
			ctr[it].r = tr.data[it];
			ctr[it].i = 0.0;
			rms+=sqrt(tr.data[it]*tr.data[it]);
		}
		if (verbose==1) 
			warn("RMS IN %f",sqrt((double)rms/((double)nt)));
			for (it=nt;it<npad;it++){
				ctr[it].r=0.0;
				ctr[it].i=0.0;
			}
			pfacc(1,npad, ctr);

			/* loop over scales */
			for (ia=0; ia<na; ia++) {
				for(i=0; i < npad;i++) {
					ctmp[i]= cmul(basis[ia][i],ctr[i]);
				}
				pfacc(-1, npad, ctmp);
			
				/* done with this scale, copy header */
				/* from input trace */
				memcpy( (void *) &cwt, (const void *) &tr, 240);
		
				/* set output header values */
				cwt.tracl = itracl;
				cwt.cdp = icdp;
				cwt.cdpt = ia;
				cwt.unscale = a[ia];
				cwt.sut = (short) noct;
				cwt.gut = (short) noct*nv;
				cwt.trid = FUNPACKNYQ;
				cwt.ns=nt*2;

				memcpy((void*)cwt.data,
					(const void*)ctmp,nt*sizeof(float)*2);
		
				/* send output trace on its way */
				puttr(&cwt);
		
				/* bump output trace counter */
				++itracl;
			}
		
			/* bump output cwt spectrum counter */
			++icdp;
		
	} while (gettr(&tr));
	
	
	return(CWP_Exit());
}


static void 
cmorlet (float a, float fmax, float c, float p, float dt, int nt, complex *w)
/*****************************************************************************
Compute complex morlet wavelet trace for given translation (b) and scale (a)

	w[t,fmax,a,c] = a^p Exp( - i 2 pi (fmax*t/a) ) Exp( - ( t/(a*c) )^2 )
	
Ref:	Matlab wavelet toolbox www info
http://www.math.mcgill.ca/sysdocs/matlabr12/help/toolbox/wavelet/ch06_a37.html
	
******************************************************************************
Input:
a		wavelet scale
fmax		center frequency of wavelet (default = nyquist)
c		t-domain gaussian taper parameter (default = dt)
p		power of scale in CWT 
dt		time sample rate
nt		number of time samples
w[nt]		complex wavelet trace array
******************************************************************************
Notes: 
******************************************************************************
Author:	Chris Liner, UTulsa, 11/18/2003
******************************************************************************/
{
	float t;	/* time variable */
	float cc;	/* scale factor to normalize */
	float tc;	/* center time of wavelet */
	float fc;	/* wavelet center frequency at this scale */
	int it;	/* time index */
	float arg;	/* real argument of exponential */
	complex exparg;	/* complex exponential E^z */
	float rexp;	/* real exponential E^x */
	 
	/* center at time index nt/2 - 1 */
	tc = (nt/2 - 1)*dt;
	
	/* constant scale factor 
		... normalized to give ImpResp=1.0 at all scales */
	cc = pow(a,p);

	/* wavelet center frequency at this scale */
	fc = fmax / a;
		
	/* loop over time samples to build wavelet */
	for (it=1;it<=nt;++it) {

		t = (it-1)*dt;

		/* Set up args for complex exponential */
		arg = - 2.0 * PI * fc * (t-tc);
		exparg = cwp_cexp(crmul(I, arg));
	
		/* real exponential */
		arg = (t-tc)/(a*c);
		arg = - arg*arg;
		rexp = cc*exp( arg );

		/* product of comples and real exponentials */
		w[it] = crmul(exparg,rexp); 
	}
}

static void 
cfourier (float a, float fmax, float c, float p, float dt, int nt, complex *w)
/*****************************************************************************
Compute complex morlet wavelet trace for given translation (b) and scale (a)

	w[t,fmax,a] = Exp( - i 2 pi (fmax*t/a) )
	
******************************************************************************
Input:
a		wavelet scale
fmax		center frequency of wavelet (default = nyquist)
c		t-domain gaussian taper parameter (unused)
p		power of scale in CWT 
dt		time sampling interval
nt		number of time samples
w[nt]		complex wavelet trace array
******************************************************************************
Notes: 
******************************************************************************
Author:	Chris Liner, UTulsa, 11/18/2003
******************************************************************************/
{
	float t;	/* time variable				*/
	float cc;	/* scale factor to normalize			*/
	float tc;	/* center time of wavelet			*/
	float fc;	/* wavelet center frequency at this scale	*/
	int it;		/* time index					*/
	float arg;	/* real argument of exponential			*/
	complex exparg;	/* complex exponential E^z			*/
	 
	/* center at time index nt/2 - 1 */
	tc = (nt/2 - 1)*dt;
	
	/* constant scale factor 
		... normalized to give ImpResp=1.0 at all scales
	*/
	cc = pow(a,p);
	
	/* wavelet center frequency at this scale */
	fc = fmax / a;
		
	/* loop over time samples to build wavelet */
	for (it=1;it<=nt;++it) {

		t = (it-1)*dt;

		/* complex exponential */
		arg = - 2.0 * PI * fc * (t-tc);
		exparg = cwp_cexp(crmul(I, arg));
		w[it] = crmul(exparg,cc);
	}
}
