/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUGABOR: $Revision: 1.20 $ ; $Date: 2011/11/16 23:35:04 $	*/

#include "su.h"
#include "segy.h"

/*********************** self documentation **********************/
char *sdoc[] = {
"									",
" SUGABOR -  Outputs a time-frequency representation of seismic data via",
"	        the Gabor transform-like multifilter analysis technique ",
"		presented by Dziewonski, Bloch and  Landisman, 1969.	",
"									",
"    sugabor <stdin >stdout [optional parameters]			",
"									",
" Required parameters:					 		",
"	if dt is not set in header, then dt is mandatory		",
"									",
" Optional parameters:							",
"	dt=(from header)	time sampling interval (sec)		",
"	fmin=0			minimum frequency of filter array (hz)	",
"	fmax=NYQUIST 		maximum frequency of filter array (hz)	",
"	beta=3.0		ln[filter peak amp/filter endpoint amp]	",
"	band=.05*NYQUIST	filter bandwidth (hz) 			",
"	alpha=beta/band^2	filter width parameter			",
"	verbose=0		=1 supply additional info		",
"	holder=0		=1 output Holder regularity estimate	",
"				=2 output linear regularity estimate	",
"									",
" Notes: This program produces a muiltifilter (as opposed to moving window)",
" representation of the instantaneous amplitude of seismic data in the	",
" time-frequency domain. (With Gaussian filters, moving window and multi-",
" filter analysis can be shown to be equivalent.)			",
"									",
" An input trace is passed through a collection of Gaussian filters	",
" to produce a collection of traces, each representing a discrete frequency",
" range in the input data. For each of these narrow bandwidth traces, a ",
" quadrature trace is computed via the Hilbert transform. Treating the narrow",
" bandwidth trace and its quadrature trace as the real and imaginary parts",
" of a \"complex\" trace permits the \"instantaneous\" amplitude of each",
" narrow bandwidth trace to be compute. The output is thus a representation",
" of instantaneous amplitude as a function of time and frequency.	",
"									",
" Some experimentation with the \"band\" parameter may necessary to produce",
" the desired time-frequency resolution. A good rule of thumb is to run ",
" sugabor with the default value for band and view the image. If band is",
" too big, then the t-f plot will consist of stripes parallel to the frequency",
" axis. Conversely, if band is too small, then the stripes will be parallel",
" to the time axis. 							",
"									",
" Caveat:								",
" The Gabor transform is not a wavelet transform, but rather are sharp	",
" frame basis. However, it is nearly a Morlet continuous wavelet transform",
" so the concept of Holder regularity may have some meaning. If you are	",
" computing Holder regularity of, say, a migrated seismic section, then",
" set band to 1/3 of the frequency band of your data.			",
"									",
" Examples:								",
"    suvibro | sugabor | suximage					",
"    suvibro | sugabor | suxmovie n1= n2= n3= 				",
"     (because suxmovie scales it's amplitudes off of the first panel,  ",
"      may have to experiment with the wclip and bclip parameters	",
"    suvibro | sugabor | supsimage | ... ( your local PostScript utility)",
"									",
NULL};

/* Credits:
 *
 *	CWP: John Stockwell, Oct 1994
 *      CWP: John Stockwell Oct 2004, added holder=1 option
 * Algorithm:
 *
 * This programs takes an input seismic trace and passes it
 * through a collection of truncated Gaussian filters in the frequency
 * domain.
 *
 * The bandwidth of each filter is given by the parameter "band". The
 * decay of these filters is given by "alpha", and the number of filters
 * is given by nfilt = (fmax - fmin)/band. The result, upon inverse
 * Fourier transforming, is that nfilt traces are created, with each
 * trace representing a different frequency band in the original data.
 *
 * For each of the resulting bandlimited traces, a quadrature (i.e. pi/2
 * phase shifted) trace is computed via the Hilbert transform. The 
 * bandlimited trace constitutes a "complex trace", with the bandlimited
 * trace being the "real part" and the quadrature trace being the 
 * "imaginary part".  The instantaneous amplitude of each bandlimited
 * trace is then computed by computing the modulus of each complex trace.
 * (See Taner, Koehler, and Sheriff, 1979, for a discussion of complex
 * trace analysis.
 *
 * The final output for a given input trace is a map of instantaneous
 * amplitude as a function of time and frequency.
 *
 * This is not a wavelet transform, but rather a redundant frame
 * representation.
 *
 * References: 	Dziewonski, Bloch, and Landisman, 1969, A technique
 *		for the analysis of transient seismic signals,
 *		Bull. Seism. Soc. Am., 1969, vol. 59, no.1, pp.427-444.
 *
 *		Taner, M., T., Koehler, F., and Sheriff, R., E., 1979,
 *		Complex seismic trace analysis, Geophysics, vol. 44,
 *		pp.1041-1063.
 *
 * 		Chui, C., K.,1992, Introduction to Wavelets, Academic
 *		Press, New York.
 *
 * Trace header fields accessed: ns, dt, trid, ntr
 * Trace header fields modified: tracl, tracr, d1, f2, d2, trid, ntr
 */
/**************** end self doc ***********************************/

/* Prototype of function used internally */
static void gaussianFilter(float fcent, float dt,
			int nfft, float alpha, float  band, float *filter);

/* constants used internally */
#define FRAC0   0.0	/* Ratio of default fmin to Nyquist */
#define FRAC1   1.0	/* Ratio of default fmax to Nyquist */
#define FRAC2   0.05	/* Ratio of default band to Nyquist */
#define BETA	3.0	/* Default value of beta */
#define LOOKFAC 2	/* Look ahead factor for npfao	*/
#define PFA_MAX 720720  /* Largest allowed nfft	   */

/* global SEGY declaration */
segy tr;

int
main(int argc, char **argv)
{
	register float *rt;	/* real trace				*/
	register float *qt;	/* real quadrature trace		*/
	register complex *ct;   /* complex transformed trace		*/
	register complex *fct;  /* filtered complex transformed trace	*/

	float *filter;		/* filter array				*/
	float dt;		/* sample spacing			*/
	float nyq;		/* nyquist frequency			*/
	int nt;			/* number of points on input trace	*/
	int nfft;		/* number of points for fft trace	*/
	int nf;			/* number of frequencies (incl Nyq)	*/
	cwp_Bool seismic;	/* is this seismic data?		*/

	int ntr;		/* number of traces 			*/
	int nfilt;		/* number of filters 			*/
	int tracr=0;		/* tracr counter			*/
	int verbose=0;		/* verbose flag				*/
	int holder=0;		/* holder flag				*/

	float fcent;		/* center frequency of filters		*/
	float fmin;		/* minimum frequency to window		*/
	float fmax;		/* maximum frequency to window		*/
	float band;		/* frequency bandwidth of filters	*/
	float alpha;		/* filter bandwidth parameter		*/
	float beta;		/* ln[ filter peak amp/ filter end amp] */

	float **tmpdata=NULL;	/* temporary data storage */
	
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	seismic = ISSEISMIC(tr.trid);

	ntr = tr.ntr;
		
	if (!seismic)
		warn("input is not seismic data, trid=%d", tr.trid);
	nt = tr.ns;
	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
	if (!dt) err("dt field is zero and not getparred");
	nyq = 0.5/dt;

	/* Set up FFT parameters */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
		err("Padded nt=%d -- too big", nfft);
	nf = nfft/2 + 1;
	
	/* Get parameters */
	if (!getparint("verbose", &verbose))	verbose=0;
	if (!getparint("holder", &holder))	holder=0;
	if (!getparfloat("fmin", &fmin))	fmin = FRAC0*nyq;
	if (!getparfloat("fmax", &fmax))	fmax = FRAC1*nyq;
	if (!getparfloat("band", &band))	band = FRAC2*nyq;
	if (!getparfloat("beta", &beta))	beta = BETA;
	if (!getparfloat("alpha",&alpha))	alpha = beta/(band*band);


        checkpars();

	/* Compute number of filters */
	nfilt = NINT( (fmax - fmin) / band );
	if (verbose){
		warn("            alpha = %f",alpha);
		warn("             beta = %f",beta);
		warn("             band = %f",band);
		warn("number of filters = %d",nfilt);
	}

	/* Allocate fft arrays */
	rt   = ealloc1float(nfft);
	qt   = ealloc1float(nfft);
	ct   = ealloc1complex(nf);
	fct   = ealloc1complex(nf);
	filter = ealloc1float(nf);
	tmpdata = ealloc2float(nt,nfft);

	/* Main loop over traces */
	do {
		register int i,j;

		/* Load trace into rt (zero-padded) */
		memcpy((void *) rt, (const void *) tr.data, nt*FSIZE);
		memset((void *) (rt + nt),0, (nfft-nt)*FSIZE);

		/* FFT, filter, inverse FFT */
		pfarc(1, nfft, rt, ct);

		tracr =0;
		/* Loop over filter center frequencies */
		for (i = 0, fcent=fmin; i < nfilt; ++i, fcent+=band) {

			/* compute Gaussian filter */
			gaussianFilter(fcent, dt, nfft, alpha, band, filter);

			/* apply filter */
			for (j = 0; j < nf; ++j) 
				 fct[j] = crmul(ct[j], filter[j]);

			/* inverse fourier transform */
			pfacr(-1, nfft, fct, rt);

			/* construct quadrature trace via hilbert transform */
			hilbert(nt, rt, qt);

			if (!holder) {

				/* compute instantaneous amplitude */
				for (j = 0; j < nt ; ++j) 
					tr.data[j] = sqrt(rt[j]*rt[j] + qt[j]*qt[j]);


				tr.tracr = ++tracr;
				tr.sx = tr.tracl;
				tr.gx = tr.tracr;
				tr.d1 = dt;
				tr.f2 = fmin;
				tr.d2 = band;
				tr.trid = AMPLITUDE; 
				tr.ntr = ntr*nfilt;
				puttr(&tr); /* output time-freq panels */
			} else {
				/* compute instantaneous amplitude */
				for (j = 0; j < nt ; ++j) 
					tmpdata[j][i] = sqrt(rt[j]*rt[j] + qt[j]*qt[j]);
			}
		}

		if (holder) { /* compute the Holder regularity traces */
			float *x;
			float *y;
			float coeff[4];

				x = ealloc1float(nfilt);
			y = ealloc1float(nfilt);
			
	                /* Compute an estimate of the Lipschitz (Holder)   */
			/* regularity. Here we merely fit a straight line  */
			/* through the log-log graph of the of our wavelet */
			/* transformed data and return the slope + 1/2 as  */
			/* the regularity measure. 			   */

                	for ( j =0 ; j< nt ; ++j ) {
                        	x[0]=0;
                        	for ( i = 1 ; i < nfilt ; ++i ) {
					if (holder==1) {
                                		y[i] = log(ABS(tmpdata[j][i+1]
                                               	              - tmpdata[j][0]));
                                		x[i] = log(i);
					} else if (holder==2) {
                                		y[i] = ABS(tmpdata[j][i+1]
                                               	              - tmpdata[j][0]);
                                		x[i] = i;
					} else {
						err("holder must = 0,1,2!");
					}

                        	}
                        	y[0] =y[1] - (y[3] - y[2]);


                        	linear_regression(y, x, nfilt, coeff);

                        	/*  coeff[0] is the slope of the line */
                        	tr.data[j] = coeff[0] + 0.5;

                	}
			tr.tracr = ++tracr;
			tr.sx = tr.tracl;
			tr.gx = tr.tracr;
			tr.d1 = dt;
			tr.f2 = fmin;
			tr.d2 = band;
			tr.trid = AMPLITUDE; 
			tr.ntr = ntr*nfilt;
			puttr(&tr); /* output holder regularity traces */
		}
	} while (gettr(&tr));

	return(CWP_Exit());
}


static void gaussianFilter(float fcent, float dt,
			int nfft, float alpha, float  band, float *filter)
/*************************************************************************
gaussianFilter -- Gaussian filter
**************************************************************************
Input:
fcent		center (peak) frequency of Gaussian
dt		time sampling interval
nfft		number of points in the fft
band		frequency band of filter
alpha		filter windowing parameter

Output:
filter		array[nfft] filter values
**************************************************************************
Notes: Filter is to be applied in the frequency domain.
The parameter "band" here is the full bandwidth of the filter. In Dziewonski,
et. al, it is the half-bandwitdth. Hence, there are factors of 2 that
appear in the definitions of iflower, ifupper, and alpha.

**************************************************************************
Reference: Dziewonski, Bloch, and Landisman, 1969, A technique
		for the analysis of transient seismic signals,
		Bull. Seism. Soc. Am., 1969, vol. 59, no.1, pp.427-444.
**************************************************************************
Author:  CWP: John Stockwell,   Oct 1994
*************************************************************************/
{
	int i;			/* loop counter				*/
	int iflower;		/* integerization of min frequency	*/
	int ifupper;		/* integerization of max frequency	*/
	int nf;			/* number of frequencies (incl Nyq)	*/
	float onfft;		/* reciprocal of nfft			*/
	float df;		/* frequency spacing (from dt)		*/

	/* number of frequencies */
	nf = nfft/2 + 1;
	onfft = 1.0 / nfft;
	df = onfft / dt;

	/* compute integerized min, max frequencies defining filter window */
	iflower = NINT((fcent - band/2)/df); 
	if (iflower < 0) iflower=0;
	ifupper = NINT((fcent + band/2)/df) ;
	if (ifupper > nf) ifupper=nf;

	/* zero out filter array */
	memset( (void *) filter , 0, nf * FSIZE);
	
        /* loop over integerized frequencies to build filter */
        for (i = 0 ; i < nf ; ++i) {
		float f = i * df - fcent; /* freq's as floats */

		if ( iflower <= i && i <= ifupper)
			filter[i] = exp(-4.0 * alpha * f * f) * onfft;
        }


}
