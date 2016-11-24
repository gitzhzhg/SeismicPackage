/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/* SUCDDECON: $Revision: 1.16 $ ; $Date: 2015/10/12 16:58:08 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"

#define PNOISE .001
#define LOOKFAC 5	/* Look ahead factor for npfao	*/
#define PFA_MAX 720720  /* Largest allowed nfft	   */

/*********************** self documentation ******************************/
char *sdoc[] = {
" 									",
" SUCDDECON - DECONvolution with user-supplied filter by straightforward",
" 	      Complex Division in the frequency domain			",
" 									",
" sucddecon <stdin >stdout [optional parameters]			",
" 									",
" Required parameters:							",
" filter= 		ascii filter values separated by commas		",
" 		...or...						",
" sufile=		file containing SU traces to use as filter	",
"                       (must have same number of traces as input data	",
" 			 for panel=1)					",
" Optional parameters:							",
" panel=0		use only the first trace of sufile as filter	",
" 			=1 decon trace by trace an entire gather	",
" pnoise=0.001		white noise factor for stabilizing results	",
"	 				(see below)		 	",
" sym=0		not centered, =1 center the output on each trace",
" verbose=0		silent, =1 chatty				",
" 									",
" Notes:								",
" For given time-domain input data I(t) (stdin) and deconvolution	",
" filter F(t), the frequency-domain deconvolved trace can be written as:",
"									",
"	 I(f)		I(f) * complex_conjugate[F(f)]			",
" D(f) = ----- ===> D(f) = ------------------------ 			",
"	 F(f)		|F(f)|^2 + delta				",
"									",
" The real scalar delta is introduced to prevent the resulting deconvolved",
" trace to be dominated by frequencies at which the filter power is close",
" to zero. As described above, delta is set to some fraction (pnoise) of ",
" the mean of the filter power spectra. Time sampling rate must be the 	",
" same in the input data and filter traces. If panel=1 the two input files",
" must have the same number of traces. Data and filter traces don't need to",
" necessarily have the same number of samples, but the filter trace length",
" length be always equal or shorter than the data traces. 		",
" 									",
" Caveat: 								",
" You may need to apply frequency filtering to get acceptable output	",
"   sucddecon  ...| sufilter f=f1,f2,f3,f4 				",
" where f1,f2,f3,f4 are an acceptable frequency range, and you may need ",
" to mute artifacts that appear at the beginning of the output, as well.",
" 									",
" Trace header fields accessed: ns, dt					",
" Trace header fields modified: none					",
" 									",
NULL};

/* Credits:
 *	CWP: Ivan Vasconcelos
 *              some changes by John Stockwell
 *  CAVEATS: 
 *	In the option, panel=1 the number of traces in the sufile must be 
 *	the same as the number of traces on the input.
 *
 * Trace header fields accessed: ns,dt
 * Trace header fields modified: none
 */
/**************** end self doc *******************************************/

segy tr, sutrace;

int
main(int argc, char **argv)
{
	int nt;			/* number of samples on input traces	*/
	int it,i;		/* counter 				*/
	float dt;		/* sampling interval of input data	*/
	
	float *rf=NULL;		/* filter coefficients			*/
	float *rt=NULL;		/* data coefficients in time		*/
	complex *cf=NULL;	/* filter coefficients in freq		*/
	complex *ct=NULL;	/* data coefficients in freq		*/
	
	float delta=0.0;	/* white noise term			*/
	complex cdelta;		/* delta as a complex number		*/
	float pnoise=0.0;	/* input parameter for computing delta	*/
	float sum_powspec=0.0;	/* sum of filter power spectra		*/
	
	float nyq=0.0;		/* nyquist frequency			*/
	int nfft;		/* number of points for fft data trace  */
	int nf;			/* number of frequencies (incl Nyq)	*/
	
	int nfilter=0;		/* filter length in samples		*/
	int nfiltby2=0;		/* filter length/2 in samples		*/
	int ntr=0;		/* trace counter			*/

	float *filter=NULL;	/* filter if set as filter=		*/
	cwp_String sufile;	/* name of file containing one SU trace */
	FILE *fp=NULL;		/* ... its file pointer			*/

	int panel=0;		/* xcor with trace or panel 		*/
	int verbose=0;		/* =0 silent =1 chatty	 		*/
	int sym=0;		/* =0 not centered; =1 output centered	*/

	cwp_Bool is_filter=cwp_false;	/* is filter set from commandline?*/
	cwp_Bool is_panel=cwp_false;	/* is panel set? */

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	
	/* Get info from first trace */ 
	if (!gettr(&tr)) err("can't get first trace");
	nt = tr.ns;
	dt = ((double) tr.dt)/1000000.0;
	nyq=0.5/dt;
	
	/* Set up FFT parameters */
	nfft = npfaro(nt, LOOKFAC * nt);
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)
				err("Padded nt=%d -- too big", nfft);

	nf = nfft/2 + 1;
	
	/* Get parameters and set up filter array */
	if (!getparint("panel", &panel)) 		panel = 0;
	if (panel) is_panel=cwp_true;

	if (!getparint("verbose", &verbose))		verbose = 0;
	if (!getparint("sym", &sym))			sym = 0;
	if (!getparfloat("pnoise",&pnoise))		pnoise = PNOISE;
	if (!getparstring("sufile", &sufile)) {
		if (!(nfilter = countparval("filter"))) {
			warn("must specify filter= desired filter");
			err(" or sufile= ");
		}
			nfiltby2 = nfilter/2;
			filter = ealloc1float(nfilter+nfiltby2);
			getparfloat("filter",filter);
			
			is_filter = cwp_true;
			is_panel = cwp_false;

			rf = ealloc1float(nfft);
			memset(( void *) rf, 0, nfft*FSIZE);
		
	} else {
		fp = efopen(sufile, "r");
		fgettr(fp, &sutrace);
		nfilter = sutrace.ns;
		nfiltby2 = nfilter/2;
		rf = ealloc1float(nfft+nfiltby2);
		cf = ealloc1complex(nfft+nfiltby2);
	}
        checkpars();


	/* Allocate space */
	rt = ealloc1float(nfft+nfiltby2);
	ct = ealloc1complex(nfft+nfiltby2);
	cf = ealloc1complex(nfft+nfiltby2);
	
	/* Zero out arrays */
	memset( (void *) rt, 0 , (nfft+nfiltby2)*FSIZE);
	memset( (void *) ct, 0 , (nfft+nfiltby2)*sizeof(complex));
	memset( (void *) cf, 0 , (nfft+nfiltby2)*sizeof(complex));
	

	if (is_filter) {
		memcpy(( void *) rf, (const void *) filter, nfilter*FSIZE);
	} else {
		memcpy(( void *) rf, (const void *) sutrace.data,nfilter*FSIZE);
	}
	
	/* Debugging warnings */
	if (verbose) warn(" nt=%d nfilter=%d nfft=%d ", nt, nfilter, nfft); 

	if(!is_filter) rewind(fp);
	/* Main loop over traces */
	do {
		++ntr;
		
		if (is_panel) {
			cwp_Bool is_filter_out = cwp_false;

			if (!is_filter_out) {
				if ((!fgettr(fp, &sutrace))) {
					if (verbose)
					warn("Out of traces in sufile at trace= %d !",ntr);
					is_filter_out = cwp_true;
				}
			}
	
			/* zero out trace and filter arrays */
			memset((void *)rt, 0, nfft*FSIZE);
			memset((void *)rf, 0, nfft*FSIZE);

			/* copy traces from data and filter */
			memcpy(( void *) (rt+nfiltby2), (const void *) tr.data, nt*FSIZE);
			memcpy(( void *) rf,
				(const void *) sutrace.data, nfilter*FSIZE);

			
		} else {
			/* zero out trace array and copy data */
			memset((void *)rt, 0, nfft*FSIZE);
			memcpy((void *)(rt+nfiltby2), (const void *) tr.data, nt*FSIZE);

		}

		/* zero out ct and cf arrays */
		memset((void *)ct, 0, (nfft + nfiltby2)*sizeof(complex));
		memset((void *)cf, 0, (nfft + nfiltby2)*sizeof(complex));
					
		/* compute complex traces */
		pfarc(1,nfft,rt,ct);
		pfarc(1,nfft,rf,cf);
		
		/* first compute the the noise factor delta */
		/* find sum of power spectrum */
		for (i=0; i<nfft; i++)
			sum_powspec += rcabs(cf[i]);
		
		/* delta = pnoise * mean of filter power spectrum */
		delta = pnoise * sum_powspec/nfft;
		cdelta=cmplx(delta,0.0); /*... as a complex number */
		
						
		/* compute deconvolved trace
 		 	  data		       ct(i) * conjg[cf(i)]
 		 decon = ----- ===> ctout(i) = ------------------------ 
 			  filter		cf[i]*conjg(cf[i]) + delta	
 		*/
		for (i=0; i<nfft; ++i){
			complex numer=cmul(ct[i],conjg(cf[i])); /* numerator */
			complex denom=cadd(cmul(cf[i],conjg(cf[i])),cdelta);
						/* denominator */

			/* perform deconvolution by division */
			ct[i] = cdiv(numer,denom);

			/* for centered output flip sign every other sample */
			if (sym) {
				if (ISODD(i)) {
					ct[i].r = -ct[i].r;
					ct[i].i = -ct[i].i;
				}
			}
		}
		
		/* Perform inverse fourier transform */
		pfacr(-1,nfft,ct,rt);
		
		/* Copy deconvolved samples */
		for (it=0; it<nt; ++it)
				tr.data[it] = rt[nfiltby2+it];
		
		puttr(&tr);

	} while (gettr(&tr));

	return(CWP_Exit());
}
