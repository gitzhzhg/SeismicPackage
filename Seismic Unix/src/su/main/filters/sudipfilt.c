/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUDIPFILT: $Revision: 1.20 $ ; $Date: 2011/11/12 00:09:00 $		*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 								",
" SUDIPFILT - DIP--or better--SLOPE Filter in f-k domain	",
" 								",
" sudipfilt <infile >outfile [optional parameters]		",
"								",
" Required Parameters:						",
" dt=(from header)	if not set in header then mandatory	",
" dx=(from header, d1)	if not set in header then mandatory	",
"								",
" Optional parameters:						",
" slopes=0.0		monotonically increasing slopes		",
" amps=1.0		amplitudes corresponding to slopes	",
" bias=0.0		slope made horizontal before filtering	",
"								",
" verbose=0	verbose = 1 echoes information			",
"								",
" tmpdir= 	 if non-empty, use the value as a directory path",
"		 prefix for storing temporary files; else if the",
"	         the CWP_TMPDIR environment variable is set use	",
"	         its value for the path; else use tmpfile()	",
" 								",
" Notes:							",
" d2 is an acceptable alias for dx in the getpar		",
"								",
" Slopes are defined by delta_t/delta_x, where delta		",
" means change. Units of delta_t and delta_x are the same	",
" as dt and dx. It is sometimes useful to fool the program	",
" with dx=1 dt=1, thus avoiding units and small slope values.	",
"								",
" Linear interpolation and constant extrapolation are used to	",
" determine amplitudes for slopes that are not specified.	",
" Linear moveout is removed before slope filtering to make	",
" slopes equal to bias appear horizontal.  This linear moveout	",
" is restored after filtering.  The bias parameter may be	",
" useful for spatially aliased data.  The slopes parameter is	",
" compensated for bias, so you need not change slopes when you	",
" change bias.							",
" 								",
NULL};

/* Credits:
 *
 *	CWP: Dave (algorithm--originally called slopef)
 *	     Jack (reformatting for SU)
 *
 * Trace header fields accessed: ns, dt, d2
 */
/**************** end self doc ***********************************/


#define PFA_MAX	720720	/* Largest allowed nfft	          */

/* prototypes */
void slopefilter (int nslopes, float slopes[], float amps[], float bias,
	int nt, float dt, int ntr, float dx, FILE *datafp);
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr;

int
main(int argc, char **argv)
{
	int nt;		/* number of time samples			*/
	float dt;	/* time sampling interval			*/
	int ntr;	/* number of traces				*/
	float dx;	/* trace spacing (spatial sampling interval)	*/
	int nslopes;	/* number of slopes specified			*/
	float *slopes;	/* slopes at which amplitudes are specified	*/
	int namps;	/* number of amplitudes specified		*/
	float *amps;	/* amplitudes corresponding to slopes		*/
	float bias;	/* slope bias					*/
	int verbose;	/* flag for echoing info			*/
	char *tmpdir;	/* directory path for tmp files			*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/


	/* Hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);


	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nt = tr.ns;


	/* Get parameters */
	if (!getparint("verbose", &verbose))	verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
	if (!dt) err("dt field is zero and not getparred");
	if (!getparfloat("dx", &dx) && !getparfloat("d2", &dx))	dx = tr.d2;
	if (!dx) err("d2 field is zero and dx not getparred");

	nslopes = countparval("slopes");
	if (nslopes) {
		slopes = alloc1float(nslopes);
		getparfloat("slopes", slopes);
	} else {
		nslopes = 1;
		slopes = alloc1float(nslopes);
		slopes[0] = 0.0;
	}

	namps = countparval("amps");
	if (namps) {
		amps = alloc1float(namps);
		getparfloat("amps", amps);
	} else {
		namps = 1;
		amps = alloc1float(namps);
		amps[0] = 1.0;
		warn("no amps given--doing no-op");
	}

	if (!getparfloat("bias", &bias)) bias = 0.0;


	/* Check parameters */
	if (nslopes != namps)
		err("number of slopes (%d) must equal number of amps(%d)",
			nslopes, namps);
	{ register int i;
	  for (i=1; i<nslopes; ++i)
		if (slopes[i] <= slopes[i-1])
			err("slopes must be monotonically increasing");
	}


	/* Store traces and headers in tmpfile while getting a count */
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
		efwrite(&tr, 1, HDRBYTES, headerfp);
		efwrite(tr.data, FSIZE, nt, tracefp);
	} while (gettr(&tr));
	

	/* Apply slope filter */
	slopefilter(nslopes,slopes,amps,bias,nt,dt,ntr,dx,tracefp);


	/* Output filtered traces */
	erewind(headerfp);
	erewind(tracefp);
	{ register int itr;
	  for (itr = 0; itr < ntr; ++itr) {
		efread(&tr, 1, HDRBYTES, headerfp);
		efread(tr.data, FSIZE, nt, tracefp);
		puttr(&tr);
	  }
	}


	/* Clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	efclose(tracefp);
	if (istmpdir) eremove(tracefile);
	free1float(slopes);
	free1float(amps);

	return(CWP_Exit());
}



void slopefilter (int nslopes, float slopes[], float amps[], float bias,
	int nt, float dt, int nx, float dx, FILE *tracefp)
/******************************************************************************
apply slope filter in frequency-wavenumber domain
*******************************************************************************
Input:
nslopes		number of slopes (and amplitudes) specified
slopes		slopes at which amplitudes are specified (see notes below)
amps		amplitudes corresponding to slopes (see notes below)
bias		linear moveout slope before and after filtering
nt		number of time samples
dt		time sampling interval
nx		number of traces
dx		trace space (spatial sampling interval)
tracefp		file pointer to data to be filtered

Output:
tracefp		file pointer to filtered data
*******************************************************************************
Notes:
Linear interpolation and constant extrapolation are used to
determine amplitudes for slopes that are not specified.
******************************************************************************/
{
	int ntfft;		/* nt after padding for FFT */
	int nxfft;		/* nx after padding for FFT */
	float sfft;		/* scale factor for FFT */
	int nw;			/* number of frequencies */
	float dw;		/* frequency sampling interval */
	float fw;		/* first frequency */
	int nk;			/* number of wavenumbers */
	float dk;		/* wavenumber sampling interval */
	float w,k;		/* frequency and wavenumber */
	int it,ix,iw,ik;	/* sample indices */
	float slope,amp;	/* slope and amplitude for particular w,k */
	complex **cpfft;	/* complex FFT workspace */
	float **pfft;		/* float FFT workspace */
	float phase;		/* phase shift for bias */
	complex cshift;		/* complex phase shifter for bias */

	/* determine lengths and scale factors for prime-factor FFTs */
	ntfft = npfar(nt);
	nxfft = npfa(nx);
	sfft = 1.0/(ntfft*nxfft);
	
	/* determine frequency and wavenumber sampling */
	nw = ntfft/2+1;
	dw = 2.0*PI/(ntfft*dt);
	fw = 0.000001*dw; /* non-zero to avoid divide by zero w */
	nk = nxfft;
	dk = 2.0*PI/(nxfft*dx);

	/* allocate real and complex workspace for FFTs */
	cpfft = alloc2complex(nw,nk);
	pfft = alloc2float(ntfft,nxfft);

	/* copy data from input to FFT array and pad with zeros */
	rewind(tracefp);
	for (ix=0; ix<nx; ix++) {
		efread(pfft[ix], FSIZE, nt, tracefp);
		for (it=nt; it<ntfft; it++)
			pfft[ix][it] = 0.0;
	}
	for (ix=nx; ix<nxfft; ix++)
		for (it=0; it<ntfft; it++)
			pfft[ix][it] = 0.0;
	
	/* Fourier transform t to w */
	pfa2rc(1,1,ntfft,nx,pfft[0],cpfft[0]);
	
	/* do linear moveout bias via phase shift */
	for (ix=0; ix<nx; ix++) {
		for (iw=0,w=0.0; iw<nw; iw++,w+=dw) {
			phase = -ix*dx*w*bias;
			cshift = cmplx(cos(phase),sin(phase));
			cpfft[ix][iw] = cmul(cpfft[ix][iw],cshift);
		}
	}
	
	/* Fourier transform x to k */
	pfa2cc(-1,2,nw,nxfft,cpfft[0]);
	
	/* loop over wavenumbers */
	for (ik=0; ik<nk; ik++) {
	
		/* determine wavenumber */
		k = (ik<=nk/2) ? ik*dk : (ik-nk)*dk;
		
		/* loop over frequencies */
		for (iw=0,w=fw; iw<nw; iw++,w+=dw) {
		
			/* determine biased slope */
			slope = k/w+bias;
			
			/* linearly interpolate to find amplitude */
			intlin(nslopes,slopes,amps,amps[0],amps[nslopes-1],
				1,&slope,&amp);
			
			/* include fft scaling */
			amp *= sfft;
			
			/* filter real and imaginary parts */
			cpfft[ik][iw].r *= amp;
			cpfft[ik][iw].i *= amp;
		}
	}

	/* Fourier transform k to x */
	pfa2cc(1,2,nw,nxfft,cpfft[0]);

	/* undo linear moveout bias via phase shift */
	for (ix=0; ix<nx; ix++) {
		for (iw=0,w=0.0; iw<nw; iw++,w+=dw) {
			phase = ix*dx*w*bias;
			cshift = cmplx(cos(phase),sin(phase));
			cpfft[ix][iw] = cmul(cpfft[ix][iw],cshift);
		}
	}

	/* Fourier transform w to t */
	pfa2cr(-1,1,ntfft,nx,cpfft[0],pfft[0]);
	
	/* copy filtered data from FFT array to output */
	rewind(tracefp);
	for (ix=0; ix<nx; ix++)
		efwrite(pfft[ix], FSIZE, nt, tracefp);

	/* free workspace */
	free2complex(cpfft);
	free2float(pfft);
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
