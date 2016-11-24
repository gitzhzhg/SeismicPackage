/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUK1K2FILTER: $Revision: 1.13 $ ; $Date: 2011/11/12 00:09:00 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUK1K2FILTER - symmetric box-like K-domain filter defined by the	",
"		  cartesian product of two sin^2-tapered polygonal	",
"		  filters defined in k1 and k2				",
" 									",
"     suk1k2filter <infile >outfile [optional parameters]		",
"									",
" Optional parameters:							",
" k1=val1,val2,...	array of K1 filter wavenumbers			",
" k2=val1,val2,...	array of K2 filter wavenumbers			",
" amps1=a1,a2,...	array of K1 filter amplitudes			",
" amps2=a1,a2,...	array of K2 filter amplitudes			",
" d1=tr.d1 or 1.0	sampling interval in first (fast) dimension	",
" d2=tr.d1 or 1.0	sampling interval in second (slow) dimension	",
" quad=0		=0 all four quandrants				",
"			=1 (quadrants 1 and 4) 				",
"			=2 (quadrants 2 and 3) 				",
"									",
" Defaults:								",
" k1=.10*(nyq1),.15*(nyq1),.45*(nyq1),.50*(nyq1)			",
" k2=.10*(nyq2),.15*(nyq2),.45*(nyq2),.50*(nyq2)			",
" amps1=0.,1.,...,1.,0.  trapezoid-like bandpass filter			",
" amps2=0.,1.,...,1.,0.  trapezoid-like bandpass filter			",
"									",
" The nyquist wavenumbers, nyq1 and nyq2, are computed internally.	",
"									",
" verbose=0	verbose = 1 echoes information				",
"									",
" tmpdir= 	 if non-empty, use the value as a directory path	",
"		 prefix for storing temporary files; else if the	",
"	         the CWP_TMPDIR environment variable is set use		",
"	         its value for the path; else use tmpfile()		",
" 									",
" Notes:								",
" The filter is assumed to be symmetric, to yield real output		",
"									",
" Because the data are assumed to be purely spatial (i.e. non-seismic), ",
" the data are assumed to have trace id (30), corresponding to (z,x) data",
"									",
" The relation: w = 2 pi F is well known for frequency, but there	",
" doesn't seem to be a commonly used letter corresponding to F for the	",
" spatial conjugate transform variables.  We use K1 and K2 for this.	",
" More specifically we assume a phase:					",
"		-i(k1 x1 + k2 x2) = -2 pi i(K1 x1 + K2 x2).		",
" and K1, K2 define our respective wavenumbers.				",
" 									",
NULL};

/* Credits:
 *     CWP: John Stockwell, November 1995.
 *
 * Trace header fields accessed: ns, d1, d2
 */
/**************** end self doc ***********************************/


/* definitions */
#define PFA_MAX	720720	/* Largest allowed nfft		  */
#define LOOKFAC 2	/* look factor			  */
#define FRAC0   0.10    /* Ratio of default k1 to Nyquist */
#define FRAC1   0.15    /* Ratio of default k2 to Nyquist */
#define FRAC2   0.45    /* Ratio of default k3 to Nyquist */
#define FRAC3   0.50    /* Ratio of default k4 to Nyquist */

/* prototype of function used internally */
void polygonalFilter(float *f, float *amps, int npoly,
				int nfft, float dt, float *filter);
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/

segy tr;

int main(int argc, char **argv)
{
	int nx1,nx2;		/* numbers of samples			*/
	float dx1,dx2;		/* sampling intervals			*/
       
        float nyq1;		/* K1 Nyquist wavenumber		*/
        float nyq2;		/* K2 Nyquist wavenumber		*/

	int npoly1;		/* number of points defining k1 filter	*/
	int npoly2;		/* number of points defining k2 filter	*/
	int namps1;		/* number of amps defining k1 filter	*/
	int namps2;		/* number of amps defining k2 filter	*/

        float *k1;		/* wavenumber values defining k1 filter	*/
        float *k2;		/* wavenumber values defining k2 filter	*/
        float *amps1;		/* amplitude values defining k1 filter	*/
        float *amps2;		/* amplitude values defining k2 filter	*/

	float *k1filt;		/* k1 wavenumber filter			*/
	float *k2filt;		/* k2 wavenumber filter 		*/
	float **kfilter;	/* wavenumber filter 			*/

	int ix1,ix2;		/* sample indices			*/
	int nx1fft,nx2fft;	/* dimensions after padding for FFT	*/
	int nK1,nK2;		/* transform (output) dimensions	*/

        int ik;			/* k counter				*/
        int ik1;		/* k1 counter				*/
        int ik2;		/* k2 counter				*/
        int iamps;		/* amplitude counter			*/
        int icount;		/* zero counter				*/

        int quad;		/* flag for diagonal filter		*/

	register complex **ct;	/* complex FFT workspace		*/
	register float **rt;	/* float FFT workspace			*/
	int verbose;		/* flag for echoing info		*/
	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path		*/


	/* Hook up getpar to handle the parameters */
	initargs(argc,argv);
	requestdoc(1);

	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	if (tr.trid != TRID_DEPTH)
				warn("tr.trid = %d",tr.trid);
	nx1 = tr.ns;

	/* get sampling intervals */
	if (!getparfloat("d1", &dx1)) {
		if (tr.d1) { /* is dt field set? */
			dx1 = (float) tr.d1;
		} else { /* d1 not set, assume 1.0 */
			dx1 = 1.0;
			warn("tr.d1 not set, assuming d1=1.0");
		}
	}
	if (!getparfloat("d2",&dx2)) {
		if (tr.d2) { /* is d2 field set? */
			dx2 = tr.d2;
		} else {
			dx2 = 1.0;
			warn("tr.d2 not set, assuming d2=1.0");
		}
	}

	if (!getparint("verbose", &verbose))	verbose = 0;

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	    !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
		err("you can't write in %s (or it doesn't exist)", tmpdir);

	/* Compute Nyquist wavenumbers */
	nyq1 = 0.5/dx1;
	nyq2 = 0.5/dx2;

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

	nx2 = 0;
	do { 
		++nx2;
		efwrite(&tr, HDRBYTES, 1, headerfp);
		efwrite(tr.data, FSIZE, nx1, tracefp);
	} while (gettr(&tr));


	/* Determine lengths for prime-factor FFTs */
	nx1fft = npfaro(nx1, LOOKFAC*nx1);
	nx2fft = npfa(nx2);
	if (nx1fft >= SU_NFLTS || nx1fft >= PFA_MAX)
		err("Padded nx1=%d--too big",nx1fft);
	if (nx2fft >= SU_NFLTS || nx2fft >= PFA_MAX)
		err("Padded nx2=%d--too big",nx2fft);

	/* Determine number of wavenumbers in K1 and K2 */
	nK1 = nx1fft/2 + 1;
	nK2 = nx2fft/2 + 1;

        /* Get wavenumbers that define the k1 filter */
        if ((npoly1 = countparval("k1"))!=0) {
                k1 = ealloc1float(npoly1);
                getparfloat("k1",k1);
        } else {
                npoly1 = 4;
                k1 = ealloc1float(npoly1);

                k1[0] = FRAC0 * nyq1;
                k1[1] = FRAC1 * nyq1;
                k1[2] = FRAC2 * nyq1;
                k1[3] = FRAC3 * nyq1;
        }

	/* Check k1 values */
	if(npoly1 < 2) warn("Only %d value defining filter",npoly1);
        for(ik=0; ik < npoly1-1; ik++)
		if(k1[ik] < 0.0 || k1[ik] > k1[ik+1])
                                err("Bad filter parameters");

	/* Get k1 filter amplitude values */
        if ((namps1 = countparval("amps1"))!=0) {
                amps1 = ealloc1float(namps1);
                getparfloat("amps1",amps1);
        } else {
                namps1 = npoly1;
                amps1 = ealloc1float(namps1);

		/* default is a trapezoidal bandpass filter */
		for(iamps=1; iamps<namps1-1; iamps++) amps1[iamps]=1.;

		amps1[0]=0.;
		amps1[namps1-1]=0.;
        }
	if (!(namps1==npoly1)) 
		err("number of k1 values must = number of amps1 values");

        /* Check amps1 values */
        for(iamps = 0, icount=0; iamps < namps1 ; iamps++) {
		icount+=amps1[iamps];
                if( amps1[iamps] < 0.) err("amps1 values must be positive");
        }
        if (icount==0) err("All amps1 values are zero");
        for(iamps = 0, icount=0; iamps < namps1-1 ; ++iamps) {
			if(!(amps1[iamps]==amps1[iamps+1])) ++icount;
	}
        if (icount==0) warn("All amps1 values are the same");


        /* Get wavenumbers that define the k2 filter */
        if ((npoly2 = countparval("k2"))!=0) {
                k2 = ealloc1float(npoly2);
                getparfloat("k2",k2);
        } else {
                npoly2 = 4;
                k2 = ealloc1float(npoly2);

                k2[0] = FRAC0 * nyq2;
                k2[1] = FRAC1 * nyq2;
                k2[2] = FRAC2 * nyq2;
                k2[3] = FRAC3 * nyq2;
        }

	/* Check k2 values */
	if(npoly2 < 2) warn("Only %d value defining filter",npoly2);
        for(ik=0; ik < npoly2 - 1; ++ik)
		if(k2[ik] < 0.0 || k2[ik] > k2[ik+1])
                                err("Bad filter parameters");

	/* Get k2 filter amplitude values */
        if ((namps2 = countparval("amps2"))!=0) {
                amps2 = ealloc1float(namps2);
                getparfloat("amps2",amps2);
        } else {
                namps2 = npoly2;
                amps2 = ealloc1float(namps2);

		/* default is a trapezoidal bandpass filter */
		for(iamps=1; iamps<namps2-1; ++iamps) amps2[iamps]=1.;
		amps2[0]=0.;
		amps2[namps2-1]=0.;
        }
	if (!(namps2==npoly2)) 
		err("number of k2 values must = number of amps2 values");
        
	
        /* Check amps2 values */
        for(iamps = 0, icount=0; iamps < namps2 ; ++iamps) {
		icount+=amps2[iamps];
                if( amps2[iamps] < 0.) err("amps2 values must be positive");
        }
        if (icount==0) err("All amps2 values are zero");
        for(iamps = 0, icount=0; iamps < namps2-1 ; ++iamps) {
			if(!(amps2[iamps]==amps2[iamps+1])) ++icount;
	}
        if (icount==0) warn("All amps2 values are the same");

        if (!getparint("quad",&quad))		quad=0;

	/* Allocate space */
	rt = alloc2float(nx1fft, nx2fft);
	ct = alloc2complex(nK1,nx2fft);
	kfilter = alloc2float(nx1fft,nx2fft);
	k1filt = alloc1float(nK1);
	k2filt = alloc1float(nK2);

	/* Zero all arrays */
	memset((void *) rt[0], 0, nx1fft*nx2fft*FSIZE);
	memset((void *) kfilter[0], 0, nx1fft*nx2fft*FSIZE);
	memset((void *) ct[0], 0, nK1*nx2fft*sizeof(complex));
	memset((void *) k1filt, 0, nK1*FSIZE);
	memset((void *) k2filt, 0, nK2*FSIZE);

	/* Build Filters */
	polygonalFilter(k1,amps1,npoly1,nx1fft,dx1,k1filt);
	polygonalFilter(k2,amps2,npoly2,nx2fft,dx2,k2filt);

	/* There are only positive k1 values, but both pos and neg k2 values */	
	/* positive k1, positive k2 */
	if (quad==0 || quad==1)
		for (ik2=0; ik2<nK2; ++ik2) 
			for (ik1=0; ik1<nK1; ++ik1) 
				kfilter[ik2][ik1]=k1filt[ik1]*k2filt[ik2];

	/* positive k1, negative k2 */
	if (quad==0 || quad==2)
		for (ik2=nK2; ik2<nx2fft; ++ik2) 
			for (ik1=0; ik1<nK1; ++ik1) 
				kfilter[ik2][ik1]=k1filt[ik1]*k2filt[nx2fft-ik2];

	/* Load traces into fft arrays and close tmpfile */
	rewind(tracefp);
	for (ix2=0; ix2<nx2; ++ix2)
		efread(rt[ix2], FSIZE, nx1, tracefp);
	
	/* Fourier transform dimension 1 */
	pfa2rc(-1,1,nx1fft,nx2,rt[0],ct[0]);

	/* Fourier transform dimension 2 */
	pfa2cc(-1,2,nK1,nx2fft,ct[0]);

	/* Apply filter */
	for (ik2=0; ik2 < nx2fft; ++ik2)
		for (ik1=0; ik1 < nK1; ++ik1)
			ct[ik2][ik1] = crmul(ct[ik2][ik1], kfilter[ik2][ik1]);

	/* Inverse Fourier transform dimension 2 */
	pfa2cc(1,2,nK1,nx2fft,ct[0]);
	
	/* Inverse Fourier transform dimension 1 */
	pfa2cr(1,1,nx1fft,nx2,ct[0],rt[0]);

	erewind(headerfp);
	/* Output filtered traces */
	for (ix2=0; ix2 < nx2; ++ix2) { 

		efread(&tr, HDRBYTES, 1, headerfp);
		for (ix1=0; ix1 <nx1 ; ++ix1) 
			tr.data[ix1] = rt[ix2][ix1];

		puttr(&tr);
	}

	/* Clean up */
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	efclose(tracefp);
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());
}

void polygonalFilter(float *f, float *amps, int npoly,
				int nfft, float dt, float *filter)
/*************************************************************************
polygonalFilter -- polygonal filter with sin^2 tapering
**************************************************************************
Input:
f		array[npoly] of frequencies defining the filter
amps		array[npoly] of amplitude values
npoly		size of input f and amps arrays
dt		time sampling interval
nfft		number of points in the fft

Output:
filter		array[nfft] filter values
**************************************************************************
Notes: Filter is to be applied in the frequency domain
**************************************************************************
Author:  CWP: John Stockwell   1992
*************************************************************************/
#define PIBY2   1.57079632679490
{
        int *intfr;             /* .... integerizations of f		*/
        int icount,ifs;		/* loop counting variables              */
	int taper=0;		/* flag counter				*/
        int nf;                 /* number of frequencies (incl Nyq)     */
        int nfm1;               /* nf-1                                 */
        float onfft;            /* reciprocal of nfft                   */
        float df;               /* frequency spacing (from dt)          */

        
	intfr=alloc1int(npoly);

        nf = nfft/2 + 1;
        nfm1 = nf - 1;
        onfft = 1.0 / nfft;

        /* Compute array of integerized frequencies that define the filter*/
        df = onfft / dt;
        for(ifs=0; ifs < npoly ; ++ifs) {
                intfr[ifs] = NINT(f[ifs]/df);
                if (intfr[ifs] > nfm1) intfr[ifs] = nfm1;
        }

	/* Build filter, with scale, and taper specified by amps[] values*/
	/* Do low frequency end first*/
	for(icount=0; icount < intfr[0] ; ++icount) 
		filter[icount] = amps[0] * onfft;

	/* now do the middle frequencies */
	for(ifs=0 ; ifs<npoly-1 ; ++ifs){
	   if(amps[ifs] < amps[ifs+1]) {	
		++taper;
		for(icount=intfr[ifs]; icount<=intfr[ifs+1]; ++icount) {
		    float c = PIBY2 / (intfr[ifs+1] - intfr[ifs] + 2);
		    float s = sin(c*(icount - intfr[ifs] + 1));
		    float adiff = amps[ifs+1] - amps[ifs];
		    filter[icount] = (amps[ifs] + adiff*s*s) * onfft;
		}
	   } else if (amps[ifs] > amps[ifs+1]) {	
		++taper;
		for(icount=intfr[ifs]; icount<=intfr[ifs+1]; ++icount) {
			   float c = PIBY2 / (intfr[ifs+1] - intfr[ifs] + 2);
                	   float s = sin(c*(intfr[ifs+1] - icount + 1));
			   float adiff = amps[ifs] - amps[ifs+1];
                	   filter[icount] = (amps[ifs+1] + adiff*s*s) * onfft;
		  }
	   } else 
		if(!(taper)){
		for(icount=intfr[ifs]; icount <= intfr[ifs+1]; ++icount)
		   	   filter[icount] = amps[ifs] * onfft;
		} else {
		for(icount=intfr[ifs]+1; icount <= intfr[ifs+1]; ++icount)
		   	   filter[icount] = amps[ifs] * onfft;
		}
	}

	/* finally do the high frequency end */
	for(icount=intfr[npoly-1]+1; icount<nf; ++icount){
		filter[icount] = amps[npoly-1] * onfft;
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
