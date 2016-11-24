/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUFXDECON: $Revision: 1.10 $ ; $Date: 2011/11/16 17:47:47 $	*/	

#include "su.h"
#include "segy.h"
#include "header.h"
#include <signal.h>

/*********************** self documentation **********************/
char *sdoc[] = {
" 									",
" SUFXDECON - random noise attenuation by FX-DECONvolution              ",
"									",
" sufxdecon <stdin >stdout [...]	                                ",
"									",
" Required Parameters:							",
"									",
" Optional Parameters:							",
" taper=.1	length of taper                                         ",
" fmin=6.       minimum frequency to process in Hz  (accord to twlen)   ",
" fmax=.6/(2*dt)  maximum frequency to process in Hz                    ",
" twlen=entire trace  time window length (minimum .3 for lower freqs)   ",
" ntrw=10       number of traces in window                              ",
" ntrf=4        number of traces for filter (smaller than ntrw)         ",
" verbose=0	=1 for diagnostic print					",
" tmpdir=	if non-empty, use the value as a directory path	prefix	",
"		for storing temporary files; else, if the CWP_TMPDIR	",
"		environment variable is set, use its value for the path;",
"		else use tmpfile()					",
"									",
" Notes: Each trace is transformed to the frequency domain.             ",
"        For each frequency, Wiener filtering, with unity prediction in ",
"        space, is used to predict the next sample.                     ",
"        At the end of the process, data is mapped back to t-x domain.  ", 
"									",
"                                                                       ",
NULL};

/* Credits:			
 *
 *	CWP: Carlos E. Theodoro (10/07/97)
 *
 * References:      							
 *		Canales(1984):'Random noise reduction' 54th. SEGM	
 *		Gulunay(1986):'FXDECON and complex Wiener Predicition   
 *                             filter' 56th. SEGM	                
 *		Galbraith(1991):'Random noise attenuation by F-X        
 *                             prediction: a tutorial' 61th. SEGM	
 *
 * Algorithm:
 *	- read data
 *	- loop over time windows
 *		- select data
 *		- FFT (t -> f)
 *		- loop over space windows
 *			- select data
 *			- loop over frequencies
 *				- autocorelation
 *				- matrix problem
 *				- construct filter
 *				- filter data
 *			- loop along space window
 *				- FFT (f -> t)
 *				- reconstruct data
 * 	- output data
 *
 * Trace header fields accessed: ns, dt, d1
 * Trace header fields modified: 
 */

/**************** end self doc ***********************************/

#define LOOKFAC	2	/* Look ahead factor for npfaro	  */
#define PFA_MAX	720720	/* Largest allowed nfft	          */

/* Function Prototypes */
void cxcor (int lx, int ifx, complex *x,
            int ly, int ify, complex *y, 
            int lz, int ifz, complex *z);
void cconv (int lx, int ifx, complex *x,
	    int ly, int ify, complex *y,
	    int lz, int ifz, complex *z);
static void closefiles(void);

/* Globals (so can trap signal) defining temporary disk files */
char tracefile[BUFSIZ];	/* filename for the file of traces	*/
char headerfile[BUFSIZ];/* filename for the file of headers	*/
FILE *tracefp;		/* fp for trace storage file		*/
FILE *headerfp;		/* fp for header storage file		*/

/* segy trace */
segy tr;

int
main(int argc, char **argv)
{
        int nsp;		/* number of samples in input trace	*/
	int nspw;		/* number of samples in time window     */
	int nspws;	       	/* number of samples in window (wo taper)*/
	int nspwf;	       	/* number of samples in first window    */
	int nspwi;	       	/* number of samples in intermed. window */
	int nstaper;	       	/* number of samples in taper		*/
	int ntrc;	       	/* number of input traces		*/
	int ntrw;	       	/* number of traces in window		*/
	int ntrwu;	       	/* updated number of traces in window	*/
	int ntw;	       	/* number of time windows		*/
	int nwx;	       	/* number of spacial windows		*/
	int nfft;		/* transform length			*/
	int nf;			/* number of frequencies		*/
	int ntrf;		/* number of traces for filter		*/
	int jx,jw,jr,itt;	/* summation indexes			*/
	int it,iw,ifq,ir;	/* summation indexes			*/
	int ig,ifv;		/* summation indexes			*/
	int verbose;		/* flag to get advisory messages	*/
	int *ipvt;		/* indices of pivot permutations	*/

	float *info;		/* index of last zero pivot		*/
	float dt;		/* sampling interval in secs		*/
	float d1;		/* sample interval in Hz		*/
	float fmin;		/* minimum frequency to process in Hz	*/
	float fmax;		/* maximum frequency to process in Hz	*/
	float taper;		/* length of taper			*/
	float twlen;       	/* time window length 			*/
	float **tdatain;      	/* real trace			   	*/
	float **tdataout;     	/* real trace			   	*/
	float *tidataw;       	/* real trace in time window - input   	*/
	float *ttidataw;      	/* real trace in time window - input   	*/
	float *todataw;       	/* real trace in time window - output   */
	float *ttodataw;      	/* real trace in time window - output   */
	float *rautoc;       	/* real part of autocorrelation   	*/
	float *iautoc;       	/* imaginary part of autocorretation 	*/
	float **rmatrix;      	/* complex autocorrelation matrix	*/
	float *gvector;       	/* advanced autocorrelation vector	*/

	complex *ct;		/* complex transformed trace (window)  	*/
	complex **fdata;	/* data - freq. domain          	*/
	complex **fdataw;	/* data - freq. domain - in window     	*/
	complex *sfreq;		/* single frequency vector 	  	*/
	complex **ffreq;	/* filtered frequency vector 	  	*/
	complex *freqv;		/* frequency vector		      	*/
	complex *sfreqout;	/* single frequency output vector      	*/
	complex *sfreqcj;	/* complex conjugate of sfreq	  	*/
	complex *autocorr;	/* autocorrelation output	  	*/
	complex *fvector;	/* filter               	  	*/

	char *tmpdir;		/* directory path for tmp files		*/
	cwp_Bool istmpdir=cwp_false;/* true for user-given path	*/
 
	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);

	/* Get info from first trace */ 
	if (!gettr(&tr))  err("can't get first trace");
	nsp = tr.ns;

	if (!getparint("verbose", &verbose))	verbose=0;

	if (verbose) warn("nsp = %i",nsp);

	/* dt is used only to set output header value d1 */
	if (!getparfloat("dt", &dt))	dt = ((double) tr.dt)/1000000.0;
	if (!dt) {
	        dt = .004;
		if (verbose) warn("dt not set, assumed to be .004");
	}

	if (verbose) warn("dt = %f",dt);

	if (!getparfloat("taper", &taper)) taper=.1;
	if (taper==0.0) taper=.004; 

	if (!getparfloat("fmin", &fmin)) fmin= 6.;
	if (fmin==0.0) 
	        if (verbose) warn("using fmin=6 Hz");

	if (!getparfloat("fmax", &fmax)) fmax=.6/(2*dt);
	if (fmax==0.0) 
	        if (verbose) warn("using fmax=.6/(2*dt) Hz");

	if (!getparfloat("twlen", &twlen)) twlen=(float)(nsp-1)*dt;
	if (twlen<.3) {
	        twlen=.3;
	        if (verbose) warn("twlen cannot be less than .3s, using .3s");
	}

	/* setting taper and spatial and temporal windows */
	nstaper=NINT(taper/dt);
	nstaper = (nstaper%2 ? nstaper+1 : nstaper); 

	ntw = NINT((nsp-1)*dt/twlen);

	if (ntw==1) taper=0.0;

	nspws = NINT(twlen/dt) + 1;
	nspwf = nspws + nstaper/2;
	nspwi = nspws + nstaper;

	if (!getparint("ntrw", &ntrw)) ntrw=10;
	if (!ntrw) {
	        ntrw = 10;
		if (verbose) warn("ntrw cannot be zero, using 10 traces");
	}

	if (verbose) warn("ntrw = %i",ntrw);

	ntrwu = ntrw;

	if (!getparint("ntrf", &ntrf)) ntrf=4;
	if (!ntrf) 
	        if (verbose) warn("using ntrf=4");

	/* Look for user-supplied tmpdir */
	if (!getparstring("tmpdir",&tmpdir) &&
	        !(tmpdir = getenv("CWP_TMPDIR"))) tmpdir="";
	if (!STREQ(tmpdir, "") && access(tmpdir, WRITE_OK))
	        err("you can't write in %s (or it doesn't exist)", tmpdir);

        checkpars();

	/* store traces and headers in tempfiles while getting a count */
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

	ntrc = 0;
	do {
	        ++ntrc;
		efwrite(&tr,HDRBYTES,1,headerfp);
		efwrite(tr.data, FSIZE, nsp, tracefp);
	} while (gettr(&tr));
	erewind(tracefp);
	erewind(headerfp);
	if (verbose) warn("%i traces input",ntrc);

	nwx = ntrc/ntrw;

	/* Set up pfa fft */
	nfft = npfaro(2*nspwf, LOOKFAC * 2*nspwf);

/*
	if (nfft >= SU_NFLTS || nfft >= PFA_MAX)  
	        err("Padded nsp=%d--too big", nfft);
*/
	if (nfft >= PFA_MAX)  
	        err("Padded nsp=%d--too big", nfft);

	nf = nfft/2 + 1;
	d1 = 1.0/(nfft*dt);

	/* space allocation */
	ct = ealloc1complex(nfft);
	tidataw=ealloc1float(nfft);
	ttidataw=ealloc1float(nfft);
	fdata = ealloc2complex(nf,ntrc);
	fdataw = ealloc2complex(nf,2*ntrw+2*ntrf);
	tdatain = ealloc2float(nsp,ntrc);
	tdataout = ealloc2float(nsp,ntrc);
	sfreq = ealloc1complex(2*ntrw+2*ntrf);
	sfreqcj = ealloc1complex(2*ntrw+2*ntrf);
	autocorr = ealloc1complex(ntrf+1);
	rautoc = ealloc1float(ntrf+1);
	iautoc = ealloc1float(ntrf+1);
	rmatrix = ealloc2float(2*ntrf,2*ntrf);
	gvector = ealloc1float(2*ntrf);
	fvector = ealloc1complex(2*ntrf+1);
	ffreq = ealloc2complex(nf,2*ntrw);
	sfreqout = ealloc1complex(2*ntrw);
	freqv = ealloc1complex(nfft);
	todataw = ealloc1float(nfft);
	ttodataw = ealloc1float(nfft);
	ipvt = ealloc1int(4*ntrw);
	info = ealloc1float(4*ntrw);

	/* zero output file */
	memset((void *) tdataout[0], 0, ntrc*nsp*FSIZE);

	/* load traces into the zero-offset array and close tmpfile */
	efread(*tdatain, FSIZE, nsp*ntrc, tracefp);
	efclose(tracefp);

	/* If dt not set, issue advisory on frequency step d1 */
	if (dt && verbose)  warn("d1=%f", 1.0/(nfft*dt));

	if (verbose)
	   warn("nf=%i, d1=%f, nfft=%i, nstaper=%i",
			nf,d1,nfft,nstaper);

	/* loop over time windows */
	for (iw=0;iw<ntw;iw++) {

	        if (iw>0 && iw<ntw-1) nspw=nspwi; 
		else if (iw==0) 
		        if (ntw>1) nspw = nspwf;
			else        nspw = nsp;
		else
		        nspw = nsp - nspws*iw + nstaper/2;
 
		if (verbose)
		        warn("iw=%i, ntw=%i, nspw=%i, twlen=%f",
					iw,ntw,nspw,twlen); 

		/* zero fdata */
		memset((void *) fdata[0], 0, nf*ntrc*sizeof(complex));
     
		/* select data */
		for (jw=0;jw<ntrc;jw++) {
 
		        if (iw>0)
			   for (it=0;it<nspw;it++)
			     tidataw[it]=tdatain[jw][it + iw*nspws - nstaper/2];
			else
			        for (it=0;it<nspw;it++)  
				         tidataw[it]=tdatain[jw][it];	  

			memset((void *) (tidataw + nspw), 0, (nfft-nspw)*FSIZE);
			memset((void *) ct, 0, nfft*sizeof(complex));

			/* FFT from t to f */
			for (it=0;it<nfft;it++)
			        ttidataw[it]=(it%2 ? -tidataw[it] : tidataw[it]);
			pfarc(1, nfft, ttidataw, ct);

			/* Store values */    
			for (ifq = 0; ifq < nf; ifq++) { 
			        fdata[jw][ifq] = ct[nf-1-ifq];
			}
		}

		/* Loop over space windows */
		for (jx=0;jx<nwx;jx++){

		        /* to take care of a possible incomplete last window */
		        if (ntrc<jx*ntrw+2*ntrw) 
			        ntrwu = ntrc - jx*ntrw;
			else
			        ntrwu = ntrw;

			if (verbose) {
			        warn("jx=%i, ntrc=%i, ntrw=%i",
					jx,ntrc,ntrw);
				warn("ntrwu=%i, nwx=%i, ntrf=%i",
					ntrwu,nwx,ntrf);
			}

			/* zero fdataw */
			for (jw=0;jw<ntrw+2*ntrf;jw++)
			        memset((void *) fdataw[jw], 0, nf*sizeof(complex));

			/* select data */
			for (jw=0;jw<ntrwu+2*ntrf;jw++) 
			        for (ifq = 0; ifq < nf; ifq++) {

				   if (jx>0 && jx<nwx-1)  
				       fdataw[jw][ifq] = fdata[jw + jx*ntrw - ntrf][ifq];
					else if (jx==0)
					        if (jw>=ntrf && jw<ntrw+ntrf) 
						        fdataw[jw][ifq] = fdata[jw - ntrf][ifq];
						else if (jw<ntrf) 
						        fdataw[jw][ifq] = fdata[0][ifq];
						else 
						        if (nwx>1) 
							        fdataw[jw][ifq] = fdata[jw - ntrf][ifq];
							else 
							        fdataw[jw][ifq] = fdata[ntrc-1][ifq];
					else
					        if (jw<ntrwu+ntrf)
						        fdataw[jw][ifq] = fdata[jw + jx*ntrw - ntrf][ifq];
						else 
						        fdataw[jw][ifq] = fdata[ntrc-1][ifq];
	}

			/* loop over frequencies */
			for (ifq=0;ifq<nf;ifq++) {

			        if ((float)ifq*d1>=fmin && (float)ifq*d1<=fmax) {

				       /* Loop over space window */
				       memset((void *) sfreq, 0, (ntrwu+2*ntrf)*sizeof(complex));
				       memset((void *) sfreqcj, 0, (ntrwu+2*ntrf)*sizeof(complex));

				       for (jw=0;jw<ntrwu+2*ntrf;jw++) {
	  
					       sfreq[jw]=fdataw[jw][ifq];
					       sfreqcj[jw]=conjg(fdataw[jw][ifq]);
				       }

				       memset((void *) autocorr, 0, (ntrf+1)*sizeof(complex));

				       /* complex autocorrelation */
				       cxcor(ntrwu,0,sfreq,ntrwu,0,sfreq,ntrf+1,0,autocorr);

				       /* zeroing files */
				       memset((void *) rautoc, 0, (ntrf+1)*FSIZE);
				       memset((void *) iautoc, 0, (ntrf+1)*FSIZE);

				       /* taking real and imaginary parts */
				       for (jw=0;jw<ntrf+1;jw++) {
					       rautoc[jw]=autocorr[jw].r;
					       iautoc[jw]=autocorr[jw].i;
				       }

				       /* zeroing files */
				       memset((void *) gvector, 0, 2*ntrf*FSIZE);
				       memset((void *) fvector, 0, (2*ntrf+1)*sizeof(complex));
				       for (ir=0;ir<2*ntrf;ir++) 
					       memset((void *) rmatrix[ir], 0, 2*ntrf*FSIZE);

				       /* matrix problem */
				       for (ir=0;ir<ntrf;ir++) 
					       for (jr=0;jr<ntrf;jr++) { 
						       if (ir>=jr) rmatrix[ir][jr]=autocorr[ir-jr].r;
						       else        rmatrix[ir][jr]=autocorr[jr-ir].r;
					       }

				       for (ir=ntrf;ir<2*ntrf;ir++)
					       for (jr=0;jr<ntrf;jr++) {
						       if (ir-ntrf<jr) rmatrix[ir][jr]=-autocorr[jr-ir+ntrf].i;
						       else            rmatrix[ir][jr]= autocorr[ir-jr-ntrf].i;
					       }

				       for (ir=ntrf;ir<2*ntrf;ir++)
					       for (jr=ntrf;jr<2*ntrf;jr++)
						       rmatrix[ir][jr]=rmatrix[ir-ntrf][jr-ntrf];

				       for (ir=0;ir<ntrf;ir++)
					       for (jr=ntrf;jr<2*ntrf;jr++)
						       rmatrix[ir][jr]=-rmatrix[ir+ntrf][jr-ntrf];

				       for (ig=0;ig<2*ntrf;ig++) {
					       if (ig<ntrf) gvector[ig]=autocorr[ig+1].r;
				  else gvector[ig]=autocorr[ig-ntrf+1].i;
				       }

				       LU_decomposition(2*ntrf,rmatrix,ipvt,info);
				       backward_substitution(2*ntrf,rmatrix,ipvt,gvector);
      
				       /* construct filter */
				       for (ifv=0,ig=ntrf-1;ifv<ntrf;ifv++,ig--) 
					       fvector[ifv]=conjg(cmplx(gvector[ig]/2.,gvector[ig+ntrf]/2.));

				       for (ifv=ntrf+1,ig=0;ifv<2*ntrf+1;ifv++,ig++) 
					       fvector[ifv]=cmplx(gvector[ig]/2.,gvector[ig+ntrf]/2.);
	 
				       memset((void *) sfreqout, 0, ntrwu*sizeof(complex));

				       /* convolution of data with filter */
				       /* output is one sample ahead */
				       cconv(ntrwu+2*ntrf,-ntrf,sfreq,2*ntrf+1,-ntrf,fvector,ntrwu,0,sfreqout); 

				       /* store filtered values */
				       for (jw=0;jw<ntrwu;jw++) ffreq[jw][ifq]=sfreqout[jw];

				}
			} /* end of frequencies loop */

			/* loop along space windows */
			for (jw=0;jw<ntrwu;jw++) {
    
			        /* select data */
			        for (ifq=0,itt=nf-1;ifq<nf;ifq++,itt--)
				        freqv[ifq] = ffreq[jw][itt]; 

				memset((void *) (freqv+nf), 0, (nfft-nf)*sizeof(complex));
				memset((void *) todataw, 0, nfft*FSIZE);

				/* FFT back from f to t and scaling */
				pfacr(-1, nfft, freqv, todataw);
				for (it=0;it<MIN(nsp,nfft);it++)
					todataw[it]/=nfft; 
				for (it=0;it<MIN(nsp,nfft);it++)
				        ttodataw[it]=(it%2 ? -todataw[it] : todataw[it]); 
      
				/*loop along time */
				if (ntw>1) {
				        /* first portion of time window */
				        if (iw>0) 
					        for (it=0;it<nstaper;it++)
						        tdataout[jx*ntrw+jw][it+iw*nspws-nstaper/2]+=
							        ttodataw[it]*((float)(it)*dt/taper);
					else 
					        for (it=0;it<nstaper;it++)
						        tdataout[jx*ntrw+jw][it]=ttodataw[it];

					/* intermediate portion of time window */
					if (iw>0) 
					        for (it=nstaper;it<nspw-nstaper;it++)
						        tdataout[jx*ntrw+jw][it+iw*nspws-nstaper/2]=ttodataw[it];
					else 
					        for (it=nstaper;it<nspw-nstaper;it++)
						        tdataout[jx*ntrw+jw][it]=ttodataw[it];

					/* last portion of time window */
					if (iw>0 && iw<ntw-1) 
					        for (it=nspw-nstaper;it<nspw;it++)
						        tdataout[jx*ntrw+jw][it+iw*nspws-nstaper/2]+=
							        ttodataw[it]*(1.-((float)(it-nspw+nstaper))*dt/taper);
					else if (iw==ntw-1)
					        for (it=nspw-nstaper;it<nspw;it++)
						        tdataout[jx*ntrw+jw][it+iw*nspws-nstaper/2]=ttodataw[it];
					else 
					        for (it=nspw-nstaper;it<nspw;it++)
						        tdataout[jx*ntrw+jw][it]+=ttodataw[it]*(1.-((float)(it-nspw+nstaper))*dt/taper);
				}
				else {
				        for (it=0;it<nsp;it++) 
					        tdataout[jx*ntrw+jw][it]=ttodataw[it];
				}

			} /* end loop over space windows */

		} /* end loop over space windows */

	} /* end of time windows loop */

	for (jx=0;jx<ntrc;jx++) {
	        efread(&tr,HDRBYTES,1,headerfp);
		memcpy( (void *) tr.data, (const void *) tdataout[jx], nsp*FSIZE);
		puttr(&tr);
	}

	/* Clean up */
	free1float(tidataw);
	free1float(ttidataw);
	free2complex(fdataw);
	free1complex(sfreq);
	free1complex(sfreqcj);
	free1complex(autocorr);
	free1float(rautoc);
	free1float(iautoc);
	free2float(rmatrix);
	free1float(gvector);
	free1complex(fvector);
	free1complex(sfreqout);
	free1complex(freqv);
	free1float(todataw);
	free1float(ttodataw);
	free2complex(ffreq);
	free2float(tdatain);
 
	efclose(headerfp);
	if (istmpdir) eremove(headerfile);
	if (istmpdir) eremove(tracefile);

	return(CWP_Exit());

}

/* complex correlation */
void cxcor (int lx, int ifx, complex *x,
            int ly, int ify, complex *y, 
            int lz, int ifz, complex *z)
{
        int i,j;
	complex *xr;
	xr = alloc1complex(lx);
	for (i=0,j=lx-1; i<lx; ++i,--j)
	        xr[i] = conjg (x[j]);
	cconv(lx,1-ifx-lx,xr,ly,ify,y,lz,ifz,z);
	free1complex(xr);
}

/* complex convolution */
void cconv (int lx, int ifx, complex *x,
       	    int ly, int ify, complex *y,
	    int lz, int ifz, complex *z)
{
        int ilx=ifx+lx-1,ily=ify+ly-1,ilz=ifz+lz-1,i,j,jlow,jhigh;
	complex sum;
  
	x -= ifx;  y -= ify;  z -= ifz; 
	for (i=ifz; i<=ilz; ++i) {
	        jlow = i-ily;  if (jlow<ifx) jlow = ifx;
		jhigh = i-ify;  if (jhigh>ilx) jhigh = ilx;
		for (j=jlow,sum=cmplx(0.,0.); j<=jhigh; ++j){
		        sum = cadd(sum,cmul(x[j],y[i-j]));
		}
		z[i] = sum;
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
