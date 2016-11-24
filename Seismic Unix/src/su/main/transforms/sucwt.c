/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* SUCWT: $Revision: 1.12 $ ; $Date: 2015/08/07 22:34:39 $	*/

#include "su.h"
#include "segy.h"
#include "header.h"


/*********************** self documentation ******************************/
char *sdoc[] = {
"									",
" SUCWT - generates Continous Wavelet Transform amplitude, regularity	",
"         analysis in the wavelet basis					",
"									",
"     sucwt < stdin [Optional parameters ] > stdout			",
"									",
" Required Parameters:							",
" none									",
"									",
" Optional Parameters:							",
" base=10	Base value for wavelet transform scales			",
" first=-1	First exponent value for wavelet transform scales	",
" expinc=0.01	Exponent increment for wavelet transform scales		",
" last=1.5	Last exponent value for wavelet transform scales	",
"									",
" Wavelet Parameters:							",
" wtype=0		2nd derivative of Gaussian (Mexican hat)	",
"			=1 4th derivative of Gaussian (witch's hat)	",
"			=2 6th derivative of Gaussian (wizard's hat)	",
" nwavelet=1024		number of samples in the wavelet		",
" xmin=-20		minimum x value wavelet is computed		",
" xcenter=0		center x value  wavelet is computed 		",
" xmax=20		maximum x value wavelet is computed		",
" sigma=1		sharpness parameter ( sigma > 1 sharper)	",
"									",
" verbose=0		silent, =1 chatty				",
" holder=0		=1 compute Holder regularity estimate		",
" divisor=1.0		a floating point number >= 1.0 (see notes)	",
"									",
" Notes: 								",
" This is the CWT version of the time frequency analysis notion that is ",
" applied in sugabor.							",
" The parameter base is the base of the power that is applied to scale	",
" the wavelet. Some mathematical literature assume base 2. Base 10 works",
" well here.								",
" 									",
" Default option yields an output similar to that of sugabor. With the  ",
" parameter holder=1 an estimate of the instantaneous Holder regularity ",
" (the Holder exponent) is output for each input data value. The result ",
" is a Holder exponent trace for each corresponding input data trace.	",
" 									",
" The strict definition of the Holder exponent is the maximum slope of  ",
" the rise of the spectrum in the log(amplitude) versus log(scale) domain:",
" 									",
" divisor=1.0 means the exponent is computed simply by fitting a line   ",
" through all of the values in the transform. A value of divisor>1.0    ",
" indicates that the Holder exponent is determined as the max of slopes ",
" found in (total scales)/divisor length segments.			",
" 									",
" Some experimentation with the parameters nwavelet, first, last, and   ",
" expinc may be necessary before a desirable output is obtained. The	",
" most effective way to proceed is to perform a number of tests with    ",
" holder=0 to determine the range of first, last, and expinc that best  ",
" represents the data in the wavelet domain. Then experimentation with  ",
" holder=1 and values of divisor>=1.0 may proceed.			",
" 									",
NULL};

/*
 * Credits: 
 *	CWP: John Stockwell, Nov 2004
 * inspired in part by "bhpcwt" in the BHP_SU package, code written by
 *	BHP: Michael Glinsky,	c. 2002, based loosely on a Matlab CWT function
 *
 * References: 
 *         
 * Li C.H., (2004), Information passage from acoustic impedence to
 * seismogram: Perspectives from wavelet-based multiscale analysis, 
 * Journal of Geophysical Research, vol. 109, B07301, p.1-10.
 *         
 * Mallat, S. and  W. L. Hwang, (1992),  Singularity detection and
 * processing with wavelets,  IEEE Transactions on information, v 38,
 * March 1992, p.617 - 643.
 *         
 */

/**************** end self doc ********************************/


void
MexicanHatFunction(int nwavelet, float xmin, float xcenter,
			float xmax, float sigma, float *wavelet);

segy tr;	/* data for which transform is calculated */
segy outtr;	/* transforms */

int
main(int argc, char **argv)
{

	int i,j,k;		/* counters */
	int ns=0;		/* number of samples in input data */
	int nwavelet=1024;	/* number of samples in mother wavelet */

	float base=0.0;		/* base */
	float first=0.0;	/* first exponent */
	float expinc=0.0;	/* exponent increment */
	float last=0.0;		/* last exponent */
	float exponent=0.0;	/* each exponent */
	float maxscale=0.0;	/* maximum scale value */
	float minscale=0.0;	/* minimum scale value */

	float x=0.0;
	float dx=0.0;		/* xvalues incr */

	float xmin=0.0;		/* last xvalues - first vval */
	float xcenter=0.0;	/* x value of center of wavelet */
	float xmax=0.0;		/* last xvalues - first vval */
	float sigma=1.0;	/* sharpening parameter */

	float waveletinc=0.0;		/* wavelet interval */
	float fmin=0.0;		/* min, max filt value (debug) */
	float *xvalues=NULL;	/* wavelet xvalues */
	float **filt=NULL;	/* filter used for each conv */

	float *f=NULL;		/* scratch for filter fliplr */
	float *sucwt_buff=NULL;	/* scratch for convolution */
	float *scales=NULL;	/* scales */
	float *waveletsum=NULL;	/* cumulative sum of wavelet */

	float *rt=NULL;		/* temp data storage */
	float *qt=NULL;		/* temp hilbert transformed data storage */
	float **tmpdata=NULL;	/* temp data storage */

	int wtype=0;		/* type of wavelet selected */
	float *wavelet=NULL;	/* pointer to data constituting the wavelet */

	int verbose=0;		/* verbose flag */
	int *index=NULL;	/* wavelet subscripts to use for filter */
	int *nconv=NULL;	/* length of each filter */
	int nscales=0;		/* number of scales */

	int holder=0;		/* =1 compute the Holder-Lipschitz regularity */
	float divisor=1.0;	/* divisor used in Holder exponent calculation*/

	/* Initialize */
	initargs(argc, argv);
	requestdoc(1);


	/* Get parameters */
	if(!getparfloat("base",&base))			base = 10.;
	if(!getparfloat("first",&first))		first = -1.0;
	if(!getparfloat("expinc",&expinc))			expinc = 0.01;
	if(!getparfloat("last",&last))			last = 1.5;

	if(!getparint("wtype",&wtype))			wtype = 0;
	if(!getparint("nwavelet",&nwavelet))		nwavelet = 1024;
	if(!getparfloat("xmin",&xmin))			xmin = -20.0;
	if(!getparfloat("xcenter",&xcenter))		xmin = 0.0;
	if(!getparfloat("xmax",&xmax))			xmax = 20.0;
	if(!getparfloat("sigma",&sigma))		sigma = 1.0;

	if(!getparint("holder",&holder))		holder = 0;
	if(!getparfloat("divisor",&divisor))		divisor = 1.0;

	if(!getparint("verbose",&verbose))		verbose = 0;


        checkpars();

	if(verbose)
	 warn("base=%f, first=%f, expinc=%f, last=%f",base,first,expinc,last);


	/* Allocate space */
	xvalues = ealloc1float(nwavelet);
	wavelet = ealloc1float(nwavelet);
	memset((void *) xvalues, 0, nwavelet*FSIZE);
	memset((void *) wavelet, 0, nwavelet*FSIZE);

	/* Compute wavelet */
	if (wtype == 0 ) { /* so far only Mex. Hat function */
		MexicanHatFunction(nwavelet, xmin, xcenter,
					xmax, sigma, wavelet);
	} else {
		err("%d  type of wavelet not yet implemented",wtype); 
	}

	/* wavelet increment */
	waveletinc = (xmax - xmin)/(nwavelet - 1);

	/* verbose  warning */
	if(verbose)
	 warn("xmin=%f, xmax=%f, nwavelet=%d, waveletinc=%f",
			xmin,xmax,nwavelet,waveletinc);

	/* form xvalues[] array */
	for(i=0,x=xmin; i<nwavelet; ++i,x+=waveletinc) xvalues[i] = x;

	xvalues[nwavelet-1] = xmax;

	/* compute scales */
	scales = ealloc1float(SHRT_MAX);
	memset((void *) scales, 0, SHRT_MAX*FSIZE);

	exponent = first;
	x = 0;
	nscales = 0;
	minscale = pow(base,first);
	maxscale = pow(base,last);
	while(x <= maxscale) {
		x = pow(base,exponent);
		scales[nscales] = x;
		exponent+=expinc;
		++nscales;

		if(nscales == SHRT_MAX)
			err("Too many scales, change params and re-run\n");
	}
	--nscales;


	/* Allocate space */
	nconv = ealloc1int(nscales);
	index = ealloc1int(nwavelet);
	waveletsum = ealloc1float(nwavelet);
	filt = ealloc2float(nwavelet,nscales);
	f = ealloc1float(nwavelet);

	/* Zero out arrays */
	memset((void *) nconv, 0, nscales*ISIZE);
	memset((void *) index, 0, nwavelet*ISIZE);
	memset((void *) waveletsum, 0, nwavelet*FSIZE);
	memset((void *) filt[0], 0, nwavelet*nscales*FSIZE);
	memset((void *) f, 0, nwavelet*FSIZE);

	/* Form difference of xvalues */
	for(i=nwavelet-1; i>=0; --i)
		xvalues[i] = xvalues[i] - xvalues[0];	

	dx = xvalues[1];
	xmax = xvalues[nwavelet-1];

	/* verbose warning */
	if(verbose) {
		warn("first xvalues=%f, last xvalues=%f",
				xvalues[0],xvalues[nwavelet-1]);
		warn("dx=%f, xmax=%f",dx,xmax);
	}
	
	/* waveletsum is cumulative sum of wavelet multipled by dx */
	fmin = 0;

	for(i=0; i<nwavelet; ++i) {
		fmin += wavelet[i];
		waveletsum[i] = fmin * dx;
	}

	/* Build filters from summed wavelet */
	for(i=0; i<nscales; ++i) {
		nconv[i] = 1 + (int)(scales[i] * xmax);

		for(j=0; j<nconv[i]; ++j)
			index[j] = 1 + j / (scales[i] * dx);

		for(j=0; j<nconv[i]; ++j)
			f[j] = waveletsum[index[j]-1];

		/* flip left right */
		for(j=0,k=nconv[i]-1; j<nconv[i]; ++j,--k)
			filt[i][j] = f[k];
	}

	/* Verbose warning */
	if(verbose) {
		warn("Convolution Lengths");
		for(i=0; i<nscales; ++i) warn("%d ",nconv[i]);
	}
	if(verbose) warn("%d scales will be used for transforms",nscales);

	/* Get information from first trace */
	if(!gettr(&tr))
		err("Cannot get first trace\n");
	ns = tr.ns;

	/* Allocate temporary storage space */
	rt = ealloc1float(ns);
	qt = ealloc1float(ns);
	tmpdata = ealloc2float(nscales,ns);

	/* Zero out rt and qt */
	memset((void *) rt, 0, ns*FSIZE);
	memset((void *) qt, 0, ns*FSIZE);

	/* Alloc sucwt_buffer for longest convolution */
	sucwt_buff = ealloc1float(ns+nconv[nscales-1]+1);
	
	do {  /* main loop over traces */

		outtr.d2 = waveletinc;
		outtr.f2 = minscale;

		memcpy((void *)&outtr,(const void *)&tr,HDRBYTES);

		/* Apply filters to produce wavelet transform */
		for(i=0; i<nscales; ++i) { /* loop over scales */

			for(j=0; j<ns+nconv[nscales-1]+1; ++j)
			sucwt_buff[j] = 0;

			/* convolve wavelet with data */
			convolve_cwp(ns,0,tr.data,nconv[i],0,
					filt[i],ns,0,sucwt_buff);

			for(j=0; j<ns; ++j) 
				rt[j] = sucwt_buff[j+nconv[i]/2-1];

			for(j=ns-1; j>0; --j) 
				rt[j] = rt[j] - rt[j-1];

			for(j=0; j<ns; ++j)
				rt[j] = -sqrt(scales[i]) * rt[j];

				/* form the hilbert transform of rt */
				hilbert(ns,rt,qt);

			/* If not holder, then output envelope */
			if (!holder) {
				
				for (j=0 ; j<ns; ++j) {			
			  		outtr.data[j] = sqrt(rt[j]*rt[j] 
						               + qt[j]*qt[j]);
				}

				outtr.cdpt = i + 1;
				puttr(&outtr);
			} else {
				/* compute the modulus */
				for (j=0 ; j<ns; ++j) {			
			  		tmpdata[j][i] = sqrt(rt[j]*rt[j] + qt[j]*qt[j]);
				}

			}
		}


		if (holder) { /* compute the Holder regularity traces */
			float *x;
			float *y;
			float lrcoeff[4];

			x = ealloc1float(nscales);
			y = ealloc1float(nscales);
			
	                /* Compute an estimate of the Lipschitz (Holder)
			* regularity. Following Mallat (1992)	
                        *				
                        * ln | Wf(x,s)| <  ln C + alpha * ln|s|
                        *					
                        * alpha here is the Holder or Lipschitz exponent
                        * s is the length scale, and Wf(x,s) is f in the
                        * wavelet basis.			         
                        *					         
			* Here we merely fit a straight line		 
			* through the log-log graph of the of our wavelet
			* transformed data and return the slope as      
			* the regularity measure. 			
                        *					         
			*/

                	for ( j =0 ; j< ns ; ++j ) {
				int icount=0;
                        	x[0]=0;
                        	for ( i = 1 ; i < nscales ; ++i ) {
				
					
			          /* We stay away from values that will make */
				  /*  NANs in the output */
				  if ((i>1) && 
				      (tmpdata[j][i-1] - tmpdata[j][1] > 0.0)) {
                               		y[icount] = log(ABS(tmpdata[j][i]
                                     	              - tmpdata[j][1]));
                                			x[icount] = log(scales[i]-scales[1]);
						
						++icount;
					}

                        	}
				--icount;

				/* straight line fit, return slope */
				if ((icount> 10) && (divisor==1.0) ) {
                        	   linear_regression(y, x, icount, lrcoeff);
                        	   /* lrcoeff[0] is the slope of the line */
				   /* which is the Holder (Lipschitz) */
				   /* exponent */

                        	   outtr.data[j] = lrcoeff[0];

				} else  if ((icount> 10) && (divisor>1.0) ) {

				   float maxalpha=0.0;
				   float interval=icount/divisor;

				   for ( k = interval; k < icount; k+=interval){
                        	   	   linear_regression(y, x, k, lrcoeff);
					   maxalpha = MAX(lrcoeff[0],maxalpha);
					}
				   outtr.data[j] = maxalpha;		

				} else if ((icount < 10) && (divisor>=1.0)) {
				   outtr.data[j] = 0.0;		
				} else if ( divisor < 1.0 ) {
				   err("divisor = %f < 1.0!", divisor);	
				}
				



                	}

			puttr(&outtr); /* output holder regularity traces */
		}
	} while(gettr(&tr));

	return(CWP_Exit());
}

void
MexicanHatFunction(int nwavelet, float xmin, float xcenter, 
			float xmax, float sigma, float *wavelet) 
/***********************************************************************
MexicanHat - the classic Mexican hat function of length nwavelet 
************************************************************************
Input:
nwavelet	number of points total
xmin		minimum  x value
xcenter		central x value
xmax		maximum  x value
Output:
wavelet[]	array of floats of length nwavelet
************************************************************************
Notes:  

Gaussian:  g(x) = 1
		------------------- exp[ - ( x - xcenter)^2/2*sigma^2) ]
	       sigma*sqrt(2 * PI)

1st derivative of Gaussian:
g'(x) =  -(x - xcenter) 
	------------------- exp[ - (x - xcenter)^2/(2*sigma^2) ]
        (sigma^3)*sqrt(2 * PI)

Mexican Hat (2nd derivative of a Gaussian):
g''(x) = 1  
	--- 
      (sigma^3)* sqrt(2*PI)

           / ( x - xcenter)^2         \
	* |----------------   -  1  |
	   \ (sigma^2)              /
   
        * exp[ - (x - xcenter)^2/(2*sigma^2) ]

3rd derivative of Gaussian:
g'''(x) = 1  
	--- 
      (sigma^3)* sqrt(2*PI)

	* (x - xcenter)

           /      3 * ( x - xcenter)^3   \
	* |  1 -  ----------------        |
	   \         (sigma^2)           /
   
        * exp[ - (x - xcenter)^2/(2*sigma^2) ]

4th derivative of Gaussian (Witches' hat)
 (iv)
g  (x) =  1
	----
      (sigma^5)* sqrt(2*PI)
  
	   /      10 *( x - xcenter)^2         3 * ( x - xcenter)^4    \
        * | 1 -  -----------------------   +  ----------------------   |
           \           (sigma^2)                   (sigma^4)           /

        * exp[ - (x - xcenter)^2/(2*sigma^2) ]

************************************************************************
Author: CWP: John Stockwell (Nov 2004) 
************************************************************************/
{
	int i;			/* counter */
	double dxwavelet;	/* sampling interval in x */
	double mult;		/* multiplier on function */
	double twopi=2*PI;	/* 2 PI */

	/* check for error in nwavelet */
	if (nwavelet<=1) err("nwavelet must be greater than 1!");

	/* increment in x */
	dxwavelet = (xmax - xmin)/( nwavelet - 1);


	/* generate mexican hat function */
        /* ... multiplier ....*/
        mult = (1.0/(sigma*sigma*sigma * sqrt(twopi)));
	for(i=0; i<nwavelet; ++i) {
		float x = i*dxwavelet - xcenter; 

		wavelet[i] = mult * (x*x/(sigma*sigma) - 1.0) 
				* exp(- x*x/(2.0*sigma*sigma) );
	}
}
