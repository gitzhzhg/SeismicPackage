/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

# include "par.h"

/*********************** self documentation **********************/
/*********************************************************************
SMOOTH - Functions to compute smoothing of 1-D or 2-D input data

dlsq_smoothing		perform 1 or 2-D smoothing of a 2_D dataset
			via damped least squares. The user specifies
			the degree of smoothing required and the program
			outputs an rms error function to help decide if the
			degree of smoothing was right 

SG_smoothing_filter	computes a 1-D smmothing filter via Savitzky-Golay
			least squares. This filter is then convolved with 
			the data to effect the smoothing. A 2-D array can be
			smoothed in both directions with two passes through
			this function 

rwa_smoothing_filter	computes 1-D smoothing filter via running window average
			The window can be rectangular or triangular (weighted).
			This filter will then be convolved with the data.
			A 2-D array can be smoothed first in one direction and 
			then n the other with two passes through the function.

smooth_segmented_array  Smooth a segmented array

smooth_1		Zhenuye Liu's damped least squares smoothing of 1D data

**************************************************************************
Function Prototypes:
void dlsq_smoothing (int nt, int nx, int ift, int ilt, int ifx, int ilx, 
	float r1, float r2, float rw, float **traces);

void SG_smoothing_filter(int np, int nl, int nr, int ld, int m, float *filter);

void rwa_smoothing_filter (int flag, int nl, int nr, float *filter);

void gaussian2d_smoothing (int nx, int nt, int nsx, int nst, float **data);

void gaussian1d_smoothing (int ns, int nsr, float *data);

void smooth_histogram (int nintlh, float *pdf);

void smooth_segmented_array (float *indx,float *val,int n,int sm,int inc,int m);

void smooth_1(float *x, float *z, float r, int n);

**************************************************************************
dlsq_smoothing:
Input:
nt		number of samples in vertical (time) axis
nx		number of samples in horizontal axis (traces)
ift		index of upper left corner of data window to smooth
ilt		index of lower left corner of data window to smooth
ifx		index of upper right corner of data window to smooth
ilx		index of lower right corner of data window to smooth
r1		smoothing parameter in first dimension, usually between 1&20
r2		smoothing parameter in second dimension, usually between 1&20
rw		smoothing parameter of window function. If zero, the window
		will have sharp edges
traces		2-D array of data to smooth

Output:
traces		2-D array of smoothed data

Notes:
Larger r1 and r2 result in smoother data. Recommended ranges are from 1 to 20.

The program outputs a file called err which gives the rms error between the
input and the smoothed data as a function of the first dimension (time or
depth). For most applications, if this error is between 0.1 and 0.01 the 
smoothing parameters are O.K, otherwise, try changing them.

The smoothing can be done in a window of data rather than in the whole data
by specifying the index coordinates of the window. If smoothing of the whole
data is desired, ifx=0, ilx=nx, ift=0, ilt=nt.

If smoothing of only one trace is desired, ifx=0, ilx=1, ift=0, ilt=nt.

Smoothing of the window boundaries (blurring the edges of the window) can be 
done with a rw different than zero, otherwise the edges will be sharp.	

Credits:
Adapted by Gabriel Alvarez (1995) from the program smooth2.c of the CWP library 
	written by Zhenyue Liu (1992)

***************************************************************************

SG_smoothing_filter:
Input:
np		number of points in output filter (np>=nl+nr+1)
nl		number of left (past) data points used
nr		number of right (future) data points used
ld		order of desired derivative (=0 for smoothed function)
m		order of smoothing polynomial (normally 2 or 4)

Output parameters:
filter		vector of m-points filter coefficients to be convolved 
		with the data to be smoothed

Notes:
This filter is based on the assumption that the underlying function can be
well-fitted by a polynomial. When this is the case, the results are pretty
good. 
 
The filter is particularly good to smooth data that contains narrow peaks,
since it tends to honor them. If the peaks are too narrow with respect to the
filter width, however, some attenuation of the peaks occurs. Higher order 
(higher m) filters tend to perform better with narrower peaks at the expense of
the degree of smoothing of broader features. In general, the best results are
obtained when the full width (nl+nr+1) of the 4-degree filter is between 1 and
2 times the FWHM (full width at half of maximum) of the features that we
desire to smooth in the data.

Wider filters produce greater smoothing effect than narrower ones for filters
of a given order.

To within the roundoff error, the filter coefficients should add to one so that
the area under the curve is preserved after the smoothing. The sign of the 
samples is not preserved in general.

In order to compute filtered numerical derivatives (and not just to smooth 
the data), the parameter ld should be given the value of the desired
derivative (everything else is the same). 
	
A typical call to the convolution subroutine to apply the filter will be:
conv (nl+nr+1,-nl,filter,nd,0,data,nd,0,result);  where data[nd] is the input
data to smooth and result[nd] is the smoothed data

References:
Press, Teukolsky, Vettering, Flannery, Numerical Recipes in C:
		the art of scientific programming. Cambridge University
		Press. Second Edition (1992).
Ziegler, Applied Spectroscopy, Vol. 35, pp. 88-92.
Bromba and Ziegler, Analytical Chemistry, Vol. 53, pp 1582-1586.
***************************************************************************
Credits:
Adapted from Numerical Recipes in C by Gabriel Alvarez (1995)

**************************************************************************** 
rwa_smoothing_filter:
Input:
flag		=1 for rectangular window. =2 for triangular (weighted) window
nl		number of left (past) data points used
nr		number of right (future) data points used

Output:
filter		array[nl+nr+1] of filter points to be convolved with the data

Notes:
The rectangular window should only be used when the data to be smoothed is
fairly smooth already.  larger windows are not recommended unless real
smoothing is desired.

The triangular window will give more weight to the points that are closer
to the one to smooth. Although this is normally desirable, the degree of
smoothing for a given filter length is much less than with the rectangular
windom.

Both of this windows preserve the are under the curve and the sign of the 
samples to be smoothed.

A typical call to the convolution subroutine to apply the filter will be:
conv (nl+nr+1,-nl,filter,nd,0,data,nd,0,result);  where data[nd] is the input
data to smooth and result[nd] is the smoothed data
************************************************************************
Credits:
Gabriel Alvarez (1995).

***********************************************************************
smooth_segmented_array - Smooth a segmented array
Input:
index		float array of indexes
val		float array of values
n		number of elements in arrays
sm		smoothing factor
inc		normal increment of index, if 
			index[i]-index[i-1] > inc then new segment
m		method of smoothing

        1 Gaussian 1d
        2 damped least squares
        3 Sawitzky-Golay
        4 Running average
        5 Running average with triangluar window
        6 median filter

***********************************************************************
Author: Potash Corporation: Balasz Nemeth
Code given to CWP in 2008.
***********************************************************************/
/**************************************************************************
smooth_1  --- damped least squares smoothing of nonuniformly sampled 1d array
**************************************************************************
Input:
	x     array of horizontal coordinates
	z     array of vertical coordinate
	r     smoothing operator length
	n     number of points input 
Output:
	z     array of smoothed vertical coordinates
**************************************************************************
Reference:
    Liu, Zhenyue, 1994, Velocity smoothing: theory and implementation, 
    Project Review, 1994, Consortium Project on Seismic Inverse Methods
    for Complex Stuctures (in review).
**************************************************************************
Author: CWP: Zhenyue Liu, 1994
**************************************************************************/


/**************** end self doc ********************************/



/**********************************************************************

	Subroutine to smooth a 2-D array in both dimensions
		based on a damped least squares technique

**********************************************************************/
void dlsq_smoothing (int nt, int nx, int ift, int ilt, int ifx, int ilx, 
	float r1, float r2, float rw, float **traces)
/**********************************************************************
Input:
nt		number of samples in vertical (time) axis
nx		number of samples in horizontal axis (traces)
ift		index of upper left corner of data window to smooth
ilt		index of lower left corner of data window to smooth
ifx		index of upper right corner of data window to smooth
ilx		index of lower right corner of data window to smooth
r1		smoothing parameter in first dimension, usually between 1&20
r2		smoothing parameter in second dimension, usually between 1&20
rw		smoothing parameter of window function. If zero, the window
		will have sharp edges
traces		2-D array of data to smooth

Output:
traces		2-D array of smoothed data

Notes:
Larger r1 and r2 result in smoother data. Recommended ranges are from 1 to 20.
***************************************************************************
Credits:
Adapted by Gabriel Alvarez (1995) from the program smooth2.c written by
	Zhenyue Liu (1992)
**************************************************************************/
 
{
	int nmax;	/* maxumum between nt and nx */
	int ix, it;	/* loop counters */
	int *win;	/* 1d array defining the corners of smoothing window */
	float **input;	/* array of input data */
	float **w;	/* intermediate array */
	float *esterr;	/* array of error estimates as a function of x1 */
	float *d, *e;	/* input arrays for subroutine tripd */
	float *f;	/* intermediate array */
	float err0=0.0;	/* error variable */
	float rms=0.0;	/* rms value */
	float amp1,amp2;
	FILE *out1;
	
	/* scale the smoothing parameter */
	r1 = r1*r1*0.25;
	r2 = r2*r2*0.25;
	rw = rw*rw*0.25;
 
	/* allocate space */
	nmax = (nt<nx)?nx:nt;
	win = alloc1int(4);
	input = alloc2float(nt,nx);
	w = alloc2float(nt,nx);
	esterr = alloc1float(nmax);
	d = alloc1float(nmax);
	e = alloc1float(nmax);
	f = alloc1float(nmax);
 
	/* save the original data */
        for(ix=0; ix<nx; ++ix)
	 	for(it=0; it<nt; ++it) 
			input[ix][it]=traces[ix][it];

	/* define window to smooth */
	win[0] = ift;
	win[1] = ilt;
	win[2] = ifx;
	win[3] = ilx;

	/* define the window function */
	for(ix=0; ix<nx; ++ix)
	 	for(it=0; it<nt; ++it)
			w[ix][it] = 0;	
	for(ix=win[2]; ix<win[3]; ++ix)
	 	for(it=win[0]; it<win[1]; ++it) 
			w[ix][it] = 1;	

	/* do the smoothing making sure that the window makes sense */
	if(win[0]>0 || win[1]<nt || win[2]>0 || win[3]<nx){

		/* smooth the window function */
         	for(it=0; it<nt; ++it){
	 		for(ix=0; ix<nx; ++ix){
				d[ix] = 1.0+2.0*rw;
				e[ix] = -rw;
				f[ix] = w[ix][it];
			}
        		d[0] -= rw;
         		d[nx-1] -= rw;
         		tripd(d,e,f,nx);
	 		for(ix=0; ix<nx; ++ix)
				w[ix][it] = f[ix];
		}
         	for(ix=0; ix<nx; ++ix){
	 		for(it=0; it<nt; ++it){
				d[it] = 1.0+2.0*rw;
				e[it] = -rw;
				f[it] = w[ix][it];
		}
        		d[0] -= rw;
         		d[nt-1] -= rw;
         		tripd(d,e,f,nt);
	 		for(it=0; it<nt; ++it)
				w[ix][it] = f[it];
		}
	}

	/* solving for the smoothing data */
        for(it=0; it<nt; ++it){
	 	for(ix=0; ix<nx-1; ++ix){
			d[ix] = 1.0+r2*(w[ix][it]+w[ix+1][it]);
			e[ix] = -r2*w[ix+1][it];
			f[ix] = traces[ix][it];
		}
		d[0] -= r2*w[0][it];
		d[nx-1] = 1.0+r2*w[nx-1][it];
		f[nx-1] = traces[nx-1][it];
         	tripd(d,e,f,nx);
	 	for(ix=0; ix<nx; ++ix) 
			traces[ix][it] = f[ix];
	}
        for(ix=0; ix<nx; ++ix){
	 	for(it=0; it<nt-2; ++it){
			d[it] = 1.0+r1*(w[ix][it+1]+w[ix][it+2]);
			e[it] = -r1*w[ix][it+2];
			f[it] = traces[ix][it+1];
		}
		f[0] += r1*w[ix][1]*traces[ix][0];
         	d[nt-2] = 1.0+r1*w[ix][nt-1];
		f[nt-2] = traces[ix][nt-1];
         	tripd(d,e,f,nt-1);
	 	for(it=0; it<nt-1; ++it) 
			traces[ix][it+1] = f[it];
	}

	/*calculate the RMS error of smoothed data */
      	for(it=0; it<nt; ++it){
		for(ix=0; ix<nx; ++ix){
			amp1=input[ix][it];
			amp2=traces[ix][it];
			err0 += (amp1-amp2)*(amp1-amp2);
			rms += amp1*amp1;
		}
		if (rms<0.001) esterr[it] = 1.0;
		else esterr[it] = sqrt(err0/rms);
	}

	/* output error data */
	out1=fopen("error","w");
	fwrite(esterr,sizeof(float),nt,out1);
	fclose(out1);

	/* free allocated space */
	free2float(input);
	free2float(w);
	free1float(esterr);
	free1float(d);
	free1float(e);
	free1float(f);
}

/**********************************************************************

	Subroutine to compute smoothing filter coefficients via 
		Savitzky-Golay least squares fitting

***********************************************************************/
void SG_smoothing_filter (int np, int nl, int nr, int ld, int m, float *filter)
/***********************************************************************
Input Parameters:
np		number of points in output filter
nl		number of left (past) data points used
nr		number of right (future) data points used
ld		order of desired derivative (=0 for smoothed function)
m		order of smoothing polynomial (normally 2 or 4)

Output parameters:
filter		vector of m-points filter coefficients to be convolved 
		with the data to be smoothed

Notes:
This filter is based on the assumption that the underlying function can be
well-fitted by a polynomial. When this is the case, the results are pretty
good. 
 
The filter is particularly good to smooth data that contains narrow peaks,
since it tends to honor them. If the peaks are too narrow with respect to the
filter width, however, some attenuation of the peaks occurs. Higher order 
(higher m) filters tend to perform better with narrower peaks at the expense of
the degree of smoothing of broader features. In general, the best results are
obtained when the full width (nl+nr+1) of the 4-degree filter is between 1 and
2 times the FWHM (full width at half of maximum) of the features that we
desire to smooth in the data.

Wider filters produce greater smoothing effect than narrower ones for filters
of a given order.

To within the roundoff error, the filter coefficients should add to one so that
the area under the curve is preserved after the smoothing. The sign of the 
samples is not preserved in general.

In order to compute filtered numerical derivatives (and not just to smooth 
the data), the parameter ld should be given the value of the desired
derivative (everything else is the same). 
	
A typical call to the convolution subroutine to apply the filter will be:
conv (nl+nr+1,-nl,filter,nd,0,data,nd,0,result);  where data[nd] is the input
data to smooth and result[nd] is the smoothed data

References:
Press, Teukolsky, Vettering, Flannery, Numerical Recipes in C:
		the art of scientific programming. Cambridge University
		Press. Second Edition (1992).
Ziegler, Applied Spectroscopy, Vol. 35, pp. 88-92.
Bromba and Ziegler, Analytical Chemistry, Vol. 53, pp 1582-1586.
***************************************************************************
Credits:
**************************************************************************
Credits
slightly modified from Numerical Recipes in C by Gabriel Alvarez (1995)
*************************************************************************/

{
	int imj,ipj,j,k,kk,mm;
	int *idx;			/* vector of indices */
	float d,fac,sum;		/* auxiliary variables */
	float **a;			/* matrix of .. */
	float *b;			/* vector of ... */

	/* defensive programming, check input */
	if (np < nl+nr+1 || nl < 0 || nr < 0 || ld > m || nl+nr < m)
		err(" error in smoothing function, check input parameters");

	/* allocate working space */
	idx = alloc1int(m+1);
	a = alloc2float(m+1,m+1);
	b = alloc1float(m+1);

	/* set up normal equations of the desired least-squares fit */
	for (ipj=0; ipj<=(m << 1); ipj++) {
		sum = (ipj ? 0.0 : 1.0);
		for (k=1; k<=nr; k++)
			sum +=pow((double)k,(double)ipj);
		for (k=1; k<=nl; k++)
			sum +=pow((double)-k,(double)ipj);
		mm = MIN(ipj,2*m-ipj);
		for (imj= -mm; imj<=mm; imj+=2)
			a[(ipj+imj)/2][(ipj-imj)/2]=sum;
	}

	/* solve normal equation via LU decomposition */
	LU_decomposition(m+1, a, idx, &d);

	/* initialize vector */
	for (j=0; j<=m; j++)
		b[j]=0.0;
	b[ld]=1.0;		/* right-hand-side vector is unit depending
					on what derivative we want */

	/* get one row of the inverse matrix */
	backward_substitution(m+1, a, idx, b);

	/* initialize output vector */
	for (kk=0; kk<np; kk++)
		filter[kk]=0.0;

	/* compute Savitzky-Golay coefficients */
	for (k= -nl; k<=nr; k++) {
		sum = b[0];
		fac=1.0;

		/* each coefficient is the dot product of powers of an 
		integer	with the inverse matrix row */
		for (mm=0; mm<m; mm++)
			sum +=b[mm+1]*(fac *=k);	
		/*kk=((np-k) % np);  store coefficients in wrap around order*/
		kk=k+nl;	/*store coefficients in normal order */
		filter[kk]=sum;	
	}

	/* free working space */
	free2float(a);
	free1float(b);
	free1int(idx);
}
/************************************************************************
	Subroutine to compute one-dimensional smoothing filter
		via running window average. 

************************************************************************/
void rwa_smoothing_filter (int flag, int nl, int nr, float *filter)
/************************************************************************
Input:
flag		=1 for rectangular window. =2 for triangular (weighted) window
nl		number of left (past) data points used
nr		number of right (future) data points used

Output:
filter		vector[nl+nr+1] of filter points to be convolved with the data

Notes:
The rectangular window should only be used when the data to be smoothed is
fairly smooth already; larger windows are not recommended unless extreme
smoothing is desired.

The triangular window will give more weight to the points that are closer
to the one to smooth. Although this is normally desirable, the degree of
smoothing for a given filter length is much less than with the rectangular
windom.

Both of these windows preserve the are under the curve and the sign of the 
samples to be smoothed.

A typical call to the convolution subroutine to apply the filter will be:
conv (nl+nr+1,-nl,filter,nd,0,data,nd,0,result);  where data[nd] is the input
data to smooth and result[nd] is the smoothed data
************************************************************************
Credits:
Gabriel Alvarez (1995).
************************************************************************/
{
	int i;				/* loop counter */
	int np=nl+nr+1;			/* number of filter points */

	if (flag==1) {
		float scale=1.0/np;	/* scale for rectangular window */

		for (i=0; i<np; i++) filter[i]=scale;

	} else if (flag==2) {
		float scale=0.0;	/* scale for triangular window */
		
		for (i= -nl; i<0; i++) filter[i+nl]=1+(float)i/nl;
		for (i=1; i<nr; i++) filter[i+nl]=1-(float)i/nr;
		filter[nl]=1.0;
	
		/* normalize */
		for (i=0;i<np;i++) scale+=filter[i];
		for (i=0;i<np;i++) filter[i] /=scale;
	} else err("error in rwa_smoothing filter, flag should 1 or 2\n");
}

/******************************************************************************

		Subroutine to apply 2-D gaussian smoothing
	
******************************************************************************/	
void gaussian2d_smoothing (int nx, int nt, int nsx, int nst, float **data)
/******************************************************************************
Input:
nx		number of horizontal samples (traces)
nt		number of vertical (time) samples 
nsx		number of samples for horizontal smoothing (0 for no smoothing)
nst		number of samples for vertical smoothing (0 for no smoothing)
data		2-D array[nx][nt] of data to be smoothed

Output:
data		2-D array[nx][nt] of smoothed data
******************************************************************************/
{
	int ix,it;			/* loop counters */	
	float **temp;			/* temporary (scratch) array */
	
	/* allocate temporary space */
	temp=alloc2float(nx,nt);

	/* smooth over time */
	if (nst>0 && nt>1) {
		
		/* apply 1-D gaussian smoothing over time */
		for (ix=0; ix<nx; ix++)
			 gaussian1d_smoothing (nt, nst, data[ix]);
	}

	/*out1=fopen ("data/Stoz49","w");
	fwrite (data[49],sizeof(float),nt,out1);
	fclose(out1);*/

	/* if parameters are not reasonable, return */
	if (nsx<=0 || nx<=1) return;
	
	/* smooth over traces */
	for (it=0; it<nt; it++) {

		/* copy time smoothed data to temporary array */
		for (ix=0; ix<nx; ix++) temp[it][ix]=data[ix][it];

		/*if (it==300) {
			out1=fopen ("data/xoz300","w");
			fwrite(temp[300],sizeof(float),nx,out1);
			fclose(out1);
		}*/
	
		/* apply 1-D gaussian smoothing over traces */
		gaussian1d_smoothing (nx, nsx, temp[it]);

		/*if (it==300) {
			out1=fopen ("data/Sxoz300","w");
			fwrite(temp[300],sizeof(float),nx,out1);
			fclose(out1);
		}*/

		/* copy smoothed data back to output array */
		for (ix=0; ix<nx; ix++) data[ix][it]=temp[it][ix];
	}
	
	/* free allocated space */
	free2float(temp);
}

/******************************************************************************

	Subroutine to apply a one-dimensional gaussian smoothing 

******************************************************************************/
void gaussian1d_smoothing (int ns, int nsr, float *data)
/******************************************************************************
Input:
ns		number of samples in the input data 
nsr		width (in samples) of the gaussian for which 
		amplitude > 0.5*max amplitude
data		1-D array[ns] of data to smooth

Output:
data		1-D array[ns] of smoothed data
******************************************************************************/
{
	int is;				/* loop counter */
	float sum=0.0;
	float fcut;
	float r;
	float fcutr=1.0/nsr;
	static int n;
	static int mean;
	static float fcutl;
	static float s[401];		/* smoothing filter array */
	float *temp;			/* temporary array */

	/* allocate space */
	temp=alloc1float(ns);

	/* save input fcut */
	fcut=fcutr;

	/* don't smooth if nsr equal to zero */
	if (nsr==0 || ns<=1) return;

	/* if halfwidth more than 100 samples, truncate */
	if (nsr>100) fcut=1.0/100;

	/* initialize smoothing function if not the same as the last one used */
	if (fcut !=fcutl) {
		fcutl=fcut;

		/* set span of 3, at width of 1.5*exp(-PI*1.5**2)=1/1174 */
		n=3.0/fcut+0.5;
		n=2*n/2+1;		/* make it odd for symmetry */

		/* mean is the index of the zero in the smoothing wavelet */
		mean=n/2;

		/* s(n) is the smoothing gaussian */
		for (is=1; is<=n; is++) {
			r=is-mean-1;
			r= -r*r*fcut*fcut*PI;
			s[is-1]=exp(r);
		}
			
		/* normalize to unit area, will preserve DC frequency at full 
		amplitude. Frequency at fcut will be half amplitude */
		for (is=0; is<n; is++) sum +=s[is];
		for (is=0; is<n; is++) s[is] /=sum;
	}

	/* convolve by gaussian into buffer */
	if (1.01/fcutr>(float)ns) {

		/* replace drastic smoothing by averaging */
		sum=0.0;
		for (is=0; is<ns; is++) sum +=data[is];
		sum /=ns;
		for (is=0; is<ns; is++) data[is]=sum;	

	} else {

		/* convolve with gaussian */
		convolve_cwp (n, -mean, s, ns, -mean, data, ns, -mean, temp);

		/* copy filtered data back to output array */
		for (is=0; is<ns; is++) data[is]=temp[is];
	}

	/* free allocated space */
	free1float(temp);
}

/******************************************************************************

	Subroutine to apply gaussian smoothing to a histogram

******************************************************************************/
void smooth_histogram (int nintlh, float *pdf)
/******************************************************************************
Input:
nintlh		number of histogram bins
pdf		1-D array[nintlh] of input histogram to be smoothed

Output:
pdf		1-D array[nintlh] of smoothed histogram
******************************************************************************/
{
	int i;			/* loop counter */
	int ng=11;		/* number of samples in smoothing filter */
	int mg=6;		/* index of sample with zero lag */
	float rv=1.0;		/* ?? */
	float r;		/* auxiliary variable */
	float sum=0.0;		/* auxiliary variable */
	float *filter;		/* gaussian filter */
	float *spdf;		/* smoothed histogram */

	/* allocate working space */
	filter=alloc1float(ng);
	spdf=alloc1float(nintlh);

	/* compute gaussian filter */	
	for (i=1; i<=ng; i++) {
	
		r=i-mg;
		r= -r*r/rv/rv*PI;
		filter[i-1]=exp(r);	
	}

	/* apply filter */
	convolve_cwp (ng, -mg+1, filter, nintlh, 0, pdf, nintlh, 0, spdf);

	/* normalize output histogram */
	for (i=0; i<nintlh; i++) sum +=spdf[i]; 
	for (i=0; i<nintlh; i++) pdf[i]=spdf[i]/sum; 

	/* free allocated space */
	free1float(filter);
	free1float(spdf);
}

void smooth_segmented_array(float *index,float *val,int n,int sm,int inc,int m)
/***********************************************************************
smooth_segmented_array - Smooth a segmented array
***********************************************************************
Input:
index		float array of indexes
val		float array of values
n		number of elements in arrays
sm		smoothing factor
inc		normal increment of index, if 
			index[i]-index[i-1] > inc then new segment
m		method of smoothing

        1 Gaussian 1d
        2 damped least squares
        3 Sawitzky-Golay
        4 Running average
        5 Running average with triangluar window
        6 median filter

***********************************************************************
Author: Potash Corporation: Balasz Nemeth
Code given to CWP in 2008.
***********************************************************************/
{
	float *p=NULL;
	float *t=NULL;
	float *ts=NULL;
	int *ind=NULL;
	int s1;		/* segment start */
	int s2=0;		/* segment end */
	int si=0;		/* increment */
	int sq=0;		/* segment number */
	int ns;		/* datapoints in segment */
	float *filter=NULL;
	float *data=NULL;
	
	p=ealloc1float(n);
	t=ealloc1float(n);
	ts=ealloc1float(n);
	ind=ealloc1int(n);
	
	{ register int i;
		for(i=0;i<n;i++){
			p[i]=index[i];
			t[i]=val[i];
			ind[i]=i;
		}
	}
	
	qkisort(n,p,ind);	
	
	s1=0;
	while(s2<n-1) {
		ns=1;
		si=1;
		s2=s1+si;
		while(p[ind[s1]]+(float)si*inc==p[ind[s2]]) {
			si++;
			s2=s1+si;
			ns++;
			if(ns==n) break;
		}
		{ register int i,j;
			for(i=s1,j=0;i<s2;i++,j++)
				ts[j]=t[ind[i]];
			/* smoothing */
			switch (m) {
			/* Gaussian */
			case 1 :
				gaussian1d_smoothing (ns,sm,ts);
			break;
			/* damped least squares */
			case 2 :
				dlsq_smoothing(ns,1,0,1,0,ns,sm,20,0,(float**)ts);
			break;
			/* Sawitzky-Golay */ 
			case 3 :
				filter=ealloc1float(2*sm+1);
				data=ealloc1float(ns);
				memcpy((void*) data,(const void *) ts,ns*FSIZE);
				SG_smoothing_filter(2*sm+1,sm,sm,0,4,filter);
				convolve_cwp (2*sm+1,-sm,filter,ns,0,data,ns,0,ts);
				free1float(filter);
				free1float(data);
			break;
			/* Running average */
			case 4 :
				filter=ealloc1float(2*sm+1);
				data=ealloc1float(ns);
				memcpy((void*) data,(const void *) ts,ns*FSIZE);
				rwa_smoothing_filter (1,sm,sm,filter);
				convolve_cwp (2*sm+1,-sm,filter,ns,0,data,ns,0,ts);
				free1float(data);
				free1float(filter);
			break;
			/* Running average triangular window*/
			case 5 :
				filter=ealloc1float(2*sm+1);
				data=ealloc1float(ns);
				memcpy((void*) data,(const void *) ts,ns*FSIZE);
				rwa_smoothing_filter (2,sm,sm,filter);
				convolve_cwp (2*sm+1,-sm,filter,ns,0,data,ns,0,ts);
				free1float(data);
				free1float(filter);
			break;
			/* median filter*/
			case 6 :
				data = ealloc1float(ns);
				if(!ISODD(sm)) sm++;
				filter = ealloc1float(sm);
		
				memcpy((void *) data,(const void *) ts,ns*FSIZE);
				
				{ int it,fl,*index,is,ifl;
				  float *sign;
					sign = ealloc1float(sm);
					index = ealloc1int(sm);
					fl=sm;
					/* No filtering before filter half length distance */
					for(it=fl/2,is=0;it<ns-(fl/2);it++,is++) {
						for(ifl=0;ifl<fl;ifl++) {
							index[ifl]=ifl;
							sign[ifl] = SGN(data[is+ifl]); 
							filter[ifl] = fabs(data[is+ifl]);
						}
						qkisort(fl,filter,index);
						ts[it] = sign[index[fl/2+1]]*filter[index[fl/2+1]];
					}
					free1float(sign);
					free1int(index);
				}
				free1float(data);
				free1float(filter);
				
			break;
			
			default :
				warn(" Non existing filter mode %d\n",m);
			break;
			
			}
				 	
			for(i=s1,j=0;i<s2;i++,j++)
				t[ind[i]]=ts[j];
		}
		/*fprintf(stderr," Segment # %d",sq);
		fprintf(stderr," %10.3f %10.3f %d\n",p[ind[s1]],p[ind[s2-1]],ns); */
		sq++;
		s1=s2;
	}
	
	{ register int i;
		for(i=0;i<n;i++)
			val[i]=t[i];
	}
	free1float(p);
	free1float(t);
	free1float(ts);
	free1int(ind);
}

void smooth_1(float *x, float *z, float r, int n)
/**************************************************************************
smooth_1  --- damped least squares smoothing of nonuniformly sampled 1d array
**************************************************************************
Input:
	x     array of horizontal coordinates
	z     array of vertical coordinate
	r     smoothing operator length
	n     number of points input 
Output:
	z     array of smoothed vertical coordinates
**************************************************************************
Reference:
    Liu, Zhenyue, 1994, Velocity smoothing: theory and implementation, 
    Project Review, 1994, Consortium Project on Seismic Inverse Methods
    for Complex Stuctures (in review).
**************************************************************************
Author: CWP: Zhenyue Liu, 1994
**************************************************************************/
{
	int  ir, i;		
	float *d, *e, *o;

	o = alloc1float(n+1);
	d = alloc1float(n);
	e = alloc1float(n);

	for(i=0; i<n-1; ++i){
		if(x[i+1]==x[i])
 			err("x-spacing of data values must not be zero!\n");
		o[i] = 1.0/(x[i+1]-x[i]);
		o[i] = o[i]*o[i];
	}
	o[n-1] = 0.;
		
	/* scale smoothing parameter */
	r = 0.5*r*r;
	r /= 5.19*5.19;

	for(ir=0; ir<2; ++ir) {
		for(i=1; i<n; ++i){
			d[i] = 1.0+r*(o[i]+o[i-1]);
			e[i-1] = -r*o[i-1];
		}
		d[0] = 1.0+r*o[0];
         	tripd(d,e,z,n);
	}

	free1float(d);
	free1float(e);
	free1float(o);
}

