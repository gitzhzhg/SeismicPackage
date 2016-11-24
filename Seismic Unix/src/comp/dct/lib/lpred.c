/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

 
/*********************** self documentation **********************/
/**************************************************************************
LPRED - Lateral Prediction of Several Plane Waves

lpred - lateral prediction of several plane waves

***************************************************************************
Function Prototype:
void lpred(float **f, int n1, int n2, int lblock, int hlfilter, int nslope,
	   lpFilter *lpf, float **res);

***************************************************************************
Input:
f        array[][] of signal
n1       length of the signal along the 1st dimension
n2       length of the signal along the 2nd dimension
lblock   length of each block
hlfilter half-length of the filter
nslope   number of plane waves predicted
lpf      lateral prediction filter
res      residual of prediction

***************************************************************************
Author:        Tong Chen, 01/26/95
Modifier:      Tong Chen, 05/25/95, from single to multiple plane waves
**************************************************************************/

/**************** end self doc ********************************/
 
#include "comp.h"
#define ERR .0001


/* functions used internally */
static lpFilter *filtAlloc(int hlfilter, int nslope, int nblock);
static void cgsolver(float **a, float *rhs, int nrow, int ncol, 
		     int niter, float error, float *x);


void lpred(float **f, int n1, int n2, int lblock, int hlfilter, int nslope,
	   lpFilter *lpf, float **res)
/**************************************************************************
lateral prediction of several plane waves
***************************************************************************
f        array[][] of signal
n1       length of the signal along the 1st dimension
n2       length of the signal along the 2nd dimension
lblock   length of each block
hlfilter half-length of the filter
nslope   number of plane waves predicted
lpf      lateral prediction filter
res      residual of prediction
***************************************************************************
Author:        Tong Chen, 01/26/95
Modifier:      Tong Chen, 05/25/95, from single to multiple plane waves
**************************************************************************/
{
   int iblock,itrace,it,i,j,ifirst,ifilter;
   int nblock, ltrace, lpadded;
   int irow, icol, nrow, ncol, islope;
   int lfilter, niter;
   float *rhs, *tmp, *unknown, ***filter;
   float **paddedtrace, **xmatrics;
   float xabs, xmax, rscale;
   

   /* calculate the # of blocks, etc */
   nblock = (n1-1)/lblock+1;
   ltrace = nblock*lblock;
   lpadded = ltrace+2*hlfilter;

  
   /* allocate the prediction filter */
   lpf = filtAlloc(hlfilter, nslope, nblock);
   lfilter = lpf->lfilter;
   filter = lpf->filter;

   nrow = (n2-nslope)*lblock;
   ncol = lfilter*nslope;
   niter = ncol;
   
   /* allocate memory */
   rhs = alloc1float(nrow);
   paddedtrace = alloc2float(lpadded, n2);
   xmatrics = alloc2float(ncol, nrow);
   tmp = alloc1float(lblock);
   unknown = alloc1float(ncol);

   /* copy the data first */
   for(itrace=0; itrace<n2; itrace++)
   {
      for(it=0; it<hlfilter; it++)
	 paddedtrace[itrace][it] = 0.;
      for(it=hlfilter, i=0; i<n1; it++, i++)
	 paddedtrace[itrace][it] = f[itrace][i];
      for(it=n1+hlfilter; it<lpadded; it++)
	 paddedtrace[itrace][it] = 0.;
   }
   
   /* for each block */
   for(iblock=0, ifirst=0; iblock<nblock; iblock++, ifirst+=lblock)
   {
      
      /* form the matrix and perform an overall scaling */
      xmax = 0.;
      for(itrace=0; itrace<n2-nslope; itrace++)
	 for(islope=0; islope<nslope; islope++)
	    for(i=0; i<lblock; i++)
	       for(ifilter=0; ifilter<lfilter; ifilter++)
	       {

		  irow = itrace*lblock+i;
		  icol = islope*lfilter+ifilter;
		  j = itrace+islope;
		  it = ifirst+i+ifilter;
		  
		  xmatrics[irow][icol] = paddedtrace[j][it];

		  xabs = ABS(xmatrics[irow][icol]);
		  xmax = (xmax > xabs)? xmax : xabs;
	       }

      if(xmax < FLT_MIN)
      {
	 
	 for(islope=0; islope<nslope; islope++)
	    for(ifilter=0; ifilter<lfilter; ifilter++)
	       filter[iblock][islope][ifilter] = 0.;
	       
	 /* just copy the data */
	 for(itrace=0; itrace<n2; itrace++)
	    for(i=0, it=ifirst; i<lblock; i++, it++)
	       res[itrace][it] = f[itrace][it];
/*
	 fprintf(stderr,"zeroes \n");
*/
	 
	 continue;
	 
      }

      /* else */
      else
      {
	 
	 rscale = 1./xmax;
	 for(irow=0; irow<nrow; irow++)
	    for(icol=0; icol<ncol; icol++)
	       xmatrics[irow][icol] *= rscale;
      
	 /* form the RHS */
	 for(itrace=nslope; itrace<n2; itrace++)
	    for(i=0; i<lblock; i++)
	    {
	       it = ifirst+i;
	       irow = (itrace-nslope)*lblock+i;
	       rhs[irow] = rscale*f[itrace][it];
	    }

	 
	 /* solve for the filter, using CG */
	 cgsolver(xmatrics, rhs, nrow, ncol, niter, ERR, unknown);
      }
      
      /* obtain the filter */
      for(islope=0; islope<nslope; islope++)
	 for(ifilter=0; ifilter<lfilter; ifilter++)
	 {
	    icol = islope*lfilter+ifilter;
	    filter[iblock][islope][ifilter] = unknown[icol];
/*
	    fprintf(stderr,"filter[%d] = %f\n", ifilter, filter[iblock][islope][ifilter]);
*/
	    
	 }
      

      /* keep the base traces */
      for(islope=0; islope<nslope; islope++)
	 for(i=0; i<lblock; i++)
	 {
	    it = ifirst+i;
	    res[islope][it] = f[islope][it];
	 }

/*
      for(i=0; i<lblock; i++)
      {
	 it = ifirst+i;
	 res[0][it] = f[0][it];
      }

      for(islope=1; islope<nslope; islope++)
	 for(i=0; i<lblock; i++)
	 {
	    it = ifirst+i;
	    res[islope][it] = f[islope][it] - f[islope-1][it];
	 }
*/
      

      /* apply the filter to the later traces and form residuals */
      for(itrace=nslope; itrace<n2; itrace++)
      {

	 j = itrace-nslope;
	 
	 /* the predicted part */
	 for(i=0; i<lblock; i++)
	 {
	    tmp[i] = 0;

	    for(islope=0; islope<nslope; islope++)
	       for(ifilter=0; ifilter<lfilter; ifilter++)
	       {
		  it = i+ifirst+ifilter;
		  tmp[i] += paddedtrace[j+islope][it]*filter[iblock][islope][ifilter];
	       }
	 }
	 
	 for(i=0, it=ifirst; i<lblock; i++, it++)
	    res[itrace][it] = tmp[i];
	    
	 /* the residual */
	 /* use predicted instead 
*/
	 for(i=0, it=ifirst; i<lblock; i++, it++)
	    res[itrace][it] = f[itrace][it] - res[itrace][it];
      }
   }


   /* free-up the spaces */
   free1float(rhs);
   free2float(paddedtrace);
   free2float(xmatrics);
   free1float(tmp);
   free1float(unknown);
   
}


static lpFilter *filtAlloc(int hlfilter, int nslope, int nblock)
{

   lpFilter *lpf;
   int lfilter;
   
   lpf = (lpFilter *) malloc(sizeof(lpFilter));
   
   /* the parameters */
   lfilter = 2*hlfilter + 1;
   lpf->hlfilter = hlfilter;
   lpf->lfilter = lfilter;
   lpf->nslope = nslope;
   lpf->nblock = nblock;
   lpf->filter = alloc3float(lfilter, nslope, nblock);
   
   return (lpf);
}


static void cgsolver(float **a, float *rhs, int nrow, int ncol, 
		     int niter, float error, float *x)
/***************************************************************************
least-squares linear system solver using CG
****************************************************************************
a        array[][] of the matrix
rhs      array[] of the righ hand side
nrow     number of rows
ncol     number of columns
niter    number of CG iterations
error      thresholding relative error
x        least-squares solution
***************************************************************************/
{
   float *s, *r, *p, *q, *rn;
   int i, j, icount;
   float alpha, beta;
   float rsum, qsum, rnsum, r0sum;
   

   /* allocate spaces */
   s = alloc1float(nrow);
   r = alloc1float(ncol);
   p = alloc1float(ncol);
   q = alloc1float(nrow);
   rn = alloc1float(ncol);

   /* initialize the vectors */

   /* x = 0 */
   for(i=0; i<ncol; i++) x[i] = 0.;
   
   /* s = rhs */
   for(i=0; i<nrow; i++) s[i] = rhs[i];

   /* p = r = A^T s*/
   for(i=0; i<ncol; i++) p[i] = 0.;
   for(j=0; j<nrow; j++)
      for(i=0; i<ncol; i++)
	 p[i] += a[j][i]*s[j];
   for(i=0; i<ncol; i++) r[i] = p[i];
   
   /* q = A p */
   for(i=0; i<nrow; i++)
   {
      q[i] = 0.;
      for(j=0; j<ncol; j++)
	 q[i] += a[i][j]*p[j];
   }

   for(i=0, rsum=0.; i<ncol; i++) rsum += r[i]*r[i];
   r0sum = rsum*error;
   
   /* start iteration */
   for(icount=0; icount < niter && rsum>r0sum; icount++)
   {
      for(i=0, qsum=0.; i<nrow; i++) qsum += q[i]*q[i];
      
      /* alpha = r.r/q.q */
      alpha = rsum/qsum;

      /* x = x + alpha*p, s = s - alpha*q */
      for(i=0; i<ncol; i++) x[i] += alpha*p[i];
      for(i=0; i<nrow; i++) s[i] -= alpha*q[i];
      
      /* rn = A^T s */
      for(i=0; i<ncol; i++) rn[i] = 0.;
      for(j=0; j<nrow; j++)
	 for(i=0; i<ncol; i++)
	    rn[i] += a[j][i]*s[j]; 

      for(i=0, rnsum=0.; i<ncol; i++) rnsum += rn[i]*rn[i];
      
      /* beta = rn.rn/r.r */
      beta = rnsum/rsum;
      
      /* p = rn + beta*p */
      for(i=0; i<ncol; i++) p[i] = rn[i] + beta*p[i];

      /* q = A p */
      for(i=0; i<nrow; i++)
      {
	 q[i] = 0.;
	 for(j=0; j<ncol; j++)
	    q[i] += a[i][j]*p[j];
      }      

      /* r = rn */
      for(i=0; i<ncol; i++) r[i] = rn[i];
      rsum = rnsum;
   }

   /* free the spaces */
   free1float(s);
   free1float(r);
   free1float(p);
   free1float(q);
   free1float(rn);

}   
