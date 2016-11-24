/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/***************************************************************************
WAVEPACK1 - 1D wavelet packet transform

****************************************************************************
Function Prototype:
int waveTrans_1(float *x, float *y, waveFilter *filter, 
	int twopow, int stage, int type);
****************************************************************************
Input:
x	array[] for the signal
y 	array[] for the wavelet coefficients
filter	wavelet filter structure
twopow	2^pow is the length of the signal
stage   stage of decomposition 
type	0 for decomposition, 1 for reconstruction
****************************************************************************
Author: 	Tong Chen, 04/05/95
***************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"


int waveTrans_1(float *x, float *y, waveFilter *filter, 
	int twopow, int stage, int type)
/***************************************************************************
1D wavelet packet transform
****************************************************************************
x	array[] for the signal
y 	array[] for the wavelet coefficients
filter	wavelet filter structure
twopow	2^pow is the length of the signal
stage   stage of decomposition 
type	0 for decomposition, 1 for reconstruction
****************************************************************************
Author: 	Tong Chen, 04/05/95
***************************************************************************/
{
	int n, nhalf, nmid, nmidh, len, ishift, pos;
	int i, j, jj, k, ilevel, ifirst, ofirst;
	float *tmp, *h, *g;

	if(stage>twopow) return 0;

	/* length of the signal */
	n = 1<<twopow;

	/* if stage=0, just copy */
	if(stage==0) {
	    if(!type)
	       for(i=0; i<n; i++)
		  y[i] = x[i];
	    else 
	       for(i=0; i<n; i++)
		  x[i] = y[i];
    	    return 1;
    	}

	len = filter->len;  
	ishift = filter->ishift;  
	h = filter->filterh;
	g = filter->filterg;

	nhalf = n>>1;
	tmp = alloc1float(n);

	/* decomposition */
	if(!type){

	    pos = n;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    /* first stage */
	    for(j=0;j<nhalf;j++){

		tmp[j] = 0.;
		y[j] = 0.;

	    	for(i=1;i<len;i+=2){	
		    k = (i+2*j-1+pos)%n; 
		    tmp[j] += x[k]*h[i-1] + x[k+1]*h[i];
		    y[j] += x[k]*g[i-1] + x[k+1]*g[i];
		}
	    }

	    /* later stages */
	    for(ilevel=1;ilevel<stage;ilevel++){

		nmid = twopow-ilevel;	
		nmid = 1<<nmid;
		nmidh = nmid>>1;
		ifirst = n - (nmid << 1);
		ofirst = ifirst + nmid;
		pos = nmid;
		while(pos < abs(ishift)) pos <<= 1;
		pos += ishift;

		for(j=0,jj=ofirst;j<nmidh;j++,jj++){
	
		      tmp[jj] = 0.;
		      y[jj] = 0.;

		      for(i=1;i<len;i+=2){
			    k = (i+2*j-1+pos)%nmid+ifirst;
			    tmp[jj] += tmp[k]*h[i-1]+tmp[k+1]*h[i];
			    y[jj] += tmp[k]*g[i-1]+tmp[k+1]*g[i];
		      }
		}

	    }

	    /* copy the average */
	    nmid = twopow - stage;
	    nmid = 1<<nmid;
	    ifirst = n - (nmid << 1);
	    ofirst = n - nmid;

	    for(i=0,jj=ifirst,j=ofirst;i<nmid;i++,j++,jj++) 
		    y[j] = tmp[jj];
	}

	/* reconstruction */
	else{ 

	    /* last stage */
	    for(i=0;i<n;i++) x[i] = y[i];

	    /* early stages */
	    for(ilevel=stage;ilevel>0;ilevel--){

		nmid = twopow - ilevel;
		nmid = 1<<nmid;
		nmidh = nmid<<1; 	
		pos = nmidh;
		while(pos < abs(ishift)) pos <<= 1;
		pos += ishift;
		ifirst = n - nmidh;

		for(i=0,j=ifirst;i<nmidh;i++,j++)
		{
		   tmp[j] = x[j];
		   x[j] = 0.;
		}
		
		for(j=0,jj=ifirst;j<nmid;j++,jj++)
		   for(i=0;i<len;i++){
			 k = (2*j+i+pos)%nmidh+ifirst; 
			 x[k] += tmp[jj]*g[i] + tmp[jj+nmid]*h[i];
		      }
	     }
	}

	free1float(tmp);
    	return 1;
}
