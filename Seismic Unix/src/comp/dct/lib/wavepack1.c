/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/***************************************************************************
WAVEPACK1 - 1D wavelet packet transform

wavepack1 - 1D wavelet package transform

****************************************************************************
Function Prototype:
int wavePack_1(float *x, float *y, waveFilter *filter, 
	int twopow, int stage, int type);

****************************************************************************
Input:
x	array[] for the signal
y 	array[] for the wavelet coefficients
filter	wavelet filter structure
twopow	2^pow is the length of the signal 
type	0 for decomposition, 1 for reconstruction

****************************************************************************
Author: 	Tong Chen, 05/25/94
***************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"

int wavePack_1(float *x, float *y, waveFilter *filter, 
	int twopow, int stage, int type)
/***************************************************************************
1D wavelet packet transform
****************************************************************************
x	array[] for the signal
y 	array[] for the wavelet coefficients
filter	wavelet filter structure
twopow	2^pow is the length of the signal 
type	0 for decomposition, 1 for reconstruction
****************************************************************************
Author: 	Tong Chen, 05/25/94
***************************************************************************/
{
	int n, nhalf, nmid, nmidh, len, ishift, nband, iband,  pos;
	int i, j, jj, k, ilevel, ifirst, ofirst;
	float *tmp, *h, *g;

	if(stage>twopow) return 0;

	/* length of the signal */
	n = 1<<twopow;

	/* if stage=0, no transform */
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

		nband = 1<<ilevel;
		nband = nband>>1;
		nmid = twopow-ilevel;	
		nmid = 1<<nmid;
		nmidh = nmid>>1;
		pos = nmid;
		while(pos < abs(ishift)) pos <<= 1;
		pos += ishift;

		for(iband=0;iband<nband;iband++){

		    ifirst = iband*nmid;
		    ifirst = ifirst<<1;
		    ofirst = ifirst + nmid;

		    /* average */
		    for(j=0,jj=ofirst;j<nmidh;j++,jj++){
	
		    	tmp[jj] = 0.;
		    	y[jj] = 0.;

		        for(i=1;i<len;i+=2){
			    k = (i+2*j-1+pos)%nmid+ifirst;
			    tmp[jj] += tmp[k]*h[i-1]+tmp[k+1]*h[i];
			    y[jj] += tmp[k]*g[i-1]+tmp[k+1]*g[i];
		   	}
		    }

		    /* difference */
		    ofirst += nmidh;
		    for(j=0,jj=ofirst;j<nmidh;j++,jj++){
	
		    	tmp[jj] = 0.;
		    	y[jj] = 0.;

		        for(i=1;i<len;i+=2){
			    k = (i+2*j-1+pos)%nmid+ifirst;
			    tmp[jj] += y[k]*g[i-1]+y[k+1]*g[i];
			    y[jj] += y[k]*h[i-1]+y[k+1]*h[i];
		   	}
		    }

		    /* copy the difference */
		    for(j=0,i=ifirst,jj=ofirst;j<nmidh;j++,jj++,i++){
			tmp[i] = tmp[jj];
			y[i] = y[jj];
		    }
		}
	    }

	    /* last */
	    nmid = twopow - stage;
	    nmid = 1<<nmid;
	    nmidh = nmid <<1;
	    nband = 1<<stage;
	    nband = nband>>1;

	    for(iband=0;iband<nband;iband++){

		ifirst = iband*nmidh;
		ofirst = ifirst+nmid;

	    	for(i=0,jj=ifirst,j=ofirst;i<nmid;i++,j++,jj++) 
		    y[j] = tmp[jj];
	    }
	}

	/* reconstruction */
	else{ 

	    /* last stage */
	    for(i=0;i<n;i++) x[i] = y[i];

	    /* early stages */
	    for(ilevel=stage;ilevel>1;ilevel--){

		nmid = twopow - ilevel;
		nmid = 1<<nmid;
		nmidh = nmid<<1; 	
		nband = 1<<ilevel;
		nband = nband>>1;
		pos = nmidh;
		while(pos < abs(ishift)) pos <<= 1;
		pos += ishift;
	
		for(iband=0;iband<nband;iband++){

		    ifirst = iband*nmidh;
		    ofirst = ifirst+nmid;
		    if(iband==((iband>>1) <<1)){
			for(i=0,j=ifirst,jj=ofirst;i<nmid;i++,j++,jj++)
			    tmp[j] = x[jj];
			for(i=0,j=ifirst,jj=ofirst;i<nmid;i++,j++,jj++)
			    tmp[jj] = x[j];
		    }
		    else
			for(i=0,j=ifirst;i<nmidh;i++,j++)
			    tmp[j] = x[j];
		    

		    for(i=0,j=ifirst;i<nmidh;i++,j++) 
		    	x[j] = 0.;

		    for(j=0,jj=ifirst;j<nmid;j++,jj++)
		    	for(i=0;i<len;i++){
			    k = (2*j+i+pos)%nmidh+ifirst; 
			    x[k] += tmp[jj]*g[i] + tmp[jj+nmid]*h[i];
		    	}
		}

	    }

	    /* first stage */
	    for(i=0;i<n;i++){
		tmp[i] = x[i];
		x[i] = 0.;
	    }

	    nhalf = n>>1;
	    pos = n;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    for(j=0,jj=nhalf;j<nhalf;j++,jj++)
	    	for(i=0;i<len;i++){
		    k = (2*j+i+pos)%n; 
		    x[k] += tmp[j]*g[i] + tmp[jj]*h[i];
	    	}
	    
	}

	free1float(tmp);
    	return 1;
}

