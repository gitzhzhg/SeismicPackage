
/* WPC1TRANS: $Revision: 1.2 $ ; $Date: 1997/01/10 18:39:58 $	*/

/*********************** self documentation **********************/
/*************************************************************************
WPC1TRANS - routines to perform a 1D wavelet packet transform 

wpc1Trans - decompose/reconstruct a signal into/from its wavelet packet
		coefficients
*************************************************************************
Function Prototype:
void wpc1Trans(float *x, void *inconf, int type);

*************************************************************************
Author:		Tong Chen, 08/04/94
*************************************************************************/
/**************** end self doc ********************************/


#include "wpc1.h"
#include "wpc1lib.h"

/* structure definitions only used here */
typedef struct waveFilterStruct{
	int len;
	int ishift;
	float *filterh;
	float *filterg;
} waveFILTER;

/* prototype of function used internally */
void wavePack1(float *x, float *y, waveFILTER *filter, 
	int twopow, int stage, int type);

void wpc1Trans(float *x, void *inconf, int type)
/***************************************************************************
1D wavelet packet transform 
****************************************************************************
x		array[] for the signal
inconf		configuration 
type		0 for decomposition, 1 for reconstruction
****************************************************************************
Author:		Tong Chen, 08/04/94
***************************************************************************/
{
	
	wpc1CONFIG *config = (wpc1CONFIG *) inconf;
	waveFILTER *filter;
	int twopow, stage;
	int n, i;
	float *z;

	twopow = config->powt; 
	n = config->tileszt;
	stage = config->staget;

	filter = (waveFILTER *) malloc(sizeof(waveFILTER));

	filter->len =  config->len;
	filter->ishift = config->ishift;
	filter->filterh = config->filterh; 
	filter->filterg = config->filterg; 

	z = alloc1float(n);

	if(!type)
	    wavePack1(x,z,filter,twopow,stage,type);
	else 
	    wavePack1(z,x,filter,twopow,stage,type);

	/* copy the data */
	for(i=0; i<n; i++) x[i] = z[i];

	free((void *) z);
	free((void *) filter);
}


void wavePack1(float *x, float *y, waveFILTER *filter, 
	int twopow, int stage, int type)
/***************************************************************************
1D wavelet packet transform
****************************************************************************
x	array[] for the signal
y 	array[] for the wavelet coefficients
filter	wavelet filter structure
twopow	2^pow is the length of the signal 
stage	stage of decomposition
type	0 for decomposition, 1 for reconstruction
****************************************************************************
Author: 	Tong Chen, 05/25/94
***************************************************************************/
{
	int n, nhalf, nmid, nmidh, len, ishift, nband, iband, mask, pos;
	int i, j, jj, k, ilevel, ifirst, ofirst;
	float *tmp, *h, *g;


	/* length of the signal */
	n = 1<<twopow;

	if(stage==0) {
	    if(!type) 
		for(i=0; i<n; i++)
		    y[i] = x[i];

	    else 
		for(i=0; i<n; i++)
		    x[i] = y[i];
    	}

	len = filter->len;  
	ishift = filter->ishift;  
	h = filter->filterh;
	g = filter->filterg;

	nhalf = n>>1;
	tmp = alloc1float(n);

	/* decomposition */
	if(!type){

	    mask = n - 1;
	    pos = n;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    /* first stage */
	    for(j=0;j<nhalf;j++){

		tmp[j] = 0.;
		y[j] = 0.;

	    	for(i=1;i<len;i+=2){	
/*
		    k = (i+2*j-1)%n; 
*/
		    k = (i+2*j-1+pos) & mask; 
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
		mask = nmid - 1;
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
/*
			    k = (i+2*j-1)%nmid+ifirst;
*/
			    k = ((i+2*j-1+pos) & mask)+ifirst;
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
/*
			    k = (i+2*j-1)%nmid+ifirst;
*/
			    k = ((i+2*j-1+pos) & mask)+ifirst;
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
		mask = nmidh - 1;
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
/*
			    k = (2*j+i)%nmidh+ifirst; 
*/
			    k = ((2*j+i+pos) & mask)+ifirst; 
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
	    mask = n - 1;
	    pos = n;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    for(j=0,jj=nhalf;j<nhalf;j++,jj++)
	    	for(i=0;i<len;i++){
/*
		    k = (2*j+i)%n; 
*/
		    k = (2*j+i+pos) & mask; 
		    x[k] += tmp[j]*g[i] + tmp[jj]*h[i];
	    	}
	    
	}

	free1float(tmp);
}
