/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* WPCPACK2: $Revision: 1.4 $ ; $Date: 2011/11/21 16:24:37 $	*/


/*********************** self documentation **********************/
/*************************************************************************
WPCPACK2 - routine to perform a 2D wavelet packet transform 

wavePack2	decompose/reconstruct a 2D signal into/from its wavelet
		packet coefficients

**************************************************************************
Function Prototype:
void wavePack2(float **x, float **y, void *inconf, int type);

**************************************************************************
wavePack2:
Input:
x		array[][] for the signal
y 		array[][] for the wavelet coefficients
inconf		configuration 
type		0 for decomposition, 1 for reconstruction

**************************************************************************
Author:		Tong Chen, 05/25/94
Modifier: 	Tong Chen, 07/28/94, for API
*************************************************************************/
/**************** end self doc ********************************/

#include "wpc.h"
#include "wpclib.h"

/* this is a crude version which requires the extra storage of 
two 2D arrays, will be revised */

/* structure definitions only used here */
typedef struct waveFilterStruct{
	int len;
	int ishift;
	float *filterh;
	float *filterg;
} waveFILTER;


/* routines used internally */
static void wavePack_row(float **x, float **y, waveFILTER *filter,
        int twopow1, int twopow2, int stage, int type);

void wavePack1(float *x, float *y, waveFILTER *filter, 
	int twopow, int stage, int type);

void waveReorder(float **x, float **y, int twopow1, int twopow2, 
	int stage1, int stage2, int type);


void wavePack2(float **x, float **y, void *inconf, int type)
/***************************************************************************
2D wavelet packet transform 
****************************************************************************
x		array[][] for the signal
y 		array[][] for the wavelet coefficients
inconf		configuration 
type		0 for decomposition, 1 for reconstruction
****************************************************************************
Author:		Tong Chen, 05/25/94
Modifier: 	Tong Chen, 07/28/94, for API
***************************************************************************/
{
	
	wpcCONFIG *config = (wpcCONFIG *) inconf;
	waveFILTER *filter1, *filter2;
	int twopow1, twopow2, stage1, stage2;
	int n1, n2, i;
	float **z;

	twopow1 = config->powt; 
	twopow2 = config->powx; 
	n1 = config->tileszt;
	n2 = config->tileszx;
	stage1 = config->staget;
	stage2 = config->stagex;

	filter1 = (waveFILTER *) malloc(sizeof(waveFILTER));
	filter2 = (waveFILTER *) malloc(sizeof(waveFILTER));

	filter1->len =  config->len;
	filter2->len = config->len;
	filter1->ishift = config->ishift;
	filter2->ishift = config->ishift;
	filter1->filterh = config->filterh; 
	filter2->filterh = config->filterh; 
	filter1->filterg = config->filterg; 
	filter2->filterg = config->filterg; 

	z = alloc2float(n1,n2);

	if(!type){

	/* first transform along the faster dimension */
	    for(i=0;i<n2;i++)
		wavePack1(x[i],z[i],filter1,twopow1,stage1,type);

	/* then along the slower dimension */
	    wavePack_row(z,x,filter2,twopow1,twopow2,stage2,type);

	/* reorder the coefficients */
	    waveReorder(x, y, twopow1, twopow2, stage1, stage2, type);
	
	}
	
	else{

	/* reorder the coefficients */
	    waveReorder(x, y, twopow1, twopow2, stage1, stage2, type);

	/* first transform along the faster dimension */
	    for(i=0;i<n2;i++)
		wavePack1(z[i],x[i],filter1,twopow1,stage1,type);

	/* then along the slower dimension */
	    wavePack_row(x,z,filter2,twopow1,twopow2,stage2,type);
	}
	
	free2float(z);
	free((void *) filter1);
	free((void *) filter2);
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


static void wavePack_row(float **x, float **y, waveFILTER *filter,
	int twopow1, int twopow2, int stage, int type)
/***************************************************************************
wavelet packet transform along the slower dimension
****************************************************************************
x		array[][] for the signal
y 		array[][] for the wavelet coefficients
filter		wavelet filter structure 
twopow1		2^pow is the length of the signal for the faster dimension 
twopow2		2^pow is the length of the signal for the slower dimension 
stage		stage of decomposition for the faster dimension
type		0 for decomposition, 1 for reconstruction
****************************************************************************
Author:		Tong Chen, 05/25/94
***************************************************************************/
{ 
	int n1, n2, nhalf, nmid, nmidh, len, ishift, mask, pos;
	int i, j, jj, k, l, ilevel, ifirst, ofirst, nband, iband;
	float **tmp, *h, *g;

	/* length of the signal */
	n1 = 1<<twopow1;
	n2 = 1<<twopow2;

	if(stage==0){

		if(!type) 
		    for(i=0; i<n2; i++)
		    	for(j=0; j<n1; j++)
			    y[i][j] = x[i][j];
		else 
		    for(i=0; i<n2; i++)
		    	for(j=0; j<n1; j++)
			    y[i][j] = x[i][j];
	}

	len = filter->len;  
	ishift = filter->ishift;  
	h = filter->filterh;
	g = filter->filterg;

	nhalf = n2>>1;

	tmp = alloc2float(n1,n2);

	/* decomposition */
	if(!type){

	    mask = n2 - 1;
	    pos = n2;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    /* first stage */
	    for(j=0;j<nhalf;j++){

		for(l=0;l<n1;l++){
		    tmp[j][l] = 0.;
		    y[j][l] = 0.;
		}

	    	for(i=1;i<len;i+=2){	
/*
		    k = (i+2*j-1)%n2; 
*/
		    k = (i+2*j-1+pos) & mask; 
		    for(l=0;l<n1;l++){
		    	tmp[j][l] += x[k][l]*h[i-1] + x[k+1][l]*h[i];
		    	y[j][l] += x[k][l]*g[i-1] + x[k+1][l]*g[i];
		    }
		}
	    }

	    /* later stages */
	    for(ilevel=1;ilevel<stage;ilevel++){

		nband = 1<<ilevel;
		nband = nband>>1;
		nmid = twopow2-ilevel;	
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
	
			for(l=0;l<n1;l++){
		    	    tmp[jj][l] = 0.;
		    	    y[jj][l] = 0.;
			}

		        for(i=1;i<len;i+=2){
/*
			    k = (i+2*j-1)%nmid+ifirst;
*/
			    k = ((i+2*j-1+pos) & mask) +ifirst;
			    for(l=0;l<n1;l++){ 
			    	tmp[jj][l] += tmp[k][l]*h[i-1]+tmp[k+1][l]*h[i];
			    	y[jj][l] += tmp[k][l]*g[i-1]+tmp[k+1][l]*g[i];
			    }
		   	}
		    }

		    /* difference */
		    ofirst += nmidh;
		    for(j=0,jj=ofirst;j<nmidh;j++,jj++){
	
			for(l=0;l<n1;l++){
		    	    tmp[jj][l] = 0.;
		    	    y[jj][l] = 0.;
			}

		        for(i=1;i<len;i+=2){
/*
			    k = (i+2*j-1)%nmid+ifirst;
*/
			    k = ((i+2*j-1+pos) & mask) +ifirst;
			    for(l=0;l<n1;l++){ 
			    	tmp[jj][l] += y[k][l]*g[i-1]+y[k+1][l]*g[i];
			    	y[jj][l] += y[k][l]*h[i-1]+y[k+1][l]*h[i];
			    }
		   	}
		    }

		    /* copy the difference */
		    for(j=0,i=ifirst,jj=ofirst;j<nmidh;j++,jj++,i++){
			for(l=0;l<n1;l++){
			    tmp[i][l] = tmp[jj][l];
			    y[i][l] = y[jj][l];
			}
		    }
		}
	    }

	    /* last */
	    nmid = twopow2 - stage;
	    nmid = 1<<nmid;
	    nmidh = nmid <<1;
	    nband = 1<<stage;
	    nband = nband>>1;

	    for(iband=0;iband<nband;iband++){

		ifirst = iband*nmidh;
		ofirst = ifirst+nmid;

	    	for(i=0,jj=ifirst,j=ofirst;i<nmid;i++,j++,jj++) 
		    for(l=0;l<n1;l++)
		    	y[j][l] = tmp[jj][l];
	    }
	}

	/* reconstruction */
	else{ 

	    /* last stage */
	    for(i=0;i<n2;i++) 
		for(l=0;l<n1;l++)
		   x[i][l] = y[i][l];

	    /* early stages */
	    for(ilevel=stage;ilevel>1;ilevel--){

		nmid = twopow2 - ilevel;
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
			    for(l=0;l<n1;l++)
			        tmp[j][l] = x[jj][l];
			for(i=0,j=ifirst,jj=ofirst;i<nmid;i++,j++,jj++)
			    for(l=0;l<n1;l++)
			        tmp[jj][l] = x[j][l];
		    }
		    else
			for(i=0,j=ifirst;i<nmidh;i++,j++)
			    for(l=0;l<n1;l++)
			        tmp[j][l] = x[j][l];
		    

		    for(i=0,j=ifirst;i<nmidh;i++,j++) 
		  	for(l=0;l<n1;l++)
		    	    x[j][l] = 0.;

		    for(j=0,jj=ifirst;j<nmid;j++,jj++)
		    	for(i=0;i<len;i++){
/*
			    k = (2*j+i)%nmidh+ifirst; 
*/
			    k = ((2*j+i+pos) & mask)+ifirst; 
			    for(l=0;l<n1;l++)
			    	x[k][l] += tmp[jj][l]*g[i] + 
					tmp[jj+nmid][l]*h[i];
		    	}
		}

	    }

	    /* first stage */
	    for(i=0;i<n2;i++)
		for(l=0;l<n1;l++){
		    tmp[i][l] = x[i][l];
		    x[i][l] = 0.;
	    	}

	    nhalf = n2>>1;
	    mask = n2 - 1;
	    pos = n2;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    for(j=0,jj=nhalf;j<nhalf;j++,jj++)
	    	for(i=0;i<len;i++){
/*
		    k = (2*j+i)%n2; 
*/
		    k = (2*j+i+pos) & mask; 
		    for(l=0;l<n1;l++)
		    	x[k][l] += tmp[j][l]*g[i] + tmp[jj][l]*h[i];
	    	}
	    
	}

	free2float(tmp);
}


/* reorder the wavelet coefficients */
void waveReorder(float **x, float **y, int twopow1, int twopow2, 
	int stage1, int stage2, int type)
{
	int i1, i2, j1, j2, ibeg1, iend1, ibeg2, iend2, ibeg, iblock;
	int nband1, nband2, lband1, lband2;

	nband1 = 1 << stage1;
	nband2 = 1 << stage2;
	lband1 = 1 << (twopow1 - stage1);
	lband2 = 1 << (twopow2 - stage2);

	if(!type){
	    for(j2=0, ibeg2=0; j2 < nband2; j2++){

	   	iend2 = ibeg2 + lband2;
	        for(j1=0, ibeg1=0; j1 < nband1; j1++){

	   	    iend1 = ibeg1 + lband1;
	   	    iblock = j2*nband1 + j1;

	    	    ibeg = 0;
                    for(i2=ibeg2;i2<iend2;i2++){
		    	for(i1=ibeg1;i1<iend1;i1++){
			    y[iblock][ibeg] = x[i2][i1];
		    	    ibeg ++;
		     	}
		    }

		    ibeg1 = iend1;
	     	}

	      	ibeg2 = iend2;
	    }
	}

	/* else */
	else {
	    for(j2=0, ibeg2=0; j2 < nband2; j2++){

	   	iend2 = ibeg2 + lband2;
	        for(j1=0, ibeg1=0; j1 < nband1; j1++){

	   	    iend1 = ibeg1 + lband1;
	   	    iblock = j2*nband1 + j1;

	    	    ibeg = 0;
                    for(i2=ibeg2;i2<iend2;i2++){
		    	for(i1=ibeg1;i1<iend1;i1++){
			    x[i2][i1] = y[iblock][ibeg];
		    	    ibeg ++;
		     	}
		    }

		    ibeg1 = iend1;
	     	}

	      	ibeg2 = iend2;
	    }
	}
}
