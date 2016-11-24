/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/***************************************************************************
WAVETRANS2 - 2D wavelet transform by tensor-product of two 1D transforms

****************************************************************************
Function Prototype:
int waveTrans_2(float **x, float **y, waveFilter *filter, 
	int twopow1, int twopow2, int stage1, int stage2, int type);

****************************************************************************
Input:
x	array[][] for the signal
y 	array[][] for the wavelet coefficients
filter	wavelet filter structure
twopow1	2^pow1 is the length of the signal along 1st dimension
twopow2	2^pow2 is the length of the signal along 2nd dimension
stage1   stage of decomposition along 1st dimension 
stage2   stage of decomposition along 2nd dimension 
type	0 for decomposition, 1 for reconstruction

****************************************************************************
Author: 	Tong Chen, 04/12/95
***************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"


/* function used internally */
static int waveTrans_row(float **x, float **y, waveFilter *filter, 
        int twopow1, int twopow2, int stage, int type);

int waveTrans_2(float **x, float **y, waveFilter *filter, 
	int twopow1, int twopow2, int stage1, int stage2, int type)
/***************************************************************************
2D wavelet transform by tensor-product of two 1D transforms
****************************************************************************
x	array[][] for the signal
y 	array[][] for the wavelet coefficients
filter	wavelet filter structure
twopow1	2^pow1 is the length of the signal along 1st dimension
twopow2	2^pow2 is the length of the signal along 2nd dimension
stage1   stage of decomposition along 1st dimension 
stage2   stage of decomposition along 2nd dimension 
type	0 for decomposition, 1 for reconstruction
****************************************************************************
Author: 	Tong Chen, 04/12/95
***************************************************************************/
{
	int n1, n2;
	int i;
	float **z;

	if(stage1>twopow1 || stage2>twopow2) return 0;

	/* lengths of the signal */
	n1 = 1<<twopow1;
	n2 = 1<<twopow2;

	/* allocate space */
	z = alloc2float(n1, n2);

	if(!type){
	    for(i=0; i<n2; i++)
	    	waveTrans_1(x[i], z[i], filter, twopow1, stage1, type);

	    waveTrans_row(z, y, filter, twopow1, twopow2, stage2, type);

	} else {
	    for(i=0; i<n2; i++)
	    	waveTrans_1(z[i], y[i], filter, twopow1, stage1, type);

	    waveTrans_row(x, z, filter, twopow1, twopow2, stage2, type);
	}

	free2float(z);
	return EXIT_SUCCESS;
}


static int waveTrans_row(float **x, float **y, waveFilter *filter, 
        int twopow1, int twopow2, int stage, int type)
{
	int n1, n2, nhalf, nmid, nmidh, len, ishift, pos;
	int i, ii, j, jj, k, ilevel, ifirst, ofirst;
	float **tmp, *h, *g;

	if(stage>twopow2) return 0;

	/* length of the signal */
	n1 = 1<<twopow1;
	n2 = 1<<twopow2;

	/* if stage=0, just copy */
	if(stage==0) {
	    if(!type)
	       for(i=0; i<n2; i++)
		  for(ii=0; ii<n1; ii++)
		     y[i][ii] = x[i][ii];
	    else 
	       for(i=0; i<n2; i++)
		  for(ii=0; ii<n1; ii++)
		     x[i][ii] = y[i][ii];

    	    return 1;
    	}

	len = filter->len;  
	ishift = filter->ishift;  
	h = filter->filterh;
	g = filter->filterg;

	nhalf = n2>>1;
	tmp = alloc2float(n1, n2);

	/* decomposition */
	if(!type){

	    pos = n2;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    /* first stage */
	    for(j=0;j<nhalf;j++){
		  
		  for(ii=0; ii<n1; ii++){
		     tmp[j][ii] = 0.;
		     y[j][ii] = 0.;
		  }
		  

		  for(i=1;i<len;i+=2){	
		    k = (i+2*j-1+pos)%n2; 
		    for(ii=0; ii<n1; ii++){
			  tmp[j][ii] += x[k][ii]*h[i-1] + x[k+1][ii]*h[i];
			  y[j][ii] += x[k][ii]*g[i-1] + x[k+1][ii]*g[i];
		       }
		 }
	    }

	    /* later stages */
	    for(ilevel=1;ilevel<stage;ilevel++){

		nmid = twopow2 - ilevel;	
		nmid = 1<<nmid;
		nmidh = nmid>>1;
		ifirst = n2 - (nmid << 1);
		ofirst = ifirst + nmid;
		pos = nmid;
		while(pos < abs(ishift)) pos <<= 1;
		pos += ishift;

		for(j=0,jj=ofirst;j<nmidh;j++,jj++){
	
		      for(ii=0; ii<n1; ii++){
			    tmp[jj][ii] = 0.;
			    y[jj][ii] = 0.;
			 }
		      
		      for(i=1;i<len;i+=2){
			    k = (i+2*j-1+pos)%nmid+ifirst;
			    for(ii=0; ii<n1; ii++){
				  tmp[jj][ii] += tmp[k][ii]*h[i-1]+tmp[k+1][ii]*h[i];
				  y[jj][ii] += tmp[k][ii]*g[i-1]+tmp[k+1][ii]*g[i];
			       }
		      }
		}

	    }

	    /* copy the average */
	    nmid = twopow2 - stage;
	    nmid = 1<<nmid;
	    ifirst = n2 - (nmid << 1);
	    ofirst = n2 - nmid;

	    for(i=0,jj=ifirst,j=ofirst;i<nmid;i++,j++,jj++) 
	       for(ii=0; ii<n1; ii++)
		    y[j][ii] = tmp[jj][ii];
	}

	/* reconstruction */
	else{ 

	    /* last stage */
	    for(i=0;i<n2;i++) 
	       for(ii=0; ii<n1; ii++)
		  x[i][ii] = y[i][ii];

	    /* early stages */
	    for(ilevel=stage;ilevel>0;ilevel--){

		nmid = twopow2 - ilevel;
		nmid = 1<<nmid;
		nmidh = nmid<<1; 	
		pos = nmidh;
		while(pos < abs(ishift)) pos <<= 1;
		pos += ishift;
		ifirst = n2 - nmidh;

		for(i=0,j=ifirst;i<nmidh;i++,j++)
		{
		   for(ii=0; ii<n1; ii++){
			 tmp[j][ii] = x[j][ii];
			 x[j][ii] = 0.;
		      }
		}
		
		for(j=0,jj=ifirst;j<nmid;j++,jj++)
		   for(i=0;i<len;i++){
			 k = (2*j+i+pos)%nmidh+ifirst; 
			 for(ii=0; ii<n1; ii++)
			    x[k][ii] += tmp[jj][ii]*g[i] + tmp[jj+nmid][ii]*h[i];
		      }
	     }
	}

	free2float(tmp);
    	return 1;
}
