/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/***************************************************************************
WAVEPACK2 - 2D Wavelet PACKet transform 

****************************************************************************
Function Prototype:
int wavePack_2(float **x, float **y, waveFilter *filter1, 
	waveFilter *filter2, int twopow1, int twopow2,
	int stage1, int stage2, int type);

****************************************************************************
Input:
x		array[][] for the signal
y 		array[][] for the wavelet coefficients
filter1		wavelet filter structure for the faster dimension
filter2		wavelet filter structure for the slower dimension
twopow1		2^pow is the length of the signal for the faster dimension 
twopow2		2^pow is the length of the signal for the slower dimension 
stage1		stage of decomposition for the faster dimension
stage2		stage of decomposition for the slower dimension
type		0 for decomposition, 1 for reconstruction
****************************************************************************
Author:		Tong Chen, 05/25/94
***************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"

/* function used internally */
static int wavePack_row(float **x, float **y, waveFilter *filter,
	int twopow1, int twopow2, int stage, int type);

int wavePack_2(float **x, float **y, waveFilter *filter1, 
	waveFilter *filter2, int twopow1, int twopow2,
	int stage1, int stage2, int type)
/***************************************************************************
2D wavelet packet transform 
****************************************************************************
x		array[][] for the signal
y 		array[][] for the wavelet coefficients
filter1		wavelet filter structure for the faster dimension
filter2		wavelet filter structure for the slower dimension
twopow1		2^pow is the length of the signal for the faster dimension 
twopow2		2^pow is the length of the signal for the slower dimension 
stage1		stage of decomposition for the faster dimension
stage2		stage of decomposition for the slower dimension
type		0 for decomposition, 1 for reconstruction
****************************************************************************
Author:		Tong Chen, 05/25/94
***************************************************************************/
{
	int n1, n2, i, flag=1;
	float **z;

	if((stage1>twopow1)||(stage2>twopow2)) return 0;

	n1 = 1<<twopow1;
	n2 = 1<<twopow2;

	z = alloc2float(n1,n2);

	/* first transform along the faster dimension */
	if(!type){

	    for(i=0;i<n2;i++)
		if(wavePack_1(x[i],z[i],filter1,twopow1,stage1,type))
		    flag = flag && 1;
		else flag = 0;

	/* then along the slower dimension */
	    if(wavePack_row(z,y,filter2,twopow1,twopow2,stage2,type))
		flag = flag && 1;
	    else flag = 0;
	}
	
	else{

	    for(i=0;i<n2;i++)
		if(wavePack_1(z[i],y[i],filter1,twopow1,stage1,type))
		    flag = flag && 1;
		else flag = 0;

	/* then along the slower dimension */
	    if(wavePack_row(x,z,filter2,twopow1,twopow2,stage2,type))
		flag = flag && 1;
	    else flag = 0;
	}
	
	free2float(z);
	return flag;
}


static int wavePack_row(float **x, float **y, waveFilter *filter,
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
	int n1, n2, nhalf, nmid, nmidh, len, ishift, pos;
	int i, j, jj, k, l, ilevel, ifirst, ofirst, nband, iband;
	float **tmp, *h, *g;

	if(stage>twopow2) return 0;

	/* length of the signal */
	n1 = 1<<twopow1;
	n2 = 1<<twopow2;

	/* if stage=0, just copy */
	if(stage==0){

		if(!type) 
		   for(i=0; i<n2; i++)
		      for(j=0; j<n1; j++)
			 y[i][j] = x[i][j];
		else
		   for(i=0; i<n2; i++)
		      for(j=0; j<n1; j++)
			 x[i][j] = y[i][j];

		return 1;
	}

	len = filter->len;  
	ishift = filter->ishift;  
	h = filter->filterh;
	g = filter->filterg;

	nhalf = n2>>1;

	tmp = alloc2float(n1,n2);

	/* decomposition */
	if(!type){

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
		    k = (i+2*j-1+pos)%n2; 
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
			    k = (i+2*j-1+pos)%nmid+ifirst;
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
			    k = (i+2*j-1+pos)%nmid+ifirst;
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
			    k = (2*j+i+pos)%nmidh+ifirst; 
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
	    pos = n2;
	    while(pos < abs(ishift)) pos <<= 1;
	    pos += ishift;

	    for(j=0,jj=nhalf;j<nhalf;j++,jj++)
	    	for(i=0;i<len;i++){
		    k = (2*j+i+pos)%n2; 
		    for(l=0;l<n1;l++)
		    	x[k][l] += tmp[j][l]*g[i] + tmp[jj][l]*h[i];
	    	}
	    
	}

	free2float(tmp);
    	return 1;
}
