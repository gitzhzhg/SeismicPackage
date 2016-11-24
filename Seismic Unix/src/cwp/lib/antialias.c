/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
ANTIALIAS - Butterworth anti-aliasing filter

antialias	use before increasing the sampling interval of data
		 i.e. subsampling 

******************************************************************************
Function Prototype:
void antialias (float frac, int phase, int n, float p[], float q[]);

******************************************************************************
Input:
frac		current sampling interval / future interval (should be <= 1)
phase		=0 for zero-phase filter; =1 for minimum-phase filter
n		number of samples
p		array[n] of input samples

Output:
q		array[n] of output (anti-alias filtered) samples		

******************************************************************************
Notes:
The anti-alias filter is a recursive (Butterworth) filter.  For zero-phase
anti-alias filtering, the recursive filter is applied forwards and backwards.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/06/90
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

void antialias (float frac, int phase, int n, float p[], float q[])
/*****************************************************************************
Anti-alias filter - use before increasing the sampling interval (sub-sampling)
******************************************************************************
Input:
frac		current sampling interval / future interval (should be <= 1)
phase		=0 for zero-phase filter; =1 for minimum-phase filter
n		number of samples
p		array[n] of input samples

Output:
q		array[n] of output (anti-alias filtered) samples		
******************************************************************************
Notes:
The anti-alias filter is a recursive (Butterworth) filter.  For zero-phase
anti-alias filtering, the recursive filter is applied forwards and backwards.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/06/90
*****************************************************************************/
{
	int i,j,npoles,ntemp;
	float fnyq,fpass,apass,fstop,astop,f3db,*ptemp,ptempi;
	
	/* if no anti-alias filter need be applied, then simply copy input */
	if (ABS(frac)>=1.0) {
		for (i=0; i<n; ++i)
			q[i] = p[i];
		return;
	}
	
	/* determine number of poles and -3db point for filter */
	fnyq = 0.5*ABS(frac);
	fpass = 0.6*fnyq;
	apass = 0.99;
	fstop = fnyq;
	astop = 0.01;
	bfdesign(fpass,apass,fstop,astop,&npoles,&f3db);
	
	/* if minimum-phase, then use npoles*2 poles in one direction only */
	if (phase!=0) {
		bflowpass(npoles*2,f3db,n,p,q);
	
	/* else, if zero-phase, use npoles in both directions */
	} else {
	
		/* pad input with zeros to catch recursive filter tail */
		ntemp = n+100;
		ptemp = alloc1float(ntemp);
		for (i=0; i<n; ++i)
			ptemp[i] = p[i];
		for (i=n; i<ntemp; ++i)
			ptemp[i] = 0.0;
		
		/* filter zero-padded input */
		bflowpass(npoles,f3db,ntemp,ptemp,ptemp);
		
		/* reverse filtered input and filter again */
		for (i=0,j=ntemp-1; i<j; ++i,--j) {
			ptempi = ptemp[i];
			ptemp[i] = ptemp[j];
			ptemp[j] = ptempi;
		}
		bflowpass(npoles,f3db,ntemp,ptemp,ptemp);
		
		/* undo the reverse while copying to output */
		for (i=0,j=ntemp-1; i<n; ++i,--j)
			q[i] = ptemp[j];
		free1float(ptemp);
	}
}
