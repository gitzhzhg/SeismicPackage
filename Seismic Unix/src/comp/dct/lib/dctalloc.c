/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/*****************************************************************************
DCTALLOC - ALLOCate space for transform tables for 1D DCT

dctivAlloc - allocate space for the transform table for dctiv
dctAlloc - allocate space for the transform table for dct

******************************************************************************
Function Prototype:
float **dctivAlloc(int n);
float **dctAlloc(int n);

******************************************************************************
dctivAlloc:
Input:
n         length of the signal

dctAlloc:
n         length of the signal
******************************************************************************
Author:		Tong Chen, 06/01/95
*****************************************************************************/


/**************** end self doc ********************************/
#include "comp.h"


float **dctivAlloc(int n)
/*****************************************************************************
dctivAlloc - generate the transform table for dctiv
******************************************************************************
n         length of the signal
******************************************************************************
Author:		Tong Chen, 06/01/95
*****************************************************************************/
{
	float **c;
	int i, j;
	float c1, c2;

	c = alloc2float(n, n);

	/* initialize the transform matrix */
	c1 = 1./sqrt(.5*n);
	c2 = PI/n;

	for(j=0; j<n; j++)
	   for(i=0; i<n; i++)
		c[j][i] = c1*cos((i+.5)*(j+.5)*c2);
	
	/* return the table */
	return (c);
}



float **dctAlloc(int n)
/*****************************************************************************
dctAlloc - generate the transform table for dct
******************************************************************************
n         length of the signal
******************************************************************************
Author:		Tong Chen, 06/01/95
*****************************************************************************/
{
	float **c;
	int i, j;
	float c1, c2, c3;

	c = alloc2float(n, n);

	/* initialize the transform matrix */
	c1 = 1./sqrt(n);
	c2 = 1./sqrt(.5*n);
	c3 = .5*PI/n;

	for(i=0; i<n; i++) c[0][i] = c1;
	
	for(j=1; j<n; j++)
	   for(i=0; i<n; i++)
		c[j][i] = c2*cos((2.*i+1.)*j*c3);
	
	/* return the table */
	return (c);
}
