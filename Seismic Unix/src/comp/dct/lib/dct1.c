/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/

/*****************************************************************************
DCT1 - 1D Discreet Cosine Transform Routines

dctiv -  in-place IV-type DCT, whose inverse is just itself 
dct   -  in-place DCT 

******************************************************************************
Function Prototypes:
void dctiv(float *x, int n, float **c);
void dct(float *x, int n, float **c, int type);

******************************************************************************
dctiv:
Input:
x         array[] of the signal before and after transform
c         table used in the transform 
n         length of the signal

dct:
x         array[] of the signal before and after transform
n         length of the signal
c         table used in the transform 
type      0 for forward and 1 for inverse transform
******************************************************************************
Author:		Tong Chen, 03/16/95
Modifier:       Tong Chen, 06/01/95, use the pre-generated table
*****************************************************************************/
/**************** end self doc ********************************/

#include "comp.h"


void dctiv(float *x, int n, float **c)
/*****************************************************************************
 dctiv - in-place IV-type DCT, whose inverse is just itself 
******************************************************************************
x         array[] of the signal before and after transform
n         length of the signal
c         table used in the transform 
******************************************************************************
Author:		Tong Chen, 03/16/95
Modifier:       Tong Chen, 06/01/95, use the pre-generated table
*****************************************************************************/
{
	float *tmp;
	int i, j;

	tmp = alloc1float(n);

	/* forward transform */
	for(i=0; i<n; i++){

	    tmp[i] = 0.;
	    for(j=0; j<n; j++)
		tmp[i] += x[j]*c[i][j];
       }

       for(i=0; i<n; i++)	
	   x[i] = tmp[i];

       free1float(tmp);
}

void dct(float *x, int n, float **c, int type)
/*****************************************************************************
dct - in-place DCT 
******************************************************************************
x         array[] of the signal before and after transform
n         length of the signal
c         table used in the transform 
type      0 for forward and 1 for inverse transform
*****************************************************************************/
{
	float *tmp;
	int i, j;

	tmp = alloc1float(n);

	/* forward transform */
	if(!type)
	{
	   
	   for(i=0; i<n; i++){

		 tmp[i] = 0.;
		 for(j=0; j<n; j++)
		    tmp[i] += x[j]*c[i][j];
	      }
	}

	else
	{
	   for(i=0; i<n; i++) tmp[i] = 0.;
	   
	   for(j=0; j<n; j++)
	      for(i=0; i<n; i++)
		 tmp[i] += x[j]*c[j][i];
	}
	
       for(i=0; i<n; i++)	
	   x[i] = tmp[i];

       free1float(tmp);
}

