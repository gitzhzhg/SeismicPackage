/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/************************************************************************
WINDOW - windowing routines

hanningnWindow - returns an n element long hanning window 

************************************************************************
Function prototypes:
void hanningnWindow(int n,float *w);
************************************************************************
Author: Potash Corporation, Sascatchewan: Balasz Nemeth given to CWP 2008
************************************************************************/

/**************** end self doc ********************************/
	

#include "cwp.h"

void hanningnWindow(int n,float *w)
/************************************************************************
hanningnWindow - returns an n element long hanning window 
************************************************************************
Input:
n	size of window
w	hanning window function of size n
************************************************************************
Notes:
	w[k] = 0.5(1-cos(2PI * K/n+1)) k=1,....n
************************************************************************
Author: Potash Corporation, Sascatchewan: Balasz Nemeth, given to CWP 2008
************************************************************************/
	
{
	int i;
	float PI2=2.0*PI;
	
	for(i=0;i<n;i++) 
		w[i] = 0.5*(1-cos(PI2*(i+1)/(n+1)));		
}
