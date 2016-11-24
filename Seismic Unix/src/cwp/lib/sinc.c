/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
SINC - Return SINC(x) for as floats or as doubles

fsinc		return float value of sinc(x) for x input as a float
dsinc		return double precision sinc(x) for double precision x

******************************************************************************
Function Prototype:
float fsinc (float x);
double dsinc (double x);

******************************************************************************
Input:
x		value at which to evaluate sinc(x)

Returned: 	sinc(x)

******************************************************************************
Notes:
    sinc(x) = sin(PI*x)/(PI*x) 

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

float fsinc (float x)
/*****************************************************************************
Return sinc(x) = sin(PI*x)/(PI*x) (float version)
******************************************************************************
Input:
x		value at which to evaluate sinc(x)

Returned: 	sinc(x)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	float pix;

	if (x==0.0) {
		return 1.0;
	} else {
		pix = PI*x;
		return sin(pix)/pix;
	}
}

double dsinc (double x)
/*****************************************************************************
Return sinc(x) = sin(PI*x)/(PI*x) (double version)
******************************************************************************
Input:
x		value at which to evaluate sinc(x)

Returned:	sinc(x)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	double pix;

	if (x==0.0) {
		return 1.0;
	} else {
		pix = PI*x;
		return sin(pix)/pix;
	}
}
