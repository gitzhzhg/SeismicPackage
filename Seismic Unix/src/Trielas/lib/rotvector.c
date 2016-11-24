/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"

void rotvector (float *x, float *z, float si, float co )
/*****************************************************************************
Rotate 2-D vector
******************************************************************************
Input:
x,z		input/output vector elements
si/co 		sin/cos of rotation angle

Author:  Andreas Rueger, Colorado School of Mines, 03/14/94
******************************************************************************/
{
		float o,p;

		o=*x;
		p=*z;

		*x=o*co + p*si;
		*z=p*co - o*si;
}
