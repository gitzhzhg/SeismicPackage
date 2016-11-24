/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */


/*********************** self documentation **********************/
/****************************************************************************
ROTTENSH - rotate a 2-D elastic stiffness tensor

rottensh - Rotate 2-D elastic stiffness tensor

Function prototype:
void rottensh (float *a1212, float *a2323, float *a1223, float si, float co );

******************************************************************************
Input:
aijkl		input/output stiffness elements
si,co 		sin/cos of rotation angle

Author:  Andreas Rueger, Colorado School of Mines, 03/14/94
******************************************************************************/
/**************** end self doc ********************************/

#include "par.h"

void rottensh (float *a1212, float *a2323, float *a1223, float si, float co )
/*****************************************************************************
Rotate 2-D elastic stiffness tensor
******************************************************************************
Input:
aijkl		input/output stiffness elements
si,co 		sin/cos of rotation angle

Author:  Andreas Rueger, Colorado School of Mines, 03/14/94
******************************************************************************/
{
		float ss,cc,sc,o,p,q;
		ss=si*si; cc=co*co;
		sc=si*co;

		o=*a1212;
		p=*a2323;
		q=*a1223;

		*a1212=cc*o-2*sc*q+ss*p;
		*a2323=ss*o+2*sc*q+cc*p;
		*a1223=sc*o+(cc-ss)*q-sc*p;

}
