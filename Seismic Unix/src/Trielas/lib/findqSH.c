/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"


int findqSH(float si, float co, float pl, float a1212, float a1223, float 	
	a2323, float *pxnew, float *pznew, float *vgx, float *vgz, int r)
/*****************************************************************************
Continue slowness across interface 
******************************************************************************
Input:
s,c		slope of interface measured with respect to horizontal
pl		tangential component of slowness
aijkl		density-normalized stiffness elements
r		=1 reflection =0 transmission

******************************************************************************
Output:
*pxnew		address pointing to new px
*pznew 		address pointing to new pz
-1		no root found
1		root found
*vgx,*vgz  	group velocity pointers
*g11,*g33,*g13  polarizations

*****************************************************************************
Author: Andreas Rueger, Colorado School of Mines, 02/01/94 */
{
	float n1,n3,a,b,c,sqr,b1,b3;
	float eta=0.0,px,pz,vgxd,vgzd,vgm;

	/* unit vector h normal to edge */
 	n1 = -si;
 	n3 = co;
	b1 = pl*co;
	b3 = pl*si;

	a  = n1*n1*a1212 + n3*n3*a2323 + 2* n1*n3*a1223;
	b  = 2*(b1*n1*a1212 + b3*n3*a2323 +a1223*(b1*n3 + b3*n1));
	c  = b1*b1*a1212 + b3*b3*a2323 + 2*a1223*b1*b3 -1;

	sqr = b*b-4*a*c;

	if(sqr < 0)
		return 0;
	else if(ABS(a) < FLT_EPSILON)
		return 0;
	else if(r == 0){
		sqr = sqrt(sqr);
		eta = 0.5/a * (sqr-b);
	} else if(r == 1){
		sqr = sqrt(sqr);
		eta = 0.5/a * (-sqr-b);
	} else 
		err(" ERROR in findqSH \n");

	px = b1+eta*n1;
	pz = b3+eta*n3;

	vgxd = a1212*px + a1223*pz;
	vgzd = a1223*px + a2323*pz;

	vgm=-si*vgxd+co*vgzd;

	if((vgm > 0 && r == 1) || (vgm < 0 && r == 0))
		return 0;
	else {
		*pxnew = px;
		*pznew = pz;
		*vgx   = vgxd;
		*vgz   = vgzd;

		return 1;
	}
}
