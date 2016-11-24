/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/* Copyright (c) Colorado School of Mines, 1999.*/
/* All rights reserved.                       */

#include "par.h"

void rottensor (float *a1111, float *a3333, float *a1133, float *a1313,
	float *a1113, float *a3313, float si, float co )
/*****************************************************************************
Rotate 2-D elastic stiffness tensor
******************************************************************************
Input:
aijkl		input/output stiffness elements
angle 		rotation angle

Authors:  Andreas Rueger, Colorado School of Mines, 03/14/94
          Tariq Alkalifah, Colorado School of Mines, 02/02/94

******************************************************************************/
{
		float ss,cc,sc,a,c,f,l,m,n;
		ss=si*si; cc=co*co;
		sc=si*co;
		a=*a1111;
		c=*a3333;
		f=*a1133;
		l=*a1313;
		m=*a1113;
		n=*a3313;

		*a1111=a*cc*cc+c*ss*ss+2*f*cc*ss+4*l*cc*ss-4*m*cc*sc-4*n*ss*sc;
		*a3333=a*ss*ss+c*cc*cc+2*f*cc*ss+4*l*cc*ss+4*m*ss*sc+4*n*cc*sc;
		*a1133=a*ss*cc+c*ss*cc+f*(ss*ss+cc*cc)-4*l*ss*cc-2*m*
			(ss*sc-cc*sc)-2*n*(cc*sc- ss*sc);
		*a1313=a*ss*cc+c*ss*cc-2*f*ss*cc+l*(cc*cc+ss*ss-2*ss*cc)
			-2*m*(ss*sc-cc*sc)-2*n*(cc*sc- ss*sc);
		*a1113=a*cc*sc-c*ss*sc+f*(ss*sc-cc*sc)+2*l*(ss*sc-cc*sc)+
			m*(cc*cc -3*cc*ss) +(3*cc*ss - ss*ss )*n;
		*a3313=a*ss*sc-c*cc*sc+f*(cc*sc-ss*sc)+2*l*(cc*sc-ss*sc)+
			 m*( 3*cc*ss - ss*ss ) + n*( cc*cc - 3*ss*cc);
}

