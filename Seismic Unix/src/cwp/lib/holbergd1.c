/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
HOLBERG1D - Compute coefficients of Holberg's 1st derivative filter

holberg1d	comput the coefficients of Holberg's 1st derivative filter

******************************************************************************
Function Prototype:
void holbergd1 (float e, int n, float d[]);

******************************************************************************
Input:
e		maximum relative error in group velocity
n		number of coefficients in filter (must be 2, 4, 6, or 8)

Output:
d		array[n] of coefficients

******************************************************************************
Notes:
Coefficients are output in a form suitable for convolution.  The
derivative is centered halfway between coefficients d[n/2-1] and d[n/2].

Coefficients are computed via the power series method of Kindelan et al.,
1990, On the construction and efficiency of staggered numerical
differentiators for the wave equation:  Geophysics 55, 107-110.
See also, Holberg, 1987, Computational aspects of the choice of
operator and sampling interval for numerical differentiation in
large-scale simulation of wave phenomena:  Geophys. Prosp., 35, 629-655

******************************************************************************
Reference:
Kindelan et al., 1990, 
On the construction and efficiency of staggered numerical
differentiators for the wave equation:  Geophysics 55, 107-110.

See also, Holberg, 1987, Computational aspects of the choice of
operator and sampling interval for numerical differentiation in
large-scale simulation of wave phenomena:  Geophys. Prosp., 35, 629-655

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/06/91
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

void holbergd1 (float e, int n, float d[])
/*****************************************************************************
Compute coefficients of Holberg's 1st derivative filter
******************************************************************************
Input:
e		maximum relative error in group velocity
n		number of coefficients in filter (must be 2, 4, 6, or 8)

Output:
d		array[n] of coefficients
******************************************************************************
Notes:
Coefficients are output in a form suitable for convolution.  The
derivative is centered halfway between coefficients d[n/2-1] and d[n/2].

Coefficients are computed via the power series method of Kindelan et al.,
1990, On the construction and efficiency of staggered numerical
differentiators for the wave equation:  Geophysics 55, 107-110.
See also, Holberg, 1987, Computational aspects of the choice of
operator and sampling interval for numerical differentiation in
large-scale simulation of wave phenomena:  Geophys. Prosp., 35, 629-655
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/06/91
*****************************************************************************/
{
	static float b[4][4][6] = {
	{
		{1.0,1.0,0.0,0.0,0.0,0.0},
		{0.0,0.0,0.0,0.0,0.0,0.0},
		{0.0,0.0,0.0,0.0,0.0,0.0},
		{0.0,0.0,0.0,0.0,0.0,0.0}
	},
	{
		{1.125,0.4330,-0.4583,0.2566,0.0,0.0},
		{-0.04167,-0.1443,-0.1806,-0.0855,0.0,0.0},
		{0.0,0.0,0.0,0.0,0.0,0.0},
		{0.0,0.0,0.0,0.0,0.0,0.0}
	},
	{
		{1.172,0.2742,-0.3006,0.2637,-0.2391,0.0},
		{-0.06510,-0.1371,-0.0100,0.1077,-0.0086,0.0},
		{0.004688,0.0274,0.0661,0.0826,0.0530,0.0},
		{0.0,0.0,0.0,0.0,0.0,0.0}
	},
	{
		{1.1963,0.1987,-0.2195,0.2096,-0.2038,0.1779},
		{-0.07975,-0.1192,0.03929,0.04049,-0.05841,0.04389},
		{0.009570,0.03975,0.04853,-0.00967,-0.05507,-0.01117},
		{-0.0006975,-0.00568,-0.02014,-0.04039,-0.04937,-0.03625}
	} };
	int i,j;
	
	/* compute coefficients via power series */
	for (i=0; i<n; ++i)
		for (j=0,d[i]=0.0; j<6; ++j)
			d[i] += b[n/2-1][i][j]*pow(e,2.0*j/n);
			
	/* arrange coefficients for convolutional derivative filter */
	for (i=n/2; i<n; ++i)
		d[i] = -d[i-n/2];
	for (i=0; i<n/2; ++i)
		d[i] = -d[n-i-1];	
}

#ifdef TEST
void main()
{
	int i,n;
	char s[100];
	float e,d[100],dd[100];
	
	while(1) {
		printf("Enter e, n:  ");
		gets(s);  sscanf(s,"%f %d",&e,&n);
		holberg1(e,n,d);
		for (i=0; i<n; ++i)
			printf("d[%d]=%g\n",i,d[i]);
		conv(n,0,d,n,0,d,n*2-1,0,dd);
		for (i=0; i<n*2-1; ++i)
			printf("dd[%d]=%g\n",i,dd[i]);
	}
}
#endif /* TEST */
