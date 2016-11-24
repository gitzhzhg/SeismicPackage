/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
VANDERMONDE - Functions to solve Vandermonde system of equations Vx=b 

vanded		solve Vandermonde system of doubles
vandef		solve Vandermonde system of floats

******************************************************************************
Function Prototypes:
void vanded (int n, double v[], double b[], double x[]);
void vandef (int n, float v[], float b[], float x[]);

******************************************************************************
Input:
n		dimension of system
v		array[n] of 2nd row of Vandermonde matrix (1st row is all ones)
b		array[n] of right-hand-side column vector

Output:
x		array[n] of solution column vector

******************************************************************************
Notes:
The arrays b and x may be equivalenced.

******************************************************************************
Reference:
Adapted from Algorithm 5.6-2 in Golub, G. H., and Van Loan, C. F., 1983,
Matrix Computations, John-Hopkins University Press.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"


void vanded (int n, double v[], double b[], double x[])
/*****************************************************************************
Solve Vandermonde system of equations Vx=b (double version)
******************************************************************************
Input:
n		dimension of system
v		array[n] of 2nd row of Vandermonde matrix (1st row is all ones)
b		array[n] of right-hand-side column vector

Output:
x		array[n] of solution column vector
******************************************************************************
Notes:
The arrays b and x may be equivalenced.

Adapted from Algorithm 5.6-2 in Golub, G. H., and Van Loan, C. F., 1983,
Matrix Computations, John-Hopkins University Press.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	int i,j;

	for (i=0; i<n; i++)
		x[i] = b[i];
	for (i=0; i<n-1; i++)
		for (j=n-1; j>i; j--)
			x[j] -= v[i]*x[j-1];
	for (i=n-1; i>0; i--) {
		for (j=i; j<n; j++)
			x[j] /= (v[j]-v[j-i]);
		for (j=i; j<n; j++)
			x[j-1] -= x[j];
	}
}

void vandef (int n, float v[], float b[], float x[])
/*****************************************************************************
Solve Vandermonde system of equations Vx=b (float version)
******************************************************************************
Input:
n		dimension of system
v		array[n] of 2nd row of Vandermonde matrix (1st row is all ones)
b		array[n] of right-hand-side column vector

Output:
x		array[n] of solution column vector
******************************************************************************
Notes:
The arrays b and x may be equivalenced.

Adapted from Algorithm 5.6-2 in Golub, G. H., and Van Loan, C. F., 1983,
Matrix Computations, John-Hopkins University Press.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/02/89
*****************************************************************************/
{
	int i,j;

	for (i=0; i<n; i++)
		x[i] = b[i];
	for (i=0; i<n-1; i++)
		for (j=n-1; j>i; j--)
			x[j] -= v[i]*x[j-1];
	for (i=n-1; i>0; i--) {
		for (j=i; j<n; j++)
			x[j] /= (v[j]-v[j-i]);
		for (j=i; j<n; j++)
			x[j-1] -= x[j];
	}
}
