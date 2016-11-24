/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "cwp.h"

/*********************** self documentation **********************/
/*************************************************************************
AXB - Functions to solve a linear system of equations Ax=b by LU
	decomposition, invert a square matrix or directly multiply an
	inverse matrix by another matrix (without explicitely computing
	the inverse).

LU_decomposition	Decompose a matrix (A) into a lower triangular (L)
			and an upper triangular (U) such that A=LU

backward_substitution	Apply backward substitution to an LU decomposed
			matrix to solve the linear system of equations Ax=b

inverse_matrix		compute the inverse of a square non-singular matrix

inverse_matrix_multiply	computes the product A^(-1)*B without explicitely
			computing the inverse matrix

***************************************************************************
Function prototypes:
void LU_decomposition (int nrows, float **matrix, int *idx, float *d);
void backward_substitution (int nrows, float **matrix, int *idx, float *b);
void inverse_matrix (int nrows, float **matrix);
void inverse_matrix_multiply (int nrows1, float **matrix1, int ncols2,
        int nrows2, float **matrix2, float **out_matrix);

***************************************************************************
LU_decomposition:
Input:
nrows		number of rows of matrix to invert
matrix		matrix of coefficients in linear system Ax=b 

Output:
matrix		matrix containing LU decomposition (original matrix destroyed)
idx		vector recording the row permutations effected by partial
		pivoting
d		+/- 1 depending on whether the number of row interchanges
		was even or odd
*************************************************************************
backward_substitution
Input:
nrows		number of rows (and columns) of input matrix
matrix		matrix of coefficients (after LU decomposition)
idx		permutation vector obtained from routine LU_decomposition 
b		right hand side vector in equation Ax=b

Output:
b		vector with the solution
**************************************************************************
inverse_matrix
Input:
nrows		number of rows (and columns) of input matrix
matrix		matrix to invert

Output:
matrix		inverse of input matrix 
**************************************************************************
inverse_matrix_multiply
nrows1          number of rows (and columns) of matrix to invert
matrix1         square matrix to invert
ncols2          number of coulmns of second matrix
nrows2		number of rows of second matrix
matrix          second matrix (multiplicator)

Output Parameters:
out_matrix      matrix containing the product of the inverse of the first
                matrix by the second one.
Note:
matrix1 and matrix2 are not destroyed, (not clobbered)
*************************************************************************
Notes:
To solve the set of linear equations Ax=b, first do the LU decomposition of
A (which will clobber A with its LU decomposition) and then do the backward 
substitution with this new matrix and the right-hand side vector b. The vector
b will be clobbered with the solution. Both, the original matrix and vector B,
will have been destroyed.

The LU decomposition is carried out with the Crout's method with implicit
partial pivoting that guaratees that the maximum pivot is used in every
step of the algorithm.

The operation count to solve a linear system of equations via LU decomposition
is 1/3N^3 and is a factor of 3 better than the standard Gauss-Jordan algorithm
To invert a matrix the count is the same with both algorithms: N^3.

Once a linear system Ax=b has been solved, to solve another linear system
with the same matrix A but with different vetor b, ONLY the back substitution 
has to be repeated with the new b (remember that the matrix in backsubstitution
is not the original matrix but its LU decomposition)

If you want to compute A^(-1)*B from matrices A and B, it is better to 
use the subroutine inverse_matrix_multiply rather than explicitely computing
the inverse. This saves a whole martix multiplication and is also more accurate.

***************************************************************************
Refferences:
Press, Teukolsky, Vettering and Flannery, Numerical Recipes in C: 
	The art of scientific computing. Cambridge University Press.
	second edition. (1992).
Golub and Van Loan, Matrix Computations. John Hopkins University Press.
	Second Edition. (1989). 
Horn and Johnson, Matrix Analysis. Cambridge University Press. (1985).
*************************************************************************
Credits:
Adapted from discussions in Numerical Recipes, by Gabriel Alvarez (1995)
*************************************************************************/
/**************** end self doc ********************************/

#define TINY 0.000001

/*************************************************************************

Subroutine for performing an LU decomposition of a square matrix

*************************************************************************/
void LU_decomposition (int nrows, float **matrix, int *idx, float *d)
/*************************************************************************
Input:
nrows		number of rows of matrix to invert
matrix		matrix to invert

Output:
matrix		inverted matrix
idx		vector recording the row permutations effected by partial
		pivoting
d		+/- 1 depending on whether the number of row interchanges
		was even or odd
*************************************************************************

Credits
	Adapted from discussions in Numerical Recipes by Gabriel Alvarez (1995)

**************************************************************************/


{
	int i,j,k;	/* loop counters for rows, columns and summs */
	int imax=0;	/* index of maximum pivot */
	float big;	/* largest number in input matrix */
	float dum;	/* pivot scale factor */
	float sum;	/* auxiliary variable for summ  */
	float temp;	/* auxiliary variable */
	float *vv;	/* vector to store implicit scaling for each row */

	/* allocate working space */
	vv = alloc1float(nrows);

	/* initialize interchanges counter */
	*d = 1.0;

	/* loop over rows to get implicit scaling information */
	for (i=0; i<nrows; i++) {
		big = 0.0;
		for (j=0; j<nrows; j++)
			if((temp=ABS(matrix[i][j]))>big) big=temp;
		if (big == 0.0) 
			fprintf(stderr,"error, singular matrix in LU decomposition\n");
		
		/* save the scaling */
		vv[i] = 1.0/big;
	}

	/* loop over columns (Crout's method) */
	for (j=0; j<nrows; j++) {
		for (i=0; i<j; i++) {
			sum = matrix[i][j];
			for (k=0; k<i; k++)
				sum -= matrix[i][k]*matrix[k][j];
			matrix[i][j] = sum;
		}
	
		/* initialize for the search for largest pivot element */
		big = 0.0;
		for (i=j; i<nrows; i++) {
			sum = matrix[i][j];
			for (k=0; k<j; k++)
				sum -= matrix[i][k]*matrix[k][j];
			matrix[i][j] = sum;

			/*  Is new pivot better than best so far? */
			if ((dum=vv[i]*ABS(sum)) >= big) {
 				big = dum;
				imax = i;
			}
		}
	
		/* Do we need to interchange rows */
		if (j != imax) {
			for (k=0; k<nrows; k++) {
				dum = matrix[imax][k];
				matrix[imax][k] = matrix[j][k];
				matrix[j][k] = dum;
			}

			/* change the parity of d */
			*d = -(*d);

			/* interchange the scale factor */
			vv[imax] = vv[j];
		}
		idx[j] = imax;

		/* if matrix becomes singular don't use pivot=0 */
		if (matrix[j][j] == 0.0) matrix[j][j]= TINY;
	
		if (j !=nrows) {

			/* divide by the pivot element */
			dum = 1.0/matrix[j][j];
			for (i=j+1; i<nrows; i++)
				matrix[i][j] *= dum;
		}
	}

	/* free workspace */
	free1float(vv);
}

/************************************************************************

	Subroutine to solve Ax=b by backward substitution where A 
		is the LU decomposition of the original matrix

************************************************************************/
void backward_substitution (int nrows, float **matrix, int *idx, float *b)
/************************************************************************
Input Parameters:
nrows		number of rows (and columns) of input matrix
matrix		matrix of coefficients (after LU decomposition)
idx		permutation vector obtained from routine LU decomposition 
b		right hand side vector in equation Ax=b

Output Parameters:
b		vector with the solution
************************************************************************
Credits:
	Adapted from discussions in Numerical Recipes in C by Gabriel Alvarez (1995)
************************************************************************/

{
	int i,ii = -1,j;	/* loop counters */
	int ip;			/* index of first nonvanishing element of b */
	float sum;		/* auxiliary variable for partial sums */

	for (i=0; i<nrows; i++) {
		ip = idx[i];

		/* do forward substitution */
		sum = b[ip];
		b[ip] = b[i];
		if (ii !=-1)
			for (j=ii; j<i; j++)
				sum -= matrix[i][j]*b[j];
		else if (sum !=0.0)
			ii = i;
		b[i] = sum;
	}

	/* now, do the backward substitution */
	for (i=nrows-1; i>=0; i--) {
		sum = b[i];
		for (j=i+1; j<nrows; j++)
			sum -= matrix[i][j]*b[j];

		/* store results in output vector */
		b[i] = sum/matrix[i][i];
	}
}

/************************************************************************
	
	Subroutine to invert a square non-singular matrix via LU
	decomposition. The original matrix is clobbered with the inverse

************************************************************************/
void inverse_matrix (int nrows, float **matrix)
/************************************************************************
Input:
nrows		number of rows (and columns) in matrix  to invert 
matrix		square, non-singular matrix to invert

Output:
matrix		inverted matrix
************************************************************************
Credits:
	Adapted from discussions in Numerical Recipes by Gabriel Alvarez (1995)

************************************************************************/
{
	int i,j;		/* loop counters */
	float d;		/* +/-1 depending on row interchanges even/odd*/
	int *idx;		/* vector of row permutations */
	float *column;		/* unit vector for backward substitution*/
	float **inverse;	/* array to hold the inverse matrix */

	/* allocate working space */
	idx = alloc1int(nrows);
	column = alloc1float(nrows);	
	inverse = alloc2float(nrows,nrows);

	/* first, do the LU decomposition of input matrix */
	LU_decomposition (nrows, matrix, idx, &d);

	/* find inverse by columns */
	for (j=0; j<nrows; j++) {

		/* unit vector corresponding to current column */
		for (i=0; i<nrows; i++) column[i]=0.0;
		column[j]=1.0;
			
		/* backward substitution column by column */
		backward_substitution (nrows, matrix, idx, column);

		/* compute inverse matrix column by column */
		for (i=0; i<nrows; i++)
			inverse[i][j]=column[i]; 
	}

	/* clobber original matrix with its inverse */
	for (i=0; i<nrows; i++)
		for (j=0; j<nrows; j++)
			matrix[i][j]=inverse[i][j];

	/* free allocated space */
	free1int(idx);
	free1float(column);
	free2float(inverse);
}

/*************************************************************************

	Subroutine to multiply the inverse of a square matrix and 
		by another matrix without computing the inverse

*************************************************************************/
void inverse_matrix_multiply (int nrows1, float **matrix1, int ncols2,
	int nrows2, float **matrix2, float **out_matrix)
/*************************************************************************
Input Parameters:
nrows1		number of rows (and columns) of matrix to invert
matrix1		square matrix to invert
ncols2		number of coulmns of second matrix
nrows2		number of rows of second matrix
matrix		second matrix (multiplicator)

Output Parameters:
out_matrix	matrix containing the product of the inverse of the first 
		matrix by the second one. 
Note:
matrix1 and matrix2 are not destroyed (not clobbered)
*************************************************************************
Credits:
	Adapted from discussions in Numerical Recipes in C by Gabriel Alvarez (1995)
*************************************************************************/

{
	int i,j;		/* loop counters for rows and coulmns */
	float d;		/* to use in LU decomposition */
	int *idx;		/* to record permutations by partial pivoting*/
	float **scratch1;	/* array to hold input matrix1 */
	float *scratch2;	/* vector to hold column of input matrix2 */

	/* allocate working space */
	idx = alloc1int(nrows1);
	scratch1 = alloc2float(nrows1,nrows1);
	scratch2 = alloc1float(nrows2);

	/* copy input matrix1 to scratch to avoid clobbering */
	for (i=0; i<nrows1; i++)
		for (j=0; j<nrows1; j++)
			scratch1[i][j]=matrix1[i][j];

	/* do the LU decomposition */
	LU_decomposition (nrows1, scratch1, idx, &d);
	
	/* find inverse by columns */
	for (j=0; j<ncols2; j++) {
	
		/* copy column of second input matrix to scratch vector */
		for (i=0; i<nrows2; i++) scratch2[i]=matrix2[i][j];

		/* do backward substitution */
		backward_substitution (nrows1, scratch1, idx, scratch2);

		/* copy results to output matrix */
		for (i=0; i<nrows1; i++) out_matrix[i][j] = scratch2[i];
	}

	/* free allocated space */
	free2float(scratch1);
	free1float(scratch2);
}

