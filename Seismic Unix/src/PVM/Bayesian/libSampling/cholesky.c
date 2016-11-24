/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

#include "cwp.h"
#define INFINITY 9e+99
#define EPSLON 0.001
/*
   This function implements the partial Cholesky factorization described
   in the paper "Computing Modified Newton Directions Using a Partial
   Cholesky Factorization", by Forsgren, Gill and Murray, published 
   in the January 1995 issue of the SIAM Journal of Scientific Computing.


   Function name: int partialCholesky(H, n, nu, perm, L, B)
   Input parameters:
           
              1. float **H:          Pointer to the Hessian matrix
	      2. int n:              Dimension of the Hessian
	      3. float nu:           Acceptance factor for pivot
	      
   Output parameters (indirectly):

              1. float *perm:        Pointer to the permutation vector for 
	                             accessing lower triangular matrix L
	      2. float **L:          Pointer to the permuted lower 
	                             triangular matrix
	      3. float **B:          Pointer to the block diagonal matrix
	      4. int *n1:            Size of principal submatrix   

   Output parameters (directly):
     
              1. If partialCholesky returns 0 the Hessian was not positive
	         definite and the complete factorization was performed. 
		 Otherwise, partialCholesky returns 1.
	
*/
int partialCholesky(float **H, int n, float nu, int *perm,
		    float **L, float **B, int *n1)
{
   /* declaration of variables */
   int k = 0;             /* current row */
   int i, j;              /* pointers */
   int iP = 0;            /* pivot index */
   int flag = 0;          /* if 1 Hessian is non-positive definite */ 
                          /* if 0 positive definite */
   float offD;            /* off-diagonal term */
   float pivot;           /* current pivot */
   float aux;             /* auxiliary variable */


   /* initializing permutation vector perm */
   for (i = 0; i < n; i++)
      perm[i] = i;
   
   /* copying H into B */
   for (i = 0; i < n; i++)
      for (j = 0; j < n; j++)
	 B[i][j] = H[i][j];

   while (k < n)
   {
      /* getting pivot of the matrix B */
      for(pivot = -INFINITY, i = k; i < n; i++)
      {
	 if (B[i][i] > pivot)
	 {
	    iP = i;
	    pivot = B[i][i];
	 }
      }
      /* finding the maximum absolute value of all off-diagonal terms */
      /* with respect to the pivot */
      for (offD = -INFINITY, i = 0; i < iP; i++)
      {
	 if (fabs(B[iP][i]) > offD)
	 {
	    offD = fabs(B[iP][i]);
	 }
      }
      for (i = iP + 1; i < n; i++)
      {
	 if (fabs(B[iP][i]) > offD)
	 {
	    offD = fabs(B[iP][i]);
	 }
      }

      /* checking stopping criterion */
      if (pivot > EPSLON && pivot > nu * offD)
      {
	 *n1 = k;
	 if (k != iP)
	 {
	    /* doing the permutation of rows and columns k <> iP */
	    /* rows */
	    for (i = 0; i < n; i++)
	    {
	       aux = B[iP][i]; 
	       B[iP][i] = B[k][i]; 
	       B[k][i] = aux;
	    }
	    /* columns */
	    for (i = 0; i < n; i++)
	    {
	       aux = B[i][iP]; 
	       B[i][iP] = B[i][k]; 
	       B[i][k] = aux;
	    }
	    
	    /* updating the permutation vector perm */
	    aux = perm[iP];
	    perm[iP] = perm[k];
	    perm[k] = aux;
	 }
	 
	 /* begining standard Cholesky decomposition */
	 /* See Golub and VanLoan, pg. 143 */
	 for (i = k; i < n; i++)
	 {
	    L[perm[i]][k] = B[i][k] / B[k][k];
	 }
	 
	 if (k < n)
	 {
	    for (j = k + 1; j < n; j++)
	    {
	       for (i = k + 1; i < n; i++)
	       {
		  B[i][j] = B[i][j] - L[perm[i]][k] * B[j][k];
	       }
	    }
	    
	    /* zeroing off-diagonals of B */
	    for (i = k + 1; i < n; i++)
	    {
	       B[i][k] = 0;
	       B[k][i] = 0;
	    }
	 }
	 k = k + 1;
      }
      else
      {
	 /* filling the lower diagonal with 1's */
	 for (i = k; i < n; i++)
	 {
	    L[perm[i]][i] = 1;
	 }
	 k = n + 1;
      }
   }
   if (*n1 + 1 != n) flag = 1;
   return(flag);
}
