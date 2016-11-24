/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
SGA - Single precision general matrix functions adapted from LINPACK FORTRAN:

sgefa	Gaussian elimination to obtain the LU factorization of a matrix.
sgeco	Gaussian elimination to obtain the LU factorization and 
	condition number of a matrix.
sgesl	Solve linear system Ax = b or A'x = b after LU factorization.

******************************************************************************
Function Prototypes:
void sgefa (float **a, int n, int *ipvt, int *info);
void sgeco (float **a, int n, int *ipvt, float *rcond, float *z);
void sgesl (float **a, int n, int *ipvt, float *b, int job);

******************************************************************************
sgefa:
Input:
a		matrix[n][n] to be factored (see notes below)
n		dimension of a

Output:
a		matrix[n][n] factored (see notes below)
ipvt		indices of pivot permutations (see notes below)
info		index of last zero pivot (or -1 if no zero pivots)

******************************************************************************
sgeco:
Input:
a		matrix[n][n] to be factored (see notes below)
n		dimension of a

Output:
a		matrix[n][n] factored (see notes below)
ipvt		indices of pivot permutations (see notes below)
rcond		reciprocal condition number (see notes below)

Workspace:
z		array[n]

******************************************************************************
sgesl
Input:
a		matrix[n][n] that has been LU factored (see notes below)
n		dimension of a
ipvt		indices of pivot permutations (see notes below)
b		right-hand-side vector[n]
job		=0 to solve Ax = b
		=1 to solve A'x = b

Output:
b		solution vector[n]

******************************************************************************
Notes:
These functions were adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix a
is actually a pointer to an array of pointers to floats, as declared
above and used below.

Elements of a are stored as follows:
a[0][0]    a[1][0]    a[2][0]   ... a[n-1][0]
a[0][1]    a[1][1]    a[2][1]   ... a[n-1][1]
a[0][2]    a[1][2]    a[2][2]   ... a[n-1][2]
.                                       .
.             .                         .
.                        .              .
.                                       .
a[0][n-1]  a[1][n-1]  a[2][n-1] ... a[n-1][n-1]

Both the factored matrix a and the pivot indices ipvt are required
to solve linear systems of equations via sgesl.

sgeco:
Given the reciprocal of the condition number, rcond, and the float
epsilon, FLT_EPSILON, the number of significant decimal digits, nsdd,
in the solution of a linear system of equations may be estimated by:
	nsdd = (int)log10(rcond/FLT_EPSILON)

This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix a
is actually a pointer to an array of pointers to floats, as declared
above and used below.

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 10/01/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

void
sgefa (float **a, int n, int *ipvt, int *info)
/*****************************************************************************
Gaussian elimination to obtain the LU factorization of a matrix
******************************************************************************
Input:
a		matrix[n][n] to be factored (see notes below)
n		dimension of a

Output:
a		matrix[n][n] factored (see notes below)
ipvt		indices of pivot permutations (see notes below)
info		index of last zero pivot (or -1 if no zero pivots)
******************************************************************************
Notes:
This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix a
is actually a pointer to an array of pointers to floats, as declared
above and used below.

Elements of a are stored as follows:
a[0][0]    a[1][0]    a[2][0]   ... a[n-1][0]
a[0][1]    a[1][1]    a[2][1]   ... a[n-1][1]
a[0][2]    a[1][2]    a[2][2]   ... a[n-1][2]
.                                       .
.             .                         .
.                        .              .
.                                       .
a[0][n-1]  a[1][n-1]  a[2][n-1] ... a[n-1][n-1]

Both the factored matrix a and the pivot indices ipvt are required
to solve linear systems of equations via sgesl.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 10/01/89
*****************************************************************************/
{
	int j,k,kp1,l,nm1;
	float t;

	*info = -1;
	nm1 = n-1;
	for (k=0; k<nm1; k++) {
		kp1 = k+1;

		/* find l = pivot index */
		l = k+isamax(n-k,&a[k][k],1);
		ipvt[k] = l;

		/* zero pivot implies this column already triangularized */
		if (a[k][l]==0.0) {
			*info = k;
			continue;
		}

		/* if necessary, interchange */
		if (l!=k) {
			t = a[k][l];
			a[k][l] = a[k][k];
			a[k][k] = t;
		}

		/* compute multipliers */
		t = -1.0/a[k][k];
		sscal(n-k-1,t,&a[k][k+1],1);

		/* row elimination with column indexing */
		for (j=kp1; j<n; j++) {
			t = a[j][l];
			if (l!=k) {
				a[j][l] = a[j][k];
				a[j][k] = t;
			}
			saxpy(n-k-1,t,&a[k][k+1],1,&a[j][k+1],1);
		}
	}
	ipvt[n-1] = n-1;
	if (a[n-1][n-1]==0.0) *info = n-1;
}

void
sgeco (float **a, int n, int *ipvt, float *rcond, float *z)
/*****************************************************************************
Gaussian elimination to obtain the LU factorization and
condition number of a matrix.
******************************************************************************
Input:
a		matrix[n][n] to be factored (see notes below)
n		dimension of a

Output:
a		matrix[n][n] factored (see notes below)
ipvt		indices of pivot permutations (see notes below)
rcond		reciprocal condition number (see notes below)

Workspace:
z		array[n]
******************************************************************************
Notes:
This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix a
is actually a pointer to an array of pointers to floats, as declared
above and used below.

Elements of a are stored as follows:
a[0][0]    a[1][0]    a[2][0]   ... a[n-1][0]
a[0][1]    a[1][1]    a[2][1]   ... a[n-1][1]
a[0][2]    a[1][2]    a[2][2]   ... a[n-1][2]
.                                       .
.             .                         .
.                        .              .
.                                       .
a[0][n-1]  a[1][n-1]  a[2][n-1] ... a[n-1][n-1]

Both the factored matrix a and the pivot indices ipvt are required
to solve linear systems of equations via sgesl.

Given the reciprocal of the condition number, rcond, and the float
epsilon, FLT_EPSILON, the number of significant decimal digits, nsdd,
in the solution of a linear system of equations may be estimated by:
	nsdd = (int)log10(rcond/FLT_EPSILON)
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 10/01/89
*****************************************************************************/
{
	int info,j,k,kp1,l;
	float ek,t,wk,wkm,anorm,s,sm,ynorm;

	/* compute 1-norm of a */
	for (j=0,anorm=0.0; j<n; j++) {
		t = sasum(n,a[j],1);
		anorm = (t>anorm)?t:anorm;
	}

	/* factor */
	sgefa(a,n,ipvt,&info);

	/* rcond = 1/(norm(a)*(estimate of norm(inverse(a)))).
	 * estimate = norm(z)/norm(y) where Az = y and A'y = e.
	 * A' is the transpose of A.  The components of e are
	 * chosen to cause maximum local growth in the elements of
	 * w where U'w = e.  The vectors are frequently rescaled
	 * to avoid overflow
	 */

	/* solve U'w = e */
	ek = 1.0;
	for (j=0; j<n; j++)
		z[j] = 0.0;
	for (k=0; k<n; k++) {
		if (z[k]!=0.0) ek = (z[k]>0.0)?-ABS(ek):ABS(ek);
		if (ABS(ek-z[k])>ABS(a[k][k])) {
			s = ABS(a[k][k])/ABS(ek-z[k]);
			sscal(n,s,z,1);
			ek *= s;
		}
		wk = ek-z[k];
		wkm = -ek-z[k];
		s = ABS(wk);
		sm = ABS(wkm);
		if (a[k][k]==0.0) {
			wk = 1.0;
			wkm = 1.0;
		} else {
			wk = wk/a[k][k];
			wkm = wkm/a[k][k];
		}
		kp1 = k+1;
		if (kp1<n) {
			for (j=kp1; j<n; j++) {
				t = z[j]+wkm*a[j][k];
				sm += ABS(t);
				z[j] += wk*a[j][k];
				s += ABS(z[j]);
			}
			if (s<sm) {
				t = wkm-wk;
				wk = wkm;
				for (j=kp1; j<n; j++)
					z[j] += t*a[j][k];
			}
		}
		z[k] = wk;
	}
	s = 1.0/sasum(n,z,1);
	sscal(n,s,z,1);

	/* solve L'y = w */
	for (k=n-1; k>=0; k--) {
		if (k<n-1) z[k] += sdot(n-k-1,&a[k][k+1],1,&z[k+1],1);
		if (ABS(z[k])>1.0) {
			s = 1.0/ABS(z[k]);
			sscal(n,s,z,1);
		}
		l = ipvt[k];
		t = z[l];
		z[l] = z[k];
		z[k] = t;
	}
	s = 1.0/sasum(n,z,1);
	sscal(n,s,z,1);

	ynorm = 1.0;

	/* solve Lv = y */
	for (k=0; k<n; k++) {
		l = ipvt[k];
		t = z[l];
		z[l] = z[k];
		z[k] = t;
		if (k<n-1) saxpy(n-k-1,t,&a[k][k+1],1,&z[k+1],1);
		if (ABS(z[k])>1.0) {
			s = 1.0/ABS(z[k]);
			sscal(n,s,z,1);
			ynorm *= s;
		}
	}
	s = 1.0/sasum(n,z,1);
	sscal(n,s,z,1);
	ynorm *= s;

	/* solve Uz = v */
	for (k=n-1; k>=0; k--) {
		if (ABS(z[k])>ABS(a[k][k])) {
			s = ABS(a[k][k])/ABS(z[k]);
			sscal(n,s,z,1);
			ynorm *= s;
		}
		if (a[k][k]!=0.0) 
			z[k] /= a[k][k];
		else
			z[k] = 1.0;
		t = -z[k];
		saxpy(k,t,a[k],1,z,1);
	}

	/* make znorm = 1.0 */
	s = 1.0/sasum(n,z,1);
	sscal(n,s,z,1);
	ynorm *= s;

	if (anorm!=0.0) 
		*rcond = ynorm/anorm;
	else
		*rcond = 0.0;
}

void
sgesl (float **a, int n, int *ipvt, float *b, int job)
/*****************************************************************************
solve linear system Ax = b or A'x = b after LU factorization
******************************************************************************
Input:
a		matrix[n][n] that has been LU factored (see notes below)
n		dimension of a
ipvt		indices of pivot permutations (see notes below)
b		right-hand-side vector[n]
job		=0 to solve Ax = b
		=1 to solve A'x = b

Output:
b		solution vector[n]
******************************************************************************
Notes:
This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix a
is actually a pointer to an array of pointers to floats, as declared
above and used below.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 10/01/89
*****************************************************************************/
{
	int k,l,nm1;
	float t;

	nm1 = n-1;

	/* if solving Ax = b */
	if (job==0) {

		/* first solve Ly = b */
		for (k=0; k<nm1; k++) {
			l = ipvt[k];
			t = b[l];
			if (l!=k) {
				b[l] = b[k];
				b[k] = t;
			}
			saxpy(n-k-1,t,&a[k][k+1],1,&b[k+1],1);
		}

		/* now solve Ux = y */
		for (k=n-1; k>=0; k--) {
			b[k] /= a[k][k];
			t = -b[k];
			saxpy(k,t,a[k],1,b,1);
		}

	/* else, if solving A'x = b */
	} else {

		/* first solve U'y = b */
		for (k=0; k<n; k++) {
			t = sdot(k,a[k],1,b,1);
			b[k] = (b[k]-t)/a[k][k];
		}

		/* now solve L'x = y */
		for (k=n-2; k>=0; k--) {
			b[k] += sdot(n-k-1,&a[k][k+1],1,&b[k+1],1);
			l = ipvt[k];
			if (l!=k) {
				t = b[l];
				b[l] = b[k];
				b[k] = t;
			}
		}
	}
}
