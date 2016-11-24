/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
SQR - Single precision QR decomposition functions adapted from LINPACK FORTRAN:

sqrdc	Compute QR decomposition of a matrix.
sqrsl	Use QR decomposition to solve for coordinate transformations,
	projections, and least squares solutions.
sqrst	Solve under-determined or over-determined least squares problems,
	with a user-specified tolerance.

*****************************************************************************
Function Prototypes:
void sqrdc (float **x, int n, int p, float *qraux, int *jpvt,
	float *work, int job);
void sqrsl (float **x, int n, int k, float *qraux,
	float *y, float *qy, float *qty,
	float *b, float *rsd, float *xb, int job, int *info);
void sqrst (float **x, int n, int p, float *y, float tol,
	float *b, float *rsd, int *k,
	int *jpvt, float *qraux, float *work);

******************************************************************************
sqrdc:
Input:
x		matrix[p][n] to decompose (see notes below)
n		number of rows in the matrix x
p		number of columns in the matrix x
jpvt		array[p] controlling the pivot columns (see notes below)
job		=0 for no pivoting;
		=1 for pivoting

Output:
x		matrix[p][n] decomposed (see notes below)
qraux		array[p] containing information required to recover the
		orthogonal part of the decomposition
jpvt		array[p] with jpvt[k] containing the index of the original
		matrix that has been interchanged into the k-th column, if
		pivoting is requested.

Workspace:
work		array[p] of workspace

******************************************************************************
sqrsl:
Input:
x		matrix[p][n] containing output of sqrdc.
n		number of rows in the matrix xk; same as in sqrdc.
k		number of columns in the matrix xk; k must not be greater
		than MIN(n,p), where p is the same as in sqrdc.
qraux		array[p] containing auxiliary output from sqrdc.
y		array[n] to be manipulated by sqrsl.
job		specifies what is to be computed.  job has the decimal
		expansion ABCDE, with the following meaning:
		if A != 0, compute qy.
		if B, C, D, or E != 0, compute qty.
		if C != 0, compute b.
		if D != 0, compute rsd.
		if E != 0, compute xb.
		Note that a request to compute b, rsd, or xb automatically
		triggers the computation of qty, for which an array must
		be provided.

Output:
qy		array[n] containing Qy, if its computation has been
		requested.
qty		array[n] containing Q'y, if its computation has
		been requested.  Here Q' denotes the transpose of Q.
b		array[k] containing solution of the least squares problem:
			minimize norm2(y - xk*b),
		if its computation has been requested.  (Note that if
		pivoting was requested in sqrdc, the j-th component of
		b will be associated with column jpvt[j] of the original
		matrix x that was input into sqrdc.)
rsd		array[n] containing the least squares residual y - xk*b,
		if its computation has been requested.  rsd is also the
		orthogonal projection of y onto the orthogonal complement
		of the column space of xk.
xb		array[n] containing the least squares approximation xk*b,
		if its computation has been requested.  xb is also the
		orthogonal projection of y onto the column space of x.
info		=0 unless the computation of b has been requested and R
		is exactly singular.  In this case, info is the index of
		the first zero diagonal element of R and b is left
		unaltered.

******************************************************************************
sqrst:
Input:
x		matrix[p][n] of coefficients (x is destroyed by sqrst.)
n		number of rows in the matrix x (number of observations)
p		number of columns in the matrix x (number of parameters)
y		array[n] containing right-hand-side (observations)
tol		relative tolerance used to determine the subset of
		columns of x included in the solution.  If tol is zero,
		a full complement of the MIN(n,p) columns is used.
		If tol is non-zero, the problem should be scaled so that
		all the elements of X have roughly the same absolute
		accuracy eps.  Then a reasonable value for tol is roughly
		eps divided by the magnitude of the largest element.

Output:
x		matrix[p][n] containing output of sqrdc
b		array[p] containing the solution (parameters); components
		corresponding to columns not used are set to zero.
rsd		array[n] of residuals y - Xb
k		number of columns of x used in the solution
jpvt		array[p] containing pivot information from sqrdc.
qraux		array[p] containing auxiliary information from sqrdc.

Workspace:
work		array[p] of workspace.	

******************************************************************************
Notes:
!!! WARNING !!!
These functions have many options, not all of which have been tested!
(Dave Hale, 12/31/89)


This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix x
is actually a pointer to an array of pointers to floats, as declared
above and used below.

Elements of x are stored as follows:
x[0][0]    x[1][0]    x[2][0]   ... x[p-1][0]
x[0][1]    x[1][1]    x[2][1]   ... x[p-1][1]
x[0][2]    x[1][2]    x[2][2]   ... x[p-1][2]
.                                       .
.             .                         .
.                        .              .
.                                       .
x[0][n-1]  x[1][n-1]  x[2][n-1] ... x[p-1][n-1]

sqrdc:
Uses Householder transformations to compute the QR decomposition of an n by p
matrix x.  Column pivoting based on the 2-norms of the reduced columns may be
performed at the user's option.

After decomposition, x contains in its upper triangular matrix R of the QR
decomposition.  Below its diagonal x contains information from which the
orthogonal part of the decomposition can be recovered.  Note that if
pivoting has been requested, the decomposition is not that of the original
matrix x but that of x with its columns permuted as described by jpvt.

The selection of pivot columns is controlled by jpvt as follows.
The k-th column x[k] of x is placed in one of three classes according to
the value of jpvt[k].
	if jpvt[k] >  0, then x[k] is an initial column.
	if jpvt[k] == 0, then x[k] is a free column.
	if jpvt[k] <  0, then x[k] is a final column.
Before the decomposition is computed, initial columns are moved to the
beginning of the array x and final columns to the end.  Both initial and
final columns are frozen in place during the computation and only free
columns are moved.  At the k-th stage of the reduction, if x[k] is occupied
by a free column it is interchanged with the free column of largest reduced
norm.  jpvt is not referenced if job == 0.

sqrsl:
Uses the output of sqrdc to compute coordinate transformations, projections,
and least squares solutions.  For k <= MIN(n,p), let xk be the matrix
	xk = (x[jpvt[0]], x[jpvt[1]], ..., x[jpvt[k-1]])
formed from columns jpvt[0], jpvt[1], ..., jpvt[k-1] of the original
n by p matrix x that was input to sqrdc.  (If no pivoting was done, xk
consists of the first k columns of x in their original order.)  sqrdc
produces a factored orthogonal matrix Q and an upper triangular matrix R
such that
	xk = Q * (R)
	         (0)
This information is contained in coded form in the arrays x and qraux.

The parameters qy, qty, b, rsd, and xb are not referenced if their
computation is not requested and in this case can be replaced by NULL
pointers in the calling program.  To save storage, the user may in
some cases use the same array for different parameters in the calling
sequence.  A frequently occuring example is when one wishes to compute
any of b, rsd, or xb and does not need y or qty.  In this case one may
equivalence y, qty, and one of b, rsd, or xb, while providing separate
arrays for anything else that is to be computed.  Thus the calling
sequence
	sqrsl(x,n,k,qraux,y,NULL,y,b,y,NULL,110,&info)
will result in the computation of b and rsd, with rsd overwriting y.
More generally, each item in the following list contains groups of
permissible equivalences for a single calling sequence.
	1. (y,qty,b) (rsd) (xb) (qy)
	2. (y,qty,rsd) (b) (xb) (qy)
	3. (y,qty,xb) (b) (rsd) (qy)
	4. (y,qy) (qty,b) (rsd) (xb)
	5. (y,qy) (qty,rsd) (b) (xb)
	6. (y,qy) (qty,xb) (b) (rsd)
In any group the value returned in the array allocated to the group
corresponds to the last member of the group.

sqrst:
Computes least squares solutions to the system
	Xb = y
which may be either under-determined or over-determined.  The user may
supply a tolerance to limit the columns of X used in computing the solution.
In effect, a set of columns with a condition number approximately bounded
by 1/tol is used, the other components of b being set to zero.

On return, the arrays x, jpvt, and qraux contain the usual output from
sqrdc, so that the QR decomposition of x with pivoting is fully available
to the user.  In particular, columns jpvt[0], jpvt[1], ..., jpvt[k-1]
were used in the solution, and the condition number associated with
those columns is estimated by ABS(x[0][0]/x[k-1][k-1]).

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 12/29/89
*****************************************************************************/
/**************** end self doc ********************************/

#include "cwp.h"

void
sqrdc (float **x, int n, int p, float *qraux, int *jpvt,
	float *work, int job)
/*****************************************************************************
Use Householder transformations to compute the QR decomposition of an n by p
matrix x.  Column pivoting based on the 2-norms of the reduced columns may be
performed at the user's option.
******************************************************************************
Input:
x		matrix[p][n] to decompose (see notes below)
n		number of rows in the matrix x
p		number of columns in the matrix x
jpvt		array[p] controlling the pivot columns (see notes below)
job		=0 for no pivoting;
		=1 for pivoting

Output:
x		matrix[p][n] decomposed (see notes below)
qraux		array[p] containing information required to recover the
		orthogonal part of the decomposition
jpvt		array[p] with jpvt[k] containing the index of the original
		matrix that has been interchanged into the k-th column, if
		pivoting is requested.

Workspace:
work		array[p] of workspace
******************************************************************************
Notes:
This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix x
is actually a pointer to an array of pointers to floats, as declared
above and used below.

Elements of x are stored as follows:
x[0][0]    x[1][0]    x[2][0]   ... x[p-1][0]
x[0][1]    x[1][1]    x[2][1]   ... x[p-1][1]
x[0][2]    x[1][2]    x[2][2]   ... x[p-1][2]
.                                       .
.             .                         .
.                        .              .
.                                       .
x[0][n-1]  x[1][n-1]  x[2][n-1] ... x[p-1][n-1]

After decomposition, x contains in its upper triangular matrix R of the QR
decomposition.  Below its diagonal x contains information from which the
orthogonal part of the decomposition can be recovered.  Note that if
pivoting has been requested, the decomposition is not that of the original
matrix x but that of x with its columns permuted as described by jpvt.

The selection of pivot columns is controlled by jpvt as follows.
The k-th column x[k] of x is placed in one of three classes according to
the value of jpvt[k].
	if jpvt[k] >  0, then x[k] is an initial column.
	if jpvt[k] == 0, then x[k] is a free column.
	if jpvt[k] <  0, then x[k] is a final column.
Before the decomposition is computed, initial columns are moved to the
beginning of the array x and final columns to the end.  Both initial and
final columns are frozen in place during the computation and only free
columns are moved.  At the k-th stage of the reduction, if x[k] is occupied
by a free column it is interchanged with the free column of largest reduced
norm.  jpvt is not referenced if job == 0.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 12/29/89
*****************************************************************************/
{
	int j,jp,l,lup,maxj,pl,pu,negj,swapj;
	float maxnrm,t,tt,ttt,nrmxl;
	
	pl = 0;
	pu = -1;
	
	/* if pivoting has been requested */
	if (job!=0) {
		
		/* rearrange columns according to jpvt */
		for (j=0; j<p; j++) {
			swapj = jpvt[j]>0;
			negj = jpvt[j]<0;
			jpvt[j] = j;
			if (negj) jpvt[j] = -j;
			if (swapj) {
				if (j!=pl) sswap(n,x[pl],1,x[j],1);
				jpvt[j] = jpvt[pl];
				jpvt[pl] = j;
				pl++;
			}
		}
		pu = p-1;
		for (j=p-1; j>=0; j--) {
			if (jpvt[j]<0) {
				jpvt[j] = -jpvt[j];
				if (j!=pu) {
					sswap(n,x[pu],1,x[j],1);
					jp = jpvt[pu];
					jpvt[pu] = jpvt[j];
					jpvt[j] = jp;
				}
				pu--;
			}
		}
	}
	
	/* compute the norms of the free columns */
	for (j=pl; j<=pu; j++) {
		qraux[j] = snrm2(n,x[j],1);
		work[j] = qraux[j];
	}
	
	/* perform the Householder reduction of x */
	lup = MIN(n,p);
	for (l=0; l<lup; l++) {
		if (l>=pl && l<pu) {
			
			/* 
			 * locate the column of largest norm and
			 * bring it into pivot position.
			 */
			maxnrm = 0.0;
			maxj = l;
			for (j=l; j<=pu; j++) {
			 	if (qraux[j]>maxnrm) {
					maxnrm = qraux[j];
					maxj = j;
				}
			}
			if (maxj!=l) {
				sswap(n,x[l],1,x[maxj],1);
				qraux[maxj] = qraux[l];
				work[maxj] = work[l];
				jp = jpvt[maxj];
				jpvt[maxj] = jpvt[l];
				jpvt[l] = jp;
			}
		}
		qraux[l] = 0.0;
		if (l!=n-1) {
		
			/*
			 * compute the Householder transformation
			 * for column l
			 */
			nrmxl = snrm2(n-l,&x[l][l],1);
			if (nrmxl!=0.0) {
				if (x[l][l]!=0.0)
					nrmxl = (x[l][l]>0.0) ?
						ABS(nrmxl) :
						-ABS(nrmxl);
				sscal(n-l,1.0/nrmxl,&x[l][l],1);
				x[l][l] += 1.0;
				
				/*
				 * apply the transformation to the remaining
				 * columns, updating the norms
				 */
				 for (j=l+1; j<p; j++) {
					 t = -sdot(n-l,&x[l][l],1,&x[j][l],1)/
					 	x[l][l];
					saxpy(n-l,t,&x[l][l],1,&x[j][l],1);
					if (j>=pl && j<=pu && qraux[j]!=0.0) {
						tt = ABS(x[j][l])/qraux[j];
						tt = 1.0-tt*tt;
						tt = MAX(tt,0.0);
						t = tt;
						ttt = qraux[j]/work[j];
						tt = 1.0+0.05*tt*ttt*ttt;
						if (tt!=1.0) {
							qraux[j] *= sqrt(t);
						} else {
							qraux[j] = snrm2(n-l-1,
								&x[j][l+1],1);
							work[j] = qraux[j];
						}
					}
				}
				
				/* save the transformation */
				qraux[l] = x[l][l];
				x[l][l] = -nrmxl;
			}
		}
	}
}	
							
void
sqrsl (float **x, int n, int k, float *qraux,
	float *y, float *qy, float *qty,
	float *b, float *rsd, float *xb, int job, int *info)
/*****************************************************************************
Use the output of sqrdc to compute coordinate transformations, projections,
and least squares solutions.  For k <= MIN(n,p), let xk be the matrix
	xk = (x[jpvt[0]], x[jpvt[1]], ..., x[jpvt[k-1]])
formed from columns jpvt[0], jpvt[1], ..., jpvt[k-1] of the original
n by p matrix x that was input to sqrdc.  (If no pivoting was done, xk
consists of the first k columns of x in their original order.)  sqrdc
produces a factored orthogonal matrix Q and an upper triangular matrix R
such that
	xk = Q * (R)
	         (0)
This information is contained in coded form in the arrays x and qraux.
******************************************************************************
Input:
x		matrix[p][n] containing output of sqrdc.
n		number of rows in the matrix xk; same as in sqrdc.
k		number of columns in the matrix xk; k must not be greater
		than MIN(n,p), where p is the same as in sqrdc.
qraux		array[p] containing auxiliary output from sqrdc.
y		array[n] to be manipulated by sqrsl.
job		specifies what is to be computed.  job has the decimal
		expansion ABCDE, with the following meaning:
		if A != 0, compute qy.
		if B, C, D, or E != 0, compute qty.
		if C != 0, compute b.
		if D != 0, compute rsd.
		if E != 0, compute xb.
		Note that a request to compute b, rsd, or xb automatically
		triggers the computation of qty, for which an array must
		be provided.

Output:
qy		array[n] containing Qy, if its computation has been
		requested.
qty		array[n] containing Q'y, if its computation has
		been requested.  Here Q' denotes the transpose of Q.
b		array[k] containing solution of the least squares problem:
			minimize norm2(y - xk*b),
		if its computation has been requested.  (Note that if
		pivoting was requested in sqrdc, the j-th component of
		b will be associated with column jpvt[j] of the original
		matrix x that was input into sqrdc.)
rsd		array[n] containing the least squares residual y - xk*b,
		if its computation has been requested.  rsd is also the
		orthogonal projection of y onto the orthogonal complement
		of the column space of xk.
xb		array[n] containing the least squares approximation xk*b,
		if its computation has been requested.  xb is also the
		orthogonal projection of y onto the column space of x.
info		=0 unless the computation of b has been requested and R
		is exactly singular.  In this case, info is the index of
		the first zero diagonal element of R and b is left
		unaltered.
******************************************************************************
Notes:
This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix x
is actually a pointer to an array of pointers to floats, as declared
above and used below.

Elements of x are stored as follows:
x[0][0]    x[1][0]    x[2][0]   ... x[k-1][0]
x[0][1]    x[1][1]    x[2][1]   ... x[k-1][1]
x[0][2]    x[1][2]    x[2][2]   ... x[k-1][2]
.                                       .
.             .                         .
.                        .              .
.                                       .
x[0][n-1]  x[1][n-1]  x[2][n-1] ... x[k-1][n-1]

The parameters qy, qty, b, rsd, and xb are not referenced if their
computation is not requested and in this case can be replaced by NULL
pointers in the calling program.  To save storage, the user may in
some cases use the same array for different parameters in the calling
sequence.  A frequently occuring example is when one wishes to compute
any of b, rsd, or xb and does not need y or qty.  In this case one may
equivalence y, qty, and one of b, rsd, or xb, while providing separate
arrays for anything else that is to be computed.  Thus the calling
sequence
	sqrsl(x,n,k,qraux,y,NULL,y,b,y,NULL,110,&info)
will result in the computation of b and rsd, with rsd overwriting y.
More generally, each item in the following list contains groups of
permissible equivalences for a single calling sequence.
	1. (y,qty,b) (rsd) (xb) (qy)
	2. (y,qty,rsd) (b) (xb) (qy)
	3. (y,qty,xb) (b) (rsd) (qy)
	4. (y,qy) (qty,b) (rsd) (xb)
	5. (y,qy) (qty,rsd) (b) (xb)
	6. (y,qy) (qty,xb) (b) (rsd)
In any group the value returned in the array allocated to the group
corresponds to the last member of the group.
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 12/29/89
*****************************************************************************/
{
	int i,j,ju,cb,cqy,cqty,cr,cxb;
	float t,temp;
	
	/* set info flag */
	*info = 0;
	
	/* determine what is to be computed */
	cqy = job/10000!=0;
	cqty = job%10000!=0;
	cb = (job%1000)/100!=0;
	cr = (job%100)/10!=0;
	cxb = job%10!=0;
	ju = MIN(k,n-1);
	
	/* special action when n=1 */
	if (ju==0) {
		if (cqy) qy[0] = y[0];
		if (cqty) qty[0] = y[0];
		if (cxb) xb[0] = y[0];
		if (cb) {
			if (x[0][0]==0.0)
				*info = 1;
			else
				b[0] = y[0]/x[0][0];
		}
		if (cr) rsd[0] = 0.0;
		return;
	}
	
	/* set up to compute Qy or Q'y */
	if (cqy) scopy(n,y,1,qy,1);
	if (cqty) scopy(n,y,1,qty,1);
	if (cqy) {
	
		/* compute Qy */
		for (j=ju-1; j>=0; j--) {
			if (qraux[j]!=0.0) {
				temp = x[j][j];
				x[j][j] = qraux[j];
				t = -sdot(n-j,&x[j][j],1,&qy[j],1)/x[j][j];
				saxpy(n-j,t,&x[j][j],1,&qy[j],1);
				x[j][j] = temp;
			}
		}
	}
	if (cqty) {
		
		/* compute Q'y */
		for (j=0; j<ju; j++) {
			if (qraux[j]!=0.0) {
				temp = x[j][j];
				x[j][j] = qraux[j];
				t = -sdot(n-j,&x[j][j],1,&qty[j],1)/x[j][j];
				saxpy(n-j,t,&x[j][j],1,&qty[j],1);
				x[j][j] = temp;
			}
		}
	}
	
	/* set up to compute b, rsd, or xb */
	if (cb) scopy(k,qty,1,b,1);
	if (cxb) scopy(k,qty,1,xb,1);
	if (cr && k<n) scopy(n-k,&qty[k],1,&rsd[k],1);
	if (cxb && k<n)
		for (i=k; i<n; i++)
			xb[i] = 0.0;
	if (cr)
		for (i=0; i<k; i++)
			rsd[i] = 0.0;
	if (cb) {
	
		/* compute b */
		for (j=k-1; j>=0; j--) {
			if (x[j][j]==0.0) {
				*info = j;
				break;
			}
			b[j] /= x[j][j];
			if (j!=0) {
				t = -b[j];
				saxpy(j,t,x[j],1,b,1);
			}
		}
	}
	if (cr || cxb) {
	
		/* compute rsd or xb as requested */
		for (j=ju-1; j>=0; j--) {
			if (qraux[j]!=0.0) {
				temp = x[j][j];
				x[j][j] = qraux[j];
				if (cr) {
					t = -sdot(n-j,&x[j][j],1,&rsd[j],1)/
						x[j][j];
					saxpy(n-j,t,&x[j][j],1,&rsd[j],1);
				}
				if (cxb) {
					t = -sdot(n-j,&x[j][j],1,&xb[j],1)/
						x[j][j];
					saxpy(n-j,t,&x[j][j],1,&xb[j],1);
				}
				x[j][j] = temp;
			}
		}
	}
}
							
void
sqrst (float **x, int n, int p, float *y, float tol,
	float *b, float *rsd, int *k,
	int *jpvt, float *qraux, float *work)
/*****************************************************************************
Compute least squares solutions to the system
	Xb = y
which may be either under-determined or over-determined.  The user may
supply a tolerance to limit the columns of X used in computing the solution.
In effect, a set of columns with a condition number approximately bounded
by 1/tol is used, the other components of b being set to zero.
******************************************************************************
Input:
x		matrix[p][n] of coefficients (x is destroyed by sqrst.)
n		number of rows in the matrix x (number of observations)
p		number of columns in the matrix x (number of parameters)
y		array[n] containing right-hand-side (observations)
tol		relative tolerance used to determine the subset of
		columns of x included in the solution.  If tol is zero,
		a full complement of the MIN(n,p) columns is used.
		If tol is non-zero, the problem should be scaled so that
		all the elements of X have roughly the same absolute
		accuracy eps.  Then a reasonable value for tol is roughly
		eps divided by the magnitude of the largest element.

Output:
x		matrix[p][n] containing output of sqrdc
b		array[p] containing the solution (parameters); components
		corresponding to columns not used are set to zero.
rsd		array[n] of residuals y - Xb
k		number of columns of x used in the solution
jpvt		array[p] containing pivot information from sqrdc.
qraux		array[p] containing auxiliary information from sqrdc.

Workspace:
work		array[p] of workspace.	
******************************************************************************
Notes:
This function was adapted from LINPACK FORTRAN.  Because two-dimensional 
arrays cannot be declared with variable dimensions in C, the matrix x
is actually a pointer to an array of pointers to floats, as declared
above and used below.

Elements of x are stored as follows:
x[0][0]    x[1][0]    x[2][0]   ... x[k-1][0]
x[0][1]    x[1][1]    x[2][1]   ... x[k-1][1]
x[0][2]    x[1][2]    x[2][2]   ... x[k-1][2]
.                                       .
.             .                         .
.                        .              .
.                                       .
x[0][n-1]  x[1][n-1]  x[2][n-1] ... x[k-1][n-1]

On return, the arrays x, jpvt, and qraux contain the usual output from
sqrdc, so that the QR decomposition of x with pivoting is fully available
to the user.  In particular, columns jpvt[0], jpvt[1], ..., jpvt[k-1]
were used in the solution, and the condition number associated with
those columns is estimated by ABS(x[0][0]/x[k-1][k-1]).
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 12/31/89
*****************************************************************************/
{
	int info,j,kk,m;
	
	/* initialize jpvt so that all columns are free */
	for (j=0; j<p; j++)
		jpvt[j] = 0;
	
	/* decompose x */
	sqrdc(x,n,p,qraux,jpvt,work,1);
	
	/* determine which columns to use */
	m = MIN(n,p);
	for (j=0,kk=0; j<m && ABS(x[j][j])>tol*ABS(x[0][0]); j++)
		kk++;
	
	/* solve the truncated least squares problem */
	if (kk>0) sqrsl(x,n,kk,qraux,y,rsd,rsd,work,rsd,rsd,110,&info);
	
	/* unscramble the components of b corresponding to columns used */
	for (j=0; j<kk; j++)
		b[jpvt[j]] = work[j];
	
	/* zero components of b corresponding to unused columns */
	for (j=kk; j<p; j++)
		b[jpvt[j]] = 0.0;

	/* return number of columns used */
	*k = kk;
}			
