/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/***************************************************************************
SVD - Singular Value Decomposition related routines
****************************************************************************

compute_svd - perform singular value decomposition 
svd_backsubstitute - back substitute results of compute_svd
svd_sort - sort singular values in descending order

****************************************************************************
Credits:
The compute_svd routine is adapted from svdecomp.c in XLISP-STAT 2.1 which is
code from Numerical Recipes adapted by Luke Tierney and David Betz.

Other contributors:
Ian Kay, Canadian Geological Survey, Ottawa, Ontario 1999.
This is a translation in C from an code written in Fortran that appeared
in NETLIB, EISPACK, and SLATEC collections that was itself a translation
from an original Algol code that appeared in:

Original Algol code that is the basis of svdecomp.c is from:
Golub, G. and C. Reinsch (1971) Handbook for automatic computation II,
 linear algebra, p 134-151. SpringerVerlag, New York.

See also discussions of a similar code in Numerical Recipes in C.
****************************************************************************
Credits
svd_sort: Nils Maercklin, GeoForschungsZentrum (GFZ) Potsdam, Germany, 2001.
***************************************************************************/

#include <cwp.h>


void 
svd_backsubstitute(float **u, float w[], float **v, int m, int n, float b[], float x[])
/**************************************************************************
svd_backsubstitute - back substitute results of compute_svd
***************************************************************************
Credits: similar to code in Netlib's EISPACK and CLAPACK
see also discussions in NR in C
**************************************************************************/
{
	int jj,j,i;
	float s,*tmp;

	tmp=alloc1float(n);

	for (j=0;j<n;j++) {
		s=0.0;
		if (w[j]) {
			for (i=0;i<m;i++) 
				s += u[i][j]*b[i];
			s /= w[j];
		}
		tmp[j]=s;
	}
	for (j=0;j<n;j++) {
		s=0.0;
		for (jj=0;jj<n;jj++) 
			s += v[j][jj]*tmp[jj];
		x[j]=s;
	}
	free1float (tmp);
}

/* macros used internally */
#define Abs(x)				  ( (x)>0.0?  (x) : (-(x)) ) 
#define SIGN(u,v)			   ( (v)>=0.0 ? Abs(u) : -Abs(u) )
 
static double PYTHAG(double a, double b)
{
	double at = fabs(a), bt = fabs(b), ct, result;

	if (at > bt)	   { ct = bt / at; result = at * sqrt(1.0 + ct * ct); }
	else if (bt > 0.0) { ct = at / bt; result = bt * sqrt(1.0 + ct * ct); }
	else result = 0.0;
	return(result);
}


int compute_svd(float **a, int m, int n, float *w, float **v)
/*************************************************************************
compute_svd - SVD decomposition routine. 
*************************************************************************
Input:
a	m by n matrix to be decomposed, gets overwritten with u
m	row dimension of a
n	column dimension of a
Returns:
w	vector of singular values of a
v	returns the right orthogonal transformation matrix
*************************************************************************
Notes:
  Takes an mxn matrix a and decomposes it into udv, where u,v are
  left and right orthogonal transformation matrices, and d is a 
  diagonal matrix of singular values.

*************************************************************************
Credits:
 This routine is adapted from svdecomp.c in XLISP-STAT 2.1 which is 
 code from Numerical Recipes adapted by Luke Tierney and David Betz.
 Copyright (c) 1989-1999, by Luke Tierney.
*************************************************************************/
{
	int flag, i, its, j, jj, k, l, nm;
	double c, f, h, s, x, y, z;
	double anorm = 0.0, g = 0.0, scale = 0.0;
	double *rv1;
  
	if (m < n) 
	{
		fprintf(stderr, "#rows must be > #cols \n");
		return(0);
	}
  
	rv1 = malloc((unsigned int) n*sizeof(double));

/* Householder reduction to bidiagonal form */
	for (i = 0; i < n; i++) {
		/* left-hand reduction */
		l = i + 1;
		rv1[i] = scale * g;
		g = s = scale = 0.0;
		if (i < m) {
			for (k = i; k < m; k++) 
				scale += fabs((double)a[k][i]);
			if (scale) {
				for (k = i; k < m; k++) {
					a[k][i] = (float)((double)a[k][i]/scale);
					s += ((double)a[k][i] * (double)a[k][i]);
				}
				f = (double)a[i][i];
				g = -SIGN(sqrt(s), f);
				h = f * g - s;
				a[i][i] = (float)(f - g);
				if (i != n - 1) {
					for (j = l; j < n; j++) {
						for (s = 0.0, k = i; k < m; k++) 
							s += ((double)a[k][i] * (double)a[k][j]);
						f = s / h;
						for (k = i; k < m; k++) 
							a[k][j] += (float)(f * (double)a[k][i]);
					}
				}
				for (k = i; k < m; k++) 
					a[k][i] = (float)((double)a[k][i]*scale);
			}
		}
		w[i] = (float)(scale * g);
	
		/* right-hand reduction */
		g = s = scale = 0.0;
		if (i < m && i != n - 1) {
			for (k = l; k < n; k++) 
				scale += fabs((double)a[i][k]);
			if (scale) {
				for (k = l; k < n; k++) {
					a[i][k] = (float)((double)a[i][k]/scale);
					s += ((double)a[i][k] * (double)a[i][k]);
				}
				f = (double)a[i][l];
				g = -SIGN(sqrt(s), f);
				h = f * g - s;
				a[i][l] = (float)(f - g);
				for (k = l; k < n; k++) 
					rv1[k] = (double)a[i][k] / h;
				if (i != m - 1) {
					for (j = l; j < m; j++) {
						for (s = 0.0, k = l; k < n; k++) 
							s += ((double)a[j][k] * (double)a[i][k]);
						for (k = l; k < n; k++) 
							a[j][k] += (float)(s * rv1[k]);
					}
				}
				for (k = l; k < n; k++) 
					a[i][k] = (float)((double)a[i][k]*scale);
			}
		}
		anorm = MAX(anorm, (fabs((double)w[i]) + fabs(rv1[i])));
	}
  
	/* accumulate the right-hand transformation */
	for (i = n - 1; i >= 0; i--) {
		if (i < n - 1) {
			if (g) {
				for (j = l; j < n; j++)
					v[j][i] = (float)(((double)a[i][j] / (double)a[i][l]) / g);
					/* double division to avoid underflow */
				for (j = l; j < n; j++) 
				{
					for (s = 0.0, k = l; k < n; k++) 
						s += ((double)a[i][k] * (double)v[k][j]);
					for (k = l; k < n; k++) 
						v[k][j] += (float)(s * (double)v[k][i]);
				}
			}
			for (j = l; j < n; j++) 
				v[i][j] = v[j][i] = 0.0;
		}
		v[i][i] = 1.0;
		g = rv1[i];
		l = i;
	}
  
	/* accumulate the left-hand transformation */
	for (i = n - 1; i >= 0; i--) {
		l = i + 1;
		g = (double)w[i];
		if (i < n - 1) 
			for (j = l; j < n; j++) 
				a[i][j] = 0.0;
		if (g) {
			g = 1.0 / g;
			if (i != n - 1) {
				for (j = l; j < n; j++) {
					for (s = 0.0, k = l; k < m; k++) 
						s += ((double)a[k][i] * (double)a[k][j]);
					f = (s / (double)a[i][i]) * g;
					for (k = i; k < m; k++) 
						a[k][j] += (float)(f * (double)a[k][i]);
				}
			}
			for (j = i; j < m; j++) 
				a[j][i] = (float)((double)a[j][i]*g);
		} else {
			for (j = i; j < m; j++) 
				a[j][i] = 0.0;
		}
		++a[i][i];
	}

	/* diagonalize the bidiagonal form */
	for (k = n - 1; k >= 0; k--) { /* loop over singular values */
		for (its = 0; its < 30; its++) {/* loop over allowed iterations */
			flag = 1;
			for (l = k; l >= 0; l--) {/* test for splitting */
				nm = l - 1;
				if (fabs(rv1[l]) + anorm == anorm) {
					flag = 0;
					break;
				}
				if (fabs((double)w[nm]) + anorm == anorm) 
					break;
			}
			if (flag) {
				c = 0.0;
				s = 1.0;
				for (i = l; i <= k; i++) {
					f = s * rv1[i];
					if (fabs(f) + anorm != anorm) {
						g = (double)w[i];
						h = PYTHAG(f, g);
						w[i] = (float)h; 
						h = 1.0 / h;
						c = g * h;
						s = (- f * h);
						for (j = 0; j < m; j++) {
							y = (double)a[j][nm];
							z = (double)a[j][i];
							a[j][nm] = (float)(y * c + z * s);
							a[j][i] = (float)(z * c - y * s);
						}
					}
				}
			}
			z = (double)w[k];
			if (l == k) { /* convergence */
				if (z < 0.0) { /* make singular value nonnegative */
					w[k] = (float)(-z);
					for (j = 0; j < n; j++) 
						v[j][k] = (-v[j][k]);
				}
				break;
			}
			if (its >= 30) {
				free((void*) rv1);
				fprintf(stderr, "No convergence after 30,000! iterations \n");
				return(0);
			}
	
			/* shift from bottom 2 x 2 minor */
			x = (double)w[l];
			nm = k - 1;
			y = (double)w[nm];
			g = rv1[nm];
			h = rv1[k];
			f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y);
			g = PYTHAG(f, 1.0);
			f = ((x - z) * (x + z) + h * ((y / (f + SIGN(g, f))) - h)) / x;
		  
			/* next QR transformation */
			c = s = 1.0;
			for (j = l; j <= nm; j++) {
				i = j + 1;
				g = rv1[i];
				y = (double)w[i];
				h = s * g;
				g = c * g;
				z = PYTHAG(f, h);
				rv1[j] = z;
				c = f / z;
				s = h / z;
				f = x * c + g * s;
				g = g * c - x * s;
				h = y * s;
				y = y * c;
				for (jj = 0; jj < n; jj++) {
					x = (double)v[jj][j];
					z = (double)v[jj][i];
					v[jj][j] = (float)(x * c + z * s);
					v[jj][i] = (float)(z * c - x * s);
				}
				z = PYTHAG(f, h);
				w[j] = (float)z;
				if (z) {
					z = 1.0 / z;
					c = f * z;
					s = h * z;
				}
				f = (c * g) + (s * y);
				x = (c * y) - (s * g);
				for (jj = 0; jj < m; jj++) {
					y = (double)a[jj][j];
					z = (double)a[jj][i];
					a[jj][j] = (float)(y * c + z * s);
					a[jj][i] = (float)(z * c - y * s);
				}
			}
			rv1[l] = 0.0;
			rv1[k] = f;
			w[k] = (float)x;
		}
	}
	free((void*) rv1);
	return(1);
}



void svd_sort(float **u, float *w, float **v, int n, int m)
/**********************************************************************
svd_sort - sort singular values and corresponding eigenimages
		   in descending order
**********************************************************************
Input:
u[][]	
**********************************************************************
Notes:
We assume input of the singular value decomposition of a matrix a[][]
of the form:
					t
a[][] = u[][] w[] v[][]

**********************************************************************
Credits: Nils Maercklin, GeoForschungsZentrum (GFZ) Potsdam, Germany, 2001.
**********************************************************************/
{
		int k,j,i;
		float p;

		for (i=0;i<n-1;i++) {
			    p=w[k=i];
                for (j=i+1;j<=n;j++)
                        if (w[j] >= p) p=w[k=j];
                if (k != i) {
                        w[k]=w[i];
                        w[i]=p;
                        for (j=0;j<n;j++) {
                                p=v[j][i];
                                v[j][i]=v[j][k];
                                v[j][k]=p;
                        }
                        for (j=0;j<m;j++) {
                                p=u[j][i];
                                u[j][i]=u[j][k];
                                u[j][k]=p;
                        }
                }
        }
}

