
/*********************** self documentation **********************/
/************************************************************************************
Conjugate Gradient routines -

simple_conj_gradient -  simple conjugate gradient routine.


*************************************************************************************
Function Prototypes:
void simple_conj_gradient(int n, float *x, int m, float *b, float **a, int niter);

*************************************************************************************

*********************************************************************
References:
Claerbout, J. F. (1985), Conjugate gradients for beginners, SEP 44,
Stanford Exploration Project.
Shewchuk, Jonathan Richard (1994), An introduction to the conjugate 
gradient method without the agonizing pain,
edition 1 1/4, School of Computer Science Carnegie Mellon University
*********************************************************************
Credits:
CWP: John Stockwell,  May 2014
Based on the subroutine cg() which appeared in (Claerbout, 1985)

*************************************************************************************/
/**************** end self doc ********************************/
#include "cwp.h"


/* Prototype of function used internally */
float inner_product(float *x, float *y, int n);

#define CONJ_EPS 1E-30;
void simple_conj_gradient(int n, float *x, int m, 
				float *b, float **a, int niter)
/*********************************************************************
simple_conj_gradient - compute the least-squares solution 
			to Ax = b  where A is an m by n (m>=n) system,
			using the conjugate gradient method.

*******************************************************************
Input:
int n			size of the unknown vector x
int m			number of equations (size of b vector)
float *b		the data
float **a		the m by n matrix of the system
int niter		number of iterations permitted

Output:
float *x 		the unknown vector being solved for

********************************************************************
Notes:
We are solving the system  b = A x   by minimizing the residual r
as defined by  r = b - A x.  The vectors r and b are m dimensional
whereas the vector x is n dimensional and we assume m>=n. Hence
A is an n by m matrix.

The minimization is taken in the L-2 sense, and is therefore a
least squares problem.

Theory:
If we consider the problem of solving  Ax = b  for x in a least-squares
sense, we may consider the square of the error E^2 as

E^2 = [b - A x]^T [b - A x]

Taking the gradient of E^2  with respect to x, and setting it to 0, we 
find that minimizing E^2 is equivalent to:

E^2 = A^T [ b - A x] = g = 0

The gradient of E^2 with respect x vanishes at the solution point.
However in general g is a nonzero vector, and we may consider
[b - Ax ] as being a non zero vector, as well, called the residual r

g = A^T [ b - A x ] = A^T r,   where r = [b - A x]

The "conjugate gradient" is given by the expression
gamma = A g

Why this is called the conjgate gradient may be seen by considering
g dot gamma which is 
g^T gamma = g^T A g

If we are at the solution point   g^T A g = 0 implying that  g^T and
g are  "A orthogonal" in the sense of quadratic forms and are thus
conjugate vectors in this sense.

We arrive at the solution vector x by successive
steps  x = s_1 + s_2 + s_3 + ...

We define  the update in the step by sigma_j = A s_j-1

The solution update is given by  x_j = x_j-1 + s_j-1
The residual update is given by  r_j = r_j-1 - s_j-1

If we are minimizing the residual, we may write this in terms of these
new quantities as the square of the error in the residual, modified
by subtracting from the residual a vector that is the linear
combination of the previous conjugate gradient and the previous
step, which is given by alpha * gamma + beta*sigma  where alpha and
beta are scalars.

As we are again considering this in the least squares sense:
E_1^2 = [ r  -  alpha*gamma - beta*sigma] [ r - alpha*gamma - beta*sigma]

Taking the gradient of E_1^2 with respect to (alpha, beta), and setting
the result to zero yields the two equations that we must solve in order
to find alpha and beta:

gamma . ( r - alpha*gamma - beta*sigma) = 0
sigma . ( r - alpha*gamma - beta*sigma) = 0

Here . denotes the scalar product of vectors.

In a 2x2 matrix form:

| gamma.gamma     gamma.sigma | | alpha |   | gamma.r |
|                             | |       | = |         |
| gamma.sigma     sigma.sigma | | beta  |   | sigma.r |

The solution for alpha and beta is a simple inverse of the 2x2 matrix

| alpha |    1  | sigma.sigma  -gamma.sigma |  | gamma.r |
|       | = ----|                           |  |         |
| beta  | =  D  | -gamma.sigma  gamma.gamma |  | sigma.r |

The determinant of the original matrix D is given by
D = (gamma.gamma)(sigma.sigma) - (gamma.sigma)^2

One step of steepest descents is needed to start the process, which
is derived from the above equations by setting sigma=0, beta=0, and r = b
for the initial value, yielding:

alpha = ( gamma.b)/(gamma.gamma)

where, again . indicates the inner product of vectors

The iterative process then continues by calculating the values
of alpha and beta and updating the residual and the step until
the number of iterations given by "niter" is completed.

*********************************************************************
References:
Claerbout, J. F. (1985), Conjugate gradients for beginners, SEP 44,
Stanford Exploration Project.
Shewchuk, Jonathan Richard (1994), An introduction to the conjugate 
gradient method without the agonizing pain,
edition 1 1/4, School of Computer Science Carnegie Mellon University
*********************************************************************

Credits:
CWP: John Stockwell,  May 2014
Based on the subroutine cg() which appeared in (Claerbout, 1985)
*********************************************************************/
{
	int i,j,iter;		/* counters 				*/

	float gamma2=0.0;	/* gamma dot gamma 			*/
	float sigma2=0.0;	/* sigma dot sigma 			*/
	float gambysig=0.0;	/* sigma dot gamma 			*/
	float gambyr=0.0;	/* gamma dot r	 			*/
	float sigbyr=0.0;	/* sigma dot r	 			*/
	float determ=0.0;	/* determinant 				*/
	float alpha=0.0, beta=0.0;	/* updating parameters		*/

	float *s=NULL;		/* step in x 				*/
	float *r=NULL;		/* residual r = b - Ax 			*/
	float *g=NULL;		/* gradient				*/
	float *gamma=NULL;	/* conjugate gradient			*/
	float *sigma=NULL;	/* step update				*/

	/* allocate working space */
	s = alloc1float(n);
	r = alloc1float(m);
	g = alloc1float(n);
	gamma = alloc1float(m);
	sigma = alloc1float(m);
	
	/* zero out vectors */
	memset( (void *) x, 0, n*FSIZE);
	memset( (void *) s, 0, n*FSIZE);
	memset( (void *) gamma, 0, m*FSIZE);
	memset( (void *) sigma, 0, m*FSIZE);

	/* initialize the residual vector to the values of b */
	memcpy((void *) r, (const void *) b, m*FSIZE);


	/* begin iterations */
	for (iter=0; iter<niter; ++iter) {
		for (i=0; i<n; ++i) {
			g[i] = 0.0;
			for (j=0; j<m; ++j) { /* gradient g =  A^T r  */
				g[i] += r[j]*a[j][i] ;
			}
		}
		for (j=0; j < m; ++j) { /*  conjugate gradient gamma = A g */
			gamma[j] = 0.0;
			for (i=0; i<n; ++i) { 
				gamma[j] += a[j][i] * g[i];
			}
		}
		if (iter == 0) {  /* first step is steepest descent */
			gambyr = inner_product(gamma,r,m);
			gamma2 = inner_product(gamma,gamma,m);
			alpha = gambyr/gamma2;
			beta = 0.0;

		} else { /* search plane by applying the conj gradient */

			gamma2 = inner_product(gamma,gamma,m);
			sigma2 = inner_product(sigma,sigma,m);
			gambysig = inner_product(gamma,sigma,m);

			determ = gamma2*sigma2 - gambysig*gambysig + CONJ_EPS; 

			gambyr = inner_product(gamma,r,m);
			sigbyr = inner_product(sigma,r,m);

			alpha = ( sigma2*gambyr - gambysig*sigbyr)/determ;
			beta = ( gamma2*sigbyr - gambysig*gambyr )/determ;
		}

		for (i = 0; i < n; ++i) { /* update the step */
			s[i] = alpha*g[i] + beta*s[i];
		}
		
		for (i = 0; i < m; ++i) { /* sigma update */
			sigma[i] = alpha*gamma[i] + beta*sigma[i];
		}

		for (i = 0; i < n; ++i) { /* solution x update */
			x[i] += (float) s[i];
		}

		for (i = 0; i < m; ++i) { /* residual r update */
			r[i] -= sigma[i];
		}

	}

	free1float(r);
	free1float(s);
	free1float(g);
	free1float(sigma);
	free1float(gamma);

	return;
}	
		
	
				 

float inner_product( float *x , float *y , int n)
/************************************************************************
inner_product - compute the inner or dot (scalar) product of two vectors

*************************************************************************
Input:
float *x		n vector
float *y		n vector
int n			length of vectors
Returns:
float prod		value of the inner product

*************************************************************************
Credits:
CWP: John Stockwell May 2014
*************************************************************************/
{
	int i;	/* counter */
	float prod = 0.0 ;
	
	for (i=0; i < n ; ++i) {
		float tmpx=x[i];
		float tmpy=y[i];
		prod+=tmpx*tmpy;
	}
	return prod;
}

