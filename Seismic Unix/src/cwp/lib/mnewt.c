/* Copyright (c) Colorado School of Mines, 2011.*/
/* All rights reserved.                       */

/*********************** self documentation **********************/
/*****************************************************************************
MNEWT - Solve non-linear system of equations f(x) = 0 via Newton's method

mnewt	Solve non-linear system of equations f(x) = 0 via Newton's method

******************************************************************************
Function Prototype:
int mnewt (int maxiter, float ftol, float dxtol, int n, float *x, void *aux,
	void (*fdfdx)(int n, float *x, float *f, float **dfdx, void *aux));

******************************************************************************
Input:
maxiter		maximum number of iterations
ftol		converged when sum of absolute values of f less than ftol
dxtol		converged when sum of absolute values of dx less than dxtol
n		number of equations
x		array[n] containing initial guess of solution
aux		pointer to auxiliary parameters to be passed to fdfdx
fdfdx		pointer to function to evaluate f(x) and f'(x)

Output:
x		array[n] containing solution

Returned:	number of iterations; -1 if failed to converge in maxiter

******************************************************************************
Input to the user-supplied function fdfdx:
n		number of equations
x		array[n] of x0, x1, ...
aux		pointer to auxiliary variables required by fdfdx.

Output from the user-supplied function fdfdx:
f		array[n] of f0(x), f1(x), ...
dfdx		array[n][n] of f'(x);  dfdx[j][i] = dfi/dxj

******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/06/91
*****************************************************************************/
/**************** end self doc ********************************/
		
#include "cwp.h"

int mnewt (int maxiter, float ftol, float dxtol, int n, float *x, void *aux,
	void (*fdfdx)(int n, float *x, float *f, float **dfdx, void *aux))
/*****************************************************************************
Solve non-linear system of equations f(x) = 0 via Newton's method
******************************************************************************
Input:
maxiter		maximum number of iterations
ftol		converged when sum of absolute values of f less than ftol
dxtol		converged when sum of absolute values of dx less than dxtol
n		number of equations
x		array[n] containing initial guess of solution
aux		pointer to auxiliary parameters to be passed to fdfdx
fdfdx		pointer to function to evaluate f(x) and f'(x)

Output:
x		array[n] containing solution

Returned:	number of iterations; -1 if failed to converge in maxiter
******************************************************************************
Input to the user-supplied function fdfdx:
n		number of equations
x		array[n] of x0, x1, ...
aux		pointer to auxiliary variables required by fdfdx.

Output from the user-supplied function fdfdx:
f		array[n] of f0(x), f1(x), ...
dfdx		array[n][n] of f'(x);  dfdx[j][i] = dfi/dxj
******************************************************************************
Author:  Dave Hale, Colorado School of Mines, 06/06/91
*****************************************************************************/
{
	int niter,i,info,*ipvt;
	float sum,*dx,*f,**dfdx;
	
	/* allocate workspace */
	ipvt = alloc1int(n);
	dx = f = alloc1float(n);
	dfdx = alloc2float(n,n);
	
	/* loop over iterations */
	for (niter=0; niter<maxiter; ++niter) {
		
		/* compute f(x) and f'(x) */
		(*fdfdx)(n,x,f,dfdx,aux);
		
		/* check for ftol convergence */
		for (i=0,sum=0.0; i<n; ++i)
			sum += ABS(f[i]);
		if (sum<=ftol) break;
		
		/* compute lu decomposition of dfdx */
		sgefa(dfdx,n,ipvt,&info);
		
		/* if dfdx is singular, then return without solution */
		if (info>=0) return -1;
		
		/* solve system of equations for dx */
		sgesl(dfdx,n,ipvt,f,0);
		
		/* check for xtol convergence */
		for (i=0,sum=0.0; i<n; ++i)
			sum += ABS(dx[i]);
		if (sum<=dxtol) break;
		
		/* update solution */
		for (i=0; i<n; ++i)
			x[i] -= dx[i];
	}
	
	/* free workspace */
	free1int(ipvt);
	free1float(f);
	free2float(dfdx);
	
	/* return number of iterations; -1 if solution not found */
	if (niter==maxiter)
		return -1;
	else
		return niter;
}

#ifdef TEST
void fdfdx (int n, float *x, float *f, float **dfdx, void *aux)
{
	f[0] = sin(x[0])-cos(x[1]);
	f[1] = x[0]+x[1]-1.0;
	dfdx[0][0] = cos(x[0]);
	dfdx[0][1] = 1.0;
	dfdx[1][0] = sin(x[1]);
	dfdx[1][1] = 1.0;
}

void main()
{
	int niter;
	char s[100];
	float x[2];
	
	while(1) {
		printf("Enter x, y:  ");
		gets(s);
		sscanf(s,"%f %f",&x[0],&x[1]);
		niter = mnewt(100,FLT_EPSILON,FLT_EPSILON,2,x,NULL,fdfdx);
		printf("\nniter=%d x=%g y=%g\n",niter,x[0],x[1]);
	}
}
#endif /* TEST */
